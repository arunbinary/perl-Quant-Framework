package Quant::Framework::VolSurface::Delta;

=head1 NAME

Quant::Framework::VolSurface::Delta

=head1 DESCRIPTION

Represents a volatility surface, built from market implied volatilities.

=head1 SYNOPSIS

    my $surface = Quant::Framework::VolSurface::Delta->new({underlying_config => $underlying_config});

=cut

use Moose;

extends 'Quant::Framework::VolSurface';

use Date::Utility;
use VolSurface::Utils qw( get_delta_for_strike get_strike_for_moneyness );
use Math::Function::Interpolator;
use Storable qw( dclone );

sub _document_content {
    my $self = shift;

    my %structure = (
        surface => $self->surface,
        date    => $self->recorded_date->datetime_iso8601,
        symbol  => $self->symbol,
        type    => $self->type,
    );

    return \%structure;
}

=head2 save

Saves current surface using given chronicle writer.

=cut

sub save {
    my $self = shift;

    #if chronicle does not have this document, first create it because in document_content we will need it
    if (not defined $self->chronicle_reader->get('volatility_surfaces', $self->symbol)) {
        #Due to some strange coding of retrieval for recorded_date, there MUST be an existing document (even empty)
        #before one can save a document. As a result, upon the very first storage of an instance of the document, we need to create an empty one.
        $self->chronicle_writer->set('volatility_surfaces', $self->symbol, {});
    }

    return $self->chronicle_writer->set('volatility_surfaces', $self->symbol, $self->_document_content, $self->recorded_date);
}

=head1 ATTRIBUTES

=head2 type

Return the surface type

=cut

has '+type' => (
    default => 'delta',
);

=head2 deltas

Get the available deltas for which we have vols.

Returns an ArrayRef, is required and read-only.

=cut

has deltas => (
    is         => 'ro',
    isa        => 'ArrayRef',
    lazy_build => 1,
);

sub _build_deltas {
    my $self = shift;
    return $self->smile_points;
}

has atm_spread_point => (
    is      => 'ro',
    isa     => 'Num',
    default => '50',
);

=head2 get_volatility

Given a maturity of some form and a barrier of some form, gives you a vol
from the surface.

The barrier can be specified either as a strike (strike => $bet->barrier) or a
delta (delta => 25).

The maturity can be given either as a number of days (days => 7), an expiry date
(expiry_date => Date::Utility->new) or a tenor (tenor => 'ON').

When given an expiry_date, get_volatility assumes that you want an integer number
of days, and calculates that based on the number of vol rollovers between the
recorded_date and the given expiry. So if the rollover is at GMT2200 (NY1700) and
recorded_date is 2012-02-01 10:00:00, a given expiry of 2012-02-02 10:00:00 would
give you the ON vol, but an expiry of 2012-02-02 23:59:59 would give a 2-day vol.

USAGE:

  my $vol = $s->get_volatility({delta => 25, days => 7});
  my $vol = $s->get_volatility({strike => $bet->barrier, tenor => '1M'});
  my $vol = $s->get_volatility({delta => 50, expiry_date => Date::Utility->new});

=cut

sub get_volatility {
    my ($self, $args) = @_;

    # args validity checks
    die("Must pass exactly one of delta, strike or moneyness to get_volatility.")
        if (scalar(grep { defined $args->{$_} } qw(delta strike moneyness)) != 1);
    die("Must pass exactly one of days, tenor or expirty_date to get_volatility.")
        if (scalar(grep { defined $args->{$_} } qw(days tenor expiry_date)) != 1);

    if (not $args->{days}) {
        $args->{days} = $self->_convert_expiry_to_day($args);
    }

    my $sought_point =
          (defined $args->{delta})  ? $args->{delta}
        : (defined $args->{strike}) ? $self->_convert_strike_to_delta($args)
        :                             $self->_convert_moneyness_to_delta($args);

    return $self->SUPER::get_volatility({
        sought_point => $sought_point,
        days         => $args->{days},
    });
}

=head2 interpolate

Quadratic interpolation to interpolate across smile
->interpolate({smile => $smile, sought_point => $sought_point});

=cut

sub interpolate {
    my ($self, $args) = @_;

    return Math::Function::Interpolator->new(points => $args->{smile})->quadratic($args->{sought_point});
}

sub _convert_moneyness_to_delta {
    my ($self, $args) = @_;

    $args->{strike} = get_strike_for_moneyness({
        moneyness => $args->{moneyness},
        spot      => $self->underlying_config->spot
    });

    delete $args->{moneyness};
    my $delta = $self->_convert_strike_to_delta($args);

    return $delta;
}

sub _convert_strike_to_delta {
    my ($self, $args) = @_;

    my $conversion_args = $self->_ensure_conversion_args($args);

    return 100 * get_delta_for_strike($conversion_args);
}

sub _ensure_conversion_args {
    my ($self, $args) = @_;

    my %new_args          = %{$args};
    my $underlying_config = $self->underlying_config;

    $new_args{t}                ||= $new_args{days} / 365;
    $new_args{spot}             ||= $underlying_config->spot;
    $new_args{premium_adjusted} ||= $underlying_config->{market_convention}->{delta_premium_adjusted};
    $new_args{r_rate}           ||= $self->builder->build_interest_rate->interest_rate_for($new_args{t});
    $new_args{q_rate}           ||= $self->builder->build_dividend->dividend_rate_for($new_args{t});

    $new_args{atm_vol} ||= $self->get_volatility({
        days  => $new_args{days},
        delta => 50,
    });

    return \%new_args;
}

sub _extrapolate_smile_down {
    my ($self, $days) = @_;

    my $first_market_point = $self->original_term_for_smile->[0];
    return $self->surface->{$first_market_point}->{smile} if $self->_market_name eq 'indices';
    my $market     = $self->get_market_rr_bf($first_market_point);
    my %initial_rr = %{$self->_get_initial_rr($market)};

    # we won't be using the indices case unless we revert back to delta surfaces
    my %rr_bf = (
        ATM   => $market->{ATM},
        BF_25 => $market->{BF_25},
    );
    $rr_bf{BF_10} = $market->{BF_10} if (exists $market->{BF_10});

    # Only RR is interpolated at this point.
    # Data structure is here in case that changes, plus it's easier to understand.
    foreach my $which (keys %initial_rr) {
        my $interp = Math::Function::Interpolator->new(
            points => {
                $first_market_point => $market->{$which},
                0                   => $initial_rr{$which},
            });
        $rr_bf{$which} = $interp->linear($days);
    }

    my $extrapolated_smile->{smile} = {
        25 => $rr_bf{RR_25} / 2 + $rr_bf{BF_25} + $rr_bf{ATM},
        50 => $rr_bf{ATM},
        75 => $rr_bf{BF_25} - $rr_bf{RR_25} / 2 + $rr_bf{ATM},
    };

    if (exists $market->{RR_10}) {
        $extrapolated_smile->{smile}->{10} = $rr_bf{RR_10} / 2 + $rr_bf{BF_10} + $rr_bf{ATM};
        $extrapolated_smile->{smile}->{90} = $rr_bf{RR_10} / 2 + $rr_bf{BF_10} + $rr_bf{ATM};
    }

    return $extrapolated_smile->{smile};
}

=head2 clone

USAGE:

  my $clone = $s->clone({
    surface => $my_new_surface,
  });

Returns a new cloned instance.
You can pass overrides to override an attribute value as it is on the original surface.

=cut

sub clone {
    my ($self, $args) = @_;

    my $clone_args;
    $clone_args = dclone($args) if $args;

    $clone_args->{underlying_config} = $self->underlying_config if (not exists $clone_args->{underlying_config});

    if (not exists $clone_args->{surface}) {
        my $orig_surface = dclone($self->surface);
        my %surface_to_clone = map { $_ => $orig_surface->{$_} } @{$self->original_term_for_smile};
        $clone_args->{surface} = \%surface_to_clone;
    }

    $clone_args->{recorded_date}   = $self->recorded_date         if (not exists $clone_args->{recorded_date});
    $clone_args->{original_term}   = dclone($self->original_term) if (not exists $clone_args->{original_term});
    $clone_args->{chronicle_reader} = $self->chronicle_reader;
    $clone_args->{chronicle_writer} = $self->chronicle_writer;

    return $self->meta->name->new($clone_args);
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;
