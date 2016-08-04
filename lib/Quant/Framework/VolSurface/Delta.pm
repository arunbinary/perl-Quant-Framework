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
use List::MoreUtils qw(none);
use Math::Function::Interpolator;
use Storable qw( dclone );
use Try::Tiny;

=head2 for_date

The date for which we wish data

=cut

has for_date => (
    is      => 'ro',
    isa     => 'Maybe[Date::Utility]',
    default => undef,
);

sub _document_content {
    my $self = shift;

    my %structure = (
        surfaces      => $self->surfaces_to_save,
        date          => $self->recorded_date->datetime_iso8601,
        master_cutoff => $self->cutoff->code,
        symbol        => $self->symbol,
        type          => $self->type,
    );

    return \%structure;
}

has document => (
    is         => 'rw',
    lazy_build => 1,
);

sub _build_document {
    my $self = shift;

    my $document = $self->chronicle_reader->get('volatility_surfaces', $self->symbol);

    if ($self->for_date and $self->for_date->epoch < Date::Utility->new($document->{date})->epoch) {
        $document = $self->chronicle_reader->get_for('volatility_surfaces', $self->symbol, $self->for_date->epoch);

        # This works around a problem with Volatility surfaces and negative dates to expiry.
        # We have to use the oldest available surface.. and we don't really know when it
        # was relative to where we are now.. so just say it's from the requested day.
        # We do not allow saving of historical surfaces, so this should be fine.
        $document //= {};
    }

    return $document;
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
        spot      => $self->builder->build_spot->spot_quote,
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
    $new_args{spot}             ||= $self->builder->build_spot->spot_quote;
    $new_args{premium_adjusted} ||= $underlying_config->{market_convention}->{delta_premium_adjusted};
    $new_args{r_rate}           ||= $self->builder->build_interest_rate->interest_rate_for($new_args{t});
    $new_args{q_rate}           ||= $self->builder->build_dividend->dividend_rate_for($new_args{t});

    $new_args{atm_vol} ||= $self->get_volatility({
        days  => $new_args{days},
        delta => 50,
    });

    return \%new_args;
}

=head2 generate_surface_for_cutoff

Transforms the surface to a given cutoff. Cutoff can be given either
as a qf_cutoff_code string, or a Quant::Framework::VolSurface::Cutoff instance.

Returns the cut surface data-structure (not a B::M::VS instance).

=cut

sub generate_surface_for_cutoff {
    my ($self, $cutoff) = @_;

    # Everything with a trailing 1 is from what we are transforming from.
    # Everything else is what we're transforming to.

    my $surface1 = $self;
    $cutoff = Quant::Framework::VolSurface::Cutoff->new($cutoff) if (not ref $cutoff);
    my $surface_hashref   = {};
    my $underlying_config = $surface1->underlying_config;

    foreach my $maturity (@{$surface1->term_by_day}) {
        my $t1 = $surface1->cutoff->seconds_to_cutoff_time({
            from     => $surface1->recorded_date,
            maturity => $maturity,
            calendar => $self->builder->build_trading_calendar,
        });
        my $t = $cutoff->seconds_to_cutoff_time({
            from     => $surface1->recorded_date,
            maturity => $maturity,
            calendar => $self->builder->build_trading_calendar,
        });

        foreach my $delta (@{$surface1->deltas}) {
            my $v1 = $surface1->get_volatility({
                days  => $maturity,
                delta => $delta,
            });

            my $v = $v1 * sqrt($t / $t1);
            $surface_hashref->{$maturity}->{smile}->{$delta} = $v;

            if (defined $surface1->surface->{$maturity}->{vol_spread}->{$delta}) {
                $surface_hashref->{$maturity}->{vol_spread}->{$delta} = $surface1->surface->{$maturity}->{vol_spread}->{$delta};
            }

        }

        if (my $tenor = $surface1->surface->{$maturity}->{tenor}) {
            $surface_hashref->{$maturity}->{tenor} = $tenor;
        }
    }

    return $surface_hashref;
}

has _default_cutoff_list => (
    is      => 'ro',
    isa     => 'ArrayRef',
    default => sub { ['UTC 21:00', 'UTC 23:59'] },
);

=head2 surfaces_to_save

The surfaces that will be saved on Chronicle

=cut

has surfaces_to_save => (
    is         => 'ro',
    isa        => 'HashRef',
    lazy_build => 1,
);

sub _build_surfaces_to_save {
    my $self = shift;

    my $master_surface = $self->surface;
    my %surfaces;
    $surfaces{$self->cutoff->code} = $master_surface;

    $surfaces{$_} = $self->generate_surface_for_cutoff($_) foreach @{$self->_default_cutoff_list};

    return \%surfaces;
}

sub _build_surface {
    my $self = shift;

    my $doc    = $self->document;
    my $cutoff = $self->cutoff->code;

    return $doc->{surfaces}->{$cutoff} if $doc->{surfaces}->{$cutoff};

    my $master_cutoff = $doc->{master_cutoff};
    if (not $doc->{surfaces}->{$master_cutoff}) {
        die('master surface is missing for ' . $self->symbol . ' on ' . $self->recorded_date->datetime_iso8601);
    }

    my $master_surface = __PACKAGE__->new(
        symbol            => $self->symbol,
        cutoff            => $master_cutoff,
        for_date          => $self->for_date,
        chronicle_reader  => $self->chronicle_reader,
        chronicle_writer  => $self->chronicle_writer,
        underlying_config => $self->underlying_config,
    );
    my $surface = $master_surface->generate_surface_for_cutoff($cutoff);
    $self->_stores_surface($cutoff, $surface);

    return $surface;
}

sub _stores_surface {
    my ($self, $cutoff, $surface_hashref) = @_;

    try {
        my $doc = $self->document;
        $doc->{surfaces} ||= {};
        $doc->{surfaces}->{$cutoff} = $surface_hashref;
    }
    catch {
        warn('Could not save ' . $cutoff . ' cutoff for ' . $self->symbol);
    };

    return;
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
    cutoff  => $my_new_cutoff,
  });

Returns a new cloned instance.
You can pass overrides to override an attribute value as it is on the original surface.

=cut

sub clone {
    my ($self, $args) = @_;

    my $clone_args;
    $clone_args = dclone($args) if $args;

    $clone_args->{underlying_config} = $self->underlying_config if (not exists $clone_args->{underlying_config});
    $clone_args->{cutoff}            = $self->cutoff            if (not exists $clone_args->{cutoff});

    if (not exists $clone_args->{surface}) {
        my $orig_surface = dclone($self->surface);
        my %surface_to_clone = map { $_ => $orig_surface->{$_} } @{$self->original_term_for_smile};
        $clone_args->{surface} = \%surface_to_clone;
    }

    $clone_args->{recorded_date}   = $self->recorded_date         if (not exists $clone_args->{recorded_date});
    $clone_args->{print_precision} = $self->print_precision       if (not exists $clone_args->{print_precision});
    $clone_args->{original_term}   = dclone($self->original_term) if (not exists $clone_args->{original_term});
    $clone_args->{chronicle_reader} = $self->chronicle_reader;
    $clone_args->{chronicle_writer} = $self->chronicle_writer;

    return $self->meta->name->new($clone_args);
}

=head2 cutoff

cutoff is New York 10:00 when we save new volatility delta surfaces. (Surfaces that we get from Bloomberg)

cutoff is UTC 23:59 or UTC 21:00 for contract pricing.

=cut

has cutoff => (
    is         => 'ro',
    isa        => 'qf_cutoff_helper',
    lazy_build => 1,
    coerce     => 1,
);

sub _build_cutoff {
    my $self = shift;

    my $date = $self->for_date ? $self->for_date : Date::Utility->new;
    my $cutoff_string =
        $self->_new_surface ? 'New York 10:00' : 'UTC ' . $self->builder->build_trading_calendar->standard_closing_on($date)->time_hhmm;

    return Quant::Framework::VolSurface::Cutoff->new($cutoff_string);
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;
