package Quant::Framework::VolSurface::Moneyness;

=head1 NAME

Quant::Framework::VolSurface::Moneyness

=head1 DESCRIPTION

Base class for strike-based volatility surfaces by moneyness.

=cut

use Moose;
extends 'Quant::Framework::VolSurface';

use Date::Utility;
use VolSurface::Utils qw(get_delta_for_strike get_strike_for_moneyness);
use Try::Tiny;
use Math::Function::Interpolator;
use List::MoreUtils qw(indexes);
use List::Util qw(min first);
use Storable qw( dclone );
use JSON qw(from_json);

sub _document_content {
    my $self = shift;

    my %structure = (
        surface        => $self->surface,
        date           => $self->recorded_date->datetime_iso8601,
        symbol         => $self->symbol,
        type           => $self->type,
        spot_reference => $self->spot_reference,
    );

    return \%structure;
}

=head2 effective_date

Effective date for the volatility surface.
For moneyness where there's no concept of rollover, the effective_date is the recorded date.

=cut

has effective_date => (
    is         => 'ro',
    lazy_build => 1,
);

sub _build_effective_date {
    my $self = shift;
    return $self->recorded_date;
}

=head2 surface

Volatility surface in a hash reference.

=cut

has surface => (
    is         => 'ro',
    isa        => 'HashRef',
    lazy_build => 1,
);

sub _build_surface {
    my $self = shift;

    my $date = $self->for_date // Date::Utility->new;

    return $self->document->{surface} // $self->document->{surfaces}{'UTC ' . $self->calendar->standard_closing_on($date)->time_hhmm} // {};
}

has variance_table => (
    is         => 'ro',
    lazy_build => 1,
);

sub _build_variance_table {
    my $self = shift;

    my $raw_surface    = $self->surface;
    my $effective_date = $self->effective_date;
    my $close          = $self->calendar->standard_closing_on($effective_date);

    die 'Attempt to update volatility surface for ' . $self->symbol . ' when the exchange is closed' unless $close;

    # keys are tenor in epoch, values are associated variances.
    my %table = (
        $self->recorded_date->epoch => {map { $_ => 0 } @{$self->smile_points}},
    );
    foreach my $tenor (@{$self->original_term_for_smile}) {
        my $epoch = $effective_date->plus_time_interval($tenor . 'd' . $close->seconds_after_midnight . 's')->epoch;
        foreach my $moneyness (@{$self->smile_points}) {
            my $volatility = $raw_surface->{$tenor}{smile}{$moneyness};
            $table{$epoch}{$moneyness} = $volatility**2 * $tenor if $volatility;
        }
    }

    return \%table;
}

=head2 type

Return the surface type

=cut

has '+type' => (
    default => 'moneyness',
);

=head2 min_vol_spread

minimum volatility spread that we can accept for this volatility surface.

=cut

has min_vol_spread => (
    is      => 'ro',
    isa     => 'Num',
    default => 3.1 / 100,
);

has atm_spread_point => (
    is      => 'ro',
    isa     => 'Num',
    default => 100,
);

=head2 spot_reference

Get the spot reference used to calculate the surface.
We should always use reference spot of the surface for any moneyness-related vol calculation

=cut

has spot_reference => (
    is         => 'rw',
    isa        => 'Num',
    lazy_build => 1,
);

sub _build_spot_reference {
    my $self = shift;

    return $self->document->{spot_reference};
}

=head2 get_volatility

USAGE:

  my $vol = $s->get_volatility({moneyness => 96, days => 7});
  my $vol = $s->get_volatility({strike => $bet->barrier, tenor => '1M'});
  my $vol = $s->get_volatility({moneyness => 90, expiry_date => Date::Utility->new});

=cut

sub get_volatility {
    my ($self, $args) = @_;

    if (scalar(grep { defined $args->{$_} } qw(delta moneyness strike)) != 1) {
        die("Must pass exactly one of [delta, moneyness, strike] to get_volatility.");
    }

    die "Must pass two dates [from, to] to get volatility." if (not($args->{from} and $args->{to}));

    $args->{days} = $self->underlying_config->default_volatility_duration // (($args->{to}->epoch - $args->{from}->epoch) / 86400);

    my $vol;
    if ($args->{delta}) {

        # we are handling delta seperately because it involves
        # a lot more steps to calculate vol for a delta point
        # on a moneyness surface
        $vol = $self->_calculate_vol_for_delta({
            delta => $args->{delta},
            days  => $args->{days},
        });
    } else {
        my $sought_point =
              $args->{strike}
            ? $args->{strike} / $self->spot_reference * 100
            : $args->{moneyness};

        my $calc_args = {
            sought_point => $sought_point,
            days         => $args->{days}};
        $vol = $self->SUPER::get_volatility($calc_args);
    }

    return $vol;
}

=head2 interpolate

This is how you could interpolate across smile.
This uses the default interpolation method of the surface.

    $surface->interpolate({smile => $smile, sought_point => $sought_point});
=cut

sub interpolate {
    my ($self, $args) = @_;

    my $method = keys %{$args->{smile}} < 5 ? 'quadratic' : 'cubic';
    my $interpolator = Math::Function::Interpolator->new(points => $args->{smile});

    return $interpolator->$method($args->{sought_point});
}

# rr and bf only make sense in delta term. Here we convert the smile to a delta smile.
override get_market_rr_bf => sub {
    my ($self, $day) = @_;

    my %smile = map { $_ => $self->_calculate_vol_for_delta({delta => $_, days => $day}) } qw(25 50 75);

    return $self->get_rr_bf_for_smile(\%smile);
};

## PRIVATE ##

sub _calculate_vol_for_delta {
    my ($self, $args) = @_;

    my $delta = $args->{delta};
    my $days  = $args->{days};
    my $smile = $self->_convert_moneyness_smile_to_delta($days);

    return $smile->{$delta}
        ? $smile->{$delta}
        : $self->_interpolate_delta({
            smile        => $smile,
            sought_point => $delta
        });
}

sub _interpolate_delta {
    my ($self, $args) = @_;

    my %smile = %{$args->{smile}};

    die('minimum of three points on a smile')
        if keys %smile < 3;

    my @sorted = sort { $a <=> $b } keys %smile;
    my %new_smile =
        map { $_ => $smile{$_} } grep { $_ > 1 and $_ < 99 } @sorted;

    if (keys %new_smile < 5) {
        my @diff = map { abs($_ - 50) } @sorted;
        my $atm_index = indexes { min(@diff) == abs($_ - 50) } @sorted;
        %new_smile =
            map { $sorted[$_] => $smile{$sorted[$_]} } ($atm_index - 1 .. $atm_index + 1);
    }

    $args->{smile} = \%new_smile;

    return $self->interpolate($args);
}

sub _convert_moneyness_smile_to_delta {
    my ($self, $days) = @_;

    my $moneyness_smile = $self->get_smile($days);

    my %strikes =
        map { get_strike_for_moneyness({moneyness => $_ / 100, spot => $self->spot_reference,}) => $moneyness_smile->{$_} } keys %$moneyness_smile;

    my $tiy    = $days / 365;
    my $r_rate = $self->builder->build_interest_rate->interest_rate_for($tiy);
    my $q_rate = $self->builder->build_dividend->dividend_rate_for($tiy);
    my %deltas;
    foreach my $strike (keys %strikes) {
        my $vol   = $strikes{$strike};
        my $delta = 100 * get_delta_for_strike({
            strike           => $strike,
            atm_vol          => $vol,
            t                => $tiy,
            spot             => $self->spot_reference,
            r_rate           => $r_rate,
            q_rate           => $q_rate,
            premium_adjusted => $self->underlying_config->market_convention->{delta_premium_adjusted},
        });
        $deltas{$delta} = $vol;
    }

    return \%deltas,;
}

sub _extrapolate_smile_down {
    my $self = shift;

    my $first_market_point = $self->original_term_for_smile->[0];

    return $self->surface->{$first_market_point}->{smile};
}

=head2 clone

USAGE:

  my $clone = $s->clone({
    surface => $my_new_surface,
  });

Returns a new Quant::Framework::VolSurface instance. You can pass overrides to override an attribute value as it is on the original surface.

=cut

sub clone {
    my ($self, $args) = @_;

    my %clone_args;
    %clone_args = %$args if $args;

    $clone_args{spot_reference} = $self->spot_reference
        if (not exists $clone_args{spot_reference});
    $clone_args{underlying_config} = $self->underlying_config
        if (not exists $clone_args{underlying_config});

    if (not exists $clone_args{surface}) {
        my $orig_surface = dclone($self->surface);
        my %surface_to_clone = map { $_ => $orig_surface->{$_} } @{$self->original_term_for_smile};
        $clone_args{surface} = \%surface_to_clone;
    }

    $clone_args{recorded_date} = $self->recorded_date
        if (not exists $clone_args{recorded_date});
    $clone_args{original_term} = dclone($self->original_term)
        if (not exists $clone_args{original_term});

    $clone_args{chronicle_reader} = $self->chronicle_reader;
    $clone_args{chronicle_writer} = $self->chronicle_writer;

    return $self->meta->name->new(\%clone_args);
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;
