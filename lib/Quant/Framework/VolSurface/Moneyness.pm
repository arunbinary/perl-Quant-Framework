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
use Scalar::Util qw( looks_like_number );
use Math::Function::Interpolator;
use List::MoreUtils qw(indexes);
use List::Util qw(min max);

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

=head2 surface_data

The original surface data.

=cut

has surface_data => (
    is         => 'ro',
    lazy_build => 1,
);

sub _build_surface_data {
    my $self = shift;

    # For backward compatibility
    my $date = $self->for_date // Date::Utility->new;
    my $surface = $self->document->{surface} // $self->document->{surfaces}{'UTC ' . $self->calendar->standard_closing_on($date)->time_hhmm} // {};

    return $self->_clean($surface);
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

    $args->{days} = ($args->{to}->epoch - $args->{from}->epoch) / 86400;
    delete $args->{to};
    delete $args->{from};

    die "Argument 'days' must be positive, non-zero number." if $args->{days} <= 0;

    # we are handling delta seperately because it involves
    # a lot more steps to calculate vol for a delta point
    # on a moneyness surface
    return $self->_calculate_vol_for_delta($args) if $args->{delta};

    my $moneyness =
          $args->{strike}
        ? $args->{strike} / $self->spot_reference * 100
        : $args->{moneyness};

    die "Sought point must be a number." if not looks_like_number($moneyness);
    die "Sought point must be a positive number." if $moneyness < 0;

    my $smile = $self->get_smile($args->{days});

    return $smile->{$moneyness} if $smile->{$moneyness};

    return $self->interpolate({
        smile        => $smile,
        sought_point => $moneyness,
    });
}

=head2 get_smile

Get the smile for specific day.

Usage:

    my $smile = $vol_surface->get_smile($days);

=cut

sub get_smile {
    my ($self, $day) = @_;

    die("day[$day] must be a number.")
        unless looks_like_number($day);

    my $surface = $self->surface;

    return $surface->{$day}{smile} if (exists $surface->{$day} and exists $surface->{$day}{smile});
    return $self->_compute_and_set_smile($day);
}

sub _compute_and_set_smile {
    my ($self, $day) = @_;

    my @points = $self->_get_points_to_interpolate($day, $self->original_term_for_smile);
    my $method =
        ($self->_is_between($day, \@points))
        ? '_interpolate_smile'
        : '_extrapolate_smile';
    my $smile = $self->$method($day, \@points);

    $self->set_smile({
        days  => $day,
        smile => $smile
    });

    return $smile;
}

sub _interpolate_smile {
    my ($self, $seek, $points) = @_;

    my $surface          = $self->surface;
    my $first_point      = $points->[0];
    my $second_point     = $points->[1];
    my $interpolate_func = $self->_market_maturities_interpolation_function($seek, $first_point, $second_point);
    my $interpolated_smile;

    foreach my $smile_point (@{$self->smile_points}) {
        my $first_iv  = $surface->{$first_point}->{smile}->{$smile_point};
        my $second_iv = $surface->{$second_point}->{smile}->{$smile_point};
        $interpolated_smile->{$smile_point} = $interpolate_func->($first_iv, $second_iv);
    }

    return $interpolated_smile;
}

sub _extrapolate_smile {
    my ($self, $seek, $points) = @_;

    my $index = $seek > max(@$points) ? -1 : 0;

    # we do not extrapolate for moneyness smiles. We will take the market points at both ends.
    return $self->surface->{$self->original_term_for_smile->[$index]}->{smile};
}

sub _market_maturities_interpolation_function {
    my ($self, $T, $T1, $T2) = @_;

    # Implements interpolation over time based on the way Iain M Clark does
    # it in Foreign Exchange Option Pricing, A Practitioner's Guide, page 70.
    my $effective_date = $self->effective_date;
    my $day_in_seconds = 86400;

    # Setting it to end of day. That's why we need to minus 1 second.
    my %dates = (
        T  => $effective_date->plus_time_interval($T * $day_in_seconds - 1),
        T1 => $effective_date->plus_time_interval($T1 * $day_in_seconds - 1),
        T2 => $effective_date->plus_time_interval($T2 * $day_in_seconds - 1),
    );

    my $tau1 = $self->builder->build_trading_calendar->weighted_days_in_period($dates{T1}, $dates{T}) / 365;
    my $tau2 = $self->builder->build_trading_calendar->weighted_days_in_period($dates{T1}, $dates{T2}) / 365;

    warn(     'Error in volsurface['
            . $self->recorded_date->datetime
            . '] for symbol['
            . $self->underlying_config->symbol
            . '] for maturity['
            . $T
            . '] points ['
            . $T1 . ','
            . $T2 . "]\n")
        unless $tau2;

    return sub {
        my ($vol1, $vol2) = @_;
        return sqrt(($tau1 / $tau2) * ($T2 / $T) * $vol2**2 + (($tau2 - $tau1) / $tau2) * ($T1 / $T) * $vol1**2);
    };
}

=head2 interpolate

This is how you could interpolate across smile.
This uses the default interpolation method of the surface.

    $surface->interpolate({smile => $smile, sought_point => $sought_point});
=cut

sub interpolate {
    my ($self, $args) = @_;

    my $method = keys %{$args->{smile}} < 5 ? 'quadratic' : 'cubic';

    return Math::Function::Interpolator->new(points => $args->{smile})->$method($args->{sought_point});
}

=head2 get_market_rr_bf

Returns the rr and bf values for a given day

=cut

sub get_market_rr_bf {
    my ($self, $day) = @_;

# rr and bf only make sense in delta term. Here we convert the smile to a delta smile.
    my %smile = map { $_ => $self->_calculate_vol_for_delta({delta => $_, days => $day}) } qw(25 50 75);

    return $self->get_rr_bf_for_smile(\%smile);
}

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

no Moose;
__PACKAGE__->meta->make_immutable;

1;
