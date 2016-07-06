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

use Number::Closest::XS qw(find_closest_numbers_around);
use List::Util qw(min);
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

=head2 variance_table

A variance surface. Converted from raw volatility input surface.

=cut

has variance_table => (
    is         => 'ro',
    lazy_build => 1,
);

sub _build_variance_table {
    my $self = shift;

    my $raw_surface    = $self->surface;
    my $effective_date = $self->effective_date;
    # New York 10:00
    my $ny_offset_from_gmt     = $effective_date->timezone_offset('America/New_York')->hours;
    my $seconds_after_midnight = $effective_date->plus_time_interval(10 - $ny_offset_from_gmt . 'h')->seconds_after_midnight;

    # keys are tenor in epoch, values are associated variances.
    my %table = (
        $self->recorded_date->epoch => {
            10 => 0,
            25 => 0,
            50 => 0,
            75 => 0,
            90 => 0,
        });
    foreach my $tenor (@{$self->original_term_for_smile}) {
        my $epoch = $effective_date->plus_time_interval($tenor . 'd' . $seconds_after_midnight . 's')->epoch;
        foreach my $delta (@{$self->deltas}) {
            my $volatility = $raw_surface->{$tenor}{smile}{$delta};
            $table{$epoch}{$delta} = $volatility**2 * $tenor if $volatility;
        }
    }

    return \%table;
}

=head2 effective_date

Surfaces roll over at 5pm NY time, so the vols of any surfaces recorded after 5pm NY but
before GMT midnight are effectively for the next GMT day. This attribute holds this
effective date.

=cut

has effective_date => (
    is         => 'ro',
    isa        => 'Date::Utility',
    init_arg   => undef,
    lazy_build => 1,
);

sub _build_effective_date {
    my $self = shift;

    return $self->_vol_utils->effective_date_for($self->recorded_date);
}

=head2 surface

Volatility surface in a hash reference.

=cut

has surface => (
    is         => 'ro',
    lazy_build => 1,
);

sub _build_surface {
    my $self = shift;

    # for backward compatibility
    return $self->document->{surface} // $self->document->{surfaces}{'New York 10:00'} // {};
}

=head2 get_volatility

Expects 3 mandatory arguments as input.

1) from - Date::Utility object
2) to - Date::Utility object
3) delta | strike | moneyness.

Calculates volatility from the surface based input parameters.

USAGE:

  my $from = Date::Utility->new('2016-06-01 10:00:00');
  my $to   = Date::Utility->new('2016-06-01 15:00:00');
  my $vol  = $s->get_volatility({delta => 25, from => $from, to => $to});
  my $vol  = $s->get_volatility({strike => $bet->barrier, from => $from, to => $to});
  my $vol  = $s->get_volatility({delta => 50, from => $from, to => $to});

=cut

sub get_volatility {
    my ($self, $args) = @_;

    # args validity checks
    die("Must pass exactly one of delta, strike or moneyness to get_volatility.")
        if (scalar(grep { defined $args->{$_} } qw(delta strike moneyness)) != 1);

    die "Must pass two dates [from, to] to get volatility." if (not($args->{from} and $args->{to}));

    die 'Inverted dates[from=' . $args->{from}->datetime . ' to= ' . $args->{to}->datetime . '] to get volatility.'
        if $args->{from}->epoch > $args->{to}->epoch;

    # This sanity check prevents negative variance
    die 'Requested dates are too far in the past ['
        . $args->{from}->datetime
        . '] with surface recorded date ['
        . $self->recorded_date->datetime . ']'
        if $args->{from}->epoch < $self->recorded_date->epoch;

    my $delta =
          (defined $args->{delta})  ? $args->{delta}
        : (defined $args->{strike}) ? $self->_convert_strike_to_delta($args)
        :                             $self->_convert_moneyness_to_delta($args);

    die 'Delta is negative.' if $delta < 0;

    my $smile = $self->get_smile($args->{from}, $args->{to});

    return $smile->{$delta} if $smile->{$delta};

    return $self->interpolate({
        smile        => $smile,
        sought_point => $delta,
    });
}

=head2 get_smile

Calculate the requested smile from volatility surface.

=cut

sub get_smile {
    my ($self, $from, $to) = @_;

    # each smile is calculated on the fly.
    my $number_of_days = ($to->epoch - $from->epoch) / 86400;
    my $variances_from = $self->get_variances($from);
    my $variances_to   = $self->get_variances($to);
    my $smile;

    foreach my $delta (@{$self->deltas}) {
        $smile->{$delta} = sqrt(($variances_to->{$delta} - $variances_from->{$delta}) / $number_of_days);
    }

    return $smile;
}

=head2 get_variances

Calculate the variance for a given date based on volatility surface data.

=cut

sub get_variances {
    my ($self, $date) = @_;

    my $epoch = $date->epoch;
    my $table = $self->variance_table;

    return $table->{$epoch} if $table->{$epoch};

    my @available_tenors = sort { $a <=> $b } keys %{$table};
    my @closest = map { Date::Utility->new($_) } @{find_closest_numbers_around($date->epoch, \@available_tenors, 2)};
    my $weight = $self->get_weight($closest[0], $date);
    my $weight2 = $weight + $self->get_weight($date, $closest[1]);

    my %variances;
    foreach my $delta (@{$self->deltas}) {
        my $var1 = $table->{$closest[0]->epoch}{$delta};
        my $var2 = $table->{$closest[1]->epoch}{$delta};
        $variances{$delta} = $var1 + ($var2 - $var1) / $weight2 * $weight;
    }

    return \%variances;
}

=head2 get_weight

Get the weight between to given dates.

=cut

sub get_weight {
    my ($self, $date1, $date2) = @_;

    my ($date1_epoch, $date2_epoch) = ($date1->epoch, $date2->epoch);
    my $time_diff       = $date2_epoch - $date1_epoch;
    my $weight_interval = 4 * 3600;

    my $remainder     = $date1_epoch % $weight_interval;
    my $next_interval = $date1_epoch - $remainder + $weight_interval;

    my @dates = ($date1_epoch, $next_interval);
    if ($time_diff <= $weight_interval and $next_interval != $date2_epoch) {
        push @dates, $date2_epoch;
    } else {
        my $start = $next_interval;
        while ($start < $date2_epoch) {
            my $to_add = min($date2_epoch - $start, $weight_interval);
            $start += $to_add;
            push @dates, $start;
        }
    }

    my $calendar     = $self->builder->build_trading_calendar;
    my $total_weight = 0;
    for (my $i = 0; $i < $#dates; $i++) {
        my $dt = $dates[$i + 1] - $dates[$i];
        $total_weight += $calendar->weight_on(Date::Utility->new($dates[$i])) * $dt / 86400;
    }

    return $total_weight;
}

=head2 interpolate

Quadratic interpolation to interpolate across smile
->interpolate({smile => $smile, sought_point => $sought_point});

=cut

sub interpolate {
    my ($self, $args) = @_;

    return Math::Function::Interpolator->new(points => $args->{smile})->quadratic($args->{sought_point});
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

    $clone_args->{recorded_date} = $self->recorded_date         if (not exists $clone_args->{recorded_date});
    $clone_args->{original_term} = dclone($self->original_term) if (not exists $clone_args->{original_term});
    $clone_args->{chronicle_reader} = $self->chronicle_reader;
    $clone_args->{chronicle_writer} = $self->chronicle_writer;

    return $self->meta->name->new($clone_args);
}

# PRIVATE #

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

    $new_args{t} ||= ($args->{to}->epoch - $args->{from}->epoch) / (365 * 86400);
    $new_args{spot}             ||= $underlying_config->spot;
    $new_args{premium_adjusted} ||= $underlying_config->{market_convention}->{delta_premium_adjusted};
    $new_args{r_rate}           ||= $self->builder->build_interest_rate->interest_rate_for($new_args{t});
    $new_args{q_rate}           ||= $self->builder->build_dividend->dividend_rate_for($new_args{t});

    $new_args{atm_vol} ||= $self->get_volatility({
        delta => 50,
        from  => $args->{from},
        to    => $args->{to},
    });

    return \%new_args;
}

no Moose;
__PACKAGE__->meta->make_immutable;

1;
