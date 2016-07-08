package Quant::Framework::VolSurface;

=head1 NAME

Quant::Framework::VolSurface

=head1 DESCRIPTION

Base class for all volatility surfaces.

=cut

use Moose;
use Try::Tiny;
use DateTime::TimeZone;
use List::MoreUtils qw(notall none all);
use Scalar::Util qw( looks_like_number weaken );
use Scalar::Util::Numeric qw( isnum );
use List::Util qw( min max first );
use Number::Closest::XS qw(find_closest_numbers_around);
use Math::Function::Interpolator;

use Quant::Framework::Utils::Types;
use Quant::Framework::VolSurface::Validator;
use Quant::Framework::VolSurface::Utils;
use Quant::Framework::Utils::Builder;

=head2 for_date

The date for which we want to have the volatility surface data

=cut

has for_date => (
    is      => 'ro',
    isa     => 'Maybe[Date::Utility]',
    default => undef,
);

has chronicle_reader => (
    is  => 'ro',
    isa => 'Data::Chronicle::Reader',
);

has chronicle_writer => (
    is  => 'ro',
    isa => 'Data::Chronicle::Writer',
);

has underlying_config => (
    is       => 'ro',
    required => 1,
);

has document => (
    is         => 'rw',
    lazy_build => 1,
);

sub _build_document {
    my $self = shift;

    return $self->chronicle_reader->get_for('volatility_surfaces', $self->symbol, $self->for_date->epoch) // {} if $self->for_date;
    return $self->chronicle_reader->get('volatility_surfaces', $self->symbol) // {};
}

has calendar => (
    is         => 'ro',
    isa        => 'Quant::Framework::TradingCalendar',
    lazy_build => 1,
);

sub _build_calendar {
    my $self = shift;

    return $self->builder->build_trading_calendar;
}

has builder => (
    is         => 'ro',
    isa        => 'Quant::Framework::Utils::Builder',
    lazy_build => 1,
);

sub _build_builder {
    my $self = shift;

    return Quant::Framework::Utils::Builder->new({
        for_date          => $self->for_date,
        chronicle_reader  => $self->chronicle_reader,
        chronicle_writer  => $self->chronicle_writer,
        underlying_config => $self->underlying_config,
    });
}

=head1 ATTRIBUTES

=head2 symbol

The symbol of the underlying that this surface is for (e.g. frxUSDJPY)

=cut

has symbol => (
    is       => 'ro',
    isa      => 'Str',
    required => 1,
);

has _market_name => (
    is         => 'ro',
    isa        => 'Str',
    lazy_build => 1,
);

sub _build__market_name {
    return shift->underlying_config->market_name;
}

=head2 recorded_date

The date (and time) that the surface was recorded, as a Date::Utility.

=cut

has recorded_date => (
    is         => 'ro',
    isa        => 'Date::Utility',
    lazy_build => 1,
);

sub _build_recorded_date {
    my $self          = shift;
    my $recorded_date = Date::Utility->new($self->document->{date});

    #for a flat volatility surface, we assume it is always fresh with date equal to now or the date for which
    #we want to fetch the surface (because it never changes)
    if ($self->type eq 'flat') {
        $recorded_date = $self->for_date // Date::Utility->new;
    }

    return $recorded_date;
}

=head2 type

Type of the surface, delta, moneyness or flat.

=cut

has type => (
    is       => 'ro',
    isa      => 'qf_surface_type',
    required => 1,
    init_arg => undef,
    default  => undef,
);

=head2 smile_points

The points across a smile.

It can be delta points, moneyness points or any other points that we might have in the future.

=cut

has smile_points => (
    is         => 'ro',
    isa        => 'ArrayRef',
    lazy_build => 1,
);

sub _build_smile_points {
    my $self = shift;

    # Default to the point found in the first day we find
    # in $self->surface that has a smile. As long as each smile
    # has the same points, this works. If each smile has different
    # points, the Validator is going to give you trouble!
    my $surface = $self->surface;

    my $suitable_day = first { exists $surface->{$_}->{smile} } @{$self->term_by_day};
    my @smile_points = ();
    if ($suitable_day) {
        @smile_points =
            sort { $a <=> $b } keys %{$surface->{$suitable_day}->{smile}};
    }

    return \@smile_points;
}

=head2 spread_points

This will give an array-reference containing volatility spreads for first tenor which has a volatility spread (or ATM if none).
=cut

has spread_points => (
    is         => 'ro',
    isa        => 'ArrayRef',
    lazy_build => 1,
);

sub _build_spread_points {
    my $self = shift;

    # Default to the point found in the first day we find
    # in $self->surface that has a volspread. As long as each volspread
    # has the same points, this works. If each smile has different
    # points, the Validator is going to give you trouble!
    my $surface = $self->surface;
    my $suitable_day = first { exists $surface->{$_}->{vol_spread} } keys %{$surface};

    return [sort { $a <=> $b } keys %{$surface->{$suitable_day}{vol_spread}}] if $suitable_day;
    return [];
}

=head2 term_by_day

Get all the terms in a surface in ascending order.

=cut

has term_by_day => (
    is         => 'ro',
    isa        => 'ArrayRef',
    init_arg   => undef,
    lazy_build => 1,
);

sub _build_term_by_day {
    my $self = shift;

    my @days = sort { $a <=> $b } keys %{$self->surface};

    return \@days;
}

=head2 original_term

The terms we were originally supplied for this surface.

=cut

has original_term => (
    is         => 'ro',
    isa        => 'HashRef',
    lazy_build => 1,
);

sub _build_original_term {
    my $self = shift;

    my $surface     = $self->surface;
    my @days        = @{$self->term_by_day};
    my @vol_spreads = grep { exists $surface->{$_}{vol_spread} } @days;
    my @smiles      = grep { exists $surface->{$_}{smile} } @days;

# We must have at least 2 atm_spreads if this is a valid surface with multiple smiles.
# Set the end points to the default if they are unset to make up any difference.
    if (scalar @vol_spreads < min(2, scalar @smiles)) {
        foreach my $end_index (0, -1) {
            my $day = $days[$end_index];
            next if exists $surface->{$day}{vol_spread};
            $surface->{$day}{vol_spread} = {$self->atm_spread_point => 0.1};
            push @vol_spreads, $day;
        }
    }

    return +{
        vol_spread => \@vol_spreads,
        smile      => \@smiles,
    };

}

=head2 original_term_for_smile

The original term structure we have for smiles on a surface

=cut

has original_term_for_smile => (
    is         => 'ro',
    isa        => 'ArrayRef',
    lazy_build => 1,
);

sub _build_original_term_for_smile {
    my $self = shift;

    return [sort { $a <=> $b } @{$self->original_term->{smile}}];
}

=head2 original_term_for_spread

The original term structure we have for spreads on a surface

=cut

has original_term_for_spread => (
    is         => 'ro',
    isa        => 'ArrayRef',
    lazy_build => 1,
);

sub _build_original_term_for_spread {
    my $self = shift;

    return [sort { $a <=> $b } @{$self->original_term->{vol_spread}}];
}

# PRIVATE ATTRIBUTES:

#A flag which determines whether this surface is a newly created surface or a one which is read from historical data.
has _new_surface => (
    is      => 'ro',
    default => 0,
);

has _vol_utils => (
    is         => 'ro',
    isa        => 'Quant::Framework::VolSurface::Utils',
    init_arg   => undef,
    lazy_build => 1,
);

sub _build__vol_utils {
    return Quant::Framework::VolSurface::Utils->new;
}

has _vol_surface_validator => (
    is         => 'ro',
    isa        => 'Quant::Framework::VolSurface::Validator',
    init_arg   => undef,
    lazy_build => 1,
);

sub _build__vol_surface_validator {
    return Quant::Framework::VolSurface::Validator->new;
}

has _ON_day => (
    is         => 'ro',
    isa        => 'Int',
    lazy_build => 1,
);

#Returns the day for over-night tenor of this surface
#ON = over-night
sub _build__ON_day {
    my $self = shift;

    return $self->calendar->calendar_days_to_trade_date_after($self->effective_date);
}

around BUILDARGS => sub {
    my $orig  = shift;
    my $class = shift;
    my %args  = (ref $_[0] eq 'HASH') ? %{$_[0]} : @_;
    my %day_for_tenor;

    die "Chronicle reader is required to create a vol-surface" if not defined $args{chronicle_reader};
    die "Attribute underlying_config is required"              if not defined $args{underlying_config};

    my $underlying_config = $args{underlying_config};
    if (ref $underlying_config
        and $underlying_config->isa('Quant::Framework::Utils::UnderlyingConfig'))
    {
        $args{symbol} = $underlying_config->system_symbol;
    }

    if ($args{surface} or $args{recorded_date}) {

        if (not $args{surface} or not $args{recorded_date}) {
            die('Must pass both "surface" and "recorded_date" if passing either.');
        }

        $args{_new_surface} = 1;
        my $builder = Quant::Framework::Utils::Builder->new({
            for_date          => $args{for_date},
            chronicle_reader  => $args{chronicle_reader},
            chronicle_writer  => $args{chronicle_writer},
            underlying_config => $args{underlying_config},
        });

        my $expiry_conventions = $builder->build_expiry_conventions;

        # If the smile's day is given as a tenor, we convert
        # it to a day and add the tenor to the smile:
        foreach my $maturity (keys %{$args{surface}}) {
            my $effective_date;

            if (_is_tenor($maturity)) {
                $effective_date ||= Quant::Framework::VolSurface::Utils->new->effective_date_for($args{recorded_date});

                my $vol_expiry_date = $expiry_conventions->vol_expiry_date({
                    from => $effective_date,
                    term => $maturity
                });
                my $day = $vol_expiry_date->days_between($effective_date);

                $args{surface}->{$day} = $args{surface}->{$maturity};
                $args{surface}->{$day}->{tenor} = $maturity;
                delete $args{surface}->{$maturity};

                $day_for_tenor{$maturity} = $day;
            }
        }
    }

    if (ref $args{original_term}) {
        if (
            not exists $args{original_term}->{smile}
            and (
                not(   exists $args{original_term}->{vol_spread}
                    or exists $args{original_term}->{atm_spread})))
        {

            die('Given original_term attr must have both smile and vol_spread keys present.');
        }
        foreach my $which (qw( smile vol_spread )) {
            if (exists $args{original_term}->{$which}) {
                $args{original_term}->{$which} = [
                    sort { $a <=> $b }
                    map { _is_tenor($_) ? $day_for_tenor{$_} : $_ } @{$args{original_term}->{$which}}];
            }
        }
    }
    return $class->$orig(%args);
};

sub _is_tenor {
    my $day = shift;

    return ($day =~ /^(?:ON|\d{1,2}[WMY])$/) ? 1 : 0;
}

=head2 get_spread

Spread is ask-bid difference which can be stored for different tenor and atm/max.

USAGE:

    my $spread = $volsurface->get_spread({sought_point=> 'atm', day=> 7});
    will return the atm spread for the day.

   my $spread = $volsurface->get_spread({sought_point=> 'max', day=> 7});
    will return the max spread for the day.

=cut

sub get_spread {
    my ($self, $args) = @_;

    my $sought_point = $args->{sought_point};
    my $day          = $args->{day};
    $day = $self->get_day_for_tenor($day)
        if ($day =~ /^(?:ON|\d[WMY])$/);    # if day looks like tenor

    return $self->_get_atm_spread($day)              if $sought_point eq 'atm';
    return $self->_get_max_spread_from_surface($day) if $sought_point eq 'max';
    die 'Unrecognized spread type ' . $sought_point;
}

sub _get_atm_spread {
    my ($self, $day) = @_;

    my $surface          = $self->surface;
    my $atm_spread_point = $self->atm_spread_point;

    if (exists $surface->{$day}) {
        return $surface->{$day}{vol_spread}{$atm_spread_point};
    }

    my $smile_spread = $self->get_smile_spread($day);
    return $smile_spread->{$atm_spread_point};
}

sub _get_max_spread_from_surface {
    my ($self, $day) = @_;

    my $surface = $self->surface;
    if (exists $surface->{$day}) {
        return max(values %{$surface->{$day}{vol_spread}});
    }

    my $smile_spread = $self->get_smile_spread($day);
    return max(values %$smile_spread);
}

=head2 get_smile_spread

Returns the ask/bid spread for smile of this volatility surface

=cut

sub get_smile_spread {
    my ($self, $day) = @_;

    my $surface = $self->surface;
    # if a surface has a minimum volatility spread that needs to be applied,
    # we will do the check.
    if ($self->can('min_vol_spread')) {
        my %market_points = map { $_ => 1 } @{$self->original_term_for_spread};
        foreach my $day (keys %{$surface}) {
            #check and add min_vol_spread for shorter term vol_spreads
            next if (not $market_points{$day} or $day >= 30);
            foreach my $point (keys %{$surface->{$day}{vol_spread}}) {
                $surface->{$day}{vol_spread}{$point} = max($surface->{$day}{vol_spread}{$point}, $self->min_vol_spread);
            }
        }
    }

    return $surface->{$day}{vol_spread} if (exists $surface->{$day});
    return $self->_compute_and_set_smile_spread($day);
}

sub _compute_and_set_smile_spread {
    my ($self, $day) = @_;

    my $surface      = $self->surface;
    my $spread_terms = $self->original_term_for_spread;
    my @points       = $self->_get_points_to_interpolate($day, $spread_terms);

    if (not $self->_is_between($day, \@points)) {
        my $index = $day > $spread_terms->[-1] ? $spread_terms->[-1] : $spread_terms->[0];
        return $surface->{$index}{vol_spread};
    }

    my %smile_spread = map {
        $_ => Math::Function::Interpolator->new(
            points => {
                $points[0] => $surface->{$points[0]}->{vol_spread}->{$_},
                $points[1] => $surface->{$points[1]}->{vol_spread}->{$_},
            }
            )->linear($day)
    } @{$self->spread_points};

    $self->surface->{$day} = \%smile_spread;

    return \%smile_spread;
}

=head2 get_day_for_tenor

USAGE:

    my $day = $volsurface->get_day_for_tenor('1W');

Get the corresponding day for a given tenor, if one exists.

=cut

sub get_day_for_tenor {
    my ($self, $tenor) = @_;

    my $surface_data = $self->surface;
    my $day          = first {
        $surface_data->{$_}->{tenor} && $surface_data->{$_}->{tenor} eq $tenor;
    }
    @{$self->term_by_day};

    # If tenor doesn't already exist on surface, get $days via the vol expiry date.
    if (not $day) {
        $day = $self->builder->build_expiry_conventions->vol_expiry_date({
                from => $self->effective_date,
                term => $tenor,
            })->days_between($self->effective_date);
    }

    return $day;
}

sub _get_points_to_interpolate {
    my ($self, $seek, $available_points) = @_;
    die('Need 2 or more term structures to interpolate.')
        if scalar @$available_points <= 1;

    return @{find_closest_numbers_around($seek, $available_points, 2)};
}

sub _is_between {
    my ($self, $seek, $points) = @_;

    my @points = @$points;

    die('some of the points are not defined')
        if (notall { defined $_ } @points);
    die('less than two available points')
        if (scalar @points < 2);

    return if (all { $_ < $seek } @points);
    return if (all { $_ > $seek } @points);
    return 1;
}

sub _market_maturities_interpolation_function {
    my ($self, $T, $T1, $T2) = @_;

    # Implements interpolation over time based on the way Iain M Clark does
    # it in Foreign Exchange Option Pricing, A Practitioner's Guide, page 70.
    my $effective_date = $self->effective_date;

    my %dates = (
        T  => $effective_date->plus_time_interval($T - 1 . 'd23h59m59s'),
        T1 => $effective_date->plus_time_interval($T1 - 1 . 'd23h59m59s'),
        T2 => $effective_date->plus_time_interval($T2 - 1 . 'd23h59m59s'),
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

=head1 METHODS

=head2 get_volatility

USAGE:

    my $vol = $vol_surface->get_volatility({
                   days  => 7,
                   delta => 50,
               });

Interpolates (on the two-dimensional vol surface) using Bloomberg's method.

=cut

sub get_volatility {
    my ($self, $args) = @_;

    my $sought_point = $args->{sought_point};
    my $days         = $args->{days};

    $self->_validate_sought_values($days, $sought_point);

    my $vol = $self->_get_volatility_from_surface({
        days         => $days,
        sought_point => $sought_point,
    });

    return $vol;
}

sub _convert_expiry_to_day {
    my ($self, $args) = @_;

    return $args->{days} if $args->{days};
    my $days =
          $args->{expiry_date}
        ? $self->_get_days_for_expiry_date($args->{expiry_date})
        : $self->get_day_for_tenor($args->{tenor});
    delete $args->{expiry_date};
    delete $args->{tenor};
    return $days;
}

sub _validate_sought_values {
    my ($self, $day, $sought_point) = @_;

    if (notall { defined $_ } ($sought_point, $day)) {
        $sought_point ||= '';
        $day          ||= '';
        die("Days[$day] or sought_point[$sought_point] is undefined for underlying[" . $self->symbol . "]");
    }

    if (!isnum($sought_point)) {
        die("Point[$sought_point] must be a number");
    }

    if ($day <= 0 or $sought_point < 0) {
        die("get_volatility requires positive numeric days[$day] and sought_point[$sought_point]");
    }

    return 1;
}

sub _get_volatility_from_surface {
    my ($self, $args) = @_;

    my $day          = $args->{days};
    my $sought_point = $args->{sought_point};
    my $vol;

    my $smile = $self->get_smile($day);
    if ($smile->{$sought_point}) {
        $vol = $smile->{$sought_point};
    } else {
        $vol = $self->interpolate({
            smile        => $smile,
            sought_point => $sought_point,
        });
    }

    return $vol;
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
    my $smile =
        (exists $surface->{$day} and exists $surface->{$day}->{smile})
        ? $surface->{$day}->{smile}
        : $self->_compute_and_set_smile($day);

    return $smile;
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

sub _days_with_smiles {
    my $self = shift;
    my @days_with_smiles =
        grep { exists $self->surface->{$_}->{smile} } @{$self->term_by_day};
    return \@days_with_smiles;
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
        $interpolated_smile->{smile}->{$smile_point} = $interpolate_func->($first_iv, $second_iv);
    }

    return $interpolated_smile->{smile};
}

sub _extrapolate_smile {
    my ($self, $seek, $points) = @_;

    my $extrapolation_direction = $self->_get_extrapolate_direction($seek, $points);
    my $extrapolation_method = '_extrapolate_smile_' . $extrapolation_direction;

    return $self->$extrapolation_method($seek);
}

#returns initial risk reversal as a hash-ref
sub _get_initial_rr {
    my ($self, $market) = @_;

    my %initial_rr;
    my $rr_adjustment;
    if ($self->_market_name ne 'indices') {
        $rr_adjustment = {
            rr_25 => 0.1,
            rr_10 => 0.1
        };
        $initial_rr{RR_25} = $rr_adjustment->{rr_25} * $market->{RR_25};
        $initial_rr{RR_10} = $rr_adjustment->{rr_10} * $market->{RR_10} if (exists $market->{RR_10});
    }

    return \%initial_rr;
}

sub _extrapolate_smile_up {
    my $self = shift;

# This is not an ideal solution and duration of contracts is not supposed to be more than 1 year
# But, you'll get a 1 year smile if the unexpected happens :-P
    my $last_term = $self->original_term_for_smile->[-1];
    return $self->surface->{$last_term}->{smile};
}

sub _get_extrapolate_direction {
    my ($self, $seek, $points) = @_;

    my @points = @$points;
    my $direction = ($seek > max(@points)) ? 'up' : 'down';

    return $direction;
}

sub _get_days_for_expiry_date {
    my ($self, $expiry_date) = @_;

# In general, days_between the effective_date of the surface and the asked-for
# expiry_date gives us what we want.
#
# However, due to vol-cuts logic cutting to wherever the cut time falls within the
# effective day of the vol, and due to FX end-of-day being at GMT23:59, we
# cut the vol back into the previous GMT day, e.g. we cut the NY1000 vol for contracts
# expiring on the 24th back to GMT23:59 on the 23rd. This means that if we ask
# for a vol for the 23rd, and the underling closes after NY1700, we need to use the
# vol for the next day. We do this by considering days between the effective day of
# the recorded date and the expiry.
# This can potentially cause problems if the effective date (surface recorded date) is too old
    my $days = $self->_vol_utils->effective_date_for($expiry_date)->days_between($self->effective_date);

# If we asked for the vol for an expiry date (which translated to an integer number
# of days), default to the 1-day vol if
    return max($days, 1);
}

=head2 set_smile

Set a smile into the volatility surface
Input: day and smile (key: smile_point ; value: volatility)

Usage:

    $vol_surface->set_smile($day, \%smile);

=cut

sub set_smile {
    my ($self, $args) = @_;

    my $day = $args->{days};
    die("day[$day] must be a number.") if $day <= 0;

    my $surface = $self->surface;
    my $smile   = $args->{smile};

    # throws exception on error.
    $self->_vol_surface_validator->check_smile($day, $smile, $self->symbol);

    $surface->{$day}->{smile}      = $smile;
    $surface->{$day}->{vol_spread} = $self->get_smile_spread($day);

    # We just changed the surface, so clear the caches.
    $self->clear_term_by_day;

    return;
}

=head2 get_rr_bf_for_smile

Return the rr and bf values for a given smile
For more info see: https://en.wikipedia.org/wiki/Risk_reversal and https://en.wikipedia.org/wiki/Butterfly_(options)

=cut

sub get_rr_bf_for_smile {
    my ($self, $market_smile) = @_;

    my $result = {
        ATM   => $market_smile->{50},
        RR_25 => $market_smile->{25} - $market_smile->{75},
        BF_25 => ($market_smile->{25} + $market_smile->{75}) / 2 - $market_smile->{50},
    };
    if (exists $market_smile->{10}) {
        $result->{RR_10} = $market_smile->{10} - $market_smile->{90};
        $result->{BF_10} = ($market_smile->{10} + $market_smile->{90}) / 2 - $market_smile->{50};
    }
    return $result;
}

=head2 get_market_rr_bf

Returns the rr and bf values for a given day

=cut

sub get_market_rr_bf {
    my ($self, $from, $to) = @_;

    my %smile = %{$self->get_smile($from, $to)};

    return $self->get_rr_bf_for_smile(\%smile);
}

=head2 set_smile_flag

Sets a flag to a smile.

It is an ArrayRef of smile flag messages.
If a flag is set we will not allow clients to buy new contracts.

Usage:

    $vol_surface->set_smile_flag($time_days, $flag)

=cut

sub set_smile_flag {
    my ($self, $day, $flag_message) = @_;

    $day = $self->_ON_day if ($day eq 'ON');

    my $flag_array_ref = $self->get_smile_flag($day);
    if (not defined $flag_array_ref) {
        $flag_array_ref = [];
        $self->surface->{$day}->{flag} = $flag_array_ref;
    }

    push @{$flag_array_ref}, $flag_message;

    return 1;
}

=head2 get_smile_flag

Get the flag (as an array-ref) for a specific smile.

A flag means there is an error/irregularity of the smile/quote too old/ etc.

Usage:

    my $smile_flag = $vol_surface->get_smile_flag($day)

=cut

sub get_smile_flag {
    my ($self, $day) = @_;

    my $daily_vol_info = $self->surface->{$day};

    return $daily_vol_info->{flag};
}

=head2 get_smile_flags

Get all flags of the surface. Takes no args.
Return value is a string in the format "day1:flag1;day2:flag2;day3:flag3..." 

=cut

sub get_smile_flags {
    my ($self) = @_;

    my @days = @{$self->term_by_day};
    my @flag_strs;

    foreach my $day (@days) {
        if (my $flag = $self->get_smile_flag($day)) {
            push @flag_strs, join '', ($day, ': ', @{$flag});
        }
    }

    return join '; ', @flag_strs;
}

=head2 fetch_historical_surface_date

Get historical vol surface dates going back a given number of historical revisions.

=cut

sub fetch_historical_surface_date {
    my ($self, $args) = @_;
    my $back_to = $args->{back_to} || 1;

    my $vdoc = $self->chronicle_reader->get('volatility_surfaces', $self->symbol);
    my $current_date = $vdoc->{date};

    my @dates;
    push @dates, $current_date;

    for (2 .. $back_to) {
        $vdoc = $self->chronicle_reader->get_for('volatility_surfaces', $self->symbol, Date::Utility->new($current_date)->epoch - 1);

        last if not $vdoc or not %{$vdoc};

        $current_date = $vdoc->{date};

        push @dates, $current_date;
    }

    return \@dates;
}

=head2 is_valid

Validates this volatility surface and returns possible errors (or empty if surface is valid).

=cut

sub is_valid {
    my $self = shift;

    # An old/saved surface is always valid.
    # It has to be validated before it gets saved.
    return 1 if not $self->_new_surface;

    my $err;

    # This should not die.
    try {
        Quant::Framework::VolSurface::Validator->new->validate_surface($self);
        $err = 'Surface has smile flags: ' . $self->get_smile_flags
            if $self->get_smile_flags;
    }
    catch {
        $err = 'Die while being validated with error: ' . $_;
    };

    # Updates validation error.
    $self->validation_error($err) if $err;

    return !$err;
}

has validation_error => (
    is      => 'rw',
    default => '',
);

=head2 get_existing_surface

Returns original surface and not the cut surface

=cut

sub get_existing_surface {
    my $self = shift;

    # existing surface will return you the original surface and not the cut surface
    return $self->_new_surface
        ? $self->new({
            underlying_config => $self->underlying_config,
            chronicle_reader  => $self->chronicle_reader,
            chronicle_writer  => $self->chronicle_writer,
        })
        : $self;
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

no Moose;
__PACKAGE__->meta->make_immutable;
1;
