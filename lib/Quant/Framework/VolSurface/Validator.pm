package Quant::Framework::VolSurface::Validator;

=head1 NAME

Quant::Framework::VolSurface::Validator

=head1 DESCRIPTION

Provides subroutines that can perform various
validation checks on Quant::Framework::VolSurface objects.

=head1 SYNOPSIS

    use Quant::Framework::VolSurface::Validator;

    my $validator = Quant::Framework::VolSurface::Validator->new;

    try {
        $validator->validate_surface($surface);
    }
    catch {
            # handle as necessary...
    };

=cut

use Moose;
use 5.010;

use Format::Util::Numbers qw(roundnear);
use List::Util qw( min reduce );
use Date::Utility;
use Math::Business::BlackScholes::Binaries;
use List::MoreUtils qw(indexes any);
use VolSurface::Utils qw( get_strike_for_spot_delta );

=head1 METHODS

=head2 validate_surface

This method validates a given surface.

For certain errors, exceptions will be thrown. For others,
smile_flags will be set on the surface.

=cut

sub validate_surface {
    my ($self, $surface) = @_;

    return 1 if ($surface->type eq 'flat');

    # Throws exceptions if unsuccessful
    $self->_do_raw_validation_on($surface);

    return 1;
}

# Performs all checks implemented in this package on the given surface.
sub _do_raw_validation_on {
    my ($self, $surface) = @_;

    _check_structure($surface);
    _check_identical_surface($surface);
    _check_volatility_jump($surface);
    _check_spot_reference($surface) if $surface->type eq 'moneyness';
    _check_age($surface);

    $self->check_smiles($surface);

    _check_termstructure_for_calendar_arbitrage($surface);
    _admissible_check($surface);

    return 1;
}

sub _check_identical_surface {
    my $surface = shift;

    my $existing       = $surface->get_existing_surface;
    my @points         = @{$existing->smile_points};
    my @existing_terms = @{$existing->original_term_for_smile};
    my @new_terms      = @{$surface->original_term_for_smile};

    # we return if it is not identical
    return 1 if $#existing_terms != $#new_terms;
    return 1 if grep { $existing_terms[$_] != $new_terms[$_] } (0 .. $#existing_terms);

    foreach my $term (@existing_terms) {
        foreach my $point (@points) {
            return 1 if $existing->surface->{$term}->{smile}->{$point} != $surface->surface->{$term}->{smile}->{$point};
        }
    }
    if (time - $existing->recorded_date->epoch > 15000 and not $surface->{underlying_config}->quanto_only) {
        die('Surface data has not changed since last update [' . $existing->recorded_date->epoch . '].');
    }
    return;
}

sub _check_volatility_jump {
    my $surface = shift;

    my $existing        = $surface->get_existing_surface;
    my @terms           = @{$surface->original_term_for_smile};
    my @new_expiry      = sort { $a <=> $b } keys %{$surface->variance_table};
    my @existing_expiry = sort { $a <=> $b } keys %{$existing->variance_table};

    my @points = @{$surface->smile_points};
    my $type   = $surface->type;

    for (my $i = 1; $i < $#new_expiry; $i++) {
        for (my $j = 0; $j < $#points; $j++) {
            my $sought_point = $points[$j];
            my $new_vol      = $surface->get_volatility({
                $type => $sought_point,
                from  => $surface->recorded_date,
                to    => Date::Utility->new($new_expiry[$i]),
            });
            my $existing_vol = $existing->get_volatility({
                $type => $sought_point,
                from  => $existing->recorded_date,
                to    => Date::Utility->new($existing_expiry[$i]),
            });
            my $diff            = abs($new_vol - $existing_vol);
            my $percentage_diff = $diff / $existing_vol * 100;
            if ($diff > 0.03 and $percentage_diff > 100) {
                die(      'Big difference found on term['
                        . $terms[$i - 1]
                        . '] for point ['
                        . $sought_point
                        . '] with absolute diff ['
                        . $diff
                        . '] percentage diff ['
                        . $percentage_diff
                        . ']');
            }
        }
    }

    return 1;
}

sub _admissible_check {
    my $surface = shift;

    my $underlying_config = $surface->underlying_config;
    my $builder           = $surface->builder;
    my $calendar          = $surface->calendar;
    my $surface_type      = $surface->type;
    my $S                 = ($surface_type eq 'delta') ? $underlying_config->spot : $surface->spot_reference;
    my $premium_adjusted  = $underlying_config->{market_convention}->{delta_premium_adjusted};
    my @expiries          = sort { $a <=> $b } keys %{$surface->variance_table};
    my @tenors            = @{$surface->original_term_for_smile};
    my $now               = Date::Utility->new;

    my $utils = Quant::Framework::VolSurface::Utils->new;
    for (my $i = 1; $i <= $#expiries; $i++) {
        my $day    = $tenors[$i - 1];
        my $expiry = Date::Utility->new($expiries[$i]);

        my $date_expiry = Date::Utility->new(time + $day * 86400);
        $date_expiry = $calendar->trades_on($date_expiry) ? $date_expiry : $calendar->trade_date_after($date_expiry);
        my $adjustment;
        if ($underlying_config->market_prefer_discrete_dividend) {
            $adjustment = $builder->build_dividend->dividend_adjustments_for_period({
                start => $now,
                end   => $date_expiry,
            });
            $S += $adjustment->{spot};
        }
        my $atid = $utils->effective_date_for($date_expiry)->days_between($utils->effective_date_for($now));
        # If intraday or not FX, then use the exact duration with fractions of a day.
        die("Invalid tenor[$atid] on surface") if ($atid == 0);
        my $t     = $atid / 365;
        my $r     = $builder->build_interest_rate->interest_rate_for($t);
        my $q     = ($underlying_config->market_prefer_discrete_dividend) ? 0 : $builder->build_dividend->dividend_rate_for($t);
        my $smile = $surface->surface->{$day}->{smile};

        my @volatility_level = sort { $a <=> $b } keys %{$smile};
        my $first_vol_level  = $volatility_level[0];
        my $last_vol_level   = $volatility_level[-1];

        my %prev;

        foreach my $vol_level (@volatility_level) {
            my $vol = $smile->{$vol_level};
            my $barrier;
            # Temporarily get the Call strike via the Put side of the algorithm,
            # as it seems not to go crazy at the extremities. Should give the same barrier.
            if ($surface_type eq 'delta') {
                my $conversion_args = {
                    atm_vol          => $vol,
                    t                => $t,
                    r_rate           => $r,
                    q_rate           => $q,
                    spot             => $S,
                    premium_adjusted => $premium_adjusted
                };

                if ($vol_level > 50) {
                    $conversion_args->{delta}       = exp(-$r * $t) - $vol_level / 100;
                    $conversion_args->{option_type} = 'VANILLA_PUT';
                } else {
                    $conversion_args->{delta}       = $vol_level / 100;
                    $conversion_args->{option_type} = 'VANILLA_CALL';
                }
                $barrier = get_strike_for_spot_delta($conversion_args);
            } elsif ($surface_type eq 'moneyness') {
                $barrier = $vol_level / 100 * $S;
            }

            if ($underlying_config->market_prefer_discrete_dividend) {

                $barrier += $adjustment->{barrier};
            }
            my $prob = Math::Business::BlackScholes::Binaries::vanilla_call($S, $barrier, $t, $r, $r - $q, $vol);
            my $slope;

            if (exists $prev{prob}) {
                $slope = ($prob - $prev{prob}) / ($vol_level - $prev{vol_level});

                # Admissible Check 1.
                # For delta surface, the strike(prob) is decreasing(increasing) across delta point, hence the slope is positive
                # For moneyness surface, the strike(prob) is increasing(decreasing) across moneyness point, hence the slope is negative
                if ($surface_type eq 'delta' and $slope <= 0) {
                    die("Admissible check 1 failure for maturity[$day]. BS digital call price decreases between $prev{vol_level} and " . $vol_level);
                }

                if ($surface_type eq 'moneyness' and $slope >= 0.0) {
                    die("Admissible check 1 failure for maturity[$day]. BS digital call price decreases between $prev{vol_level} and " . $vol_level);
                }
            }

            # Admissible Check 4.
            # The actual check is that when K = 0, BS call price = e^(-rt)S.
            # What we've implemented is largely equivalent, and easier to check:
            # digital call prob = 1 for very small K.
            $barrier = 0.00000001;
            my $bs_theo = Math::Business::BlackScholes::Binaries::call($S, $barrier, $t, $r, $r - $q, $vol);
            my $bs_theo_before_discounted = $bs_theo / (exp(-$r * $t));

            if ($bs_theo_before_discounted < 0.95) {
                die(
                    "Admissible check 4 failure for maturity[$day] vol at level [$vol_level]. BS digital call theo prob[$bs_theo] not 1 when barrier -> zero."
                );

            }

            # Admissible Check 5.
            # The call price of an infinity strike should be zero.
            $barrier = $S * 10000;
            my $call_prob_for_inf_strike = Math::Business::BlackScholes::Binaries::call($S, $barrier, $t, $r, $r - $q, $vol);
            if ($call_prob_for_inf_strike > 1e-10) {
                die(      "Admissible check 5 failure for maturity[$day] vol at level[$vol_level].BS digital call price not -> 0 ["
                        . $call_prob_for_inf_strike
                        . "] as barrier -> 'infinity'.");
            }

            %prev = (
                slope     => $slope,
                prob      => $prob,
                vol_level => $vol_level,
            );

            next
                if ($vol_level == $first_vol_level
                || $vol_level == $last_vol_level
                || $surface_type eq 'delta');

            $barrier = $vol_level / 100 * $S;
            my $h = (2.0 / 100) * $S;

            my %vol_period = (
                from => $surface->recorded_date,
                to   => $expiry,
            );
            $vol = $surface->get_volatility({
                moneyness => ($vol_level - 2),
                %vol_period,
            });
            my $bet_minus_h = Math::Business::BlackScholes::Binaries::vanilla_call($S, $barrier - $h, $t, $r, $r - $q, $vol);

            $vol = $surface->get_volatility({
                moneyness => ($vol_level),
                %vol_period,
            });
            my $bet = Math::Business::BlackScholes::Binaries::vanilla_call($S, $barrier, $t, $r, $r - $q, $vol);

            $vol = $surface->get_volatility({
                moneyness => ($vol_level + 2),
                %vol_period,
            });
            my $bet_plus_h = Math::Business::BlackScholes::Binaries::vanilla_call($S, $barrier + $h, $t, $r, $r - $q, $vol);

            ## The actual finite difference formula has a / $h**2, but since we're checking
            ## for negativity, we don't need it. Also, we introducted an error margin to allow
            ## surfaces that are close to passing through.
            my $convexity_flag = $bet_minus_h - 2 * $bet + $bet_plus_h;
            if ($convexity_flag < -0.009) {
                die("Admissible check 2 failure for maturity[$day] strike center[$vol_level] convexity value is [$convexity_flag].");
            }
        }
    }

    return 1;
}

sub _check_spot_reference {
    my $surface = shift;

    die('spot_reference is undef during volupdate for underlying [' . $surface->symbol . ']')
        unless $surface->spot_reference;

    return 1;
}

sub _check_age {
    my $surface = shift;

    my $market = $surface->underlying_config->market_name;
    my $delay = $market eq 'forex' ? 7200 : 12 * 3600;

    die('VolSurface is more than 2 hours old')
        if time - $surface->recorded_date->epoch > $delay;

    return 1;
}

# Make sure given surface seems reasonable.
sub _check_structure {
    my $surface = shift;

    my $surface_hashref = $surface->surface;
    my $system_symbol   = $surface->underlying_config->system_symbol;

    # Somehow I do not know why there is a limit of term on delta surface, but
    # for moneyness we might need at least up to 2 years to get the spread.
    my $type = $surface->type;
    my ($max_term, $diff_smile_point) = $type eq 'delta' ? (380, 30) : (750, 100);

    my $extra_allowed = $surface->underlying_config->extra_vol_diff_by_delta || 0;
    my $max_vol_change_by_delta = 0.4 + $extra_allowed;

    my @days = keys %{$surface_hashref};
    die('Must be at least two maturities on vol surface.') if scalar @days < 2;

    foreach my $day (@days) {
        if ($day !~ /^\d+$/) {
            die("Invalid day[$day] in volsurface for underlying[$system_symbol]. Not a positive integer.");
        } elsif ($day > $max_term) {
            die("Day[$day] in volsurface for underlying[$system_symbol] greater than allowed.");
        }

        if (not grep { exists $surface_hashref->{$day}->{$_} } qw(smile vol_spread)) {
            die("Missing both smile and atm_spread (must have at least one) for day [$day] on underlying [$system_symbol]");
        }
    }

    my $min_day = min @days;
    my $market  = $surface->underlying_config->market_name;

    if ($market eq 'forex' and $min_day > 7) {
        die("ON term is missing in volsurface for underlying $system_symbol, the minimum term is $min_day");
    }

    @days = sort { $a <=> $b } @days;
    my @volatility_level;

    if ($type eq 'delta') {
        @volatility_level = sort { $a <=> $b } @{$surface->deltas};
    } else {
        @volatility_level = sort { $a <=> $b } @{$surface->moneynesses};
    }

    foreach my $vol_level (@volatility_level) {
        if ($vol_level !~ /^\d+\.?\d+$/) {
            die("Invalid vol_point[$vol_level] for underlying[$system_symbol]");
        }
    }

    reduce {
        abs($a - $b) <= $diff_smile_point
            ? $b
            : die("Difference between point $a and $b too great.");
    }
    @volatility_level;

    foreach my $day (@days) {

        if (exists $surface_hashref->{$day}->{smile}) {
            my $smile = $surface_hashref->{$day}->{smile};
            my @vol_levels_for_smile = sort { $a <=> $b } keys %{$smile};

            if ($type eq 'delta') {
                my $levels_mismatch = (@volatility_level != @vol_levels_for_smile)
                    || (any { $volatility_level[$_] != $vol_levels_for_smile[$_] } (0 .. @volatility_level - 1));
                if ($levels_mismatch) {
                    die(      'Deltas['
                            . join(',', @vol_levels_for_smile)
                            . "] for maturity[$day], underlying["
                            . $system_symbol
                            . '] are not the same as deltas for rest of surface['
                            . join(',', @volatility_level)
                            . '].');
                }
            }

            reduce {
                return $a if (not defined $b);
                abs($smile->{$a} - $smile->{$b}) <= $max_vol_change_by_delta * $smile->{$a}
                    ? $b
                    : die("Invalid volatility points: too big jump from "
                        . "$a:$smile->{$a} to $b:$smile->{$b}"
                        . "for maturity[$day], underlying[$system_symbol]");
            }
            @volatility_level;

        }
    }

    return 1;
}

=head2 check_smiles

Performs error checking on multiple smilesof a given surface.

=cut

sub check_smiles {
    my ($self, $surface) = @_;

    foreach my $day (@{$surface->original_term_for_smile}) {
        $self->check_smile($day, $surface->surface->{$day}->{smile}, $surface->underlying_config->system_symbol);
    }

    return 1;
}

=head2 check_smile

Performs error checking on an individual smile of a given surface.

=cut

sub check_smile {
    my ($self, $day, $smile, $system_symbol) = @_;

    $system_symbol ||= 'unknown symbol';

    foreach my $vol_level (keys %{$smile}) {
        my $vol = $smile->{$vol_level};
        if ($vol !~ /^\d?\.?\d*$/ or $vol > 5) {
            die(      'Invalid smile volatility for '
                    . $day
                    . ' days at volatility level (either delta or moneyness level) '
                    . $vol_level . ' ('
                    . $vol
                    . ') for '
                    . $system_symbol);
        }
    }

    return 1;
}

#
# To ensure that the volatility is arbitrage-free, the total implied variance must be strictly
# increasing by forward moneyness.
#       As proven by Fengler, 2005 "Arbitrage-free smoothing of the implied volatility surface"
#       p.10, Proposition 2.1
#
# We check the surface market points. This is usually done at startup when volsurface object is
# being created.
#
# Forward Moneyness = K/F_T
sub _check_termstructure_for_calendar_arbitrage {
    my $surface = shift;
    my @sorted_expiries = sort { $a <=> $b } @{$surface->original_term_for_smile};

    my $cloned_surface = $surface->clone;
    my $surface_type   = $surface->type;
    my $flag           = 0;
    my $error_maturity = 0;
    my $error_strike   = 0;
    my $message;
    for (my $i = 1; $i < scalar(@sorted_expiries); $i++) {
        my $smile      = $surface->surface->{$sorted_expiries[$i]}->{smile};
        my $smile_prev = $surface->surface->{$sorted_expiries[$i - 1]}->{smile};

        my @volatility_level = sort { $a <=> $b } keys %{$smile};
        if ($surface_type eq 'delta') {
            my $vol      = $smile->{50};
            my $vol_prev = $smile_prev->{50};
            my $T        = $sorted_expiries[$i];
            my $T_prev   = $sorted_expiries[$i - 1];
            if (((($vol)**2) * $T) < (($vol_prev**2) * $T_prev)) {
                $flag           = -1;
                $error_maturity = $sorted_expiries[$i - 1];
                $error_strike   = 50;
                $message        = 'Variance negative for maturity ' . $error_maturity . ' for ATM';
                last;
            }
        } else {
            my $atm_level = 100;
            my $atm_index = indexes { $_ == $atm_level } @volatility_level;

            my $symbol = $surface->symbol;
        }
    }
# This check is ad-hoc, just to ensure there are no huge outliers, or calendar arbitrages.
    if ($flag == -1) {
        $surface->set_smile_flag($error_maturity, $message);
    }

    return 1;
}

no Moose;
__PACKAGE__->meta->make_immutable;
1;
