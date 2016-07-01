package Quant::Framework::VolSurface::TestDelta;

use Moose;
extends 'Quant::Framework::VolSurface';

use Number::Closest::XS qw(find_closest_numbers_around);
use List::Util qw(min);
use Quant::Framework::VolSurface::Utils;
use Math::Function::Interpolator;

has for_date => (
    is      => 'ro',
    isa     => 'Maybe[Date::Utility]',
    default => undef,
);

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

has variance_table => (
    is         => 'ro',
    lazy_build => 1,
);

sub _build_variance_table {
    my $self = shift;

    my $raw_surface    = $self->surface;
    my $recorded_date  = Date::Utility->new($self->document->{date});
    my $effective_date = Quant::Framework::VolSurface::Utils->new->effective_date_for($recorded_date);
    # New York 10:00
    my $ny_offset_from_gmt     = $effective_date->timezone_offset('America/New_York')->hours;
    my $seconds_after_midnight = $effective_date->plus_time_interval(10 - $ny_offset_from_gmt . 'h')->seconds_after_midnight;

    # keys are tenor in epoch, values are associated variances.
    my %table = ($recorded_date->epoch => 0);
    foreach my $tenor (sort { $a <=> $b } keys %$raw_surface) {
        my $epoch      = $effective_date->plus_time_interval($tenor . 'd' . $seconds_after_midnight . 's')->epoch;
        my $volatility = $raw_surface->{$tenor}{smile}{50};                                                          # just atm for now
        $table{$epoch} = $volatility**2 * $tenor;
    }

    return \%table;
}

has surface => (
    is         => 'ro',
    lazy_build => 1,
);

sub _build_surface {
    my $self = shift;

    return $self->document->{surfaces}{'New York 10:00'} // {};
}

sub get_volatility {
    my ($self, $from, $to) = @_;

    die '$from is after $to in get_volatility function.' if $from->epoch > $to->epoch;

    my $days_between = Time::Duration::Concise->new(interval => ($to->epoch - $from->epoch))->days;
    my $var1         = $self->get_variance($from);
    my $var2         = $self->get_variance($to);
    my $volatility   = sqrt($var2 - $var1) / $days_between;

    return $volatility;
}

sub get_variance {
    my ($self, $date) = @_;

    my $epoch = $date->epoch;
    my $table = $self->variance_table;

    return $table->{$epoch} if $table->{$epoch};

    my @available_tenors = sort { $a <=> $b } keys %{$table};
    my $epoch_closest = find_closest_numbers_around($date->epoch, \@available_tenors, 2)->[1];
    my $var1          = $table->{$epoch_closest};
    my $ratio         = $self->get_weight_ratio($date, Date::Utility->new($epoch_closest));

    return $ratio * $var1;
}

sub get_weight_ratio {
    my ($self, $date1, $date2) = @_;

    return ($self->get_weight($date1) / $self->get_weight($date2));
}

sub get_weight {
    my ($self, $date) = @_;

    # always starts from surface recorded date to $date
    my $recorded_date   = Date::Utility->new($self->document->{date});
    my $time_diff       = $date->epoch - $recorded_date->epoch;
    my $weight_interval = 4 * 3600;
    my @dates           = ($recorded_date);

    if ($time_diff <= $weight_interval) {
        push @dates, $date;
    } else {
        my $start = $recorded_date;
        while ($start->epoch < $date->epoch) {
            my $to_add = min($date->epoch - $start->epoch, $weight_interval);
            $start = $start->plus_time_interval($to_add . 's');
            push @dates, $start;
        }
    }

    my $total_weight = 0;
    for (my $i = 1; $i <= $#dates; $i++) {
        my $dt = $dates[$i]->epoch - $dates[$i - 1]->epoch;
        $total_weight += $self->builder->build_trading_calendar->weight_on($dates[$i]) * $dt / 86400;
    }

    return $total_weight;
}

has '+type' => (
    default => 'delta',
);

no Moose;
__PACKAGE__->meta->make_immutable;
1;
