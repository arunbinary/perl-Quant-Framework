package Quant::Framework::Holiday;

use strict;
use warnings;

use Date::Utility;
use Moo;
use List::Util qw(first);
use List::MoreUtils qw(uniq);

with('Quant::Framework::Document');

=head1 NAME

Quant::Framework::Holiday - A module to save/load market holidays

=head1 DESCRIPTION

This module saves/loads holidays to/from Chronicle.

  my $holiday = Quant::Framework::Holiday->load(
    storage_accessor => $storage_accessor
  );
  $holiday->update({ $now->epoch => { 'Some-event-affects-USD' => ['USD'], }  }, $now)->save;

  my $holidays_data = Quant::Framework::Holiday::holidays_for($storage_accessor, 'EUR', $some_date);


=head1 SUBROUTINES

=head2 namespace

returns hard-coded string 'holidays'. Required to conform Document role contract.

=head2 initialize_data

returns default data hash, i.e. C<{ calendar => {}}> Required to conform Document role contract.

=head2 create($package, %data)

Creates new holiday object

  Quant::Framework::Holiday->create(
    storage_accessor => $storage_accessor,
    for_date         => $now,
  )

=head2 load($package, %data)

Loads persisted Holiday object. Returns undef it is not present. C<$for_date>
is optional.

  Quant::Framework::Holiday->load(
    storage_accessor => $storage_accessor,
    for_date         => $now,
  );

=head2 update($self, $new_events, $date);

It migrates B<non-occurred> existing events, i.e. those which are later then
C<$date>, adds C<$new_events> and returns new unpersisted Holiday object.

  $holiday->update({ $now->epoch => { 'Some-event-affects-USD' => ['USD'], }  }, $now)
    ->save;

=cut

sub namespace {
    return 'holidays';
}

sub initialize_data {
    return {calendar => {}};
}

# override default create function, to supply symbol, as we don't have
# symbol for Holiday (it is equal to 'holidays')
sub create {
    my ($package, %data) = @_;
    $data{symbol} = namespace;
    return $package->create_default(%data);
}

sub load {
    my ($package, %data) = @_;
    $data{symbol} = namespace;
    return $package->load_default(%data);
}

sub update {
    my ($self, $new_events, $new_date) = @_;
    my $recorded_date = $self->recorded_date->truncate_to_day->epoch;
    my $start_epoch   = $new_date->truncate_to_day->epoch;

    # filter all events, that will happen later then $new_date
    my $persisted_events = $self->data->{calendar};
    my @actual_dates     = grep { $_ >= $start_epoch } keys %$persisted_events;
    my %actual_events    = @$persisted_events{@actual_dates};

    # append / update persisited events with new
    while (my ($new_holiday_date, $new_holiday) = each %$new_events) {
        my $epoch = Date::Utility->new($new_holiday_date)->truncate_to_day->epoch;

        my $persisted_event = $actual_events{$epoch};
        # persist new event, if it was completely absent
        if (!$persisted_event) {
            $actual_events{$epoch} = $new_holiday;

            # otherwise merge descriptions and affected symbols
        } else {
            while (my ($description, $new_symbols) = each %$new_holiday) {
                my @merged_symbols = uniq(@${$persisted_event->{$description}}, @$new_symbols);
                $persisted_event->{$description} = \@merged_symbols;
            }
        }
    }

    my $new_obj = __PACKAGE__->new(
        data             => {calendar => \%actual_events},
        storage_accessor => $self->storage_accessor,
        recorded_date    => $new_date,
        symbol           => namespace,
    );

    return $new_obj;
}

=head2 holidays_for($symbol, $for_date)

This method looks for holidays of the given symbol (at the optional given time) using
chronicle_reader object passed to it.

  Quant::Framework::Holiday::holidays_for('USD');

=cut

sub holidays_for {
    my ($storage_accessor, $symbol, $for_date) = @_;
    my $holiday = __PACKAGE__->load(
        storage_accessor => $storage_accessor,
        for_date         => $for_date,
    );

    my $data = $holiday ? $holiday->data->{calendar} : {};

    my %holidays;
    while (my ($epoch, $holiday) = each %$data) {
        while (my ($description, $symbols) = each %$holiday) {
            $holidays{$epoch} = $description if (first { $symbol eq $_ } @$symbols);
        }
    }
    return \%holidays;
}

1;
