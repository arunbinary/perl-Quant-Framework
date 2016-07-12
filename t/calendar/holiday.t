#!/usr/bin/perl

use strict;
use warnings;

use Test::More;
use Date::Utility;

use Quant::Framework::StorageAccessor;
use Quant::Framework::Holiday;
use Quant::Framework::Utils::Test;
use Data::Chronicle::Writer;
use Data::Chronicle::Reader;
use Data::Chronicle::Mock;

my ($chronicle_r, $chronicle_w) = Data::Chronicle::Mock::get_mocked_chronicle;

my $storage_accessor = Quant::Framework::StorageAccessor->new(
    chronicle_reader => $chronicle_r,
    chronicle_writer => $chronicle_w,
);

my $non_existing_holiday = Quant::Framework::Holiday->load(
    storage_accessor => $storage_accessor,
);

is $non_existing_holiday, undef, 'document is not present';

my $now = Date::Utility->new;

subtest 'save and retrieve event' => sub {
  my $a_bit_earlier = Date::Utility->new($now->epoch - 100);
  my $a_bit_later = Date::Utility->new($now->epoch + 100);
  my $h = Quant::Framework::Holiday->create(
    storage_accessor => $storage_accessor,
    for_date         => $a_bit_earlier,
  );
  ok $h;

  # the both events occur in some time in future, but information about that
  # appears in different times

  $h->update({ $a_bit_later->epoch => { 'Test Event' => ['USD'], }  }, $now)->save;
  $h->update({ $a_bit_later->epoch => { 'Test Event 2' => ['EURONEXT'], }  }, $a_bit_later)->save;

  my $h2 = Quant::Framework::Holiday->load(
    storage_accessor => $storage_accessor,
  );
  ok $h2;

  my $event = Quant::Framework::Holiday::holidays_for($storage_accessor, 'EURONEXT');
  ok $event->{$a_bit_later->truncate_to_day->epoch}, 'has a holiday';
  is $event->{$a_bit_later->truncate_to_day->epoch}, 'Test Event 2', 'Found saved holiday';

  my $next_day = $now->plus_time_interval('1d');
  $h2->update({$next_day->epoch => { 'Test Event Update' => ['AUD'] }}, $next_day)->save;

  my $h3 =Quant::Framework::Holiday->load(
    storage_accessor => $storage_accessor,
  );
  $event = Quant::Framework::Holiday::holidays_for($storage_accessor, 'USD');
  ok !(%$event), "no holiday";
  $event = Quant::Framework::Holiday::holidays_for($storage_accessor, 'AUD');
  ok $event->{$next_day->truncate_to_day->epoch}, 'has a holiday';
  is $event->{$next_day->truncate_to_day->epoch}, 'Test Event Update', 'Found saved holiday';

  my $h4 = Quant::Framework::Holiday->load(
    storage_accessor => $storage_accessor,
    for_date         => $now,
  );
  ok $h4, "historical holiday object has been loaded";
  ok Quant::Framework::Holiday::holidays_for($storage_accessor, 'USD', $now)->{$now->truncate_to_day->epoch}, 'Historical holyday has been loaded';
  is scalar(%{ Quant::Framework::Holiday::holidays_for($storage_accessor, 'EURONEXT')}), 0, "holiday from future isn't available in past-holiday records";
};

done_testing;
