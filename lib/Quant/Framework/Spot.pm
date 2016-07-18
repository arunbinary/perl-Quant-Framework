package Finance::Spot;

use 5.006;
use strict;
use warnings;

use Moose;
use Scalar::Util qw( looks_like_number );
use Cache::RedisDB;
use Finance::Spot::Tick;
use Finance::Spot::DatabaseAPI;

=head1 NAME

Finance::Spot - Used to store/retrieve spot prices into/from local Redis storage

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

has symbol => (
    is       => 'ro',
    required => 1,
);

has for_date => (
    is => 'ro',
);

has calendar => (
    is => 'ro',
);

has use_official_ohlc => (
    is => 'ro',
);

has 'feed_api' => (
    is      => 'ro',
    isa     => 'Finance::Spot::DatabaseAPI',
    handles => {
        ticks_in_between_start_end   => 'ticks_start_end',
        ticks_in_between_start_limit => 'ticks_start_limit',
        ticks_in_between_end_limit   => 'ticks_end_limit',
        ohlc_between_start_end       => 'ohlc_start_end',
        next_tick_after              => 'tick_after',
    },
);

=head1 SYNOPSIS

Used to store/retrieve spot prices into/from local Redis storage
You will need to provide the module with a code reference which will be used
in exception cases where local redis is empty and a request to read spot price
is received. In this case, module will use the given code references to fetch
spot price from a secondary storage (this can be a database).
If you omit for_date parameter, you will fetch latest spot price.

    use Finance::Spot;

    my $foo = Finance::Spot->new({
        symbol => 'GDAXI',
        for_date => Date::Utility->new('2016-09-11 12:29:10')
    });

    ...

=head1 SUBROUTINES/METHODS

=head2 $self->spot_tick

Get last tick value for symbol from Redis. It will rebuild value from
feed db if it is not present in cache.

=cut

sub spot_tick {
    my $self = shift;

    return $self->tick_at($self->for_date->epoch, {allow_inconsistent => 1}) if $self->for_date;

    my $value = Cache::RedisDB->get('COMBINED_REALTIME', $self->symbol);
    my $tick;
    if ($value) {
        $tick = Finance::Spot::Tick->new($value);
    } else {
        $tick = $self->tick_at(time, {allow_inconsistent => 1});
        if ($tick) {
            $self->set_spot_tick($tick);
        }
    }

    return $tick;
}


=head2 spot_tick_hash      

 Returns a hash reference denoting available fields in the spot_tick       

=cut      

sub spot_tick_hash {      
    my $self = shift;     

    my $tick = $self->spot_tick;      

    return ($tick) ? $tick->as_hash : undef;      
}     


=head2 tick_at

This method receives a timestamp and an option for inconsistency.
Returns the tick for current symbol at the given time. If inconssitency is enabled
and there is no tick at that moment, the latest tick before that time is 
returned.

=cut

sub tick_at {
    my ($self, $timestamp, $allow_inconsistent_hash) = @_;

    my $inconsistent_price;
    # get official close for previous trading day
    if (defined $allow_inconsistent_hash->{allow_inconsistent}
        and $allow_inconsistent_hash->{allow_inconsistent} == 1)
    {
        $inconsistent_price = 1;
    }

    my $pricing_date = Date::Utility->new($timestamp);
    my $tick;

    if ($self->use_official_ohlc
        and not $self->calendar->trades_on($pricing_date))
    {
        my $last_trading_day = $self->calendar->trade_date_before($pricing_date);
        $tick = $self->closing_tick_on($last_trading_day->date_ddmmmyy);
    } else {
        my $request_hash = {};
        $request_hash->{end_time} = $timestamp;
        $request_hash->{allow_inconsistent} = 1 if ($inconsistent_price);

        $tick = $self->feed_api->tick_at($request_hash);
    }

    return $tick;
}

=head2 closing_tick_on

Get the market closing tick for a given date.

Example : $underlying->closing_tick_on("10-Jan-00");

=cut

sub closing_tick_on {
    my ($self, $end, $closing) = @_;
    my $date = Date::Utility->new($end);

    if ($closing and time > $closing->epoch) {
        my $ohlc = $self->feed_api->ohlc_start_end({
            start_time         => $date,
            end_time           => $date,
            aggregation_period => 86400,
        });

        if ($ohlc and scalar @{$ohlc} > 0) {

            # We need a tick, but we can only get an OHLC
            # The epochs for these are set to be the START of the period.
            # So we also need to change it to the closing time. Meh.
            my $not_tick = $ohlc->[0];
            return Finance::Spot::Tick->new({
                symbol => $self->symbol,
                epoch  => $closing->epoch,
                quote  => $not_tick->close,
            });
        }
    }
    return;
}

=head2 $self->set_spot_tick($value)

Save last tick value for symbol in Redis. Returns true if operation was
successfull, false overwise. Tick value should be a hash reference like this:

{
    epoch => $unix_timestamp,
    quote => $last_price,
}

You will need to provide the module with a code reference which will be used
in exception cases where local redis is empty and a request to read spot price
is received. In this case, module will use the given code references to fetch
spot price from a secondary storage (this can be a database).

=cut

sub set_spot_tick {
    my ($self, $value) = @_;

    my $tick;
    if (ref $value eq 'Finance::Spot::Tick') {
        $tick  = $value;
        $value = $value->as_hash;
    } else {
        $tick = Finance::Spot::Tick->new($value);
    }
    Cache::RedisDB->set_nw('COMBINED_REALTIME', $self->symbol, $value);
    return $tick;
}

=head2 spot_quote

What is the current spot price for this underlying?

=cut

sub spot_quote {
    my $self = shift;
    my $last_price;

    my $last_tick = $self->spot_tick;
    $last_price = $last_tick->quote if $last_tick;

    return $last_price;
}

=head2 spot_time

The epoch timestamp of the latest recorded tick in the .realtime file or undef if we can't find one.

=cut

sub spot_time {
    my $self      = shift;
    my $last_tick = $self->spot_tick;
    return $last_tick && $last_tick->epoch;
}

=head2 spot_age

The age in seconds of the latest tick

=cut

sub spot_age {
    my $self      = shift;
    my $tick_time = $self->spot_time;
    return defined $tick_time && time - $tick_time;
}

=head1 AUTHOR

Binary.com, C<< <support at binary.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-finance-spot at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Finance-Spot>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Finance::Spot


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Finance-Spot>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Finance-Spot>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Finance-Spot>

=item * Search CPAN

L<http://search.cpan.org/dist/Finance-Spot/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2016 Binary.com.

This program is free software; you can redistribute it and/or modify it
under the terms of the the Artistic License (2.0). You may obtain a
copy of the full license at:

L<http://www.perlfoundation.org/artistic_license_2_0>

Any use, modification, and distribution of the Standard or Modified
Versions is governed by this Artistic License. By using, modifying or
distributing the Package, you accept this license. Do not use, modify,
or distribute the Package, if you do not accept this license.

If your Modified Version has been derived from a Modified Version made
by someone other than you, you are nevertheless required to ensure that
your Modified Version complies with the requirements of this license.

This license does not grant you the right to use any trademark, service
mark, tradename, or logo of the Copyright Holder.

This license includes the non-exclusive, worldwide, free-of-charge
patent license to make, have made, use, offer to sell, sell, import and
otherwise transfer the Package with respect to any patent claims
licensable by the Copyright Holder that are necessarily infringed by the
Package. If you institute patent litigation (including a cross-claim or
counterclaim) against any party alleging that the Package constitutes
direct or contributory patent infringement, then this Artistic License
to you shall terminate on the date that such litigation is filed.

Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER
AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY
YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR
CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR
CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


=cut

1;    # End of Finance::Spot
