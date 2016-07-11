use strict;
use warnings;

use Test::More;
use Test::Exception;

use Date::Utility;
use Quant::Framework::Utils::Test;
use Quant::Framework::VolSurface::Delta;
use Quant::Framework::VolSurface::Moneyness;

my ($chronicle_r, $chronicle_w) = Data::Chronicle::Mock::get_mocked_chronicle();
my $now = Date::Utility->new;

subtest 'legacy vol spread structure' => sub {
    my $data = {
        1 => {
            smile => {
                25 => 0.1,
                50 => 0.1,
                75 => 0.1,
            },
            atm_spread => 0.1,
        },
        7 => {
            smile => {
                25 => 0.11,
                50 => 0.12,
                75 => 0.13,
            },
            atm_spread => 0.12,
        },
    };

    my $uc = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    Quant::Framework::Utils::Test::create_doc('volsurface_delta', {
        recorded_date => $now,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
        underlying_config => $uc,
        surface => $data,
    });

    my $s = Quant::Framework::VolSurface::Delta->new({
        underlying_config => $uc,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
    });
    ok exists $s->surface->{1}->{vol_spread}, 'converted atm_spread to vol_spread';
    ok !exists $s->surface->{1}->{atm_spread}, 'atm_spread is deleted from surface data';
    is $s->surface->{1}->{vol_spread}->{50}, 0.1, 'vol_spread is 0.1';

    my $moneyness_data = {
        30 => {
            smile => {
                90 => 0.1,
                100 => 0.12,
                110 => 0.1,
            },
            atm_spread => 0.12
        },
        60 => {
            smile => {
                90 => 0.1,
                100 => 0.12,
                110 => 0.1,
            },
            atm_spread => 0.2
        },
    };
    $uc = Quant::Framework::Utils::Test::create_underlying_config('SPC');
    Quant::Framework::Utils::Test::create_doc('volsurface_moneyness', {
        recorded_date => $now,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
        underlying_config => $uc,
        surface => $moneyness_data,
    });
    $s = Quant::Framework::VolSurface::Moneyness->new({
        underlying_config => $uc,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
        });
    ok exists $s->surface->{30}->{vol_spread}, 'converted atm_spread to vol_spread';
    ok !exists $s->surface->{30}->{atm_spread}, 'atm_spread is deleted from surface data';
    is $s->surface->{30}->{vol_spread}->{100}, 0.12, 'vol_spread is 0.12';
};

subtest 'get_smile_spread' => sub {
    my $uc = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $s = Quant::Framework::VolSurface::Delta->new({
        underlying_config => $uc,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
    });
    lives_ok {
        my $ss = $s->get_smile_spread(1);
        is $ss->{50}, 0.1, 'fetched 0.1';
        $ss = $s->get_smile_spread(2);
        is $ss->{50}, 0.103333333333333, 'fetched 0.103333333333333';
        $ss = $s->get_smile_spread(0.1);
        is $ss->{50}, 0.1, 'fetched 0.1';
        $ss = $s->get_smile_spread(366);
        is $ss->{50}, 0.12, 'fetched 0.12';
    } 'get_smile_spread';
};

subtest 'get_spread' => sub {
    my $data = {
        1 => {
            smile => {
                25 => 0.1,
                50 => 0.1,
                75 => 0.1,
            },
            vol_spread => {
                25 => 0.3,
                50 => 0.21,
                75 => 0.1,
            },
        },
    };

    my $uc = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    Quant::Framework::Utils::Test::create_doc('volsurface_delta', {
        recorded_date => $now,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
        underlying_config => $uc,
        surface => $data,
    });
    my $s = Quant::Framework::VolSurface::Delta->new({
        underlying_config => $uc,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
    });

    lives_ok {
        my $spread = $s->get_spread({sought_point => 'atm', day => 1});
        is $spread, 0.21, 'atm spread is 0.21';
        $spread = $s->get_spread({sought_point => 'max', day => 1});
        is $spread, 0.3, 'atm spread is 0.3';
    } 'get_spread';

    throws_ok {
        $s->get_spread({sought_point => 'unknown', day => 1})
    } qr/Unrecognized spread type/ , 'throws error if spread type is unknown';
};

done_testing();
