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

subtest 'clone' => sub {
    my $underlying = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $orig_delta    = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            recorded_date     => Date::Utility->new,
            save              => 0,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
        });

    lives_ok {
        my $clone = $orig_delta->clone;
        is $clone->symbol, $orig_delta->symbol, 'symbol matches';
        is_deeply $clone->surface_data, $orig_delta->surface_data, 'surface_data matches';
        is $clone->recorded_date->epoch, $orig_delta->recorded_date->epoch, 'recorded_date matches';
    } 'clone with parameters';

    my $surface_data = {
        1 => {
            smile => {
                50 => 0.1,
            },
            vol_spread => {
                50 => 0.2
            },
        },
    };

    lives_ok {
        my $clone = $orig_delta->clone({surface_data => $surface_data});
        is $clone->symbol, $orig_delta->symbol, 'symbol matches';
        is $clone->recorded_date->epoch, $orig_delta->recorded_date->epoch, 'recorded_date matches';
        is_deeply $clone->surface_data, $surface_data, 'surface_data is overwritten';
        is_deeply $clone->surface, $surface_data, 'surface is overwritten';
    } 'clone with surface';

    lives_ok {
        my $surface = {
            1 => {
                smile => {
                    50 => 0.1,
                },
                vol_spread => {
                    50 => 0.2
                },
            },
            7 => {
                smile => {
                    50 => 0.1,
                },
                vol_spread => {
                    50 => 0.2
                },
            },
        };
        my $clone = $orig_delta->clone({surface => $surface, surface_data => $surface_data});
        is $clone->symbol, $orig_delta->symbol, 'symbol matches';
        is $clone->recorded_date->epoch, $orig_delta->recorded_date->epoch, 'recorded_date matches';
        is_deeply $clone->surface_data, $surface_data, 'surface_data is overwritten';
        is_deeply $clone->surface, $surface_data, 'surface is overwritten';
    } 'clone with surface and surface_data';

    my $underlying_config = Quant::Framework::Utils::Test::create_underlying_config('SPC');
    my $moneyness = Quant::Framework::Utils::Test::create_doc(
        'volsurface_moneyness', {
        underlying_config => $underlying_config,
        spot_reference    => $underlying_config->spot,
        recorded_date     => Date::Utility->new,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    });

    lives_ok {
        my $clone = $moneyness->clone;
        is $clone->symbol, $moneyness->symbol, 'symbol matches';
        is $clone->recorded_date->epoch, $moneyness->recorded_date->epoch, 'recorded_date matches';
        is $clone->spot_reference, $moneyness->spot_reference, 'spot reference matches';
        is_deeply $clone->surface_data, $moneyness->surface_data, 'surface_data is overwritten';
        is_deeply $clone->surface, $moneyness->surface_data, 'surface is overwritten';
    } 'clone without arguments';

    lives_ok {
        my $clone = $moneyness->clone({spot_reference => 100});
        is $clone->spot_reference, 100, 'spot_reference matches';
    };
};

subtest 'original_term' => sub {
    my $underlying_config = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $surface_data = {
        1 => {
            smile => {
                50 => 0.1,
            },
            vol_spread => {
                50 => 0.2
            },
        },
    };
    my $delta      = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying_config,
            recorded_date     => Date::Utility->new,
            surface_data      => $surface_data,
            save              => 0,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
        });
    $delta->surface->{5} = {
        smile => {
            50 => 0.2,
        },
        vol_spread => {
            50 => 0.2,
        },
    };
    is scalar(keys %{$delta->surface}), 2, 'added one term to surface';
    is scalar(@{$delta->original_term_for_smile}), 1, 'original term for smile does not change.';
    is scalar(@{$delta->original_term_for_spread}), 1, 'original term for spread does not change';
};

done_testing();
