use strict;
use warnings;

use 5.010;
use Test::Most;

use List::Util qw( max );
use Test::MockObject::Extends;
use Test::Warn;
use Scalar::Util qw( looks_like_number );
use Test::MockModule;
use File::Spec;
use JSON qw(decode_json);

use Format::Util::Numbers qw(roundnear);
use Date::Utility;
use Quant::Framework::Utils::Test;
use Quant::Framework::VolSurface::Delta;

my ($chronicle_r, $chronicle_w) = Data::Chronicle::Mock::get_mocked_chronicle();

Quant::Framework::Utils::Test::create_doc(
    'currency',
    {
        symbol           => $_,
        recorded_date    => Date::Utility->new,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
    }) for (qw/EUR JPY USD/);

my @mocked_builders;

subtest 'get_volatility for different expiries ' => sub {
    plan tests => 5;
    my $surface = _get_surface();

    throws_ok { $surface->get_volatility({delta => 50, days => undef}) }
    qr/Must pass exactly one of/i,
        "throws exception when expiry is undef in get_vol";
    throws_ok { $surface->get_volatility({delta => 50, days => 1, expiry_date => '12-Jan-12'}) }
    qr/Must pass exactly/i,
        "throws exception when more than one expiry format is passed into get_volatility";
    lives_ok { $surface->get_volatility({delta => 50, expiry_date => Date::Utility->new('12-Jan-12')}) } "can get volatility for expiry_date";
    lives_ok { $surface->get_volatility({delta => 50, days        => 1}) } "can get volatility for days";
    lives_ok { $surface->get_volatility({delta => 50, tenor       => '1W'}) } "can get volatility for tenor";
};

subtest 'get_volatility for different sought points' => sub {
    plan tests => 5;
    my $surface = _get_surface();

    throws_ok { $surface->get_volatility({strike => 76.8, delta => 50, days => 1}) }
    qr/exactly one of/i,
        "throws exception when more than on sough points are parsed in get_volatility";
    throws_ok { $surface->get_volatility({strike => undef, days => 1}) } qr/exactly one/i, "throws exception if strike is undef";
    lives_ok { $surface->get_volatility({strike    => 76.5, days => 1}) } "can get_vol for strike";
    lives_ok { $surface->get_volatility({delta     => 50,   days => 1}) } "can get_vol for delta";
    lives_ok { $surface->get_volatility({moneyness => 100,  days => 1}) } "can get_vol for moneyness";
};

subtest 'get_smile' => sub {
    plan tests => 19;
    my $surface = _get_surface();

    my $smile;
    lives_ok { $smile = $surface->get_smile(1) } "can get_smile for term that already exist on the surface";
    is($smile->{25}, 0.2, "correct value for 25D");
    is($smile->{50}, 0.1, "correct value for 50D");
    is($smile->{75}, 0.3, "correct value for 75D");

    lives_ok { $smile = $surface->get_smile(3) } "can get_smile for interpolated term on the surface";
    my $D25 = $smile->{25};
    my $D50 = $smile->{50};
    my $D75 = $smile->{75};
    cmp_ok($D25, "<", 0.26, "25D for the interpolated 3-day smile if less than 7-day 25D ");
    cmp_ok($D25, ">", 0.2,  "25D for the interpolated 3-day smile if more than 1-day 25D ");
    cmp_ok($D50, "<", 0.22, "50D for the interpolated 3-day smile if less than 7-day 50D ");
    cmp_ok($D50, ">", 0.1,  "50D for the interpolated 3-day smile if more than 1-day 50D ");
    cmp_ok($D75, "<", 0.40, "75D for the interpolated 3-day smile if less than 7-day 75D ");
    cmp_ok($D75, ">", 0.3,  "75D for the interpolated 3-day smile if more than 1-day 75D ");

    lives_ok { $smile = $surface->get_smile(0.5) } "can get_smile for term less than the minimum term on the surface";
    is($smile->{25}, 0.2 + 0.0225, "correct value for 25D");
    is($smile->{50}, 0.1,          "correct value for 50D");    # ATM stays the same
    is($smile->{75}, 0.3 - 0.0225, "correct value for 75D");

    lives_ok { $smile = $surface->get_smile(366) } "can get_smile for term more than the maximum term on the surface";
    # Just return the highest term smile
    is($smile->{25}, 0.324, "correct value for 25D");
    is($smile->{50}, 0.3,   "correct value for 50D");
    is($smile->{75}, 0.45,  "correct value for 75D");
};

subtest set_smile => sub {
    plan tests => 7;
    my $surface = _get_surface();

    my %args = (
        smile => {
            50 => 0.2,
            25 => 0.1,
            75 => 0.5
        },
        days => 4
    );
    ok(!exists $surface->surface->{4}, "smile does not exist on surface");
    lives_ok { $surface->set_smile(\%args) } "can set_smile";
    ok(exists $surface->surface->{4}, "smile exists on surface after set_smile");
    my @a = @{$surface->original_term_for_smile};
    foreach (@a) {
        cmp_ok($_, '!=', 4, "the newly set smile is not in the original surface");
    }

    $args{days} = 'notnumber';
    throws_ok { $surface->set_smile(\%args) } qr/must be a number./, 'throws exception when term/day set to surface is not a number';
};

subtest get_spread => sub {
    plan tests => 13;

    my $surface = _get_surface({
            surface => {
                7 => {
                    smile => {
                        25 => 0.11,
                        50 => 0.1,
                        75 => 0.101
                    },
                    vol_spread => {50 => 0.05}
                },
                14 => {
                    smile => {
                        25 => 0.11,
                        50 => 0.1,
                        75 => 0.101
                    }
                },
                21 => {
                    smile => {
                        25 => 0.11,
                        50 => 0.1,
                        75 => 0.101
                    },
                    vol_spread => {50 => 0.05}
                },
            }});
    cmp_ok(
        $surface->get_spread({
                sought_point => 'atm',
                day          => 7
            }
        ),
        '==', 0.05,
        'Cause get_spread to interpolate.'
    );

    $surface = _get_surface();
    my $spread;
    lives_ok { $spread = $surface->get_spread({sought_point => 'atm', day => '1W'}) } 'can get spread for tenor';
    ok(looks_like_number($spread),    'spread looks like number');
    ok(exists $surface->surface->{7}, '7-day smile exists');
    lives_ok { $spread = $surface->get_spread({sought_point => 'atm', day => 7}) } "can get spread from spread that already exist on the smile";
    is($spread, 0.15, "get the right spread");
    lives_ok { $spread = $surface->get_spread({sought_point => 'atm', day => 4}) } "can get interpolated spread";
    cmp_ok($spread, '<', 0.2,  "interpolated spread < 0.2");
    cmp_ok($spread, '>', 0.15, "interpolated spread > 0.15");
    lives_ok { $spread = $surface->get_spread({sought_point => 'atm', day => 0.5}) }
    "can get the extrapolated spread when seek is smaller than the minimum of all terms";
    is(roundnear(0.01, $spread), 0.2, "correct extrapolated atm_spread");
    lives_ok { $spread = $surface->get_spread({sought_point => 'atm', day => 366}) }
    "can get the extrapolated spread when seek is larger than the maximum of all terms";
    is($spread, 0.1, "correct extrapolated atm_spread");
};

subtest get_day_for_tenor => sub {
    plan tests => 5;
    my $surface = _get_surface({
            surface => {
                '1W' => {
                    smile => {
                        50 => 0.1,
                    },
                    vol_spread => {50 => 0.1}}}});
    is_deeply(
        $surface->surface,
        {
            7 => {
                smile      => {50 => 0.1},
                tenor      => '1W',
                vol_spread => {50 => 0.1}}});
    my $day;
    lives_ok { $day = $surface->get_day_for_tenor('1W') } "can get day for tenor that is already present on the surface";
    is($day, 7, "returns the day on smile if present");

    my $surface2 = _get_surface({recorded_date => Date::Utility->new('12-Jun-12')});
    lives_ok { $day = $surface2->get_day_for_tenor('1W') } "can get day for tenor that does not exist on the surface";
    is($day, 7, "returns the calculated day for tenor");
};

subtest get_market_rr_bf => sub {
    plan tests => 6;
    my $surface = _get_surface({
            surface => {
                7 => {
                    smile => {
                        10 => 0.25,
                        25 => 0.2,
                        50 => 0.1,
                        75 => 0.22,
                        90 => 0.4
                    }}}});

    my $value;
    lives_ok { $value = $surface->get_market_rr_bf(7) } "can get market RR and BF values";
    ok(looks_like_number($value->{RR_25}), "RR_25 is a number");
    ok(looks_like_number($value->{BF_25}), "BF_25 is a number");
    ok(looks_like_number($value->{ATM}),   "ATM is a number");
    ok(looks_like_number($value->{RR_10}), "RR_10 is a number");
    ok(looks_like_number($value->{BF_10}), "BF_10 is a number");
};

subtest 'Flagging System' => sub {
    plan tests => 9;

    my $underlying = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $surface    = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            recorded_date     => Date::Utility->new,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
            save              => 0
        });

    $surface->set_smile_flag(1, 'first message');
    $surface->set_smile_flag(1, 'second message');
    $surface->set_smile_flag(1, 'third message');
    $surface->set_smile_flag(7, '007');

    my $smile_flag;
    lives_ok { $smile_flag = $surface->get_smile_flag(1) } 'can get smile flag for a particular day';
    is(scalar @{$smile_flag}, 3,                "We have three one day flags.");
    is($smile_flag->[0],      'first message',  "First one day flag is 'first message'");
    is($smile_flag->[1],      'second message', "Second one day flag is 'second message'");
    is($smile_flag->[2],      'third message',  "Third one day flag is 'third message'");

    lives_ok { my $smile_flags = $surface->get_smile_flags() } 'can get all smile flags';

    $smile_flag = $surface->get_smile_flag(7);

    is(scalar @{$smile_flag}, 1,     "We have 1 smile flag for 7 days expiry.");
    is($smile_flag->[0],      '007', "Flag is '007'");

    ok($surface->set_smile_flag('ON', 'ON is bad.'), 'Set smile flag for ON.');
};

subtest 'object creaion error check' => sub {
    plan tests => 3;
    my $underlying    = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $recorded_date = Date::Utility->new();
    my $surface       = {1 => {smile => {50 => 0.1}}};
    throws_ok {
        Quant::Framework::VolSurface::Delta->new({
                surface          => $surface,
                recorded_date    => $recorded_date,
                chronicle_reader => $chronicle_r,
                chronicle_writer => $chronicle_w,
            })
    }
    qr/Attribute underlying_config is required/, 'Cannot create volsurface without underlying_config';
    throws_ok {
        Quant::Framework::VolSurface::Delta->new({
                surface           => $surface,
                underlying_config => $underlying,
                chronicle_reader  => $chronicle_r,
                chronicle_writer  => $chronicle_w,
            })
    }
    qr/Must pass both "surface" and "recorded_date" if passing either/, 'Cannot create volsurface without recorded_date';
    lives_ok {
        Quant::Framework::VolSurface::Delta->new({
                surface           => $surface,
                underlying_config => $underlying,
                recorded_date     => $recorded_date,
                chronicle_reader  => $chronicle_r,
                chronicle_writer  => $chronicle_w,
            })
    }
    'can create volsurface';
};

subtest effective_date => sub {
    plan tests => 2;

    my $underlying = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $surface    = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            recorded_date     => Date::Utility->new('2012-03-09 21:15:00'),
            save              => 0,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
        });

    is($surface->_ON_day, 3, 'In winter, 21:15 on Friday is before rollover so _ON_day is 3.');

    $surface = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            recorded_date     => Date::Utility->new('2012-03-16 21:15:00'),
            save              => 0,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
        });

    is($surface->_ON_day, 2, 'In summer, 21:15 on Friday is after rollover so _ON_day is 2.');
};

subtest cloning => sub {
    plan tests => 7;

    my $underlying = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $surface    = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            recorded_date     => Date::Utility->new,
            save              => 0,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
        });

    my $clone = $surface->clone;

    isa_ok($clone, 'Quant::Framework::VolSurface::Delta');
    is($surface->underlying_config->symbol, $clone->underlying_config->symbol, 'clone without overrides: underlying.');
    is_deeply($surface->surface, $clone->surface, 'clone without overrides: surface.');
    is($surface->recorded_date->datetime, $clone->recorded_date->datetime, 'clone without overrides: recorded_date.');

    $clone = $surface->clone({
            underlying_config => Quant::Framework::Utils::Test::create_underlying_config('frxGBPNOK'),
            surface           => {
                7 => {
                    smile => {
                        25 => 0.55,
                        50 => 0.55,
                        75 => 0.55
                    }}
            },
            recorded_date   => Date::Utility->new('20-Jan-12'),
        });

    isnt($surface->underlying_config->symbol, $clone->underlying_config->symbol, 'clone with overrides: underlying.');
    cmp_ok(scalar @{$surface->term_by_day}, '!=', scalar @{$clone->term_by_day}, 'clone with overrides: surface.');
    isnt($surface->recorded_date->datetime, $clone->recorded_date->datetime, 'clone with overrides: recorded_date.');
};

subtest 'get_volatility, part 1.' => sub {
    my $expected_vols_delta = {
        7 => {
            smile => {
                25 => 0.17,
                50 => 0.16,
                75 => 0.22
            },
            vol_spread => {50 => 0.1},
        },
        14 => {
            smile => {
                25 => 0.17,
                50 => 0.152,
                75 => 0.218
            },
            vol_spread => {50 => 0.1},
        },
        30 => {
            smile => {
                25 => 0.173,
                50 => 0.15,
                75 => 0.213
            },
            vol_spread => {50 => 0.1},
        },
        60 => {
            smile => {
                25 => 0.183,
                50 => 0.158,
                75 => 0.215
            },
            vol_spread => {50 => 0.1},
        },
        91 => {
            smile => {
                25 => 0.189,
                50 => 0.167,
                75 => 0.217
            },
            vol_spread => {50 => 0.1},
        },
        182 => {
            smile => {
                25 => 0.202,
                50 => 0.183,
                75 => 0.222
            },
            vol_spread => {
                50 => 0.1,
            }
        },
    };

    my @days_to_expiry = sort { $a <=> $b } keys %{$expected_vols_delta};

    plan tests => scalar(@days_to_expiry) * 6 + 5;

    my $underlying = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $surface    = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            surface           => $expected_vols_delta,
            recorded_date     => Date::Utility->new,
            save              => 0,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
        });

    cmp_deeply(\@days_to_expiry, $surface->term_by_day, 'Term structures (delta) are same.');
    # Test that delta object is populated correctly
    foreach my $day (@days_to_expiry) {
        foreach my $delta (50, 75, 25) {
            my $vol;
            lives_ok {
                $vol = $surface->get_volatility({
                    days  => $day,
                    delta => $delta,
                });
            }
            'Can get volatility from delta surface. delta: ' . $delta;

            # The market quote vols (non-interpolated) must agree
            cmp_ok(
                $vol, '==',
                $expected_vols_delta->{$day}->{smile}->{$delta},
                "$delta delta Vol for $day days is $expected_vols_delta->{$day}->{smile}->{$delta}"
            );
        }
    }

    # Test format given for expiries.
    my @test_data = ('1.003', 8 / 86400, 1 / 86400, '1.003e-05');

    foreach my $day (@test_data) {
        lives_ok {
            my $vol = $surface->get_volatility({
                days        => $day,
                delta       => 50,
                extrapolate => 0,
            });
        }
        'Can get volatility from delta surface.';
    }
};

subtest 'save surface to chronicle' => sub {
    plan tests => 1;

    my $surface = _get_surface();
    lives_ok { $surface->save } 'can save surface to chronicle';
};

# PRIVATE METHODS

subtest _days_with_smiles => sub {
    plan tests => 4;
    my $surface = _get_surface();

    my @days;
    lives_ok {
        @days = sort { $a <=> $b } @{$surface->_days_with_smiles()};
    }
    'can call _get_days_with_smile';
    foreach my $day (@days) {
        ok(exists $surface->surface->{$day}->{smile}, "smile for $day exists");
    }
};

subtest _convert_expiry_to_day => sub {
    plan tests => 3;
    my $surface = _get_surface();
    lives_ok { $surface->_convert_expiry_to_day({expiry_date => Date::Utility->new()}) } 'can convert for expiry_date';
    lives_ok { $surface->_convert_expiry_to_day({tenor       => '1W'}) } 'can convert for tenor';
    lives_ok { $surface->_convert_expiry_to_day({days        => 1}) } 'can convert for days';
};

subtest _validate_sought_points => sub {
    plan tests => 6;

    my $underlying = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    my $surface    = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            surface           => {
                7 => {
                    atm_spread => 0.01,
                },
            },
            recorded_date    => Date::Utility->new,
            chronicle_reader => $chronicle_r,
            chronicle_writer => $chronicle_w,
            save             => 0,
        });

    throws_ok {
        $surface->_validate_sought_values(undef, undef);
    }
    qr/Days\S+ or sought_point\S+ is undefined/, 'undefined day.';
    throws_ok {
        $surface->_validate_sought_values(7, undef);
    }
    qr/Days\S+ or sought_point\S+ is undefined/, 'undefined sought_point.';
    throws_ok {
        $surface->_validate_sought_values(7, 'chicken');
    }
    qr/must be a number/, 'sought_point is not a number.';
    throws_ok {
        $surface->_validate_sought_values(0, 7);
    }
    qr/requires positive numeric days\S+ and sought_point/, 'day is zero.';
    throws_ok {
        $surface->_validate_sought_values(7, -1);
    }
    qr/requires positive numeric days\S+ and sought_point/, 'sought_point is negative.';
    cmp_ok(scalar @{$surface->smile_points}, '==', 0, 'Surface with no smiles (very sad).');
};

subtest '_get_points_to_interpolate' => sub {
    plan tests => 15;
    my $surface = _get_surface();

    throws_ok { $surface->_get_points_to_interpolate(7, []) } qr/Need 2 or more/, "throws exception if there's no available points to interpolate";
    throws_ok { $surface->_get_points_to_interpolate(7, [1]) } qr/Need 2 or more/, "throws exception if there's only 1 term structure available";
    lives_ok { $surface->_get_points_to_interpolate(7, [1, 2]) } "can _get_points_to_interpolate with at least two available points";

    my @points;
    lives_ok { @points = $surface->_get_points_to_interpolate(7, [1, 2, 3]) }
    "get the last two points in the array of available points if the seek point is larger than max of availale points";
    is(scalar @points, 2, 'only return 2 closest points with _get_points_to_interpolate');
    is($points[0],     2, "correct first point");
    is($points[1],     3, "correct second point");

    lives_ok { @points = $surface->_get_points_to_interpolate(1, [4, 2, 3]) }
    "get the first two points in the array of avaialble points if the seek point is smaller than min of availale points";
    is(scalar @points, 2, 'only return 2 closest points with _get_points_to_interpolate');
    is($points[0],     2, "correct first point");
    is($points[1],     3, "correct second point");

    lives_ok { @points = $surface->_get_points_to_interpolate(5, [4, 6, 3]) } "get points in between";
    is(scalar @points, 2, 'only return 2 closest points with _get_points_to_interpolate');
    is($points[0],     4, "correct first point");
    is($points[1],     6, "correct second point");
};

subtest _is_between => sub {
    plan tests => 5;
    my $surface = _get_surface();

    lives_ok { $surface->_is_between(2, [1, 3]) } "can call _is_between";
    throws_ok { $surface->_is_between(2, [1]) } qr/less than two available points/, 'throws exception when available points is less that 2';
    throws_ok { $surface->_is_between(2, [1, undef]) } qr/some of the points are not defined/,
        'throws exception if at least one of the points are not defined';
    ok($surface->_is_between(2, [1, 3]), "returns true if seek is between available points");
    ok(!$surface->_is_between(4, [1, 2]), 'returns false if seek if not in between available points');
};

subtest _is_tenor => sub {
    plan tests => 3;

    lives_ok { Quant::Framework::VolSurface::_is_tenor('1W') } 'can call _is_tenor';
    ok(!Quant::Framework::VolSurface::_is_tenor(3),   'returns false if not tenor');
    ok(Quant::Framework::VolSurface::_is_tenor('2M'), 'returns true if tenor');
};

subtest 'Private method _get_initial_rr' => sub {
    plan tests => 2;

    my $ul                 = Quant::Framework::Utils::Test::create_underlying_config('frxEURUSD');
    my $surface            = _get_surface({underlying_config => $ul});
    my $first_market_point = $surface->original_term_for_smile->[0];
    my $market             = $surface->get_market_rr_bf($first_market_point);
    my %initial_rr         = %{$surface->_get_initial_rr($market)};
    is($initial_rr{RR_25}, 0.1 * $market->{RR_25}, 'correct interpolated RR');

    $ul                 = Quant::Framework::Utils::Test::create_underlying_config('frxEURUSD');
    $surface            = _get_surface({underlying_config => $ul});
    $first_market_point = $surface->original_term_for_smile->[0];
    $market             = $surface->get_market_rr_bf($first_market_point);
    %initial_rr         = %{$surface->_get_initial_rr($market)};
    ok(looks_like_number($initial_rr{RR_25}), 'Got reasonable RR_25 for Index.');
};

subtest fetch_historical_surface_date => sub {
    plan tests => 3;

    my $underlying = Quant::Framework::Utils::Test::create_underlying_config('frxUSDJPY');
    # Save a couple of surface so that we have some history.
    Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            symbol            => 'frxUSDJPY',
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
            recorded_date     => Date::Utility->new->minus_time_interval('5m'),
        });

    my $surface = Quant::Framework::Utils::Test::create_doc(
        'volsurface_delta',
        {
            underlying_config => $underlying,
            symbol            => 'frxUSDJPY',
            recorded_date     => Date::Utility->new,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
        });

    my $dates = $surface->fetch_historical_surface_date({
        back_to => 100,
    });

    is(ref $dates, 'ARRAY', 'fetch_historical_surface_date returns an array ref.');

    my $contains_non_dates = grep { $_ !~ /^\d{4}\-\d{2}\-\d{2}/ } @{$dates};

    cmp_ok($contains_non_dates, '==', 0, 'All elements of fetch_historical_surface_date are Date::Utilitys.');

    $dates = $surface->fetch_historical_surface_date({
        back_to => 1,
    });
    is(scalar @{$dates}, 1, 'fetch_historical_surface_date going back only one revision.');
};

sub _get_surface {
    my $override = shift || {};
    my %override = %$override;
    my $ul       = Quant::Framework::Utils::Test::create_underlying_config(
        'frxUSDJPY',
        {
            default_interest_rate => 0.5,
            default_dividend_rate => 0.5
        });

    my $surface = Quant::Framework::VolSurface::Delta->new(
        underlying_config => $ul,
        recorded_date     => Date::Utility->new('20-Jun-12'),
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
        surface           => {
            ON => {
                smile => {
                    25 => 0.2,
                    50 => 0.1,
                    75 => 0.3
                },
                vol_spread => {50 => 0.2},
            },
            '1W' => {
                smile => {
                    25 => 0.224,
                    50 => 0.2,
                    75 => 0.35
                },
                vol_spread => {50 => 0.15},
            },
            '2W' => {
                smile => {
                    25 => 0.324,
                    50 => 0.3,
                    75 => 0.45
                },
                vol_spread => {50 => 0.1},
            },
        },
        %override,
    );

    return $surface;
}

done_testing;
