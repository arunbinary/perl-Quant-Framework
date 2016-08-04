use strict;
use warnings;

use Test::Most qw(-Test::Deep);
use Test::MockObject::Extends;
use Test::FailWarnings;
use Test::MockModule;
use File::Spec;
use JSON qw(decode_json);

use Date::Utility;
use Scalar::Util qw(looks_like_number);
use Quant::Framework::Utils::Test;
use Quant::Framework::VolSurface::Moneyness;

my ($chronicle_r, $chronicle_w) = Data::Chronicle::Mock::get_mocked_chronicle();
my $underlying_config = Quant::Framework::Utils::Test::create_underlying_config('SPC');

#instead of mocking Builder's *_rate_for, we set default rates here
$underlying_config->{default_dividend_rate} = 0.5;
$underlying_config->{default_interest_rate} = 0.5;

Quant::Framework::Utils::Test::create_doc(
    'volsurface_moneyness',
    {
        underlying_config => $underlying_config,
        recorded_date     => Date::Utility->new,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    });

Quant::Framework::Utils::Test::create_doc(
    'currency',
    {
        symbol           => $_,
        date             => Date::Utility->new,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
    }) for (qw/USD EUR/);

my $recorded_date = Date::Utility->new;

my $surface = {
    7 => {
        smile => {
            90  => 0.313,
            95  => 0.2848,
            100 => 0.2577,
            105 => 0.2335,
            110 => 0.143,
        },
        vol_spread => {100 => 0.1}
    },
    9 => {
        smile => {
            90  => 0.3007,
            95  => 0.378,
            100 => 0.2563,
            105 => 0.2364,
            110 => 0.2187,
        },
        vol_spread => {100 => 0.1}
    },
};

my $v = Quant::Framework::VolSurface::Moneyness->new(
    recorded_date     => $recorded_date,
    underlying_config => $underlying_config,
    spot_reference    => 100,
    surface           => $surface,
    chronicle_reader  => $chronicle_r,
    chronicle_writer  => $chronicle_w,
);

subtest "get_vol for term structure that exists on surface" => sub {
    plan tests => 6;

    lives_ok { $v->get_volatility({days => 7, moneyness => 90}) } "can get volatility from smile";
    is(
        $v->get_volatility({
                days      => 7,
                moneyness => 90
            }
        ),
        0.313,
        "returns vol for moneyness point if exist on surface"
    );

    my $linearly_interpolated;
    lives_ok { $linearly_interpolated = $v->get_volatility({days => 7, moneyness => 93}) } "can interpolate across moneyness smile";
    cmp_ok($linearly_interpolated, '<', 0.313,  "vol is smaller than first point");
    cmp_ok($linearly_interpolated, '>', 0.2848, "vol is larger than second point");
    ok(!exists $v->get_smile(7)->{93}, "doesn't save interpolated smile point on smile");
};

subtest 'Interpolating down.' => sub {
    plan tests => 1;

    my $underlying_config = Quant::Framework::Utils::Test::create_underlying_config('GDAXI');
    my $surface           = Quant::Framework::Utils::Test::create_doc(
        'volsurface_moneyness',
        {
            underlying_config => $underlying_config,
            recorded_date     => Date::Utility->new,
            chronicle_reader  => $chronicle_r,
            chronicle_writer  => $chronicle_w,
        });

    ok(
        looks_like_number(
            $surface->get_volatility({
                    days      => 0.5,
                    moneyness => 90,
                })
        ),
        'Can get reasonable vol when interpolating down.'
    );
};

subtest "get_vol for interpolated term structure" => sub {
    plan tests => 7;

    my $underlying_config = Quant::Framework::Utils::Test::create_underlying_config('GDAXI');
    my $surface           = Quant::Framework::Utils::Test::create_doc(
        'volsurface_moneyness',
        {
            underlying_config => $underlying_config,
            surface           => {
                7 => {
                    smile => {
                        90  => 0.2,
                        100 => 0.2,
                        110 => 0.2
                    }}
            },
            recorded_date    => Date::Utility->new,
            chronicle_reader => $chronicle_r,
            chronicle_writer => $chronicle_w,
        });

    cmp_ok(scalar @{$surface->original_term_for_smile}, '==', 1, "Surface's original_term_for_smile.");
    throws_ok { $surface->get_volatility({days => 8, moneyness => 90}) } qr/Need 2 or more/i,
        "cannot interpolate with only one term structure on surface";

    my $vol;

    lives_ok { $vol = $v->get_volatility({days => 8, moneyness => 90}) } "can get volatility for term that doesn't exist on surface";
    cmp_ok($vol, '<', 0.313,  "vol is smaller than first point");
    cmp_ok($vol, '>', 0.2848, "vol is larger than second point");
    ok(exists $v->get_smile(8)->{90}, "interpolated smile is saved on surface");

    is(scalar keys %{$v->surface}, 3, "successfully added one smile on surface");
};

subtest "get_vol for a smile that has a single point" => sub {
    plan
        tests => 1,

        $v->clear_smile_points;
    $v = Test::MockObject::Extends->new($v);
    $v->mock('surface', sub { {7 => {smile => {80 => 0.1}}} });
    throws_ok { $v->get_volatility({days => 7, moneyness => 70}) } qr/cannot interpolate/i, "cannot interpolate with one point on smile";
    $v->unmock('surface');
};

subtest "get_vol for delta" => sub {
    plan tests => 10;

    my $new_v = Quant::Framework::VolSurface::Moneyness->new(
        recorded_date     => $recorded_date,
        underlying_config => $underlying_config,
        spot_reference    => 100,
        surface           => $surface,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    );

    ok(!exists $new_v->corresponding_deltas->{8}, "corresponding_deltas does not exist before request");
    lives_ok { $new_v->get_volatility({delta => 50, days => 8}) } "can get_vol for 50 delta";
    is(scalar keys %{$new_v->corresponding_deltas}, 1, "1 delta smile added");
    ok(exists $new_v->corresponding_deltas->{8}, "calculated delta smile is saved on corresponding_deltas");

    ok(!exists $new_v->corresponding_deltas->{8.5}, "corresponding_deltas does not exist before request");
    lives_ok { $new_v->get_volatility({delta => 50, days => 8.5}) } "can get_vol for 50 delta";
    is(scalar keys %{$new_v->corresponding_deltas}, 2, "2 delta smiles added");
    ok(exists $new_v->corresponding_deltas->{8.5}, "calculated delta smile is saved on corresponding_deltas");

    lives_ok { $new_v->get_volatility({delta => 0.05, days => 7}) } "can get_vol for 0.05 delta";
    lives_ok { $new_v->get_volatility({delta => 99,   days => 7}) } "can get_vol for 99 delta";
};

subtest 'get_vol for term less than the available term on surface' => sub {
    plan tests => 4;

    my $volsurface = Quant::Framework::VolSurface::Moneyness->new(
        recorded_date     => $recorded_date,
        underlying_config => $underlying_config,
        spot_reference    => 100,
        surface           => $surface,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    );

    my $vol;
    lives_ok { $vol = $volsurface->get_volatility({days => 1, moneyness => 100}) }
    'can get volatility for term less than the smallest term of the surface';
    ok(exists $volsurface->surface->{1}, 'does not extrapolate smile');
    is($vol, $volsurface->surface->{7}->{smile}->{100}, "returns the correct vol from the smallest term's smile");
    lives_ok { $volsurface->get_market_rr_bf(1) } 'can get market_rr_bf for extrapolated smile';
};

done_testing;
