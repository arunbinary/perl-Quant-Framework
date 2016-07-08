use Test::MockTime qw/:all/;
use Test::Most qw(-Test::Deep);
use Scalar::Util qw( looks_like_number );
use Test::MockObject::Extends;
use Test::FailWarnings;
use Test::MockModule;
use File::Spec;
use JSON qw(decode_json);

use Date::Utility;
use Quant::Framework::Utils::Test;
use Quant::Framework::VolSurface::Moneyness;

my ($chronicle_r, $chronicle_w) = Data::Chronicle::Mock::get_mocked_chronicle();
my $underlying_config = Quant::Framework::Utils::Test::create_underlying_config('HSI');
$underlying_config->{spot} = 100;

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
    }) for (qw/HKD USD/);

Quant::Framework::Utils::Test::create_doc(
    'index',
    {
        symbol           => 'SPC',
        date             => Date::Utility->new,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
    });

subtest clone => sub {
    plan tests => 5;
    my $now     = Date::Utility->new('2012-06-14 08:00:00');
    my $surface = {
        'ON' => {smile => {100 => 0.1}},
        '1W' => {smile => {100 => 0.2}}};
    my $volsurface = Quant::Framework::VolSurface::Moneyness->new(
        underlying_config => $underlying_config,
        spot_reference    => $underlying_config->spot,
        surface           => $surface,
        recorded_date     => $now,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    );

    lives_ok { $volsurface->clone } 'Can clone Quant::Framework::VolSurface::Moneyness';
    my $clone;
    lives_ok { $clone = $volsurface->clone({surface => {ON => {smile => {100 => 0.5}}}}) };
    isa_ok($clone, 'Quant::Framework::VolSurface::Moneyness');
    is($clone->surface->{1}->{smile}->{100}, 0.5, 'can change attribute value when clone');

    #change spot refernce by 10 pips
    my $spot_reference = $underlying_config->spot - 0.1;
    $clone = $volsurface->clone({spot_reference => $spot_reference});

    cmp_ok($clone->spot_reference, '==', $spot_reference, 'Adjusted spot ref preserved through clone.');
};

subtest 'get available strikes on surface' => sub {
    plan tests => 2;
    my $now     = Date::Utility->new('2012-06-14 08:00:00');
    my $surface = {
        'ON' => {smile => {100 => 0.1}},
        '1W' => {smile => {100 => 0.2}}};
    my $volsurface = Quant::Framework::VolSurface::Moneyness->new(
        underlying_config => $underlying_config,
        spot_reference    => $underlying_config->spot,
        surface           => $surface,
        recorded_date     => $now,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    );
    my $moneyness_points;
    lives_ok { $moneyness_points = $volsurface->moneynesses } 'can call moneynesses';
    is_deeply($moneyness_points, [100], 'get correct value for moneyness points');
};

subtest 'get surface spot reference' => sub {
    plan tests => 3;
    my $date = Date::Utility->new('2012-06-14 08:00:00');

    my $surface = {
        'ON' => {smile => {100 => 0.1}},
        '1W' => {smile => {100 => 0.2}},
    };
    my $volsurface = Quant::Framework::VolSurface::Moneyness->new(
        underlying_config => $underlying_config,
        surface           => $surface,
        recorded_date     => $date,
        spot_reference    => 100,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    );

    my $spot;
    lives_ok { $spot = $volsurface->spot_reference } 'can call spot reference of the surface';
    is($spot, 100, 'Got what I put in');
    ok(looks_like_number($spot), 'spot is a number');
};

subtest _convert_strike_to_delta => sub {
    plan tests => 3;

    my $underlying_config = Quant::Framework::Utils::Test::create_underlying_config('SPC');
    $underlying_config->{spot} = 100;

    my $surface = {
        'ON' => {smile => {100 => 0.1}},
        '1W' => {smile => {100 => 0.2}}};
    my $volsurface = Quant::Framework::VolSurface::Moneyness->new(
        underlying_config => $underlying_config,
        surface           => $surface,
        recorded_date     => Date::Utility->new('2012-06-14 08:00:00'),
        spot_reference    => 100,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    );
    my $args = {
        strike => 100,
        days   => 7,
        vol    => 0.11
    };
    my $delta;
    lives_ok { $delta = $volsurface->_convert_strike_to_delta($args) } 'can convert strike to delta';
    ok(looks_like_number($delta), 'delta is a number');
    cmp_ok($delta, '<=', 100, 'delta is <= 100');
};

done_testing;
