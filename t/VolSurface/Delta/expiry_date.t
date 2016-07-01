use Test::Most;
use Test::MockModule;
use File::Spec;
use JSON qw(decode_json);

use Quant::Framework::Utils::Test;
use Quant::Framework::VolSurface::Delta;
use Date::Utility;

my ($chronicle_r, $chronicle_w) = Data::Chronicle::Mock::get_mocked_chronicle();
my $underlying_config = Quant::Framework::Utils::Test::create_underlying_config('frxEURUSD');

Quant::Framework::Utils::Test::create_doc(
    'currency',
    {
        symbol           => 'USD',
        date             => Date::Utility->new,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
    });

subtest 'get_volatility for expiry_date: Surface date Monday before NY5pm.' => sub {
    plan tests => 2;

    my $surface = _sample_surface({recorded_date => Date::Utility->new('2012-01-23 20:00:00')});

    my $jan24vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-24 23:59:59'),
        delta       => 50
    });
    my $jan25vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-25 23:59:59'),
        delta       => 50
    });
    my $twodayvol = $surface->get_volatility({
        days  => 2,
        delta => 50
    });
    my $threedayvol = $surface->get_volatility({
        days  => 3,
        delta => 50
    });

    is($jan24vol, $twodayvol,   'Jan 24 vol is the 2 day vol.');
    is($jan25vol, $threedayvol, 'Jan 25 vol is the 3 day vol.');
};

subtest 'get_volatility for expiry_date: Surface date Monday after NY5pm.' => sub {
    plan tests => 4;

    my $surface = _sample_surface({recorded_date => Date::Utility->new('2012-01-23 23:00:00')});

    my $onedayvol = $surface->get_volatility({
        days  => 1,
        delta => 50
    });
    my $twodayvol = $surface->get_volatility({
        days  => 2,
        delta => 50
    });
    my $threedayvol = $surface->get_volatility({
        days  => 3,
        delta => 50
    });

    my $jan23vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-23 23:59:59'),
        delta       => 50
    });
    is($jan23vol, $onedayvol, 'Jan 23 vol is the 1 day vol.');

    my $jan24vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-24 23:59:59'),
        delta       => 50
    });
    is($jan24vol, $onedayvol, 'Jan 24 vol is the 1 day vol.');

    my $jan25vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-25 23:59:59'),
        delta       => 50
    });
    is($jan25vol, $twodayvol, 'Jan 25 vol is the 2 day vol.');

    my $jan26vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-26 23:59:59'),
        delta       => 50
    });
    is($jan26vol, $threedayvol, 'Jan 26 vol is the 3 day vol.');
};

subtest 'get_volatility for expiry_date: Surface date Tuesay shortly after midnight GMT.' => sub {
    plan tests => 2;

    my $surface = _sample_surface({recorded_date => Date::Utility->new('2012-01-24 01:00:00')});

    my $jan25vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-25 23:59:59'),
        delta       => 50
    });
    my $jan26vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-26 23:59:59'),
        delta       => 50
    });
    my $twodayvol = $surface->get_volatility({
        days  => 2,
        delta => 50
    });
    my $threedayvol = $surface->get_volatility({
        days  => 3,
        delta => 50
    });

    is($jan25vol, $twodayvol,   'Jan 25 vol is the 2 day vol.');
    is($jan26vol, $threedayvol, 'Jan 26 vol is the 3 day vol.');
};

subtest 'Fridays: early close.' => sub {
    plan tests => 4;

    # This may seem wrong at first. Why are both the Jan 26 & 27 vols the 3-day vol?
    # In practice, the Friday expiry (on FX, as we are here), is to GMT21:00, so
    # we'll have cut the surface forward to GMT21:00 on the same effective day of the
    # vol. When we cut to the usual FX expiry of GMT23:59, the vol is cut backwards
    # into the previous GMT day.
    # So, we expect both to be the same vol, but in practice we'll have cut to different
    # vols beforehand.
    my $surface = _sample_surface({recorded_date => Date::Utility->new('2012-01-24 01:00:00')});

    my $jan25vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-25'),
        delta       => 50
    });
    my $jan26vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-26'),
        delta       => 50
    });
    my $jan27vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-27'),
        delta       => 50
    });
    my $jan30vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-01-30'),
        delta       => 50
    });

    my $onedayvol = $surface->get_volatility({
        days  => 1,
        delta => 50
    });
    my $twodayvol = $surface->get_volatility({
        days  => 2,
        delta => 50
    });
    my $threedayvol = $surface->get_volatility({
        days  => 3,
        delta => 50
    });
    my $sixdayvol = $surface->get_volatility({
        days  => 6,
        delta => 50
    });

    is($jan25vol, $onedayvol,   'Jan 25 is the 1 day vol');
    is($jan26vol, $twodayvol,   'Jan 26 is the 2 day vol');
    is($jan27vol, $threedayvol, 'Jan 27 (Friday) is the 3 day vol');
    is($jan30vol, $sixdayvol,   'Jan 30 is the 6 day vol');
};

subtest 'get_volatility by tenor.' => sub {
    plan tests => 7;

    my $surface = _sample_surface({recorded_date => Date::Utility->new('2012-03-14 01:00:00')});

    throws_ok { $surface->get_volatility({tenor => 'ON', days => 1, delta => 50}) } qr/Must pass exactly one of/i,
        'Cannot ask for vol for both tenor and days.';

    lives_ok { $surface->get_volatility({tenor => '1M', delta => 50}) } 'Can get vol for tenor, even if surface does not have it.';

    my $volON = $surface->get_volatility({
        tenor => 'ON',
        delta => 50
    });
    my $vol1day = $surface->get_volatility({
        days  => 1,
        delta => 50
    });
    is($volON, $vol1day, 'ON is same as one day.');

    my $vol7D = $surface->get_volatility({
        tenor => '7D',
        delta => 50
    });
    my $vol7day = $surface->get_volatility({
        days  => 7,
        delta => 50
    });
    is($vol7D, $vol7day, '7D is same as seven days.');

    my $mar21vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-03-21'),
        delta       => 50
    });
    is($vol7D, $mar21vol, '7D is same as vol with expiry 2012-03-21.');

    my $vol3D = $surface->get_volatility({
        tenor => '3D',
        delta => 50
    });
    my $vol5day = $surface->get_volatility({
        days  => 5,
        delta => 50
    });
    is($vol3D, $vol5day, '3D is same as five days  ');

    my $vol1M = $surface->get_volatility({
        tenor => '1M',
        delta => 50
    });

    my $apr12vol = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-04-12'),
        delta       => 50
    });

    # Clark(2011), p. 8
    # 2012-03-14 -> spot date: 2012-03-16 -> delivery date: 2012-04-16 -> expiry date: 2012-04-12
    is($vol1M, $apr12vol, "1M is same as vol with expiry 2012-04-12, see Clark(2011), p. 8.");
};

subtest 'get_volatility after rollover' => sub {
    plan tests => 3;

    my $surface = _sample_surface({recorded_date => Date::Utility->new('2012-03-14 21:00:00')});

    # Clark(2011), p. 8
    # 2012-03-14 -> rolled over at 5 pm NY (9 pm UTC): 2012-03-15 -> spot date: 2012-03-19 -> delivery date: 2012-04-19 -> expiry date: 2012-04-17
    my $days = $surface->get_day_for_tenor('1M');

    # NOTE: in calendar days it is actually 34 days
    is($days, 33, '1M is equal to 33 days at 9 pm');

    my $vol_20120417 = $surface->get_volatility({
        expiry_date => Date::Utility->new('2012-04-17'),
        delta       => 50
    });

    my $vol_1M = $surface->get_volatility({
        tenor => '1M',
        delta => 50
    });

    my $vol_33 = $surface->get_volatility({
        days  => 33,
        delta => 50
    });

    is($vol_1M, $vol_33,       '1M is equal to 33 days at 9 pm');
    is($vol_1M, $vol_20120417, '1M is equal to 2012-04-17 at 9 pm');
};

sub _sample_surface {
    my @args = @_;

    my %surface_data = (
        1 => {
            smile => {
                25 => 0.11,
                50 => 0.12,
                75 => 0.13,
            },
            vol_spread => {50 => 0.03},
            tenor      => 'ON',
        },
        2 => {
            smile => {
                25 => 0.21,
                50 => 0.22,
                75 => 0.23,
            },
            vol_spread => {50 => 0.03},
        },
        3 => {
            smile => {
                25 => 0.31,
                50 => 0.32,
                75 => 0.33,
            },
            vol_spread => {50 => 0.03},
        },
        6 => {
            smile => {
                25 => 0.61,
                50 => 0.62,
                75 => 0.63,
            },
            vol_spread => {50 => 0.03},
        },
        7 => {
            smile => {
                25 => 0.71,
                50 => 0.72,
                75 => 0.73,
            },
            vol_spread => {50 => 0.03},
            tenor      => '7D',
        },
        30 => {
            smile => {
                25 => 0.81,
                50 => 0.82,
                75 => 0.83,
            },
            vol_spread => {50 => 0.03},
        },
    );

    my $surface = Quant::Framework::VolSurface::Delta->new(
        underlying_config => $underlying_config,
        surface           => \%surface_data,
        recorded_date     => Date::Utility->new,
        deltas            => [25, 50, 75],
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    );

    return $surface->clone(@args);
}

done_testing;
