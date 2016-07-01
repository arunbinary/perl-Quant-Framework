use strict;
use warnings;

use Cwd qw( abs_path );
use Data::Dumper;
use File::Basename qw( dirname );
use List::Util qw( max sum );
use Test::More (tests => 7);
use Test::NoWarnings;
use Test::Exception;
use Text::SimpleTable;
use Test::MockModule;
use File::Spec;
use JSON qw(decode_json);

use lib abs_path(dirname(__FILE__));
use BloombergSurfaces;
use ClarkSurfaces;

use Quant::Framework::Currency;
use Quant::Framework::Utils::Test;
use Quant::Framework::VolSurface::Delta;
use Quant::Framework::Exchange;
use Quant::Framework::TradingCalendar;

my ($chronicle_r, $chronicle_w) = Data::Chronicle::Mock::get_mocked_chronicle();

Quant::Framework::Utils::Test::create_doc(
    'currency',
    {
        symbol           => $_,
        date             => Date::Utility->new,
        chronicle_reader => $chronicle_r,
        chronicle_writer => $chronicle_w,
    }) for (qw/EUR GBP JPY USD/);

my $tabular = (scalar @ARGV and $ARGV[0] eq 'tabular');

# Clear all holidays and weights before preceding, as the
# surfaces tested against did not take any into account.
Quant::Framework::Currency->new({
    symbol           => 'EUR',
    chronicle_reader => $chronicle_r,
    chronicle_writer => $chronicle_w,
});
Quant::Framework::Currency->new({
    symbol           => 'USD',
    chronicle_reader => $chronicle_r,
    chronicle_writer => $chronicle_w,
});
Quant::Framework::Currency->new({
    symbol           => 'JPY',
    chronicle_reader => $chronicle_r,
    chronicle_writer => $chronicle_w,
});
Quant::Framework::Exchange->new('FOREX');

my $bbss = BloombergSurfaces->new({
    relative_data_dir => 'interpolation',
    chronicle_reader  => $chronicle_r,
    chronicle_writer  => $chronicle_w,
});

my $clss = ClarkSurfaces->new({
    chronicle_reader => $chronicle_r,
    chronicle_writer => $chronicle_w,
});

my @surfaces = (
    $bbss->get('frxUSDJPY', '2012-01-11 02:40:00'),
    $bbss->get('frxEURUSD', '2012-01-13 02:40:00'),
    $bbss->get('frxUSDJPY', '2012-01-13 02:43:00'),
    $bbss->get('frxEURUSD', '2012-01-17 23:15:00'),
    $bbss->get('frxUSDJPY', '2012-01-17 23:10:00'),
    $clss->get,
);

my %tolerance = (
    max  => 0.005,
    mean => 0.0013,
);

foreach my $surface (@surfaces) {
    my $results = get_results($surface);

    if ($tabular) {
        print $surface->underlying->symbol . ', ' . $surface->recorded_date->datetime_yyyymmdd_hhmmss;
        print show_table_of($results);
    } else {
        run_tests_on($results);
    }
}

sub get_results {
    my $source_full   = shift;
    my $source_market = _get_market_points_only_surface_from($source_full);

    my @maturitites = @{$source_full->term_by_day};
    my @deltas      = @{$source_full->deltas};

    my $output = {};

    MATURITY:
    foreach my $maturity (@maturitites) {

        foreach my $delta (@deltas) {
            $output->{$maturity}->{rmg}->{$delta} = sprintf(
                '%.5f',
                $source_market->get_volatility({
                        delta => $delta,
                        days  => $maturity
                    }));
            $output->{$maturity}->{source}->{$delta} = sprintf(
                '%.5f',
                $source_full->get_volatility({
                        delta => $delta,
                        days  => $maturity
                    }));
        }
        $output->{$maturity}->{market_point} = scalar grep { $_ == $maturity } @{$source_market->original_term_for_smile};
    }

    return $output;
}

# Runs tests on a set of given results.
sub run_tests_on {
    my $results = shift;
    my @errors;

    MATURITY:
    foreach my $maturity (keys %{$results}) {
        next MATURITY if ($results->{$maturity}->{market_point});

        foreach my $delta (keys %{$results->{$maturity}->{rmg}}) {
            my $rmg    = $results->{$maturity}->{rmg}->{$delta};
            my $source = $results->{$maturity}->{source}->{$delta};
            my $error  = abs($rmg - $source);
            push @errors, $error;

            cmp_ok($error, '<=', $tolerance{max},
                'mat[' . $maturity . '] delta[' . $delta . '] error[' . $error . '] within ' . $tolerance{max} . '.');
        }
    }

    my $mean = (not scalar @errors) ? 0 : sum(@errors) / scalar(@errors);
    cmp_ok($mean, '<=', $tolerance{mean}, 'mean is within tolerance');

    return;
}

# Given a set of results, displays them in an ASCII table.
sub show_table_of {
    my $results = shift;

    my @maturities = keys %{$results};
    my @deltas     = keys %{$results->{$maturities[0]}->{rmg}};

    my %count = (
        equal => 0,
        close => 0,
        miss  => 0
    );
    my @errors;
    my $close = 0.001;

    # setting up the tabular output.
    my $last_index = scalar(@deltas) - 1;
    my $table = Text::SimpleTable->new(3, map { 32 } (0 .. $last_index));
    $table->row(' ', map { $deltas[$_] } (0 .. $last_index));
    $table->hr;

    foreach my $maturity (sort { $a <=> $b } @maturities) {

        my (%rmg, %source, %error);

        foreach my $delta (@deltas) {
            $rmg{$delta}    = $results->{$maturity}->{rmg}->{$delta};
            $source{$delta} = $results->{$maturity}->{source}->{$delta};

            $error{$delta} = sprintf('%.5f', abs $rmg{$delta} - $source{$delta});

            if (not $results->{$maturity}->{market_point}) {
                push @errors, $error{$delta};

                if ($rmg{$delta} == $source{$delta}) {
                    $count{equal} += 1;
                } elsif (abs($rmg{$delta} - $source{$delta}) <= $close) {
                    $count{close} += 1;
                } else {
                    $count{miss} += 1;
                }
            }
        }
        $table->row(
            $maturity,
            map {
                      'RMG:'
                    . $rmg{$deltas[$_]} . ' SRC:'
                    . $source{$deltas[$_]} . ' '
                    . ($results->{$maturity}->{market_point} ? 'MK' : $error{$deltas[$_]})
            } (0 .. $last_index));
    }

    my $total_interps = $count{equal} + $count{close} + $count{miss};
    my $output        = ''
        . $table->draw
        . 'Totals: EQUAL: '
        . $count{equal} . '/'
        . $total_interps
        . ', WITHIN '
        . ($close * 100)
        . ' VOLPOINT: '
        . $count{close} . '/'
        . $total_interps
        . ', OUTWITH '
        . ($close * 100)
        . ' VOLPOINT: '
        . $count{miss} . '/'
        . $total_interps . ', '
        . 'MEAN ERROR: '
        . sprintf('%.5f', (sum(@errors) / $total_interps)) . ', '
        . 'MAX ERROR: '
        . max(@errors) . "\n\n";

    return $output;
}

sub _reduce_for_extrapolation_test {
    my $surface = shift;

    my %surface_data        = %{$surface->surface};
    my %extrap_surface_data = %surface_data;

    return $surface->clone({
            surface       => \%extrap_surface_data,
            market_points => {
                smile      => [7, 30, 60, 90],
                vol_spread => [7, 30, 60, 90],
            },
        });
}

# Pulls market smiles out of a given surface and returns a
# new surface with only those market smiles.
sub _get_market_points_only_surface_from {
    my $surface = shift;

    my @market_points = @{$surface->original_term_for_smile};
    my %surface_data  = %{$surface->surface};

    my %mpo_surface;
    @mpo_surface{@market_points} = @surface_data{@market_points};

    my $mpo_surface = Quant::Framework::VolSurface::Delta->new(
        surface           => \%mpo_surface,
        underlying_config => $surface->underlying_config,
        recorded_date     => $surface->recorded_date,
        chronicle_reader  => $chronicle_r,
        chronicle_writer  => $chronicle_w,
    );

    return $mpo_surface;
}

1;
