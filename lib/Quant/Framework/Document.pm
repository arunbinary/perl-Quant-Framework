package Quant::Framework::Document;

use strict;
use warnings;
use Date::Utility;

use Moo::Role;

=head1 NAME

Quant::Framework::Document - Binds data with Chronicle

=head1 DESCRIPTION

Internal representation of persistend data. Do not B<create> the class directly outside
of Quant::Framework, although the usage of public fields outside of Quant::Framework
is allowed.

The class is responsible for loading and storing data via Data::Chronicle. The
data itself is a hash, which content is provided by users of the class (i.e. by
CorporateActions).

 # create new (transient / not-yet-persisted) Document

 my $document = Quant::Framework::Document->new(
  storage_accessor => $storage_accessor,
  symbol           => 'frxUSDJPY',
  data             => {},
  for_date         => Date::Utility->new,
 );

 # persist document
 $document->save('currency');

 # load document
 my $document2 = Quant::Framework::Document::load(
  $storage_accessor,
  'currency',
  'frxUSDJPY',
  Date::Utility->new, # optional
 )

=cut

=head1 ATTRIBUTES

=head2 storage_accessor

Chronicle assessor

=head2 recorded_date

The date of document creation (C<Date::Utility>)

=head2 data

Hashref of data. Should be defined by the class, which uses Document. Currently the fields
C<date> and C<symbol> are reserved.


=head2 symbol

The domain-specific name of document; e.g. "USAAPL" for corporate actions

=cut

has storage_accessor => (
    is       => 'ro',
    required => 1,
);

has recorded_date => (
    is  => 'ro',
    isa => sub {
        die("Quant::Framework::Document::recorded_date should be Date::Utility")
            unless ref($_[0]) eq 'Date::Utility';
    },
    required => 1,
);

has data => (
    is       => 'ro',
    required => 1,
);

has symbol => (
    is       => 'ro',
    required => 1,
);

requires 'namespace';

requires 'default_section';

sub create {
    my ($package, $storage_accessor, $symbol, $for_date) = @_;
    my $obj = $package->new(
        storage_accessor => $storage_accessor,
        recorded_date    => $for_date,
        symbol           => $symbol,
        data             => {$package->default_section => {}},
    );
}

sub load {
    my ($package, $storage_accessor, $symbol, $for_date) = @_;

    my $namespace = $package->namespace;

    my $data = $storage_accessor->chronicle_reader->get($namespace, $symbol)
        or return;

    if ($for_date && $for_date->datetime_iso8601 lt $data->{date}) {
        $data = $storage_accessor->chronicle_reader->get_for($namespace, $symbol, $for_date->epoch)
            or return;
    }

    return $package->new(
        storage_accessor => $storage_accessor,
        recorded_date    => $for_date // Date::Utility->new($data->{date}),
        symbol           => $symbol,
        data             => $data,
    );
}

=head2 save

 $document->save;

Stores (persists) the document in Chronicle database.

=cut

sub save {
    my $self = shift;
    # the most probably this is redundant, anc can be removed in future
    $self->data->{date}   = $self->recorded_date->datetime_iso8601;
    $self->data->{symbol} = $self->symbol;
    $self->storage_accessor->chronicle_writer->set($self->namespace, $self->symbol, $self->data, $self->recorded_date);
}

1;
