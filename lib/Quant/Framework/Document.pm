package Quant::Framework::Document;

use strict;
use warnings;
use Date::Utility;

use Moo::Role;

=head1 NAME

Quant::Framework::Document - Role, which binds data with Chronicle

=head1 DESCRIPTION

The class is responsible for Create, Loading and Save Q::F Objects via Data::Chronicle.
The data itself is a hash, which content is provided by users of the class (i.e. by
CorporateActions).

The role provides C<load> (C<load_default>), C<create> (C<create_default>) and C<save>
methods. The C<*_default> varians are provided with the intention to allow override
the original methods with possibility still to use the default impementation. This
might be needed to specialized symbol-less objects like Holiday.


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

=head2 namespace

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

=head1 REQUIED METHODS

=head2 namespace()

returns namespace (string) for the object, which the role is applied to,
e.g. "corporate_actions" or "holidays"

=head2 initialize_data()

Returns initialized hash ref for data.  For example for Holidays, it can
be C<{ calendar => {} }>. The fields C<symbol> and C<date> are reserved,
please, do not fill them.

=cut

requires 'namespace';

requires 'initialize_data';

=head1 METHODS

=head2 create($package, %data)

=head2 create_default($package, %data)


Creates new unsaved (non-persisted) object, to which the role is applied
to. The C<create> can be overriden, while The C<create_default> not.
They have the same impelemtation.


  Quant::Framework::CorporateAction->create(
    storage_accessor => $storage_accessor,
    symbol           => 'USAAPL',
    for_date         => $date,
  );

Please note, it should be invoked with package name (the module, to which the
role is applied to), otherwise it will not work.

All paremeters are required.


=head2 load($package, %data)

=head2 load_default($package, %data)

Loads persiseted object. All paramters are mandatory, except
C<$for_date> which is optional. If C<$for_date> is not specified,
it loads the last stored object.

In case, when object does not exist, it returns C<undef>.

The C<load> can be overriden, while The C<load_default> not.
They have the same impelemtation.


  Quant::Framework::CorporateAction->load(
    storage_accessor => $storage_accessor,
    symbol           => 'USAAPL',
  )

Please note, it should be invoked with package name (the module, to which the
role is applied to), otherwise it will not work.

=cut

sub create_default {
    my ($package, %params) = @_;

    my $storage_accessor = $params{storage_accessor} // die("missing mandatory parameter: storage_accessor");
    my $symbol           = $params{symbol}           // die("missing mandatory parameter: symbol");
    my $for_date         = $params{for_date}         // die("missing mandatory parameter: for_date");

    my $data = $package->initialize_data;
    die("$package->initialize_data must return an hashref") unless ref($data) eq 'HASH';
    die("$package->initialize_data must not fill 'date' field")   if exists $data->{date};
    die("$package->initialize_data must not fill 'symbol' field") if exists $data->{symbol};

    my $obj = $package->new(
        storage_accessor => $storage_accessor,
        recorded_date    => $for_date,
        symbol           => $symbol,
        data             => $data,
    );
    return $obj;
}
*create = \&create_default;

sub load_default {
    my ($package, %params) = @_;

    my $storage_accessor = $params{storage_accessor} // die("missing mandatory parameter: storage_accessor");
    my $symbol           = $params{symbol}           // die("missing mandatory parameter: symbol");
    # optional
    my $for_date = $params{for_date};

    my $namespace = $package->namespace;

    my $data = $storage_accessor->chronicle_reader->get($namespace, $symbol)
        or return;

    my $recorded_date = Date::Utility->new($data->{date});

    if ($for_date && $for_date->epoch < $recorded_date->epoch) {
        $data = $storage_accessor->chronicle_reader->get_for($namespace, $symbol, $for_date->epoch)
            or return;

        $recorded_date = Date::Utility->new($data->{date});
    }

    return $package->new(
        storage_accessor => $storage_accessor,
        recorded_date    => $recorded_date,
        symbol           => $symbol,
        data             => $data,
    );
}
*load = \&load_default;

=head2 save

Stores (persists) the object in Chronicle database.

=cut

sub save {
    my $self = shift;
    # the most probably this is redundant, and can be removed in future
    $self->data->{date}   = $self->recorded_date->datetime_iso8601;
    $self->data->{symbol} = $self->symbol;
    $self->storage_accessor->chronicle_writer->set($self->namespace, $self->symbol, $self->data, $self->recorded_date);
    return;
}

1;
