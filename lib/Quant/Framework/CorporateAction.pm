package Quant::Framework::CorporateAction;

use strict;
use warnings;

use Date::Utility;
use Moo;

with('Quant::Framework::Document');

=head1 NAME

Quant::Framework::CorporateAction

=head1 DESCRIPTION

Represents the corporate actions data of an underlying from database. Create
new unpersistent CorporateAction can be done via:

 my $ca = Quant::Framework::CorporateAction->create(
    storage_accessor => $storage_accessor,
    symbol           => 'QWER',
    for_date         => Date::Utility->new,
  )

Obviously, it will have empty actions hash

 $ca->actions; # empty hashref by default

Corporate actions can be persisted (stored in Chronicle) via C<save> method

 $ca->save;

Load persisted actions can be done via:

 my $ca = Quant::Framework::CorporateAction->load(
    storage_accessor => $storage_accessor,
    symbol           => 'QWER',
    for_date         => Date::Utility->new,
 )

It will return C<undef> if there are no presisted actions in Chronicle. The date can be
ommitted, than, it will try to load the most recent (last) corporate actions, i.e.

 my $ca = Quant::Framework::CorporateAction->load(
    storage_accessor => $storage_accessor,
    symbol           => 'QWER',
 )


To update actions, the C<update> method should be invoked, with appropriate structure, i.e.

 my $ca = $ca->update({
   "62799500" => {
      "monitor_date" => "2014-02-07T06:00:07Z",
      "type" => "ACQUIS",
      "monitor" => 1,
      "description" =>  "Acquisition",
      "effective_date" =>  "15-Jul-14",
      "flag" => "N",
 }, Date::Utility->new);

The C<update> in scalar context will return B<new unpersisted> Corporate object. You have
to presist it via C<save> method.

In the list context it will return new unpersisted Corporate object, and two hasref for
new and cancelled action.

=head1 SUBROUTINES

=head2 namespace

returns hard-coded string 'corporate_actions'. Required to conform Document role contract.

=head2 initialize_data

returns hard-coded hash with 'actions'. Required to conform Document role contract.

=head2 update($actions, $date);

 my $new_corp_actions = $corp_actions->update({
    "32799500" => {
        "monitor_date" => "2015-02-07T06:00:07Z",
        "type" => "DIV",
        "monitor" => 1,
        "description" =>  "Divided Stocks",
        "effective_date" =>  "15-Jul-15",
        "flag" => "N"
    },
 }, Date::Utility->new)
 $new_corp_actions->save;

Takes the existing actions, applies "diff" of actions in Bloomberg
format (i.e. adds new actions, or cancels exising ones), and
returns new unpersisted (non-saved) CorporateAction object.

You have to invoke C<save> method to persist new corporate actions
in Chronicle.

The C<$date> argument in mandatory.

=cut

sub namespace {
    return 'corporate_actions';
}

sub initialize_data {
    return {actions => {}};
}

sub update {
    my ($self, $actions, $new_date) = @_;

    # clone original data
    my $original = $self->data;
    my $data     = {%$original};

    my %new;
    foreach my $action_id (keys %$actions) {
        # flag 'N' = New & 'U' = Update
        my $action = $actions->{$action_id};
        my $is_new = ($action->{flag} eq 'N' and not $original->{actions}->{$action_id})
            || $action->{flag} eq 'U';

        $new{$action_id} = $action if ($is_new);
    }

    my %merged_actions = (%{$data->{actions}}, %new);

    my %cancelled;
    foreach my $action_id (keys %$actions) {
        my $action = $actions->{$action_id};
        # flag 'D' = Delete
        if ($action->{flag} eq 'D' and $original->{actions}->{$action_id}) {
            $cancelled{$action_id} = $action;
            delete $merged_actions{$action_id};
        }
    }

    $data->{actions} = \%merged_actions;

    my $new_obj = __PACKAGE__->new(
        data             => $data,
        storage_accessor => $self->storage_accessor,
        recorded_date    => $new_date,
        symbol           => $self->symbol,
    );

    return wantarray ? ($new_obj, \%new, \%cancelled) : $new_obj;
}

=head2 actions

 my $actions = $corp_actions->actions;

Returns hashref of actions in Bloomberg format. If there are no actions, the empty hashref
is returned.

=cut

sub actions {
    return shift->data->{actions};
}

1;
