#!/usr/bin/perl -w
# This code is automatically generated.  Do not edit.


#### CBBlib::Acct slot methods ###########################

package CBBlib::Acct;
use strict;

sub make_internals_ {
  return [undef,
          undef,
          undef,
          undef,
          0,
          0,
          0];
}

sub get_db_ { my $self = shift; return $$self[0]; }
sub set_db_ { my($self, $val) = @_; $$self[0] = $val; }

sub get_db { my $self = shift; return $$self[0]; }

sub get_name_ { my $self = shift; return $$self[1]; }
sub set_name_ { my($self, $val) = @_; $$self[1] = $val; }

sub get_name { my $self = shift; return $$self[1]; }

sub set_name { my($self, $val) = @_; $$self[1] = $val; }

sub get_notes_ { my $self = shift; return $$self[2]; }
sub set_notes_ { my($self, $val) = @_; $$self[2] = $val; }

sub get_notes { my $self = shift; return $$self[2]; }

sub set_notes { my($self, $val) = @_; $$self[2] = $val; }

sub get_ledger_ { my $self = shift; return $$self[3]; }
sub set_ledger_ { my($self, $val) = @_; $$self[3] = $val; }

sub get_ledger_usage_count_ { my $self = shift; return $$self[4]; }
sub set_ledger_usage_count_ { my($self, $val) = @_; $$self[4] = $val; }

sub get_cleared_balance_ { my $self = shift; return $$self[5]; }
sub set_cleared_balance_ { my($self, $val) = @_; $$self[5] = $val; }

sub get_cleared_balance { my $self = shift; return $$self[5]; }

sub get_final_balance_ { my $self = shift; return $$self[6]; }
sub set_final_balance_ { my($self, $val) = @_; $$self[6] = $val; }

sub get_final_balance { my $self = shift; return $$self[6]; }


#### CBBlib::Cat slot methods ###########################

package CBBlib::Cat;
use strict;

sub make_internals_ {
  return [undef,
          undef,
          undef,
          undef,
          0,
          0,
          0];
}

sub get_db_ { my $self = shift; return $$self[0]; }
sub set_db_ { my($self, $val) = @_; $$self[0] = $val; }

sub get_db { my $self = shift; return $$self[0]; }

sub get_name_ { my $self = shift; return $$self[1]; }
sub set_name_ { my($self, $val) = @_; $$self[1] = $val; }

sub get_name { my $self = shift; return $$self[1]; }

sub set_name { my($self, $val) = @_; $$self[1] = $val; }

sub get_notes_ { my $self = shift; return $$self[2]; }
sub set_notes_ { my($self, $val) = @_; $$self[2] = $val; }

sub get_notes { my $self = shift; return $$self[2]; }

sub set_notes { my($self, $val) = @_; $$self[2] = $val; }

sub get_ledger_ { my $self = shift; return $$self[3]; }
sub set_ledger_ { my($self, $val) = @_; $$self[3] = $val; }

sub get_ledger_usage_count_ { my $self = shift; return $$self[4]; }
sub set_ledger_usage_count_ { my($self, $val) = @_; $$self[4] = $val; }

sub get_cleared_balance_ { my $self = shift; return $$self[5]; }
sub set_cleared_balance_ { my($self, $val) = @_; $$self[5] = $val; }

sub get_cleared_balance { my $self = shift; return $$self[5]; }

sub get_final_balance_ { my $self = shift; return $$self[6]; }
sub set_final_balance_ { my($self, $val) = @_; $$self[6] = $val; }

sub get_final_balance { my $self = shift; return $$self[6]; }


#### CBBlib::Txn slot methods #############################

package CBBlib::Txn;
use strict;

sub make_internals_ {
  return [undef,
          undef,
          undef,
          undef,
          undef,
          undef,
          '',
          []];
}

sub get_db_ { my $self = shift; return $$self[0]; }
sub set_db_ { my($self, $val) = @_; $$self[0] = $val; }

sub get_db { my $self = shift; return $$self[0]; }

sub get_clone_ { my $self = shift; return $$self[1]; }
sub set_clone_ { my($self, $val) = @_; $$self[1] = $val; }

sub get_date_ { my $self = shift; return $$self[2]; }
sub set_date_ { my($self, $val) = @_; $$self[2] = $val; }

sub get_date {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_date_();
  } else {
    return $self->get_date_();
  }
}

sub set_date {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_date_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_date_($val);    
  }
}
sub get_source_ { my $self = shift; return $$self[3]; }
sub set_source_ { my($self, $val) = @_; $$self[3] = $val; }

sub get_source { my $self = shift; return $$self[3]; }

sub get_checkno_ { my $self = shift; return $$self[4]; }
sub set_checkno_ { my($self, $val) = @_; $$self[4] = $val; }

sub get_checkno {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_checkno_();
  } else {
    return $self->get_checkno_();
  }
}

sub set_checkno {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_checkno_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_checkno_($val);    
  }
}
sub get_desc_ { my $self = shift; return $$self[5]; }
sub set_desc_ { my($self, $val) = @_; $$self[5] = $val; }

sub get_desc {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_desc_();
  } else {
    return $self->get_desc_();
  }
}

sub set_desc {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_desc_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_desc_($val);    
  }
}
sub get_status_ { my $self = shift; return $$self[6]; }
sub set_status_ { my($self, $val) = @_; $$self[6] = $val; }

sub get_status {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_status_();
  } else {
    return $self->get_status_();
  }
}

sub set_status {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_status_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_status_($val);    
  }
}
sub get_splits_ { my $self = shift; return $$self[7]; }
sub set_splits_ { my($self, $val) = @_; $$self[7] = $val; }

sub get_splits { my $self = shift; return $$self[7]; }


#### CBBlib::Split slot methods ###########################

package CBBlib::Split;
use strict;

sub make_internals_ {
  return [undef,
          undef,
          undef,
          undef,
          undef,
          undef,
          undef,
          undef,
          ''];
}

sub get_txn_ { my $self = shift; return $$self[0]; }
sub set_txn_ { my($self, $val) = @_; $$self[0] = $val; }

sub get_txn { my $self = shift; return $$self[0]; }

sub get_clone_ { my $self = shift; return $$self[1]; }
sub set_clone_ { my($self, $val) = @_; $$self[1] = $val; }

sub get_pos__ { my $self = shift; return $$self[2]; }
sub set_pos__ { my($self, $val) = @_; $$self[2] = $val; }

sub get_pos_ {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_pos__();
  } else {
    return $self->get_pos__();
  }
}

sub set_pos_ {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_pos__($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_pos__($val);    
  }
}
sub get_dest_ { my $self = shift; return $$self[3]; }
sub set_dest_ { my($self, $val) = @_; $$self[3] = $val; }

sub get_dest {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_dest_();
  } else {
    return $self->get_dest_();
  }
}

sub set_dest {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_dest_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_dest_($val);    
  }
}
sub get_notes_ { my $self = shift; return $$self[4]; }
sub set_notes_ { my($self, $val) = @_; $$self[4] = $val; }

sub get_notes {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_notes_();
  } else {
    return $self->get_notes_();
  }
}

sub set_notes {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_notes_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_notes_($val);    
  }
}
sub get_debit_ { my $self = shift; return $$self[5]; }
sub set_debit_ { my($self, $val) = @_; $$self[5] = $val; }

sub get_debit {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_debit_();
  } else {
    return $self->get_debit_();
  }
}

sub set_debit {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_debit_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_debit_($val);    
  }
}
sub get_credit_ { my $self = shift; return $$self[6]; }
sub set_credit_ { my($self, $val) = @_; $$self[6] = $val; }

sub get_credit {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_credit_();
  } else {
    return $self->get_credit_();
  }
}

sub set_credit {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_credit_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_credit_($val);    
  }
}
sub get_desc_ { my $self = shift; return $$self[7]; }
sub set_desc_ { my($self, $val) = @_; $$self[7] = $val; }

sub get_desc {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_desc_();
  } else {
    return $self->get_desc_();
  }
}

sub set_desc {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_desc_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_desc_($val);    
  }
}
sub get_status_ { my $self = shift; return $$self[8]; }
sub set_status_ { my($self, $val) = @_; $$self[8] = $val; }

sub get_status {
  my $self = shift;
  my $clone = $self->get_clone_();
  if($clone) {
    return $clone->get_status_();
  } else {
    return $self->get_status_();
  }
}

sub set_status {
  my($self, $val) = @_;

  my $db = $self->get_db();
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $clone->set_status_($val);
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  } else {
    $self->set_status_($val);    
  }
}

#### CBBlib::Db slot methods ###########################

package CBBlib::Db;
use strict;

sub make_internals_ {
  return [[],
          [],
          undef,
          [],
          undef,
          0,
          {}];
}

sub get_accts_ { my $self = shift; return $$self[0]; }
sub set_accts_ { my($self, $val) = @_; $$self[0] = $val; }

sub get_accts { my $self = shift; return $$self[0]; }

sub get_cats_ { my $self = shift; return $$self[1]; }
sub set_cats_ { my($self, $val) = @_; $$self[1] = $val; }

sub get_cats { my $self = shift; return $$self[1]; }

sub get_default_sink_ { my $self = shift; return $$self[2]; }
sub set_default_sink_ { my($self, $val) = @_; $$self[2] = $val; }

sub get_default_sink { my $self = shift; return $$self[2]; }

sub set_default_sink { my($self, $val) = @_; $$self[2] = $val; }

sub get_txns_ { my $self = shift; return $$self[3]; }
sub set_txns_ { my($self, $val) = @_; $$self[3] = $val; }

sub get_txns { my $self = shift; return $$self[3]; }

sub get_modified_txns_ { my $self = shift; return $$self[4]; }
sub set_modified_txns_ { my($self, $val) = @_; $$self[4] = $val; }

sub get_modified_txns_level_ { my $self = shift; return $$self[5]; }
sub set_modified_txns_level_ { my($self, $val) = @_; $$self[5] = $val; }

sub get_callbacks_hash_ { my $self = shift; return $$self[6]; }
sub set_callbacks_hash_ { my($self, $val) = @_; $$self[6] = $val; }

1;
__END__
