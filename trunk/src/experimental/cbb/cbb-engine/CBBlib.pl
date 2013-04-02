#!/usr/bin/perl -w
#use Exporter();

package CBBlib;

use strict;
use English;
use IO;
##  @file
# @brief Belongs to package CBBlib
#
#### To do ######################
#
# Check remove transactions.
# Move everything to Cbb package.
#
# put warnings into add_txns and remove_txns if attempted when inside
# begin/end_txn_modifications
#
# Check to see that set_db is OK in the face of modifications...
#
# update_ledger has to return a sorted list of txn indices for modified txns
#
# ledger_add/modify/remove_txns?
#
# note_txn_modification
#
# Dirty should be set whenever a modification is made.
# Dirty should be cleared whenever the client tells us we're clear...
#
# What about clones and begin/end_modify_txns and no db?
#
# check copy_obj, and automate?
#
# Need to clone account balances too?
#
# Should have list positions in txns?
#
# Need to create "Unitemized" category by default.
#
# Don't sort modifications by serial number.  Order is irrelevant.
# Just use dates
# @cond Perl

STDOUT->autoflush(1);
STDERR->autoflush(1);

#@ISA       = qw(Exporter);
#@EXPORT    = qw(func1 func2);
#@EXPORT_OK = qw($sally @listabob %harry func3);

## private #################################################################

sub BEGIN {}

sub END {
  my $old_status = $?;
  
  $? = $old_status;
}

## public ##################################################################

my $pref_debug = 0;
my $pref_verbose = 0;

sub set_debug {
  my($value) = @_;
  $pref_debug = 1;
}

sub set_verbose {
  my($value) = @_;
  $pref_verbose = 1;
}

sub debug {
  my($message) = @_;
  print STDERR $message if $pref_debug;
}

sub verbose {
  my($message) = @_;
  print STDERR $message if ($pref_verbose || $pref_debug);
}

my $elapsed_offset = 0;

sub elapsed_reset {
  my $prefix = shift;
  my($user,$system) = times;
  $elapsed_offset = $user + $system;
  print STDERR $prefix . " ... " if $prefix;
}
sub elapsed {
  my $prefix = shift;
  print STDERR $prefix if $prefix;
  my($user,$system) = times;
  print STDERR '(elapsed time: ' . ($user + $system - $elapsed_offset) . ")\n";
}

package CBBlib::Sink;
use strict;
use English;
use IO;

# Abstract class -- no "new" defined.

# A destination for money.  Parent class for Acct and Cat.

# It's really possible that ledgers should be a separate class, but
# I'm tired and that's too big a change right now.


sub acquire_ledger {
  my $self = shift;
  my $ledger = $self->get_ledger_();
  if(!$ledger) {
    $self->build_ledger_();
    print STDERR "Building ledger\n";
  }
  $self->set_ledger_usage_count_($self->get_ledger_usage_count_() + 1);
  return $self->get_ledger_();
}

sub release_ledger {
  my $self = shift;
  my $ledger = $self->get_ledger_();
  if(!$ledger) {
    print STERR "Big problem.  Released ledger that didn't exist!\n";
    exit 1;
  }

  my $usage_count = $self->get_ledger_usage_count_();
  if(!$usage_count) {
    print STERR
        "Big problem.  Released ledger that no-one could be holding!\n";
    exit 1;
  }

  $self->set_ledger_usage_count_($usage_count - 1);
  if($usage_count == 1) {
    print STDERR "Killing ledger\n";
    $self->set_ledger_(undef);
  }
}

sub build_ledger_ {
  my($self) = @_;
  my $db = $self->get_db();
  my $transactions = $db->get_txns();
  my $ledger = $self->get_ledger_();
  if(!$ledger) {
    $self->set_ledger_([]);
    $ledger = $self->get_ledger_();
  }
  @$ledger = ();
  
  my $cleared_balance = 0;
  my $total = 0;
  my $txn;
  foreach $txn (@$transactions) {    
    my($debit, $credit, $applicable_txn) = $txn->totals_wrt($self);
    if($applicable_txn) {
      $total += ($credit - $debit);
      if($txn->cleared_wrt_p($self)) {
        $cleared_balance += ($credit - $debit);
      }
      push @$ledger, [ $txn, $total ];
    }
  }
  $self->set_cleared_balance_($cleared_balance);
  $self->set_final_balance_($total);
}

sub compare_ledger_and_txn_ {
  my($ledger, $txn) = @_;
  return($$ledger[0]->get_date() cmp $txn->get_date());
}

sub match_ledger_and_txn_ {
  my($ledger, $txn) = @_;
  return($$ledger[0] == $txn);
}

my $started_mods_p;
my $mods_acct;
my $cleared_balance_diff_tmp;

sub handle_ledger_entry_merge_ {
  my($ledger, $index, $new_p) = @_;

  if($new_p || $started_mods_p) {
    if($new_p) {
      # convert txn to ledger pair.
      my $txn = $$ledger[$index];
      $$ledger[$index] = [$txn, 0.0];
      $started_mods_p = 1;
    }
    
    my $entry = $$ledger[$index];
    my $txn = $$entry[0];
    my($debit, $credit, $applicable_txn) = $txn->totals_wrt($mods_acct);
    
    if($new_p) {
      if($txn->cleared_wrt_p($mods_acct)) {
        $cleared_balance_diff_tmp += ($credit - $debit);
      }
    }
    
    my $prev_total = 0;
    my $prev_entry = $$ledger[$index - 1];
    if($prev_entry) {
      $prev_total = $$prev_entry[1];
    }
    $$entry[1] = $prev_total + ($credit - $debit);
  }
}

my $ledger_removal_diff;

sub handle_ledger_entry_removal_ {
  my($killed_p, $ledger, $index, $old_item) = @_;

  if($killed_p || $started_mods_p) {
    my $txn = $$ledger[$index]; $txn = $$txn[0];
    if($killed_p) {
      my($debit, $credit, $applicable_txn)  = $txn->totals_wrt($mods_acct);
      $ledger_removal_diff += ($credit - $debit);
      if($txn->cleared_wrt_p($mods_acct)) {
        $cleared_balance_diff_tmp -= ($credit - $debit);
      }      
      $started_mods_p = 1;
    } else {
      my $prev_total = 0;
      my $prev_entry = $$ledger[$index - 1];
      if($prev_entry) { $prev_total = $$prev_entry[1]; }
      my $entry = $$ledger[$index];
      $$entry[1] = $prev_total - $ledger_removal_diff;
    }
  }
}

sub ledger_add_txns_ {
  my($self, $txns) = @_;
  
  my $ledger = $self->get_ledger_();
  if(!$ledger) {
    return [];
  }
  
  # Globals, yuck...
  $cleared_balance_diff_tmp = 0;
  $started_mods_p = 0;
  $mods_acct = $self;
 CBBlib::debug("There are " . scalar(@$ledger) . " ledger entries\n");
 CBBlib::debug("  adding " . scalar(@$txns) . " ledger entries\n");
  $$txns[0]->print(\*STDERR);
  my $added_indices =
    main::destructive_merge_mangle($ledger, $txns, 
                                   \&compare_ledger_and_txn_,
                                   \&handle_ledger_entry_merge_);
  $self->set_cleared_balance_($self->get_cleared_balance() + 
                              $cleared_balance_diff_tmp);  
  my $final = $$ledger[$#$ledger];
  $self->set_final_balance_($$final[1]);  
  return $added_indices;
}

sub ledger_modify_txns_ {
  my($self, $txn_date_changed, $txns) = @_;
  # presumes @$txns is sorted on date to match the database order.
  
  my $ledger = $self->get_ledger_();
  if(!$ledger) {
    return ([], []);
  }
  
  # Brute force date change handling.  We should do better later...
  my @txns_w_date_changes = grep {
    $$txn_date_changed{$_};
  } @$txns;
  
  my @moves = ();
  
  if(@txns_w_date_changes) {
    print STDERR "Handling ledger mods for date changes\n";

    # Get the current positions of the changed $txns
    my %move_from_txn;
    my @candidates = @txns_w_date_changes;
    my $candidate = shift @candidates;
    # Unfortunately, these txns won't be in the order relative to the
    # old ledger since their dates have changed, so we have to loop.
    # This may be marginally faster than just traversing once per txn.
    while($candidate) {
      print STDERR "Looking for candidate $candidate\n";
      my $initial_value = $candidate;
      my $i;
      for($i=0; $candidate && $i < scalar(@$ledger); $i++) {
        my $entry = $$ledger[$i];
        my $txn = $$entry[0];
        print STDERR "[$txn] [$candidate]\n";
        if($txn == $candidate) {          
          my $move = [$i];
          push @moves, $move;
          $move_from_txn{$txn} = $move;
          $candidate = shift @candidates;
        }
      }
      if($initial_value == $candidate) {
        die "Couldn't find transaction in ledger during ledger modify";
      }
    }

    # Just trash the existing one to get the new one with these
    # transactions in the proper positions.
    $self->build_ledger_();

    # Now things should be properly sorted to match the order of
    # @txns_w_date_changes.  We'll leave the extra loop just in case,
    # but this should go off in one pass.
    @candidates = @txns_w_date_changes;
    $candidate = shift @candidates;
    while($candidate) {
      my $initial_value = $candidate;
      my $i;
      for($i=0; $candidate && $i < scalar(@$ledger); $i++) {
        my $entry = $$ledger[$i];
        my $txn = $$entry[0];
        if($txn == $candidate) {
          my $move = $move_from_txn{$txn};
          die "FATAL: Couldn't find move in ledger search" if ! $move;
          push @$move, $i;
          $candidate = shift @candidates;
        }
      }
      if($initial_value == $candidate) {
        die "Couldn't find transaction in ledger during ledger modify";
      }
    }
  }

  my @modified_indices = ();
  my $current_index = 0;

  if(@$txns) {
    my @mod_txns = @$txns;
    my $next_mod_txn = shift @mod_txns; 
    my $prev_ledger_value = 0;
    my $started_mods = 0;
    my $cleared_balance_diff = 0;
    my $diff = 0;
    my $entry;
    foreach $entry (@$ledger) {
      my $txn = $$entry[0];
      if(defined($next_mod_txn) && $next_mod_txn == $txn) {
        $started_mods = 1;
        my $prev_value = $$entry[1];
        my($debit, $credit, $applicable_txn) = $txn->totals_wrt($self);
        my $new_value = $prev_ledger_value + ($credit - $debit);
        my $local_change = $new_value - $prev_value;
        if($txn->cleared_wrt_p($self)) {
          $cleared_balance_diff -= ($credit - $debit);
        }      
        $diff +=  $local_change;
        $$entry[1] = $new_value;
        $next_mod_txn = shift @mod_txns;
        push @modified_indices, $current_index;
      } elsif ($started_mods) {
        $$entry[1] += $diff;
      }
      $prev_ledger_value = $$entry[1];
      $current_index++;
    }
    $self->set_cleared_balance_($self->get_cleared_balance() + 
                                $cleared_balance_diff);  
  }
  my $final = $$ledger[$#$ledger];
  $self->set_final_balance_($$final[1]);      
  return (\@modified_indices, \@moves);
}

sub ledger_remove_txns_ {
  my($self, $txns) = @_;
  # presumes @$txns is sorted on date to match the database order.
  # returns a list where each item is [$txn, $old_index].

  my $ledger = $self->get_ledger_();
  if(!$ledger) {
    return;
  }

  # Globals, yuck...
  $ledger_removal_diff = 0;
  $cleared_balance_diff_tmp = 0;
  $started_mods_p = 0;
  $mods_acct = $self;
  
  my $removed_indices = 
    main::destructive_remove_mangle($ledger,
                                    $txns,
                                    \&match_ledger_and_txn_,
                                    \&handle_ledger_entry_removal_);

  print STDERR
      "Finished removing ledger items (" . join(" ", @$removed_indices) .
          ")\n";

  $self->set_cleared_balance_($self->get_cleared_balance() + 
                              $cleared_balance_diff_tmp);  
  my $final = $$ledger[$#$ledger];
  $self->set_final_balance_($$final[1]);  
  
  my @txns_tmp = @$txns;
  my @result = map { 
    my $txn = shift @txns_tmp;
    [$_, $txn];
  } @$removed_indices;
  return(\@result);
}


package CBBlib::Acct;
use strict;
use English;
use IO;

use vars qw(@ISA);
unshift @ISA, qw(CBBlib::Sink);

sub new {
  my $class = shift;
  my ($db, $name, $notes) = @_;  
  my $self = make_internals_();
  bless $self, $class;
  
  $self->set_db_($db);
  $self->set_name_($name);
  $self->set_notes_($notes);
  
  return $self;
}

sub print {
  my($self, $fh, $prefix, $id_map) = @_;
  my $name = $self->get_name();
  my $notes = $self->get_notes();
  $notes = "" if ! $notes;
  $prefix  = "" unless $prefix;
  if($id_map) {
    print $fh $prefix . $$id_map{$self} . "\t$name\t$notes\n"; 
  } else {
    print $fh $prefix . "$self\t$name\t$notes\n"; 
  }
}


package CBBlib::Cat;
use strict;
use English;
use IO;

use vars qw(@ISA);
unshift @ISA, qw(CBBlib::Sink);

sub new {
  my $class = shift;
  my ($db, $name, $notes) = @_;  
  my $self = make_internals_();
  bless $self, $class;
  
  $self->set_db_($db);
  $self->set_name_($name);
  $self->set_notes_($notes);
  
  return $self;
}

sub print {
  my($self, $fh, $prefix, $id_map) = @_;
  my $name = $self->get_name();
  my $notes = $self->get_notes();
  $notes = "" if ! $notes;
  $prefix  = "" unless $prefix;
  if($id_map) {
    print $fh $prefix . $$id_map{$self} . "\t$name\t$notes\n"; 
  } else {
    print $fh $prefix . "$self\t$name\t$notes\n"; 
  }
}

package CBBlib::Txn;
use strict;
use English;
use IO;

sub new {
  my $class = shift;
  my ($date, $source, $checkno, $desc, $status) = @_;
  $status = "" if !$status;
  
  my $self = make_internals_();
  bless $self, $class;
  
  $self->set_date_($date);
  
  die "CBBlib::Txn new: source must be a CBBlib::Acct."
      unless (ref($source) eq 'CBBlib::Acct');
  
  $self->set_source_($source);
  
  $self->set_checkno_($checkno);
  $self->set_desc_($desc);
  $self->set_status_($status);
  
  return $self;
}


sub make_clone_ {
  my ($self) = @_;
  
  my $clone = $self->get_clone_();
  if(!$clone) {
    $self->set_clone_($self->copy_obj_());
    $clone = $self->get_clone_();
  }
  return $clone;
}  




# All the set/get slot functions are in CBBlib-auto.pl.

# ""     = irrelevant (for split line with category destination)
# " "    = new and untouched
# "*"    = selected from the balance window to tentatively be
#          cleared (stage one of the balance process)
# "x"    = cleared (stage two of the balance process)
#
# "?"    = a tentative future (recurring) transaction
# "!"    = a past (recurring) transaction

sub get_status_wrt {
  my($self, $sink) = @_;
  my $result;
  if($self->get_source() == $sink) {
    $result = $self->get_status();
  } else {
    $result = $self->get_transfer_status($sink);
  }
  if(!defined($result)) {
    print STDERR "Undefined status wrt $sink for\n";
    $self->print(\*STDERR, "  ");
  }
  return $result;
}

sub set_status_wrt_ {
  my($self, $sink, $val) = @_;
  if($self->get_source() == $sink) {
    $self->set_status($val);
  } else {
    $self->set_transfer_status_($sink, $val);
  }
}

sub copy_obj_ {
  my($self) = shift;
  
  my @copy = @$self;
  my $copy_ref = \@copy;
  bless $copy_ref, ref($self);

  my $splits = $copy_ref->get_splits_();

  # Copy only the spine of the list (so we can do a "diff" later).
  my @splits_copy = @$splits;

  $copy_ref->set_splits_(\@splits_copy);
  return $copy_ref;
}


sub get_transfer_status {
  my($self, $sink) = @_;
  my $splits = $self->get_splits_();
  my $result;
  my $split;
  foreach $split (@$splits) {
    if($split->get_dest() == $sink) {
      $result = $split;
      last;
    }
  }
  undef $split;

  if(!$result) {
    return undef;
  } else {
    return $result->get_status();
  }
}


sub set_transfer_status_ {
  my($self, $acct, $new_status) = @_;
  my $splits = $self->get_splits_();
  my $result;
  my $split;
  foreach $split (@$splits) {
    my $dest = $split->get_dest();
    if($split->get_dest() == $acct) {
      $split->set_status($new_status);
    }
  }
}

sub cleared_p_ { 
  my($self) = @_;  
  return $self->get_status() eq 'x';
}

sub cleared_wrt_p { 
  my($self, $acct) = @_;  
  return $self->get_status_wrt($acct) eq 'x';
}

sub clear_wrt { 
  my($self, $acct) = @_;  
  $self->set_status_wrt_($acct, 'x');
}

sub clear_pending_wrt_p { 
  my($self, $acct) = @_;  
  return $self->get_status_wrt($acct) eq '*';
}

sub clear_pending_wrt { 
  my($self, $acct) = @_;  
  $self->set_status_wrt_($acct, '*');
}

sub uncleared_wrt_p { 
  my($self, $acct) = @_;  
  return $self->get_status_wrt($acct) eq ' ';
}

sub unclear_wrt { 
  my($self, $acct) = @_;  
  $self->set_status_wrt_($acct, ' ');
}

sub void {
  # ???
}

sub add_split {
  my($self, $split, $insert_position) = @_;

  # Don't forget to change modify functions in CBBlib-auto.plp too
  # whenever you make changes here.

  my $splits;

  $split->set_txn_($self);

  my $db = $self->get_db();
  if($db) {
    
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $splits = $clone->get_splits_();

    if($insert_position) {
      splice @$splits, $insert_position, 0, ($split);
    } else {
      push @$splits, $split;
      $insert_position = $#$splits;
    }
  } else {
    $splits = $self->get_splits_();

    if($insert_position) {
      splice @$splits, $insert_position, 0, ($split);
    } else {
      push @$splits, $split;
      $insert_position = $#$splits;
    }    
  }

  my $i;
  for($i = $insert_position; $i < scalar(@$splits); $i++) {
    $$splits[$i]->set_pos_($i);
  }
  
  if($db) {
    $db->record_txn_modification_($self);
    $db->end_txn_modifications();
  }
}

sub remove_split {
  my($self, $split) = @_;

  # Don't forget to change modify functions in CBBlib-auto.plp too
  # whenever you make changes here.

  my $db = $self->get_db();
  my $splits;
  if($db) {
    $db->begin_txn_modifications();
    
    my $clone = $self->make_clone_();
    $splits = $clone->get_splits_();
  } else {
    $splits = $self->get_splits_();
  }
  
  my $old_index = 0;
  my $split_found = 0;
  my $candidate;
  foreach $candidate (@$splits) {
    if($candidate == $split) {
      $split_found = 1;
      last;
    }
    $old_index++;
  }  
  if(!$split_found) {
    die "Failed to find split in Txn::remove_split";
  }  
  splice @$splits, $old_index, 0;
  
  my $i;
  for($i = $old_index; $i < scalar(@$splits); $i++) {
    $$splits[$i]->set_pos_($i);
  }

  if($db) {
    my $txn = $split->get_txn_();
    $split->set_txn_(undef);
    $db->record_txn_modification_($txn);
    $db->end_txn_modifications();
  }
}

sub totals_wrt {
  my($self, $sink) = @_;
  # O(n)
  # Returns (total_debit, total_credit, applicable)

  my($total_debit, $total_credit, $applicability) = (0, 0, 0); 
  my $splits = $self->get_splits_();
  
  my $split;
  foreach $split (@$splits) {
    my $dest = $split->get_dest();
    
    if($self->get_source() == $sink) {
      $total_debit += $split->get_debit();
      $total_credit += $split->get_credit();
      $applicability = 1;
    } elsif($dest == $sink) {
      #} elsif((ref($dest) eq 'CBBlib::Acct') && ($dest == $sink)) {
      $total_debit += $split->get_credit();
      $total_credit += $split->get_debit();
      $applicability = 1;
    }
  }
  return($total_debit, $total_credit, $applicability);
}


sub affected_sinks {
  my($self) = @_;
  # O(n)
  # Returns list of affected sinks
  
  my $source = $self->get_source();
  my %result = ($source => $source);
  my $splits = $self->get_splits_();  

  map {
    my $dest = $_->get_dest();
    if(!$dest) {
      die "\nNo dest in $_\n";
    }
    $result{$dest} = $dest;
  } @$splits;
  return(values(%result));
}


sub print {
  my($self, $fh, $prefix, $id_map) = @_;
  $prefix = "" if ! $prefix;
  
  print $fh $prefix . $self->get_date() . "\t";
  if($id_map) {
    print $fh $$id_map{$self->get_source()} . "\t";
  } else {
    print $fh $self->get_source() . "\t";
  }
  print $fh $self->get_checkno() . "\t";
  print $fh $self->get_desc() . "\t";
  print $fh $self->get_status() . "\n";
  my $splits = $self->get_splits_();
  my $split;
  foreach $split (@$splits) {
    $split->print($fh, $prefix . ' ', $id_map);
  }
}

sub print_pretty {
  my($self, $fh, $prefix) = @_;
  $prefix = "" if ! $prefix;

  print $fh $prefix . $self->get_date() . ":";
  print $fh ($self->get_source())->get_name() . ":";
  print $fh $self->get_checkno() . ":";
  print $fh $self->get_desc() . ":";
  print $fh $self->get_status() . "\n";
  my $splits = $self->get_splits_();
  my $split;
  foreach $split (@$splits) {
    $split->print_pretty($fh, $prefix . "  ");
  }
}

package CBBlib::Split;
use strict;
use English;
use IO;

sub new {
  my $class = shift;
  my ($dest, $notes, $debit, $credit, $status) = @_;
  my $self = make_internals_();
  bless $self, $class;

  $status = '' if ! $status;

  $self->set_dest_($dest);
  $self->set_notes_($notes);
  $self->set_debit_($debit);
  $self->set_credit_($credit);
  $self->set_status_($status);

  return $self;
}

sub copy_obj_ {
  my($self) = shift;
  
  my @copy = @$self;
  my $copy_ref = \@copy;
  bless $copy_ref, ref($self);
  return $copy_ref;
}

sub make_clone_ {
  my ($self) = @_;
  
  my $clone = $self->get_clone_();
  if(!$clone) {
    my $txn = $self->get_txn_();
    if($txn) {
      $txn->make_clone_();
    }
    $self->set_clone_($self->copy_obj_());
    $clone = $self->get_clone_();
  }
  return $clone;
}  

sub get_db {
  my $self = shift;
  my $txn = $self->get_txn();
  if($txn) {
    return $txn->get_db();
  } else {
    return undef;
  }
}

sub cleared_p_ { 
  my($self) = @_;  
  my $status = $self->get_status();
  if($status) {
    return $status eq 'x';
  } else {
    return undef;
  }
}


sub print {
  my($self, $fh, $prefix, $id_map) = @_;
  $prefix = "" if ! $prefix;

  print $fh $prefix;
  if($id_map) {
    print $fh $$id_map{$self->get_dest()} . "\t";
  } else {
    print $fh $self->get_dest() . "\t";
  }
  print $fh $self->get_notes() . "\t";
  print $fh $self->get_debit() . "\t";
  print $fh $self->get_credit() . "\t";
  print $fh $self->get_status() . "\n";
}


package CBBlib::Db;
use strict;
use English;
use IO;

sub new {
  my($class, $default_sink) = @_;
  my $self = make_internals_();
  bless $self, $class;
  
  if($default_sink) {
    $self->add_sinks([$default_sink]);
  } else {
    $default_sink = new CBBlib::Cat($self, '<<unitemized>>', '');
    $self->add_sinks([$default_sink]);
  }
  $self->set_default_sink($default_sink);
  
  return $self;
}

sub clean_p {
  # Should eventually know the truth, but this is safe at the moment.
  return 0;
}

sub add_sinks {
  my($self, $sinks) = @_;
  
  my $accts = $self->get_accts_();
  my $cats = $self->get_cats_();
  map {
    $_->set_db_($self);
    if(ref($_) eq 'CBBlib::Acct') {
      push @$accts, $_;
    } elsif(ref($_) eq 'CBBlib::Cat') {
      push @$cats, $_;
    } else {
      die "Unknown sink type in CBBlib::Db::add_sinks()";
    }
  } @$sinks;
}

sub record_txn_modification_ {
  my($self, $txn) = @_;
  
  my $mod_level = $self->get_modified_txns_level_();
  if(!$mod_level) {
    die "Tried to record_txn_modification_ when not in update region.";
  }
  my $modified_txns = $self->get_modified_txns_();
  #my $serial_number = $self->get_modified_txns_serial_num_();
  #$self->set_modified_txns_serial_num_($serial_number + 1);
  
  if(ref($txn) eq 'CBBlib::Split') {
    $txn = $txn->get_txn_();
  }
  $$modified_txns{$txn} = $txn;
}

sub update_dirty_txns_hash_ {
  my($self, $dirty_hash, $txn) = @_;

  my @affected_sinks = $txn->affected_sinks();
  
  map {
    my $sink = $_;
    if(!$$dirty_hash{$sink}) {
      $$dirty_hash{$sink} = [$sink, [$txn]];
    } else {
      my $list = $$dirty_hash{$sink};
      $list = $$list[1];
      push @$list, $txn;
    }
  } @affected_sinks;
}


sub debug_txns_modified_data {
  my($self, $modifications, $dirty_sinks) = @_;
 CBBlib::debug("CBBlib CALLBACK: txns-modified\n");

 CBBlib::debug("  Modifications:\n");
  map {
    my $txn = $$_[0];
    my $mods = $$_[1];
  CBBlib::debug("    [$txn");
    map {
    CBBlib::debug("\n     [");
      my $first = 1;
      map {
        if($first) {
          $first = 0;
        } else {
          print " ";
        }
        if(!defined($_)) {
          print "<<undefined>>";
        } else {
          print $_;
        }
      } @$_;
    CBBlib::debug("]");
    } @$mods;
  CBBlib::debug("]\n");
  } @$modifications;

 CBBlib::debug("  Affected sinks:\n");
  map {
    my ($sink, $mod_txns, $indices) = @$_;
    $sink->print(\*STDERR, "    ");
  } values(%$dirty_sinks);
}


sub post_modification_notices_ {
  my $self = shift;
 CBBlib::debug("CBBlib::post_modification_notices_: checking for changes.\n");

  my $modified_txns_hash = $self->get_modified_txns_();
  my @sorted_txns = sort { 
    $a->get_date <=> $b->get_date(); 
  } values(%$modified_txns_hash);
  my @modifications = ();

  # Have to treat credit/debit mods the same as other field mods since
  # in the end we need the ledger indices of all the txns.
  my %dirty_sinks;

  # This is not the most efficient way to go about this, but it's
  # easy, and I'm in a hurry.  Someone can make it do the merge/remove
  # mangle thing later.  Be careful, though, you don't want to cause
  # spurious add/remove events.
  my $date_changed;
  my %txn_date_changed;

  # generate modified events for each transaction
  my $txn;
  foreach $txn (@sorted_txns) {
    my $new_txn = $txn->get_clone_();
    my $old_splits = $txn->get_splits_();
    my $new_splits = $new_txn->get_splits_();
    my @txn_mods = ();

    my $i;
    for($i=0; $i < scalar(@$old_splits); $i++) {
      my $old_item = $$old_splits[$i];
      if(! grep { $_ == $old_item } @$new_splits) {
        my $old_index = $i;
        push @txn_mods, ['split-removed', $old_item, $old_index];
      }
    }

    for($i=0; $i < scalar(@$new_splits); $i++) {
      my $new_item = $$new_splits[$i];
      if(! grep { $_ == $new_item } @$old_splits) {
        my $new_index = $i;
        push @txn_mods, ['split-added', $new_item, $new_index];
      }
    }

    # This could be much faster with a better algorithm.
    my @modified_splits = grep { $_->get_clone_() } @$new_splits;
    
    if($new_txn->get_date_ ne $txn->get_date_()) {
      push @txn_mods, ['date', $txn->get_date_()];
      $txn->set_date_($new_txn->get_date_());
      $date_changed = 1;
      $txn_date_changed{$txn} = $txn;
    }
    if($new_txn->get_checkno_ ne $txn->get_checkno_()) {
      push @txn_mods, ['checkno', $txn->get_checkno_()];
      $txn->set_checkno_($new_txn->get_checkno_());
    }
    if($new_txn->get_desc_ ne $txn->get_desc_()) {
      push @txn_mods, ['desc', $txn->get_desc_()];
      $txn->set_desc_($new_txn->get_desc_());
    }
    if($new_txn->get_status_ ne $txn->get_status_()) {
      push @txn_mods, ['status', $txn->get_status_()];
      $txn->set_status_($new_txn->get_status_());
    }
    
    my $split;
    foreach $split (@modified_splits) {
      my $new_split = $split->get_clone_();
      
      my $old_txn = $split->get_txn();
      if(defined($old_txn) && ($old_txn == $new_split->get_txn())) {
        # This is really a modification to an existing split (is that
        # what this test should be doing)?

        my $old_pos = $split->get_pos__();
        my $new_pos = $new_split->get_pos__();
        if((defined($new_pos) != defined($old_pos)) ||
           ($new_pos != $old_pos)) {
          push @txn_mods, ['split-modified', $split, 'pos',
                           $split->get_pos__()];
          $split->set_pos__($new_split->get_pos__());
        }
        if($new_split->get_dest_ != $split->get_dest_()) {
          push @txn_mods, ['split-modified', $split, 'dest',
                           $split->get_dest_()];
          $split->set_dest_($new_split->get_dest_());
        }
        if($new_split->get_notes_ ne $split->get_notes_()) {
          push @txn_mods, ['split-modified', $split, 'notes',
                           $split->get_notes_()];
          $split->set_notes_($new_split->get_notes_());
        }
        if($new_split->get_debit_ != $split->get_debit_()) {
          push @txn_mods, ['split-modified', $split, 'debit',
                           $split->get_debit_()];
          $split->set_debit_($new_split->get_debit_());
        }
        if($new_split->get_credit_ != $split->get_credit_()) {
          push @txn_mods, ['split-modified', $split, 'credit',
                           $split->get_credit_()];
          $split->set_credit_($new_split->get_credit_());
        }
        if($new_split->get_status_ ne $split->get_status_()) {
          push @txn_mods, ['split-modified', $split, 'status',
                           $split->get_status_()];
          $split->set_status_($new_split->get_status_());
        }
      }
      $split->set_clone_(undef);
    }
    $txn->set_splits_($new_splits);
    $txn->set_clone_(undef);
    
    if(@txn_mods) {
      $self->update_dirty_txns_hash_(\%dirty_sinks, $txn);
      push @modifications, [ $txn, \@txn_mods ];
    }
  }

  # Re-sort if needed, so the positions will be right for the next
  # steps.
  if($date_changed) {
    print STDERR "Re-sorting txns\n";
    my $txns = $self->get_txns();
    @$txns = sort {
      $a->get_date <=> $b->get_date(); 
    } @$txns;
  }

  # Act on dirty ledgers hash.
  my $acct_data;
  foreach $acct_data (values %dirty_sinks) {
    my $acct = $$acct_data[0];
    my $acct_txns = $$acct_data[1];
    my ($txn_indices, $moves) = 
        $acct->ledger_modify_txns_(\%txn_date_changed, 
                                   $acct_txns);
    push @$acct_data, $txn_indices, $moves;
  }
  
  if(@modifications) {
    
    debug_txns_modified_data($self, \@modifications, \%dirty_sinks);
    
    # txns-modified callback
    my $callbacks_hash = $self->get_callbacks_hash_();
    my $callbacks = $$callbacks_hash{'txns-modified'};
    my $callback;
    foreach $callback (@$callbacks) {
      my $func = $$callback[0];
      my $args = $$callback[1];

      # @modifications elements are of the form
      # [ $txn, [ mod, mod, mod, ...]]

      # %dirty_sinks
      # Key: $sink
      # Value: [ $sink, @$txns, @$current_indices, @$moves]
      #        where @$moves is a ref to a list of pairs
      #        of the form [$prev_pos, $new_pos] sorted on $prev_pos

      &$func($self, \@modifications, \%dirty_sinks, $args);
      
    }
  }
}


sub begin_txn_modifications {
  my ($self) = shift;
  my $mod_level = $self->get_modified_txns_level_();
  if(!$mod_level) {
    $self->set_modified_txns_({});
    #$self->set_modified_txns_serial_num_(0);
  }
  $self->set_modified_txns_level_($mod_level + 1);
}

sub end_txn_modifications {
  my ($self) = shift;
  my $mod_level = $self->get_modified_txns_level_();
  if($mod_level == 1) {
    $self->post_modification_notices_();
  } elsif(!$mod_level) {
    die
        "Big problem.  Db::end_txn_modifications called when not modifying.\n";
  }
  $self->set_modified_txns_level_($mod_level - 1);
}

sub get_accts_by_name {
  my($self, $name) = @_;
  my $accts = $self->get_accts();
  my @matches = grep { 
    if($_) {
      $_->get_name() eq $name;
    } else {
      0;
    }
  } @$accts;
  return \@matches;
}

sub get_cats_by_name {
  my($self, $name) = @_;
  my $cats = $self->get_cats();
  my @matches = grep { 
    if($_) {
      $_->get_name() eq $name;
    } else {
      0;
    }
  } @$cats;
  return \@matches;
}

sub extract_accounts_ {
  my($text, $hash) = @_;
  my @accounts = split("\n", $text);
  return map {
    my $acct = $_;
    my @fields = split("\t", $acct);
    (scalar(@fields) < 4) or die "Wrong number of fields in account."; 
    
    my $name = $fields[1];
    my $notes = $fields[2];
    $acct = new CBBlib::Acct(undef, $name, $notes);
    $$hash{$fields[0]} = $acct;
    $acct;
  } @accounts;
}

sub extract_categories_ {
  my($text, $hash) = @_;
  my @categories = split("\n", $text);
  return map {
    my $cat = $_;
    my @fields = split("\t", $cat);
    (scalar(@fields) < 4) or die "Wrong number of fields in category."; 

    my $name = $fields[1];
    my $notes = $fields[2];
    $cat = new CBBlib::Cat(undef, $name, $notes);
    $$hash{$fields[0]} = $cat;
    $cat;
  } @categories;
}

sub calc_account_totals_only_ {
  my($self) = @_;
  my $transactions = $self->get_txns();
  my $accts = $self->get_accts();
  map {
    if($_) {
      $_->set_cleared_balance_(0);
      $_->set_final_balance_(0);
    }
  } @$accts;

  #my $cleared_balance = 0;
  #my $final_balance = 0;

  my $txn;
  foreach $txn (@$transactions) {
    my $splits = $txn->get_splits_();
    
    my $split;
    foreach $split (@$splits) {
      
      my $source = $txn->get_source();
      my $dest = $split->get_dest();
      my $debit = $split->get_debit();
      my $credit = $split->get_credit();
      
      my $cleared_bal;
      my $final_bal;
      my $diff = $credit - $debit;
      
      if($txn->cleared_p_()) {
        $cleared_bal = $source->get_cleared_balance() + $diff;
        $source->set_cleared_balance_($cleared_bal);
      }
      $final_bal = $source->get_final_balance() + $diff;
      $source->set_final_balance_($final_bal);
      
      if(ref($dest) eq 'CBBlib::Acct' && ($source != $dest)) {
        if($split->cleared_p_()) {
          $cleared_bal = $dest->get_cleared_balance();
          $dest->set_cleared_balance_($cleared_bal - $diff);
        }
        $final_bal = $dest->get_final_balance() - $diff;
        $dest->set_final_balance_($final_bal);
      }
    }
  }
}


sub add_callback_ {
  my($self, $name, $callback, $user_data) = @_;
  my $data = [$callback, $user_data];
  my $callbacks_hash = $self->get_callbacks_hash_();
  my $txn_callbacks = $$callbacks_hash{$name};
  if(!$txn_callbacks) { 
    $$callbacks_hash{$name} = [];
    $txn_callbacks = $$callbacks_hash{$name};
  }
  push @$txn_callbacks, $data;
  return $data;
}

sub remove_callback_ {
  my($self, $name, $callback_id) = @_;
  my $callbacks_hash = $self->get_callbacks_hash_();
  my $callbacks = $$callbacks_hash{$name};
  if(scalar(@$callbacks)) {    
    @$callbacks = grep { !($_ == $callback_id) } @$callbacks;
  }
}

# add_txn_callback
# Called whenever new transactions are added
# Called with args ($db, $new_txns, $user_data)

sub add_txns_added_callback {
  my($self, $callback, $user_data) = @_;
  return $self->add_callback_('txns-added', $callback, $user_data);
}


sub remove_txns_added_callback {
  my($self, $callback_id) = @_;
  $self->remove_callback_('txns-added', $callback_id);
}


# add_txn_callback
# Called whenever new transactions are added
# Called with args ($db, $dead_txns, $user_data)

sub add_txns_removed_callback {
  my($self, $callback, $user_data) = @_;
  return $self->add_callback_('txns-removed', $callback, $user_data);
}


sub remove_txns_removed_callback {
  my($self, $callback_id) = @_;
  $self->remove_callback_('txns-removed', $callback_id);
}

sub add_txns_modified_callback {
  my($self, $callback, $user_data) = @_;
  return $self->add_callback_('txns-modified', $callback, $user_data);
}

sub remove_txns_modified_callback {
  my($self, $callback_id) = @_;
  $self->remove_callback_('txns-modified', $callback_id);
}

sub merge_new_txns_into_main_list_ {
  my($self, $new_txns) = @_;
  my $txns = $self->get_txns();
  
  my $added_indices =
    main::destructive_merge_mangle($txns, $new_txns, sub {
      return $_[0]->get_date() cmp $_[1]->get_date();
    });
  
  map { $_->set_db_($self); } @$new_txns;

  my %affected_sinks;
  my $txn;
  foreach $txn (@$new_txns) {
    my @accts = $txn->affected_sinks();
    my $acct;
    foreach $acct (@accts) {
      my $data = $affected_sinks{$acct};
      if(!$data) { $data = $affected_sinks{$acct} = [$acct, []]; }
      my $list = $$data[1];
      push @$list, $txn;
    }
  }
  
  return (\%affected_sinks, $added_indices);
}  

sub merge_new_txns_into_ledger_lists_ {
  my($self, $new_txns, $affected_accts) = @_;

  # $affected accts is a hash mapping accounts to [acct,
  # relevant_txns] acct is a ref to the account, and relevant txns is
  # a ref to a list of the relevant transactions.  The transactions
  # must be ordered in each list like they are in the global DB.

  # We're going to add the resulting new ledger indices to the hash
  # values so we have: [acct, relevant_txns, indices]

  my $data;
  foreach $data (values(%$affected_accts)) {
    my $acct_ref = $$data[0];
    my $txns = $$data[1];
    my $added_indices = $acct_ref->ledger_add_txns_($txns);
    push @$data, $added_indices;
  }
}

sub add_txns {
  my($self, $new_txns) = @_;

  @$new_txns = sort {
    $a->get_date() <=> $b->get_date();
  } @$new_txns;

  my ($affected_accts, $added_indices) =
      $self->merge_new_txns_into_main_list_($new_txns);
  
  $self->merge_new_txns_into_ledger_lists_($new_txns, $affected_accts);
  
  my $callbacks_hash = $self->get_callbacks_hash_();
  my $txn_callbacks = $$callbacks_hash{'txns-added'};
  my $callback;
  foreach $callback (@$txn_callbacks) {
    my $func = $$callback[0];
    my $args = $$callback[1];
    
    if($main::pref_debug) {
      print STDERR
          "(txns-added\n" .
              "   db: $self\n" . 
                  '   added-indices: (' . join("\n" . 
                                               '                   ', @$added_indices) . ")\n" .
                                                   "   (affected-accts\n";
      my $acct_data;
      foreach $acct_data (values(%$affected_accts)) {
        my $acct = $$acct_data[0];
        my $txns = $$acct_data[1];
        my $indices = $$acct_data[2];
        print STDERR
            "      acct: $acct\n" . 
                '      txns: (' . join("\n" . 
                                       '             ', @$txns) . ")\n" .
                                           '      indices: (' . join("\n" . 
                                                                     '                ', @$indices) . ")\n";
      }
    CBBlib::debug('   args: ' . $args . "))\n");
    }
    &$func($self, $added_indices, $affected_accts, $args);
  } 
}

sub remove_txns_from_ledger_lists_ {
  my($self, $dead_txns, $affected_accts) = @_;
  # $affected accts is a hash mapping accounts to refs to lists
  # of relevant transactions.  The transactions must be ordered
  # in each list like they are in the global DB.

  # returns a hash from $sink to a listref of [$sink, @$txn_info] where
  # @$txn_info is a lists of [$txn, $prev_ledger_index] pairs

  my %result;

  my $acct;
  foreach $acct (keys(%$affected_accts)) {
    my $data = $$affected_accts{$acct};
    my $acct_ref = $$data[0];
    my $txns = $$data[1];
    my $removal_info = $acct_ref->ledger_remove_txns_($txns);
    $result{$acct} = [$acct, $removal_info];
  }
  return \%result;
}  

sub remove_txns_from_main_list_ {
  my($self, $dead_txns) = @_;
  my $txns = $self->get_txns();

  my $removed_indices = 
    main::destructive_remove_mangle($txns, $dead_txns, sub {
      return $_[0] == $_[1];
    });

  map { $_->set_db_($self); } @$dead_txns;

  my %affected_accts;
  my $txn;
  foreach $txn (@$dead_txns) {
    my @accts = $txn->affected_sinks();
    my $acct;
    foreach $acct (@accts) {
      my $data = $affected_accts{$acct};
      if(!$data) { $data = $affected_accts{$acct} = [$acct, []]; }
      my $list = $$data[1];
      push @$list, $txn;
    }
  }
  
  return (\%affected_accts, $removed_indices);
}  

sub remove_txns {
  my($self, $dead_txns) = @_;
  
  @$dead_txns = sort {
    $a->get_date() cmp $b->get_date();
  } @$dead_txns;

  my ($affected_accts, $removed_db_indices) =
      $self->remove_txns_from_main_list_($dead_txns);

  my $ledger_removal_info = 
      $self->remove_txns_from_ledger_lists_($dead_txns, $affected_accts);
  
  my $callbacks_hash = $self->get_callbacks_hash_();
  my $txn_callbacks = $$callbacks_hash{'txns-removed'};
  my $callback;
  foreach $callback (@$txn_callbacks) {
    my $func = $$callback[0];
    my $args = $$callback[1];
    &$func($self, $dead_txns,
           $removed_db_indices,
           $ledger_removal_info,
           $args);
  } 
}

sub print_sinks {
  my ($self, $fh, $id_map) = @_;

  print $fh "#### Accounts ####\n";
      my $accts = $self->get_accts();
  map { $_->print($fh, '', $id_map); } @$accts;
  undef $accts;
  print $fh "\n";

  print $fh "#### Categories ####\n";
      my $cats = $self->get_cats();
  map { $_->print($fh, '', $id_map); } @$cats;
  undef $cats;
  print $fh "\n";
}

sub print_txns {
  my ($self, $fh, $id_map) = @_;
  print $fh "#### Transactions ####\n\n";
      my $txns = $self->get_txns();
  my $txn;
  foreach $txn (@$txns) {
    $txn->print($fh, '',$id_map);
    print $fh "\n";
  }
}

sub print {
  my($self, $fh) = @_;  
  
  my %id_map;
  my $i = 0;
  my $accts = $self->get_accts();
  map {
    $id_map{$_} = "a$i";
    $i++;
  } @$accts;
  $i = 0;
  my $cats = $self->get_cats();
  map {
    $id_map{$_} = "c$i";
    $i++;
  } @$cats;

  print $fh "# CBB data file\n";
  print $fh "Version: 1.0\n";
  print $fh 'Default-sink: ' . $id_map{$self->get_default_sink()} . "\n";
  print $fh "\n";

  $self->print_sinks($fh, \%id_map);
  $self->print_txns($fh, \%id_map);
}

package CBBlib;

sub key_colon_value_to_hash {
  my($text) = @_;
  # Assumes comment lines have already been stripped.
  
  my @lines = split("\n", $text);
  my %data;
  
  map {
    if($_ =~ m/\s*([^:]+):\s*(.*)$/o) {
      $data{$1} = $2;
    } else {
      die 'Bad line in database file, first "key: value" section.';
    }
  } @lines;
  return \%data;        
}

sub load_file {
  # Args (filename:<string>)

  elapsed_reset("Starting load");
  my $name = shift;
  my $categories;
  my $accounts;
  my @transactions = ();
  my $fh = new IO::File;
  my $file;
  
  $fh->open($name) or die "Can't open input data file $file.";
  $fh->input_record_separator('');
  
  # Get the initial key/value pairs.
  my $text = <$fh>;
  $text =~ s/#.*//mgo;  # Kill comment lines.
  $text =~ s/^\n//mgo;  # Kill blank lines.
  my $file_data = key_colon_value_to_hash($text);

  die "Couldn't determine data file version"
      unless $$file_data{'Version'};
  die "Couldn't find default sink in $text." 
      unless $$file_data{'Default-sink'};

  my %sink_map = ();

  # Get accounts
  $text = <$fh>;
  $text =~ s/#.*\n//mgo;  # Kill comment lines.
  my @sinks = CBBlib::Db::extract_accounts_($text, \%sink_map);
  
  # Get categories
  $text = <$fh>;
  $text =~ s/#.*\n//mgo;  # Kill comment lines.
  push @sinks, CBBlib::Db::extract_categories_($text, \%sink_map);

  my $default_sink = $sink_map{$$file_data{'Default-sink'}};
  die "Default sink " . $$file_data{'Default-sink'} . 
      " not in database file." unless $default_sink;

  my $self = new CBBlib::Db($default_sink);

  $self->add_sinks(\@sinks);

  
  my @new_txns = ();

  # Read the "Transactions" comment line.
  scalar(<$fh>);

  my $count = 1;
  while(<$fh>) {
    print "\rLoading record $count";
    $count++;
    
    my @lines = split('\n', $_);
    my $transaction_info = shift @lines;
    my($date, $source, $checknum, $description, $status) = 
        split("\t", $transaction_info);
    $source = $sink_map{$source};
    
    if(!defined($status) || $status eq '') { $status = ' '; }

    my $transaction =
        new CBBlib::Txn($date, $source, $checknum, $description, $status);
    
    # @lines is now just the split lines
    my $split_line;
    foreach $split_line (@lines) {
      $split_line =~ s/^\s*//o;
      my($destination, $note, $debit, $credit, $status) = 
          split("\t", $split_line);
      if($destination) {
        my $old_dest = $destination;
        $destination = $sink_map{$destination};
        if(!$destination) {
          die "No destination for key [$old_dest]\n";
        }
      } else {
        $destination = $self->get_default_sink();
      }

      $transaction->add_split(new CBBlib::Split($destination, $note,
                                                $debit, $credit, $status));
    }
    push @new_txns, $transaction;
  }
  print "\n";
  elapsed();
  elapsed_reset("  Now adding transactions");
  $self->add_txns(\@new_txns);
  elapsed();
  elapsed_reset("  Calculating totals");
  $self->calc_account_totals_only_();
  elapsed();
  $fh->close() or die "Can't open close file $name.";
  print STDERR "finished load\n";

  return $self;
}

1;
__END__
## @endcond Perl
