#!/usr/bin/perl -w
#  common.pl - common routines shared by many CBB files
#
#  Written by Curtis Olson.  Started August 22, 1994.
#
#  Copyright (C) 1994 - 1997  Curtis L. Olson  - curt@sledge.mn.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# $Id$
# (Log is kept at end of this file)

## @file
# @brief common routines shared by many CBB files 
# @author Curtis Olson
# @date Started August 22, 1994
# @cond PERL
# ignore the following for doxygen

use strict;

sub destructive_merge_mangle {
  my($destination_ref, $source_ref, $comparison, $mangler) = @_;
  # Merges elements of destination into source according to the
  # comparison function.  Assumes that both source and destination
  # are already sorted wrt comparison.

  # mangler is optional, but if provided will be called for each
  # element of the modified destination with the args
  # ($destination_ref, $index, $new_item_p).  You can check for undef
  # on the next or previous index values to see if you're inserting at
  # the end/beginning of the list.

  # returns the indexes (in the new list) of the txns that were
  # inserted

  my @inserted_indices = ();
  my @source = @$source_ref;
  my $src_head = shift @source;
  my $current_splice_pos = 0;
  my $dest_items_left = scalar(@$destination_ref);
  my $next_dest = $$destination_ref[$current_splice_pos];
  while($src_head && $dest_items_left) {
    if(&$comparison($next_dest, $src_head) == 1) {
      # i.e. next_dest > src_head
      splice @$destination_ref, $current_splice_pos, 0, $src_head;
      push @inserted_indices, $current_splice_pos;
      &$mangler($destination_ref, $current_splice_pos, 1) if $mangler;
      $src_head = shift @source;
      $current_splice_pos++;
    } else {
      &$mangler($destination_ref, $current_splice_pos, 0) if $mangler;
      $current_splice_pos++;
      $next_dest = $$destination_ref[$current_splice_pos];
      $dest_items_left--;
    }
  }
  if($src_head) {
    push @$destination_ref, $src_head, @source;
    my $tail;
    foreach $tail ($src_head, @source) {
      &$mangler($destination_ref, $current_splice_pos, 1) if $mangler;
      push @inserted_indices, $current_splice_pos;
      $current_splice_pos++;
    }
  } else {
    # Must be some destination_ref items left.
    while ($dest_items_left) {
      &$mangler($destination_ref, $current_splice_pos, 0) if $mangler;
      $current_splice_pos++;
      $dest_items_left--;
    }
  }
  return \@inserted_indices;
}

sub destructive_remove_mangle {
  my($destination_ref, $source_ref, $comparison, $mangler) = @_;

  # Removes elements of source from destination according to the
  # comparison function which is just boolean.  Assumes that source
  # items are in the same order in source_ref as they are in
  # destination_ref

  # mangler is optional, but if provided will be called for each item
  # in the new destination list with the args:

  # ($killed_p, $destination_ref, $index, $old_item).  If killed is true
  # ($killed_p, $destination_ref, $index).  If killed is false

  # if killed is true, index is the delete pos, while if false, it's
  # the new index.  You can check for undef on the next or previous
  # index values to see if you're at the end/beginning of the list.

  my @removed_indices = ();
  my $old_position = 0;
  my @source = @$source_ref;
  my $src_head = shift @source;
  my $current_splice_pos = 0;
  my $dest_items_left = scalar(@$destination_ref);
  my $next_dest = $$destination_ref[$current_splice_pos];
  while($src_head && $dest_items_left) {
    if(&$comparison($next_dest, $src_head)) {
      # found an item to delete.
      splice @$destination_ref, $current_splice_pos, 1;
      &$mangler(1, $destination_ref, $current_splice_pos, $src_head)
        if $mangler;
      $src_head = shift @source;
      push @removed_indices, $old_position;
    } else {
      &$mangler(0, $destination_ref, $current_splice_pos) if $mangler;
      $current_splice_pos++;
    }
    $old_position++;
    $dest_items_left--;
    $next_dest = $$destination_ref[$current_splice_pos];
  }
  if($src_head) {
    print STDERR
      "Warning: source items left after destructive_remove_mangle\n";
  }
  if($mangler && $dest_items_left) {
    while($dest_items_left) {
      &$mangler(0, $destination_ref, $current_splice_pos);
      $current_splice_pos++;
      $dest_items_left--;
    }
  }
  return \@removed_indices;
}

sub timestamp {
  my($sec,$min,$hour,$mday,$month,$year,$wday,$yday,$isdst) = localtime(time);
  $month++; # don't want 0 based months.
  $month = "0" . $month if $month < 10;
  $mday = "0" . $mday if $mday < 10;
  $hour = "0" . $hour if $hour < 10;
  $min = "0" . $min if $min < 10;
  $sec = "0" . $sec if $sec < 10;
  $year += 1900;

  return("$year-$month-$mday-$hour-$min-$sec");
}

# We need a version number
$CBB::version = "Version <not_installed>";
($CBB::junk, $CBB::version_num, $CBB::junk) = split(/ +/, $CBB::version);


# Contributed by Christopher Browne, Oct. 24/94
sub pad { 
  return sprintf("%02d", $_[0]); 
}


# return the directory of a file name 
sub file_dirname {
  my($file) = @_;
  my($pos);
  
  $pos = rindex($file, "/");
  if ( $pos >= 0 ) {
    return substr($file, 0, ($pos + 1));
  } else {
    return "./";
  }
}


# return the base file name
sub file_basename {
  my($file) = @_;
  my($pos);
  
  $pos = rindex($file, "/");
  return substr($file, ($pos + 1));
}


# return the file name root (ending at last ".")
sub file_root {
  my($file) = @_;
  my($pos);
  
  $pos = rindex($file, ".");
  return substr($file, 0, $pos);
}


# return the file name extension (starting at first ".")
sub file_extension {
  my($file) = @_;
  my($pos);
  
  $pos = rindex($file, ".");
  return substr($file, ($pos + 1));
}


# return current date in a nice format
sub nice_date {
  my($date_fmt) = @_;
  
  my($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = 
    localtime(time());

  # right now we're only going to deal with yyyymmdd.  We'll change
  # this soon.

  return(sprintf("%04d", 1900 + $year) .
         sprintf("%02d", $mon + 1) .
         sprintf("%02d", $mday)); 

  #if ( $date_fmt eq 'usa' ) {
  #  return &pad($mon+1) . "/" . &pad($mday) . "/" . &pad($year);
  #} else {
  #  return &pad($mday) . "." . &pad($mon+1) . "." . &pad($year);
  #}
}


# return current date in a raw format
sub raw_date {
  my($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = 
    localtime(time);
  return &century() . &pad($year) . &pad($mon+1) . &pad($mday);
}

# start date: return date in raw format, takes argument of those types:
# -[num]m months (eg. "-0m" means only current month, "-1m" means current and last)
# -[num]d days (eg. "-10m" means 10 days)
# dd.mm.yy[yy] : "international" format
# mm/dd/yy[yy] : "us" format
# yyyymmdd     : "raw" format
#
# This can get a bit complicated, thank god we don't have to care whether
# we return invalid days

sub start_date {
  my($idate) = @_;
  my($odate, $value);
  my($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = 
    localtime(time);
  
  $mon = $mon + 1;
  
  if ( $idate =~ /^\d{8}$/ ) {   # "raw" format
    $odate = $idate;
  } elsif ($idate =~ /^-\d{1,2}m$/ ) {  # "month" format
    
    $value = substr($idate, 1, 3);  # a maximum of 99 months !
    if ($value >= $mon) {
      $year = $year - 1 - int( ($value - $mon) / 12 );
      $value = ($value % 12 );
    }
    $mon = $mon - $value;
    if ($mon < 1) {
      $value = $value + 12; 
    }
    $odate = &century() . &pad($year) . &pad($mon) . &pad(1);
    
  } elsif ($idate =~ /^-\d{1,3}d$/ ) {  # "day" format
    
    $value = substr($idate, 1, 4);  # a maximum of 999 days !
    if ($value >= $yday) {
      $year = $year - 1 - int( ($value - $yday) / 360 );
      $value = ( $value % 360 );
    }
    if ($value >= $mday) {
      $mon = $mon - 1 - int( ($value - $mday) / 30 );
      if ($mon < 1) {
        $mon = $mon + 12;
      }
      $value = ( $value % 30 );
    }
    $mday = $mday - $value;
    if ($mday < 1) {
      $mday = $mday + 30;
    }
    $odate = &century() . &pad($year) . &pad($mon) . &pad($mday);
    
  } elsif ( $idate =~ /^\d{1,2}\/\d{1,2}\/\d{2,4}$/ ) {   # "us" format
    
    ($mon, $mday, $year) = split(/\//, $idate);
    if ($year < 100) {
      $value = &century();
    } else {
      $value = $year / 100;
    }
    $odate = &pad($value) . &pad($year) . &pad($mon) . &pad($mday);
    
  } elsif ( $idate =~ /^\d{1,2}\.\d{1,2}\.\d{2,4}$/ ) {   # "int" format
    
    ($mday, $mon, $year) = split(/\./, $idate);
    if ($year < 100) {
      $value = &century();
    } else {
      $value = $year / 100;
    }
    $odate = &pad($value) . &pad($year) . &pad($mon) . &pad($mday);
    
  } else {  # nonsense, give them everything since 1900
    $odate = "19000101";
  }
  
  return ($odate);
}

# return the current century in the form 19, 20, 21, etc.
# requires the Unix "date" command to be in the path
sub century {
  my($unix_date, $year, $century, $junk);
  
  $unix_date = localtime;  # e.g. "Thu Oct  3 16:53:37 1996"
  ($junk, $junk, $junk, $junk, $year) = split(/\s+/, $unix_date);
  $century = substr($year, 0, 2);
  
  return($century);
}


sub mypwd {
  my $dir = `pwd`;
  chomp($dir);
  return $dir;
}


1;        # need to return a true value
__END__

## @endcond
# ----------------------------------------------------------------------------
# $Log$
# Revision 1.1  2000/06/02 09:00:14  peticolas
# Rob Browning's patch to add automake.
#
# Revision 1.2  1999/01/17 17:05:59  linas
# patch from  msimons@fsimons01.erols.com (Mike Simons)
#
# Revision 1.1  1998/04/22 03:02:38  linas
# CBB conversion tools from Rob Browning
#
# Revision 1.3  1998/01/24 02:21:24  rlb
# Many changes.  Hopefully I'll be better about commits now.
#
# Revision 1.2  1997/10/22 03:46:00  rlb
# Working (before txn data stucture switch)
#
# Revision 1.1  1997/10/10 18:15:53  rlb
# Initial submission
#
# Revision 2.5  1996/12/17 14:53:54  curt
# Updated copyright date.
#
# Revision 2.4  1996/12/14 17:15:21  curt
# The great overhaul of December '96.
#
# Revision 2.3  1996/12/11 18:33:31  curt
# Ran a spell checker.
#
# Revision 2.2  1996/12/08 07:39:58  curt
# Rearranged quite a bit of code.
# Put most global variables in cbb() structure.
#
# Revision 2.1  1996/12/07 20:38:14  curt
# Renamed *.tk -> *.tcl
#
# Revision 2.3  1996/09/30 15:14:36  curt
# Updated CBB URL, and hardwired wish path.
#
# Revision 2.2  1996/07/13 02:57:39  curt
# Version 0.65
# Packing Changes
# Documentation changes
# Changes to handle a value in both debit and credit fields.
#
# Revision 2.1  1996/02/27  05:35:38  curt
# Just stumbling around a bit with cvs ... :-(
#
# Revision 2.0  1996/02/27  04:41:50  curt
# Initial 2.0 revision.  (See "Log" files for old history.)



# ----------------------------------------------------------------------------
# $Log$
# Revision 1.1  2000/06/02 09:00:14  peticolas
# Rob Browning's patch to add automake.
#
# Revision 1.2  1999/01/17 17:05:59  linas
# patch from  msimons@fsimons01.erols.com (Mike Simons)
#
# Revision 1.1  1998/04/22 03:02:38  linas
# CBB conversion tools from Rob Browning
#
# Revision 1.3  1998/01/24 02:21:24  rlb
# Many changes.  Hopefully I'll be better about commits now.
#
# Revision 1.2  1997/10/22 03:46:00  rlb
# Working (before txn data stucture switch)
#
# Revision 1.1  1997/10/10 18:15:53  rlb
# Initial submission
#
# Revision 2.11  1997/05/06 01:00:26  curt
# Added patches contributed by Martin Schenk <schenkm@ping.at>
# - Default to umask of 066 so .CBB files get created rw by owner only
# - Added support for pgp encrypting data files
# - Added support for displaying only recent parts of files (avoids
#   waiting to load in lots of old txns you don't currently need.)
# - Added a feature to "cache" whole accounts in the perl engine so
#   that switching between accounts can be faster.
# - The above options can be turned on/off via the preferrences menu.
#
# Revision 2.10  1997/01/18 03:28:41  curt
# Added "use strict" pragma to enforce good scoping habits.
#
# Revision 2.9  1996/12/17 14:53:54  curt
# Updated copyright date.
#
# Revision 2.8  1996/12/11 18:33:30  curt
# Ran a spell checker.
#
# Revision 2.7  1996/10/03 22:02:25  curt
# I found a way in perl to get the century directly, so I was able to
# eliminate the dependency on the external Unix date command.
#
# Revision 2.6  1996/10/03 04:48:59  curt
# Fixed an inconsistency in &raw_date() in common.pl (with how it was
# called.)
#
# Version now is 0.67-beta-x
#
# Revision 2.5  1996/10/03 04:13:39  curt
# Refined default century handling code.
#
# Revision 2.4  1996/10/03 03:52:57  curt
# CBB now determines the current century automatically ... no need for it
# to be hard coded.  Removed all hardcoded instances of the century (especially
# in reports.pl and recur.pl)
#
# Added an optional --debug flag to the invocation of CBB.
#
# Revision 2.3  1996/10/02 19:37:18  curt
# Replaced instances of hardcoded century (19) with a variable.  We need to
# know the current century in cases where it is not provided and it is
# assumed to be the current century.  Someday I need to figure out how
# to determine the current century, but I have a couple of years to do it. :-)
#
# I still need to fix conf-reports and reports.pl
#
# Revision 2.2  1996/07/13 02:57:39  curt
# Version 0.65
# Packing Changes
# Documentation changes
# Changes to handle a value in both debit and credit fields.
#
# Revision 2.1  1996/02/27  05:35:37  curt
# Just stumbling around a bit with cvs ... :-(
#
# Revision 2.0  1996/02/27  04:41:50  curt
# Initial 2.0 revision.  (See "Log" files for old history.)



