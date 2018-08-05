#!/usr/bin/perl -w
use strict;
use File::Basename;

my $license_text_c = <<'END';
/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

END

my $license_text_scm = <<'END';
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

END

sub find_first_non_comment_scm {
    my ($array) = @_;
    my $re = qr/^;/;
    my ($index, $match);
    for ($index = 0; $index < @$array; ++$index) {
        if ($array->[$index] !~ $re) {
            $match = $index;
            last;
        }
    }
    if (defined $match) {
        return $match;
    }
    return -1;
}

sub find_first_non_comment_c {
    my ($array) = @_;
    my $re_open = qr/^\s*\/\*/;
    my $re_close = qr/\*\//; #Assumes no inline comments
    my ($index, $match);
    my $begun = 0;
    for ($index = 0; $index < @$array; ++$index) {
        if (!$begun && $$array[$index] !~ $re_open) {
            $match = $index;
            last;
        }
        if (!$begun && $$array[$index] =~ $re_open) {
            $begun = 1;
        }
        if ($begun && $$array[$index] =~ $re_close) {
            $begun = 0;
        }
    }
    if (defined $match) {
        return $match;
    }
    return -1;
}

my ($path) = @ARGV;
my ($dir, $name, $ext) = fileparse($path, qr/\..*/);

open(FD, "+< $path") or die "Failed to open $!\n";

my @contents = <FD>;
seek(FD, 0, 0);
if ($ext eq ".scm") {
    my $index = find_first_non_comment_scm(\@contents);
    if ($index > 0) {
        print FD splice(@contents, 0, $index);
    }
    print FD $license_text_scm;
}
else {
    my $index = find_first_non_comment_c(\@contents);
    if ($index > 0) {
        print FD splice(@contents, 0, $index);
    }
    print FD $license_text_c;
}
print FD @contents;
truncate(FD, tell(FD));
close(FD);

=POD
=head1 NAME

   fix-license.pl

=head1 SYNOPSIS

   fix-license.pl file

=head1 DESCRIPTION

   Adds the GPL text to a source file as a comment at the first non-comment line
   in the file. It is intended to work with files taking Scheme and C-style
   comments, and determines which based on whether or not the filename has the
   extension ".scm": If it does, then scheme-style comments are searched and
   used, otherwise C-style comments are.

=head1 AUTHOR

  John Ralls <jralls@ceridwen.us>

=head1 COPYRIGHT

  Copyright 2015 John Ralls

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, contact:

  Free Software Foundation           Voice:  +1-617-542-5942
  51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
  Boston, MA  02110-1301,  USA       gnu@gnu.org
=cut
