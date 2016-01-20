#!/usr/bin/perl -w
use strict;

my $filename = $ARGV[0];
my ($last_merge, $last_rev, $msgtot, $msgtrans, $msgfuzz);
my $date_re = '(\d{4}-\d{2}-\d{2})';
my $merge_re = qr/^"POT-Creation-Date: $date_re/;
my $rev_re = qr/^"PO-Revision-Date: $date_re/;
my $msgid_re = qr(^msgid ".+");
my $str_re = qr/^msgstr ".+"/;

while (<>) {
    $last_merge = $1 if ($_ =~ $merge_re);
    $last_rev = $1 if ($_ =~ $rev_re);
}
my ($strings, $fuzzy, $missing);
open STATS, "msgfmt --stat $filename 2>&1 |" or die;
while (<STATS>) {
    #print;
    $_ =~ /^(\d+) translated messages(?:, (\d+) fuzzy translations)?(?:, (\d+) untranslated messages)?./;
    $strings = $1 || 0;
    $fuzzy = $2 || 0;
    $missing = $3 ||0;
}
close STATS;
my $total = ($strings + $fuzzy + $missing) || 1;
my $percent = $strings/$total * 100.0;
my $fuzpct = ($strings + $fuzzy) / $total * 100.0;
printf "%14s: %s %s\t%4d\t%4d\t%4d\t%3.2f%%\t %3.2f%%\n", $filename, $last_rev,
        $last_merge, $strings, $fuzzy, $total, $percent, $fuzpct;

=head1 NAME

check-po.pl

=head1 SYNOPSIS

check-po.pl po-filename

=head1 SUMMARY

Prints the filename, last revision date, date of the potfile with which it was
last merged, the number of translated strings not marked fuzzy, the number of
translated strings marked fuzzy, the total number of translatable strings, the
percentage of strings with not-fuzzy translations and the percentage of strings
with both fuzzy and non-fuzzy translations.

Most effectively used in a shell for loop, e.g.:

C<for i in *.po; do check-po.pl $i; done>

The results of which can be piped to sort(1) for simple analysis.

=cut
