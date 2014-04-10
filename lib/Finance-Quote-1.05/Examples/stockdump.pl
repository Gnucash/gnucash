#!/usr/bin/perl -w
use strict;
use lib '../lib';
use Finance::Quote;
use Data::Dumper;
use Getopt::Std;

# A very very simple script.  Takes a source and a symbol, looks it up,
# and dumps it to STDOUT.  Useful for debugging.

my %options = ('c' => '');

getopts('c:',\%options);

die "Usage: $0 [-c currency] source symbol\n" unless (defined $ARGV[1]);

my $q = Finance::Quote->new;

if ($options{'c'}) {
	$q->set_currency($options{'c'});
}

my %quotes = $q->fetch(@ARGV);

print Dumper(\%quotes);

