#!/usr/bin/perl -w
use strict;
use lib '../lib';
use Finance::Quote;

# This script demonstrates how currencies can be converted using
# Finance::Quote.

# Example usage:   currency-lookup.pl USD AUD
# (Converts from US Dollars to Australian Dollars)

die "Usage: $0 FROM TO\n" unless defined($ARGV[1]);

my $q = Finance::Quote->new();

my $exchange_rate = $q->currency($ARGV[0],$ARGV[1]);

die "Urgh!  Nothing back\n" unless $exchange_rate;

print $ARGV[0]."->".$ARGV[1]." = ".$exchange_rate."\n";
