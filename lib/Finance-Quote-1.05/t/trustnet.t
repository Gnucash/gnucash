#!/usr/bin/perl -w
use strict;
use Test;

BEGIN {plan tests => 8 };

use Finance::Quote;

# Test trustnet functions.

my $q = Finance::Quote->new();

my @stocks = ("ABBEY AMERICAN GROWTH","PERPETUAL GLOBAL BOND");

my %quotes = $q->fetch("trustnet",@stocks);

ok(%quotes);

# For each of our stocks, check to make sure we got back some
# useful information.

foreach my $stock (@stocks) {
	ok($quotes{$stock,"success"});
	ok($quotes{$stock,"price"});
	ok($quotes{$stock,"date"});
}

# Test that a bogus stock gets no success.

%quotes = $q->fetch("trustnet","BOGUS");
ok(! $quotes{"BOGUS","success"});
