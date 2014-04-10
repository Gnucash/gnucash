#!/usr/bin/perl -w
use strict;
use Test;
use Data::Dumper;
BEGIN {plan tests => 9};

use Finance::Quote;

# Test Yahoo_europe functions.

my $q      = Finance::Quote->new();

my %quotes = $q->yahoo_europe("12150.PA","BOGUS");
ok(%quotes);

# Check the nav values are defined.  These are the most
#  used and most reliable indicators of success.
ok($quotes{"12150.PA","last"} > 0);
ok(length($quotes{"12150.PA","name"}) > 0);
ok($quotes{"12150.PA","success"});
ok($quotes{"12150.PA", "currency"} eq "EUR");

# Make sure we don't have spurious % signs.

ok($quotes{"12150.PA","p_change"} !~ /%/);

# Check that a bogus stock returns no-success.
ok(! $quotes{"BOGUS","success"});

# London stocks should be returned in British Pounds (GBP).

my %londonquotes = $q->fetch("yahoo_europe","BAY.L");
ok($londonquotes{"BAY.L","success"});
ok($londonquotes{"BAY.L","currency"} eq "GBP");
