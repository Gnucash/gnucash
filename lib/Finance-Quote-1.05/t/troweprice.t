#!/usr/bin/perl -w
use strict;
use Test;
BEGIN {plan tests => 9};

use Finance::Quote;

# Test troweprice functions.

my $q      = Finance::Quote->new();

my %quotes = $q->troweprice;
ok(%quotes);

# Check that nav and date are defined as our tests.
ok($quotes{"PRFDX","nav"} > 0);
ok($quotes{"PRFDX","success"});
ok($quotes{"PRFDX","currency"} eq "USD");
ok(length($quotes{"PRFDX","date"}) > 0);


ok($quotes{"PRIDX","success"});
ok($quotes{"PRIDX","nav"} > 0);
ok(length($quotes{"PRIDX","date"}) > 0);

# Check a bogus fund returns no-success

%quotes = $q->troweprice("BOGUS");
ok(! $quotes{"BOGUS","success"});
