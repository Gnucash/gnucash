#!/usr/bin/perl -w
use strict;
use Test;
BEGIN {plan tests => 8};

use Finance::Quote;

# Test Yahoo functions.

my $q      = Finance::Quote->new();

my %quotes = $q->yahoo("IBM","SGI","BOGUS");
ok(%quotes);

# Check the last values are defined.  These are the most
#  used and most reliable indicators of success.
ok($quotes{"IBM","last"} > 0);
ok($quotes{"IBM","success"});
ok($quotes{"IBM", "currency"} eq "USD");

ok($quotes{"SGI","last"} > 0);
ok($quotes{"SGI","success"});

# Make sure there are no spurious % signs.

ok($quotes{"SGI","p_change"} !~ /%/);

# Check that bogus stocks return failure:

ok(! $quotes{"BOGUS","success"});
