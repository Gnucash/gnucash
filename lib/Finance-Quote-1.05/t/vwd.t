#!/usr/bin/perl -w
use strict;
use Test;
BEGIN {plan tests => 7};

use Finance::Quote;

# Test vwd functions.

my $q      = Finance::Quote->new("VWD");

my %quotes = $q->vwd("847402","BOGUS");
ok(%quotes);

# Check that the last and date values are defined.
ok($quotes{"847402","success"});
ok($quotes{"847402","last"} > 0);
ok(length($quotes{"847402","date"}) > 0);
ok($quotes{"847402","currency"} eq "EUR");

# Check that a bogus fund returns no-success.
ok($quotes{"BOGUS","success"} == 0);
ok($quotes{"BOGUS","errormsg"} eq "Parse error");
