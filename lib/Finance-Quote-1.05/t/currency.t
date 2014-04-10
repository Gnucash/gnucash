#!/usr/bin/perl -w
use strict;
use Test;
BEGIN {plan tests => 8};

use Finance::Quote;

# Test currency conversion, both explicit requests and automatic
# conversion.

my $q      = Finance::Quote->new();

# Explicit conversion...
ok($q->currency("USD","AUD"));
ok($q->currency("EUR","JPY"));
ok(! defined($q->currency("XXX","YYY")));
ok(($q->currency("10 AUD","AUD")) == (10 * ($q->currency("AUD","AUD"))));

# Euros into French Francs are fixed at a conversion rate of
# 1:6.559576 .  We can use this knowledge to test that a stock is
# converting correctly.

my %baseinfo = $q->fetch("europe","12150.PA");
ok($baseinfo{"12150.PA","success"});

$q->set_currency("FRF");	# All new requests in French Francs.

my %info = $q->fetch("europe","12150.PA");
ok($info{"12150.PA","success"});
ok($info{"12150.PA","currency"} eq "FRF");
ok($info{"12150.PA","price"} > 0);
