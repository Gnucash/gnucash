#!/usr/bin/perl -w

# Test to see if Finance::Quote can at least be loaded and used.
# This file gets a capital name so it will be run before any other
# test.

use strict;
use Test;
BEGIN {plan tests => 2};

use Finance::Quote;
ok(1);			# Yup.  It loaded okay.  Good.  :)

my $quote = Finance::Quote->new();

ok($quote);	# Did we get an object okay?
