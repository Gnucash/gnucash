#!/usr/bin/perl -w
use strict;
use Test;
BEGIN {plan tests => 11};

use Finance::Quote;

# Test ASX functions.

my $q      = Finance::Quote->new();

$q->timeout(120);	# ASX is broken regularly, so timeouts are good.

my %quotes = $q->asx("CML","BHP");
ok(%quotes);

# Check the last values are defined.  These are the most used and most
# reliable indicators of success.
ok($quotes{"CML","last"} > 0);
ok($quotes{"CML","success"});
ok($quotes{"BHP","last"} > 0);
ok($quotes{"BHP","success"});

# Exercise the fetch function a little.
%quotes = $q->fetch("asx","ITE");
ok(%quotes);
ok($quotes{"ITE","last"} > 0);
ok($quotes{"ITE","success"} > 0);

# Check that we're getting currency information.
ok($quotes{"ITE", "currency"} eq "AUD");

# Check we're not getting bogus percentage signs.
$quotes{"ITE","p_change"} ||= "";	# Avoid warning if undefined.
ok($quotes{"ITE","p_change"} !~ /%/);

# Check that looking up a bogus stock returns failure:
%quotes = $q->asx("BOG");
ok(! $quotes{"BOG","success"});

