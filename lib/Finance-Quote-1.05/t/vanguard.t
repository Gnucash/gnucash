#!/usr/bin/perl -w
use strict;
use Test;
BEGIN {plan tests => 18};

use Finance::Quote;

# Test Vanguard functions.

my $q      = Finance::Quote->new();
my @funds = qw/VBINX VIVAX VWINX VFIIX/;

my %quotes = $q->vanguard(@funds);
ok(%quotes);

# Check that the name and last are defined for all of the funds.
foreach my $fund (@funds) {
	ok($quotes{$fund,"last"} > 0);
	ok(length($quotes{$fund,"name"}));
	ok($quotes{$fund,"success"});
        ok($quotes{$fund, "currency"} eq "USD");
}

# Make sure we're not getting spurious percentage signs.

ok($quotes{"VBINX","p_change"} !~ /%/);
