#!/usr/bin/perl -w
use strict;
use Test;
BEGIN {plan tests => 23};

use Finance::Quote;

# Test Fidelity functions.

my $q      = Finance::Quote->new();
my @funds = qw/FGRIX FNMIX FASGX FCONX/;

my %quotes = $q->fidelity(@funds);
ok(%quotes);

# Check that the name and nav are defined for all of the funds.
foreach my $fund (@funds) {
	ok($quotes{$fund,"nav"} > 0);
	ok(length($quotes{$fund,"name"}));
	ok($quotes{$fund,"success"});
        ok($quotes{$fund, "currency"} eq "USD");
}

# Some funds have yields instead of navs.  Check one of them too.
%quotes = $q->fidelity("FGRXX");
ok(%quotes);
ok(length($quotes{"FGRXX","name"}));
ok($quotes{"FGRXX","yield"} != 0);
ok($quotes{"FGRXX","success"});
ok($quotes{"FGRXX", "currency"} eq "USD");

# Check that a bogus fund returns no-success.
%quotes = $q->fidelity("BOGUS");
ok(! $quotes{"BOGUS","success"});
