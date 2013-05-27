#!/usr/bin/perl -w
##@file
# @brief
# example script showing how to use the Quote perl module.
# gets prices for some stocks, for some mutual funds
#
# Note that this example uses the meta-level "fetch" command.  We do
# NOT used that in Gnucash because it's behavior is unpredictable If
# the given method/exchange doesn't work, it'll fall back to other
# methods, and I've seen no guarantee that all exchanges treat all
# symbols the same.  So in Gnucash, we use the backend methods
# directly, i.e. $quoter->fidelity_direct("IBM", "LNUX");, etc.  The
# documentation page for each Finance::Quote sub-module describes how
# to call it directly without fallbacks.
#
# @cond PERL

use Finance::Quote;

# Create a quote object.
my $quoter = Finance::Quote->new();

# -----------------------------------
# get quotes for two stocks ...
%quotes = $quoter->fetch("yahoo","IBM", "SGI"); 

# print some selected values 
print "NYSE by Yahoo: ", $quotes {"IBM", "name"},  
       " last price: ", $quotes {"IBM", "last"},  "\n";
print "NYSE by Yahoo: ", $quotes {"SGI", "name"},   
       " last price: ", $quotes {"SGI", "last"},  "\n";
       
# loop over and print all values.
# Notes that values are stored ion a multi-dimensional associative array
foreach $k (sort (keys %quotes)) {
     ($sym, $attr) = split ($;, $k, 2);
     $val = $quotes {$sym, $attr};
     # $val = $quotes {$k};     # this also works, if desired ...
     print "\t$sym $attr =\t $val\n";
} 
print "\n\n";

# -----------------------------------
# get quotes from Fidelity Investments
@funds = ("FGRIX", "FNMIX", "FASGX", "FCONX");
%quotes = $quoter->fetch("fidelity",@funds);

foreach $f (@funds) {
     $name = $quotes {$f, "name"};
     $nav = $quotes {$f, "nav"};
     print "Fidelity Fund $f $name \tNAV = $nav\n";
}
print "\n\n";

# -----------------------------------
@funds = ("FGRXX");
%quotes = $quoter->fetch("fidelity",@funds);

print "Not all funds have a NAV; some have Yeilds:\n";
foreach $f (@funds) {
     $name = $quotes {$f, "name"};
     $yield = $quotes {$f, "yield"};
     print "\tFidelity $f $name 30-day Yield = $yield percent\n";
}
print "\n\n";

# -----------------------------------
# demo T. Rowe Price -- same as above
@funds = ("PRFDX", "PRIDX");
%quotes = $quoter->fetch("troweprice",@funds);

foreach $f (@funds) {
     $nav = $quotes {$f, "nav"};
     $dayte = $quotes {$f, "date"};
     print "T. Rowe Price $f NAV = $nav as of $dayte\n";
}
print "\n\n";


# -----------------------------------

# demo for ASX.  Grab the price of Coles-Myer and Telstra
@funds = ("CML","TLS");
%quotes = $quoter->fetch("australia",@funds);
foreach $f (@funds) {
	print "ASX Price of $f is ".$quotes{$f,"last"}." at ".
	      $quotes{$f,"date"}."\n";
}
print "\n\n";
##@endcond Perl
