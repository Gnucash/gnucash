#!/usr/bin/perl -w
#
# example script showing how to use the Quote perl module.
# gets prices for some stocks, for some mutual funds
#
# This script was originally part of GnuCash.

use lib '../lib';
use Finance::Quote;

my $q = Finance::Quote->new();

# -----------------------------------
# get quotes for two stocks ...
%quotes = $q->yahoo ("IBM", "SGI"); 

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
%quotes = $q->fidelity (@funds);

foreach $f (@funds) {
     $name = $quotes {$f, "name"};
     $nav = $quotes {$f, "nav"};
     print "Fidelity Fund $f $name \tNAV = $nav\n";
}
print "\n\n";

# -----------------------------------
@funds = ("FGRXX");
%quotes = $q->fidelity (@funds);

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
%quotes = $q->troweprice (@funds);

foreach $f (@funds) {
     $nav = $quotes {$f, "nav"};
     $dayte = $quotes {$f, "date"};
     print "T. Rowe Price $f NAV = $nav as of $dayte\n";
}
print "\n\n";


# -----------------------------------

# demo for ASX.  Grab the price of Coles-Myer and Telstra
@funds = ("CML","TLS");
%quotes = $q->asx(@funds);
foreach $f (@funds) {
	print "ASX Price of $f is ".$quotes{$f,"last"}." at ".
	      $quotes{$f,"date"}."\n";
}
print "\n\n";

# Demo for TIAA-CREF.
@funds = qw/CREFstok BOGOname TIAAreal CREFmony/;
%quotes = $q->tiaacref(@funds);
foreach $f (@funds) {
        if ($quotes{$f,"success"} == 1) {
	print "TIAA-CREF Price of ".$quotes{$f,"name"}." is ".$quotes{$f,"nav"}.
              " at ".$quotes{$f,"date"}."\n";
	} else {
	print "Error: ".$quotes{$f,"errormsg"}." for ".$f."\n";
	}
}
print "\n\n";
