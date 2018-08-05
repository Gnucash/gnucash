#!/usr/bin/perl -w

# get_quotes.pl -- Addition to example Script quotes_historc.py. Reads online stock quotes to file INTC.
#
 
##  @file
#   @brief Addition to example Script quotes_historic.py. Reads online stock quotes to file INTC.
#   @author Peter Holtermann
#   @date January 2011
#   @ingroup python_bindings_examples
#  
#   Call this script before calling @code 
#   python quotes_historic.py
#   @endcode 
# 
#   For explanation of use have a look at the wiki:
#   http://wiki.gnucash.org/wiki/Stocks/get_prices
#
#   @cond PERL

use Finance::QuoteHist;
print "Will get stock quotes of $ARGV[0] and save it into the file $ARGV[0]\n";
$fname = $ARGV[0];
   open (MYFILE, ">$fname");
   $q = Finance::QuoteHist->new
      (
       symbols    => [($ARGV[0])],
       start_date => '01/01/2000',
       end_date   => 'today',
      ); 


print "name,date, open, high, low, close, volume\n";
foreach $row ($q->quotes()) {
       ($name,$date, $open, $high, $low, $close, $volume) = @$row;
       print MYFILE "$name,$date, $open, $high, $low, $close, $volume\n";
   }

close(MYFILE);

## @endcond
