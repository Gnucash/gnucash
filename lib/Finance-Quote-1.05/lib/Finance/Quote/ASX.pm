#!/usr/bin/perl -w
#
#    Copyright (C) 1998, Dj Padzensky <djpadz@padz.net>
#    Copyright (C) 1998, 1999 Linas Vepstas <linas@linas.org>
#    Copyright (C) 2000, Yannick LE NY <y-le-ny@ifrance.com>
#    Copyright (C) 2000, Paul Fenwick <pjf@Acpan.org>
#    Copyright (C) 2000, Brent Neal <brentn@users.sourceforge.net>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
#    02111-1307, USA
#
#
# This code derived from Padzensky's work on package Finance::YahooQuote,
# but extends its capabilites to encompas a greater number of data sources.
#
# This code was developed as part of GnuCash <http://www.gnucash.org/>

require 5.005;

use strict;

package Finance::Quote::ASX;

use HTTP::Request::Common;
use LWP::UserAgent;

use vars qw/$ASX_URL $VERSION/;

$VERSION = "1.02";

$ASX_URL = 'http://www.asx.com.au/nd50/nd_ISAPI_50.dll/asx/markets/EquitySearchResults.jsp?method=post&template=F1001&ASXCodes=';

sub methods {return (australia => \&asx,asx => \&asx)}

{
	my @labels = qw/name last p_change bid offer high low volume
	                price method exchange/;

	sub labels { return (australia => \@labels,
	                     asx       => \@labels); }
}

# Australian Stock Exchange (ASX)
# The ASX provides free delayed quotes through their webpage.
#
# Maintainer of this section is Paul Fenwick <pjf@cpan.org>

sub asx {
	my $quoter = shift;
	my @stocks = @_;
	return unless @stocks;
	my %info;

	my $ua = $quoter->user_agent;

	my $response = $ua->request(GET $ASX_URL.join("%20",@stocks));
	unless ($response->is_success) {
		foreach my $stock (@stocks) {
			$info{$stock,"success"} = 0;
			$info{$stock,"errormsg"} = "HTTP session failed";
		}
		return wantarray() ? %info : \%info;
	}
	my $reply = $response->content;

	# These first two steps aren't really needed, but are done for
	# safety.

	# Remove the bottom part of the page.
	$reply =~ s#</table>\s*\n<table>.*$##s;
	# Remove top of page.
	$reply =~ s#.*Chart</font></a></td></tr><tr><td nowrap valign=top>##s;

	# Grab the values
	my @values;

	while ($reply =~ m#<font size='2' face='Arial' color='\#000051'>([^<]*).*?</Font>#g) {
		push @values, $1;
	}

	if (@values % 13) { # Wrong number of fields?  Damn!
		warn "Bad number of fields returned from ASX website in ".
		     "Finance::Quote::ASX.  Aborting query.\n";

		foreach my $stock (@stocks) {
			$info{$stock,"success"}  = 0;
			$info{$stock,"errormsg"} = "ASX website corrupted.";
		}
		return %info if wantarray;
		return \%info;
	}

	# Go through all the values and pack them into our structure.
	# We rely upon the particular ordering of fields at the ASX,
	# so this is a little dangerous.

	while (@values) {
		my $stock = shift @values;
		$stock =~ s/&nbsp;//;	# Remove guff.

		foreach my $label (qw/name last p_change bid offer open
			      high low volume JUNK JUNK JUNK/) {

			my $value = shift @values;
			next if ($label eq "JUNK");

			# Clean the value.

			$value =~ tr/$,%//d;
			$value =~ s/&nbsp;//;

			$info{$stock,$label} = $value;
		}

		# If that stock does not exist, it will have a empty
		# string for all the fields.  The "last" price should
		# always be defined (even if zero), if we see an empty
		# string here then we know we've found a bogus stock.

		if ($info{$stock,'last'} eq '') {
			$info{$stock,'success'} = 0;
			$info{$stock,'errormsg'}="Stock does not exist on ASX.";
			next;
		}

		# The ASX returns zeros for a number of things if there
		# has been no trading.  This not only looks silly, but
		# can break things later.  "correct" zero'd data.

		foreach my $label (qw/open high low/) {
			if ($info{$stock,$label} == 0) {
				$info{$stock,$label} = $info{$stock,"last"};
			}
		}

		# We get a dollar plus/minus change, rather than a
		# percentage change, so we convert this into a
		# percentage change, as required.  We should never have
		# zero opening price, but if we do warn about it.

		if ($info{$stock,"open"} == 0) {
			warn "Zero opening price in p_change calcuation for ".
			     "stock $stock.  P_change set to zero.";
			$info{$stock,"p_change"} = 0;
		} else {
			$info{$stock,"p_change"} = sprintf("%.2f",
		                           ($info{$stock,"p_change"}*100)/
				             $info{$stock,"open"});
		}

		# Australian indexes all begin with X, so don't tag them
		# as having currency info.

		$info{$stock, "currency"} = "AUD" unless ($stock =~ /^X/);

		$info{$stock, "method"} = "asx";
		$info{$stock, "exchange"} = "Australian Stock Exchange";
		$info{$stock, "price"} = $info{$stock,"last"};
		$info{$stock, "success"} = 1;
	}

	# All done.

	return %info if wantarray;
	return \%info;
}

1;

=head1 NAME

Finance::Quote::ASX	- Obtain quotes from the Australian Stock Exchange.

=head1 SYNOPSIS

    use Finance::Quote;

    $q = Finance::Quote->new;

    %stockinfo = $q->fetch("asx","BHP");	   # Only query ASX.
    %stockinfo = $q->fetch("australia","BHP"); # Failover to other sources OK.

=head1 DESCRIPTION

This module obtains information from the Australian Stock Exchange
http://www.asx.com.au/.  All Australian stocks and indicies are
available.  Indexes start with the letter 'X'.  For example, the
All Ordinaries is "XAO".

This module is loaded by default on a Finance::Quote object.  It's
also possible to load it explicity by placing "ASX" in the argument
list to Finance::Quote->new().

This module provides both the "asx" and "australia" fetch methods.
Please use the "australia" fetch method if you wish to have failover
with other sources for Australian stocks (such as Yahoo).  Using
the "asx" method will guarantee that your information only comes
from the Australian Stock Exchange.

Information returned by this module is governed by the Australian
Stock Exchange's terms and conditions.

=head1 LABELS RETURNED

The following labels may be returned by Finance::Quote::ASX:
date, bid, ask, open, high, low, last, close, p_change, volume,
and price.

=head1 SEE ALSO

Australian Stock Exchange, http://www.asx.com.au/

Finance::Quote::Yahoo::Australia.

=cut
