#!/usr/bin/perl -w
#
#    Copyright (C) 1998, Dj Padzensky <djpadz@padz.net>
#    Copyright (C) 1998, 1999 Linas Vepstas <linas@linas.org>
#    Copyright (C) 2000, Yannick LE NY <y-le-ny@ifrance.com>
#    Copyright (C) 2000, Paul Fenwick <pjf@cpan.org>
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

package Finance::Quote::Yahoo::USA;
require 5.005;

use strict;
use HTTP::Request::Common;
use LWP::UserAgent;
use Finance::Quote::Yahoo::Base qw/yahoo_request base_yahoo_labels/;

use vars qw/$VERSION $YAHOO_URL/;

$VERSION = '1.00';

# URLs of where to obtain information.

$YAHOO_URL = ("http://finance.yahoo.com/d");

sub methods {return (canada   => \&yahoo,
                     usa      => \&yahoo,
		     yahoo    => \&yahoo,
		     nyse     => \&yahoo,
		     nasdaq   => \&yahoo,
		     vanguard => \&yahoo,
		     fidelity => \&yahoo_fidelity)};

{
	my @labels = (base_yahoo_labels(),"currency", "method");

	sub labels { return (canada	=> \@labels,
			     usa	=> \@labels,
			     yahoo	=> \@labels,
			     nyse	=> \@labels,
			     nasdaq	=> \@labels,
			     vanguard	=> \@labels,
			     fidelity   => [@labels,'yield','nav']); }
}

# This is a special wrapper to provide information compatible with
# the primary Fidelity function of Finance::Quote.  It does a good
# job of a failover.
{

	# Really this list should be common for both the Fidelity.pm
	# and this module.  We could possibly get away with checking
	# for /XX$/, but I don't know how reliable that is.

	my %yield_funds = (FDRXX => 1,
	                   FDTXX => 1,
			   FGMXX => 1,
			   FRTXX => 1,
			   SPRXX => 1,
			   SPAXX => 1,
			   FDLXX => 1,
			   FGRXX => 1);

	sub yahoo_fidelity {
		my $quoter = shift;
		my @symbols = @_;
		return unless @symbols;

		# Call the normal yahoo function (defined later in this
		# file).

		my %info = yahoo($quoter,@symbols);

		foreach my $symbol (@symbols) {
			next unless $info{$symbol,"success"};
			if ($yield_funds{$symbol}) {
				$info{$symbol,"yield"}=$info{$symbol,"price"};
			} else {
				$info{$symbol,"nav"} = $info{$symbol,"price"};
			}
		}

		return wantarray ? %info : \%info;
	}
}

sub yahoo
{
	my $quoter = shift;
	my @symbols = @_;
	return unless @symbols;	# Nothing if no symbols.

	# This does all the hard work.
	my %info = yahoo_request($quoter,$YAHOO_URL,\@symbols);

	foreach my $symbol (@symbols) {
		# Yahoo indexes all start with a hat, so don't
		# tag them with a currency.
		if ($info{$symbol,"success"} and $symbol !~ /^\^/) {
			$info{$symbol,"currency"} = "USD";
			$info{$symbol,"method"} = "yahoo";
		}
	}
	return %info if wantarray;
	return \%info;
}

1;

=head1 NAME

Finance::Quote::Yahoo::USA - Obtain information about stocks and funds
in the USA and Canada.

=head1 SYNOPSIS

    use Finance::Quote;

    $q = Finance::Quote->new;

    %info = $q->fetch("usa","SGI");

=head1 DESCRIPTION

This method provides access to financial information from a number
of exhcanges in the United States and Canada.  The following methods
are available:

	canada
	usa
	yahoo
	nyse
	nasdaq
	vanguard
	fidelity

These methods all use the same information source, and hence can
be considered somewhat interchangable.  However, the method "yahoo"
should be passed to fetch if you wish to obtain information
from any source that Yahoo tracks.

This method is loaded by default by Finance::Quote, although it
can be explicitly loaded by passing the argument "Yahoo::USA"
to Finance::Quote->new().

Information returned by this module may be subject to Yahoo's
terms and conditions.  See http://finance.yahoo.com/ for more
information.

=head1 LABELS RETURNED

This module returns all the standard labels that Yahoo provides,
as well as the currency label.  See Finance::Quote::Yahoo::Base
for more information.

=head1 BUGS

Yahoo does not make a distinction between the various exchanges
in the United States and Canada.  For example, it is possible to request
a stock using the "NYSE" method and still obtain data even if that stock
does not exist on the NYSE but exists on a different exchange.

=head1 SEE ALSO

Yahoo Finance, http://finance.yahoo.com/

Finance::Quote::Base

=cut
