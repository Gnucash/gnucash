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
# This code derived from Padzensky's work on package Finance::YahooQuote,
# but extends its capabilites to encompas a greater number of data sources.
#
# This code was developed as part of GnuCash <http://www.gnucash.org/>

package Finance::Quote::Yahoo::Europe;
require 5.005;

use strict;
use HTTP::Request::Common;
use LWP::UserAgent;
use Finance::Quote::Yahoo::Base qw/yahoo_request base_yahoo_labels/;

use vars qw($VERSION $YAHOO_EUROPE_URL);

$VERSION = '1.02';

# URLs of where to obtain information.

$YAHOO_EUROPE_URL = ("http://finance.uk.yahoo.com/d/quotes.csv");

sub methods {return (europe => \&yahoo_europe,yahoo_europe => \&yahoo_europe)};

{
	my @labels = (base_yahoo_labels(),"currency","method");

	sub labels { return (europe => \@labels, yahoo_europe => \@labels); }
}

# =======================================================================
# yahoo_europe gets quotes for European markets from Yahoo.
sub yahoo_europe
{
	my $quoter = shift;
	my @symbols = @_;
	return unless @symbols;	# Nothing if no symbols.

	# This does all the hard work.
	my %info = yahoo_request($quoter,$YAHOO_EUROPE_URL,\@symbols);

	foreach my $symbol (@symbols) {
		if ($info{$symbol,"success"}) {
			$info{$symbol,"method"} = "yahoo_europe";

			# London sources return in pence, not Euros.
			# We'd like them to return in pounds (divide
			# by 100).

			if ($symbol =~ /\.L$/i) {
				$info{$symbol,"currency"} = "GBP";
				foreach my $field ($quoter->default_currency_fields) {
					next unless ($info{$symbol,$field});
					$info{$symbol,$field} = $quoter->scale_field($info{$symbol,$field},0.01);
				}
			} elsif (substr($symbol,0,1) ne "^") {
				# All other non-indexes are in Euros.
				$info{$symbol,"currency"} = "EUR";
			} else {
				$info{$symbol,"currency"} = undef;
			}
		}
	}

	return %info if wantarray;
	return \%info;
}

1;

=head1 NAME

Finance::Quote::Yahoo::Europe - Fetch quotes from Yahoo Europe

=head1 SYNOPSIS

    use Finance::Quote;
    $q = Finance::Quote->new;

    %info = $q->fetch("europe","12150.PA"); # Failover to other methods ok.
    %info = $q->fetch("yahoo_europe","12150.PA"); # Use this module only.

=head1 DESCRIPTION

This module fetches information from Yahoo Europe.  Symbols should be
provided in the format "SYMBOL.EXCHANGE", where the exchange code is
one of the following:

	PA - Paris
	BC - Barcelona
	BE - Berlin
	BI - Bilbao
	BR - Breme
	CO - Copenhagen
	D  - Dusseldorf
	F  - Frankfurt
	H  - Hamburg
	HA - Hanover
	L  - London
	MA - Madrid
	MC - Madrid (M.C.)
	MI - Milan
	MU - Munich
	O  - Oslo
	ST - Stockholm
	SG - Stuttgart
	VA - Valence
	DE - Xetra (was FX)

This module provides both the "europe" and "yahoo_europe" methods.
The "europe" method should be used if failover methods are desirable.
The "yahoo_europe" method should be used you desire to only fetch
information from Yahoo Europe.

This module is loaded by default by Finance::Quote, but can be loaded
explicitly by specifying the parameter "Yahoo::Europe" to
Finance::Quote->new().

Information obtained by this module may be covered by Yahoo's terms
and conditions.  See http://finance.uk.yahoo.com/ for more details.

=head1 LABELS RETURNED

This module returns all the standard labels (where available) provided
by Yahoo.  See Finance::Quote::Yahoo::Base for a list of these.  The
currency label is also returned.

=head1 SEE ALSO

Yahoo Europe, http://finance.uk.yahoo.com/

=cut
