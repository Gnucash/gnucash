# perl -w
#
#    Copyright (C) 1998, Dj Padzensky <djpadz@padz.net>
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
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

package Finance::YahooQuote;
require 5.000;

require Exporter;
use strict;
use vars qw($VERSION @EXPORT @ISA $QURL);

use LWP::UserAgent;
use HTTP::Request::Common;

$VERSION = '0.06';
$QURL = ("http://quote.yahoo.com/d?f=snl1d1t1c1p2va2bapomwerr1dyj1&s=");
@ISA = qw(Exporter);
@EXPORT = qw(&getquote &getonequote);

sub getquote {
    my @symbols = @_;
    my($x,@q,@qr,$ua,$url);
    $x = $";
    $" = "+";
    $url = $QURL."@symbols";
    $" = $x;
    $ua = LWP::UserAgent->new;
    foreach (split('\n',$ua->request(GET $url)->content)) {
	@q = grep { s/^"?(.*?)\s*"?\s*$/$1/; } split(',');
	push(@qr,[@q]);
    }
    return wantarray() ? @qr : \@qr;
}

# Input: A single stock symbol
# Output: An array, containing the list elements mentioned above.

sub getonequote {
    my @x;
    @x = &getquote($_[0]);
    return @{$x[0]} if defined @x;
}

__END__

1;

=head1 NAME

Finance::YahooQuote - Get a stock quote from Yahoo!

=head1 SYNOPSIS

  use Finance::YahooQuote;
  @quote = getonequote $symbol;	# Get a quote for a single symbol
  @quotes = getquote @symbols;	# Get quotes for a bunch of symbols

=head1 DESCRIPTION

This module gets stock quotes from Yahoo! Finance.  The B<getonequote>
function will return a quote for a single stock symbol, while the
B<getquote> function will return a quote for each of the stock symbols
passed to it.  The return value of B<getonequote> is an array, with
the following elements:

    0 Symbol
    1 Company Name
    2 Last Price
    3 Last Trade Date
    4 Last Trade Time
    5 Change
    6 Percent Change
    7 Volume
    8 Average Daily Vol
    9 Bid
    10 Ask
    11 Previous Close
    12 Today's Open
    13 Day's Range
    14 52-Week Range
    15 Earnings per Share
    16 P/E Ratio
    17 Dividend Pay Date
    18 Dividend per Share
    19 Dividend Yield
    20 Market Capitalization

The B<getquote> function returns an array of pointers to arrays with
the above structure.

=head1 COPYRIGHT

Copyright 1998, Dj Padzensky

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

The information that you obtain with this library may be copyrighted
by Yahoo! Inc., and is governed by their usage license.  See
http://www.yahoo.com/docs/info/gen_disclaimer.html for more
information.

=head1 AUTHOR

Dj Padzensky (C<djpadz@padz.net>), PadzNet, Inc.

The Finance::YahooQuote home page can be found at
http://www.padz.net/~djpadz/YahooQuote/

=cut
