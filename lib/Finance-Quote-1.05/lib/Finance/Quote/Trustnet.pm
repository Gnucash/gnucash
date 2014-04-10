#!/usr/bin/perl -w
#
#    Copyright (C) 1998, Dj Padzensky <djpadz@padz.net>
#    Copyright (C) 1998, 1999 Linas Vepstas <linas@linas.org>
#    Copyright (C) 2000, Yannick LE NY <y-le-ny@ifrance.com>
#    Copyright (C) 2000, Paul Fenwick <pjf@cpan.org>
#    Copyright (C) 2000, Brent Neal <brentn@users.sourceforge.net>
#    Copyright (C) 2000, Keith Refson <Keith.Refson@earth.ox.ac.uk>
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

package Finance::Quote::Trustnet;
require 5.004;

use strict;

use vars qw($VERSION $TRUSTNET_URL $TRUSTNET_ALL);

use LWP::UserAgent;
use HTTP::Request::Common;
use HTML::TableExtract;

$VERSION = '1.01';

# URLs of where to obtain information.

$TRUSTNET_URL = ("http://www.trustnet.co.uk/ut/funds/perf.asp?sort=0&txtS=");

$TRUSTNET_ALL="http://www.trustnet.co.uk/ut/funds/perf.asp?sort=0";

sub methods { return (uk_unit_trusts => \&trustnet, trustnet => \&trustnet); }

{
        my @labels = qw/exchange method source name currency bid ask yield price/;
	
	sub labels { return (trustnet => \@labels, 
	                     uk_unit_trusts => \@labels); }
}

# =======================================================================

sub trustnet
  {
    my $quoter = shift;
    my @symbols = @_;
    
    return unless @symbols;
    my(@q,%aa,$ua,$url,$sym,$ts,$date,$price,$currency,$reply,$trust);
    my ($row, $datarow, $matches);
    my %curr_iso = ("£" => "GBP", "\$" => "USD");
    my( $isodate, $day, $month, $year);
    my %imonth = ( "Jan" => "01", "Feb" => "02", "Mar" => "03",
                   "Apr" => "04", "May" => "05", "Jun" => "06",
		   "Jul" => "07", "Aug" => "08", "Sep" => "09",
		   "Oct" => "10", "Nov" => "11", "Dec" => "12");
    
    my %symbolhash;
    @symbolhash{@symbols} = map(1,@symbols);
    # 
    for (@symbols) {
      my $te = new HTML::TableExtract( headers => [("Fund Name", "Group Name", "Bid Price", "Offer Price", "Yield")]);
      $trust = $_;
      $url = "$TRUSTNET_URL$trust";
      
      # print STDERR "Retrieving \"$trust\" from $url\n";
      $ua = $quoter->user_agent;
      $reply = $ua->request(GET $url);
      return unless ($reply->is_success);
      
      # print STDERR $reply->content,"\n";
      
      $te->parse($reply->content);
      $ts  = ($te->table_states)[0];
      
      if( defined ($ts)) {
	
	$matches = 0;
	foreach $row ($ts->rows) {
	  ($sym = $$row[0]) =~ s/^ +//;
	  if ($sym =~ /$trust/i) {
	    $matches++;
	    $datarow = $row;
	  }
	}
	if ($matches > 1 ) {
	  $aa {$trust, "success"} = 0;
	  $aa {$trust, "errormsg"} = "Fund name $trust is not unique.  See \"$TRUSTNET_ALL\"";
	  next;
	} elsif ($matches < 1 ) {
	  $aa {$trust, "success"} = 0;
	  #$aa {$trust, "errormsg"} = "Fund name $trust is not found.  See \"$TRUSTNET_ALL\"";
	  $aa {$trust, "errormsg"} = "Error retrieving  $trust -- unexpected input";
	  next;
	} else {
	  $aa {$trust, "exchange"} = "Trustnet";
	  $aa {$trust, "method"} = "trustnet";
	  $aa {$trust, "source"} = "http://www.trustnet.co.uk/";
	  # ($aa {$trust, "name"} = $$datarow[0]) =~ s/^ +//;
	  $aa {$trust, "name"} = $trust; # no name supplied ... 
	  ($price = $$datarow[2]) =~ s/\s*\((.*)\)//;
	  $currency=$1;
	  $aa {$trust, "currency"} = $curr_iso{"$currency"};
	  $aa {$trust, "bid"} = $price * 0.01;
	  ($price = $$datarow[3]) =~ s/\s*\((.*)\)//;
	  $price = $aa {$trust, "bid"} if  $price eq "";
	  $aa {$trust, "ask"} = $price * 0.01;
	  $aa {$trust, "yield"} = $$datarow[4];
	  $aa {$trust, "price"} = $aa{$trust,"bid"};
	  $aa {$trust, "success"} = 1;
	  # print STDERR "Trustnet:: Flagging success for $trust\n";
	  #
	  # Get date and convert to US format (Ugh!)
	  $date= "notfound";
	  if ( $reply->content =~ 
	       /Source : TrustNet - ([\w\d-]+) - http:\/\/www.trustnet.com/m) {
	    $isodate=$1;
	    ($day,$month,$year) = ($isodate =~ /([0-9]+)-(\w+)-([0-9]+)/);
	    $date = $imonth{$month}."/".$day."/".$year;
	  }
	  $aa {$trust, "date"} = $date;
	}
      } else {
	$aa {$trust, "success"} = 0;
	$aa {$trust, "errormsg"} = "Fund name $trust is not found.  See \"$TRUSTNET_ALL\"";
	next;
      }
    }
    return %aa if wantarray;
    return \%aa;
  }

1;

=head1 NAME

Finance::Quote::Trustnet	- Obtain unit trust prices from trustnet.co.uk

=head1 SYNOPSIS

    use Finance::Quote;

    $q = Finance::Quote->new;

    %stockinfo = $q->fetch("unit_trusts","trust-name"); # Can failover to other methods
    %stockinfo = $q->fetch("trustnex","trust-name"); # Use this module only.

=head1 DESCRIPTION

This module obtains information about UK unit trust prices from
www.trustnet.co.uk.  The information source "unit_trusts" can be used
if the source of prices is irrelevant, and "trustnet" if you
specifically want to use trustnet.co.uk.

=head1 UNIT TRUST NAMES

Unfortunately there is no unique identifier for unit trust names.
Therefore enough of the name should be given including spaces to yield
a unique match.  Trustnet sometimes uses abbreviated names, and the
string given should match the abbreviation.

Consult http://www.trustnet.co.uk/ut/funds/perf.asp?sort=0
to find a match for your unit trusts.

Example "jupiter income"

=head1 LABELS RETURNED

Information available from Trustnet may include the following labels:
exchange method source name currency bid ask yield price.  In case of
a unit trust, "price" returns the offer (ask) price.  In case of an
OIEC, the unique price is returned in "bid", "ask" and "price".

=head1 SEE ALSO

Trustnet website - http://www.trustnet.co.uk/

Finance::Quote::Yahoo::USA

=cut
