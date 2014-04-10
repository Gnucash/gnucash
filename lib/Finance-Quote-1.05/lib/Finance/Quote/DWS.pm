#!/usr/bin/perl -w
#
#    Copyright (C) 1998, Dj Padzensky <djpadz@padz.net>
#    Copyright (C) 1998, 1999 Linas Vepstas <linas@linas.org>
#    Copyright (C) 2000, Yannick LE NY <y-le-ny@ifrance.com>
#    Copyright (C) 2000, Paul Fenwick <pjf@Acpan.org>
#    Copyright (C) 2000, Brent Neal <brentn@users.sourceforge.net>
#    Copyright (C) 2000, Volker Stuerzl <volker.stuerzl@gmx.de>
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
#
# $Id$

package Finance::Quote::DWS;
require 5.005;

use strict;
use LWP::UserAgent;
use HTTP::Request::Common;

use vars qw/$VERSION/;

$VERSION = '1.00';

sub methods { return (dwsfunds => \&dwsfunds); }
sub labels { return (dwsfunds => [qw/exchange name date price method/]); }

# =======================================================================
# The dwsfunds routine gets quotes of DWS funds (Deutsche Bank Gruppe)
# On their website DWS provides a csv file in the format
#    symbol1,price1,date1
#    symbol2,price2,date2
#    ...
#
# This subroutine was written by Volker Stuerzl <volker.stuerzl@gmx.de>

sub dwsfunds
{
  my $quoter = shift;
  my @funds = @_;
  return unless @funds;
  my $ua = $quoter->user_agent;
  my (%fundhash, @q, @date, %info);

  # create hash of all funds requested
  foreach my $fund (@funds)
  {
    $fundhash{$fund} = 0;
  }

  # get csv data
  my $response = $ua->request(GET &dwsurl);
  if ($response->is_success)
  {
    # process csv data
    foreach (split('\015?\012',$response->content))
    {
      @q = $quoter->parse_csv($_) or next;
      if (exists $fundhash{$q[0]})
      {
        $fundhash{$q[0]} = 1;

        # convert price from german (0,00) to US format (0.00)
        $q[1] =~ s/,/\./;

        # convert date from german (dd.mm.yyyy) to US format (mm/dd/yyyy)
        @date = split /\./, $q[2];
        $q[2] = $date[1]."/".$date[0]."/".$date[2];

        $info{$q[0], "exchange"} = "DWS";
        $info{$q[0], "name"}     = $q[0];
        $info{$q[0], "price"}    = $q[1];
        $info{$q[0], "last"}     = $q[1];
        $info{$q[0], "date"}     = $q[2];
        $info{$q[0], "method"}   = "dwsfunds";
        $info{$q[0], "currency"} = "EUR";
        $info{$q[0], "success"}  = 1;
      }
    }

    # check to make sure a value was returned for every fund requested
    foreach my $fund (keys %fundhash)
    {
      if ($fundhash{$fund} == 0)
      {
        $info{$fund, "success"}  = 0;
        $info{$fund, "errormsg"} = "No data returned";
      }
    }
  }
  else
  {
    foreach my $fund (@funds)
    {
      $info{$fund, "success"}  = 0;
      $info{$fund, "errormsg"} = "HTTP error";
    }
  }

  return wantarray() ? %info : \%info;
}

# DWS provides csv files containing the prices of all their funds for the 5
# most recent business days. The file names are ordered numerically, that is
# dws1.csv contains prices for monday, dws2.csv those for tuesday and so on.
# The files are updated until 6:00pm on every business day. Before that time
# the file of the previous day has to be used.
sub dwsurl
{
  # Since DWS is located at Frankfurt/Germany, this code only works for
  # central european time zone
  my @time = localtime;
  my $hour = $time[2];
  my $wday = $time[6];

  # during weekend use file of friday
  if ($wday == 6 || $wday == 0)
  {
    $wday = 5;
  }
  
  # on business days before 6:00pm use file of previous day
  else
  {
    if ($hour < 18)
    {
      $wday--;
      if ($wday == 0) { $wday = 5; };
    }
  }

  return "http://www.dws.de/aktuell/dws".$wday.".csv";
}

1;

=head1 NAME

Finance::Quote::DWS	- Obtain quotes from DWS (Deutsche Bank Gruppe).

=head1 SYNOPSIS

    use Finance::Quote;

    $q = Finance::Quote->new;

    %stockinfo = $q->fetch("dwsfunds","847402");

=head1 DESCRIPTION

This module obtains information about DWS managed funds.

Information returned by this module is governed by DWS's terms
and conditions.

=head1 LABELS RETURNED

The following labels may be returned by Finance::Quote::DWS:
exchange, name, date, price, last.

=head1 SEE ALSO

DWS (Deutsche Bank Gruppe), http://www.dws.de/

=cut
