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

# =============================================================
# Workaround by Matt Sisk for handling newlines in table cells.
# Remove when fix of HTML::TableExtract is available.
package HTML::TableExtract::Workaround;

use HTML::TableExtract;
@ISA = qw( HTML::TableExtract );

sub start {
  # Fool ourselves into translating <br> to "\n"
  my $self = shift;
  $self->text("\n") if $_[0] eq 'br';
  $self->SUPER::start(@_);
}
# =============================================================

package Finance::Quote::VWD;
require 5.005;

use strict;
use LWP::UserAgent;
use HTTP::Request::Common;
use HTML::TableExtract;

use vars qw/$VERSION $VWD_FUNDS_URL/;

$VERSION = '1.00';

$VWD_FUNDS_URL = "http://www.vwd.gfa-fonds.de/fondspreise/liste.hbs?suchtext=";

sub methods { return (vwd => \&vwd); }
sub labels { return (vwd => [qw/exchange name date price method/]); }

# =======================================================================
# The vwd routine gets quotes of funds from the website of
# vwd Vereinigte Wirtschaftsdienste GmbH.
#
# This subroutine was written by Volker Stuerzl <volker.stuerzl@gmx.de>

sub vwd
{
  my $quoter = shift;
  my @funds = @_;
  return unless @funds;
  my $ua = $quoter->user_agent;
  my %info;

  foreach my $fund (@funds)
  {
    my $response = $ua->request(GET $VWD_FUNDS_URL.$fund);
    if ($response->is_success)
    {
      # parse table
      my $te = new HTML::TableExtract::Workaround
        (headers => [qw/WKN Name Whrg Rückn Rückn Zwg Rückn Ausgabe/]);
      $te->parse($response->content);

      # extract table contents
      my @rows;
      unless (@rows = $te->rows)
      {
        $info{$fund, "success"} = 0;
        $info{$fund, "errormsg"} = "Parse error";
        next;
      }

      # split fund and company name
      my @name = split(/\n/, $rows[0][1]);

      # grab date which is contained within table header
      my $date;
      $response->content =~ /R&uuml;ckn.<BR>&nbsp;(\d{2})\.(\d{2})\.(\d{4})/;
      $date = $2."/".$1."/".$3;

      # strip whitespace and non-printable characters from price and currency
      $rows[0][2] =~ s/\W*//;
      $rows[0][3] =~ s/[^\d.]*//g;

      $info{$fund, "exchange"} = $name[1];
      $info{$fund, "name"}     = $name[0];
      $info{$fund, "price"}    = $rows[0][3];
      $info{$fund, "last"}     = $rows[0][3];
      $info{$fund, "date"}     = $date;
      $info{$fund, "method"}   = "vwd";
      $info{$fund, "currency"} = $rows[0][2];
      $info{$fund, "success"}  = 1;
    }
    else
    {
      $info{$fund, "success"}  = 0;
      $info{$fund, "errormsg"} = "HTTP error";
    }
  }

  return wantarray() ? %info : \%info;
}

1;

=head1 NAME

Finance::Quote::vwd	- Obtain quotes from vwd Vereinigte Wirtschaftsdienste GmbH.

=head1 SYNOPSIS

    use Finance::Quote;

    $q = Finance::Quote->new;

    %stockinfo = $q->fetch("vwd","847402");

=head1 DESCRIPTION

This module obtains information from vwd Vereinigte Wirtschaftsdienste GmbH
http://www.vwd.de/. Many european stocks and funds are available, but
at the moment only funds are supported.

Information returned by this module is governed by vwd's terms
and conditions.

=head1 LABELS RETURNED

The following labels may be returned by Finance::Quote::vwd:
exchange, name, date, price, last.

=head1 SEE ALSO

vwd Vereinigte Wirtschaftsdienste GmbH, http://www.vwd.de/

=cut
