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

package Finance::Quote::Fidelity;
require 5.005;

use strict;
use vars qw/$FIDELITY_GANDI_URL $FIDELITY_GROWTH_URL $FIDELITY_CORPBOND_URL
            $FIDELITY_GLBND_URL $FIDELITY_MM_URL $FIDELITY_ASSET_URL
	    $VERSION/;

use LWP::UserAgent;
use HTTP::Request::Common;

$VERSION = '1.00';

$FIDELITY_GANDI_URL = ("http://personal441.fidelity.com/gen/prices/gandi.csv");
$FIDELITY_GROWTH_URL = ("http://personal441.fidelity.com/gen/prices/growth.csv");
$FIDELITY_CORPBOND_URL = ("http://personal441.fidelity.com/gen/prices/corpbond.csv");
$FIDELITY_GLBND_URL = ("http://personal441.fidelity.com/gen/prices/glbnd.csv");
$FIDELITY_MM_URL = ("http://personal441.fidelity.com/gen/prices/mm.csv");
$FIDELITY_ASSET_URL = ("http://personal441.fidelity.com/gen/prices/asset.csv");

sub methods {return (fidelity        => \&fidelity,
                     fidelity_direct => \&fidelity);}

sub labels { return (fidelity=>[qw/exchange name number nav change ask
                                   date yield price method/]); }

# =======================================================================
# the fidelity routine gets quotes from fidelity investments
#
sub fidelity
{
    my $quoter = shift;
    my @symbols = @_;
    return unless @symbols;
    my(%aa,%cc,$sym, $k);

    # Build a small hash of symbols people want, because it provides a
    # quick and easy way to only return desired symbols.

    my %symbolhash;
    @symbolhash{@symbols} = map(1,@symbols);

    # rather irritatingly, fidelity sorts its funds into different groups.
    # as a result, the fetch that we need to do depends on the group.
    my @gandi    = ("FBALX", "FCVSX", "FEQIX", "FEQTX", "FFIDX", "FGRIX",
                    "FIUIX", "FPURX", "FRESX", );
    my @growth   = ("FBGRX", "FCNTX", "FCONX", "FDCAX", "FDEGX", "FDEQX",
                    "FDFFX", "FDGFX", "FDGRX", "FDSCX", "FDSSX", "FDVLX",
                    "FEXPX", "FFTYX", "FLCSX", "FLPSX", "FMAGX", "FMCSX",
                    "FMILX", "FOCPX", "FSLCX", "FSLSX", "FTQGX", "FTRNX",
                    "FTXMX", );
    my @corpbond = ("FAGIX", "SPHIX", "FTHRX", "FBNDX", "FSHBX", "FSIBX", 
                    "FTBDX", "FSICX", "FTTAX", "FTTBX", "FTARX", );
    my @glbnd    = ("FGBDX", "FNMIX");
    my @mm       = ("FDRXX", "FDTXX", "FGMXX", "FRTXX", "SPRXX", "SPAXX",
                    "FDLXX", "FGRXX",);
    my @asset    = ("FASMX", "FASGX", "FASIX", );

    my (%agandi, %agrowth, %acorpbond, %aglbnd, %amm, %aasset);

    my $dgandi=0;
    my $dgrowth=0;
    my $dcorpbond=0;
    my $dglbnd=0;
    my $dmm=0;
    my $dasset=0;

    for (@gandi) { $agandi{$_} ++; }
    for (@growth) { $agrowth{$_} ++; }
    for (@corpbond) { $acorpbond{$_} ++; }
    for (@glbnd) { $aglbnd{$_} ++; }
    for (@mm) { $amm{$_} ++; }
    for (@asset) { $aasset{$_} ++; }
   
    # the fidelity pages are comma-separated-values (csv's)
    # there are two basic layouts, with, and without prices
    for (@symbols) {
       if ($agandi {$_} ) {
          if (0 == $dgandi ) {
             %cc = &_fidelity_nav ($quoter, $FIDELITY_GANDI_URL,\%symbolhash);
             $dgandi = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($agrowth {$_} ) {
          if (0 ==  $dgrowth ) {
             %cc = &_fidelity_nav ($quoter, $FIDELITY_GROWTH_URL,\%symbolhash);
             $dgrowth = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($acorpbond {$_} ) {
          if (0 ==  $dcorpbond ) {
             %cc = &_fidelity_nav ($quoter, $FIDELITY_CORPBOND_URL,\%symbolhash);
             $dcorpbond = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($aglbnd {$_} ) {
          if (0 ==  $dglbnd ) {
             %cc = &_fidelity_nav ($quoter, $FIDELITY_GLBND_URL,\%symbolhash);
             $dglbnd = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($amm {$_} ) {
          if (0 ==  $dmm ) {
             %cc = &_fidelity_mm ($quoter, $FIDELITY_MM_URL,\%symbolhash);
             $dmm = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($aasset {$_} ) {
          if (0 ==  $dasset ) {
             %cc = &_fidelity_nav ($quoter, $FIDELITY_ASSET_URL,\%symbolhash);
             $dasset = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
    }

    return %aa if wantarray;
    return \%aa;
}

# =======================================================================
# Private function used by fidelity.

sub _fidelity_nav
{
    my $quoter = shift;
    my $url = shift;
    my $symbolhash = shift;
    my(@q,%aa,$ua,$sym, $dayte);
    my %days = ('Monday','Mon','Tuesday','Tue','Wednesday','Wed',
                'Thursday','Thu','Friday','Fri','Saturday','Sat',
                'Sunday','Sun');

    # for Fidelity, we get them all. 
    $ua = $quoter->user_agent;
    my $reply = $ua->request(GET $url);
    return unless ($reply->is_success);
    foreach (split('\015?\012',$reply->content))
    {
        @q = $quoter->parse_csv($_) or next;

        $sym = $q[2] or next;
        $sym =~ s/^ +//;

	# Skip symbols we didn't ask for.
	next unless (defined($symbolhash->{$sym}));

        # extract the date which is usually on the second line fo the file.
        if (! defined ($dayte)) {
           if ($days {$q[0]} ) {          
              ($dayte = $q[1]) =~ s/^ +//;
           }
        }
        if ($q[7]) {
            $aa {$sym, "exchange"} = "Fidelity";  # Fidelity
            $aa {$sym, "method"} = "fidelity_direct";
            ($aa {$sym, "name"}   = $q[0]) =~ s/^ +//;
             $aa {$sym, "name"}   =~ s/$ +//;
            ($aa {$sym, "number"} = $q[1]) =~ s/^ +//;
            ($aa {$sym, "nav"}    = $q[3]) =~ s/^ +//;
            ($aa {$sym, "change"} = $q[4]) =~ s/^ +//;
            ($aa {$sym, "ask"}    = $q[7]) =~ s/^ +//;
             $aa {$sym, "date"} = $dayte;
	     $aa {$sym, "price"} = $aa{$sym, "nav"};
	     $aa {$sym, "success"} = 1;
             $aa {$sym, "currency"} = "USD";
	}
    }

    return %aa;
}

# =======================================================================
# Private function used by fidelity.

sub _fidelity_mm
{
    my $quoter = shift;
    my $url = shift;
    my $symbolhash = shift;

    my(@q,%aa,$ua,$sym, $dayte);
    my %days = ('Monday','Mon','Tuesday','Tue','Wednesday','Wed',
                'Thursday','Thu','Friday','Fri','Saturday','Sat',
                'Sunday','Sun');

    # for Fidelity, we get them all. 
    $ua = $quoter->user_agent;
    my $reply = $ua->request(GET $url);
    return unless ($reply->is_success);
    foreach (split('\015?\012',$reply->content))
    {
        @q = $quoter->parse_csv($_) or next;

        $sym = $q[2] or next;
        $sym =~ s/^ +//;

	# Skip symbols we didn't ask for.
	next unless (defined($symbolhash->{$sym}));

        # extract the date which is usually on the second line fo the file.
        if (! defined ($dayte)) {
           if ($days {$q[0]} ) {          
              ($dayte = $q[1]) =~ s/^ +//;
           }
        }

        if ($q[3]) {
            $sym =~ s/^ +//;
            $aa {$sym, "exchange"} = "Fidelity";  # Fidelity
            ($aa {$sym, "name"} = $q[0]) =~ s/^ +//;
             $aa {$sym, "name"} =~ s/$ +//;
            ($aa {$sym, "number"} = $q[1]) =~ s/^ +//;
            ($aa {$sym, "yield"}  = $q[3]) =~ s/^ +//;
             $aa {$sym, "date"} = $dayte;
	     $aa {$sym, "price"} = $aa{$sym, "yield"};
	     $aa {$sym, "success"} = 1;
             $aa {$sym, "currency"} = "USD";
	}
    }

    return %aa;
}

1;

=head1 NAME

Finance::Quote::Fidelity - Obtain information from Fidelity Investments.

=head1 SYNOPSIS

    use Finance::Quote;

    $q = Finance::Quote->new;

    %info = Finance::Quote->fetch("fidelity","FBGRX");
    %info = Finance::Quote->fetch("fidelity_direct","FBGRX");

=head1 DESCRIPTION

This module obtains information from Fidelity Investments,
http://www.fidelity.com/.  This module is loaded by default on
the Finance::Quote object.  It is also possible to load this
module explicitly by passing "Fidelity" as one of
Finance::Quote->new()'s parameters.

The "fidelity" fetch method may make use of failover modules.
The "fidelity_direct" method will only obtain information
directly from Fidelity.

Information returned by this module is governed by Fidelity
Investment's terms and conditions.

=head1 LABELS RETURNED

The following labels may be returned by Finance::Quote::Fidelity:
exchange, name, number, nav, change, ask, date, yield, price.

=head1 SEE ALSO

Fidelity Investments, http://www.fidelity.com/

Finance::Quote::Yahoo::USA;

=cut
