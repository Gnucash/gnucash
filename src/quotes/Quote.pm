#! /usr/bin/perl -w
#
#    Copyright (C) 1998, Dj Padzensky <djpadz@padz.net>
#    Copyright (C) 1998, 1999 Linas Vepstas <linas@linas.org>
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

# This code derived from Padzensky's work on package Finance::YahooQuote,
# but extends its capabilites to encompas a greater number of data sources.
#
# package Finance::YahooQuote;
package Quote;
require 5.000;

require Exporter;
use strict;
use vars qw($VERSION @EXPORT @ISA $YAHOO_URL $FIDELITY_GANDI_URL
            $FIDELITY_GROWTH_URL $FIDELITY_CORPBOND_URL $FIDELITY_GLBND_URL
            $FIDELITY_MM_URL $FIDELITY_ASSET_URL $TROWEPRICE_URL
            $VANGUARD_QUERY_URL $VANGUARD_CSV_URL @vanguard_ids);

use LWP::UserAgent;
use HTTP::Request::Common;

$VERSION = '0.08';
@ISA = qw(Exporter);

$YAHOO_URL = ("http://quote.yahoo.com/d?f=snl1d1t1c1p2va2bapomwerr1dyj1&s=");
$FIDELITY_GANDI_URL = ("http://personal441.fidelity.com/gen/prices/gandi.csv");
$FIDELITY_GROWTH_URL = ("http://personal441.fidelity.com/gen/prices/growth.csv");
$FIDELITY_CORPBOND_URL = ("http://personal441.fidelity.com/gen/prices/corpbond.csv");
$FIDELITY_GLBND_URL = ("http://personal441.fidelity.com/gen/prices/glbnd.csv");
$FIDELITY_MM_URL = ("http://personal441.fidelity.com/gen/prices/mm.csv");
$FIDELITY_ASSET_URL = ("http://personal441.fidelity.com/gen/prices/asset.csv");
$TROWEPRICE_URL = ("http://www.troweprice.com/funds/prices.csv");
$VANGUARD_QUERY_URL = ("http://www.vanguard.com/cgi-bin/Custom/daily/custom/CustRpt?");
$VANGUARD_CSV_URL = ("http://www.vanguard.com/cgi-bin/Custom?ACTION=Download&FileName=");

# Don't export; let user invoke with Quote::getquote syntax.
# @EXPORT = qw(&yahoo, &fidelity);

# =======================================================================
# yahoo gets quotes from the Yahoo service
# which is primarily the new york stock exchange.
sub yahoo {
    my @symbols = @_;
    my($x,@q,%aa,$ua,$url,$sym);
    $x = $";
    $" = "+";
    $url = $YAHOO_URL."@symbols";
    $" = $x;
    $ua = LWP::UserAgent->new;
    foreach (split('\n',$ua->request(GET $url)->content)) {
	@q = grep { s/^"?(.*?)\s*"?\s*$/$1/; } split(',');
        $sym = $q[0];
        $aa {$sym, "exchange"} = "NYSE";  # new  york stock exchange
        $aa {$sym, "name"} = $q[1];
        $aa {$sym, "last"} = $q[2];
        $aa {$sym, "date"} = $q[3];
        $aa {$sym, "time"} = $q[4];
        $aa {$sym, "volume"} = $q[7];
        $aa {$sym, "bid"} = $q[9];
        $aa {$sym, "ask"} = $q[10];
        $aa {$sym, "close"} = $q[11];
        $aa {$sym, "open"} = $q[12];
        $aa {$sym, "eps"} = $q[15];
        $aa {$sym, "pe"} = $q[16];
        $aa {$sym, "cap"} = $q[20];
    }
    # return wantarray() ? @qr : \@qr;
    return %aa;
}

# =======================================================================
# the fideility routine gets quotes from fidelity investments
#
sub fidelity {
    my @symbols = @_;
    my(%aa,%cc,$sym, $k);

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
             %cc = &fidelity_nav ($FIDELITY_GANDI_URL);
             $dgandi = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($agrowth {$_} ) {
          if (0 ==  $dgrowth ) {
             %cc = &fidelity_nav ($FIDELITY_GROWTH_URL);
             $dgrowth = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($acorpbond {$_} ) {
          if (0 ==  $dcorpbond ) {
             %cc = &fidelity_nav ($FIDELITY_CORPBOND_URL);
             $dcorpbond = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($aglbnd {$_} ) {
          if (0 ==  $dglbnd ) {
             %cc = &fidelity_nav ($FIDELITY_GLBND_URL);
             $dglbnd = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($amm {$_} ) {
          if (0 ==  $dmm ) {
             %cc = &fidelity_mm ($FIDELITY_MM_URL);
             $dmm = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($aasset {$_} ) {
          if (0 ==  $dasset ) {
             %cc = &fidelity_nav ($FIDELITY_ASSET_URL);
             $dasset = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
    }
    return %aa;
}

# =======================================================================

sub fidelity_nav {
    my(@q,%aa,$ua,$url,$sym, $dayte);
    my %days = ('Monday','Mon','Tuesday','Tue','Wednesday','Wed',
                'Thursday','Thu','Friday','Fri','Saturday','Sat','Sunday','Sun');

    # for Fidelity, we get them all. 
    $url = $_[0];
    $ua = LWP::UserAgent->new;
    foreach (split('\n',$ua->request(GET $url)->content)) {
	@q = grep { s/^"?(.*?)\s*"?\s*$/$1/; } split(',');

        # extract the date which is usually on the second line fo the file.
        if (! defined ($dayte)) {
           if ( $days {$q[0]} ) {          
              ($dayte = $q[1]) =~ s/^ +//;
           }
        }
        $sym = $q[2];
        if ($q[7]) {
            $sym =~ s/^ +//;
            $aa {$sym, "exchange"} = "Fidelity";  # Fidelity
            ($aa {$sym, "name"}   = $q[0]) =~ s/^ +//;
             $aa {$sym, "name"}   =~ s/$ +//;
            ($aa {$sym, "number"} = $q[1]) =~ s/^ +//;
            ($aa {$sym, "nav"}    = $q[3]) =~ s/^ +//;
            ($aa {$sym, "change"} = $q[4]) =~ s/^ +//;
            ($aa {$sym, "ask"}    = $q[7]) =~ s/^ +//;
            $aa {$sym, "date"} = $dayte;
        }
    }
    return %aa;
}

# =======================================================================

sub fidelity_mm {
    my(@q,%aa,$ua,$url,$sym, $dayte);
    my %days = ('Monday','Mon','Tuesday','Tue','Wednesday','Wed',
                'Thursday','Thu','Friday','Fri','Saturday','Sat','Sunday','Sun');

    # for Fidelity, we get them all. 
    $url = $_[0];
    $ua = LWP::UserAgent->new;
    foreach (split('\n',$ua->request(GET $url)->content)) {
	@q = grep { s/^"?(.*?)\s*"?\s*$/$1/; } split(',');

        # extract the date which is usually on the second line fo the file.
        if (! defined ($dayte)) {
           if ( $days {$q[0]} ) {          
              ($dayte = $q[1]) =~ s/^ +//;
           }
        }
        $sym = $q[2];
        if ($q[3]) {
            $sym =~ s/^ +//;
            $aa {$sym, "exchange"} = "Fidelity";  # Fidelity
            ($aa {$sym, "name"} = $q[0]) =~ s/^ +//;
             $aa {$sym, "name"} =~ s/$ +//;
            ($aa {$sym, "number"} = $q[1]) =~ s/^ +//;
            ($aa {$sym, "yield"}  = $q[3]) =~ s/^ +//;
            $aa {$sym, "date"} = $dayte;
        }
    }
    return %aa;
}

# =======================================================================

sub troweprice {
    my(@q,%aa,$ua,$url,$sym);

    # for T Rowe Price,  we get them all. 
    $url = $TROWEPRICE_URL;
    $ua = LWP::UserAgent->new;
    foreach (split('\n',$ua->request(GET $url)->content)) {
	@q = grep { s/^"?(.*?)\s*"?\s*$/$1/; } split(',');

        # extract the date which is usually on the second line fo the file.
        ($sym = $q[0]) =~ s/^ +//;
        if ($sym) {
            $aa {$sym, "exchange"} = "T. Rowe Price";  # TRP
            # ($aa {$sym, "name"} = $q[0]) =~ s/^ +//;
            $aa {$sym, "name"} = $sym;  # no name supplied ... 
            $aa {$sym, "nav"} = $q[1];
            $aa {$sym, "date"} = $q[2];
        }
    }
    return %aa;
}

# =======================================================================

sub vanguard {

    # The Vanguard Group doesn't use thier ticker symbols to look up funds.
    # but we do use the ticker symbols.  Therefore, we need to do a reverse lookup.
    # Load the array on first use only
    if (! @vanguard_ids ) {
        push (@vanguard_ids, ("0002", "Bal Index ", "VBINX"));
        push (@vanguard_ids, ("0006", "Value Idx ", "VIVAX"));
        push (@vanguard_ids, ("0009", "Growth Idx", "VIGRX"));
        push (@vanguard_ids, ("0011", "Admiral MM", "VUSXX"));
        push (@vanguard_ids, ("0012", "Admiral ST", "VASTX"));
        push (@vanguard_ids, ("0014", "NJ Ins LT ", "VNJTX"));
        push (@vanguard_ids, ("0018", "FL Ins LT ", "VFLTX"));
        push (@vanguard_ids, ("0019", "Admiral IT", "VAITX"));
        push (@vanguard_ids, ("0020", "Admiral LT", "VALGX"));
        push (@vanguard_ids, ("0021", "Wellington", "VWELX"));
        push (@vanguard_ids, ("0022", "Windsor   ", "VWNDX"));
        push (@vanguard_ids, ("0023", "US Growth ", "VWUSX"));
        push (@vanguard_ids, ("0024", "Explorer  ", "VEXPX"));
        push (@vanguard_ids, ("0026", "Morgan Gro", "VMRGX"));
        push (@vanguard_ids, ("0027", "Wellesley ", "VWINX"));
        push (@vanguard_ids, ("0028", "LT Corp   ", "VWESX"));
        push (@vanguard_ids, ("0029", "HiYld Corp", "VWEHX"));
        push (@vanguard_ids, ("0030", "Prime MM  ", "VMMXX"));
        push (@vanguard_ids, ("0031", "LdT Tax-Ex", "VMLTX"));
        push (@vanguard_ids, ("0032", "ST Treas  ", "VFISX"));
        push (@vanguard_ids, ("0033", "Federal MM", "VMFXX"));
        push (@vanguard_ids, ("0035", "IT Treas  ", "VFITX"));
        push (@vanguard_ids, ("0036", "GNMA Fund ", "VFIIX"));
        push (@vanguard_ids, ("0038", "Pref Stock", "VQIIX"));
        push (@vanguard_ids, ("0039", "ST Corp   ", "VFSTX"));
        push (@vanguard_ids, ("0040", "500 Index ", "VFINX"));
        push (@vanguard_ids, ("0041", "ST Tax-Ex ", "VWSTX"));
        push (@vanguard_ids, ("0042", "IT Tax-Ex ", "VWITX"));
        push (@vanguard_ids, ("0043", "LT Tax-Ex ", "VWLTX"));
        push (@vanguard_ids, ("0044", "HY Tax-Ex ", "VWAHX"));
        push (@vanguard_ids, ("0045", "Tax-Ex MM ", "VMSXX"));
        push (@vanguard_ids, ("0046", "Intl Value", "VTRIX"));
        push (@vanguard_ids, ("0048", "Sm-Cap Idx", "NAESX"));
        push (@vanguard_ids, ("0049", "ST Federal", "VSGBX"));
        push (@vanguard_ids, ("0050", "Treas MM  ", "VMPXX"));
        push (@vanguard_ids, ("0051", "Energy    ", "VGENX"));
        push (@vanguard_ids, ("0052", "Hlth Care ", "VGHCX"));
        push (@vanguard_ids, ("0053", "Gold&Prec ", "VGPMX"));
        push (@vanguard_ids, ("0057", "Util Inc  ", "VGSUX"));
        push (@vanguard_ids, ("0058", "Ins LT TE ", "VILPX"));
        push (@vanguard_ids, ("0059", "PRIMECAP  ", "VPMCX"));
        push (@vanguard_ids, ("0062", "CA TE MM  ", "VCTXX"));
        push (@vanguard_ids, ("0063", "PA TE MM  ", "VPTXX"));
        push (@vanguard_ids, ("0065", "Equity Inc", "VEIPX"));
        push (@vanguard_ids, ("0066", "Prime Ist ", "VMRXX"));
        push (@vanguard_ids, ("0071", "IT Corp   ", "VFICX"));
        push (@vanguard_ids, ("0072", "Pacif Idx ", "VPACX"));
        push (@vanguard_ids, ("0073", "Windsor II", "VWNFX"));
        push (@vanguard_ids, ("0075", "CA Ins LT ", "VCITX"));
        push (@vanguard_ids, ("0076", "NY Ins LT ", "VNYTX"));
        push (@vanguard_ids, ("0077", "PA Ins LT ", "VPAIX"));
        push (@vanguard_ids, ("0078", "Asset Allo", "VAAPX"));
        push (@vanguard_ids, ("0079", "Euro Index", "VEURX"));
        push (@vanguard_ids, ("0081", "Int Growth", "VWIGX"));
        push (@vanguard_ids, ("0082", "Conv Secur", "VCVSX"));
        push (@vanguard_ids, ("0083", "LT Treas  ", "VUSTX"));
        push (@vanguard_ids, ("0084", "Ttl Bnd Ix", "VBMFX"));
        push (@vanguard_ids, ("0085", "Ttl Stk Ix", "VTSMX"));
        push (@vanguard_ids, ("0093", "Grow & Inc", "VQNPX"));
        push (@vanguard_ids, ("0094", "Inst Index", "VINIX"));
        push (@vanguard_ids, ("0095", "NJ TE MM  ", "VNJXX"));
        push (@vanguard_ids, ("0096", "OH TE MM  ", "VOHXX"));
        push (@vanguard_ids, ("0097", "OH Ins LT ", "VOHIX"));
        push (@vanguard_ids, ("0098", "Ext Mkt Ix", "VEXMX"));
        push (@vanguard_ids, ("0100", "CA Ins IT ", "VCAIX"));
        push (@vanguard_ids, ("0101", "TM Gro&Inc", "VTGIX"));
        push (@vanguard_ids, ("0102", "TM Cap App", "VMCAX"));
        push (@vanguard_ids, ("0103", "TM Bal    ", "VTMFX"));
        push (@vanguard_ids, ("0111", "Cap Oppor ", "VHCOX"));
        push (@vanguard_ids, ("0113", "Ttl Int Ix", "VGTSX"));
        push (@vanguard_ids, ("0114", "Aggr Grow ", "VHAGX"));
        push (@vanguard_ids, ("0115", "Glo Ast Al", "VHAAX"));
        push (@vanguard_ids, ("0122", "LifeSt Gro", "VASGX"));
        push (@vanguard_ids, ("0123", "REIT Index", "VGSIX"));
        push (@vanguard_ids, ("0129", "Global Equ", "VHGEX"));
        push (@vanguard_ids, ("0132", "ST Bnd Idx", "VBISX"));
        push (@vanguard_ids, ("0163", "NY TE MM  ", "VYFXX"));
        push (@vanguard_ids, ("0222", "Ttl Bd Ist", "VBTIX"));
        push (@vanguard_ids, ("0314", "IT Bnd Idx", "VBIIX"));
        push (@vanguard_ids, ("0522", "LT Bnd Idx", "VBLTX"));
        push (@vanguard_ids, ("0533", "Emerg Mkts", "VEIEX"));
        push (@vanguard_ids, ("0723", "LifeSt Inc", "VASIX"));
        push (@vanguard_ids, ("0724", "LifeSt Con", "VSCGX"));
        push (@vanguard_ids, ("0854", "Ins Idx Pl", "VIIIX"));
        push (@vanguard_ids, ("0855", "Ttl Ix Ist", "VITSX"));
        push (@vanguard_ids, ("0856", "Ex Idx Ist", "VIEIX"));
        push (@vanguard_ids, ("0857", "SC Idx Ist", "VSCIX"));
        push (@vanguard_ids, ("0858", "ST Crp Ist", "VFSIX"));
        push (@vanguard_ids, ("0859", "Md-Cap Idx", "VIMSX"));
        push (@vanguard_ids, ("0860", "SC Val Idx", "VISVX"));
        push (@vanguard_ids, ("0861", "SC Gro Idx", "VISGX"));
        push (@vanguard_ids, ("0864", "MC Idx Ist", "VMCIX"));
        push (@vanguard_ids, ("0867", "Val Ix Ist", "VIVIX"));
        push (@vanguard_ids, ("0868", "Gro Ix Ist", "VIGIX"));
        push (@vanguard_ids, ("0914", "LifeSt Mod", "VSMGX"));
        push (@vanguard_ids, ("0934", "Select Val", "VASVX"));
    }

    my @symbols = @_;
    my($url, $sym, $i, $fid, $reply, $ua, @q, %aa);

    # convert ticker symbols into fund numbers; build first url
    $url = $VANGUARD_QUERY_URL;
    foreach $sym (@symbols) {
       undef ($fid);
       for ($i=2; $i<=$#vanguard_ids; $i+=3) {
          if ($vanguard_ids[$i] =~ $sym) {
             $fid = $vanguard_ids[$i-2];
             last;
          }
       }
       if ($fid) {
          $url .= "ROWS=".$fid."&";  
          # print "found $fid $url\n";
       }
    }
    if ($url eq  $VANGUARD_QUERY_URL) { return undef; }

    $url .="COLS=COL1%2C3&COLS=COL4&COLS=COL5&COLS=COL11%2C12&ACTION=Accept";
    $ua = LWP::UserAgent->new;
    $reply = $ua->request(GET $url)->content;

    undef $url;
    foreach (split('\n',$reply)) {
       if (/FileName=V(.*)\.txt/) {
           $url = $VANGUARD_CSV_URL . "V" . $1 . ".txt";
           last;
       }
    }
    print "second url is $url\n";
    $reply = $ua->request(GET $url)->content;

    foreach (split('\n',$reply)) {
       @q = split (/,/);
       ($sym = $q[0]) =~ s/\W//;
       $aa {$sym, "exchange"} = "Vanguard";  # Fidelity
       if (! $q[1]) last;     # error if comma sepoaration didn't work
       ($aa {$sym, "nav"} = $q[1]) =~ s/\S//;
       ($aa {$sym, "date"} = $q[2]) =~ s/\S//;
    }
    return %aa;
}

# =======================================================================

__END__

1;

=head1 NAME

Finance::Quote - Get stock and mutual fund quotes from various exchanges

=head1 SYNOPSIS

  use Finance::Quote;
  %quotes = Quote::yahoo @symbols;	# Get NYSE quotes from Yahoo
  %quotes = Quote::fidelity @symbols;	# Get quotes from Fidelity Investments
  %quotes = Quote::troweprice @symbols;	# Get quotes from T. Rowe Price
  %quotes = Quote::vanguard @symbols;	# Get quotes from the Vanguard Group
  print ("the last price was ", $quotes {"IBM", "last"} );

=head1 DESCRIPTION

This module gets stock quotes from various internet sources, including 
Yahoo! Finance and Fidelity Investments.  The
B<quote_yahoo> function will return a quote for each of the stock symbols
passed to it.  The return value of each of the routines is an associative
array, which may include one or more of the following elements:

    name         Company or Mutual Fund Name
    last         Last Price
    date         Last Trade Date  (MM/DD/YY format)
    time         Last Trade Time
                 Change
                 Percent Change
    volume       Volume
                 Average Daily Vol
    bid          Bid
    ask          Ask
    close        Previous Close
    open         Today's Open
                 Day's Range
                 52-Week Range
    eps          Earnings per Share
    pe           P/E Ratio
                 Dividend Pay Date
                 Dividend per Share
                 Dividend Yield
    cap          Market Capitalization
    nav          Net Asset Value
    yeild        Yeild (usually 30 day avg)

=head1 COPYRIGHT

Copyright 1998, Dj Padzensky
Copyright 1998, 1999 Linas Vepstas

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
Linas Vepstas (C<linas@linas.org>)

The Finance::YahooQuote home page can be found at
http://www.padz.net/~djpadz/YahooQuote/
The GnuCash home page can be found at
http://www.gnucash.org/

=cut
