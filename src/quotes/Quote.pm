#!/usr/bin/perl -w
#
#    Copyright (C) 1998, Dj Padzensky <djpadz@padz.net>
#    Copyright (C) 1998, 1999 Linas Vepstas <linas@linas.org>
#    Copyright (C) 2000, Yannick LE NY <y-le-ny@ifrance.com>
#    Copyright (C) 2000, Paul Fenwick <pjf@schools.net.au>
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
#
#
# ==========================================================================
#
# NOTE:		This is a special GnuCash release of Finance::Quote,
#		and operates slightly differently to the 0.18
#		release of Finance::Quote.  The main two changes
#		are that the package is called "Quote" and not
#		Finance::Quote, and that all functions now implement
#		a "price" label in the hash they return.
#
#		For more information about Finance::Quote, visit:
#		http://finance-quote.sourceforge.net/
#
# ==========================================================================
#

# package Finance::Quote;
package Quote;
require 5.004;

use strict;

# These should really be moved to near their corresponding functions,
# and possibly wrapped into lexical my's around the functions similar
# to what's been done with fetch.
#
# Of course, it would also make sense to move a lot of these methods
# into their own seperate sub-modules.  Much cleaner and nicer that
# way.

use vars qw($VERSION @EXPORT @ISA $TIMEOUT @EXPORT_OK @EXPORT_TAGS
            $YAHOO_URL $YAHOO_EUROPE_URL
            $FIDELITY_GANDI_URL $FIDELITY_GROWTH_URL $FIDELITY_CORPBOND_URL
            $FIDELITY_GLBND_URL $FIDELITY_MM_URL $FIDELITY_ASSET_URL
            $TROWEPRICE_URL $YAHOO_CURRENCY_URL
            $VANGUARD_QUERY_URL $VANGUARD_CSV_URL @vanguard_ids
            $ASX_URL $TIAACREF_URL %tiaacref_ids);

use LWP::UserAgent;
use HTTP::Request::Common;
use Carp;
use Exporter ();

# Export information.  Allow lots of things to be exported, but not
# by default.
@ISA = qw/Exporter/;
@EXPORT      = ();
@EXPORT_OK   = qw/yahoo yahoo_europe fidelity troweprice asx tiaacref fetch/;
@EXPORT_TAGS = ( all => [@EXPORT_OK] );

$VERSION = '0.18';

# URLs of where to obtain information.

$YAHOO_URL = ("http://quote.yahoo.com/d?f=snl1d1t1c1p2va2bapomwerr1dyj1q&s=");
$YAHOO_EUROPE_URL = ("http://finance.fr.yahoo.com/d/quotes.csv?f=snl1d1t1c1p2va2bapomwerr1dyj1&s=");
$YAHOO_CURRENCY_URL = ("http://finance.yahoo.com/m5?");
$FIDELITY_GANDI_URL = ("http://personal441.fidelity.com/gen/prices/gandi.csv");
$FIDELITY_GROWTH_URL = ("http://personal441.fidelity.com/gen/prices/growth.csv");
$FIDELITY_CORPBOND_URL = ("http://personal441.fidelity.com/gen/prices/corpbond.csv");
$FIDELITY_GLBND_URL = ("http://personal441.fidelity.com/gen/prices/glbnd.csv");
$FIDELITY_MM_URL = ("http://personal441.fidelity.com/gen/prices/mm.csv");
$FIDELITY_ASSET_URL = ("http://personal441.fidelity.com/gen/prices/asset.csv");
$TROWEPRICE_URL = ("http://www.troweprice.com/funds/prices.csv");
$VANGUARD_QUERY_URL = ("http://www.vanguard.com/cgi-bin/Custom/daily/custom/CustRpt?");
$VANGUARD_CSV_URL = ("http://www.vanguard.com/cgi-bin/Custom?ACTION=Download&FileName=");
$ASX_URL = ('http://www3.asx.com.au/nd50/nd_isapi_50.dll/JSP/EquitySearchResults.jsp?method=post&template=F1001&ASXCodes=');
$TIAACREF_URL = ("http://www.tiaa-cref.org/financials/selection/ann-select.cgi?");

undef $TIMEOUT;

# =======================================================================
# Define some OO methods for use.  People probably don't want to type
# Finance::Quote::foo all the time.  This is just a dummy object right
# now.
sub new {
	my $self = shift;
	my $class = ref($self) || $self;
	return bless {}, $class;
}

# Timeout changing code.  Currently this changes the timeout for everything,
# but in the future can be used to change the timeout for a particular
# Quote object.
sub timeout {
	my $self = shift;
	my $timeout = shift;
	$timeout = $self unless (ref $self);
	$TIMEOUT = $timeout;
}

# =======================================================================
# Fetch is a wonderful generic fetcher.  It takes a method and stuff to
# fetch.  It's a nicer interface for when you have a list of stocks with
# different sources which you wish to deal with.
{
	# Private hash used by fetch.  We don't place it inside the
	# fetch function because then it gets rebuilt every time we 
	# run fetch.  We don't place it at the top-level because that
	# means other things that shouldn't care can play with it.
	my %methods = ( asx   => \&asx,
			fidelity => \&fidelity,
			tiaacref => \&tiaacref,
			troweprice => \&troweprice,
			yahoo => \&yahoo,
			nasdaq => \&yahoo,
			nyse => \&yahoo,
			yahoo_europe => \&yahoo_europe,
			europe => \&yahoo_europe,
			vanguard => \&yahoo );

	sub fetch {
		shift if ref ($_[0]);	# Shift off the object if there is one.
		my $method = lc(shift);
		my @stocks = @_;

		unless (exists $methods{$method}) {
			carp "Undefined fetch-method $method passed to ".
			     "Finance::Quote::fetch";
			return undef;
		}
		return &{$methods{$method}}(@stocks);
	}
}


# =======================================================================
# Grabbed from the Perl Cookbook. Parsing csv isn't as simple as you thought!
#
sub _parse_csv
{
    my $text = shift;      # record containing comma-separated values
    my @new  = ();

    push(@new, $+) while $text =~ m{
        # the first part groups the phrase inside the quotes.
        # see explanation of this pattern in MRE
        "([^\"\\]*(?:\\.[^\"\\]*)*)",?
           |  ([^,]+),?
           | ,
       }gx;
       push(@new, undef) if substr($text, -1,1) eq ',';

       return @new;      # list of values that were comma-separated
}  

# =======================================================================
# yahoo gets quotes from the Yahoo service
# which is primarily the new york stock exchange.
sub yahoo
{
    shift if (ref $_[0]);	# Shift off the object if there is one.
    my @symbols = @_;
    return undef unless @symbols;	# Nothing if no symbols.
    my($x,@q,%aa,$ua,$url,$sym);

    $x = $";
    $" = "+";
    $url = $YAHOO_URL."@symbols";
    $" = $x;
    $ua = LWP::UserAgent->new;
    $ua->timeout($TIMEOUT) if defined $TIMEOUT;
    $ua->env_proxy();

    my $reply = $ua->request(GET $url);
    return undef unless ($reply->is_success);
    foreach (split('\015?\012',$reply->content))
    {
      @q = _parse_csv($_);

      $sym = $q[0];
      $aa {$sym, "name"} = $q[1];
      $aa {$sym, "last"} = $q[2];
      $aa {$sym, "date"} = $q[3];
      $aa {$sym, "time"} = $q[4];
      $aa {$sym, "net"}  = $q[5];
      $aa {$sym, "p_change"} = $q[6];
      $aa {$sym, "volume"} = $q[7];
      $aa {$sym, "avg_vol"} = $q[8];
      $aa {$sym, "bid"} = $q[9];
      $aa {$sym, "ask"} = $q[10];
      $aa {$sym, "close"} = $q[11];
      $aa {$sym, "open"} = $q[12];
      $aa {$sym, "day_range"} = $q[13];
      $aa {$sym, "year_range"} = $q[14];
      $aa {$sym, "eps"} = $q[15];
      $aa {$sym, "pe"} = $q[16];
      $aa {$sym, "div_date"} = $q[17];
      $aa {$sym, "div"} = $q[18];
      $aa {$sym, "div_yield"} = $q[19];
      $aa {$sym, "cap"} = $q[20];
      $aa {$sym, "ex_div"} = $q[21];
      $aa {$sym, "price"} = $aa{$sym,"last"};

      # Yahoo returns a line filled with N/A's if we look up a
      # non-existant symbol.  AFAIK, the date flag will /never/
      # be defined properly unless we've looked up a real stock.
      # Hence we can use this to check if we've successfully
      # obtained the stock or not.

      if ($aa{$sym,"date"} eq "N/A") {
        $aa{$sym,"success"}  = 0;
        $aa{$sym,"errormsg"} = "Stock lookup failed";
      } else {
        $aa{$sym,"success"} = 1;
      }

      if ($q[13] =~ m{^"?\s*(\S+)\s*-\s*(\S+)"?$}) {
        $aa {$sym, "low"} = $1;
        $aa {$sym, "high"} = $2;
      }
    }

    # Return undef's rather than N/As.  This makes things more suitable
    # for insertion into databases, etc.
    foreach my $key (keys %aa) {
      undef $aa{$key} if (defined($aa{$key}) and $aa{$key} eq "N/A");
    }

    return %aa;
}

# =======================================================================
# yahoo_europe gets quotes for European markets from Yahoo.
sub yahoo_europe
{
    shift if (ref $_[0]);	# Shift off the object if there is one.
    my @symbols = @_;
    return undef unless @symbols;	# Nothing if no symbols.
    my($x,@q,%aa,$ua,$url,$sym);

    $x = $";
    $" = "+";
    $url = $YAHOO_EUROPE_URL."@symbols";
    $" = $x;
    $ua = LWP::UserAgent->new;
    $ua->timeout($TIMEOUT) if defined $TIMEOUT;
    $ua->env_proxy();
    my $reply = $ua->request(GET $url);
    return undef unless ($reply->is_success);
    foreach (split('\015?\012',$reply->content))
    {
      @q = _parse_csv($_);

      $sym = $q[0];
      # $aa {$sym, "exchange"} = "NYSE";
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
      $aa {$sym, "price"} = $aa{$sym,"last"};

      # Yahoo returns a line filled with N/A's if we look up a
      # non-existant symbol.  AFAIK, the date flag will /never/
      # be defined properly unless we've looked up a real stock.
      # Hence we can use this to check if we've successfully
      # obtained the stock or not.
      if ($aa{$sym,"date"} eq "N/A") {
        $aa{$sym, "success"} = 0;
	$aa{$sym, "errormsg"} = "Stock lookup failed.";
      } else {
        $aa{$sym, "success"} = 1;
      }
    }

    # Return undef's rather than N/As.  This makes things more suitable
    # for insertion into databases, etc.  Also remove silly HTML that
    # yahoo inserts to put in little euro symbols.
    foreach my $key (keys %aa) {
      $aa{$key} =~ s/<[^>]*>//g;
      undef $aa{$key} if (defined($aa{$key}) and $aa{$key} eq "N/A");
    }

    # return wantarray() ? @qr : \@qr;
    return %aa;
}

# =======================================================================
# the fidelity routine gets quotes from fidelity investments
#
sub fidelity
{
    shift if (ref $_[0]);	# Shift off the object if there is one.
    my @symbols = @_;
    return undef unless @symbols;
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
             %cc = &_fidelity_nav ($FIDELITY_GANDI_URL);
             $dgandi = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($agrowth {$_} ) {
          if (0 ==  $dgrowth ) {
             %cc = &_fidelity_nav ($FIDELITY_GROWTH_URL);
             $dgrowth = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($acorpbond {$_} ) {
          if (0 ==  $dcorpbond ) {
             %cc = &_fidelity_nav ($FIDELITY_CORPBOND_URL);
             $dcorpbond = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($aglbnd {$_} ) {
          if (0 ==  $dglbnd ) {
             %cc = &_fidelity_nav ($FIDELITY_GLBND_URL);
             $dglbnd = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($amm {$_} ) {
          if (0 ==  $dmm ) {
             %cc = &_fidelity_mm ($FIDELITY_MM_URL);
             $dmm = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
       if ($aasset {$_} ) {
          if (0 ==  $dasset ) {
             %cc = &_fidelity_nav ($FIDELITY_ASSET_URL);
             $dasset = 1;
             foreach $k (keys %cc) { $aa{$k} = $cc{$k}; }
          }
       }
    }

    return %aa;
}

# =======================================================================
# Private function used by fidelity.

sub _fidelity_nav
{
    shift if (ref $_[0]);	# Shift off the object if there is one.
    my(@q,%aa,$ua,$url,$sym, $dayte);
    my %days = ('Monday','Mon','Tuesday','Tue','Wednesday','Wed',
                'Thursday','Thu','Friday','Fri','Saturday','Sat',
                'Sunday','Sun');

    # for Fidelity, we get them all. 
    $url = $_[0];
    $ua = LWP::UserAgent->new;
    $ua->timeout($TIMEOUT) if defined $TIMEOUT;
    $ua->env_proxy();
    my $reply = $ua->request(GET $url);
    return undef unless ($reply->is_success);
    foreach (split('\015?\012',$reply->content))
    {
        @q = _parse_csv($_) or next;

        # extract the date which is usually on the second line fo the file.
        if (! defined ($dayte)) {
           if ($days {$q[0]} ) {          
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
	     $aa {$sym, "price"} = $aa {$sym, "nav"};
	     $aa {$sym, "success"} = 1;
	}
    }

    return %aa;
}

# =======================================================================
# Private function used by fidelity.

sub _fidelity_mm
{
    shift if (ref $_[0]);	# Shift off the object if there is one.
    my(@q,%aa,$ua,$url,$sym, $dayte);
    my %days = ('Monday','Mon','Tuesday','Tue','Wednesday','Wed',
                'Thursday','Thu','Friday','Fri','Saturday','Sat',
                'Sunday','Sun');

    # for Fidelity, we get them all. 
    $url = $_[0];
    $ua = LWP::UserAgent->new;
    $ua->timeout($TIMEOUT) if defined $TIMEOUT;
    $ua->env_proxy();
    my $reply = $ua->request(GET $url);
    return undef unless ($reply->is_success);
    foreach (split('\015?\012',$reply->content))
    {
        @q = _parse_csv($_) or next;

        # extract the date which is usually on the second line fo the file.
        if (! defined ($dayte)) {
           if ($days {$q[0]} ) {          
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
	     # Yield doesn't seem to make a great deal of sense
	     # for the "price" field, but it's the best there is.
	     $aa {$sym, "price"} = $aa {$sym, "yield"};
	     $aa {$sym, "success"} = 1;
	}
    }

    return %aa;
}

# =======================================================================

sub troweprice
{
    shift if (ref $_[0]);	# Shift off the object if there is one.
    my(@q,%aa,$ua,$url,$sym);

    # for T Rowe Price,  we get them all. 
    $url = $TROWEPRICE_URL;
    $ua = LWP::UserAgent->new;
    $ua->timeout($TIMEOUT) if defined $TIMEOUT;
    $ua->env_proxy();
    my $reply = $ua->request(GET $url);
    return undef unless ($reply->is_success);
    foreach (split('\015?\012',$reply->content))
    {
        @q = _parse_csv($_);

        # extract the date which is usually on the second line fo the file.
        ($sym = $q[0]) =~ s/^ +//;
        if ($sym) {
            $aa {$sym, "exchange"} = "T. Rowe Price";  # TRP
            # ($aa {$sym, "name"} = $q[0]) =~ s/^ +//;
            $aa {$sym, "name"} = $sym;  # no name supplied ... 
            $aa {$sym, "nav"} = $q[1];
            $aa {$sym, "date"} = $q[2];
	    $aa {$sym, "price"} = $aa {$sym, "nav"};
	    $aa {$sym, "success"} = 1;
        } else {
	    $aa {$sym, "success"} = 0;
	    $aa {$sym, "errormsg"} = "Stock lookup failed.";
	}
    }

    return %aa;
}

# =======================================================================
# The Vanguard function is depreciated and no longer works due to a
# re-structure of the Vanguard website.  We hope to fix this in the
# future.
sub vanguard
{
    shift if (ref $_[0]);	# Shift off the object if there is one.
    # The Vanguard Group doesn't use their ticker symbols to look up funds.
    # but we do use the ticker symbols. Therefore, we need to do a reverse
    # lookup. Load the array on first use only
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
    return undef unless @symbols;
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

    # if no symbols matched, return empty-handed.
    if ($url eq  $VANGUARD_QUERY_URL) { return undef; }

    # ask for a special report with these symbols in them
    $url .="COLS=COL1%2C3&COLS=COL4&COLS=COL5&COLS=COL11%2C12&ACTION=Accept";
    $ua = LWP::UserAgent->new;
    $ua->timeout($TIMEOUT) if defined $TIMEOUT;
    $ua->env_proxy();
    $reply = $ua->request(GET $url)->content;

    # now build the second url which will actually contain the data
    undef $url;
    foreach (split('\015?\012',$reply)) {
       if (/FileName=V(.*)\.txt/) {
           $url = $VANGUARD_CSV_URL . "V" . $1 . ".txt";
           last;
       }
    }
    # print "second url is $url\n";
    $reply = $ua->request(GET $url)->content;

    # parse the data, stick it into the array the user will get
    foreach (split('\015?\012',$reply)) {
       @q = split (/,/);
       ($sym = $q[0]) =~ s/\W//g;
       $aa {$sym, "exchange"} = "Vanguard";  # Vanguard

       # check for error message it commas are missing
       # (unortunately very common for Vanguard)
       if (! $q[1]) { print "Error: $_\n"; last; }
       ($aa {$sym, "nav"} = $q[1]) =~ s/\s//g;
       ($aa {$sym, "date"} = $q[2]) =~ s/\s//g;

       # look up a name for the ticker symbol
       undef ($fid);
       for ($i=2; $i<=$#vanguard_ids; $i+=3) {
          if ($vanguard_ids[$i] =~ $sym) {
             $fid = $vanguard_ids[$i-1];
             last;
          }
       }
       if ($fid) {
          $aa {$sym, "name"} = $fid;
       }
    }

    return %aa;
}

# =======================================================================

# Australian Stock Exchange (ASX)
# The ASX provides free delayed quotes through their webpage.
#
# Maintainer of this section is Paul Fenwick <pjf@schools.net.au>
#
# TODO: It's possible to fetch multiple stocks in one operation.  It would
#       be nice to do this, and should not be hard.
sub asx {
    shift if (ref $_[0]);	# Shift off the object if there is one.
    my @stocks = @_;
    return undef unless @stocks;
    my %info;

    my $ua = LWP::UserAgent->new;
    $ua->timeout($TIMEOUT) if defined $TIMEOUT;
    $ua->env_proxy();

    foreach my $stock (@stocks) {
        my $response = $ua->request(GET $ASX_URL.$stock);
	unless ($response->is_success) {
	    $info{$stock,"success"} = 0;
	    $info{$stock,"errormsg"} = "HTTP session failed";
	    next;
	}
	my $reply = $response->content;

	# Grab the date.  This is a pretty clunky way of doing it, but
	# my mind's still in brain-saver mode.

	my ($day, $month, $year) = $reply =~ /(\d\d?) (January|February|March|April|May|June|July|August|September|October|November|December) (\d{4})/;

	unless ($month) {
	    $info{$stock,"sucess"} = 0;
	    $info{$stock,"errormsg"} = "Symbol Lookup failed";
	    next;
	}

	$_ = $month;
	(s/January/1/    or
	 s/February/2/   or
	 s/March/3/      or
	 s/April/4/      or
	 s/May/5/        or
	 s/June/6/       or
	 s/July/7/       or
	 s/August/8/     or
	 s/September/9/  or
	 s/October/10/   or
	 s/November/11/ or
	 s/December/12/  or (warn "Bizarre month $_ from ASX. Skipped $stock\n"
	                          and return undef));

	$info{$stock,"date"} = "$_/$day/$year"; # Silly 'merkin format.

	# These first two steps aren't really needed, but are done for
	# safety.
	# Remove the bottom part of the page.
	$reply =~ s#</table>\s*\n<table>.*$##s;
	# Remove top of page.
	$reply =~ s#.*<table##s;

        # Now pluck out the headings.
	my @headings;
	while ($reply =~ m#<FONT +SIZE=2><B>([%\w ]*).*?</B>#g) {
	    push @headings, $1;
	}

	# Now grab the values
	my @values;
	while ($reply =~ m#<td align=(left|right)><Font Size=2>(.*?)</Font>#g) {
	    push @values, $2;
	}

	# Put the two together and we get shares information.
	foreach my $heading (@headings) {
	    my $value = shift @values;

	    # Check the code that we got back.
	    if ($heading =~ /ASX CODE/) {
		if ($value ne $stock) {
		    # Oops!  We got back a stock that we didn't want?
		    warn "Bad stocks returned from the ASX.  ".
			 "Wanted $stock but got $value.";
		    return undef;
		}
		next;
	    }

	    # Convert ASX headings to labels we want to return.
	    $_ = $heading;
	    (s/LAST/last/)  or
	    (s/BID/bid/)    or
	    (s/OFFER/ask/)  or
	    (s/OPEN/open/)  or
	    (s/HIGH/high/)  or
	    (s/LOW/low/)    or
	    (s/LAST/last/)  or
	    (s/PDC/close/)  or
	    (s/%/p_change/) or
	    (s/VOLUME/volume/) or (warn "Unknown heading from ASX: $_.  Skipped"
	                           and next);

	    # Clean the value
	    $value =~ tr/$,%//d;

	    # If the value if nbsp then skip it.  Some things are not
	    # defined outside trading hours.

            next if $value =~ /&nbsp;/;

	    # Put the info into our hash.
	    $info{$stock,$_} = $value;
	}
	$info{$stock,"name"} = $stock;	# ASX doesn't give names.  :(

	# Outside of business hours, the last price is the same as the
	# previous day's close.
	$info{$stock,"last"} ||= $info{$stock,"close"};
        $info{$stock, "price"} = $info{$stock,"last"};
	$info{$stock,"success"} = 1;
    }
    return %info;
}
# =======================================================================
# TIAA-CREF Annuities are not listed on any exchange, unlike their mutual funds
# TIAA-CREF provides unit values via a cgi on their website. The cgi returns
# a csv file in the format 
#		bogus_symbol1,price1,date1
#		bogus_symbol2,price2,date2
#       ..etc.
# where bogus_symbol takes on the following values for the various annuities:
#
#Stock: 			CREFstok
#Money Market:			CREFmony
#Equity Index:			CREFequi
#Inf-Linked Bond:		CREFinfb
#Bond Market:			CREFbond
#Social Choice:			CREFsoci
#Global Equities:		CREFglob
#Growth:			CREFgrow
#TIAA Real Estate:		TIAAreal
#PA Stock Index:		TIAAsndx
#PA Select Stock:		TIAAsele

#
# This subroutine was written by Brent Neal <brent@phys.lsu.edu>
#
# TODO:
#
# The TIAA-CREF cgi allows you to specify the exact dates for which to retrieve
# price data. That functionality could be worked into this subroutine.
# Currently, we only grab the most recent price data.
# 

sub tiaacref
{
    shift if (ref $_[0]);	# Shift off the object if there is one.
    if (! %tiaacref_ids ) {  #build a name hash for the annuities (once only)
    	$tiaacref_ids{"CREFstok"} = "CREF Stock";
    	$tiaacref_ids{"CREFmony"} = "CREF Money Market";
    	$tiaacref_ids{"CREFequi"} = "CREF Equity Index";
    	$tiaacref_ids{"CREFinfb"} = "CREF Inflation-Linked Bond";
    	$tiaacref_ids{"CREFbond"} = "CREF Bond Market";
    	$tiaacref_ids{"CREFsoci"} = "CREF Social Choice";
    	$tiaacref_ids{"CREFglob"} = "CREF Global Equities";
    	$tiaacref_ids{"CREFgrow"} = "CREF Growth";
    	$tiaacref_ids{"TIAAreal"} = "TIAA Real Estate";
    	$tiaacref_ids{"TIAAsndx"} = "TIAA Teachers Personal Annuity Stock Index";
    	$tiaacref_ids{"TIAAsele"} = "TIAA Teachers Personal Annuity Select Stock"; 
    }
    my(@funds) = @_;
    return undef unless @funds;
    my(@line);		#holds the return from _parse_csv
    my(%info);
    my(%check);		#holds success value if data returned	
    my($ua,$url);   #useragent and target url
    my($reply);		#the reply from TIAA-CREF's cgi

    $url = $TIAACREF_URL;
    foreach my $fund (@funds) {
	if ($tiaacref_ids{$fund}) {
		$url .=  $fund . "=yes&";
		$check{$fund} = 0;
	} else {
		$info{$fund,"success"} = 0;
		$info{$fund,"errormsg"} = "Bad symbol";
	}
    }
    $url .=  "selected=1";

    $ua = LWP::UserAgent->new;
    $ua->timeout($TIMEOUT) if defined $TIMEOUT;
    $ua->env_proxy();
    $reply = $ua->request(GET $url);
    if ($reply ->is_success) {

       foreach (split('\012',$reply->content) ){
           @line = _parse_csv($_);
           if (exists $check{$line[0]}) {   #did we ask for this data?
		  $info{$line[0],"symbol"} = $line[0]; #in case the caller needs this in the hash
         	  $info{$line[0],"exchange"} = "TIAA-CREF";
         	  $info{$line[0],"name"} = $tiaacref_ids{$line[0]};
         	  $info{$line[0],"date"} = $line[2];
         	  $info{$line[0],"nav"} =  $line[1];
		  $info{$line[0],"price"} = $info{$line[0],"nav"};
	 	  $info{$line[0],"success"} = 1; #this contains good data, beyond a reasonable doubt
	 	  $check{$line[0]} = 1;
	  } else {
	  	$info{$line[0],"success"} = 0;
	  	$info{$line[0],"errormsg"} = "Bad data returned";
	  }
       }
    } else {
	foreach $_ (@funds) {
		$info{$_,"success"} = 0;
		$info{$_,"errormsg"} = "HTTP error";
	} # foreach
	
    } #if $reply->is_success else
    
    
    #now check to make sure a value was returned for every symbol asked for
    foreach my $k (keys %check) {
    	if ($check{$k} == 0) {
    		$info{$k,"success"} = 0;
    		$info{$k,"errormsg"} = "No data returned";
    	}
    }

    return %info;
}

# Currency allows the user to convert from one currency to another.
# WARNING - This function is still under development.  Use at your
#           own risk.  This function's interface and behaviour can
#           and WILL change in the future.

sub currency {
	shift if (ref($_[0]));	# Pop the object if we have one.
	my ($from, $to) = @_;
	return undef unless ($from and $to);

	my $ua = LWP::UserAgent->new;
	$ua->timeout($TIMEOUT) if defined $TIMEOUT;
	$ua->env_proxy();
	my $data = $ua->request(GET "${YAHOO_CURRENCY_URL}s=$from&t=$to")->content;
	
	my ($exchange) = $data =~ m#$from$to=X</a></td><td>1</td><td>\d\d?:\d\d\w\w</td><td>(\d+\.\d+)</td>#;

	return ( from => $from, to => $to, exchange => $exchange );
}

# =======================================================================

1;

__END__

=head1 NAME

Finance::Quote - Get stock and mutual fund quotes from various exchanges

=head1 SYNOPSIS

 use Finance::Quote;
 my $q = Finance::Quote->new;          # New Finance::Quote object.
 $q->timeout(60);		       # Timeout max of 60 seconds
 %quotes = $q->yahoo(@symbols);	       # NYSE quotes 
 %quotes = $q->yahoo_europe(@symbols); # Europe quotes
 %quotes = $q->fidelity(@symbols);     # Fidelity Investments Quotes
 %quotes = $q->troweprice();           # Quotes from T. Rowe Price
 %quotes = $q->tiaacref(@symbols);     # Annuities from TIAA-CREF
 %quotes = $q->asx(@symbols);          # Australian quotes from ASX.
 %quotes = $q->fetch("asx",@symbols);  # Same as above, different syntax.
 print ("the last price was ", $quotes{"IBM", "last"} );

=head1 DESCRIPTION

This module gets stock quotes from various internet sources, including
Yahoo!  Finance, Fidelity Investments, and the Australian Stock Exchange.
The functions will return a quote for each of the stock symbols passed to
it.  The return value of each of the routines is a hash, which may include
one or more of the following elements:

    name         Company or Mutual Fund Name
    last         Last Price
    high	 Highest trade today
    low		 Lowest trade today
    date         Last Trade Date  (MM/DD/YY format)
    time         Last Trade Time
    net          Net Change
    p_change     Percent Change from previous day's close
    volume       Volume
    avg_vol      Average Daily Vol
    bid          Bid
    ask          Ask
    close        Previous Close
    open         Today's Open
    day_range    Day's Range
    year_range   52-Week Range
    eps          Earnings per Share
    pe           P/E Ratio
    div_date     Dividend Pay Date
    div          Dividend per Share
    div_yield    Dividend Yield
    cap          Market Capitalization
    ex_div	 Ex-Dividend Date.
    nav          Net Asset Value
    yield        Yield (usually 30 day avg)
    success	 Did the stock successfully return information? (true/false)
    errormsg	 If success is false, this field may contain the reason why.

    (Elements which are not yet implemented have no key associated
     with them.  Not all methods return all keys at all times.)

If all stock lookups fail (possibly because of a failed connection) then
`undef' may be returned.

You may optionally override the default LWP timeout of 180 seconds by setting
$quote->timeout() or Finance::Quote::timeout() to your preferred value.

Note that prices from the Australian Stock Exchange (ASX) are in
Australian Dollars.  Prices from Yahoo! Europe are in Euros.  All other
prices are in US Dollars.

=head2 troweprice

The troweprice() function ignores any arguments passed to it.  Instead it
returns all funds managed by T.RowePrice.

=head2 tiaacref

For TIAA and CREF Annuities, you must use TIAA-CREF's pseudosymbols. These
are as follows:

    Stock:				CREFstok
    Money Market:			CREFmony
    Equity Index:			CREFequi
    Inflation-Linked Bond:		CREFinfb
    Bond Market:			CREFbond
    TIAA Real Estate:			TIAAreal
    Social Choice:			CREFsoci
    Teachers PA Stock Index:		TIAAsndx
    Global Equities:			CREFglob
    Teachers PA Select Stock:		TIAAsele
    Growth:				CREFgrow

=head2 FETCH

    my %stocks = $q->fetch("nasdaq","IBM","MSFT");

A new function, fetch(), provides a more generic and easy-to-use interface
to the library.  It takes a source as the first argument, and then a list
of ticker-symbols to obtain from that source.  fetch() will understand the
case-insensitive sources "nasdaq", "nyse" and "europe", and map them to
the yahoo or yahoo_europe methods appropriately.

=head1 ENVIRONMENT

Finance::Quote respects all environment that your installed
version of LWP::UserAgent respects.  Most importantly, it
respects the http_proxy environment variable.

=head1 FAQ

If there's one question I get asked over and over again, it's how did I
figure out the format string for Yahoo! quotes?  Having typed the answer in
innumerable emails, I figure sticking it directly into the man page might
help save my fingers a bit...

If you have a My Yahoo! (http://my.yahoo.com) account, go to the
following URL:

    http://edit.my.yahoo.com/config/edit_pfview?.vk=v1

Viewing the source of this page, you'll come across the section that
defines the menus that let you select which elements go into a
particular view.  The <option> values are the strings that pick up
the information described in the menu item.  For example, Symbol
refers to the string "s" and name refers to the string "l".  Using
"sl" as the format string, we would get the symbol followed by the
name of the security.

If you have questions regarding this, play around with $YAHOO_URL, changing
the value of the f parameter.

=head1 BUGS

Not all functions return an errormsg when a failure results.

Not everything checks for errors as well as they could.

There is no way to add extra aliases to the fetch list.

There is no good documentation on which functions return what fields.

This documentation is getting a little long and cumbersome.  It should
be broken up into more logical sections.

=head1 COPYRIGHT

 Copyright 1998, Dj Padzensky
 Copyright 1998, 1999 Linas Vepstas
 Copyright 2000, Yannick LE NY (update for Yahoo Europe and YahooQuote)
 Copyright 2000, Paul Fenwick (update for ASX)
 Copyright 2000, Brent Neal (update for TIAA-CREF)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

The information that you obtain with this library may be copyrighted
by Yahoo! Inc., and is governed by their usage license. See
http://www.yahoo.com/docs/info/gen_disclaimer.html for more
information.

The information that you obtain with this library may be copyrighted
by the ASX, and is governed by its usage license.  See
http://www3.asx.com.au/Fdis.htm for more information.

The information that you obtain with this library may be copyrighted
by TIAA-CREF, and is governed by its usage license.

Other copyrights and conditions may apply to data fetched through this
module.

=head1 AUTHORS

  Dj Padzensky (C<djpadz@padz.net>), PadzNet, Inc.
  Linas Vepstas (C<linas@linas.org>)
  Yannick LE NY (C<y-le-ny@ifrance.com>)
  Paul Fenwick (C<pjf@schools.net.au>)
  Brent Neal (C<brent@phys.lsu.edu>)

The Finance::Quote home page can be found at
http://finance-quote.sourceforge.net/

The Finance::YahooQuote home page can be found at
http://www.padz.net/~djpadz/YahooQuote/

The GnuCash home page can be found at
http://www.gnucash.org/

=cut
