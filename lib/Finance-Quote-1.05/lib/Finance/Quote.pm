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

package Finance::Quote;
require 5.005;

use strict;
use Exporter ();
use Carp;
use Finance::Quote::UserAgent;
use HTTP::Request::Common;

use vars qw/@ISA @EXPORT @EXPORT_OK @EXPORT_TAGS
            $VERSION $TIMEOUT %MODULES %METHODS $AUTOLOAD
	    $YAHOO_CURRENCY_URL $USE_EXPERIMENTAL_UA/;

$YAHOO_CURRENCY_URL = "http://uk.finance.yahoo.com/m5?";

@ISA    = qw/Exporter/;
@EXPORT = ();
@EXPORT_OK = qw/yahoo yahoo_europe fidelity troweprice asx tiaacref/;
@EXPORT_TAGS = ( all => [@EXPORT_OK]);

$VERSION = '1.05';

$USE_EXPERIMENTAL_UA = 0;

# Autoload method for obsolete methods.  This also allows people to
# call methods that objects export without having to go through fetch.

sub AUTOLOAD {
	my $method = $AUTOLOAD;
	$method =~ s/.*:://;

	# Force the dummy object (and hence default methods) to be loaded.
	_dummy();

	# If the method we want is in %METHODS, then set up an appropriate
	# subroutine for it next time.

	if (exists($METHODS{$method})) {
		eval qq[sub $method {
			my \$this;
			if (ref \$_[0]) {
				\$this = shift;
			}
			\$this ||= _dummy();
			\$this->fetch("$method",\@_); 
		}];
		carp $@ if $@;
		no strict 'refs';	# So we can use &$method
		return &$method(@_);
	}

	carp "$AUTOLOAD does not refer to a known method.";
}

# _load_module (private class method)
# _load_module loads a module(s) and registers its various methods for
# use.

sub _load_modules {
	my $class = shift;
	my $baseclass = ref $class || $class;

	my @modules = @_;

	# Go to each module and use them.  Also record what methods
	# they support and enter them into the %METHODS hash.

	foreach my $module (@modules) {
		my $modpath = "${baseclass}::${module}";
		unless (defined($MODULES{$modpath})) {

			# Have to use an eval here because perl doesn't
			# like to use strings.
			eval "use $modpath;";
			carp $@ if $@;
			$MODULES{$modpath} = 1;

			# Methodhash will continue method-name, function ref
			# pairs.
			my %methodhash = $modpath->methods;
			my %labelhash = $modpath->labels;

			# Find the labels that we can do currency conversion
			# on.

			my $curr_fields_func = $modpath->can("currency_fields")
						|| \&default_currency_fields;
			
			my @currency_fields = &$curr_fields_func;

			# @currency_fields may contain duplicates.
			# This following chunk of code removes them.

			my %seen;
			@currency_fields=grep {!$seen{$_}++} @currency_fields;

			foreach my $method (keys %methodhash) {
				push (@{$METHODS{$method}},
					{ function => $methodhash{$method},
					  labels   => $labelhash{$method},
					  currency_fields => \@currency_fields});
			}
		}
	}
}

# =======================================================================
# new (public class method)
#
# Returns a new Finance::Quote object.  If methods are asked for, then
# it will load the relevant modules.  With no arguments, this function
# loads a default set of methods.

sub new {
	my $self = shift;
	my $class = ref($self) || $self;

	my $this = {};
	bless $this, $class;

	my @modules = ();

	# If there's no argument list, but we have the appropriate
	# environment variable set, we'll use that instead.
	if ($ENV{FQ_LOAD_QUOTELET} and !@_) {
		@_ = split(' ',$ENV{FQ_LOAD_QUOTELET});
	}

	# If we get an empty new(), or one starting with -defaults,
	# then load up the default methods.
	if (!@_ or $_[0] eq "-defaults") {
		shift if (@_);
		# Default modules
		 @modules = qw/Yahoo::Australia Fidelity ASX Troweprice
                               Tiaacref Yahoo::USA Yahoo::Europe
			       DWS VWD Trustnet/;
	}

	$this->_load_modules(@modules,@_);

	$this->{TIMEOUT} = $TIMEOUT if defined($TIMEOUT);
	$this->{FAILOVER} = 1;
	$this->{REQUIRED} = [];

	return $this;
}

# =======================================================================
# _dummy (private function)
#
# _dummy returns a Finance::Quote object.  I'd really rather not have
# this, but to maintain backwards compatibility we hold on to it.
{
	my $dummy_obj;
	sub _dummy {
		return $dummy_obj ||= Finance::Quote->new;
	}
}

# =======================================================================
# currency (public object method)
#
# currency allows the conversion of one currency to another.
#
# Usage: $quoter->currency("USD","AUD");
#	 $quoter->currency("15.95 USD","AUD");
#
# undef is returned upon error.

sub currency {
	my $this = shift if (ref($_[0]));
	$this ||= _dummy();

	my ($from, $to) = @_;
	return undef unless ($from and $to);

	$from =~ s/^\s*(\d*\.?\d*)\s*//;
	my $amount = $1 || 1;

	# Don't know if these have to be in upper case, but it's
	# better to be safe than sorry.
	$to = uc($to);
	$from = uc($from);

	return $amount if ($from eq $to);	# Trivial case.

	my $ua = $this->user_agent;

	my $data = $ua->request(GET "${YAHOO_CURRENCY_URL}s=$from&t=$to")->content;
	my ($exchange_rate) = $data =~ m#$from$to=X</a></td><td>1</td><td(?: nowrap)?>[^<]+</td><td>(\d+\.\d+)</td>#;

	return undef unless $exchange_rate;
	return ($exchange_rate * $amount);
}

# =======================================================================
# set_currency (public object method)
#
# set_currency allows information to be requested in the specified
# currency.  If called with no arguments then information is returned
# in the default currency.
#
# Requesting stocks in a particular currency increases the time taken,
# and the likelyhood of failure, as additional operations are required
# to fetch the currency conversion information.
#
# This method should only be called from the quote object unless you
# know what you are doing.

sub set_currency {
	my $this = shift if (ref $_[0]);
	$this ||= _dummy();

	unless (defined($_[0])) {
		delete $this->{"currency"};
	} else {
		$this->{"currency"} = $_[0];
	}
}

# default_currency_fields (public method)
#
# This is a list of fields that will be automatically converted during
# currency conversion.  If a module provides a currency_fields()
# function then that list will be used instead.

sub default_currency_fields {
	return qw/last high low net bid ask close open day_range year_range
	          eps div cap nav price/;
}

# _convert (private object method)
#
# This function converts between one currency and another.  It expects
# to receive a hashref to the information, a reference to a list
# of the stocks to be converted, and a reference to a  list of fields
# that conversion should apply to.

{
	my %conversion;		# Conversion lookup table.

	sub _convert {
		my $this = shift;
		my $info = shift;
		my $stocks = shift;
		my $convert_fields = shift;
		my $new_currency = $this->{"currency"};

		# Skip all this unless they actually want conversion.
		return unless $new_currency;

		foreach my $stock (@$stocks) {
			my $currency;

			# Skip stocks that don't have a currency.
			next unless ($currency = $info->{$stock,"currency"});

			# Skip if it's already in the same currency.
			next if ($currency eq $new_currency);

			# Lookup the currency conversion if we haven't
			# already.
			unless (exists $conversion{$currency,$new_currency}) {
				$conversion{$currency,$new_currency} =
					$this->currency($currency,$new_currency);
			}

			# Make sure we have a reasonable currency conversion.
			# If we don't, mark the stock as bad.
			unless ($conversion{$currency,$new_currency}) {
				$info->{$stock,"success"} = 0;
				$info->{$stock,"errormsg"} =
					"Currency conversion failed.";
				next;
			}

			# Okay, we have clean data.  Convert it.  Ideally
			# we'd like to just *= entire fields, but
			# unfortunately some things (like ranges,
			# capitalisation, etc) don't take well to that.
			# Hence we pull out any numbers we see, convert
			# them, and stick them back in.  That's pretty
			# yucky, but it works.

			foreach my $field (@$convert_fields) {
				next unless (defined $info->{$stock,$field});

				$info->{$stock,$field} = $this->scale_field($info->{$stock,$field},$conversion{$currency,$new_currency});
			}

			# Set the new currency.
			$info->{$stock,"currency"} = $new_currency;
		}
	}
}

# =======================================================================
# Helper function that can scale a field.  This is useful because it
# handles things like ranges "105.4 - 108.3", and not just straight fields.
#
# The function takes a string or number to scale, and the factor to scale
# it by.  For example, scale_field("1023","0.01") would return "10.23".

sub scale_field {
	shift if ref $_[0];	# Shift off the object, if there is one.

	my ($field, $scale) = @_;
	my @chunks = split(/([^0-9.])/,$field);

	for (my $i=0; $i < @chunks; $i++) {
		next unless $chunks[$i] =~ /\d/;
		$chunks[$i] *= $scale;
	}
	return join("",@chunks);
}


# =======================================================================
# Timeout code.  If called on a particular object, then it sets
# the timout for that object only.  If called as a class method
# (or as Finance::Quote::timeout) then it sets the default timeout
# for all new objects that will be created.

sub timeout {
	if (@_ == 1 or !ref($_[0])) {	# Direct or class call.
		return $TIMEOUT = $_[0];
	}
	
	# Otherwise we were called through an object.  Yay.
	# Set the timeout in this object only.
	my $this = shift;
	return $this->{TIMEOUT} = shift;
}

# =======================================================================
# failover (public object method)
#
# This sets/gets whether or not it's acceptable to use failover techniques.

sub failover {
	my $this = shift;
	my $value = shift;
        return $this->{FAILOVER} = $value if (defined($value));
	return $this->{FAILOVER};
}

# =======================================================================
# require_labels (public object method)
#
# Require_labels indicates which labels are required for lookups.  Only methods
# that have registered all the labels specified in the list passed to
# require_labels() will be called.
#
# require_labels takes a list of required labels.  When called with no
# arguments, the require list is cleared.
#
# This method always succeeds.

sub require_labels {
	my $this = shift;
	my @labels = @_;
	$this->{REQUIRED} = \@labels;
	return;
}

# _require_test (private object method)
#
# This function takes an array.  It returns true if all required
# labels appear in the arrayref.  It returns false otherwise.
#
# This function could probably be made more efficient.

sub _require_test {
	my $this = shift;
	my %available;
	@available{@_} = ();	# Ooooh, hash-slice.  :)
	my @required = @{$this->{REQUIRED}};
	return 1 unless @required;
	for (my $i = 0; $i < @required; $i++) {
		return 0 unless exists $available{$required[$i]};
	}
	return 1;
}

# =======================================================================
# fetch (public object method)
#
# Fetch is a wonderful generic fetcher.  It takes a method and stuff to
# fetch.  It's a nicer interface for when you have a list of stocks with
# different sources which you wish to deal with.
sub fetch {
	my $this = shift if ref ($_[0]);
	
	$this ||= _dummy();
	
	my $method = lc(shift);
	my @stocks = @_;

	unless (exists $METHODS{$method}) {
		carp "Undefined fetch-method $method passed to ".
		     "Finance::Quote::fetch";
		return;
	}

	# Failover code.  This steps through all availabe methods while
	# we still have failed stocks to look-up.  This loop only
	# runs a single time unless FAILOVER is defined.

	my %returnhash = ();

	foreach my $methodinfo (@{$METHODS{$method}}) {
		my $funcref = $methodinfo->{"function"};
		next unless $this->_require_test(@{$methodinfo->{"labels"}});
		my @failed_stocks = ();
		%returnhash = (%returnhash,&$funcref($this,@stocks));

		foreach my $stock (@stocks) {
			push(@failed_stocks,$stock)
				unless ($returnhash{$stock,"success"});
		}

		$this->_convert(\%returnhash,\@stocks,
		                $methodinfo->{"currency_fields"});

		last unless $this->{FAILOVER};
		last unless @failed_stocks;
		@stocks = @failed_stocks;
	}

	return wantarray() ? %returnhash : \%returnhash;
}

# =======================================================================
# user_agent (public object method)
#
# Returns a LWP::UserAgent which conforms to the relevant timeouts,
# proxies, and other settings on the particular Finance::Quote object.
#
# This function is mainly intended to be used by the modules that we load,
# but it can be used by the application to directly play with the
# user-agent settings.

sub user_agent {
	my $this = shift;

	return $this->{UserAgent} if $this->{UserAgent};

	my $ua;

	if ($USE_EXPERIMENTAL_UA) {
		$ua = Finance::Quote::UserAgent->new;
	} else {
		$ua = LWP::UserAgent->new;
	}

	$ua->timeout($this->{TIMEOUT}) if defined($this->{TIMEOUT});
	$ua->env_proxy;

	$this->{UserAgent} = $ua;

	return $ua;
}

# =======================================================================
# parse_csv (public object method)
#
# Grabbed from the Perl Cookbook. Parsing csv isn't as simple as you thought!
#
sub parse_csv
{
    shift if (ref $_[0]);	# Shift off the object if we have one.
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

# Dummy destroy function to avoid AUTOLOAD catching it.
sub DESTROY { return; }

1;

__END__

=head1 NAME

Finance::Quote - Get stock and mutual fund quotes from various exchanges

=head1 SYNOPSIS

   use Finance::Quote;
   $q = Finance::Quote->new;

   $q->timeout(60);

   $conversion_rate = $q->currency("AUD","USD");
   $q->set_currency("EUR");  # Return all info in Euros.

   $q->require_labels(qw/price date high low volume/);

   $q->failover(1);	# Set failover support (on by default).

   %quotes  = $q->fetch("nasdaq",@stocks);
   $hashref = $q->fetch("nyse",@stocks);

=head1 DESCRIPTION

This module gets stock quotes from various internet sources, including
Yahoo! Finance, Fidelity Investments, and the Australian Stock Exchange.
There are two methods of using this module -- a functional interface
that is depreciated, and an object-orientated method that provides
greater flexibility and stability.

With the exception of straight currency exchange rates, all information
is returned as a two-dimensional hash (or a reference to such a hash,
if called in a scalar context).  For example:

    %info = $q->fetch("australia","CML");
    print "The price of CML is ".$info{"CML","price"};

The first part of the hash (eg, "CML") is referred to as the stock.
The second part (in this case, "price") is referred to as the label.

=head2 LABELS

When information about a stock is returned, the following standard labels
may be used.  Some custom-written modules may use labels not mentioned
here.  If you wish to be certain that you obtain a certain set of labels
for a given stock, you can specify that using require_labels().

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
    exchange	 The exchange the information was obtained from.
    success	 Did the stock successfully return information? (true/false)
    errormsg	 If success is false, this field may contain the reason why.
    method	 The module (as could be passed to fetch) which found
		 this information.

If all stock lookups fail (possibly because of a failed connection) then
the empty list may be returned, or undef in a scalar context.

=head1 AVAILABLE METHODS

=head2 NEW

    my $q = Finance::Quote->new;
    my $q = Finance::Quote->new("ASX");
    my $q = Finance::Quote->new("-defaults", "CustomModule");

With no arguents, this creates a new Finance::Quote object
with the default methods.  If the environment variable
FQ_LOAD_QUOTELETS is set, then the contents of FQ_LOAD_QUOTELETS
(split on whitespace) will be used as the argument list.  This allows
users to load their own custom modules without having to change
existing code.  If you do not want users to be able to load their own
modules at run-time, pass an explicit argumetn to ->new() (usually
"-defaults").

When new() is passed one or more arguments, an object is created with
only the specified modules loaded.  If the first argument is
"-defaults", then the default modules will be loaded first, followed
by any other specified modules.

Note that the FQ_LOAD_QUOTELETS environment variable must begin
with "-defaults" if you wish the default modules to be loaded.

Any modules specified will automatically be looked for in the
Finance::Quote:: module-space.  Hence,
Finance::Quote->new("ASX") will load the module Finance::Quote::ASX.

Please read the Finance::Quote hacker's guide for information
on how to create new modules for Finance::Quote.

=head2 FETCH

    my %stocks  = $q->fetch("usa","IBM","MSFT","LNUX");
    my $hashref = $q->fetch("usa","IBM","MSFT","LNUX");

Fetch takes an exchange as its first argument.  The second and remaining
arguments are treated as stock-names.  In the standard Finance::Quote
distribution, the following exchanges are recognised:

    australia		Australan Stock Exchange
    dwsfunds		Deutsche Bank Gruppe funds
    fidelity		Fidelity Investments
    tiaacref		TIAA-CREF
    troweprice		T. Rowe Price
    europe		European Markets
    canada		Canadian Markets
    usa			USA Markets
    nyse		New York Stock Exchange
    nasdaq		NASDAQ
    uk_unit_trusts	UK Unit Trusts
    vanguard		Vanguard Investments
    vwd			Vereinigte Wirtschaftsdienste GmbH

When called in an array context, a hash is returned.  In a scalar
context, a reference to a hash will be returned.  The structure
of this hash is described earlier in this document.

The fetch method automatically arranges for failover support and
currency conversion if requested.

If you wish to fetch information from only one particular source,
then consult the documentation of that sub-module for further
information.

=head2 CURRENCY

    $conversion_rate = $q->currency("USD","AUD");

The currency method takes two arguments, and returns a conversion rate
that can be used to convert from the first currency into the second.
In the example above, we've requested the factor that would convert
US dollars into Australian dollars.

The currency method will return a false value if a given currency
conversion cannot be fetched.

At the moment, currency rates are fetched from Yahoo!, and the
information returned is governed by Yahoo!'s terms and conditions.
See Finance::Quote::Yahoo for more information.

=head2 SET_CURRENCY

    $q->set_currency("FRF");	# Get results in French Francs.

The set_currency method can be used to request that all information be
returned in the specified currency.  Note that this increases the
chance stock-lookup failure, as remote requests must be made to fetch
both the stock information and the currency rates.  In order to
improve reliability and speed performance, currency conversion rates
are cached and are assumed not to change for the duration of the
Finance::Quote object.

At this time, currency conversions are only looked up using Yahoo!'s
services, and hence information obtained with automatic currency
conversion is bound by Yahoo!'s terms and conditions.

=head2 FAILOVER

    $q->failover(1);	# Set automatic failover support.
    $q->failover(0);	# Disable failover support.

The failover method takes a single argument which either sets (if
true) or unsets (if false) automatic failover support.  If automatic
failover support is enabled (default) then multiple information
sources will be tried if one or more sources fail to return the
requested information.  Failover support will significantly increase
the time spent looking for a non-existant stock.

If the failover method is called with no arguments, or with an
undefined argument, it will return the current failover state
(true/false).

=head2 USER_AGENT

    my $ua = $q->user_agent;

The user_agent method returns the LWP::UserAgent object that
Finance::Quote and its helpers use.  Normally this would not
be useful to an application, however it is possible to modify
the user-agent directly using this method:

    $q->user_agent->timeout(10);	# Set the timeout directly.


=head2 SCALE_FIELD

    my $pounds = $q->scale_field($item_in_pence,0.01);

The scale_field() function is a helper that can scale complex fields such
as ranges (eg, "102.5 - 103.8") and other fields where the numbers should
be scaled but any surrounding text preserved.  It's most useful in writing
new Finance::Quote modules where you may retrieve information in a
non-ISO4217 unit (such as cents) and would like to scale it to a more
useful unit (like dollars).

=head1 ENVIRONMENT

Finance::Quote respects all environment that your installed
version of LWP::UserAgent respects.  Most importantly, it
respects the http_proxy environment variable.

=head1 BUGS

There are no ways for a user to define a failover list.

The two-dimensional hash is a somewhat unwieldly method of passing
around information when compared to references.  A future release
is planned that will allow for information to be returned in a
more flexible $hash{$stock}{$label} style format.

There is no way to override the default behaviour to cache currency
conversion rates.

=head1 COPYRIGHT

 Copyright 1998, Dj Padzensky
 Copyright 1998, 1999 Linas Vepstas
 Copyright 2000, Yannick LE NY (update for Yahoo Europe and YahooQuote)
 Copyright 2000, Paul Fenwick (updates for ASX, maintainence and release)
 Copyright 2000, Brent Neal (update for TIAA-CREF)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

Currency information fetched through this module is bound by
Yahoo!'s terms and conditons.

Other copyrights and conditions may apply to data fetched through this
module.  Please refer to the sub-modules for further information.

=head1 AUTHORS

  Dj Padzensky (C<djpadz@padz.net>), PadzNet, Inc.
  Linas Vepstas (C<linas@linas.org>)
  Yannick LE NY (C<y-le-ny@ifrance.com>)
  Paul Fenwick (C<pjf@schools.net.au>)
  Brent Neal (C<brentn@users.sourceforge.net>)
  Volker Stuerzl (C<volker.stuerzl@gmx.de>)
  Keith Refson (C<Keith.Refson#earth.ox.ac.uk>)

The Finance::Quote home page can be found at
http://finance-quote.sourceforge.net/

The Finance::YahooQuote home page can be found at
http://www.padz.net/~djpadz/YahooQuote/

The GnuCash home page can be found at
http://www.gnucash.org/

=head1 SEE ALSO

Finance::Quote::Yahoo, Finance::Quote::ASX, Finance::Quote::Fidelity,
Finance::Quote::Tiaacref, Finance::Quote::Troweprice, LWP::UserAgent,
Finance::Quote::DWS, Finance::Quote::VWD, Finance::Quote::Trustnet

You should have also received the Finance::Quote hacker's guide with
this package.  Please read it if you are interested in adding extra
methods to this package.  The hacker's guide can also be found
on the Finance::Quote website, http://finance-quote.sourceforge.net/

=cut
