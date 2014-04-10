#!/usr/bin/perl -w
#
#	Copyright (C) 2000, Paul Fenwick <pjf@cpan.org>
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation; either version 2 of the License, or
#	(at your option) any later version.
#	
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#	
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
#	02111-1307, USA
#
# This module defines our own LWP::UserAgent, in particular it allows
# user-defined headers to be set which will be automatically added to
# new HTTP requests.  This is particularly important if you wish to get
# through authenticated proxies and the like.

package Finance::Quote::UserAgent;
require 5.005;

use strict;
use LWP::UserAgent;
use HTTP::Headers;

use vars qw/@ISA/;

@ISA = qw/LWP::UserAgent/;

# A very simple extension.  When we generate a LWP::UserAgent object,
# we add an extra field called finance_quote_headers which stores an
# HTTP::Headers object.

sub new {
	my $ua = LWP::UserAgent::new(@_);
	$ua->{finance_quote_headers} = HTTP::Headers->new();
	return $ua;
}

# This returns the HTTP::Headers object, so the user can play with it.
sub default_headers {
	my $this = shift;
	return $this->{finance_quote_headers};
}

# Over-ride for the simple_request method.  This sets the user-supplied
# template headers if they have not already been set in the request.
sub simple_request {
	my ($this, $request, @args) = @_;
	my $new_request = $this->_add_custom_headers($request);
	return $this->SUPER::simple_request($new_request,@args);
}

# Over-ride for the request method.  This also sets the user-supplied
# template headers if they have not already been set in the request.
sub request {
	my ($this, $request, @args) = @_;
	my $new_request = $this->_add_custom_headers($request);
	return $this->SUPER::request($new_request,@args);
}

# _add_custom_headers is a private method which does the dirty work
# of copying across headers and other fun things.
#
# We take the user-defined template, and then overlay the request over the
# top of it.  This should get us by in most situations.

sub _add_custom_headers {
	my ($this, $request) = @_;
	my $header_template = $this->default_headers;
	my $new_request = $request->clone; # Modifying the original is rude.

	# Copy things that are in the template that we don't have
	# defined in the request.

	$header_template->scan(sub {
		$new_request->header($_[0],$_[1]) unless
			defined ($new_request->header($_[0]));
	});

	return $new_request;
}

# If users wish to place their username and proxy password(!) into
# the "http_proxy_auth_clear" environment variable, then we'll
# read it out and automatically use it for proxy requests.

sub env_proxy {
	my ($this, @args) = @_;
	if ($ENV{http_proxy_auth_clear}) {
		$this->default_headers->proxy_authorization_basic(
			split(/:/,$ENV{http_proxy_auth_clear}));
	}
	$this->SUPER::env_proxy(@_);
}

1;
