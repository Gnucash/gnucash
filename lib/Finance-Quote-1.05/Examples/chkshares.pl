#!/usr/bin/perl -w
use strict;
use lib '../lib';
use Finance::Quote qw/asx/;

=head1 NAME

chkshares.pl - Check share information.

=head1 USAGE

chkshares.pl australia TLS CML ITE

=head1 NOTES

Example program.  Demonstrates how to use one of the interface to
Finance::Quote.  The first argument must be the market.

=cut

my ($name, $date, $last, $p_change, $high, $low, $volume, $close);

format STDOUT_TOP =

                                 STOCK REPORT

TICKER         DATE      LAST  %CHANGE       HIGH      LOW    VOLUME     CLOSE
-------------------------------------------------------------------------------
.

format STDOUT =
@<<<<<< @>>>>>>>>>>  @###.### @###.###   @###.### @###.### @>>>>>>>>  @###.###
$name,  $date,       $last,   $p_change, $high,   $low,    $volume,   $close
.

my $quoter = Finance::Quote->new();
my $market = shift || die "Usage: $0 market stocks\n";

my %quote = $quoter->fetch($market,@ARGV);

foreach my $code (@ARGV) {
	unless ($quote{$code,"success"}) {
		warn "Lookup of $code failed - ".$quote{$code,"errormsg"}."\n";
		next;
	}
	$name = $code;
	$date = $quote{$code,'date'};
	$last = $quote{$code,'last'};
	$p_change = $quote{$code,'p_change'};
	$high = $quote{$code,'high'};
	$low = $quote{$code,'low'};
	$volume = $quote{$code,'volume'};
	$close = $quote{$code,'close'};
	write;
}
