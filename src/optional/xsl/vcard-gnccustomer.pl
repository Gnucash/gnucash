#!/usr/bin/perl -w

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

use Text::vCard::Addressbook;
use strict;
use vars qw($address_book $hex_string $num $c $d $e $i $vcard $names $name $fullname $email $address $addresses $phones $phone $filename $usage $encoding);

############### USER CONFIGURATION ####################
# Change to the encoding for you locale if your VCF data
# contains accented characters etc.
  $encoding = 'UTF-8';
###############  END CONFIGURATION ####################

# About
#
# This is a very simple file and should be simple to customise.
# A similar file is included in pilot-qof. Currently, the script
# needs to be told where to find the VCF file to transform and
# expects the user to redirect output to a file:
# $ perl vcard-gnccustomer.pl contact.vcf > contact.xml

  $usage = $#ARGV + 1;
  if($usage < 1)
  {
	print ("Error: please specify a .vcf file.\n\n");
	exit;
  }
  $filename = $ARGV[0];
  $address_book = Text::vCard::Addressbook->new({
        'source_file' => $filename,  });
  # generate a temporary GUID to relate the address to the customer
  $hex_string = '';
  for($d = 1;$d < 5;$d++)
  {
	for($c = 1; $c < 8; $c++)
	{
		  $num = (int(rand(99)) + 1) * 10**$c;
	}
	$hex_string .= sprintf("%x", $num);
  }
  print "<?xml version=\"1.0\" encoding=\"$encoding\"?>\n";
  print "<qof-qsf xmlns=\"http://qof.sourceforge.net/\">\n";
  print "  <book count=\"1\">\n";
  print "    <book-guid/>\n";
  print "    <object type=\"gncAddress\" count=\"1\">\n";
  foreach $vcard ($address_book->vcards()) {
 	$names = $vcard->get({ 'node_type' => 'name' });
	foreach $name (@{$names}){
		if($name->given()) {
			$fullname = $name->given() . " " . $name->family();
		}
		else {
			$fullname = $name->family();
		}
		&wrap_in_qsf($fullname, "name");
	}
	$phones = $vcard->get('tel');
	$i = 0;
	foreach $phone (@{$phones}){
		if($i == 0) {
			&wrap_in_qsf($phone->value(), "phone");
		}
		if($i == 1) {
			&wrap_in_qsf($phone->value(), "fax");
		}
		$i++;
	}
	$email = $vcard->get('email');
	foreach $e (@{$email}){ &wrap_in_qsf($e->value(), "email"); }
 	$addresses = $vcard->get({ 'node_type' => 'addresses' });
	foreach $address (@{$addresses}) {
	        &wrap_in_qsf($address->street(), "street");
	        &wrap_in_qsf($address->city(), "city");
	        &wrap_in_qsf($address->region(), "locality");
	}
	print "      <guid type=\"guid\">$hex_string</guid>\n";
  }
  print "    </object>\n";
  print "    <object type=\"gncCustomer\" count=\"1\">\n";
  &wrap_in_qsf($fullname, "name");
  print "      <boolean type=\"active\">true</boolean\n";
  print "      <guid type=\"addr\">$hex_string</guid>\n";
  print "  </book>\n";
  print "</qof-qsf>\n\n";

sub wrap_in_qsf()
{
	if($_[0]) { print "      <string type=\"$_[1]\">$_[0]</string>\n"; }
	else { print "      <string type=\"$_[1]\"/>\n"; }
};
