#!/usr/bin/perl

# Usage: gnucash-obfuscate.pl input_data.gnucash > output.gnucash
# Adam Buchbinder
# Christian Stimming <christian@cstimming.de>, 2019

use strict;
use warnings;
use utf8;
use Encode;

use Digest::MD5 qw(md5_hex);
use XML::DOM; # on ubuntu this requires 'libxml-dom-perl'

my $dictionary = '/usr/share/dict/american-english';
my $hex_digits = 4;

unless (-f $dictionary) {
	print STDERR "Default dictionary $dictionary not found.\n";
	$dictionary = '/usr/share/dict/words';
	unless (-f $dictionary) {
		die("Cannot open dictionary $dictionary");
	}
}

$ARGV[0] || die("Usage: $0 filename");
(-f $ARGV[0]) || die("Unable to open $ARGV[0]");
(-f $dictionary) || die("Dictionary file $dictionary does not exist");
open DICT, $dictionary || die("Unable to open $dictionary");

my @dict = <DICT>;
close DICT;

sub get_a_name {
	my $scalar = shift;
	return '' if !$scalar;
	my $scalar_encoded = utf8::is_utf8($scalar) ? Encode::encode_utf8($scalar) : $scalar;
	my $index = hex substr(md5_hex($scalar_encoded),-$hex_digits);
	my $result = $dict[$index];
	chop $result;
	return $result;
}

sub replace_text_element {
	my $doc = shift;
	my $tag = shift;
	my $elements = $doc->getElementsByTagName($tag);

	my $i; my $n = $elements->getLength;
	for ($i=0; $i<$n; $i++) {
		my $text = $elements->item($i)->getFirstChild;
                if (defined($text)) {
                  $text->setData(get_a_name($text->getData));
                }
	}
}

sub get_child_value {
	my $parent = shift;
	my $childname = shift;
	my $index = shift;
	
	my $return;
	my $elements = $parent->getElementsByTagName($childname);
	my $child;

	unless (defined($index)) {
		$child = $elements->item(0)->getFirstChild;
	} else {
		$child = $elements->item($index)->getFirstChild;
	}
	$return = $child->getData;
	return $return;
}

sub set_child_value {
	my $parent = shift;
	my $childname = shift;
	my $value = shift;
	
	my $return;
	my $elements = $parent->getElementsByTagName($childname);
	my $child;

	$child = $elements->item(0)->getFirstChild;

	$return = $child->setData($value);
	return $return;
}

my $parser = new XML::DOM::Parser;
print STDERR "Parsing XML document.\n";
my $doc = $parser->parsefile($ARGV[0]);

# first, clean up strings in account names and transaction names
print STDERR "Obfuscating account names.\n";
replace_text_element($doc,'act:name');
print STDERR "Obfuscating account descriptions.\n";
replace_text_element($doc,'act:description');
print STDERR "Obfuscating transaction descriptions.\n";
replace_text_element($doc,'trn:description');
print STDERR "Obfuscating check numbers.\n";
replace_text_element($doc,'trn:num');
print STDERR "Obfuscating split memos.\n";
replace_text_element($doc,'split:memo');
print STDERR "Obfuscating transaction notes.\n";
my $transactions = $doc->getElementsByTagName('trn:slots');
for (my $i=0; $i<$transactions->getLength; $i++) {
	replace_text_element($transactions->item($i),'slot:value');
}

# If "1", replace money amounts, otherwise leave unchanged
if (0) {
  print STDERR "Obfuscating money amounts.\n";
  my $elements = $doc->getElementsByTagName('trn:splits');
  my $i; my $n = $elements->getLength;
  for ($i=0; $i<$n; $i++) {
    my $parent = $elements->item($i);
    my $splits = $parent->getElementsByTagName('trn:split');
    my $count = $splits->getLength;

    foreach my $j (0..$count-1) {
      my $split = $splits->item($j);
      my $valuestring;
      my $sum = 0;
      my ($num,$denom);
      if (get_child_value($split,'split:value') eq
          get_child_value($split,'split:quantity')) {
        $valuestring = get_child_value($split,'split:value');
        ($num,$denom) = $valuestring =~ /(\-?\d+)\/(\d+)/;
        $num = int rand 10*$denom; $sum += $num;
        set_child_value($split,'split:value',"$num/$denom");
        set_child_value($split,'split:quantity',"$num/$denom");
      } else {
        $valuestring = get_child_value($split,'split:value');
        ($num,$denom) = $valuestring =~ /(\-?\d+)\/(\d+)/;
        $num = int rand 10*$denom; $sum = $num;
        set_child_value($split,'split:value',"$num/$denom");

        $valuestring = get_child_value($split,'split:quantity');
        ($num,$denom) = $valuestring =~ /(\-?\d+)\/(\d+)/;
        $num = int rand 10*$denom;
        set_child_value($split,'split:quantity',"$num/$denom");
      }
      # if we're on the last split, balance split:value's to 0
      if ($j == $count-1) {
        $valuestring = get_child_value($split,'split:value');
        ($num,$denom) = $valuestring =~ /(\d+)\/(\d+)/;
        $num = $sum;
        set_child_value($split,'split:value',"$num/$denom");
      }
    }
  }
} else {
  print STDERR "Note: The money amounts are not obfuscated.\n";
}

print STDERR "Writing output to stdout.\n";
print $doc->toString;

$doc->dispose;
