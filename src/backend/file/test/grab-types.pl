#!/usr/bin/perl -w

use strict;

my $tag = shift @ARGV;
my $filename = shift @ARGV;
my $file_to_write_to_pattern = shift @ARGV;
my $files_written = "";

open(XMLFILE, $filename);

sub write_file
{
  my $data = shift;
  my $num = shift;

  my $towriteto = $file_to_write_to_pattern;
  $towriteto =~ s/XXX/$num/;

  open(TOWRITE, ">$towriteto");

  print TOWRITE $data;

  close TOWRITE;

  $files_written .= " $towriteto";
}

my $xml_header = "<?xml version=\"1.0\"?>\n<gnc-v2>\n";
my $to_write = $xml_header;
my $grabbing = 0;
my $grab_num = 0;

while (<XMLFILE>) {
  if ($grabbing) {
    $to_write .= $_;
    if ($_ =~ m/\<\/$tag\>/) {
      $to_write .= "</gnc-v2>\n";
      write_file($to_write, $grab_num);
      $to_write = $xml_header;
      $grab_num++;
      $grabbing = 0;
    }
  }
  elsif ($_ =~ m/\<$tag .*\>/) {
    $grabbing = 1;
    $to_write .= $_;
  }
}

print $files_written
