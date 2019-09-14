#!/usr/bin/perl -w
# -*- perl -*-
#
# Sort filenames in POTFILES.in in a consistent way
# This reduces the amount of clutter in our version management system
# The files will be sorted
# * per directory
# * case-insensitive
# * ignoring punctuation (-,_,.)
#
sub sort_func
{
    my $stripped_a = $a;
    my $stripped_b = $b;
    $stripped_a =~ s/[-_.]//g;
    $stripped_b =~ s/[-_.]//g;

    lc ($stripped_a) cmp lc ($stripped_b)
      ||
    lc ($a) cmp lc ($b)
}

#open my $fh, '<', $ARGV[0];
#my @raw_files = <$fh>;
my @raw_files = <STDIN>;
my @possible_files = sort sort_func @raw_files;
print @possible_files;
