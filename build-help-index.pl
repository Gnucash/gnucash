#! /usr/bin/perl

use DB_File;

tie %contents, 'DB_File', $ARGV[0];

shift @ARGV;

foreach my $file (@ARGV) {
    my %filewords;

    open(HELPFILE, $file) or die "Bad help file $file specified.\n";
    my $size = (stat($file))[7];
    my $data;
    read HELPFILE,$data,$size;
    $data =~ s/<[^>]*>/ /gs;          # get rid of HTML tags 
    $data =~ tr/\",();&<>!$*/ /;     # get rid of extra punct
    $data =~ tr/[A-Z]/[a-z]/;        # lowercase everything
    $data =~ tr/ \012\011/ /s;       # crunch whitespace 
    $data =~ s/[\.,\'\":\;\+|-]+ / /gs; # get rid of terminal punct 
    $data =~ s/ [.,\'\":;+|-]+/ /gs;    # get rid of initial punct 
    $data =~ s/ [^ ] / /gs;          # remove 1-letter words 
    $data =~ s/ [^ ][^ ] / /gs;      # remove 2-letter words 
    $data =~ tr/ \012\011/ /s;       # crunch whitespace again
    my @words = split(' ', $data);
    @words = sort(@words);
    foreach my $w (@words) {
	$filewords{$w} = ' ';
    }
    foreach my $w (keys(%filewords)) {
	my $flist = $contents{$w};
	$contents{$w} = "$flist$file\012";
    }
}

untie %contents;


