#!/usr/bin/env perl
use warnings;
use strict;

use Git;
use Cwd qq(getcwd);
use Text::Wrap;


sub print_notes {
    my $notes = shift;
    print ("\nNOTES:\n");
    print fill ('', '', $notes);
    print "\n\n";
}

sub text_format {
    my ($string, $indent, $tab) = @_;
    my ($sum, $desc, $notes) = split('\<\|\>', $string);
    $sum =  wrap($indent, $tab, $sum);
    print "$sum\n";
    if ($desc) {
        $desc = fill($tab, $tab, $desc);
        print "$desc\n";
    }
    print_notes($notes) if ($notes);
}

sub html_format_bug {
    my $string = shift;
    my $href='"https://bugs.gnucash.org/show_bug.cgi?id=XXXXXX"';
    my ($sum, $desc, $notes) = split('\<\|\>', $string);
    $sum =~ m/Bug ([0-9]+) - /;
    my $num = $1;
    die "No bug number in $sum" if ! $num;
    $href =~ s/XXXXXX/$num/;
    print "<li><a href=$href>$sum</a>";
    print "<p>$desc</p>" if ($desc);
    print_notes($notes) if ($notes);
    print "</li>\n";
}

sub html_format_other {
    my $string = shift;
    my ($sum, $desc, $notes) = split('\<\|\>', $string);
    die "No summary in $string" if not $sum;
    print "<li>$sum";
    print "<p>$desc</p>" if ($desc);
    print_notes($notes) if ($notes);
    print"</li>\n";
}

my $repo = Git->repository(Directory => getcwd);
$repo->command('describe') =~ m/(^[.\d]+)/;
my $tag = $1 or die "Unable to determine tag";

my (@bugs, @improves);
my ($revs, $c) = $repo->command_output_pipe('log', '--topo-order', '--format=%s<|>%b<|>%N<{}>', "$tag..HEAD");
my $item = "";
while(<$revs>) {
    my $rev = $_;
    chomp($rev);
    $item .= ' ' if $item;
    $item .= $rev;
    if ($rev =~ /<\{\}>$/) {
        $item =~ s/<\{\}>//;
        if ($item =~ m/^[\s\[]*[Bb]ug[\]\s:\-\#]*[0-9]+/) {
            $item =~ s/^[\s\[]*[Bb]ug[\]\s:\-\#]*([0-9]+)[ -]*/Bug $1 - /;
            push @bugs, $item;
        } else {
            push @improves, $item;
        }
        $item = '';
    }
}
$repo->command_close_pipe($revs, $c);

print "\nThe following bugs have been fixed:\n";
foreach my $bug (sort @bugs) {
    text_format($bug, '    ', '                 ');
}

print "\nOther repairs or enhancements not marked as bugs:\n\n";
foreach my $other (@improves) {
    text_format($other, '    ', '      ');
}

print "*****HTML OUTPUT*****\n\n";
print "<h6>Between $tag and XXX, the following bugfixes were accomplished:</h6>\n<ul>\n";
foreach my $bug (sort @bugs) {
    html_format_bug($bug);
}
print "</ul>\n<h6>The following fixes and improvements were not associated with bug reports:</h6>\n<ul>\n";
foreach my $other (@improves) {
    html_format_other($other);
}
print "</ul>\n";
