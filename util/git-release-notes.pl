#!/usr/bin/env perl
use 5.012;
use warnings;
use strict;

use Encode;
use Git;
use Cwd qq(getcwd);
use Text::Wrap;
binmode(STDOUT, ":utf8");

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

# escape_html lifted from https://metacpan.org/source/TOKUHIROM/HTML-Escape-1.10/lib/HTML/Escape/PurePerl.pm.
our %_escape_table = ( '&' => '&amp;', '>' => '&gt;', '<' => '&lt;',
                       q{"} => '&quot;', q{'} => '&#39;', q{`} => '&#96;',
                       '{' => '&#123;', '}' => '&#125;' );
sub escape_html {
    my $str = shift;
    return ''
        unless defined $str;
    $str =~ s/([&><"'`{}])/$_escape_table{$1}/ge; #' for poor editors
    return $str;
}

sub html_format_bug {
    my $string = shift;
    my $href='"https://bugs.gnucash.org/show_bug.cgi?id=XXXXXX"';
    my ($sum, $desc, $notes) = split('\<\|\>', $string);
    $sum =~ m/Bug ([0-9]+) - /;
    my $num = $1;
    die "No bug number in $sum" if ! $num;
    $href =~ s/XXXXXX/$num/;
    $sum = escape_html($sum);
    $desc = escape_html($desc);
    $notes = escape_html($notes);
    print "<li><a href=$href>$sum</a>";
    print "<p>$desc</p>" if ($desc);
    print_notes($notes) if ($notes);
    print "</li>\n";
}

sub html_format_other {
    my $string = shift;
    my ($sum, $desc, $notes) = split('\<\|\>', $string);
    die "No summary in $string" if not $sum;
    $sum = escape_html($sum);
    $desc = escape_html($desc);
    $notes = escape_html($notes);
    print "<li>$sum";
    print "<p>$desc</p>" if ($desc);
    print_notes($notes) if ($notes);
    print"</li>\n";
}

my $repo = Git->repository(Directory => getcwd);
$repo->command('describe') =~ m/(^[.\d]+)/;
my $tag = $1 or die "Unable to determine tag";

my (@bugs, @improves, %l10n);
my ($revs, $c) = $repo->command_output_pipe('log', '--topo-order', '--format=%s<|>%b<|>%N<{}>', "$tag..HEAD");
my $item = "";
while(<$revs>) {
    my $rev = decode('UTF-8', $_);
    chomp($rev);
    $item .= ' ' if $item;
    $item .= $rev;
    if ($rev =~ /<\{\}>$/) {
        $item =~ s/<\{\}>//;
        if ($item =~ m/^[\s\[]*[Bb]ug[\]\s:\-\#]*[0-9]+/) {
            $item =~ s/^[\s\[]*[Bb]ug[\]\s:\-\#]*([0-9]+)[ -]*/Bug $1 - /;
            push @bugs, $item;
        } elsif ($item =~ m/^[Ll]10[Nn]:([a-z]{2}(?:[-_][A-Z]{2})?)/) {
            $l10n{$1}++ unless ($item =~ /glossary/i);
        }elsif ($item =~ m/^Translation/) {
            map { $l10n{$_}++ } $item =~ m/GnuCash\/Program \(([[:alpha:][:punct:][:space:]]+)\)/g;
        } elsif ($item =~ m/^(?:Merge|[Ll]1[08][Nn]|[Ii]1[08][Nn])/) {
            my ($sum, $desc, $notes) = split('\<\|\>', $item);
            push @improves, $item if ($desc || $notes);
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

print "\nNew and Updated Translations: ", join(", ", keys(%l10n)), "\n\n";

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
print "<p>New and Updated Translations: ", join(", ", sort(keys(%l10n))), "</p>\n";
