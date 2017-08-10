#!/usr/bin/perl

# Spit out the environment variable settings needed based on
# arguments listing @gnc_module_dirs, @guile_load_dirs, and
# @library_dirs
use Getopt::Long;

my $define_exports=1;
my @gnc_module_dirs=();
my @guile_load_dirs=();
my @library_dirs=();
GetOptions ("exports!"       => \$define_exports,  # flag
            "gnc-module-dir=s" => \@gnc_module_dirs, # arry of strings
            "guile-load-dir=s" => \@guile_load_dirs, # arry of strings
            "library-dir=s"    => \@library_dirs,    # arry of strings
            "verbose" => \$verbose) # flag
or die(
"Usage: gnc-test-env.pl [ --exports | --noexports ]\n" .
"                       [ (--gnc-module-dir dir | --guile-load-dir dir | --library-dir dir) ... ]\n");

if ( $^O =~ /MSWin32/ ) {
    $path_separator=";";
} else {
    $path_separator=":";
}

sub print_env_var {
    ($env_name, $dir_suffix, $separator, @dir_list) = @_;
    return if not @dir_list;
    my @suffixed_dir_list = map {
        my $dir = $_ . $dir_suffix;
        if ( $^O =~ /MSWin32/ ) {
           $dir =~ s!/!\\\\!g; } # Backslashes need to be escaped for the environment
        $dir;
    } @dir_list;
    print $env_name . '="' .
          join($separator, @suffixed_dir_list) .
          $separator . '${' . $env_name . '}" ';
}

print_env_var "GNC_MODULE_PATH", "/.libs", $path_separator, @gnc_module_dirs;
print_env_var "GUILE_LOAD_PATH", "", $path_separator, @guile_load_dirs;
print_env_var "GUILE_LOAD_COMPILED_PATH", "", $path_separator, @guile_load_dirs;
print_env_var "LD_LIBRARY_PATH", "/.libs", $path_separator, @library_dirs;
print_env_var "DYLD_LIBRARY_PATH", "/.libs", $path_separator, @library_dirs;

if ( $^O =~ /MSWin32/ ) {
    print_env_var "PATH", "/.libs", ":", @library_dirs;
}

if ($define_exports) {
    print ";\n";
    print "export GNC_MODULE_PATH;\n" if @gnc_module_dirs;
    print "export GUILE_LOAD_PATH;\n" if @guile_load_dirs;
    print "export GUILE_LOAD_COMPILED_PATH;\n" if @guile_load_dirs;
    print "export LD_LIBRARY_PATH;\n" if @library_dirs;
    print "export DYLD_LIBRARY_PATH;\n" if @library_dirs;
    if ( $^O =~ /MSWin32/ ) {
        print "export PATH;\n" if @library_dirs;
    }
}