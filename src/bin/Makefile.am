# Order is important here.
SUBDIRS = . overrides test

# We handle gnucash scripts in a somewhat unexpected way, but we do
# this so that a user who doesn't necessarily have the right
# directories in their path can still invoke these commands via their
# full path, say /some/dir/not/in/path/gnucash and still have the
# right thing happen (i.e. they'll still get the right guile, and the
# right scripts if the sub-exec anything from their scripts).  If you
# want to add another gnucash script, please add the name here (which
# will cause the bindir wrapper to be created, and then put the actual
# code in a script of the same name in ./overrides.  Oh, and don't
# forget to add your script to configure.in's "Adjustments" section if
# you need to.
#
# For testing and other reasons, overrides/* scripts should not modify
# the path to re-insert the overrides dir.  This should only be done
# by these top-level "common" scripts.
gnc_common_scripts = gnucash gnucash-env gnucash-run-script gnucash-make-guids

bin_SCRIPTS = ${gnc_common_scripts}

# if you change gncoverridedir, make sure you change ./overrides/Makefile.am too.
gncoverridesdir = ${GNC_LIBEXECDIR}/overrides

EXTRA_DIST = .cvsignore gnucash-build-env.in generate-gnc-script

## Gnucash scripts -- real code is in overrides, these just get you there.
${gnc_common_scripts}: generate-gnc-script
	./generate-gnc-script $@ "${gncoverridesdir}"

BUILT_SOURCES = ${gnc_common_scripts}
CLEANFILES = ${gnc_common_scripts}


