#!/bin/sh
#
# Steps will be executed in the order they were added.  They can only be added
# at most once if they have not been blocked already (adding implies blocking).
# To add a custom step <s>, just implement "function <s>()".  Keep in mind that
# blocking or reordering may render install.sh & friends non-functional.


[ ! "$BASH" -a -x /bin/bash ] && exec /bin/bash "$0" "$@"

set -e

echo -n "Build Starting at "
date

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
. ./functions.sh
. ./defaults.sh
. ./install-impl.sh

# variables
register_env_var ACLOCAL_FLAGS " "
register_env_var AUTOTOOLS_CPPFLAGS " "
register_env_var AUTOTOOLS_LDFLAGS " "
register_env_var GMP_CPPFLAGS " "
register_env_var GMP_LDFLAGS " "
register_env_var GNOME_CPPFLAGS " "
register_env_var GNOME_LDFLAGS " "
register_env_var GNUTLS_CPPFLAGS " "
register_env_var GNUTLS_LDFLAGS " "
register_env_var GUILE_LOAD_PATH ";"
register_env_var GUILE_CPPFLAGS " "
register_env_var GUILE_LDFLAGS " "
register_env_var HH_CPPFLAGS " "
register_env_var HH_LDFLAGS " "
register_env_var INTLTOOL_PERL " "
register_env_var LIBDBI_CPPFLAGS " "
register_env_var LIBDBI_LDFLAGS " "
register_env_var KTOBLZCHECK_CPPFLAGS " "
register_env_var KTOBLZCHECK_LDFLAGS " "
register_env_var PATH ":"
register_env_var PCRE_CPPFLAGS " "
register_env_var PCRE_LDFLAGS " "
register_env_var PKG_CONFIG ":" ""
register_env_var PKG_CONFIG_PATH ":"
register_env_var READLINE_CPPFLAGS " "
register_env_var READLINE_LDFLAGS " "
register_env_var REGEX_CPPFLAGS " "
register_env_var REGEX_LDFLAGS " "
register_env_var WEBKIT_CFLAGS " "
register_env_var WEBKIT_LIBS " "

# steps
# There is no reason to ever need to comment these out!
# * commented out glade, as it is not needed to run gnucash
add_step inst_prepare
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_wget
 add_step inst_dtk
 add_step inst_unzip
fi
add_step inst_mingw
add_step inst_regex
add_step inst_readline
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_active_perl
 add_step inst_mingwutils
fi
add_step inst_autotools
add_step inst_gmp
add_step inst_guile
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_svn
fi
add_step inst_exetype
add_step inst_libxslt
add_step inst_gnome
add_step inst_gnutls
add_step inst_isocodes
add_step inst_swig
add_step inst_pcre
add_step inst_libbonoboui
add_step inst_libgsf
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_hh
fi
add_step inst_goffice
#add_step inst_glade
add_step inst_opensp
add_step inst_libofx
## Online banking:
add_step inst_gwenhywfar
add_step inst_ktoblzcheck
add_step inst_aqbanking
add_step inst_libdbi
add_step inst_webkit
##
if [ "$UPDATE_SOURCES" = "yes" ]; then
 add_step svn_up
fi
if [ "$WITH_CUTECASH" = "yes" ]; then
 add_step inst_cmake
 add_step inst_cutecash
fi
add_step inst_gnucash
add_step inst_docs
if [ "$CROSS_COMPILE" != "yes" ]; then
 add_step inst_inno
fi
add_step inst_finish

# run commands registered with late_eval
eval_now

for step in "${steps[@]}" ; do
    eval $step
done
qpopd

echo -n "Build Finished at "
date

### Local Variables: ***
### sh-basic-offset: 4 ***
### indent-tabs-mode: nil ***
### End: ***
