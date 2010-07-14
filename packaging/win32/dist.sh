#!/bin/sh

set -e

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
. functions.sh
. defaults.sh
. dist-impl.sh

# variables
register_env_var PATH ":"

# steps
add_step dist_prepare
add_step dist_regex
add_step dist_autotools
add_step dist_guile
add_step dist_gnome
add_step dist_isocodes
add_step dist_pcre
add_step dist_libbonoboui
add_step dist_libgsf
add_step dist_goffice
add_step dist_libofx
if [ "$AQBANKING3" != "yes" ]; then
    add_step dist_openssl
else
    add_step dist_gnutls
    add_step dist_gmp
fi
add_step dist_gwenhywfar
add_step dist_ktoblzcheck
add_step dist_aqbanking
add_step dist_libdbi
add_step dist_webkit
add_step dist_gnucash
add_step dist_finish

# run commands registered with late_eval
eval_now

for step in "${steps[@]}" ; do
    eval $step
done

qpopd


### Local Variables: ***
### sh-basic-offset: 4 ***
### indent-tabs-mode: nil ***
### End: ***
