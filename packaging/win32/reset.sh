#!/bin/sh

set -e

echo ""
echo "This will reset your installation of gnucash."
echo -n "Are you sure you want to do this?  [y/N] "
read resp
case "$resp" in
  y*|Y*)
    ;;
  *)
    exit 0;
    ;;
esac

####  Load Custom.sh

function add_step() { echo "" >/dev/null; }
function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }

# /c/dir/sub
function unix_path() {
    echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'
}

qpushd "$(dirname $(unix_path "$0"))"
. custom.sh

####  Now clear out the install

basedir=`unix_path $GLOBAL_DIR`
qpushd $basedir

rm -rf regex readline indent guile pexports gnome swig autotools
rm -rf libgsf goffice glade gnucash

rm -f /etc/profile.d/installer.in

echo "Done"
exit 0

### Local Variables: ***
### sh-basic-offset: 4 ***
### tab-width: 8 ***
### End: ***
