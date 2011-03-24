#!/bin/sh
#
# Note: for this script to work, you need to make sure bash can
#       find svn on your system. If it's not in the default
#       Windows path, you will have to add it yourself, for
#       example like this:
#       - create a file /etc/profile.d/svn.sh
#       - add this line: export PATH=/c/soft/svn/bin:$PATH
#       (Use the real path to your svn installation, obviously)

set -e

# Only run this script on Monday night (first day of the week)
if [ `date +%u` != 2 ] ; then exit ; fi

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
svn update
./build_package.sh
qpopd
