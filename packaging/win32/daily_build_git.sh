#!/bin/sh
#
# Note: for this script to work, git must have been setup before
#       in a way that doesn't conflict with the GnuCash build.
#       The easiest way to do so is to run the build once manually
#       with a properly set up custom.sh.

set -e

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
. functions.sh
. defaults.sh

# Variables
_GIT_UDIR=`unix_path $GIT_DIR`
set_env "$_GIT_UDIR/bin/git" GIT_CMD
export GIT_CMD

$GIT_CMD pull
./build_package_git.sh
qpopd
