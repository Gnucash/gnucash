#!/bin/sh
#
# Notes:
# 1. for this script to work, git must have been setup before
#    in a way that doesn't conflict with the GnuCash build.
#    The easiest way to do so is to run the build once manually
#    with a properly set up custom.sh.
#
# 2. Should this script change in the source repository, then the
#    git pull below will fail due to a limitation in Windows that
#    won't allow to change a file that is "in use". So in the rare
#    situation this script needs to be updated, you will need to
#    run the git pull once yourself.

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

################################################################
# determine if there are any new commits since the last time we ran
#

# If we don't have a rev file then start from 'now' and force a build
revfile=last_rev_daily
if [ ! -f ${revfile} ] ; then
  echo $($GIT_CMD rev-parse HEAD) > ${revfile}
  oldrev=a   # definitely an invalid, so non-existing git rev
else
  oldrev=$(cat ${revfile})
fi

newrev=$($GIT_CMD rev-parse HEAD)
if [[ "${oldrev}" != "${oldrev}" ]]; then
  ./build_package_git.sh
fi

# move the new file into place, will only happen if the build was successful
echo ${newrev} > ${revfile}

qpopd
