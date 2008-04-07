#!/bin/sh

set -e

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
. functions.sh
. defaults.sh
reset_steps
. install.sh
qpopd

prepare
_INSTALL_WFSDIR=`win_fs_path $INSTALL_DIR`
_INSTALL_UDIR=`unix_path $INSTALL_DIR`

make_install
