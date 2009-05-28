#!/bin/sh

set -e

function qpushd() { pushd "$@" >/dev/null; }
svn update
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
. functions.sh
. defaults.sh

set_default OUTPUT_DIR $GLOBAL_DIR\\output

LOGFILENAME=build-`date +'%Y-%m-%d'`.log

_OUTPUT_DIR=`unix_path $OUTPUT_DIR`
LOGFILE=${_OUTPUT_DIR}/${LOGFILENAME}
mkdir -p ${_OUTPUT_DIR}

# Run the compile
./install.sh 2>&1 | tee ${LOGFILE}

# This directory needs to be removed before calling dist.sh
DIST_DIR=${INSTALL_DIR}\\..\\dist
_DIST_UDIR=`unix_path $DIST_DIR`
rm -rf ${_DIST_UDIR}

# Create the installer
./dist.sh 2>&1 | tee -a ${LOGFILE}

# Copy the resulting installer into the output directory
_BUILD_UDIR=`unix_path $BUILD_DIR`
_GNUCASH_UDIR=`unix_path $GNUCASH_DIR`
PKG_VERSION=`grep PACKAGE_VERSION ${_BUILD_UDIR}/config.h | cut -d" " -f3 | cut -d\" -f2 `
SVN_REV=`grep GNUCASH_SVN_REV ${_BUILD_UDIR}/src/gnome-utils/gnc-svninfo.h | cut -d" " -f3 | cut -d\" -f2 `
SETUP_FILENAME="gnucash-${PKG_VERSION}-svn-r${SVN_REV}-setup.exe"
mv ${_GNUCASH_UDIR}/${SETUP_FILENAME} ${_OUTPUT_DIR}

