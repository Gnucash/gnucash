#!/bin/sh

#
# This assumes we're in the "packaging" directory for the correct build.
# It could be the packaging/win32 subdir of a tag checkout, or it could
# be the top-level daily-build packaging directory.
# Note: GIT_CMD below should have been set by a calling script.
#

set -o pipefail
set -e
LOG_DIR=build-logs
BUILD_HOST="gnucash-win32"
LOG_HOST=upload@code.gnucash.org:public_html/win32

function on_error() {
  if [ `hostname` = ${BUILD_HOST} ]; then
    scp -p ${LOGFILE} ${LOG_HOST}/$LOG_DIR
  fi
  exit
}

function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

. functions.sh
. defaults.sh

tag="${1:-$GNUCASH_SCM_REV}"

# Determine where to upload to
# NOTE: this assumes "tag" to be either a tag or a branch, not a
#       commit hash. It will probably work with a hash as well,
#       but will create a directory for the hash
if [ -n "$($GIT_CMD tag -l $tag)" ]; then
  TARGET_DIR=releases
  LOG_TAG=$tag
else
  TARGET_DIR=$tag
  LOG_TAG=$TARGET_DIR
fi

set_default OUTPUT_DIR $GLOBAL_DIR\\output
LOGFILENAME=build-${LOG_TAG}-`date +'%Y-%m-%d'`.log

_OUTPUT_DIR=`unix_path $OUTPUT_DIR`
LOGFILE=${_OUTPUT_DIR}/${LOGFILENAME}
mkdir -p ${_OUTPUT_DIR}

# Small hack to create $LOG_DIR on the webserver if it doesn't exist yet
if [ `hostname` = ${BUILD_HOST} ]; then
  mkdir -p "$_OUTPUT_DIR/$LOG_DIR"
  scp -r "$_OUTPUT_DIR/$LOG_DIR" ${LOG_HOST}
  rmdir "$_OUTPUT_DIR/$LOG_DIR"
fi

# If we're running on the build server, copy a temporary logfile
# content to the webserver to signal that the build is in progress
if [ `hostname` = ${BUILD_HOST} ]; then
    _PWD=`pwd`
    echo "Build for tag \"${tag}\" is in progress (current working directory: ${_PWD}) ..." > ${LOGFILE}
    scp -p ${LOGFILE} ${LOG_HOST}/${LOG_DIR}
fi

set +e
trap on_error ERR

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
REVISION=`grep GNUCASH_SCM_REV ${_BUILD_UDIR}/src/core-utils/gnc-scm-info.h | cut -d" " -f3 | cut -d\" -f2 `

# Choose the output filename based on our "build_from_tarball" setting
# Make sure this logic matches the logic in dist.sh!
if [ "$BUILD_FROM_TARBALL" = "no" ]; then
  SETUP_FILENAME="gnucash-${PKG_VERSION}-$(date +'%Y-%m-%d')-git-${REVISION}-setup.exe"
else
  SETUP_FILENAME="gnucash-${PKG_VERSION}-setup.exe"
fi
mv ${_GNUCASH_UDIR}/${SETUP_FILENAME} ${_OUTPUT_DIR}

#
# Verify that PKG_VERSION == $tag, and add to the build log if it's not.
# Note: only do this if tag exists and matches x.y.z
#
if [ -n "${tag}" ] ; then
  case "${tag}" in
  [0-9]*.[0-9]*.[0-9]*)
     if [ "${PKG_VERSION}" != "${tag}" ] ; then
       echo "" >> ${LOGFILE}
       echo " *** ERROR: Package Version ${PKG_VERSION} doesn't match Tag ${tag}" >> ${LOGFILE}
       echo "" >> ${LOGFILE}
    fi
    ;;
  esac
fi

# If we're running on the build server then upload the files
if [ `hostname` = ${BUILD_HOST} ]; then
  # Small hack to create the $TARGET_DIR on the webserver if it doesn't exist yet
  mkdir -p "$_OUTPUT_DIR/$TARGET_DIR"
  scp -r "$_OUTPUT_DIR/$TARGET_DIR" ${LOG_HOST}
  rmdir "$_OUTPUT_DIR/$TARGET_DIR"
  # Copy the files to the chosen target directory
  scp -p ${LOGFILE} ${LOG_HOST}/$LOG_DIR
  scp -p ${_OUTPUT_DIR}/${SETUP_FILENAME} ${LOG_HOST}/$TARGET_DIR
fi
