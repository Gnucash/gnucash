#!/bin/sh

#
# This assumes we're in the "packaging" directory for the correct build.
# It could be the packaging/win32 subdir of a tag checkout, or it could
# be the top-level daily-build packaging directory.
#

set -e

function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

tag="$1"

. functions.sh
. defaults.sh

set_default OUTPUT_DIR $GLOBAL_DIR\\output
LOGFILENAME=build${tag:+-${tag}}-`date +'%Y-%m-%d'`.log

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

# Choose the output filename based on our "build_from_tarball" setting
# Make sure this logic matches the logic in dist.sh!
if [ "$BUILD_FROM_TARBALL" = "no" ]; then
  SETUP_FILENAME="gnucash-${PKG_VERSION}-svn-r${SVN_REV}-setup.exe"
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
       echo " *** ERROR: Package Version ${PKG_VERSION} doesn't match Tag ${tag}" >> ${LOGILE}
       echo "" >> ${LOGFILE}
    fi
    ;;
  esac
fi

# If we're running on the build server then upload the files
# Note: change this target if you're building a different branch
if [ `hostname` = "gnucash-win32" ]; then
  scp -p ${LOGFILE} ${_OUTPUT_DIR}/${SETUP_FILENAME} upload@code.gnucash.org:public_html/win32/trunk
fi
