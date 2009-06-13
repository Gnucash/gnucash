#!/bin/sh

set -e

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

TAG_URL=http://svn.gnucash.org/repo/gnucash/tags

################################################################
# Setup our environment  (we need the DOWNLOAD_DIR)

qpushd "$(dirname $(unix_path "$0"))"
pkgdir="`pwd`"
svn update
. functions.sh
. defaults.sh


################################################################
# determine if there are any new tags since the last time we ran
#

# If we don't have a tagfile then start from 'now'
tagfile=tags
if [ ! -f ${tagfile} ] ; then
  svn ls ${TAG_URL} > ${tagfile}
fi

# Figure out the new set of tags
svn ls ${TAG_URL} > ${tagfile}.new
tags="`diff --suppress-common-lines ${tagfile} ${tagfile}.new | grep '^> ' | sed -e 's/^> //g' -e 's#/$##g'`"

# move the new file into place
mv -f ${tagfile}.new ${tagfile}

################################################################
# Now iterate over all the new tags (if any) and build a package

for tag in $tags ; do
  tagbasedir=/c/soft/gnucash-${tag}
  tagdir=${tagbasedir}/gnucash
  mkdir -p ${tagdir}

  # Copy the downloads to save time
  mkdir -p ${tagbasedir}/downloads
  cp -p $(unix_path ${DOWNLOAD_DIR})/* ${tagbasedir}/downloads

  # Check out the tag and setup custom.sh
  svn co -q ${TAG_URL}/${tag} ${tagdir}/repos
  w32pkg=${tagdir}/repos/packaging/win32
  cp -p "${pkgdir}/custom.sh" ${w32pkg}/custom.sh

  # Set the global directory to the tag build
  echo "GLOBAL_DIR=c:\soft\gnucash-${tag}" >> ${w32pkg}/custom.sh

  # No need to update the sources we just checked out
  echo "UPDATE_SOURCES=no" >> ${w32pkg}/custom.sh

  # Now build the tag!  (this will upload it too)
  qpushd ${w32pkg}
    ./build_package.sh ${tag}
  qpopd
done
