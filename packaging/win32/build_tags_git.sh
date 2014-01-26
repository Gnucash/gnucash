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

################################################################
# Setup our environment  (we need the DOWNLOAD_DIR)

qpushd "$(dirname $(unix_path "$0"))"
pkgdir="`pwd`"
. functions.sh
. defaults.sh

# Variables
_GIT_UDIR=`unix_path $GIT_DIR`
set_env "$_GIT_UDIR/bin/git" GIT_CMD
export GIT_CMD

$GIT_CMD pull
. functions.sh
. defaults.sh


################################################################
# determine if there are any new tags since the last time we ran
#
$GIT_CMD fetch -t

# If we don't have a tagfile then start from 'now'
tagfile=tags_git
if [ ! -f ${tagfile} ] ; then
  for one_tag in $($GIT_CMD tag)
  do
    tag_hash=$($GIT_CMD rev-parse ${one_tag})
    echo ${tag_hash}/${one_tag} >> ${tagfile}
  done
fi

# Figure out the new set of tags
rm -f ${tagfile}.new
for one_tag in $($GIT_CMD tag)
do
  tag_hash=$($GIT_CMD rev-parse ${one_tag})
  echo ${tag_hash}/${one_tag} >> ${tagfile}.new
done
tags="`diff --suppress-common-lines ${tagfile} ${tagfile}.new | grep '^> ' | sed -e 's/^> //g'`"

# move the new file into place
mv -f ${tagfile}.new ${tagfile}

################################################################
# Now iterate over all the new tags (if any) and build a package

for tag_rev in $tags ; do
  tag=${tag_rev#*/}
  tag=${tag%/*}
  
  # Git builds are only supported from 2.5 up
  tag_major=${tag%%.*}
  tag_tmp=${tag#*.}
  tag_minor=${tag_tmp%%.*}
  major_minor=$(( $tag_major*100 + $tag_minor ))
  if (( $major_minor < 205 ))
  then
     continue
  fi
  
  tagbasedir=/c/soft/gnucash-${tag}
  tagdir=${tagbasedir}/gnucash
  rm -fr $tagbasedir
  mkdir -p ${tagdir}

  # Check out the tag and setup custom.sh
  qpushd ${tagdir}
  $GIT_CMD clone ${REPOS_URL} repos
  qpushd repos
  $GIT_CMD checkout $tag
  qpopd
  qpopd
  w32pkg=${tagdir}/repos/packaging/win32
  cp -p "${pkgdir}/custom.sh" ${w32pkg}/custom.sh

  # Set the global directory to the tag build
  echo -n 'GLOBAL_DIR=c:\\soft\\gnucash-' >> ${w32pkg}/custom.sh
  echo "${tag}" >> ${w32pkg}/custom.sh

  # Point DOWNLOAD_DIR at the global installation so we can reuse
  # most of the already downloaded packages
  echo -n "DOWNLOAD_DIR=" >> ${w32pkg}/custom.sh
  echo "${DOWNLOAD_DIR}" | sed -e 's/\\/\\\\/g' >> ${w32pkg}/custom.sh

  # UPDATE_SOURCES is obsolete, but preserved here to allow the
  # current script to also build older tags, that may still
  # use this parameter.
  # No need to update the sources we just checked out
  echo "UPDATE_SOURCES=no" >> ${w32pkg}/custom.sh

  # BUILD_FROM_TARBALL is special:
  # in install.sh place we check !=yes, in defaults.sh =yes, in dist.sh =no
  # We want it to look like 'no' in install and defaults, but yes in dist
  # so this hack works!
  echo "BUILD_FROM_TARBALL=maybe" >> ${w32pkg}/custom.sh

  # Point HH_DIR at the global installation because we don't need to redo it
  echo -n "HH_DIR=" >> ${w32pkg}/custom.sh
  echo "${GLOBAL_DIR}\\hh" | sed -e 's/\\/\\\\/g' >> ${w32pkg}/custom.sh

  # Now build the tag!  (this will upload it too)
  # Use the build_package script from trunk (cwd), not from the tag
  qpushd ${w32pkg}
    ${pkgdir}/build_package_git.sh ${tag}
  qpopd
done
