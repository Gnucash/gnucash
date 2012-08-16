#!/bin/sh
#
# Notes:
# 1. for this script to work, you need to make sure bash can
#    find svn on your system. If it's not in the default
#    Windows path, you will have to add it yourself, for
#    example like this:
#    - create a file /etc/profile.d/svn.sh
#    - add this line: export PATH=/c/soft/svn/bin:$PATH
#    (Use the real path to your svn installation, obviously)
#
# 2. The recommended setup is to call this script from within the
#    source code repository (packaging/win32 directory). This is
#    different from before, where it was assumed to be called
#    from outside the source code repository. To remain some
#    compatibility with the old way, svn update is called twice
#    - once for the repository (to update all)
#    - once for the build scripts (to also update the build scripts
#       using the old way)
#    The second invocation is superfluous if this script was called
#    from within the source code repository.
#
# 3. Should this script change in the source repository, then the
#    svn update below will fail due to a limitation in Windows that
#    won't allow to change a file that is "in use". So in the rare
#    situation this script needs to be updated, you will need to
#    run the svn update once yourself.

set -e

## Only run this script on Monday night (first day of the week)
if [ `date +%u` != 1 ] ; then exit ; fi

function qpushd() { pushd "$@" >/dev/null; }
function qpopd() { popd >/dev/null; }
function unix_path() { echo "$*" | sed 's,^\([A-Za-z]\):,/\1,;s,\\,/,g'; }

qpushd "$(dirname $(unix_path "$0"))"
svn update
./build_package.sh
qpopd
