#!/bin/bash

REV=$1
: ${ORIGBRANCH=trunk}
: ${WHOAMI=`whoami`}

TMPFILE=tmplog.tmp
#TMPFILE2=tmplog2.tmp

# Cherry-pick the other commit
ORIGID=`git svn find-rev r${REV} ${ORIGBRANCH}`
if [ -z "${ORIGID}" ] ; then
    echo "Revision ${REV} not found in branch ${ORIGBRANCH}"
    exit 1
fi
git cherry-pick ${ORIGID}

# Create new log message by modifying the old one
git log --pretty=format:"[$REV] %s%n%n%b" HEAD^..HEAD \
    | grep -v '^BP$' | grep -v 'git-svn-id:' > ${TMPFILE}
OTHERAUTHOR=`git log --pretty=format:"%an" HEAD^..HEAD`
if [ "${WHOAMI}" != "${OTHERAUTHOR}" ]; then
    echo -e "\nOriginal commit by ${OTHERAUTHOR}." >> ${TMPFILE}
fi

# Commit new log message
git commit --amend -F ${TMPFILE}

# Clean up temporary files
rm -f ${TMPFILE}
