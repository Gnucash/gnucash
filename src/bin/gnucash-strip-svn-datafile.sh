#!/bin/sh

# This script strips data elements from a gnucash data file that have
# been introduced in the SVN-trunk version of gnucash, but are not
# backwards-compatible to older versions of gnucash.

ORIGFILE=$1
ORIGTMPFILE="${ORIGFILE}.gunzip"
BKUPFILE="${ORIGFILE}.svn.gz"
TMPFILE="${ORIGFILE}.tmp"

if [ -f ${ORIGFILE} ] ; then

    # Detect compression
    in_gzip_format=yes
    gzip -ql ${ORIGFILE} > /dev/null 2> /dev/null || in_gzip_format=no

    # Uncompress the file
    if [ "$in_gzip_format" = "yes" ] ; then
	gunzip -cd ${ORIGFILE} > ${ORIGTMPFILE}
    else
	cat ${ORIGFILE} > ${ORIGTMPFILE}
    fi

    # Remove the elements that are not backwards-compatible
    grep -v '<sx:enabled>.</sx:enabled>' ${ORIGTMPFILE} > ${TMPFILE}

    # Print result of element removal
    echo "Removed the following elements:"
    diff -u ${ORIGTMPFILE} ${TMPFILE}
    echo "Keeping old data file as \"${BKUPFILE}\""

    # Compress the result again
    if [ "$in_gzip_format" = "yes" ] ; then
	gzip -c ${ORIGTMPFILE} > ${BKUPFILE}
	gzip -c ${TMPFILE} > ${ORIGFILE}
    else
	cat ${ORIGTMPFILE} > ${BKUPFILE}
	cat ${TMPFILE} > ${ORIGFILE}
    fi

    # Remove temporary files
    rm ${ORIGTMPFILE}
    rm ${TMPFILE}

else
    echo "$0: File $1 not found."
fi
