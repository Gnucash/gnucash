#! /bin/bash
#
# svnlog2ul.sh <prevrelease> <newrelease>
#
# This script will extract all the svn commit messages from the
# repository between two releases or from a previous release and the
# current checkout's HEAD.
# The result will be printed on standard out as a
# html unordered list ("bulleted list").
#
# Parameters:
#
#    <prevrelease> : the svn tag for the release to start
#                    the commit message search
#    <newrelease>  : the svn tag for the release to end
#                    the commit message search
#
# The search will return all commit messages between
# <prevrelease> and <newrelease>
#
# Examples:
# This will compile the changes between two tagged releases:
#    svnlog2ul.sh 2.3.7 2.3.8
# This will compile the changes between the tagged release and the
# current HEAD in checked out working copy:
#    svnlog2ul.sh 2.4.7

oldrelease=$1
newrelease=$2

dir=`dirname "$0"`

oldrev=$(svn info --xml http://svn.gnucash.org/repo/gnucash/tags/$oldrelease | xsltproc "$dir/getlastcommit.xslt" -)
if [ "x$2" == "x" ]
then
  newrev=$(svn info -r HEAD --xml | xsltproc "$dir/getlastcommit.xslt" -)
  svn log -r$newrev:$oldrev --xml | xsltproc "$dir/log2ul.xslt" -
else
  newrev=$(svn info --xml http://svn.gnucash.org/repo/gnucash/tags/$newrelease | xsltproc "$dir/getlastcommit.xslt" -)
  svn log -r$newrev:$oldrev --xml http://svn.gnucash.org/repo/gnucash/tags/$newrelease | xsltproc "$dir/log2ul.xslt" -
fi

