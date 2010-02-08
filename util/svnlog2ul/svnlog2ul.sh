#! /bin/bash
#
# svnlog2ul.sh <prevrelease> <newrelease>
#
# This script will extract all the svn commit messages
# from the repository between two releases.
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
# Example:
# 
#    svnlog2ul.sh 2.3.7 2.3.8

oldrelease=$1
newrelease=$2

oldrev=$(svn info --xml http://svn.gnucash.org/repo/gnucash/tags/$oldrelease | xsltproc getlastcommit.xslt -)
newrev=$(svn info --xml http://svn.gnucash.org/repo/gnucash/tags/$newrelease | xsltproc getlastcommit.xslt -)

svn log -r$newrev:$oldrev --xml http://svn.gnucash.org/repo/gnucash/tags/$newrelease | xsltproc log2ul.xslt -