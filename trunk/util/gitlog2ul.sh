#! /bin/bash
#
# gitlog2ul.sh <prevrelease> <newrelease>
#
# This script will extract all the commit messages from the git
# repository between two releases or from a previous release and the
# current checkout's HEAD.
# The result will be printed on standard out as a
# html unordered list ("bulleted list").
#
# Parameters:
#
#    <prevrelease> : the git tag for the release to start
#                    the commit message search
#    <newrelease>  : the git tag for the release to end
#                    the commit message search
#
# The search will return all commit messages between
# <prevrelease> and <newrelease>
#
# Examples:
# This will compile the changes between two tagged releases:
#    gitlog2ul.sh 2.3.7 2.3.8
# This will compile the changes between the tagged release and the
# current HEAD in checked out working copy:
#    gitlog2ul.sh 2.4.7

oldrelease=$1
newrelease=$2

dir=`dirname "$0"`

# Print basic headers (to match what svn2log generates)
cat <<EOF
<?xml version="1.0"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
</head>
  <body>
    <ul>
EOF

if [ "x$2" == "x" ]
then
  newrelease=HEAD
fi

git --no-pager log --format="<li>%s%n<br/>%b</li>" $oldrelease..$newrelease | egrep -v "git-svn-id|^(<br/>)?BP$|^$"

cat <<EOF
  </ul>
</body>
EOF
