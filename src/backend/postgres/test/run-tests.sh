#!/bin/sh

exit 0

EXIT_VALUE=0

./db-control.sh create

./db-control.sh start
./test-db || EXIT_VALUE=1
./db-control.sh stop

if test $EXIT_VALUE != 0; then exit $EXIT_VALUE; fi

diff -u test-file-1 test-file-2 || EXIT_VALUE=1

if test $EXIT_VALUE != 0; then exit $EXIT_VALUE; fi

./db-control.sh destroy
rm -f test-file-*

exit $EXIT_VALUE
