#!/bin/sh

#exit 0

EXIT_VALUE=0

rm -f test_file_*
./db-control.sh create

./db-control.sh start
./test-db || EXIT_VALUE=1
./db-control.sh stop

if test $EXIT_VALUE != 0; then exit $EXIT_VALUE; fi

./db-control.sh destroy

exit $EXIT_VALUE
