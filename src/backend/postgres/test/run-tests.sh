#!/bin/sh

exit 0

EXIT_VALUE=0

./db-control.sh create
./db-control.sh start
./test-db || EXIT_VALUE=1
./db-control.sh stop
#./db-control.sh destroy

exit $EXIT_VALUE
