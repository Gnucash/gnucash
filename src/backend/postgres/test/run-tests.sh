#!/bin/sh

EXIT_VALUE=0

# .libs/test-db  || EXIT_VALUE=1
# gdb .libs/test-db 

rm -f test_file_*
if ${srcdir}/db-control.sh create; then
    ${srcdir}/db-control.sh start
    ./test-db localhost 7777 || EXIT_VALUE=1
    ${srcdir}/db-control.sh stop
    ${srcdir}/db-control.sh destroy
elif [ "${PGHOST}X" != "X" ]; then
# This expects the logged in user to have authority
# to create databases.
    if [ "${PGPORT}X" = "X" ]; then
	export PGPORT=5432
    fi
    ./test-db $PGHOST $PGPORT || EXIT_VALUE=1
fi

if test $EXIT_VALUE != 0; then exit $EXIT_VALUE; fi


exit $EXIT_VALUE
