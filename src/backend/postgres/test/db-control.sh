#!/bin/sh

EXIT_VALUE=0

DB=$PWD/gnc_test

case $1 in
  create)
    rm -rf $DB
    initdb $DB || EXIT_VALUE=1
    ;;
  destroy)
    rm -rf $DB
    ;;
  start)
    pg_ctl -D $DB -o "-p 7777" start
    ;;
  stop)
    pg_ctl -D $DB -o "-p 7777" stop
    ;;
  *)
    echo "Bad command: $1"
    ;;
esac

exit $EXIT_VALUE
