#!/bin/sh

EXIT_VALUE=0

DB=$PWD/gnc_test

PG_CTL="pg_ctl -D $DB -o -p7777"

case $1 in
  create)
    rm -rf $DB
    initdb $DB || EXIT_VALUE=1
    ;;
  destroy)
    rm -rf $DB
    ;;
  start)
    $PG_CTL start
    ;;
  stop)
    $PG_CTL stop
    ;;
  connect)
    $PG_CTL status | grep "not running" && $PG_CTL start && sleep 1
    psql -p 7777 gnc_test
    ;;
  *)
    echo "Bad command: $1"
    ;;
esac

exit $EXIT_VALUE
