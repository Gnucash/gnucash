#!/bin/bash

EXIT_VALUE=0

PATH=/usr/lib/postgresql/bin:$PATH

DB=$PWD/gnc_test_db
SOCKDIR=$PWD/gnc_test_db_sock
SOCKNUM=7777

# I couldn't get this to work -- the shell seems to think "'-k" is an
# argument after it finishes expanding ${PG_CTL}...
# PG_CTL="pg_ctl -D "${DB}" -o '-k ${SOCKDIR} -p ${SOCKNUM}'"

our_pg_ctl ()
{
  pg_ctl -D "${DB}" -o "-k ${SOCKDIR} -p ${SOCKNUM}" "$@";
}

case $1 in
  create)
    our_pg_ctl status | grep "pid" && our_pg_ctl stop && sleep 1
    rm -rf ${DB}
    rm -rf ${SOCKDIR}
    initdb ${DB} || EXIT_VALUE=1
    mkdir ${SOCKDIR} || EXIT_VALUE=1
    ;;
  destroy)
    our_pg_ctl status | grep "pid" && our_pg_ctl stop && sleep 1
    rm -rf ${DB}
    rm -rf ${SOCKDIR}
    ;;
  start)
    our_pg_ctl start
    ;;
  stop)
    pg_ctl -D ${DB} -o '-k ${SOCKDIR} -p 7777' stop
    ;;
  status)
    our_pg_ctl status
    ;;
  connect)
    our_pg_ctl status | grep "not running" && our_pg_ctl start && sleep 1
    psql -h ${SOCKDIR} -p ${SOCKNUM} $2
    ;;
  *)
    echo "Bad command: $1"
    ;;
esac

exit $EXIT_VALUE
