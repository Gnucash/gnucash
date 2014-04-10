#! /bin/sh 

# create teh initial database
createdb "gnc_bogus"
psql gnc_bogus < gnc-init.sql
