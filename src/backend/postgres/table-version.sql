--
-- FILE:
-- table-version.sql
--
-- FUNCTION:
-- Insert the latest version information into the gncVersion table.
--
-- Inserting in the same query as creating the table does not
-- work under Postgres 7.0
--
-- HISTORY:
-- Copyright (C) 2001 Linux Developers Group
--

INSERT INTO gncVersion (major,minor,rev,name) VALUES (1,0,0,'Version Table');
INSERT INTO gncVersion (major,minor,rev,name) VALUES (1,1,1,'iGUID in Main Tables');
INSERT INTO gncVersion (major,minor,rev,name) VALUES (1,2,1,'Fix gncSubtotalReconedBalance');
INSERT INTO gncVersion (major,minor,rev,name) VALUES (1,3,1,'Add kvp_timespec tables');
-- not yet -- INSERT INTO gncVersion (major,minor,rev,name) VALUES (1,4,1,'Add support for multiple books');
