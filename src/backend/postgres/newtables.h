#ifndef __GNC_NEWTABLES_H__
#define __GNC_NEWTABLES_H__

/*
 
-- Drop indexes on the tables
-- Drop functions on the tables
-- Alter tables to rename them
-- Create new tables
-- Select into new tables from old tables
-- Create indexes on the tables
-- Create functions
-- Insert row into gncVersion
-- Commit work
-- Drop renamed tables
-- Vacuum Full Analyze the database

*/

const char *lock_entry =
"LOCK TABLE gncEntry IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncEntryTrail IN ACCESS EXCLUSIVE MODE;\n"
;

const char *lock_split =
"LOCK TABLE gncSplit IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncSplitTrail IN ACCESS EXCLUSIVE MODE;\n"
;

const char *lock_tables = 
"LOCK TABLE gncVersion IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncTransaction IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncCheckpoint IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncPrice IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncSession IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalue_timespec IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncAccountTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncBookTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncCommodityTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalueTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalue_int64Trail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalue_dblTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalue_numericTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalue_strTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalue_guidTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalue_listTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncPriceTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncTransactionTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncKVPvalue_timespecTrail IN ACCESS EXCLUSIVE MODE;\n"
"LOCK TABLE gncAuditTrail IN ACCESS EXCLUSIVE MODE;\n"
;

const char *drop_index =
"ALTER TABLE gncVersion DROP CONSTRAINT gncversion_name_key;\n"
"ALTER TABLE gncTransaction DROP CONSTRAINT gnctransaction_pkey;\n"
"ALTER TABLE gncEntry DROP CONSTRAINT gncEntry_pkey;\n"
"ALTER TABLE gncCheckpoint DROP CONSTRAINT gncCheckpoint_pkey;\n"
"ALTER TABLE gncPrice DROP CONSTRAINT gncPrice_pkey;\n"
"ALTER TABLE gncSession DROP CONSTRAINT gncSession_pkey;\n"
"DROP INDEX gncTransaction_posted_idx;\n"
"DROP INDEX gncEntry_trn_idx;\n"
"DROP INDEX gncAccountTrail_account_idx;\n"
"DROP INDEX gncBookTrail_book_idx;\n"
"DROP INDEX gncCommodityTrail_commodity_idx;\n"
"DROP INDEX gncEntryTrail_entry_idx;\n"
"DROP INDEX gncPriceTrail_price_idx;\n"
"DROP INDEX gncTransactionTrail_trans_idx;\n"
;

const char *drop_functions = 
"DROP FUNCTION gncHelperPrVal(gncEntry);\n"
"DROP FUNCTION gncHelperPrAmt(gncEntry);\n"
"DROP FUNCTION gncSubtotalBalance(CHAR(32), TIMESTAMP, TIMESTAMP);\n"
"DROP FUNCTION gncSubtotalClearedBalance (CHAR(32), TIMESTAMP, TIMESTAMP);\n"
"DROP FUNCTION gncSubtotalReconedBalance(CHAR(32), TIMESTAMP, TIMESTAMP);\n"
;

const char *alter_tables =
"ALTER TABLE gncVersion RENAME TO gncVersion_old;\n"
"ALTER TABLE gncTransaction RENAME TO gncTransaction_old;\n"
"ALTER TABLE gncEntry RENAME TO gncSplit_old;\n"
"ALTER TABLE gncCheckpoint RENAME TO gncCheckpoint_old;\n"
"ALTER TABLE gncPrice RENAME TO gncPrice_old;\n"
"ALTER TABLE gncSession RENAME TO gncSession_old;\n"
"ALTER TABLE gncKVPvalue_timespec RENAME TO gncKVPvalue_timespec_old;\n"
"ALTER TABLE gncAuditTrail RENAME to gncAuditTrail_old;\n"
"ALTER TABLE gncAccountTrail RENAME to gncAccountTrail_old;\n"
"ALTER TABLE gncBookTrail RENAME to gncBookTrail_old;\n"
"ALTER TABLE gncCommodityTrail RENAME to gncCommodityTrail_old;\n"
"ALTER TABLE gncKVPvalueTrail RENAME to gncKVPvalueTrail_old;\n"
"ALTER TABLE gncKVPvalue_int64Trail RENAME TO gncKVPvalue_int64Trail_old;\n"
"ALTER TABLE gncKVPvalue_dblTrail RENAME TO gncKVPvalue_dblTrail_old;\n"
"ALTER TABLE gncKVPvalue_numericTrail RENAME TO gncKVPvalue_numericTrail_old;\n"
"ALTER TABLE gncKVPvalue_strTrail RENAME TO gncKVPvalue_strTrail_old;\n"
"ALTER TABLE gncKVPvalue_guidTrail RENAME TO gncKVPvalue_guidTrail_old;\n"
"ALTER TABLE gncKVPvalue_listTrail RENAME TO gncKVPvalue_listTrail_old;\n"
"ALTER TABLE gncEntryTrail RENAME to gncEntryTrail_old;\n"
"ALTER TABLE gncPriceTrail RENAME to gncPriceTrail_old;\n"
"ALTER TABLE gncTransactionTrail RENAME to gncTransactionTrail_old;\n"
"ALTER TABLE gncKVPvalue_timespecTrail RENAME to gncKVPvalue_timespecTrail_old;\n"
;

const char *create_new_tables =
"CREATE TABLE gncVersion ( \n"
"	major	INT NOT NULL, \n"
"	minor	INT NOT NULL, \n"
"	rev	    INT DEFAULT '0', \n"
"	name	TEXT UNIQUE NOT NULL CHECK (name <> ''), \n"
"	date	TIMESTAMP WITH TIME ZONE DEFAULT 'NOW' \n"
"); \n"
"--\n"
"CREATE TABLE gncTransaction ( \n"
"	transGuid	CHAR(32) PRIMARY KEY, \n"
"	last_modified 	TIMESTAMP WITH TIME ZONE DEFAULT 'NOW', \n"
"	date_entered 	TIMESTAMP WITH TIME ZONE, \n"
"	date_posted 	TIMESTAMP WITH TIME ZONE, \n"
"	num             TEXT, \n"
"	description	    TEXT, \n"
"   currency	    TEXT NOT NULL CHECK (currency <> ''), \n"
"	version		    INT4 NOT NULL, \n"
"	iguid		    INT4 DEFAULT 0 \n"
"); \n"
"-- \n"
"CREATE TABLE gncSplit ( \n"
"	splitGuid		CHAR(32) PRIMARY KEY, \n"
"	accountGuid		CHAR(32) NOT NULL, \n"
"	transGuid		CHAR(32) NOT NULL, \n"
"	memo			TEXT, \n"
"	action			TEXT, \n"
"	reconciled		CHAR DEFAULT 'n', \n"
"	date_reconciled TIMESTAMP WITH TIME ZONE, \n"
"	amount			INT8 DEFAULT '0', \n"
"	value			INT8 DEFAULT '0', \n"
"	iguid			INT4 DEFAULT 0 \n"
"); \n"
"-- \n"
"CREATE TABLE gncCheckpoint ( \n"
"   accountGuid         CHAR(32) NOT NULL, \n"
"	date_start	 	    TIMESTAMP WITH TIME ZONE NOT NULL, \n"
" 	date_end	 	    TIMESTAMP WITH TIME ZONE NOT NULL, \n"
"	commodity		    TEXT NOT NULL CHECK (commodity <>''), \n"
"	type			    TEXT DEFAULT 'simple', \n"
"	balance			    INT8 DEFAULT '0', \n"
"	cleared_balance		INT8 DEFAULT '0', \n"
"	reconciled_balance	INT8 DEFAULT '0', \n"
"   PRIMARY KEY (accountGuid, date_start, commodity) \n"
"); \n"
"-- \n"
"CREATE TABLE gncPrice ( \n"
"	priceGuid	CHAR(32) PRIMARY KEY, \n"
"	commodity	TEXT NOT NULL CHECK (commodity <>''), \n"
"	currency	TEXT NOT NULL CHECK (commodity <>''), \n"
"	time		TIMESTAMP WITH TIME ZONE, \n"
"	source		TEXT, \n"
"	type		TEXT, \n"
"	valueNum	INT8 DEFAULT '0', \n"
"	valueDenom	INT4 DEFAULT '100', \n"
"	version		INT4 NOT NULL, \n"
"	bookGuid	CHAR(32) NOT NULL \n"
"); \n"
"-- \n"
"CREATE TABLE gncSession ( \n"
"	sessionGuid		CHAR(32) PRIMARY KEY, \n"
"	session_mode	CHAR(16) NOT NULL, \n"
"	hostname		TEXT, \n"
"	login_name		TEXT, \n"
"	gecos			TEXT, \n"
"	time_on			TIMESTAMP WITH TIME ZONE NOT NULL, \n"
"	time_off		TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT 'INFINITY' \n"
"); \n"
"--\n"
"CREATE TABLE gncKVPvalue_timespec ( \n"
"	data		TIMESTAMP WITH TIME ZONE \n"
") INHERITS (gncKVPvalue); \n"
"--\n"
;

const char *create_audits =
#include "table-audit.c"
;

const char *create_indexes =
"CREATE INDEX gncTransaction_posted_idx ON gncTransaction (date_posted); \n"
"CREATE INDEX gncSplit_acc_idx ON gncSplit (accountGuid); \n"
"CREATE INDEX gncSplit_trn_idx ON gncSplit (transGuid); \n"
"--\n"
;

const char *create_functions = 
#include "functions.c"
;

const char *insert_new_data = 
"INSERT INTO gncVersion select * from gncVersion_old;\n"
"INSERT INTO gncTransaction select * from gncTransaction_old;\n"
"INSERT INTO gncSplit select * from gncSplit_old;\n"
"INSERT INTO gncCheckpoint select * from gncCheckpoint_old;\n"
"INSERT INTO gncPrice select * from gncPrice_old;\n"
"INSERT INTO gncSession select * from gncSession_old;\n"
"INSERT INTO gncKVPvalue_timespec select * from gncKVPvalue_timespec_old;\n"
"INSERT INTO gncAuditTrail SELECT * FROM gncAuditTrail_old;\n"
"INSERT INTO gncAccountTrail SELECT * FROM gncAccountTrail_old;\n"
"INSERT INTO gncBookTrail SELECT * FROM gncBookTrail_old;\n"
"INSERT INTO gncCommodityTrail SELECT * FROM gncCommodityTrail_old;\n"
"INSERT INTO gncKVPvalueTrail SELECT * FROM gncKVPvalueTrail_old;\n"
"INSERT INTO gncKVPvalue_int64Trail SELECT * FROM gncKVPvalue_int64Trail_old;\n"
"INSERT INTO gncKVPvalue_dblTrail SELECT * FROM gncKVPvalue_dblTrail_old;\n"
"INSERT INTO gncKVPvalue_numericTrail SELECT * FROM gncKVPvalue_numericTrail_old;\n"
"INSERT INTO gncKVPvalue_strTrail SELECT * FROM gncKVPvalue_strTrail_old;\n"
"INSERT INTO gncKVPvalue_guidTrail SELECT * FROM gncKVPvalue_guidTrail_old;\n"
"INSERT INTO gncKVPvalue_listTrail SELECT * FROM gncKVPvalue_listTrail_old;\n"
"INSERT INTO gncSplitTrail SELECT * FROM gncEntryTrail_old;\n"
"INSERT INTO gncPriceTrail SELECT * FROM gncPriceTrail_old;\n"
"INSERT INTO gncTransactionTrail SELECT * FROM gncTransactionTrail_old;\n"
"INSERT INTO gncKVPvalue_timespecTrail SELECT * FROM gncKVPvalue_timespecTrail_old;\n"
;

const char *drop_old_tables = 
"DROP TABLE gncVersion_old;\n"
"DROP TABLE gncTransaction_old;\n"
"DROP TABLE gncSplit_old;\n"
"DROP TABLE gncCheckpoint_old;\n"
"DROP TABLE gncPrice_old;\n"
"DROP TABLE gncSession_old;\n"
"DROP TABLE gncKVPvalue_timespec_old;\n"
"DROP TABLE gncAccountTrail_old;\n"
"DROP TABLE gncBookTrail_old;\n"
"DROP TABLE gncCommodityTrail_old;\n"
"DROP TABLE gncKVPvalueTrail_old;\n"
"DROP TABLE gncKVPvalue_int64Trail_old;\n"
"DROP TABLE gncKVPvalue_dblTrail_old;\n"
"DROP TABLE gncKVPvalue_numericTrail_old;\n"
"DROP TABLE gncKVPvalue_strTrail_old;\n"
"DROP TABLE gncKVPvalue_guidTrail_old;\n"
"DROP TABLE gncKVPvalue_listTrail_old;\n"
"DROP TABLE gncEntryTrail_old;\n"
"DROP TABLE gncPriceTrail_old;\n"
"DROP TABLE gncTransactionTrail_old;\n"
"DROP TABLE gncKVPvalue_timespecTrail_old;\n"
"DROP TABLE gncAuditTrail_old;\n"
;

const char *version_sql =
"INSERT INTO gncVersion (major, minor, rev, name, date) "
"VALUES (1, 5, 0, 'Change timestamp definition', now());\n"
;

#endif
