--
-- FILE:
-- table-create.sql
--
-- FUNCTION:
-- Define the tables needed to initialize a new GnuCash database
--
-- These tables roughly mirror the c structs in 
-- TransactionP.h,  AccountP.h, gnc-commodity.c
-- Please refer to the C files to get the right level of documentation.
--
-- These tables are specifically designed for the 
-- postgres database server, but are hopefull relatively portable.
--
-- These tables are hand-built, but maybe they should be 
-- auto-built with the m4 macros ...
--
-- HISTORY:
-- Copyright (C) 2000, 2001 Linas Vepstas
--

-- Commodity structure
-- Store currency, security types.  Namespace includes
-- ISO4217 for currencies, NASDAQ, AMEX, NYSE, EUREX for 
-- stocks.   See the C documentation for details.

CREATE TABLE gncCommodity (
        commodity	TEXT PRIMARY KEY,
	fullname	TEXT,
	namespace	TEXT NOT NULL,
	mnemonic	TEXT NOT NULL,
	code		TEXT,
	fraction	INT DEFAULT '100'
);

-- Account structure -- parentGUID points to parent account
-- guid. There is no supports for Groups in this schema.
-- (there seems to be no strong need to have groups in the DB.)

CREATE TABLE gncAccount (
	accountGuid	CHAR(32) PRIMARY KEY,
	parentGuid	CHAR(32) NOT NULL,
	accountName 	TEXT NOT NULL CHECK (accountName <> ''),
	accountCode 	TEXT,
	description 	TEXT,
	notes	 	TEXT,
	type		TEXT NOT NULL,
	commodity	TEXT NOT NULL CHECK (commodity <>''),
	version		INT4 NOT NULL
);

-- CREATE INDEX gncAccount_pg_idx ON gncAccount (parentGuid);
-- CREATE INDEX gncAccount_ch_idx ON gncAccount (childrenGuid);

CREATE TABLE gncTransaction (
	transGuid	CHAR(32) PRIMARY KEY,
	last_modified 	DATETIME DEFAULT 'NOW',
	date_entered 	DATETIME,
	date_posted 	DATETIME,
	num		TEXT,
	description	TEXT,
        currency	TEXT NOT NULL CHECK (currency <> ''),
	version		INT4 NOT NULL
);

CREATE INDEX gncTransaction_posted_idx ON gncTransaction (date_posted);

-- a gncEntry is what we call 'Split' elsewhere in the engine
-- Here, we call it a 'journal entry'

CREATE TABLE gncEntry (
	entryGuid		CHAR(32) PRIMARY KEY,
	accountGuid		CHAR(32) NOT NULL,
	transGuid		CHAR(32) NOT NULL,
	memo			TEXT,
	action			TEXT,
	reconciled		CHAR DEFAULT 'n',
	date_reconciled 	DATETIME,
	amount			INT8 DEFAULT '0',
	value			INT8 DEFAULT '0'
);

CREATE INDEX gncEntry_acc_idx ON gncEntry (accountGuid);
CREATE INDEX gncEntry_trn_idx ON gncEntry (transGuid);

-- The checkpoint table provides balance information
-- The balance is provided in the indicated currency; 
-- this allows the potential of maintaining balance information
-- in multiple currencies.  
-- (e.g. report stock account balances in shares of stock, 
-- and in dollars)
-- the 'type' field indicates what type of balance this is
-- (simple, FIFO, LIFO, or other accounting method)

CREATE TABLE gncCheckpoint (
	accountGuid		CHAR(32) NOT NULL,
	date_start	 	DATETIME NOT NULL,
 	date_end	 	DATETIME NOT NULL,
	commodity		TEXT NOT NULL CHECK (commodity <>''),
	type			TEXT DEFAULT 'simple',
	balance			INT8 DEFAULT '0',
	cleared_balance		INT8 DEFAULT '0',
	reconciled_balance	INT8 DEFAULT '0',

        PRIMARY KEY (accountGuid, date_start, commodity)
);

-- The price table stores the price of 'commodity' valued
-- in units of 'currency'
CREATE TABLE gncPrice (
	priceGuid	CHAR(32) PRIMARY KEY,
	commodity	TEXT NOT NULL CHECK (commodity <>''),
	currency	TEXT NOT NULL CHECK (commodity <>''),
	time		DATETIME,
	source		TEXT,
	type		TEXT,
	valueNum	INT8 DEFAULT '0',
	valueDenom	INT4 DEFAULT '100'
);


-- The session directory serves several purposes.  First and formost,
-- it notes the database access type.  There are three modes:
--  o 'Single User' -- Only one user can have access to the database
--                     at a time. 
--  o 'Multi-User Polled' -- multiple users
--  o 'Muilti-User Event Driven'
--  See Design.txt for more info.
-- Note that a client can lie about its identity, sign-on time, etc.
-- so these records aren't really sufficient for a true audit.

CREATE TABLE gncSession (
	sessionGuid		CHAR(32) PRIMARY KEY,
	session_mode		CHAR(16) NOT NULL,
	hostname		TEXT,
	login_name		TEXT,
	gecos			TEXT,
	time_on			DATETIME NOT NULL,
	time_off		DATETIME NOT NULL DEFAULT 'INFINITY'
);


-- The kvp path-cache replaces a long path name with a single unique
-- number.  The guid-cache replaces a 32-byte guid with a shorter 
-- 4-byte identifier.  The KVP Value table stores the actual values.

CREATE TABLE gncPathCache (
	ipath		SERIAL PRIMARY KEY,
	path		TEXT
);

CREATE TABLE gncGUIDCache (
	iguid		SERIAL PRIMARY KEY,
	guid		CHAR(32) UNIQUE NOT NULL
);

CREATE TABLE gncKVPvalue (
	iguid		INT4,
	ipath		INT4,
	type		char(4),

        PRIMARY KEY (iguid, ipath)
);

CREATE TABLE gncKVPvalue_int64 (
	data		INT8
) INHERITS (gncKVPvalue);

CREATE TABLE gncKVPvalue_dbl (
	data		FLOAT8
) INHERITS (gncKVPvalue);

CREATE TABLE gncKVPvalue_numeric (
	num		INT8,
	denom		INT8
) INHERITS (gncKVPvalue);

CREATE TABLE gncKVPvalue_str (
	data		TEXT
) INHERITS (gncKVPvalue);

CREATE TABLE gncKVPvalue_guid (
	data		CHAR(32)
) INHERITS (gncKVPvalue);

CREATE TABLE gncKVPvalue_list (
	data		TEXT[]
) INHERITS (gncKVPvalue);

-- utility functions to compute chackpoint balance subtotals

CREATE FUNCTION gncSubtotalBalance (CHAR(32), DATETIME, DATETIME)
    RETURNS NUMERIC
    AS 'SELECT sum(gncEntry.value)
        FROM gncEntry, gncTransaction
        WHERE
        gncEntry.accountGuid = $1 AND
        gncEntry.transGuid = gncTransaction.transGuid AND
        gncTransaction.date_posted BETWEEN $2 AND $3'
    LANGUAGE 'sql';

CREATE FUNCTION gncSubtotalClearedBalance (char(32), DATETIME, DATETIME)
    RETURNS NUMERIC
    AS 'SELECT sum(gncEntry.value)
        FROM gncEntry, gncTransaction
        WHERE
        gncEntry.accountGuid = $1 AND
        gncEntry.transGuid = gncTransaction.transGuid AND
        gncTransaction.date_posted BETWEEN $2 AND $3 AND
        gncEntry.reconciled <> \\'n\\''
    LANGUAGE 'sql';

CREATE FUNCTION gncSubtotalReconedBalance (CHAR(32), DATETIME, DATETIME)
    RETURNS NUMERIC
    AS 'SELECT sum(gncEntry.value)
        FROM gncEntry, gncTransaction
        WHERE
        gncEntry.accountGuid = $1 AND
        gncEntry.transGuid = gncTransaction.transGuid AND
        gncTransaction.date_posted BETWEEN $2 AND $3 AND
        gncEntry.reconciled = \\'y\\''
    LANGUAGE 'sql';

-- helper functions.  These intentionally use the 'wrong' fraction. 
-- This is because value_frac * amount * price = value * amount_frac

CREATE FUNCTION gncHelperPrVal (gncEntry)
   RETURNS INT8
   AS 'SELECT abs($1 . value * gncCommodity.fraction)
       FROM gncEntry, gncAccount, gncCommodity
       WHERE
       $1 . accountGuid = gncAccount.accountGuid AND
       gncAccount.commodity = gncCommodity.commodity'
    LANGUAGE 'sql';
       
CREATE FUNCTION gncHelperPrAmt (gncEntry)
   RETURNS INT8
   AS 'SELECT abs($1 . amount * gncCommodity.fraction)
       FROM gncEntry, gncTransaction, gncCommodity
       WHERE
       $1 . transGuid = gncTransaction.transGuid AND
       gncTransaction.currency = gncCommodity.commodity'
    LANGUAGE 'sql';
       

