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
-- If these tables are changed or added to, a correspionding
-- audit-trail table (in table-audit.sql) must be updated as well.
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

CREATE TABLE gncVersion (
	major	INT NOT NULL,
	minor	INT NOT NULL,
	rev	INT DEFAULT '0',
	name	TEXT UNIQUE NOT NULL CHECK (name <> ''),
	date	DATETIME DEFAULT 'NOW'
);

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

CREATE TABLE gncBook (
	bookGuid	CHAR(32) PRIMARY KEY,
	book_open	CHAR DEFAULT 'n',
	version		INT4 NOT NULL,
	iguid		INT4 DEFAULT 0
);

-- Account structure -- parentGUID points to parent account
-- guid. There is no supports for Groups in this schema.
-- (there seems to be no strong need to have groups in the DB.)

CREATE TABLE gncAccount (
	accountGuid	CHAR(32) PRIMARY KEY,
	parentGuid	CHAR(32) NOT NULL,
	bookGuid	CHAR(32) NOT NULL,
	accountName 	TEXT NOT NULL CHECK (accountName <> ''),
	accountCode 	TEXT,
	description 	TEXT,
	type		TEXT NOT NULL,
	commodity	TEXT NOT NULL CHECK (commodity <>''),
	version		INT4 NOT NULL,
	iguid		INT4 DEFAULT 0
);

-- CREATE INDEX gncAccount_pg_idx ON gncAccount (parentGuid);

CREATE TABLE gncTransaction (
	transGuid	CHAR(32) PRIMARY KEY,
	last_modified 	DATETIME DEFAULT 'NOW',
	date_entered 	DATETIME,
	date_posted 	DATETIME,
	num		TEXT,
	description	TEXT,
        currency	TEXT NOT NULL CHECK (currency <> ''),
	version		INT4 NOT NULL,
	iguid		INT4 DEFAULT 0
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
	value			INT8 DEFAULT '0',
	iguid			INT4 DEFAULT 0
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
	valueDenom	INT4 DEFAULT '100',
	version		INT4 NOT NULL,
	bookGuid	CHAR(32) NOT NULL
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

CREATE SEQUENCE gnc_iguid_seq START 1;

CREATE TABLE gncKVPvalue (
	iguid		INT4,
	ipath		INT4,
	type		char(4),

        PRIMARY KEY (iguid, ipath)
);

--  CREATE INDEX gncKVPvalue_iguid_idx ON gncKVPvalue (iguid);

-- Add primary keys to each kvp table ... because key inheritance 
-- is ambiguously defined and thus not implemented in postgres.
-- Note, however, adding these keys degrades performance by 20%
-- (even after a vacuum analyze), and adding indexes degrades
-- an additional 15% !!  I find this result surprising, so I
-- simply leave these commented out ... (as of postgres 7.1.2)
-- Note, indexex on the main, non-inherited tables *are* important
-- for ensuring good performance, so this effect seems to be related
-- to inheritance

CREATE TABLE gncKVPvalue_int64 (
	data		INT8
--        PRIMARY KEY (iguid, ipath)
) INHERITS (gncKVPvalue);

-- CREATE INDEX gncKVPvalue_int64_iguid_idx ON gncKVPvalue_int64 (iguid);

CREATE TABLE gncKVPvalue_dbl (
	data		FLOAT8
--        PRIMARY KEY (iguid, ipath)
) INHERITS (gncKVPvalue);

-- CREATE INDEX gncKVPvalue_dbl_iguid_idx ON gncKVPvalue_dbl (iguid);

CREATE TABLE gncKVPvalue_numeric (
	num		INT8,
	denom		INT8
--        PRIMARY KEY (iguid, ipath)
) INHERITS (gncKVPvalue);

-- CREATE INDEX gncKVPvalue_numeric_iguid_idx ON gncKVPvalue_numeric (iguid);

CREATE TABLE gncKVPvalue_str (
	data		TEXT
--        PRIMARY KEY (iguid, ipath)
) INHERITS (gncKVPvalue);

-- CREATE INDEX gncKVPvalue_str_iguid_idx ON gncKVPvalue_str (iguid);

CREATE TABLE gncKVPvalue_guid (
	data		CHAR(32)
--        PRIMARY KEY (iguid, ipath)
) INHERITS (gncKVPvalue);

-- CREATE INDEX gncKVPvalue_guid_iguid_idx ON gncKVPvalue_guid (iguid);

CREATE TABLE gncKVPvalue_timespec (
	data		DATETIME
--        PRIMARY KEY (iguid, ipath)
) INHERITS (gncKVPvalue);

-- CREATE INDEX gncKVPvalue_timespec_iguid_idx ON gncKVPvalue_timespec (iguid);

CREATE TABLE gncKVPvalue_list (
	data		TEXT[]
--        PRIMARY KEY (iguid, ipath)
) INHERITS (gncKVPvalue);

-- CREATE INDEX gncKVPvalue_list_iguid_idx ON gncKVPvalue_list (iguid);

-- end of file
