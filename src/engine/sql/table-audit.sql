--
-- FILE:
-- table-audit.sql
--
-- FUNCTION:
-- Define the audit trail tables 
-- Note that these tables must be kept manually in sync with those
-- in table-create.sql
--
-- HISTORY:
-- Copyright (C) 2000, 2001 Linas Vepstas
--

-- audit trail tables
-- The change field may be 'a' -- add, 'd' -- delete/drop, 'm' -- modify
-- The objtype field may be 'a' -- account, 'c' -- commodity, 'e' -- entry,
--                          'k' -- kvp, 'p' -- price, 't' -- transaction

CREATE TABLE gncAuditTrail (
	sessionGuid		CHAR(32)  NOT NULL,   -- who changed it
	date_changed 		DATETIME,   -- when they changed it
        change			CHAR NOT NULL,
        objtype			CHAR NOT NULL
);

-- would love to inherit, but can't because this wrecks the primary key

CREATE TABLE gncAccountTrail (
	accountGuid	CHAR(32) NOT NULL,  -- override, not a primary key anymore
	parentGuid	CHAR(32) NOT NULL,
	accountName 	TEXT NOT NULL CHECK (accountName <> ''),
	accountCode 	TEXT,
	description 	TEXT,
	type		TEXT NOT NULL,
	commodity	TEXT NOT NULL CHECK (commodity <>''),
	version		INT4 NOT NULL,
	iguid		INT4 DEFAULT 0
) INHERITS (gncAuditTrail);

CREATE INDEX gncAccountTrail_account_idx ON gncAccountTrail (accountGuid);

CREATE TABLE gncCommodityTrail (
        commodity	TEXT NOT NULL,  -- override, not a primary key anymore
	fullname	TEXT,
	namespace	TEXT NOT NULL,
	mnemonic	TEXT NOT NULL,
	code		TEXT,
	fraction	INT DEFAULT '100'
) INHERITS (gncAuditTrail);

CREATE INDEX gncCommodityTrail_commodity_idx ON gncCommodityTrail (commodity);

CREATE TABLE gncEntryTrail (
	entryGuid		CHAR(32) NOT NULL,  -- override, not a primary key anymore
	accountGuid		CHAR(32) NOT NULL,
	transGuid		CHAR(32) NOT NULL,
	memo			TEXT,
	action			TEXT,
	reconciled		CHAR DEFAULT 'n',
	date_reconciled 	DATETIME,
	amount			INT8 DEFAULT '0',
	value			INT8 DEFAULT '0',
	iguid			INT4 DEFAULT 0
) INHERITS (gncAuditTrail);

CREATE INDEX gncEntryTrail_entry_idx ON gncEntryTrail (entryGuid);

CREATE TABLE gncPriceTrail (
	priceGuid	CHAR(32) NOT NULL,  -- override, not a primary key anymore
	commodity	TEXT NOT NULL CHECK (commodity <>''),
	currency	TEXT NOT NULL CHECK (commodity <>''),
	time		DATETIME,
	source		TEXT,
	type		TEXT,
	valueNum	INT8 DEFAULT '0',
	valueDenom	INT4 DEFAULT '100',
	version		INT4 NOT NULL
) INHERITS (gncAuditTrail);

CREATE INDEX gncPriceTrail_price_idx ON gncPriceTrail (priceGuid);

CREATE TABLE gncTransactionTrail (
	transGuid	CHAR(32) NOT NULL,  -- override, not a primary key anymore
	last_modified 	DATETIME DEFAULT 'NOW',
	date_entered 	DATETIME,
	date_posted 	DATETIME,
	num		TEXT,
	description	TEXT,
        currency	TEXT NOT NULL CHECK (currency <> ''),
	version		INT4 NOT NULL,
	iguid		INT4 DEFAULT 0
) INHERITS (gncAuditTrail);

CREATE INDEX gncTransactionTrail_trans_idx ON gncTransactionTrail (transGuid);

CREATE TABLE gncKVPvalueTrail (
	iguid		INT4,
	ipath		INT4,
	type		char(4)
) INHERITS (gncAuditTrail);

CREATE TABLE gncKVPvalue_int64Trail (
	iguid		INT4,
	ipath		INT4,
	type		char(4),
	data		INT8
) INHERITS (gncAuditTrail);

CREATE TABLE gncKVPvalue_dblTrail (
	iguid		INT4,
	ipath		INT4,
	type		char(4),
	data		FLOAT8
) INHERITS (gncAuditTrail);

CREATE TABLE gncKVPvalue_numericTrail (
	iguid		INT4,
	ipath		INT4,
	type		char(4),
	num		INT8,
	denom		INT8
) INHERITS (gncAuditTrail);

CREATE TABLE gncKVPvalue_strTrail (
	iguid		INT4,
	ipath		INT4,
	type		char(4),
	data		TEXT
) INHERITS (gncAuditTrail);

CREATE TABLE gncKVPvalue_guidTrail (
	iguid		INT4,
	ipath		INT4,
	type		char(4),
	data		CHAR(32)
) INHERITS (gncAuditTrail);

CREATE TABLE gncKVPvalue_listTrail (
	iguid		INT4,
	ipath		INT4,
	type		char(4),
	data		TEXT[]
) INHERITS (gncAuditTrail);

-- end of file
