
-- these tables roughly mirror the c structs in TransactionP.h and
-- AccountP.h

CREATE TABLE gncAccount (
	accountGuid	CHAR(16) PRIMARY KEY,
	accountName 	VARCHAR(40) DEFAULT 'xoxo',
	accountCode 	VARCHAR(8),
	description 	VARCHAR(120),
	notes	 	VARCHAR(120),
	type		INT2,
	currency	VARCHAR(8),
	security	VARCHAR(8)
);


-- a gncEntry is what we call 'Split' elsewhere in the engine

CREATE TABLE gncEntry (
	entryGuid		CHAR(16) PRIMARY KEY,
	accountGuid		CHAR(16),
	transGuid		CHAR(16),
	memo			VARCHAR(20),
	action			VARCHAR(20),
	date_reconciled 	DATETIME,
	amount			FLOAT8 DEFAULT '0.0',
	share_price		FLOAT8 DEFAULT '0.0'
);

CREATE TABLE gncTransaction (
	transGuid		CHAR(16) PRIMARY KEY,
	date_entered	 	DATETIME,
	date_posted	 	DATETIME,
	num			VARCHAR(8),
	description		VARCHAR(32)
);


