
-- these tables roughly mirror the c structs in 
-- TransactionP.h,  AccountP.h and GroupP.h

-- each child of a group will have its own record.
DROP TABLE gncGroup;
CREATE TABLE gncGroup (
	groupGuid	CHAR(32),
	parentGuid	CHAR(32),
	childGuid	CHAR(32)
);

-- hack alert -- docref ??

DROP TABLE gncAccount;
CREATE TABLE gncAccount (
	accountGuid	CHAR(32) PRIMARY KEY,
	parentGuid	CHAR(32),
	childrenGuid	CHAR(32),
	accountName 	VARCHAR(40) DEFAULT 'xoxo',
	accountCode 	VARCHAR(8),
	description 	VARCHAR(120),
	notes	 	VARCHAR(120),
	type		INT2,
	currency	VARCHAR(8),
	security	VARCHAR(8)
);

-- hack alert -- docref ??

DROP TABLE gncTransaction;
CREATE TABLE gncTransaction (
	transGuid		CHAR(32) PRIMARY KEY,
	date_entered	 	DATETIME,
	date_posted	 	DATETIME,
	num			VARCHAR(8),
	description		VARCHAR(32)
);

-- a gncEntry is what we call 'Split' elsewhere in the engine

DROP TABLE gncEntry;
CREATE TABLE gncEntry (
	entryGuid		CHAR(32) PRIMARY KEY,
	accountGuid		CHAR(32),
	transGuid		CHAR(32),
	memo			VARCHAR(20),
	action			VARCHAR(20),
	reconciled		CHAR,
	date_reconciled 	DATETIME,
	amount			FLOAT8 DEFAULT '0.0',
	share_price		FLOAT8 DEFAULT '0.0'
);

