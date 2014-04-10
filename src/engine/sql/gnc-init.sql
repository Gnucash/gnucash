
-- these tables roughly mirror the c structs in 
-- TransactionP.h,  AccountP.h and GroupP.h

-- each child of a group will have its own record.
DROP TABLE gncGroup;
CREATE TABLE gncGroup (
	groupGuid	CHAR(32) PRIMARY KEY, 
	parentGuid	CHAR(32)
);

CREATE INDEX gncGroup_pg_idx ON gncGroup (parentGuid);

-- hack alert -- docref ??

DROP TABLE gncAccount;
CREATE TABLE gncAccount (
	accountGuid	CHAR(32) PRIMARY KEY,
	parentGuid	CHAR(32),
	childrenGuid	CHAR(32),
	accountName 	TEXT DEFAULT 'xoxo',
	accountCode 	TEXT,
	description 	TEXT,
	notes	 	TEXT,
	type		INT2,
	currency	TEXT,
	security	TEXT
);

CREATE INDEX gncAccount_pg_idx ON gncAccount (parentGuid);
CREATE INDEX gncAccount_ch_idx ON gncAccount (childrenGuid);

-- hack alert -- docref ??

DROP TABLE gncTransaction;
CREATE TABLE gncTransaction (
	transGuid		CHAR(32) PRIMARY KEY,
	date_entered	 	DATETIME,
	date_posted	 	DATETIME,
	num			TEXT,
	description		TEXT
);

-- a gncEntry is what we call 'Split' elsewhere in the engine

DROP TABLE gncEntry;
CREATE TABLE gncEntry (
	entryGuid		CHAR(32) PRIMARY KEY,
	accountGuid		CHAR(32),
	transGuid		CHAR(32),
	memo			TEXT,
	action			TEXT,
	reconciled		CHAR,
	date_reconciled 	DATETIME,
	amount			FLOAT8 DEFAULT '0.0',
	share_price		FLOAT8 DEFAULT '0.0'
);

CREATE INDEX gncEntry_acc_idx ON gncEntry (accountGuid);
CREATE INDEX gncEntry_trn_idx ON gncEntry (transGuid);

