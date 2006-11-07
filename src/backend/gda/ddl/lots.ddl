-- Lots are tied to accounts.  The contents are slots
CREATE TABLE lots (
	guid char(32) NOT NULL,

	account_guid char(32) NOT NULL,

	PRIMARY KEY(guid),
	CONSTRAINT c_lot_account
			FOREIGN KEY(account_guid) REFERENCES accounts(guid)
);
