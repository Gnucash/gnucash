CREATE TABLE transactions (
	guid char(32) NOT NULL,
	currency_guid char(32) NOT NULL,
	num text,
	post_date date,
	enter_date date,
	description text,

	PRIMARY KEY(guid),
	CONSTRAINT c_tx_currency
			FOREIGN KEY(currency_guid) REFERENCES commodities(guid)
);

CREATE TABLE splits (
	guid char(32) NOT NULL,
	tx_guid char(32) NOT NULL,
	memo text,
	action text,
	reconcile_state char NOT NULL,
	reconcile_date date,
	value_num int NOT NULL,
	value_denom int NOT NULL,
	quantity_num int NOT NULL,
	quantity_denom int NOT NULL,
	account_guid char(32) NOT NULL,

	lot_guid char(32),

	PRIMARY KEY(guid),
	CONSTRAINT c_split_tx
			FOREIGN KEY(tx_guid) REFERENCES transactions(guid),
	CONSTRAINT c_split_account
			FOREIGN KEY(account_guid) REFERENCES accounts(guid),
	CONSTRAINT c_split_lot
			FOREIGN KEY(lot_guid) REFERENCES lots(guid)
);
