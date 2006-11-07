CREATE TABLE accounts (
	guid char(32) NOT NULL,
	name varchar(50) NOT NULL,
	account_type_id int NOT NULL,
	commodity_guid char(32) NOT NULL,
	parent_guid char(32),
	code varchar(50),
	description varchar(500),

	PRIMARY KEY(guid),
	CONSTRAINT c_account_parent
			FOREIGN KEY(parent_guid) REFERENCES accounts(guid),
	CONSTRAINT c_account_commodity
			FOREIGN KEY(commodity_guid) REFERENCES commodities(guid)
);
