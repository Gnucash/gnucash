CREATE TABLE prices (
	guid char(32) NOT NULL,
	commodity_guid char(32) NOT NULL,
	currency_guid char(32) NOT NULL,
	time date NOT NULL,
	source text,
	type text,
	value_num int,
	value_denom int,

	PRIMARY KEY(guid),
	CONSTRAINT c_price_commodity
			FOREIGN KEY(commodity_guid) REFERENCES commodities(guid),
	CONSTRAINT c_price_currency
			FOREIGN KEY(currency_guid) REFERENCES commodities(guid)
);
