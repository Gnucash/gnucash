-- Commodities table - stores currencies and stocks/mutual funds
CREATE TABLE commodities (
	guid char(32) NOT NULL,

	namespace varchar(40) NOT NULL,
	mnemonic varchar(40) NOT NULL,

	fullname varchar(40),
	cusip varchar(40),
	fraction int,
	use_quote_source int,
	quote_source varchar(40),
	quote_tz varchar(40),

	PRIMARY KEY(guid)
);
