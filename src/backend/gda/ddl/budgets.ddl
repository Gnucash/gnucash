CREATE TABLE budgets (
	guid char(32) NOT NULL,
	name text NOT NULL,
	description text,
	num_periods int NOT NULL,
	recur_mult int NOT NULL,
	recur_type int NOT NULL,
	recur_date date NOT NULL,
	
	PRIMARY KEY(guid)
);
