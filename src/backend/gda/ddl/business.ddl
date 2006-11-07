CREATE TABLE addresses (
	address_id int NOT NULL,
	name text NOT NULL,
	addr1 text,
	addr2 text,
	addr3 text,
	addr4 text,
	phone text,
	fax text,
	email text,

#	slots ???

	PRIMARY KEY(address_id)
);

CREATE TABLE billterms (
	billterm_id int NOT NULL,
	guid_1 int NOT NULL,
	guid_2 int NOT NULL,
	guid_3 int NOT NULL,
	guid_4 int NOT NULL,
	name text,
	desc text,
	refcount int,
	invisible int
	parent_billterm_id int,
	child_billterm_id int,
	type int,
	duedays int,
	discdays int,
	discount_num int,
	discount_denom int,
	prox_cutoff int,

#	slots ???

	PRIMARY KEY(billterm_id),
	CONSTRAINT FOREIGN KEY(parent_billterm_id) REFERENCES billterms(billterm_id),
	CONSTRAINT FOREIGN KEY(child_billterm_id) REFERENCES billterms(billterm_id)
);

CREATE TABLE customers (
	customer_id int NOT NULL,
	guid_1 int NOT NULL,
	guid_2 int NOT NULL,
	guid_3 int NOT NULL,
	guid_4 int NOT NULL,
	name text,
	id text,
	address --,
	shipaddress --,
	notes text,
	terms_id int,
	tax_included text,
	active int,
	discount_num int,
	discount_denom int,
	credit_num int,
	credit_denom int,
	currency_id int,
	taxtable_override int,

#	slots ???

	PRIMARY KEY(customer_id),
	CONSTRAINT FOREIGN KEY(currency_id) REFERENCES currencies(currency_id),
	CONSTRAINT FOREIGN KEY(terms_id) REFERENCES billterms(billterm_id)
);

CREATE TABLE invoices (
	invoice_id int NOT NULL,
	guid_1 int NOT NULL,
	guid_2 int NOT NULL,
	guid_3 int NOT NULL,
	guid_4 int NOT NULL,
	id text,
	owner ???
	opened_date date,
	posted_date date,
	terms_id int,
	billing_id text,
	notes text,
	active int,
	posttxn_id int,
	postlot_id int,
	postacc_id int,
	currency_id int,
	bill_to ???
	to_charge_num int,
	to_charge_denom int,

	PRIMARY KEY(invoice_id),
	CONSTRAINT FOREIGN KEY(terms_id) REFERENCES billterms(billterm_id),
	CONSTRAINT FOREIGN KEY(posttxn_id) REFERENCES transactions(trans_id),
	CONSTRAINT FOREIGN KEY(postlot_id) REFERENCES lots(lot_id),
	CONSTRAINT FOREIGN KEY(postacc_id) REFERENCES accounts(account_id),
	CONSTRAINT FOREIGN KEY(currency_id) REFERENCES currencies(currency_id)
);

CREATE TABLE jobs (
	job_id int NOT NULL,
	guid_1 int NOT NULL,
	guid_2 int NOT NULL,
	guid_3 int NOT NULL,
	guid_4 int NOT NULL,
	id text,
	name text,
	reference text,
	owner ???
	active int,

	PRIMARY KEY(job_id)
);

CREATE TABLE employees (
	employee_id int NOT NULL,
	guid_1 int NOT NULL,
	guid_2 int NOT NULL,
	guid_3 int NOT NULL,
	guid_4 int NOT NULL,
	username text,
	id text,
	address --,
	language text,
	acl text,
	active int,
	workday_num int,
	workday_denom int,
	rate_num int,
	rate_denom int,
	currency_id int,

#	ccard ??? - it's a guid

	PRIMARY KEY(customer_id),
	CONSTRAINT FOREIGN KEY(currency_id) REFERENCES currencies(currency_id)
);

CREATE TABLE orders (
	order_id int NOT NULL,
	guid_1 int NOT NULL,
	guid_2 int NOT NULL,
	guid_3 int NOT NULL,
	guid_4 int NOT NULL,
	id text,
	owner ???
	date_opened date,
	date_closed date,
	notes text,
	reference text,
	active int,

	PRIMARY KEY(order_id)
);

CREATE TABLE taxtables (
	taxtable_id int NOT NULL,
	guid_1 int NOT NULL,
	guid_2 int NOT NULL,
	guid_3 int NOT NULL,
	guid_4 int NOT NULL,
	name text,
	refcount int,
	invisible int,
	parent_id int,
	child_id int,

	PRIMARY KEY(taxtable_id),
	CONSTRAINT FOREIGN KEY(parent_id) REFERENCES taxtables(taxtable_id),
	CONSTRAINT FOREIGN KEY(child_id) REFERENCES taxtables(taxtable_id)
);

CREATE TABLE taxtable_entries (
	tt_entry_id int NOT NULL,
	account_id int,
	amount_num int,
	amount_denom int,
	type text,

	PRIMARY KEY(tt_entry_id),
	CONSTRAINT FOREIGN KEY(account_id) REFERENCES accounts(account_id)
);

CREATE TABLE vendors (
	vendor_id int NOT NULL,
	guid_1 int NOT NULL,
	guid_2 int NOT NULL,
	guid_3 int NOT NULL,
	guid_4 int NOT NULL,
	name text,
	id text,
	address --,
	notes text,
	terms_id int,
	taxincluded text,
	active int,
	currency_id,
	taxtable_override int,
	taxtable_id int,

	PRIMARY KEY(vendor_id),
	CONSTRAINT FOREIGN KEY(terms_id) REFERENCES billterms(billterm_id),
	CONSTRAINT FOREIGN KEY(currency_id) REFERENCES currencies(currency_id),
	CONSTRAINT FOREIGN KEY(taxtable_id) REFERENCES taxtables(taxtable_id)
);
