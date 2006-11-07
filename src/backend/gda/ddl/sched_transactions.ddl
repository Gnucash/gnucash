CREATE TABLE sched_transactions (
	sched_tx_guid char(32) NOT NULL,
	name text NOT NULL,
	autocreate boolean,
	notify boolean,
	create_days_in_advance int,
	remind_days_in_advance int,
	instance_count int,
	start_date date,
	last_occur_date date,
	num_occur int,
	rem_occur int,
	end_date date,

	fs_guid char(32),

	PRIMARY KEY(sched_tx_guid),
	CONSTRAINT c_sx_fs
			FOREIGN KEY(fs_guid) REFERENCES freq_specs(guid)
);

CREATE TABLE sched_tx_deferred_instances (
	id int NOT NULL,
	sched_tx_guid char(32) NOT NULL,
	last_date date,
	num_occur_rem int,
	num_inst int,

	PRIMARY KEY(id),
	CONSTRAINT c_sx_instance_tx
			FOREIGN KEY(sched_tx_guid) REFERENCES sched_transactions(sched_tx_guid)
);

CREATE TABLE sched_tx_splits (
	sched_tx_split_id int NOT NULL,
	sched_tx_guid char(32) NOT NULL,
	memo text,
	action text,
	account_guid char(32) NOT NULL,
	value text,

	PRIMARY KEY(sched_tx_split_id),
	CONSTRAINT c_sx_split_tx
			FOREIGN KEY(sched_tx_guid) REFERENCES sched_transactions(sched_tx_guid),
	CONSTRAINT c_sx_split_account
			FOREIGN KEY(account_guid) REFERENCES accounts(guid)
);
