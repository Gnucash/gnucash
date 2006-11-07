-- Slots table to store all slots
CREATE TABLE slots (
	slot_id int NOT NULL,

	-- What type of object, and what guid, does this slot belong to
	object_type int NOT NULL,
	obj_guid char(32) NOT NULL,

	-- Full name
	name text NOT NULL,

	-- Slot type and value.
	slot_type int NOT NULL,
	int64_val bigint,
	string_val text,
	timespec_val date,
	guid_val char(32),
	num_val_num int,
	num_val_denom int,
	double_val double precision,

	PRIMARY KEY(slot_id)
);
