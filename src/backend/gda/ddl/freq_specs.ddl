CREATE TABLE freq_specs (
	guid char(32) NOT NULL,

	fs_type int,
	fs_interval int,
	fs_offset int,
	day int,
	occurrence int,

-- Parent used for composite fs's
	parent_guid char(32),

	PRIMARY KEY(guid),
	CONSTRAINT c_fs_parent
			FOREIGN KEY(parent_guid) REFERENCES freq_specs(guid)
);
