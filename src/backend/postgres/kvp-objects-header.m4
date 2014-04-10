divert(-1)

/* include definitions for setter macros */
include(`table.m4')

divert

store_one_only_header(kvp_gint64);
store_one_only_header(kvp_double);
store_one_only_header(kvp_numeric);
store_one_only_header(kvp_string);
store_one_only_header(kvp_guid);
store_one_only_header(kvp_timespec);

store_audit_header(kvp_gint64);
store_audit_header(kvp_double);
store_audit_header(kvp_numeric);
store_audit_header(kvp_string);
store_audit_header(kvp_guid);
store_audit_header(kvp_timespec);

compare_one_only_header(kvp_gint64);
compare_one_only_header(kvp_double);
compare_one_only_header(kvp_numeric);
compare_one_only_header(kvp_string);
compare_one_only_header(kvp_guid);
compare_one_only_header(kvp_timespec);

put_one_only_header(kvp_gint64);
put_one_only_header(kvp_double);
put_one_only_header(kvp_numeric);
put_one_only_header(kvp_string);
put_one_only_header(kvp_guid);
put_one_only_header(kvp_timespec);
