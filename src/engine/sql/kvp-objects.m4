divert(-1)

/* include definitions for setter macros */
include(`table.m4')

divert

store_one_only(kvp_gint64)
store_one_only(kvp_double)
store_one_only(kvp_numeric)
store_one_only(kvp_string)
store_one_only(kvp_guid)

store_audit(kvp_gint64)
store_audit(kvp_double)
store_audit(kvp_numeric)
store_audit(kvp_string)
store_audit(kvp_guid)

compare_one_only(kvp_gint64)
compare_one_only(kvp_double)
compare_one_only(kvp_numeric)
compare_one_only(kvp_string)
compare_one_only(kvp_guid)

put_one_only(kvp_gint64)
put_one_only(kvp_double)
put_one_only(kvp_numeric)
put_one_only(kvp_string)
put_one_only(kvp_guid)


