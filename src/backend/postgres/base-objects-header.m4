divert(-1)

/* include definitions for setter macros */
include(`table.m4')

divert

store_one_only_header(account);
store_one_only_header(book);
store_one_only_header(modity);
store_one_only_header(session);
store_one_only_header(split);
store_one_only_header(transaction);
store_one_only_header(price);

store_audit_header(account);
store_audit_header(book);
store_audit_header(modity);
store_audit_header(split);
store_audit_header(transaction);
store_audit_header(price);

compare_one_only_header(account);
compare_one_only_header(book);
compare_one_only_header(modity);
compare_one_only_header(split);
compare_one_only_header(transaction);
compare_one_only_header(price);

put_one_only_header(account);
put_one_only_header(book);
put_one_only_header(modity);
put_one_only_header(split);
put_one_only_header(transaction);
put_one_only_header(price);

compare_version_header(account);
compare_version_header(book);
compare_version_header(transaction);
compare_version_header(price);

is_deleted_header(account);
is_deleted_header(book);
is_deleted_header(transaction);
is_deleted_header(price);
