divert(-1)

/* include definitions for setter macros */
include(`table.m4')

divert

store_one_only(account)
store_one_only(book)
store_one_only(modity)
store_one_only(session)
store_one_only(split)
store_one_only(transaction)
store_one_only(price)

store_audit(account)
store_audit(book)
store_audit(modity)
store_audit(split)
store_audit(transaction)
store_audit(price)

compare_one_only(account)
compare_one_only(book)
compare_one_only(modity)
compare_one_only(split)
compare_one_only(transaction)
compare_one_only(price)

put_one_only(account)
put_one_only(book)
put_one_only(modity)
put_one_only(split)
put_one_only(transaction)
put_one_only(price)

compare_version(account)
compare_version(book)
compare_version(transaction)
compare_version(price)

is_deleted(account)
is_deleted(book)
is_deleted(transaction)
is_deleted(price)
