divert(-1)

/* include definitions for setter macros */
include(`table.m4')

divert

store_one_only(account)
store_one_only(checkpoint)
store_one_only(modity)
store_one_only(session)
store_one_only(split)
store_one_only(transaction)
store_one_only(price)

compare_one_only(account)
compare_one_only(modity)
compare_one_only(split)
compare_one_only(transaction)
compare_one_only(price)

put_one_only(account)
put_one_only(modity)
put_one_only(split)
put_one_only(transaction)
put_one_only(price)

compare_version(account)
compare_version(transaction)
compare_version(price)
