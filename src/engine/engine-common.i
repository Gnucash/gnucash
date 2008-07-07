%inline %{
static const GUID * gncSplitGetGUID(Split *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GUID * gncTransGetGUID(Transaction *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GUID * gncAccountGetGUID(Account *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
%}

%typemap(newfree) AccountList * "g_list_free($1);"
%typemap(newfree) SplitList * "g_list_free($1);"
%typemap(newfree) TransList * "g_list_free($1);"
%typemap(newfree) PriceList * "g_list_free($1);"
%typemap(newfree) LotList * "g_list_free($1);"
%typemap(newfree) CommodityList * "g_list_free($1);"

%include <Split.h>

AccountList * gnc_account_get_children (const Account *account);
AccountList * gnc_account_get_children_sorted (const Account *account);
AccountList * gnc_account_get_descendants (const Account *account);
AccountList * gnc_account_get_descendants_sorted (const Account *account);
%ignore gnc_account_get_children;
%ignore gnc_account_get_children_sorted;
%ignore gnc_account_get_descendants;
%ignore gnc_account_get_descendants_sorted;
%include <Account.h>

%include <Transaction.h>

%include <gnc-lot.h>
