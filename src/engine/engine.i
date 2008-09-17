%module sw_engine
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <glib.h>
#include <qof.h>
#include <Query.h>
#include <gnc-budget.h>
#include <gnc-commodity.h>
#include <gnc-engine.h>
#include <gnc-filepath-utils.h>
#include <gnc-pricedb.h>
#include <gnc-lot.h>
#include <gnc-session-scm.h>
#include <gnc-hooks-scm.h>
#include <engine-helpers.h>
#include <SX-book.h>
#include <kvp-scm.h>
#include "glib-helpers.h"

SCM scm_init_sw_engine_module (void);
%}

%import "base-typemaps.i"


GLIST_HELPER_INOUT(SplitList, SWIGTYPE_p_Split);
GLIST_HELPER_INOUT(TransList, SWIGTYPE_p_Transaction);
GLIST_HELPER_INOUT(LotList, SWIGTYPE_p_GNCLot);
GLIST_HELPER_INOUT(AccountList, SWIGTYPE_p_Account);
GLIST_HELPER_INOUT(PriceList, SWIGTYPE_p_GNCPrice);
// TODO: free PriceList?
GLIST_HELPER_INOUT(CommodityList, SWIGTYPE_p_gnc_commodity);


%inline %{
static const GUID * gncSplitGetGUID(Split *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GUID * gncTransGetGUID(Transaction *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GUID * gncAccountGetGUID(Account *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GUID * gncPriceGetGUID(GNCPrice *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GUID * gncBudgetGetGUID(GncBudget *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
%}

%typemap(newfree) AccountList * "g_list_free($1);"
%typemap(newfree) SplitList * "g_list_free($1);"
%typemap(newfree) TransList * "g_list_free($1);"
%typemap(newfree) PriceList * "g_list_free($1);"
%typemap(newfree) LotList * "g_list_free($1);"
%typemap(newfree) CommodityList * "g_list_free($1);"

%typemap(newfree) gchar * "g_free($1);"

/* NB: The object ownership annotations should already cover all the
functions currently used in guile, but not all the functions that are
wrapped.  So, we should contract the interface to wrap only the used
functions. */

%delobject gnc_price_list_destroy;
%newobject gnc_pricedb_lookup_latest_any_currency;

%newobject gnc_pricedb_lookup_nearest_in_time_any_currency;
%newobject gnc_pricedb_lookup_latest_before_any_currency;
%newobject gnc_pricedb_get_prices;
%newobject gnc_pricedb_lookup_at_time;
%newobject gnc_pricedb_lookup_day;

%newobject xaccQueryGetSplitsUniqueTrans;
%newobject xaccQueryGetTransactions;
%newobject xaccQueryGetLots;

%newobject xaccSplitGetCorrAccountFullName;
%newobject gnc_numeric_to_string;
%newobject gnc_build_dotgnucash_path;
%newobject gnc_build_book_path;

/* Parse the header file to generate wrappers */
%inline {
  static QofIdType QOF_ID_BOOK_SCM (void) { return QOF_ID_BOOK; }
}

%include <Split.h>
%include <engine-helpers.h>
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
%include <gnc-pricedb.h>

QofSession * qof_session_new (void);
QofBook * qof_session_get_book (QofSession *session);

// TODO: Maybe unroll
void qof_book_kvp_changed (QofBook *book);

// TODO: Unroll/remove
const char *qof_session_get_url (QofSession *session);

extern const char *gnc_default_strftime_date_format;
const char *gnc_print_date (Timespec ts);

GUID guid_new_return(void);

%inline {
static QofQuery * qof_query_create_for_splits(void) {
  return qof_query_create_for(GNC_ID_SPLIT);
}
}
%typemap(in) GSList * "$1 = gnc_query_scm2path($input);"

void qof_query_add_guid_match (QofQuery *q, GSList *param_list,
                           const GUID *guid, QofQueryOp op);
void qof_query_set_sort_order (QofQuery *q, GSList *params1,
                           GSList *params2, GSList *params3);

%clear GSList *;
SplitList * qof_query_run (QofQuery *q);

%include <Query.h>
%ignore qof_query_add_guid_match;
%ignore qof_query_set_sort_order;
%ignore qof_query_run;
%include <qofquery.h>
%include <qofquerycore.h>

gnc_numeric gnc_numeric_create(gint64 num, gint64 denom);
gnc_numeric gnc_numeric_zero(void);
gint64 gnc_numeric_num(gnc_numeric a);
gint64 gnc_numeric_denom(gnc_numeric a);
gboolean gnc_numeric_zero_p(gnc_numeric a);
int gnc_numeric_compare(gnc_numeric a, gnc_numeric b);
gboolean gnc_numeric_negative_p(gnc_numeric a);
gboolean gnc_numeric_positive_p(gnc_numeric a);
gboolean gnc_numeric_equal(gnc_numeric a, gnc_numeric b);
gnc_numeric
gnc_numeric_add(gnc_numeric a, gnc_numeric b, gint64 denom, gint how);
gnc_numeric
gnc_numeric_sub(gnc_numeric a, gnc_numeric b, gint64 denom, gint how);
gnc_numeric
gnc_numeric_mul(gnc_numeric a, gnc_numeric b, gint64 denom, gint how);
gnc_numeric
gnc_numeric_div(gnc_numeric a, gnc_numeric b, gint64 denom, gint how);
gnc_numeric gnc_numeric_neg(gnc_numeric a);
gnc_numeric gnc_numeric_abs(gnc_numeric a);
gnc_numeric gnc_numeric_add_fixed(gnc_numeric a, gnc_numeric b);
gnc_numeric gnc_numeric_sub_fixed(gnc_numeric a, gnc_numeric b);
gnc_numeric gnc_numeric_convert(gnc_numeric in, gint64 denom, gint how);
gnc_numeric double_to_gnc_numeric(double in, gint64 denom, gint how);
double gnc_numeric_to_double(gnc_numeric in);
gchar * gnc_numeric_to_string(gnc_numeric n);

Timespec timespecCanonicalDayTime(Timespec t);

gchar * gnc_build_dotgnucash_path (const gchar *filename);
gchar * gnc_build_book_path (const gchar *filename);

%include <gnc-budget.h>

%typemap(in) GList * {
  SCM path_scm = $input;
  GList *path = NULL;

  while (!SCM_NULLP (path_scm))
  {
    SCM key_scm = SCM_CAR (path_scm);
    char *key;

    if (!SCM_STRINGP (key_scm))
      break;

    key = g_strdup (SCM_STRING_CHARS (key_scm));

    path = g_list_prepend (path, key);

    path_scm = SCM_CDR (path_scm);
  }

  $1 = g_list_reverse (path);
}

void gnc_quote_source_set_fq_installed (GList *sources_list);
%clear GList *;
%ignore gnc_quote_source_set_fq_installed;
%include <gnc-commodity.h>

%include <gnc-lot.h>
%include <gnc-session-scm.h>
void gnc_hook_add_scm_dangler (const gchar *name, SCM proc);
void gnc_hook_run (const gchar *name, gpointer data);
%include <gnc-hooks.h>

Account * gnc_book_get_template_root(QofBook *book);

// KVP stuff
%typemap(in) KvpValue * " $1 = gnc_scm_to_kvp_value_ptr($input); "
%typemap(out) KvpValue * " $result = gnc_kvp_value_ptr_to_scm($1); "
%typemap(in) GSList *key_path " $1 = gnc_scm_to_gslist_string($input);"

void gnc_kvp_frame_delete_at_path(KvpFrame *frame, GSList *key_path);
void kvp_frame_set_slot_path_gslist(
   KvpFrame *frame, const KvpValue *new_value, GSList *key_path);
KvpValue * kvp_frame_get_slot_path_gslist (KvpFrame *frame, GSList *key_path);

%clear GSList *key_path;

%inline %{
static KvpFrame * gnc_book_get_slots(QofBook *book) {
   return qof_instance_get_slots(QOF_INSTANCE(book));
}
%}


%init {
  {
    char tmp[100];

#define SET_ENUM(e) snprintf(tmp, 100, "(set! %s (%s))", (e), (e));  \
    scm_c_eval_string(tmp);

    SET_ENUM("TXN-TYPE-NONE");
    SET_ENUM("TXN-TYPE-INVOICE");
    SET_ENUM("TXN-TYPE-PAYMENT");

    SET_ENUM("ACCT-TYPE-INVALID");
    SET_ENUM("ACCT-TYPE-NONE");
    SET_ENUM("ACCT-TYPE-BANK");
    SET_ENUM("ACCT-TYPE-CASH");
    SET_ENUM("ACCT-TYPE-CREDIT");
    SET_ENUM("ACCT-TYPE-ASSET");
    SET_ENUM("ACCT-TYPE-LIABILITY");
    SET_ENUM("ACCT-TYPE-STOCK");
    SET_ENUM("ACCT-TYPE-MUTUAL");
    SET_ENUM("ACCT-TYPE-CURRENCY");
    SET_ENUM("ACCT-TYPE-INCOME");
    SET_ENUM("ACCT-TYPE-EXPENSE");
    SET_ENUM("ACCT-TYPE-EQUITY");
    SET_ENUM("ACCT-TYPE-RECEIVABLE");
    SET_ENUM("ACCT-TYPE-PAYABLE");
    SET_ENUM("NUM-ACCOUNT-TYPES");
    SET_ENUM("ACCT-TYPE-CHECKING");
    SET_ENUM("ACCT-TYPE-SAVINGS");
    SET_ENUM("ACCT-TYPE-MONEYMRKT");
    SET_ENUM("ACCT-TYPE-CREDITLINE");

    SET_ENUM("QOF-QUERY-AND");
    SET_ENUM("QOF-QUERY-OR");

    SET_ENUM("QUERY-TXN-MATCH-ALL");
    SET_ENUM("QUERY-TXN-MATCH-ANY");

    SET_ENUM("QOF-GUID-MATCH-ALL");
    SET_ENUM("QOF-GUID-MATCH-ANY");
    SET_ENUM("QOF-GUID-MATCH-NULL");
    SET_ENUM("QOF-GUID-MATCH-NONE");
    SET_ENUM("QOF-GUID-MATCH-LIST-ANY");

    SET_ENUM("QOF-COMPARE-LT");
    SET_ENUM("QOF-COMPARE-LTE");
    SET_ENUM("QOF-COMPARE-EQUAL");
    SET_ENUM("QOF-COMPARE-GT");
    SET_ENUM("QOF-COMPARE-GTE");
    SET_ENUM("QOF-COMPARE-NEQ");

    SET_ENUM("QOF-NUMERIC-MATCH-ANY");
    SET_ENUM("QOF-NUMERIC-MATCH-CREDIT");
    SET_ENUM("QOF-NUMERIC-MATCH-DEBIT");

    SET_ENUM("QOF-PARAM-BOOK");
    SET_ENUM("QOF-PARAM-GUID");
    SET_ENUM("QOF-PARAM-KVP");
    SET_ENUM("QOF-PARAM-ACTIVE");
    SET_ENUM("QOF-PARAM-VERSION");

    SET_ENUM("CLEARED-NO");
    SET_ENUM("CLEARED-CLEARED");
    SET_ENUM("CLEARED-FROZEN");
    SET_ENUM("CLEARED-RECONCILED");
    SET_ENUM("CLEARED-VOIDED");

    SET_ENUM("HOOK-REPORT");
    SET_ENUM("HOOK-SAVE-OPTIONS");

    //SET_ENUM("GNC-ID-ACCOUNT");
    SET_ENUM("QOF-ID-BOOK-SCM");
    //SET_ENUM("GNC-ID-BUDGET");
    //SET_ENUM("GNC-ID-LOT");
    //SET_ENUM("GNC-ID-PRICE");
    //SET_ENUM("GNC-ID-SPLIT");
    //SET_ENUM("GNC-ID-SCHEDXACTION");
    //SET_ENUM("QOF-ID-SESSION");
    //SET_ENUM("GNC-ID-TRANS");

    SET_ENUM("QUERY-DEFAULT-SORT");
    SET_ENUM("SPLIT-LOT");
    SET_ENUM("SPLIT-TRANS");
    SET_ENUM("SPLIT-ACCOUNT");
    SET_ENUM("SPLIT-VALUE");
    SET_ENUM("SPLIT-MEMO");
    SET_ENUM("SPLIT-DATE-RECONCILED");
    SET_ENUM("SPLIT-ACCT-FULLNAME");
    SET_ENUM("SPLIT-CORR-ACCT-NAME");
    SET_ENUM("SPLIT-CORR-ACCT-CODE");

    SET_ENUM("TRANS-DATE-POSTED");
    SET_ENUM("TRANS-DESCRIPTION");
    SET_ENUM("TRANS-NUM");

    SET_ENUM("ACCOUNT-CODE-");  /* sic */

#undefine SET_ENUM
  }

}

