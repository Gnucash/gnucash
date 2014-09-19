%module sw_engine
%{
/* Includes the header in the wrapper code */
#include "config.h"
#include <glib.h>
#include "qof.h"
#include "Query.h"
#include "gnc-budget.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-filepath-utils.h"
#include "gnc-pricedb.h"
#include "gnc-lot.h"
#include "gnc-hooks-scm.h"
#include "engine-helpers.h"
#include "engine-helpers-guile.h"
#include "SX-book.h"
#include "kvp-scm.h"
#include "glib-helpers.h"

#include "gncAddress.h"
#include "gncBillTerm.h"
#include "gncCustomer.h"
#include "gncEmployee.h"
#include "gncEntry.h"
#include "gncInvoice.h"
#include "gncJob.h"
#include "gncOrder.h"
#include "gncOwner.h"
#include "gncTaxTable.h"
#include "gncVendor.h"
#include "gncBusGuile.h"
%}
#if defined(SWIGGUILE)
%{
#include "guile-mappings.h"

SCM scm_init_sw_engine_module (void);
%}
#endif

%import "base-typemaps.i"

GLIST_HELPER_INOUT(SplitList, SWIGTYPE_p_Split);
GLIST_HELPER_INOUT(TransList, SWIGTYPE_p_Transaction);
GLIST_HELPER_INOUT(LotList, SWIGTYPE_p_GNCLot);
GLIST_HELPER_INOUT(AccountList, SWIGTYPE_p_Account);
GLIST_HELPER_INOUT(PriceList, SWIGTYPE_p_GNCPrice);
// TODO: free PriceList?
GLIST_HELPER_INOUT(CommodityList, SWIGTYPE_p_gnc_commodity);

%typemap(newfree) gchar * "g_free($1);"

/* These need to be here so that they are *before* the function
declarations in the header files, some of which are included by
engine-common.i */

%newobject gnc_account_get_full_name;

%include "engine-common.i"

%inline %{
static const GncGUID * gncPriceGetGUID(GNCPrice *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GncGUID * gncBudgetGetGUID(GncBudget *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
%}

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

/* Parse the header file to generate wrappers */
%inline {
  static QofIdType QOF_ID_BOOK_SCM (void) { return QOF_ID_BOOK; }
}

/* Allow '#f' in guile to be used to represent NULL in 'C' for functions *
 * 'gnc_set_num_action', 'gnc_get_num_action' and 'gnc_get_action_num' in *
 * 'engine-helpers.c' */
%typemap(in) Transaction *trans {
  if ($input == SCM_BOOL_F)
    $1 = NULL;
  else
    $1 = (Transaction *)SWIG_MustGetPtr($input, SWIGTYPE_p_Transaction, 1, 0);
}

%typemap(in) Split *split {
  if ($input == SCM_BOOL_F)
    $1 = NULL;
  else
    $1 = (Split *)SWIG_MustGetPtr($input, SWIGTYPE_p_Split, 2, 0);
}

%typemap(in) char * num (int must_free = 0) {
  if ($input == SCM_BOOL_F)
    $1 = NULL;
  else
  {
    $1 = (char *)SWIG_scm2str($input);
    must_free3 = 1;
  }
}

%typemap(in) char * action (int must_free = 0) {
  if ($input == SCM_BOOL_F)
    $1 = NULL;
  else
  {
    $1 = (char *)SWIG_scm2str($input);
    must_free4 = 1;
  }
}

%include <engine-helpers.h>
%include <engine-helpers-guile.h>
%typemap(in) Transaction *trans;
%typemap(in) Split *split;
%typemap(in) char * num;
%typemap(in) char * action;

%include <gnc-pricedb.h>

QofSession * qof_session_new (void);
QofBook * qof_session_get_book (QofSession *session);
/* This horror is to permit the scheme options in
 * src/app-utils/options.scm to read and write the book's KVP (another
 * horror) directly. It should be refactored into book functions that
 * handle the KVP access.
 */
%inline {
  KvpFrame *qof_book_get_slots (QofBook *book);
  extern KvpFrame *qof_instance_get_slots (QofInstance*);
  KvpFrame *qof_book_get_slots (QofBook *book) {
       return qof_instance_get_slots (QOF_INSTANCE (book));
  }
}
// TODO: Unroll/remove
const char *qof_session_get_url (QofSession *session);

extern const char *gnc_default_strftime_date_format;
const char *gnc_print_date (Timespec ts);

GncGUID guid_new_return(void);

%inline {
static QofQuery * qof_query_create_for_splits(void) {
  return qof_query_create_for(GNC_ID_SPLIT);
}
}

SplitList * qof_query_run (QofQuery *q);
SplitList * qof_query_last_run (QofQuery *q);
SplitList * qof_query_run_subquery (QofQuery *q, const QofQuery *q);

%typemap(in) QofQueryParamList * "$1 = gnc_query_scm2path($input);"

%include <Query.h>
%ignore qof_query_run;
%ignore qof_query_last_run;
%ignore qof_query_run_subquery;
%include <qofquery.h>
%include <qofquerycore.h>
%include <qofbookslots.h>
%include <qofbook.h>

%ignore GNC_DENOM_AUTO;
%ignore GNCNumericErrorCodes;
%ignore GNC_ERROR_OK;
%ignore GNC_ERROR_ARG;
%ignore GNC_ERROR_OVERFLOW;
%ignore GNC_ERROR_DENOM_DIFF;
%ignore GNC_ERROR_REMAINDER;
%include <gnc-numeric.h>

Timespec timespecCanonicalDayTime(Timespec t);

%include <gnc-budget.h>

%typemap(in) GList * {
  SCM path_scm = $input;
  GList *path = NULL;

  while (!scm_is_null (path_scm))
  {
    SCM key_scm = SCM_CAR (path_scm);
    char *key;
    gchar* gkey;

    if (!scm_is_string (key_scm))
      break;

    key = scm_to_locale_string (key_scm);
    gkey = g_strdup (key);
    free (key);

    path = g_list_prepend (path, gkey);

    path_scm = SCM_CDR (path_scm);
  }

  $1 = g_list_reverse (path);
}

void gnc_quote_source_set_fq_installed (GList *sources_list);
%clear GList *;
%ignore gnc_quote_source_set_fq_installed;
%ignore gnc_commodity_table_get_quotable_commodities;
%include <gnc-commodity.h>

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
   KvpFrame *frame, KvpValue *new_value, GSList *key_path);
KvpValue * kvp_frame_get_slot_path_gslist (KvpFrame *frame, GSList *key_path);

%clear GSList *key_path;

#if defined(SWIGGUILE)
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
    SET_ENUM("ACCT-TYPE-ROOT");
    SET_ENUM("ACCT-TYPE-TRADING");
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
    SET_ENUM("SPLIT-ACTION");
    SET_ENUM("SPLIT-DATE-RECONCILED");
    SET_ENUM("SPLIT-ACCT-FULLNAME");
    SET_ENUM("SPLIT-CORR-ACCT-NAME");
    SET_ENUM("SPLIT-CORR-ACCT-CODE");

    SET_ENUM("TRANS-DATE-POSTED");
    SET_ENUM("TRANS-DESCRIPTION");
    SET_ENUM("TRANS-NUM");
    
    SET_ENUM("KVP-OPTION-PATH");

    SET_ENUM("OPTION-SECTION-ACCOUNTS");
    SET_ENUM("OPTION-NAME-TRADING-ACCOUNTS");
    SET_ENUM("OPTION-NAME-AUTO-READONLY-DAYS");
    SET_ENUM("OPTION-NAME-NUM-FIELD-SOURCE");

    SET_ENUM("OPTION-SECTION-BUDGETING");
    SET_ENUM("OPTION-NAME-DEFAULT-BUDGET");

    SET_ENUM("ACCOUNT-CODE-");  /* sic */

    SET_ENUM("GNC-HOW-RND-CEIL");
    SET_ENUM("GNC-HOW-RND-TRUNC");
    SET_ENUM("GNC-HOW-RND-PROMOTE");
    SET_ENUM("GNC-HOW-RND-ROUND-HALF-DOWN");
    SET_ENUM("GNC-HOW-RND-ROUND-HALF-UP");
    SET_ENUM("GNC-HOW-RND-ROUND");
    SET_ENUM("GNC-HOW-RND-NEVER");

#undefine SET_ENUM
  }

}
#endif

%include business-core.i
