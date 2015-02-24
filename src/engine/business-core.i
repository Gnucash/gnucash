#if defined(SWIGGUILE)
%{
/* Disable -Waddress.  GCC 4.2 warns (and fails to compile with -Werror) when
 * passing the address of a guid on the stack to QOF_BOOK_LOOKUP_ENTITY via
 * gncInvoiceLookup and friends.  When the macro gets inlined, the compiler
 * emits a warning that the guid null pointer test is always true.
 */
#if (__GNUC__ >= 4 && __GNUC_MINOR__ >= 2)
#    pragma GCC diagnostic warning "-Waddress"
#endif
%}
#endif

%rename(gncOwnerReturnGUID) gncOwnerRetGUID;

%inline %{
static GncGUID gncTaxTableReturnGUID(GncTaxTable *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GncGUID gncInvoiceReturnGUID(GncInvoice *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GncGUID gncJobReturnGUID(GncJob *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GncGUID gncVendorReturnGUID(GncVendor *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GncGUID gncCustomerReturnGUID(GncCustomer *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GncGUID gncEmployeeReturnGUID(GncEmployee *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GncTaxTable * gncTaxTableLookupFlip(GncGUID g, QofBook *b)
{ return gncTaxTableLookup(b, &g); }

static GncInvoice * gncInvoiceLookupFlip(GncGUID g, QofBook *b)
{ return gncInvoiceLookup(b, &g); }

static GncJob * gncJobLookupFlip(GncGUID g, QofBook *b)
{ return gncJobLookup(b, &g); }

static GncVendor * gncVendorLookupFlip(GncGUID g, QofBook *b)
{ return gncVendorLookup(b, &g); }

static GncCustomer * gncCustomerLookupFlip(GncGUID g, QofBook *b)
{ return gncCustomerLookup(b, &g); }

static GncEmployee * gncEmployeeLookupFlip(GncGUID g, QofBook *b)
{ return gncEmployeeLookup(b, &g); }

%}

GLIST_HELPER_INOUT(GncInvoiceList, SWIGTYPE_p__gncInvoice);
GLIST_HELPER_INOUT(EntryList, SWIGTYPE_p__gncEntry);
GLIST_HELPER_INOUT(GncTaxTableEntryList, SWIGTYPE_p__gncTaxTableEntry);
GLIST_HELPER_INOUT(OwnerList, SWIGTYPE_p__gncOwner);

#if defined(SWIGGUILE)
%typemap(in) GncAccountValue * "$1 = gnc_scm_to_account_value_ptr($input);"
%typemap(out) GncAccountValue * "$result = gnc_account_value_ptr_to_scm($1);"
%typemap(in) AccountValueList * {
  SCM list = $input;
  GList *c_list = NULL;

  while (!scm_is_null(list)) {
        GncAccountValue *p;

        SCM p_scm = SCM_CAR(list);
        if (scm_is_false(p_scm) || scm_is_null(p_scm))
           p = NULL;
        else
           p = gnc_scm_to_account_value_ptr(p_scm);

        c_list = g_list_prepend(c_list, p);
        list = SCM_CDR(list);
  }

  $1 = g_list_reverse(c_list);
}
%typemap(out) AccountValueList * {
  SCM list = SCM_EOL;
  GList *node;

  for (node = $1; node; node = node->next)
    list = scm_cons(gnc_account_value_ptr_to_scm(node->data), list);

  $result = scm_reverse(list);
}
#endif


/* Parse the header files to generate wrappers */
%include <gncAddress.h>
%include <gncBillTerm.h>
%include <gncBusiness.h>
%include <gncCustomer.h>
%include <gncEmployee.h>
%include <gncEntry.h>
%include <gncInvoice.h>
%include <gncJob.h>
%include <gncOrder.h>
%include <gncOwner.h>
%include <gncTaxTable.h>
%include <gncVendor.h>
#if defined(SWIGGUILE)
%include <gncBusGuile.h>
#endif
/* Import query bindings for the below invoice query functions (but
 * don't generate bindings for them). */
%import <libqof/qof/qofquery.h>

#define URL_TYPE_CUSTOMER GNC_ID_CUSTOMER
#define URL_TYPE_VENDOR GNC_ID_VENDOR
#define URL_TYPE_EMPLOYEE GNC_ID_EMPLOYEE
#define URL_TYPE_JOB GNC_ID_JOB
#define URL_TYPE_INVOICE GNC_ID_INVOICE
// not exactly clean
#define URL_TYPE_OWNERREPORT "owner-report"

%inline %{
static QofQuery * qof_query_create_for_invoices(void) {
  return qof_query_create_for(GNC_ID_INVOICE);
}

static GncInvoiceList * qof_query_run_for_invoices(QofQuery *q) {
    return qof_query_run(q);
}
%}

#if defined(SWIGGUILE)
%init {
  {
    char tmp[100];

#define SET_ENUM(e) snprintf(tmp, 100, "(set! %s (%s))", (e), (e));  \
    scm_c_eval_string(tmp);

    SET_ENUM("GNC-OWNER-CUSTOMER");
    SET_ENUM("GNC-OWNER-VENDOR");
    SET_ENUM("GNC-OWNER-EMPLOYEE");
    SET_ENUM("GNC-OWNER-JOB");
    SET_ENUM("GNC-AMT-TYPE-VALUE");
    SET_ENUM("GNC-AMT-TYPE-PERCENT");

    SET_ENUM("URL-TYPE-CUSTOMER");
    SET_ENUM("URL-TYPE-VENDOR");
    SET_ENUM("URL-TYPE-EMPLOYEE");
    SET_ENUM("URL-TYPE-JOB");
    SET_ENUM("URL-TYPE-INVOICE");
    SET_ENUM("URL-TYPE-OWNERREPORT");

    SET_ENUM("INVOICE-FROM-TXN");
    SET_ENUM("INVOICE-FROM-LOT");
    SET_ENUM("INVOICE-OWNER");
    SET_ENUM("INVOICE-BILLTO");
    SET_ENUM("OWNER-PARENTG");
    SET_ENUM("OWNER-FROM-LOT");

    SET_ENUM("GNC-INVOICE-UNDEFINED");
    SET_ENUM("GNC-INVOICE-CUST-INVOICE");
    SET_ENUM("GNC-INVOICE-VEND-INVOICE");
    SET_ENUM("GNC-INVOICE-EMPL-INVOICE");
    SET_ENUM("GNC-INVOICE-CUST-CREDIT-NOTE");
    SET_ENUM("GNC-INVOICE-VEND-CREDIT-NOTE");
    SET_ENUM("GNC-INVOICE-EMPL-CREDIT-NOTE");

#undef SET_ENUM
  }

}
#endif
