%module sw_business_core
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gncAddress.h>
#include <gncBillTerm.h>
#include <gncCustomer.h>
#include <gncEmployee.h>
#include <gncEntry.h>
#include <gncInvoice.h>
#include <gncJob.h>
#include <gncOrder.h>
#include <gncOwner.h>
#include <gncTaxTable.h>
#include <gncVendor.h>
#include <gncBusGuile.h>
#include "engine-helpers.h"
#include "gncBusGuile.h"

/* Disable -Waddress.  GCC 4.2 warns (and fails to compile with -Werror) when
 * passing the address of a guid on the stack to QOF_BOOK_LOOKUP_ENTITY via
 * gncInvoiceLookup and friends.  When the macro gets inlined, the compiler
 * emits a warning that the guid null pointer test is always true.
 */
#if (__GNUC__ >= 4 && __GNUC_MINOR__ >= 2)
#    pragma GCC diagnostic warning "-Waddress"
#endif

SCM scm_init_sw_business_core_module (void);
%}

%import "base-typemaps.i"

%rename(gncOwnerReturnGUID) gncOwnerRetGUID;

%inline %{
static GUID gncTaxTableReturnGUID(GncTaxTable *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GUID gncInvoiceReturnGUID(GncInvoice *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GUID gncJobReturnGUID(GncJob *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GUID gncVendorReturnGUID(GncVendor *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GUID gncCustomerReturnGUID(GncCustomer *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GUID gncEmployeeReturnGUID(GncEmployee *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }

static GncTaxTable * gncTaxTableLookupFlip(GUID g, QofBook *b)
{ return gncTaxTableLookup(b, &g); }

static GncInvoice * gncInvoiceLookupFlip(GUID g, QofBook *b)
{ return gncInvoiceLookup(b, &g); }

static GncJob * gncJobLookupFlip(GUID g, QofBook *b)
{ return gncJobLookup(b, &g); }

static GncVendor * gncVendorLookupFlip(GUID g, QofBook *b)
{ return gncVendorLookup(b, &g); }

static GncCustomer * gncCustomerLookupFlip(GUID g, QofBook *b)
{ return gncCustomerLookup(b, &g); }

static GncEmployee * gncEmployeeLookupFlip(GUID g, QofBook *b)
{ return gncEmployeeLookup(b, &g); }

%}

GLIST_HELPER_INOUT(EntryList, SWIGTYPE_p__gncEntry);

%typemap(in) GncAccountValue * "$1 = gnc_scm_to_account_value_ptr($input);"
%typemap(out) GncAccountValue * "$result = gnc_account_value_ptr_to_scm($1);"
%typemap(in) AccountValueList * {
  SCM list = $input;
  GList *c_list = NULL;

  while (!SCM_NULLP(list)) {
        GncAccountValue *p;

        SCM p_scm = SCM_CAR(list);
        if (SCM_FALSEP(p_scm) || SCM_NULLP(p_scm))
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



/* Parse the header files to generate wrappers */
%include <gncAddress.h>
%include <gncBillTerm.h>
%include <gncCustomer.h>
%include <gncEmployee.h>
%include <gncEntry.h>
%include <gncInvoice.h>
%include <gncJob.h>
%include <gncOrder.h>
%include <gncOwner.h>
%include <gncTaxTable.h>
%include <gncVendor.h>
%include <gncBusGuile.h>

#define URL_TYPE_CUSTOMER GNC_ID_CUSTOMER
#define URL_TYPE_VENDOR GNC_ID_VENDOR
#define URL_TYPE_EMPLOYEE GNC_ID_EMPLOYEE
#define URL_TYPE_JOB GNC_ID_JOB
#define URL_TYPE_INVOICE GNC_ID_INVOICE
// not exactly clean
#define URL_TYPE_OWNERREPORT "owner-report"

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
    SET_ENUM("OWNER-PARENTG");
    SET_ENUM("OWNER-FROM-LOT");


#undefine SET_ENUM
  }

}
