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
%}

%import "engine.i"

%rename(gncOwnerReturnGUID) gncOwnerRetGUID;

%inline %{
GUID gncTaxTableReturnGUID(GncTaxTable *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }
GUID gncInvoiceReturnGUID(GncInvoice *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }
GUID gncJobReturnGUID(GncJob *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }
GUID gncVendorReturnGUID(GncVendor *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }
GUID gncCustomerReturnGUID(GncCustomer *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }
GUID gncEmployeeReturnGUID(GncEmployee *x)
{ return (x ? *(qof_instance_get_guid(QOF_INSTANCE(x))) : *(guid_null())); }
%}

//%typemap(in) EntryList {
//}

%typemap(out) EntryList {
  SCM list = SCM_EOL;
  GList *node;

  for (node = $1; node; node = node->next)
    list = scm_cons(SWIG_NewPointerObj(node->data, 
        SWIGTYPE_p__gncEntry, 0), list);

  $result = scm_reverse(list);
}

%typemap(out) AccountValueList {
  SCM list = SCM_EOL;
  GList *node;

  for (node = $1; node; node = node->next)
    list = scm_cons(SWIG_NewPointerObj(node->data, 
        SWIGTYPE_p__gncAccountValue, 0), list);

  $result = scm_reverse(list);
}

%typemap(in) GncAccountValue * "$1 = gnc_scm_to_account_value_ptr($input);"
%typemap(out) GncAccountValue * "$result = gnc_account_value_ptr_to_scm($1);"


/* Parse the header file to generate wrappers */
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

