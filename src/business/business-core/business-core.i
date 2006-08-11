%module sw_business_core
%{
/* Includes the header in the wrapper code */
#include <config.h>
#include <gncAddress.h> 
//#include <gncBillTerm.h>
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
%}

// useless. :(
//%rename(gnc_invoice_get_guid) gncInvoiceRetGUID;

/* Convert from Guile --> C */
%typemap(in) GUID {
    $1 = gnc_scm2guid($input);
}

/* Convert from C --> Guile */
%typemap(out) GUID {
    $result = gnc_guid2scm($1);
}

/*
%typemap(in) SWIGTYPE *, SWIGTYPE &, SWIGTYPE [] {
  if (SCM_FALSE_P($input)) {
      $1 = ($1_ltype)(SWIG_MustGetP
  $1 = ($1_ltype)SWIG_MustGetPtr($input, $descriptor, $argnum, 0);
}
*/

/* Parse the header file to generate wrappers */
%include <gncAddress.h> 
//%include <gncBillTerm.h>
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

