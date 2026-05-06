/*
 * gnucash_core.i -- SWIG interface file for the core parts of GnuCash
 *
 * Copyright (C) 2008 ParIT Worker Co-operative <paritinfo@parit.ca>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 *
 * @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
 * @author Jeff Green, ParIT Worker Co-operative <jeff@parit.ca>
 */

/** @file
    @brief SWIG interface file for the core parts of GnuCash

        This file is processed by SWIG and the resulting files are gnucash_core.c and gnucash_core_c.py.
        Have a look at the includes to see which parts of the GnuCash source SWIG takes as input.
    @author Mark Jenkins, ParIT Worker Co-operative <mark@parit.ca>
    @author Jeff Green,   ParIT Worker Co-operative <jeff@parit.ca>
    @ingroup python_bindings 

    @file gnucash_core.c
    @brief SWIG output file.
    @ingroup python_bindings
    @file gnucash_core_c.py
    @brief SWIG output file.
    @ingroup python_bindings
*/

%feature("autodoc", "1");
%module(package="gnucash") gnucash_core_c

%{
#include <config.h>
#include <datetime.h>
#include "qofsession.h"
#include "qofbook.h"
#include "qofbackend.h"
#include "qoflog.h"
#include "qofutil.h"
#include "qofid.h"
#include "guid.h"
#include "qofquery.h"
#include "qofquerycore.h"
#include "gnc-engine.h"
#include "Transaction.h"
#include "Split.h"
#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-environment.h"
#include "gnc-lot.h"
#include "gnc-numeric.h"
#include "gncCustomer.h"
#include "gncCustomerP.h"
#include "gncEmployee.h"
#include "gncVendor.h"
#include "gncVendorP.h"
#include "gncAddress.h"
#include "gncBillTerm.h"
#include "gncOwner.h"
#include "gncInvoice.h"
#include "gncInvoiceP.h"
#include "gncJob.h"
#include "gncEntry.h"
#include "gncTaxTable.h"
#include "gncIDSearch.h"
#include "gnc-pricedb.h"
#include "gnc-prefs-utils.h"
#include "cap-gains.h"
#include "Scrub3.h"
%}

%include <time64.i>

%include <base-typemaps.i>

/* GNC_ACCEPT_WRAPPER: Generate input typemaps that accept both raw SWIG
 * pointers and Python wrapper objects (ClassFromFunctions subclasses).
 *
 * The Python bindings use a two-layer architecture:
 *   - gnucash_core_c (SWIG-generated): exposes C functions with raw pointers
 *   - gnucash_core.py: wraps selected methods to return Python objects
 *
 * When gnucash_core.py wraps return types (via methods_return_instance),
 * the returned Python objects store the raw SWIG pointer in a .instance
 * attribute.  Without these typemaps, passing such a wrapper object to a
 * gnucash_core_c function fails because SWIG only recognizes its own
 * pointer wrappers.
 *
 * These typemaps fix that: they try normal SWIG conversion first (zero
 * overhead for the common case), and fall back to extracting .instance
 * if needed.
 */
%define GNC_ACCEPT_WRAPPER(CType)
%typemap(in) CType * (void *argp = NULL) {
    int res = SWIG_ConvertPtr($input, &argp, $1_descriptor, 0);
    if (SWIG_IsOK(res)) {
        $1 = %reinterpret_cast(argp, $1_ltype);
    } else {
        PyObject *instance = PyObject_GetAttrString($input, "instance");
        if (instance != NULL) {
            res = SWIG_ConvertPtr(instance, &argp, $1_descriptor, 0);
            Py_DECREF(instance);
            if (SWIG_IsOK(res)) {
                if (PyErr_WarnEx(PyExc_DeprecationWarning,
                    "Passing " #CType " wrapper objects directly to "
                    "gnucash_core_c is deprecated; "
                    "use the Python API or .instance attribute.", 1) < 0) {
                    SWIG_fail;
                }
                $1 = %reinterpret_cast(argp, $1_ltype);
            } else {
                SWIG_exception_fail(SWIG_TypeError,
                    "in method '$symname', argument $argnum:"
                    " .instance is not a " #CType " *");
            }
        } else {
            PyErr_Clear();
            SWIG_exception_fail(SWIG_TypeError,
                "in method '$symname', argument $argnum:"
                " expected " #CType " * or wrapper object");
        }
    }
}
%apply CType * { const CType * };
%enddef

/* Core engine types */
GNC_ACCEPT_WRAPPER(Account)
GNC_ACCEPT_WRAPPER(Split)
GNC_ACCEPT_WRAPPER(Transaction)
GNC_ACCEPT_WRAPPER(GNCLot)
GNC_ACCEPT_WRAPPER(gnc_commodity)
GNC_ACCEPT_WRAPPER(gnc_commodity_namespace)
GNC_ACCEPT_WRAPPER(gnc_commodity_table)
GNC_ACCEPT_WRAPPER(GNCPrice)
GNC_ACCEPT_WRAPPER(GNCPriceDB)
GNC_ACCEPT_WRAPPER(QofBook)
GNC_ACCEPT_WRAPPER(QofSession)
GNC_ACCEPT_WRAPPER(GncGUID)

/* Business types */
GNC_ACCEPT_WRAPPER(GncCustomer)
GNC_ACCEPT_WRAPPER(GncEmployee)
GNC_ACCEPT_WRAPPER(GncVendor)
GNC_ACCEPT_WRAPPER(GncJob)
GNC_ACCEPT_WRAPPER(GncAddress)
GNC_ACCEPT_WRAPPER(GncBillTerm)
GNC_ACCEPT_WRAPPER(GncTaxTable)
GNC_ACCEPT_WRAPPER(GncInvoice)
GNC_ACCEPT_WRAPPER(GncEntry)

%include <engine-common.i>

%include <qofbackend.h>

// this function is defined in qofsession.h, but isn't found in the libraries,
// ignored because SWIG attempts to link against (to create language bindings)
%ignore qof_session_not_saved;
%include <qofsession.h>

%include <qofbook.h>

%include <qofid.h>

%include <qofquery.h>

%include <qofquerycore.h>

/* SWIG doesn't like this macro, so redefine it to simply mean const */
#define G_CONST_RETURN const
%include <guid.h>

/* %include <Transaction.h>
%include <Split.h>
%include <Account.h> */

//Ignored because it is unimplemented
%ignore gnc_numeric_convert_with_error;
%include <gnc-numeric.h>

%include <gnc-commodity.h>

%typemap(out) GncOwner * {
    GncOwnerType owner_type = gncOwnerGetType($1);
    PyObject * owner_tuple = PyTuple_New(2);
    PyTuple_SetItem(owner_tuple, 0, PyInt_FromLong( (long) owner_type ) );
    PyObject * swig_wrapper_object;
    if (owner_type == GNC_OWNER_CUSTOMER ){
        swig_wrapper_object = SWIG_NewPointerObj(
        gncOwnerGetCustomer($1), $descriptor(GncCustomer *), 0);
    }
    else if (owner_type == GNC_OWNER_JOB){
        swig_wrapper_object = SWIG_NewPointerObj(
        gncOwnerGetJob($1), $descriptor(GncJob *), 0);
    }
    else if (owner_type == GNC_OWNER_VENDOR){
        swig_wrapper_object = SWIG_NewPointerObj(
        gncOwnerGetVendor($1), $descriptor(GncVendor *), 0);
    }
    else if (owner_type == GNC_OWNER_EMPLOYEE){
        swig_wrapper_object = SWIG_NewPointerObj(
        gncOwnerGetEmployee($1), $descriptor(GncEmployee *), 0);
    }
    else {
        swig_wrapper_object = Py_None;
    Py_INCREF(Py_None);
    }
    PyTuple_SetItem(owner_tuple, 1, swig_wrapper_object);
    $result = owner_tuple;
}


%typemap(in) GncOwner * {
    GncOwner * temp_owner = gncOwnerNew();
    void * pointer_to_real_thing;
    if ((SWIG_ConvertPtr($input, &pointer_to_real_thing,
                         $descriptor(GncCustomer *),
                         SWIG_POINTER_EXCEPTION)) == 0){
        gncOwnerInitCustomer(temp_owner, (GncCustomer *)pointer_to_real_thing);
        $1 = temp_owner;
    }
    else if ((SWIG_ConvertPtr($input, &pointer_to_real_thing,
                         $descriptor(GncJob *),
                         SWIG_POINTER_EXCEPTION)) == 0){
        gncOwnerInitJob(temp_owner, (GncJob *)pointer_to_real_thing);
        $1 = temp_owner;
    }
    else if ((SWIG_ConvertPtr($input, &pointer_to_real_thing,
                         $descriptor(GncVendor *),
                         SWIG_POINTER_EXCEPTION)) == 0){
        gncOwnerInitVendor(temp_owner, (GncVendor *)pointer_to_real_thing);
        $1 = temp_owner;
    }
    else if ((SWIG_ConvertPtr($input, &pointer_to_real_thing,
                         $descriptor(GncEmployee *),
                         SWIG_POINTER_EXCEPTION)) == 0){
        gncOwnerInitEmployee(temp_owner, (GncEmployee *)pointer_to_real_thing);
        $1 = temp_owner;
    }
    else {
    PyErr_SetString(
        PyExc_ValueError,
        "Python object passed to function with GncOwner * argument "
        "couldn't be converted back to pointer of that type");
        return NULL;
    }
}

%typemap(freearg) GncOwner * {
    gncOwnerFree($1);
}

static const GncGUID * gncEntryGetGUID(GncEntry *x);

%include <gnc-lot.h>

//core business includes
%include <gncOwner.h>
%include <gncCustomer.h>
%include <gncCustomerP.h>
%include <gncEmployee.h>
%include <gncVendor.h>
%include <gncVendorP.h>
%include <gncAddress.h>
%include <gncBillTerm.h>
%include <gncInvoice.h>
%include <gncInvoiceP.h>
%include <gncJob.h>
%include <gncEntry.h>
%include <gncTaxTable.h>
%include <gncIDSearch.h>

// Commodity prices includes and stuff
%include <gnc-pricedb.h>

%include <cap-gains.h>
%include <Scrub3.h>

%include <qoflog.h>

%init %{
gnc_environment_setup();
qof_log_init();
qof_init();
qof_query_init();
char * no_args[1] = { NULL };
gnc_engine_init(0, no_args);
gnc_prefs_init();
%}
//We must explicitly declare this or it gets left out and we can't create books.
QofBook* qof_book_new (void);
