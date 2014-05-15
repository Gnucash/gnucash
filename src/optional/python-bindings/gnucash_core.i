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
#include "config.h"
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
#include "gnc-module/gnc-module.h"
#include "engine/gnc-engine.h"
#include "Transaction.h"
#include "Split.h"
#include "Account.h"
#include "gnc-commodity.h"
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
#include "engine/gnc-pricedb.h"
#include "app-utils/gnc-prefs-utils.h"
%}

%include <timespec.i>

%include <base-typemaps.i>

%include <engine-common.i>

%include <qofbackend.h>

// this function is defined in qofsession.h, but isnt found in the libraries,
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

%init %{
gnc_environment_setup();
qof_log_init();
qof_init();
qof_query_init();
gnc_module_system_init();
char * no_args[1] = { NULL };
gnc_engine_init(0, no_args);
gnc_prefs_init();
%}

