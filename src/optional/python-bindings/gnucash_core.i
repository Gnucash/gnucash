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

%module(package="gnucash") gnucash_core_c

%{
#include "config.h"
#include <datetime.h>
#include "qofsession.h"
#include "qofbook.h"
#include "qofbackend.h"
#include "qofid.h"
#include "guid.h"
#include "Transaction.h"
#include "Split.h"
#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-lot.h"
#include "gnc-numeric.h"
#include "gncCustomer.h"
#include "gncEmployee.h"
#include "gncVendor.h"
#include "gncAddress.h"
#include "gncBillTerm.h"
#include <guile/gh.h>
%}

%include <timespec.i>

%include <base-typemaps.i>

%include <engine-common.i>

%include <qofbackend.h>

// this function is defined in qofsession.h, but isnt found in the libraries,
// ignoroed because SWIG attempts to link against (to create language bindings)
%ignore qof_session_not_saved;
%include <qofsession.h>

%include <qofbook.h>

%include <qofid.h>

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

/* %include <gnc-lot.h> */

//business-core includes
%include <gncCustomer.h>
%include <gncEmployee.h>
%include <gncVendor.h>
%include <gncAddress.h>
%include <gncBillTerm.h>

%init %{

g_type_init();
scm_init_guile();
gnc_module_load("gnucash/engine", 0);
gnc_module_load("gnucash/business-core-file", 0);

%}
