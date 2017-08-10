/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

%inline %{
static const GncGUID * gncSplitGetGUID(Split *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GncGUID * gncTransGetGUID(Transaction *x)
{ return qof_instance_get_guid(QOF_INSTANCE(x)); }
static const GncGUID * gncAccountGetGUID(Account *x)
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
