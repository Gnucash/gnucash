/*
 * gncBusGuile.c -- Business Guile Helper Functions
 * Copyright (C) 2003 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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
 */

#include "config.h"
#include <glib.h>
#include "gncBusGuile.h"
#include "engine-helpers.h"
#include <g-wrap-wct.h>

static SCM
get_acct_type ()
{
  static SCM account_type = SCM_UNDEFINED;

  if(account_type == SCM_UNDEFINED) {
    account_type = scm_c_eval_string("<gnc:Account*>");
    /* don't really need this - types are bound globally anyway. */
    if(account_type != SCM_UNDEFINED) scm_gc_protect_object(account_type);
  }

  return account_type;
}

int gnc_account_value_pointer_p (SCM arg)
{
  SCM account_type = get_acct_type();

  return (SCM_CONSP (arg) &&
	  gw_wcp_is_of_type_p(account_type, SCM_CAR (arg)) &&
	  gnc_numeric_p (SCM_CDR (arg)));
}

GncAccountValue * gnc_scm_to_account_value_ptr (SCM valuearg)
{
  GncAccountValue *res;
  Account *acc = NULL;
  gnc_numeric value;
  SCM account_type = get_acct_type();
  SCM val;

  /* Get the account */
  val = SCM_CAR (valuearg);
  if (!gw_wcp_is_of_type_p (account_type, val))
    return NULL;

  acc = gw_wcp_get_ptr (val);

  /* Get the value */
  val = SCM_CDR (valuearg);
  value = gnc_scm_to_numeric (val);

  /* Build and return the object */
  res = g_new0 (GncAccountValue, 1);
  res->account = acc;
  res->value = value;
  return res;
}

SCM gnc_account_value_ptr_to_scm (GncAccountValue *av)
{
  SCM account_type = get_acct_type();
  gnc_commodity * com;
  gnc_numeric val;

  if (!av) return SCM_BOOL_F;

  com = xaccAccountGetCommodity (av->account);
  val = gnc_numeric_convert (av->value, gnc_commodity_get_fraction (com),
			     GNC_RND_ROUND);

  return scm_cons (gw_wcp_assimilate_ptr (av->account, account_type),
		   gnc_numeric_to_scm (val));
}
