/*
 * gncBusGuile.c -- Business Guile Helper Functions
 * Copyright (C) 2003 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

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
    if(account_type != SCM_UNDEFINED) scm_protect_object(account_type);
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
