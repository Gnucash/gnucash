/********************************************************************\
 * druid-euro-conv.c -- Euro conversion druid for GnuCash           *
 * Copyright (C) 2001 Christian Stimming <stimming@tuhh.de>         *
 * shamelessly copied from druid-stock-split.c by Dave Peticolas...
 *                                                                  *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "FileDialog.h"
#include "Group.h"
#include "account-tree.h"
#include "dialog-utils.h"
#include "druid-utils.h"
#include "glade-gnc-dialogs.h"
#include "glade-support.h"
#include "gnc-component-manager.h" 
#include "gnc-ui.h" 
#include "messages.h" 
#include "query-user.h" 
#include "EuroUtils.h"
/* #include "gnc-amount-edit.h" */
/* #include "gnc-dateedit.h" */
/* #include "gnc-exp-parser.h" */


#define DRUID_EURO_CONV_CM_CLASS "druid-euro-conv"


/** structures *********************************************************/
typedef struct
{
  GtkWidget * window;
  GtkWidget * druid;

  /* The selection radiobuttons. */
  GtkWidget *radiobutton_newtoplevel;
  GtkWidget *radiobutton_sametoplevel;
  GtkWidget *radiobutton_leafaccounts;
  GtkWidget *radiobutton_new_eur;
  GtkWidget *radiobutton_old_euroland;

  /* the list of all accounts that are to be converted. */
  GList * account_list;
  /* a hash of struct CurrencyAccount to find the appropriate exchange accounts. */
  GHashTable * currency_hash;
  /* a hash of old_account vs. new_account to create the hierarchy. */
  GHashTable * account_hash;
} EuroConvInfo;


/* and where the currency accounts are remembered */
typedef struct
{
  gnc_commodity * currency;
  Account * account;
} CurrencyAccount;



/** implementations ****************************************************/
static void
window_destroy_cb (GtkObject *object, gpointer data)
{
  EuroConvInfo *info = data;

  gnc_unregister_gui_component_by_data (DRUID_EURO_CONV_CM_CLASS, info);

  g_free (info);
}

static gboolean
account_is_toplevel(Account *a)
{
  return (xaccAccountGetParent(a) == gncGetCurrentGroup() );
}

/* return true if this account shall be converted to EUR, else false. */
static gboolean
check_account_selection (Account *a, EuroConvInfo *info)
{
  /* Check whether this account is to be converted, according to the
     account selection in the dialog. */
  AccountGroup *p, *c;

  if (gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON(info->radiobutton_newtoplevel)))
    return TRUE;

  if ( account_is_toplevel(a) )
    return FALSE;

  if (gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON(info->radiobutton_leafaccounts)))
    {
      c = xaccAccountGetChildren(a);
      if (xaccGroupGetNumAccounts(c) == 0)
	return TRUE;
      else
	return FALSE;
    }
  else
    return TRUE;
}

/* fills the info->account_list with all accounts that should be
   converted, plus the info->currency_hash with all currencies that
   should be converted. */
static int
fill_account_list (EuroConvInfo *info)
{
  GList *all_accounts, *node, *accounts;
  gint num_accounts = 0;
  GHashTable *currencyhash = NULL;
  CurrencyAccount *currencyinfo = NULL;
  gnc_numeric amount;

  if (info->account_list != NULL)
    g_list_free(info->account_list);
  if (info->currency_hash != NULL)
    g_hash_table_destroy(info->currency_hash);

  accounts = g_list_alloc ();
  currencyhash = g_hash_table_new (g_direct_hash, g_direct_equal);

  all_accounts = xaccGroupGetSubAccounts (gncGetCurrentGroup ());

  for (node = all_accounts; node; node = node->next)
  {
    Account *account = node->data;
    gnc_commodity *currency;
    GNCAccountType account_type;

    account_type = xaccAccountGetType (account);
    /* if (account_type == INCOME ||
       account_type == EXPENSE)
       continue; -- nope, those need to be created as well. */

    currency = xaccAccountGetCurrency (account);
    
    /* only continue for Euroland currencies */
    if (!gnc_is_euro_currency(currency) ||
	gnc_commodity_equiv(currency, gnc_get_euro())) 
      continue;

    if (account_type == CURRENCY &&
	TRUE && /* some GUI query here */
	gnc_commodity_equiv (xaccAccountGetSecurity (account),
			     gnc_get_euro ()))
      {
	/* Has this currency already been recorded? */
	currencyinfo = g_hash_table_lookup (currencyhash, currency);
	if (!currencyinfo)
          {
	    /* take this as an exchange account, record this currency */
	    currencyinfo = g_new0 (CurrencyAccount, 1);
	    currencyinfo->currency = currency;
	    currencyinfo->account = account;
	    g_hash_table_insert(currencyhash, currency, currencyinfo);
	  }
	else
	  {
	    /* record this account as exchange account for this currency */
	    currencyinfo->account = account;
	  }
      }
    else
      {
	/* check the user's preferences */
	if (!check_account_selection(account,info))
	  continue;

	/* okay, this one needs conversion, so we store it. */
	accounts = g_list_prepend(accounts, account);
	
	num_accounts++;
	/*     printf("Account needs conversion: %s\n", */
	/* 	   xaccAccountGetName (account)); */
	
	amount = xaccAccountGetBalance(account);
	/* record the currency, but only if this account's amount needs to
	   be exchanged */
	if (!g_hash_table_lookup(currencyhash, currency) &&
	    account_type != INCOME &&
	    account_type != EXPENSE &&
	    !gnc_numeric_zero_p(amount))
	  {
	    currencyinfo = g_new0 (CurrencyAccount, 1);
	    currencyinfo->currency = currency;
	    currencyinfo->account = NULL;
	    g_hash_table_insert(currencyhash, currency, currencyinfo);
	  };
      }
  }
  
  info->account_list = g_list_reverse(accounts);
  info->currency_hash = currencyhash;
  info->account_hash = g_hash_table_new (g_direct_hash, g_direct_equal);
  /*   printf("Total %d accounts to be converted.\n",num_accounts); */

  return num_accounts;
}

/* function will be executed once for each currency that we need to exchange. */
static void
foreach_currency_cb (gpointer key, gpointer value, gpointer user_data)
{
  CurrencyAccount *caccount = value;
  EuroConvInfo *info = user_data;
  Account *a;
  char *accountname;

  g_return_if_fail (caccount != NULL);
  g_return_if_fail (info != NULL);
  /*   printf("Called upon %s.\n",  */
  /* 	 gnc_commodity_get_printname(caccount->currency)); */

  /* Already havin an account? Don't do anything. */
  if (caccount->account != NULL)
    return;

  accountname = 
    g_strdup_printf 
    ("%s %s - EUR",
     /* Base name for the Euro exchange account. The currency
	mnemonics get appended, e.g. 'Exchange DEM - EUR'.*/
     _("Exchange"),
     gnc_commodity_get_mnemonic(caccount->currency));

  /* Create the new currency exchange account. */
  a = xaccMallocAccount();
  xaccAccountBeginEdit(a);
  xaccAccountSetType(a, CURRENCY);
  xaccAccountSetName(a, accountname);
  /* Account description for the Euro-related currency exchange accounts. */
  xaccAccountSetDescription(a, _("Euro conversion"));
  xaccAccountSetCurrency(a, caccount->currency);
  xaccAccountSetSecurity(a, gnc_get_euro() );
  /* insert it into the toplevel group */
  xaccGroupInsertAccount(gncGetCurrentGroup(), a);
  xaccAccountCommitEdit(a);

  /* and store a pointer in the hash_table */
  caccount->account = a;
}

/* function where the exchange transactions are created and
   inserted. old_a is of Euroland currency, *exchange is of the right
   Euroland currency and with Security EUR, new_a is of currency
   EUR. */
static void
exchange_amounts(Account *old_a, Account *exchange, Account *new_a)
{
  Transaction *trans;
  Split *split;
  gnc_numeric amount, eur_amount, share_amount, eur_share_amount;
  GNCAccountType account_type;

  g_return_if_fail (old_a != NULL);
  g_return_if_fail (exchange != NULL);
  g_return_if_fail (new_a != NULL);
  /*   printf("Exchanging from %s over %s to %s\n",  */
  /* 	 xaccAccountGetName (old_a), */
  /* 	 xaccAccountGetName (exchange), */
  /* 	 xaccAccountGetName (new_a)); */

  amount = xaccAccountGetBalance(old_a);
  if (!gnc_numeric_zero_p(amount))
    {
      eur_amount = gnc_convert_to_euro (xaccAccountGetCurrency(old_a),
					amount);
      account_type = xaccAccountGetType (old_a);
      if ((account_type == STOCK) ||
	  (account_type == MUTUAL))
	{
	  share_amount = xaccAccountGetShareBalance(old_a);
	  eur_share_amount = share_amount;
	}
      else
	{
	  share_amount = amount;
	  eur_share_amount = eur_amount;
	};
	  
      xaccAccountBeginEdit (old_a);
      xaccAccountBeginEdit (exchange);
      xaccAccountBeginEdit (new_a);
      
      /* Transaction from old account to exchange account */
      trans = xaccMallocTransaction ();
      xaccTransBeginEdit (trans);
      
      split = xaccMallocSplit ();
      xaccSplitSetAmount (split, gnc_numeric_neg(share_amount));
      xaccSplitSetValue (split, gnc_numeric_neg(amount));
      xaccTransAppendSplit (trans, split);
      xaccAccountInsertSplit (old_a, split);
  
      split = xaccMallocSplit ();
      xaccTransAppendSplit (trans, split);
      xaccSplitSetValue (split, amount);
      xaccSplitSetAmount (split, eur_amount);
      xaccAccountInsertSplit (exchange, split);
      
      xaccTransSetDescription (trans, _("Euro conversion"));     
      xaccTransSetDateToday (trans);
      xaccTransCommitEdit (trans);

      /* Transaction from exchange account to new account */
      trans = xaccMallocTransaction ();
      xaccTransBeginEdit (trans);
      
      split = xaccMallocSplit ();
      xaccSplitSetAmount (split, eur_share_amount);
      xaccSplitSetValue (split, eur_amount);
      xaccTransAppendSplit (trans, split);
      xaccAccountInsertSplit (new_a, split);
      
      split = xaccMallocSplit ();
      xaccTransAppendSplit (trans, split);
      xaccSplitSetValue (split, gnc_numeric_neg(amount));
      xaccSplitSetAmount (split, gnc_numeric_neg(eur_amount));
      xaccAccountInsertSplit (exchange, split);

      /* Transaction description for the Euro conversion-related transaction. */
      xaccTransSetDescription (trans, _("Euro conversion"));     
      xaccTransSetDateToday (trans);
      xaccTransCommitEdit (trans);
      
      xaccAccountCommitEdit(old_a);
      xaccAccountCommitEdit(exchange);
      xaccAccountCommitEdit(new_a);
    };
}


/* function where the work happens. Gets executed once for each account that
   we exchange to EUR. */
static void
foreach_account_cb (gpointer data, gpointer user_data)
{
  Account *old_a = data;
  EuroConvInfo *info = user_data;
  Account *new_a, *parent, *new_parent;
  char *new_name, *old_name;
  AccountGroup *parentGroup;
  CurrencyAccount *caccount;
  GNCAccountType account_type;

  if (old_a == NULL)
    return;

  g_return_if_fail (old_a != NULL);
  g_return_if_fail (info != NULL);

  /* Naming scheme; new_name holds the name for the new account. */
  if (gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON(info->radiobutton_new_eur)))
    new_name = g_strdup_printf ("%s EUR",
				xaccAccountGetName (old_a));
  else
    {
      new_name = g_strdup( xaccAccountGetName (old_a));
      old_name = g_strdup_printf ("%s %s",
				  new_name,
				  gnc_commodity_get_mnemonic
				  (xaccAccountGetCurrency(old_a)));
      xaccAccountBeginEdit(old_a);
      xaccAccountSetName(old_a, old_name);
      xaccAccountCommitEdit(old_a);
    };

  /* Create the new currency exchange account. */
  new_a = xaccCloneAccountSimple(old_a);
  xaccAccountBeginEdit(new_a);
  xaccAccountSetName(new_a, new_name);
  xaccAccountSetCurrency(new_a, gnc_get_euro());

  /* Where to put this account? */
  parentGroup = xaccAccountGetParent(old_a);
  if (gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON(info->radiobutton_leafaccounts)))
    xaccGroupInsertAccount(parentGroup, new_a);
  else if (account_is_toplevel(old_a))
    xaccGroupInsertAccount(parentGroup, new_a);
  else
    {
      parent = xaccAccountGetParentAccount(old_a);
      new_parent = 
	(Account*) g_hash_table_lookup(info->account_hash, parent);
      if (new_parent != NULL)
	xaccAccountInsertSubAccount(new_parent, new_a);
      else
	xaccGroupInsertAccount(parentGroup, new_a);
    };
  xaccAccountCommitEdit(new_a);
  /* ---------- Finished creating the new account. */

  /* Store a reference to this newly created account. */
  g_hash_table_insert(info->account_hash, old_a, new_a);

  /* Now the exchange transactions have to be made. */

  /* Make amount exchange only for non-income/expense accounts */
  account_type = xaccAccountGetType (old_a);
  if (account_type == INCOME ||
      account_type == EXPENSE)
    return;

  /* Find the exchange account. */
  caccount = (CurrencyAccount *) 
    g_hash_table_lookup(info->currency_hash, 
			xaccAccountGetCurrency(old_a));
  g_return_if_fail (caccount != NULL);
  g_return_if_fail (caccount->account != NULL);

  exchange_amounts(old_a, caccount->account, new_a);

}

static void
euro_conv_finish (GnomeDruidPage *druidpage,
		  gpointer arg1,
		  gpointer user_data)
{
  EuroConvInfo *info = user_data;
  GList *accounts;
  GList *node;

  gnc_numeric amount;
  Transaction *trans;
  Account *account;
  Split *split;
  time_t date;

  if (fill_account_list (info) == 0)
    {
      gnc_warning_dialog (_("You don't have any accounts of Euroland currencies."));
      gnc_close_gui_component_by_data (DRUID_EURO_CONV_CM_CLASS, info);
      return;
    }

  gnc_suspend_gui_refresh ();

  g_hash_table_foreach(info->currency_hash,
		       foreach_currency_cb, 
		       info);

  g_list_foreach(info->account_list,
		 foreach_account_cb, 
		 info);
  
  gnc_resume_gui_refresh ();

  gnc_close_gui_component_by_data (DRUID_EURO_CONV_CM_CLASS, info);
}

static void
druid_cancel (GnomeDruid *druid, gpointer user_data)
{
  EuroConvInfo *info = user_data;

  gnc_close_gui_component_by_data (DRUID_EURO_CONV_CM_CLASS, info);
}

static void
close_handler (gpointer user_data)
{
  EuroConvInfo *info = user_data;

  gtk_widget_destroy (info->window);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  EuroConvInfo *info = user_data;
}

static void
gnc_euro_conv_druid_create (EuroConvInfo *info)
{
  GtkWidget *page;

  info->window = create_Euro_Conversion_Druid ();

  info->druid = lookup_widget (info->window, "euro_conv_druid");

  gtk_signal_connect (GTK_OBJECT (info->window), "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), info);

  gtk_signal_connect (GTK_OBJECT (info->druid), "cancel",
                      GTK_SIGNAL_FUNC (druid_cancel), info);

  gnc_druid_set_logo_image (GNOME_DRUID(info->druid), 
			     "one-euro.png"); 
  gnc_druid_set_watermark_image (GNOME_DRUID(info->druid), 
				 "euro-notes.png"); 

  page = lookup_widget (info->window, "finish_page");

  info->radiobutton_newtoplevel =
    lookup_widget (info->window, "radiobutton_newtoplevel");
  info->radiobutton_sametoplevel =
    lookup_widget (info->window, "radiobutton_sametoplevel");
  info->radiobutton_leafaccounts =
    lookup_widget (info->window, "radiobutton_leafaccounts");
  info->radiobutton_new_eur =
    lookup_widget (info->window, "radiobutton_new_eur");
  info->radiobutton_old_euroland =
    lookup_widget (info->window, "radiobutton_old_euroland");

  gtk_signal_connect (GTK_OBJECT (page), "finish",
                      GTK_SIGNAL_FUNC (euro_conv_finish), info);
}

/********************************************************************\
 * gnc_euro_conv_dialog                                           *
 *   opens up a window                                         *
 *                                                                  * 
 * Args:   nothing                     *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_euro_conv_dialog ()
{
  EuroConvInfo *info;
  gint component_id;

  info = g_new0 (EuroConvInfo, 1);

  gnc_euro_conv_druid_create (info);

  component_id = gnc_register_gui_component (DRUID_EURO_CONV_CM_CLASS,
                                             refresh_handler, close_handler,
                                             info);

  gnc_gui_component_watch_entity_type (component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  gtk_widget_show_all (info->window);

  gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}
