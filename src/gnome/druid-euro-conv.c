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

  GList * account_list;
  GHashTable * currency_hash;

} EuroConvInfo;

/* for the accounts that belong together. */
typedef struct
{
  Account * old_account;
  Account * eur_account;
} AccountPair;

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
check_account_selection (Account *a, EuroConvInfo *info)
{
  /* Check whether this account is to be converted, according to the
     account selection in the dialog. */
  AccountGroup *p, *c;

  if (gtk_toggle_button_get_active
      (GTK_TOGGLE_BUTTON(info->radiobutton_newtoplevel)))
    return TRUE;

  p = xaccAccountGetParent(a);
  if ( p == gncGetCurrentGroup() )
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

static int
fill_account_list (EuroConvInfo *info)
{
  GList *all_accounts, *node, *accounts;
  gint num_accounts = 0;
  GHashTable *currencyhash = NULL;
  CurrencyAccount *currencyinfo = NULL;;

  printf("radiobuttons: toplevel %d, subtoplevel %d, leaf %d\n",
	 gtk_toggle_button_get_active
	 (GTK_TOGGLE_BUTTON(info->radiobutton_newtoplevel)),
	 gtk_toggle_button_get_active
	 (GTK_TOGGLE_BUTTON(info->radiobutton_sametoplevel)),
	 gtk_toggle_button_get_active
	 (GTK_TOGGLE_BUTTON(info->radiobutton_leafaccounts)));

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
    if (account_type == INCOME ||
	account_type == EXPENSE)
      continue;

    currency = xaccAccountGetCurrency (account);
    
    /* only continue for Euroland currencies */
    if (!gnc_is_euro_currency(currency) ||
	gnc_commodity_equiv(currency, gnc_get_euro())) 
      continue;

    /* check our settings */
    if (!check_account_selection(account,info))
      continue;

    /* okay, this one needs conversion, so we store it. */
    num_accounts++;
    accounts = g_list_prepend(accounts, account);
    printf("Account needs conversion: %s\n",
	   xaccAccountGetName (account));

    /* record the currency */
    if (!g_hash_table_lookup(currencyhash, currency))
      {
	currencyinfo = g_new0 (CurrencyAccount, 1);
	currencyinfo->currency = currency;
	g_hash_table_insert(currencyhash, currency, currencyinfo);
      };
  }
  
  info->account_list = g_list_reverse(accounts);
  info->currency_hash = currencyhash;
  printf("Total %d accounts to be converted.\n",num_accounts);

  return num_accounts;
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
