/********************************************************************
 * window-main-summarybar.c -- summary of financial info            *
 * Copyright (C) 1998,1999 Jeremy Collins	                    *
 * Copyright (C) 1998,1999,2000 Linas Vepstas                       *
 * Copyright (C) 2001 Bill Gribble                                  *
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
 ********************************************************************/

#include "config.h"

#include <gnome.h>
#include <guile/gh.h>
#include <string.h>

#include "Account.h"
#include "EuroUtils.h"
#include "FileDialog.h"
#include "Group.h"
#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gtkselect.h"
#include "messages.h"
#include "option-util.h"
#include "top-level.h"
#include "window-main-summarybar.h"
#include "window-main.h"

typedef struct {
  GtkWidget * hbox;
  GtkWidget * totals_combo;
  GList     * totals_list; 
  int       component_id;
} GNCMainSummary;

#define WINDOW_SUMMARYBAR_CM_CLASS "summary-bar"

/* An accumulator for a given currency.
 *
 * This is used during the update to the status bar to contain the
 * accumulation for a single currency. These are placed in a GList and
 * kept around for the duration of the calculation. There may, in fact
 * be better ways to do this, but none occurred. */

typedef struct {
  gnc_commodity * currency;
  gnc_numeric assets;
  gnc_numeric profits;
} GNCCurrencyAcc;


/* An item to appear in the selector box in the status bar.
 *
 * This is maintained for the duration, where there is one per
 * currency, plus (eventually) one for the default currency
 * accumulation (like the EURO). */

typedef struct  {
  char *namespace;
  char *mnemonic;
  GtkWidget *listitem;
  GtkWidget *assets_label;
  GtkWidget *profits_label;
  gint touched : 1;
} GNCCurrencyItem;


/* Build a single currency item.
 *
 * This function handles the building of a single currency item for
 * the selector. It looks like the old code in the update function,
 * but now only handles a single currency.  */

static GNCCurrencyItem *
gnc_ui_build_currency_item(gnc_commodity * currency)
{
  GtkWidget *label;
  GtkWidget *topbox;
  GtkWidget *hbox;
  GtkWidget *listitem;
  GNCCurrencyItem *item;
  const char *mnemonic;
  char *label_str;

  item = g_new0 (GNCCurrencyItem, 1);

  item->namespace = g_strdup (gnc_commodity_get_namespace (currency));
  item->mnemonic = g_strdup (gnc_commodity_get_mnemonic (currency));

  listitem = gtk_list_item_new();
  item->listitem = listitem;

  topbox = gtk_hbox_new(FALSE, 2);
  gtk_widget_show(topbox);
  gtk_container_add(GTK_CONTAINER(listitem), topbox);

  mnemonic = gnc_commodity_get_mnemonic (currency);

  hbox = gtk_hbox_new(FALSE, 2);
  gtk_widget_show(hbox);
  gtk_box_pack_start(GTK_BOX(topbox), hbox, FALSE, FALSE, 5);

  label_str = g_strdup_printf ("%s (%s):", _("Net Assets"),
                               mnemonic ? mnemonic : "");
  label = gtk_label_new(label_str);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  gtk_widget_show(label);
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
  g_free (label_str);

  label = gtk_label_new("");
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  gtk_box_pack_end(GTK_BOX(hbox), label, FALSE, FALSE, 0);
  gtk_widget_show(label);
  item->assets_label = label;

  hbox = gtk_hbox_new(FALSE, 2);
  gtk_widget_show(hbox);
  gtk_box_pack_start(GTK_BOX(topbox), hbox, FALSE, FALSE, 5);

  label_str = g_strdup_printf ("%s (%s):", _("Profits"),
                               mnemonic ? mnemonic : "");
  label = gtk_label_new(label_str);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  gtk_widget_show(label);
  gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
  g_free (label_str);

  label = gtk_label_new("");
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  gtk_widget_show(label);
  gtk_box_pack_end(GTK_BOX(hbox), label, FALSE, FALSE, 0);
  item->profits_label = label;

  gtk_widget_show(item->listitem);

  return item;
}

static void
gnc_ui_currency_item_destroy (GNCCurrencyItem *item)
{
  if (!item) return;

  g_free (item->namespace);
  g_free (item->mnemonic);

  item->namespace = NULL;
  item->mnemonic = NULL;

  g_free (item);
}

/* Get a currency accumulator.
 *
 * This will search the given list, and if no accumulator is found,
 * will allocate a fresh one. */
static GNCCurrencyAcc *
gnc_ui_get_currency_accumulator(GList **list, gnc_commodity * currency)
{
  GList *current;
  GNCCurrencyAcc *found;

  for (current = g_list_first(*list); current;
       current = g_list_next(current)) {
    found = current->data;
    if (gnc_commodity_equiv(currency, found->currency)) {
      return found;
    }
  }

  found = g_new0 (GNCCurrencyAcc, 1);
  found->currency = currency;
  found->assets = gnc_numeric_zero ();
  found->profits = gnc_numeric_zero ();
  *list = g_list_append (*list, found);

  return found;
}

static gboolean
gnc_ui_currency_item_match (const GNCCurrencyItem *item,
                            const gnc_commodity *commodity)
{
  if (!item || !commodity) return FALSE;

  return
    (safe_strcmp (item->namespace,
                  gnc_commodity_get_namespace (commodity)) == 0) &&
    (safe_strcmp (item->mnemonic,
                  gnc_commodity_get_mnemonic (commodity)) == 0);

}

/* Get a currency item.
 *
 * This will search the given list, and if no accumulator is found, will
 * create a fresh one.
 *
 * It looks just like the function above, with some extra stuff to get
 * the item into the list. */

static GNCCurrencyItem *
gnc_ui_get_currency_item (GList **list,
                          gnc_commodity * currency,
                          GtkWidget *holder)
{
  GList *current;
  GNCCurrencyItem *found;

  for (current = g_list_first(*list); current;
       current = g_list_next(current))
  {
    found = current->data;

    if (gnc_ui_currency_item_match (found, currency))
      return found;
  }

  found = gnc_ui_build_currency_item(currency);
  *list = g_list_append(*list, found);

  current = g_list_append(NULL, found->listitem);
  gtk_select_append_items(GTK_SELECT(holder), current);

  return found;
}

static void
gnc_ui_accounts_recurse (AccountGroup *group, GList **currency_list,
                         gboolean euro)
{
  gnc_numeric amount;
  AccountGroup *children;
  GNCAccountType account_type;  
  gnc_commodity * account_currency;
  gnc_commodity * default_currency;
  gnc_commodity * euro_commodity;
  GNCCurrencyAcc *currency_accum;
  GNCCurrencyAcc *euro_accum = NULL;
  GList *list;
  GList *node;

  default_currency =
    gnc_lookup_currency_option("International",
                               "Default Currency",
                               gnc_locale_default_currency ());

  if (euro)
  {
    euro_commodity = gnc_get_euro ();
    euro_accum = gnc_ui_get_currency_accumulator(currency_list,
                                                 euro_commodity);
  }
  else
    euro_commodity = NULL;

  list = xaccGroupGetAccountList (group);
  for (node = list; node; node = node->next)
  {
    Account *account = node->data;

    account_type = xaccAccountGetType(account);
    account_currency = xaccAccountGetCurrency(account);
    children = xaccAccountGetChildren(account);
    currency_accum = gnc_ui_get_currency_accumulator(currency_list,
						     account_currency);

    switch (account_type)
    {
      case BANK:
      case CASH:
      case ASSET:
      case STOCK:
      case MUTUAL:
      case CREDIT:
      case LIABILITY:
	amount = xaccAccountGetBalance(account);
        currency_accum->assets =
          gnc_numeric_add (currency_accum->assets, amount,
                           gnc_commodity_get_fraction (account_currency),
                           GNC_RND_ROUND);

	if (euro)
	  euro_accum->assets =
            gnc_numeric_add (euro_accum->assets,
                             gnc_convert_to_euro(account_currency, amount),
                             gnc_commodity_get_fraction (euro_commodity),
                             GNC_RND_ROUND);

	if (children != NULL)
	  gnc_ui_accounts_recurse(children, currency_list, euro);
	break;
      case INCOME:
      case EXPENSE:
	amount = xaccAccountGetBalance(account);
        currency_accum->profits =
          gnc_numeric_sub (currency_accum->profits, amount,
                           gnc_commodity_get_fraction (account_currency),
                           GNC_RND_ROUND);

        if (euro)
          gnc_numeric_sub (euro_accum->profits,
                           gnc_convert_to_euro(account_currency, amount),
                           gnc_commodity_get_fraction (euro_commodity),
                           GNC_RND_ROUND);

	if (children != NULL)
	  gnc_ui_accounts_recurse(children, currency_list, euro);
	break;
      case EQUITY:
        /* no-op, see comments at top about summing assets */
	break;
      case CURRENCY:
      default:
	break;
    }
  }
}

/* The gnc_main_window_summary_refresh() subroutine redraws summary
 * information. The statusbar includes two fields, titled 'profits'
 * and 'assets'. The total assets equal the sum of all of the
 * non-equity, non-income accounts.  In theory, assets also equals the
 * grand total value of the equity accounts, but that assumes that
 * folks are using the equity account type correctly (which is not
 * likely). Thus we show the sum of assets, rather than the sum of
 * equities.
 *
 * The EURO gets special treatment. There can be one line with
 * EUR amounts and a EUR (total) line which summs up all EURO
 * member currencies.
 *
 * There should be a 'grand total', too, which sums up all accounts
 * converted to one common currency.  */

static void
gnc_main_window_summary_refresh (GNCMainSummary * summary)
{
  AccountGroup *group;
  char asset_string[256];
  char profit_string[256];
  gnc_commodity * default_currency;
  GNCCurrencyAcc *currency_accum;
  GNCCurrencyItem *currency_item;
  GList *currency_list;
  GList *current;
  gboolean euro;

  default_currency =
    gnc_lookup_currency_option("International",
                               "Default Currency",
                               gnc_locale_default_currency ());

  euro = gnc_lookup_boolean_option("International",
                                   "Enable EURO support",
                                   FALSE);

  currency_list = NULL;

  /* Make sure there's at least one accumulator in the list. */
  gnc_ui_get_currency_accumulator (&currency_list, default_currency);

  group = gncGetCurrentGroup ();
  gnc_ui_accounts_recurse(group, &currency_list, euro);

  for (current = g_list_first(summary->totals_list); current;
       current = g_list_next(current)) {
    currency_item = current->data;
    currency_item->touched = 0;
  }
  
  for (current = g_list_first(currency_list); current;
       current = g_list_next(current)) {
    currency_accum = current->data;
    currency_item = gnc_ui_get_currency_item(&summary->totals_list,
       					     currency_accum->currency,
					     summary->totals_combo);
    currency_item->touched = 1;
    
    *asset_string= '\0';
    xaccSPrintAmount(asset_string, currency_accum->assets,
                     gnc_commodity_print_info(currency_accum->currency, TRUE));
    gtk_label_set_text(GTK_LABEL(currency_item->assets_label), asset_string);
    gnc_set_label_color(currency_item->assets_label, currency_accum->assets);

    *profit_string= '\0';
    xaccSPrintAmount(profit_string, currency_accum->profits,
                     gnc_commodity_print_info(currency_accum->currency, TRUE));
    gtk_label_set_text(GTK_LABEL(currency_item->profits_label), profit_string);
    gnc_set_label_color(currency_item->profits_label, currency_accum->profits);

    g_free(currency_accum);
    current->data = NULL;
  }

  g_list_free(currency_list);
  currency_list = NULL;
  
  current = g_list_first(summary->totals_list);
  while (current) {
    GList *next = current->next;
    
    currency_item = current->data;
    if (currency_item->touched == 0 &&
        !gnc_ui_currency_item_match(currency_item, default_currency)) {
      currency_list = g_list_prepend(currency_list, currency_item->listitem);
      summary->totals_list = g_list_remove_link(summary->totals_list,
                                                current);
      gnc_ui_currency_item_destroy(currency_item);
      current->data = NULL;
      g_list_free_1(current);
    }
    
    current = next;
  }
  
  if (currency_list) {
    gtk_select_remove_items(GTK_SELECT(summary->totals_combo),
                            currency_list);
    g_list_free(currency_list);
  }
}

static void
gnc_main_window_summary_destroy_cb(GtkObject * obj, gpointer data) {
  GNCMainSummary * summary = data;
  gnc_unregister_gui_component(summary->component_id);
  g_list_free(summary->totals_list);
  g_free(summary);
}

static void
summarybar_refresh_handler(GHashTable * changes, gpointer user_data) {
  GNCMainSummary * summary = user_data;
  gnc_main_window_summary_refresh(summary);
}

GtkWidget *
gnc_main_window_summary_new (void) {
  GNCMainSummary  * retval = g_new0(GNCMainSummary, 1);
  GtkWidget       * summarybar;
  GNCCurrencyItem * def_item;
  gnc_commodity   * default_currency =
    gnc_lookup_currency_option ("International",
                                "Default Currency",
                                gnc_locale_default_currency ());
  
  retval->hbox         = gtk_hbox_new (FALSE, 5);  
  retval->totals_combo = gtk_select_new ();
  retval->totals_list  = NULL;
  retval->component_id = gnc_register_gui_component(WINDOW_SUMMARYBAR_CM_CLASS,
                                                    summarybar_refresh_handler,
                                                    NULL, retval);
  gnc_gui_component_watch_entity_type (retval->component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  def_item = gnc_ui_get_currency_item (&retval->totals_list,
                                       default_currency,
                                       retval->totals_combo);
  
  
  gtk_container_set_border_width (GTK_CONTAINER (retval->hbox), 2);
  gtk_select_select_child (GTK_SELECT(retval->totals_combo), 
                           def_item->listitem);
  gtk_box_pack_start (GTK_BOX(retval->hbox), retval->totals_combo, 
                      FALSE, FALSE, 5);
  gtk_widget_show (retval->totals_combo);
  gtk_widget_show (retval->hbox);

  gtk_signal_connect(GTK_OBJECT(retval->hbox), "destroy",
                     gnc_main_window_summary_destroy_cb, retval);

  gnc_main_window_summary_refresh(retval);

  return retval->hbox;
}

