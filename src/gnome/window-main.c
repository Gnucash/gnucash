/********************************************************************\
 * window-main.c -- the main window, and associated helper functions* 
 *                  and callback functions for GnuCash              *
 * Copyright (C) 1998,1999 Jeremy Collins	                    *
 * Copyright (C) 1998,1999,2000 Linas Vepstas                       *
 * Copyright (C) 2001 Bill Gribble                                  *
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
#include <guile/gh.h>
#include <string.h>

#include "AccWindow.h"
#include "EuroUtils.h"
#include "FileBox.h"
#include "FileDialog.h"
#include "MainWindow.h"
#include "RegWindow.h"
#include "Scrub.h"
#include "account-tree.h"
#include "dialog-account.h"
#include "dialog-fincalc.h"
#include "dialog-find-transactions.h"
#include "dialog-options.h"
#include "dialog-totd.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "file-history.h"
#include "global-options.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-ui.h"
#include "gnucash.h"
#include "gtkselect.h"
#include "messages.h"
#include "mainwindow-account-tree.h"
#include "window-help.h"
#include "window-main.h"
#include "window-reconcile.h"
#include "window-register.h"
#include "window-report.h"

#include "io-gncxml-v2.h"
#include "gnc-book.h"

/* FIXME get rid of these */
#include "gnc.h"

#define WINDOW_MAIN_CM_CLASS "window-main"

/* Main Window information structure */
typedef struct _GNCMainInfo GNCMainInfo;
struct _GNCMainInfo
{
  GtkWidget *account_tree;
  GtkWidget *totals_combo;
  GtkWidget *notebook;
  GList *totals_list;

  SCM main_window_change_callback_id;
  SCM euro_change_callback_id;
  SCM toolbar_change_callback_id;

  GList *account_sensitives;

  gint component_id;
};

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

/* Codes for the file menu */
enum {
  FMB_NEW,
  FMB_OPEN,
  FMB_IMPORT,
  FMB_SAVE,
  FMB_SAVEAS,
  FMB_QUIT,
};

/** Static function declarations ***************************************/
static GNCMainInfo * gnc_get_main_info(void);


/* An accumulator for a given currency.
 *
 * This is used during the update to the status bar to contain the
 * accumulation for a single currency. These are placed in a GList and
 * kept around for the duration of the calculation. There may, in fact
 * be better ways to do this, but none occurred. */
struct _GNCCurrencyAcc {
  gnc_commodity * currency;
  gnc_numeric assets;
  gnc_numeric profits;
};
typedef struct _GNCCurrencyAcc GNCCurrencyAcc;

/* An item to appear in the selector box in the status bar.
 *
 * This is maintained for the duration, where there is one per
 * currency, plus (eventually) one for the default currency
 * accumulation (like the EURO). */
struct _GNCCurrencyItem {
  char *namespace;
  char *mnemonic;
  GtkWidget *listitem;
  GtkWidget *assets_label;
  GtkWidget *profits_label;
  gint touched : 1;
};
typedef struct _GNCCurrencyItem GNCCurrencyItem;

/* Build a single currency item.
 *
 * This function handles the building of a single currency item for the
 * selector. It looks like the old code in the update function, but now
 * only handles a single currency.
 */
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

  label_str = g_strdup_printf ("%s (%s):", _("Net Assets"), mnemonic);
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

  label_str = g_strdup_printf ("%s (%s):", _("Profits"), mnemonic);
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

/* The gnc_ui_refresh_statusbar() subroutine redraws the summary
 *    information. The statusbar includes two fields, titled 'profits'
 *    and 'assets'. The total assets equal the sum of all of the
 *    non-equity, non-income accounts.  In theory, assets also equals
 *    the grand total value of the equity accounts, but that assumes
 *    that folks are using the equity account type correctly (which is
 *    not likely). Thus we show the sum of assets, rather than the
 *    sum of equities.
 *
 * The EURO gets special treatment. There can be one line with
 * EUR amounts and a EUR (total) line which summs up all EURO
 * member currencies.
 *
 * There should be a 'grand total', too, which sums up all accounts
 * converted to one common currency.  */
static void
gnc_ui_refresh_statusbar (void)
{
  GNCMainInfo *main_info;
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

  main_info = gnc_get_main_info();
  if (main_info == NULL)
    return;

  currency_list = NULL;

  /* Make sure there's at least one accumulator in the list. */
  gnc_ui_get_currency_accumulator (&currency_list, default_currency);

  group = gncGetCurrentGroup ();
  gnc_ui_accounts_recurse(group, &currency_list, euro);

  for (current = g_list_first(main_info->totals_list); current;
       current = g_list_next(current))
  {
    currency_item = current->data;
    currency_item->touched = 0;
  }

  for (current = g_list_first(currency_list); current;
       current = g_list_next(current))
  {
    currency_accum = current->data;
    currency_item = gnc_ui_get_currency_item(&main_info->totals_list,
       					     currency_accum->currency,
					     main_info->totals_combo);
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

  current = g_list_first(main_info->totals_list);
  while (current)
  {
    GList *next = current->next;

    currency_item = current->data;
    if (currency_item->touched == 0 &&
        !gnc_ui_currency_item_match(currency_item, default_currency))
    {
      currency_list = g_list_prepend(currency_list, currency_item->listitem);
      main_info->totals_list = g_list_remove_link(main_info->totals_list,
						  current);
      gnc_ui_currency_item_destroy(currency_item);
      current->data = NULL;
      g_list_free_1(current);
    }

    current = next;
  }

  if (currency_list)
  {
    gtk_select_remove_items(GTK_SELECT(main_info->totals_combo),
                            currency_list);
    g_list_free(currency_list);
  }
}

static void
gnc_refresh_main_window_title (void)
{
  GtkWidget *main_window;
  GNCBook *book;
  const char *filename;
  char *title;

  main_window = gnc_get_ui_data();
  if (main_window == NULL)
    return;

  book = gncGetCurrentBook ();

  filename = gnc_book_get_url (book);

  if ((filename == NULL) || (*filename == '\0'))
    filename = _("Untitled");

  title = g_strconcat("GnuCash - ", filename, NULL);

  gtk_window_set_title(GTK_WINDOW(main_window), title);

  g_free(title);
}

static void
gnc_refresh_main_window (void)
{
  gnc_ui_refresh_statusbar ();
  gnc_history_update_menu ();
  gnc_refresh_main_window_title ();
}

static void
gnc_ui_find_transactions_cb (GtkWidget *widget, gpointer data)
{
  gnc_ui_find_transactions_dialog_create(NULL);
}


static void
gnc_ui_exit_cb (GtkWidget *widget, gpointer data)
{
  gnc_shutdown (0);
}

static void
gnc_ui_about_cb (GtkWidget *widget, gpointer data)
{
  GtkWidget *about;
  const gchar *message = _("The GnuCash personal finance manager.\n"
                           "The GNU way to manage your money!");
  const gchar *copyright = "(C) 1998-2000 Linas Vepstas";
  const gchar *authors[] = {
    "Linas Vepstas <linas@linas.org>",
    NULL
  };

  about = gnome_about_new("GnuCash", VERSION, copyright,
                          authors, message, NULL);

  gnome_dialog_run_and_close(GNOME_DIALOG(about));
}

static void
gnc_ui_totd_cb (GtkWidget *widget, gpointer data)
{
  gnc_ui_totd_dialog_create_and_run();
  return;
}
    
static void
gnc_ui_help_cb (GtkWidget *widget, gpointer data)
{
  helpWindow(NULL, NULL, HH_MAIN);
}

static void
gnc_ui_add_account (GtkWidget *widget, gpointer data)
{
  gnc_ui_new_account_window (NULL);
}

static void
gnc_ui_delete_account (Account *account)
{
  gnc_suspend_gui_refresh ();

  xaccAccountBeginEdit (account);
  xaccAccountDestroy (account);

  gnc_resume_gui_refresh ();
}

static void
gnc_ui_delete_account_cb (GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account();

  if (account)
  {
    const char *format = _("Are you sure you want to delete the %s account?");
    char *message;
    char *name;

    name = xaccAccountGetFullName(account, gnc_get_account_separator ());
    message = g_strdup_printf(format, name);

    if (gnc_verify_dialog(message, FALSE))
      gnc_ui_delete_account(account);

    g_free(name);
    g_free(message);
  }
  else
  {
    const char *message = _("To delete an account, you must first\n"
                            "choose an account to delete.\n");
    gnc_error_dialog(message);
  }
}

static void
gnc_tax_info_cb (GtkWidget *widget, gpointer data)
{
  gnc_tax_info_dialog (gnc_get_ui_data ());
}

static void
gnc_ui_mainWindow_toolbar_open (GtkWidget *widget, gpointer data)
{
  RegWindow *regData;
  Account *account = gnc_get_current_account();

  if (account == NULL)
  {
    const char *message = _("To open an account, you must first\n"
                            "choose an account to open.");
    gnc_error_dialog(message);
    return;
  }

  PINFO ("calling regWindowSimple(%p)\n", account);

  regData = regWindowSimple(account);
  gnc_register_raise(regData);
}

static void
gnc_ui_mainWindow_toolbar_open_subs(GtkWidget *widget, gpointer data)
{
  RegWindow *regData;
  Account *account = gnc_get_current_account();
  
  if (account == NULL)
  {
    const char *message = _("To open an account, you must first\n"
                            "choose an account to open.");
    gnc_error_dialog(message);
    return;
  }

  PINFO ("calling regWindowAccGroup(%p)\n", account);

  regData = regWindowAccGroup(account);
  gnc_register_raise(regData);
}

static void
gnc_ui_mainWindow_toolbar_edit (GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account();
  AccountWindow *edit_window_data;
  
  if (account != NULL)
  {
    edit_window_data = gnc_ui_edit_account_window(account);
    gnc_ui_edit_account_window_raise(edit_window_data);
  }
  else
  {
    const char *message = _("To edit an account, you must first\n"
                            "choose an account to edit.\n");
    gnc_error_dialog(message);
  }
}

static void
gnc_ui_mainWindow_reconcile(GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account();
  RecnWindow *recnData;

  if (account != NULL)
  {
    recnData = recnWindow(gnc_get_ui_data(), account);
    gnc_ui_reconcile_window_raise(recnData);
  }
  else
  {
    const char *message = _("To reconcile an account, you must first\n"
                            "choose an account to reconcile.");
    gnc_error_dialog(message);
  }
}

static void
gnc_ui_mainWindow_transfer (GtkWidget *widget, gpointer data)
{
  gnc_xfer_dialog (gnc_get_ui_data (), gnc_get_current_account ());
}

static void
gnc_ui_mainWindow_stock_split (GtkWidget *widget, gpointer data)
{
  gnc_stock_split_dialog (gnc_get_current_account ());
}

static void
gnc_ui_mainWindow_scrub(GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account ();

  if (account == NULL)
  {
    const char *message = _("You must select an account to scrub.");
    gnc_error_dialog (message);
    return;
  }

  gnc_suspend_gui_refresh ();

  xaccAccountScrubOrphans (account);
  xaccAccountScrubImbalance (account);

  gnc_resume_gui_refresh ();
}

static void
gnc_ui_mainWindow_scrub_sub(GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account ();

  if (account == NULL)
  {
    const char *message = _("You must select an account to scrub.");
    gnc_error_dialog(message);
    return;
  }

  gnc_suspend_gui_refresh ();

  xaccAccountTreeScrubOrphans (account);
  xaccAccountTreeScrubImbalance (account);

  gnc_resume_gui_refresh ();
}

static void
gnc_ui_mainWindow_scrub_all(GtkWidget *widget, gpointer data)
{
  AccountGroup *group = gncGetCurrentGroup ();

  gnc_suspend_gui_refresh ();

  xaccGroupScrubOrphans (group);
  xaccGroupScrubImbalance (group);

  gnc_resume_gui_refresh ();
}

static void
gnc_ui_options_cb(GtkWidget *widget, gpointer data)
{
  gnc_show_options_dialog();
}

static void
gnc_ui_xml_v2_cb(GtkWidget *widget, gpointer menuItem)
{
    const char *filename;
    GNCBook *book;
    
    filename = fileBox(_("Save"), "*.gnc", gnc_history_get_last());
    if (!filename) return;
    
    book = gncGetCurrentBook ();

    gnc_book_write_to_xml_file_v2(book, filename);
}

static void
gnc_ui_account_hierarchy_cb(GtkWidget *widget, gpointer menuItem)
{
}

static void
gnc_ui_filemenu_cb(GtkWidget *widget, gpointer menuItem)
{
  switch (GPOINTER_TO_INT(menuItem))
  {
    case FMB_NEW:
      gncFileNew();
      break;
    case FMB_OPEN:
      gncFileOpen();
      break;
    case FMB_SAVE:
      gncFileSave();
      gnc_refresh_main_window_title();
      break;
    case FMB_SAVEAS:
      gncFileSaveAs();
      gnc_refresh_main_window_title();
      break;
    case FMB_IMPORT:
      gncFileQIFImport();
      break;
    case FMB_QUIT:
      gnc_shutdown(0);
      break;
    default:
      break;  
  }  
}

static void
gnc_ui_mainWindow_fincalc_cb(GtkWidget *widget, gpointer data)
{
  gnc_ui_fincalc_dialog_create();
}

static void
gnc_ui_mainWindow_gl_cb(GtkWidget *widget, gpointer data)
{
  xaccLedgerDisplay *ld;
  RegWindow *regData;

  ld = xaccLedgerDisplayGL ();

  regData = regWindowLedger (ld);

  gnc_register_raise (regData);
}

static gboolean
gnc_ui_mainWindow_delete_cb(GtkWidget *widget,
			    GdkEvent  *event,
			    gpointer  user_data)
{
  /* Don't allow deletes if we're in a modal dialog */
  if (gtk_main_level() == 1)
    gnc_shutdown(0);

  /* Don't delete the window, we'll handle things ourselves. */
  return TRUE;
}


static gboolean
gnc_ui_mainWindow_destroy_event_cb(GtkWidget *widget,
                                   GdkEvent  *event,
                                   gpointer   user_data)
{
  return FALSE;
}

void
gnc_ui_mainWindow_save_size(void)
{
  GtkWidget *app;
  int width = 0;
  int height = 0;

  app = gnc_get_ui_data();
  if (!app)
    return;

  gdk_window_get_geometry(GTK_WIDGET(app)->window, NULL, NULL,
                          &width, &height, NULL);

  gnc_save_window_size("main_win", width, height);
}

static void
gnc_ui_mainWindow_destroy_cb (GtkObject *object, gpointer user_data)
{
  GNCMainInfo *main_info = user_data;

  gnc_unregister_gui_component (main_info->component_id);

  gnc_unregister_option_change_callback_id
    (main_info->main_window_change_callback_id);

  gnc_unregister_option_change_callback_id
    (main_info->euro_change_callback_id);

  gnc_unregister_option_change_callback_id
    (main_info->toolbar_change_callback_id);

  g_list_free(main_info->account_sensitives);
  main_info->account_sensitives = NULL;

  g_free (main_info);
}

GNCMainWinAccountTree *
gnc_get_current_account_tree (void)
{
  GNCMainInfo *main_info;

  main_info = gnc_get_main_info ();
  if (main_info == NULL)
    return NULL;

  return GNC_MAINWIN_ACCOUNT_TREE (main_info->account_tree);
}

Account *
gnc_get_current_account (void)
{
  GNCMainWinAccountTree *list  = gnc_get_current_account_tree();
  return gnc_mainwin_account_tree_get_current_account(list);
}

GList *
gnc_get_current_accounts(void)
{
  GNCMainWinAccountTree *tree  = gnc_get_current_account_tree();
  return gnc_mainwin_account_tree_get_current_accounts(tree);
} 

static void
gnc_account_tree_activate_cb(GNCMainWinAccountTree *tree,
                             Account *account,
                             gpointer user_data)
{
  RegWindow *regData;
  gboolean expand;

  expand = gnc_lookup_boolean_option("Main Window",
                                     "Double click expands parent accounts",
                                     FALSE);

  if (expand)
  {
    AccountGroup *group;

    group = xaccAccountGetChildren(account);
    if (xaccGroupGetNumAccounts(group) > 0)
    {
      gnc_mainwin_account_tree_toggle_account_expansion(tree, account);
      return;
    }
  }

  regData = regWindowSimple(account);
  gnc_register_raise(regData);
}

static void
gnc_configure_account_tree (gpointer data)
{
  GNCMainInfo *info = data;
  GNCMainWinAccountTree *tree;
  AccountViewInfo new_avi;
  GSList *list, *node;

  memset (&new_avi, 0, sizeof(new_avi));

  tree = GNC_MAINWIN_ACCOUNT_TREE(info->account_tree);

  list = gnc_lookup_list_option("Main Window",
                                "Account types to display",
                                NULL);

  for (node = list; node != NULL; node = node->next)
  {
    if (safe_strcmp(node->data, "bank") == 0)
      new_avi.include_type[BANK] = TRUE;

    else if (safe_strcmp(node->data, "cash") == 0)
      new_avi.include_type[CASH] = TRUE;

    else if (safe_strcmp(node->data, "credit") == 0)
      new_avi.include_type[CREDIT] = TRUE;

    else if (safe_strcmp(node->data, "asset") == 0)
      new_avi.include_type[ASSET] = TRUE;

    else if (safe_strcmp(node->data, "liability") == 0)
      new_avi.include_type[LIABILITY] = TRUE;

    else if (safe_strcmp(node->data, "stock") == 0)
      new_avi.include_type[STOCK] = TRUE;

    else if (safe_strcmp(node->data, "mutual") == 0)
      new_avi.include_type[MUTUAL] = TRUE;

    else if (safe_strcmp(node->data, "currency") == 0)
      new_avi.include_type[CURRENCY] = TRUE;

    else if (safe_strcmp(node->data, "income") == 0)
      new_avi.include_type[INCOME] = TRUE;

    else if (safe_strcmp(node->data, "expense") == 0)
      new_avi.include_type[EXPENSE] = TRUE;

    else if (safe_strcmp(node->data, "equity") == 0)
      new_avi.include_type[EQUITY] = TRUE;
  }

  gnc_free_list_option_value (list);

  list = gnc_lookup_list_option("Main Window",
                                "Account fields to display",
                                NULL);

  for (node = list; node != NULL; node = node->next)
  {
    if (safe_strcmp(node->data, "type") == 0)
      new_avi.show_field[ACCOUNT_TYPE] = TRUE;

    else if (safe_strcmp(node->data, "code") == 0)
      new_avi.show_field[ACCOUNT_CODE] = TRUE;

    else if (safe_strcmp(node->data, "description") == 0)
      new_avi.show_field[ACCOUNT_DESCRIPTION] = TRUE;

    else if (safe_strcmp(node->data, "notes") == 0)
      new_avi.show_field[ACCOUNT_NOTES] = TRUE;

    else if (safe_strcmp(node->data, "currency") == 0)
      new_avi.show_field[ACCOUNT_CURRENCY] = TRUE;

    else if (safe_strcmp(node->data, "security") == 0)
      new_avi.show_field[ACCOUNT_SECURITY] = TRUE;

    else if (safe_strcmp(node->data, "balance") == 0)
    {
      new_avi.show_field[ACCOUNT_BALANCE] = TRUE;
      if(gnc_lookup_boolean_option("International",
                                   "Enable EURO support", FALSE))
	new_avi.show_field[ACCOUNT_BALANCE_EURO] = TRUE;
    }

    else if (safe_strcmp(node->data, "total") == 0)
    {
      new_avi.show_field[ACCOUNT_TOTAL] = TRUE;
      if(gnc_lookup_boolean_option("International",
                                   "Enable EURO support", FALSE))
	new_avi.show_field[ACCOUNT_TOTAL_EURO] = TRUE;
    }
  }

  gnc_free_list_option_value (list);

  new_avi.show_field[ACCOUNT_NAME] = TRUE;

  gnc_mainwin_account_tree_set_view_info (tree, new_avi);
}

static void
gnc_euro_change (gpointer data)
{
  gnc_ui_refresh_statusbar ();
  gnc_configure_account_tree (data);
  gnc_gui_refresh_all ();
}

static void
gnc_main_create_toolbar(GnomeApp *app, GNCMainInfo *main_info)
{
  GList *list;

  static GnomeUIInfo toolbar[] = 
  {
    { GNOME_APP_UI_ITEM,
      N_("Save"),
      N_("Save the file to disk"),
      gnc_ui_filemenu_cb, 
      GINT_TO_POINTER(FMB_SAVE),
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_SAVE,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("Import"),
      N_("Import a Quicken QIF file"),
      gnc_ui_filemenu_cb, 
      GINT_TO_POINTER(FMB_IMPORT),
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CONVERT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM, 
      N_("Open"),
      N_("Open the selected account"),
      gnc_ui_mainWindow_toolbar_open, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_JUMP_TO,
      0, 0, NULL 
    },
    { GNOME_APP_UI_ITEM,
      N_("Edit"),
      N_("Edit the selected account"),
      gnc_ui_mainWindow_toolbar_edit, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_PROPERTIES,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      N_("New"),
      N_("Create a new account"),
      gnc_ui_add_account, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_ADD,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("Delete"),
      N_("Delete selected account"),
      gnc_ui_delete_account_cb, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_REMOVE,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      N_("Find"),
      N_("Find transactions with a search"),
      gnc_ui_find_transactions_cb, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_SEARCH,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      N_("Exit"),
      N_("Exit GnuCash"),
      gnc_ui_exit_cb, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_QUIT,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  gnome_app_create_toolbar(app, toolbar);

  list = main_info->account_sensitives;

  list = g_list_prepend(list, toolbar[3].widget);
  list = g_list_prepend(list, toolbar[4].widget);
  list = g_list_prepend(list, toolbar[7].widget);

  main_info->account_sensitives = list;
}

static void
gnc_main_create_menus(GnomeApp *app, GtkWidget *account_tree,
                      GNCMainInfo *main_info)
{
  GList *list;

  static GnomeUIInfo filemenu[] =
  {
    GNOMEUIINFO_MENU_NEW_ITEM(N_("New File"),
                              N_("Create a new file"),
                              gnc_ui_filemenu_cb,
                              GINT_TO_POINTER(FMB_NEW)),
    GNOMEUIINFO_MENU_OPEN_ITEM(gnc_ui_filemenu_cb,
                               GINT_TO_POINTER(FMB_OPEN)),
    GNOMEUIINFO_MENU_SAVE_ITEM(gnc_ui_filemenu_cb,
                               GINT_TO_POINTER(FMB_SAVE)),
    GNOMEUIINFO_MENU_SAVE_AS_ITEM(gnc_ui_filemenu_cb,
                                  GINT_TO_POINTER(FMB_SAVEAS)),
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Import QIF..."),
      N_("Import a Quicken QIF file"),
      gnc_ui_filemenu_cb, GINT_TO_POINTER(FMB_IMPORT), NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CONVERT,
      'i', GDK_CONTROL_MASK, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    GNOMEUIINFO_MENU_EXIT_ITEM(gnc_ui_filemenu_cb,
                               GINT_TO_POINTER(FMB_QUIT)),
    GNOMEUIINFO_END
  };

  static GnomeUIInfo optionsmenu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Preferences..."),
      N_("Open the global preferences dialog"),
      gnc_ui_options_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo scrubmenu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("Scrub A_ccount"),
      N_("Identify and fix problems in the account"),
      gnc_ui_mainWindow_scrub, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Scrub Su_baccounts"),
      N_("Identify and fix problems in the account "
         "and its subaccounts"),
      gnc_ui_mainWindow_scrub_sub, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Scrub A_ll"),
      N_("Identify and fix problems in all the accounts"),
      gnc_ui_mainWindow_scrub_all, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo accountsmenu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Open Account"),
      N_("Open the selected account"),
      gnc_ui_mainWindow_toolbar_open, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
      'o', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Open S_ubaccounts"),
      N_("Open the selected account and all its subaccounts"),
      gnc_ui_mainWindow_toolbar_open_subs, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Edit Account"),
      N_("Edit the selected account"),
      gnc_ui_mainWindow_toolbar_edit, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PROP,
      'e', GDK_CONTROL_MASK, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_Reconcile..."),
      N_("Reconcile the selected account"),
      gnc_ui_mainWindow_reconcile, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      'r', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Transfer..."),
      N_("Transfer funds from one account to another"),
      gnc_ui_mainWindow_transfer, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      't', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Stock S_plit..."),
      N_("Record a stock split or a stock merger"),
      gnc_ui_mainWindow_stock_split, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("_New Account..."),
      N_("Create a new account"),
      gnc_ui_add_account, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_ADD,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Delete Account"),
      N_("Delete selected account"),
      gnc_ui_delete_account_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_REMOVE,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    GNOMEUIINFO_SUBTREE(N_("_Scrub"), scrubmenu),
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Tax Information"),
      N_("Setup tax information for all income and expense accounts"),
      gnc_tax_info_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo toolsmenu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_General Ledger"),
      N_("Open a general ledger window"),
      gnc_ui_mainWindow_gl_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Financial _Calculator"),
      N_("Use the financial calculator"),
      gnc_ui_mainWindow_fincalc_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("_Find Transactions"),
      N_("Find transactions with a search"),
      gnc_ui_find_transactions_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  static GnomeUIInfo helpmenu[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Manual"),
      N_("Open the GnuCash Manual"),
      gnc_ui_help_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Tips Of The Day"),
      N_("View the Tips of the Day"),
      gnc_ui_totd_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },

    GNOMEUIINFO_MENU_ABOUT_ITEM(gnc_ui_about_cb, NULL),
    
    GNOMEUIINFO_END
  };

  static GnomeUIInfo developer_menu[] =
  {
      {
          GNOME_APP_UI_ITEM,
          N_("_Write V2 XML File"),
          N_("Write a version 2 XML file"),
          gnc_ui_xml_v2_cb, NULL, NULL,
          GNOME_APP_PIXMAP_NONE, NULL,
          0, 0, NULL
      },
      {
          GNOME_APP_UI_ITEM,
          N_("Write _Account Hierarchy"),
          N_("Write just the account hierarchy"),
          gnc_ui_account_hierarchy_cb, NULL, NULL,
          GNOME_APP_PIXMAP_NONE, NULL,
          0, 0, NULL
      },
      GNOMEUIINFO_END
  };
  
  static GnomeUIInfo mainmenu[] =
  {
    GNOMEUIINFO_MENU_FILE_TREE(filemenu),
    GNOMEUIINFO_SUBTREE(N_("_Accounts"), accountsmenu),
    GNOMEUIINFO_SUBTREE(N_("_Tools"), toolsmenu),
    GNOMEUIINFO_MENU_SETTINGS_TREE(optionsmenu),
    GNOMEUIINFO_SUBTREE(N_("_Devel Options"), developer_menu),
    GNOMEUIINFO_MENU_HELP_TREE(helpmenu),
    GNOMEUIINFO_END
  };

  gnome_app_create_menus(app, mainmenu);
  gnome_app_install_menu_hints(app, mainmenu);

  list = main_info->account_sensitives;

  list = g_list_prepend(list, scrubmenu[0].widget);
  list = g_list_prepend(list, scrubmenu[1].widget);

  list = g_list_prepend(list, accountsmenu[0].widget);
  list = g_list_prepend(list, accountsmenu[1].widget);
  list = g_list_prepend(list, accountsmenu[2].widget);
  list = g_list_prepend(list, accountsmenu[4].widget);
  list = g_list_prepend(list, accountsmenu[9].widget);

  gnc_mainwin_account_tree_attach_popup
    (GNC_MAINWIN_ACCOUNT_TREE (account_tree), accountsmenu);

  list = g_list_prepend(list, scrubmenu[0].widget);
  list = g_list_prepend(list, scrubmenu[1].widget);

  list = g_list_prepend(list, accountsmenu[0].widget);
  list = g_list_prepend(list, accountsmenu[1].widget);
  list = g_list_prepend(list, accountsmenu[2].widget);
  list = g_list_prepend(list, accountsmenu[4].widget);
  list = g_list_prepend(list, accountsmenu[9].widget);

  main_info->account_sensitives = list;
}

static GtkWidget *
gnc_main_create_summary_bar (GnomeApp *app, GNCMainInfo *main_info)
{
  GtkWidget *summarybar;
  GtkWidget *combo_box;
  gnc_commodity * default_currency;
  GNCCurrencyItem *def_item;

  summarybar = gtk_hbox_new (FALSE, 5);

  gtk_container_set_border_width (GTK_CONTAINER (summarybar), 2);

  default_currency =
    gnc_lookup_currency_option ("International",
                                "Default Currency",
                                gnc_locale_default_currency ());

  combo_box = gtk_select_new ();
  main_info->totals_combo = combo_box;
  main_info->totals_list = NULL;

  def_item = gnc_ui_get_currency_item (&main_info->totals_list,
                                       default_currency,
                                       main_info->totals_combo);

  gtk_select_select_child (GTK_SELECT(combo_box), def_item->listitem);
  gtk_box_pack_start (GTK_BOX(summarybar), combo_box, FALSE, FALSE, 5);
  gtk_widget_show (combo_box);

  return summarybar;
}

static GNCMainInfo *
gnc_get_main_info (void)
{
  GtkWidget *app = gnc_get_ui_data ();

  if (!app)
    return NULL;

  return gtk_object_get_data (GTK_OBJECT (app), "gnc_main_info");
}

static void
gnc_configure_toolbar (gpointer data)
{
  GtkWidget *app_w = gnc_get_ui_data ();
  GnomeApp *app;
  GnomeDockItem *di;
  GtkWidget *toolbar;
  GtkToolbarStyle tbstyle;

  if (!app_w)
    return;

  app = GNOME_APP (app_w);

  di = gnome_app_get_dock_item_by_name(app, GNOME_APP_TOOLBAR_NAME);
  if (di == NULL)
    return;

  toolbar = gnome_dock_item_get_child(di);
  if (toolbar == NULL)
    return;

  tbstyle = gnc_get_toolbar_style();

  gtk_toolbar_set_style(GTK_TOOLBAR(toolbar), tbstyle);
}

static void
gnc_account_foreach_cb (gpointer widget, gpointer sensitive)
{
  gtk_widget_set_sensitive(GTK_WIDGET(widget), GPOINTER_TO_INT(sensitive));
}

static void
gnc_account_set_sensititives(GNCMainInfo *main_info, gboolean sensitive)
{
  if (main_info == NULL)
    return;

  g_list_foreach(main_info->account_sensitives,
                 gnc_account_foreach_cb,
                 GINT_TO_POINTER(sensitive));
}

static void
gnc_account_cb(GNCMainWinAccountTree *tree, Account *account, gpointer data)
{
  gboolean sensitive;

  account = gnc_mainwin_account_tree_get_current_account(tree);
  sensitive = account != NULL;

  gnc_account_set_sensititives(gnc_get_main_info(), sensitive);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  gnc_refresh_main_window ();
}

void
mainWindow (void)
{
  GNCMainInfo *main_info;
  GtkWidget *app = gnc_get_ui_data();
  GtkWidget *summarybar;
  GtkWidget *statusbar;
  int width = 0;
  int height = 0;

  gnc_get_window_size("main_win", &width, &height);
  if (height == 0)
    height = 400;
  gtk_window_set_default_size(GTK_WINDOW(app), width, height);

  main_info = g_new0 (GNCMainInfo, 1);
  gtk_object_set_data (GTK_OBJECT(app), "gnc_main_info", main_info);

  main_info->component_id = gnc_register_gui_component (WINDOW_MAIN_CM_CLASS,
                                                        refresh_handler, NULL,
                                                        main_info);

  gnc_gui_component_watch_entity_type (main_info->component_id,
                                       GNC_ID_ACCOUNT,
                                       GNC_EVENT_MODIFY | GNC_EVENT_DESTROY);

  main_info->main_window_change_callback_id =
    gnc_register_option_change_callback(gnc_configure_account_tree, main_info,
                                        "Main Window", NULL);

  main_info->euro_change_callback_id =
    gnc_register_option_change_callback(gnc_euro_change, main_info,
                                        "International",
                                        "Enable EURO support");

  summarybar = gnc_main_create_summary_bar (GNOME_APP(app), main_info);
  if (summarybar)
  {
    GnomeDockItem *dock_item;
    
    gnome_app_add_docked (GNOME_APP(app), GTK_WIDGET(summarybar),
                          "Summary Bar", GNOME_DOCK_ITEM_BEH_EXCLUSIVE,
                          GNOME_DOCK_TOP, 2, 0, 0);
 
    dock_item = gnome_app_get_dock_item_by_name (GNOME_APP(app),
                                                 "Summary Bar");
    gnome_dock_item_set_shadow_type (dock_item, GTK_SHADOW_OUT);
  }

  /* create statusbar and add it to the application. */
  statusbar = gnome_appbar_new(FALSE, /* no progress bar, maybe later? */
			       TRUE,  /* has status area */
			       GNOME_PREFERENCES_USER /* recommended */);

  gnome_app_set_statusbar(GNOME_APP(app), GTK_WIDGET(statusbar));


  gnc_main_create_toolbar(GNOME_APP(app), main_info);
  gnc_configure_toolbar(NULL);
  main_info->toolbar_change_callback_id =
    gnc_register_option_change_callback(gnc_configure_toolbar, NULL,
                                        "General", "Toolbar Buttons");

  /* create scrolled window */
  main_info->notebook = gtk_notebook_new();
  gtk_notebook_set_show_border(GTK_NOTEBOOK(main_info->notebook), TRUE);
  gtk_notebook_set_show_tabs(GTK_NOTEBOOK(main_info->notebook), TRUE);
  gtk_notebook_set_tab_pos(GTK_NOTEBOOK(main_info->notebook), 
                           GTK_POS_LEFT);

  main_info->account_tree = gnc_mainwin_account_tree_new();

  gtk_notebook_append_page(GTK_NOTEBOOK(main_info->notebook),
                           main_info->account_tree,
                           gtk_label_new("Accounts"));
  
  /* gnome_app_set_contents(GNOME_APP(app), main_info->account_tree); */
  gnome_app_set_contents(GNOME_APP(app), main_info->notebook); 
  
  gnc_main_create_menus(GNOME_APP(app), main_info->account_tree, main_info);

  gtk_signal_connect(GTK_OBJECT(main_info->account_tree), "activate_account",
		     GTK_SIGNAL_FUNC (gnc_account_tree_activate_cb), NULL);

  gtk_signal_connect(GTK_OBJECT(main_info->account_tree), "select_account",
                     GTK_SIGNAL_FUNC(gnc_account_cb), NULL);

  gtk_signal_connect(GTK_OBJECT(main_info->account_tree), "unselect_account",
                     GTK_SIGNAL_FUNC(gnc_account_cb), NULL);


  /* Attach delete and destroy signals to the main window */  
  gtk_signal_connect (GTK_OBJECT (app), "delete_event",
                      GTK_SIGNAL_FUNC (gnc_ui_mainWindow_delete_cb),
                      NULL);

  gtk_signal_connect (GTK_OBJECT (app), "destroy_event",
                      GTK_SIGNAL_FUNC (gnc_ui_mainWindow_destroy_event_cb),
                      NULL);

  gtk_signal_connect (GTK_OBJECT (app), "destroy",
                      GTK_SIGNAL_FUNC (gnc_ui_mainWindow_destroy_cb),
                      main_info);

  /* Show everything now that it is created */
  gtk_widget_show (summarybar);
  gtk_widget_show (statusbar);
  gtk_widget_show_all (main_info->notebook);
  gtk_widget_show (main_info->account_tree);

  gnc_configure_account_tree (main_info);

  gnc_refresh_main_window ();
  gnc_account_tree_refresh
    (GNC_MAINWIN_ACCOUNT_TREE (main_info->account_tree)->acc_tree);

  gnc_account_set_sensititives(main_info, FALSE);

  gtk_widget_grab_focus(main_info->account_tree);
} 

static void
component_handler (const char *class, gint component_id, gpointer iter_data)
{
  GNCMainInfo *info = iter_data;

  if (info->component_id == component_id)
    return;

  gnc_close_gui_component (component_id);
}

void
gnc_ui_destroy_all_subwindows (void)
{
  GNCMainInfo *info = gnc_get_main_info ();

  gnc_suspend_gui_refresh ();

  gnc_forall_gui_components (NULL, component_handler, info);

  gnc_resume_gui_refresh ();
}

void
gnc_report_in_main_window (int report_id) {
  GNCMainInfo       * mainwin = gnc_get_main_info();
  GtkWidget         * fr = gtk_frame_new(NULL);
  gnc_report_window * reptwin = gnc_report_window_new(fr);
  SCM               get_report = gh_eval_str("gnc:find-report");
  SCM               get_name = gh_eval_str("gnc:report-name");
  char              * report_name;
  
  gtk_frame_set_shadow_type(GTK_FRAME(fr), GTK_SHADOW_NONE);

  report_name = gh_scm2newstr(gh_call1(get_name, 
                                       gh_call1(get_report, 
                                                gh_int2scm(report_id))),
                              NULL);                                       
  gtk_widget_show_all(fr);
  gtk_notebook_append_page(GTK_NOTEBOOK(mainwin->notebook),
                           fr, gtk_label_new(report_name));
  
  gnc_report_window_show_report(reptwin, report_id);
}

/********************* END OF FILE **********************************/
