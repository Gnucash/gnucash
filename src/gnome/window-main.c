/********************************************************************\
 * window-main.c -- the main window, and associated helper functions* 
 *                  and callback functions for GnuCash              *
 * Copyright (C) 1998,1999 Jeremy Collins	                    *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "top-level.h"

#include <gnome.h>
#include <guile/gh.h>
#include <string.h>

#include "gnome-top-level.h"
#include "AccWindow.h"
#include "AdjBWindow.h"
#include "global-options.h"
#include "FileDialog.h"
#include "g-wrap.h"
#include "gnucash.h"
#include "MainWindow.h"
#include "Destroy.h"
#include "enriched-messages.h"
#include "RegWindow.h"
#include "Refresh.h"
#include "window-main.h"
#include "window-mainP.h"
#include "window-reconcile.h"
#include "window-register.h"
#include "window-help.h"
#include "account-tree.h"
#include "dialog-transfer.h"
#include "dialog-edit.h"
#include "Scrub.h"
#include "util.h"
#include "gnc.h"


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


static void
gnc_ui_refresh_statusbar()
{
  GNCMainInfo *main_info;
  double assets  = 0.0;
  double profits = 0.0;
  double amount;
  AccountGroup *group;
  AccountGroup *children;
  Account *account;
  char *asset_string;
  char *profit_string;
  int num_accounts;
  int account_type;
  int i;

  main_info = gnc_get_main_info();
  if (main_info == NULL)
    return;

  group = gncGetCurrentGroup ();
  num_accounts = xaccGroupGetNumAccounts(group);
  for (i = 0; i < num_accounts; i++)
  {
    account = xaccGroupGetAccount(group, i);

    account_type = xaccAccountGetType(account);
    children = xaccAccountGetChildren(account);

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
	if (children != NULL)
	  amount += xaccGroupGetBalance(children);

        if (gnc_reverse_balance(account))
          amount = -amount;

        assets += amount;
	break;
      case INCOME:
      case EXPENSE:
	amount = xaccAccountGetBalance(account);
	if (children != NULL)
	  amount += xaccGroupGetBalance(children);

        if (gnc_reverse_balance(account))
          amount = -amount;

        profits += amount;
	break;
      case EQUITY:
      case CURRENCY:
      default:
	break;
    }
  }

  asset_string = xaccPrintAmount(assets, PRTSYM | PRTSEP);
  gtk_label_set_text(GTK_LABEL(main_info->assets_label), asset_string);
  gnc_set_label_color(main_info->assets_label, assets);

  profit_string = xaccPrintAmount(profits, PRTSYM | PRTSEP);
  gtk_label_set_text(GTK_LABEL(main_info->profits_label), profit_string);
  gnc_set_label_color(main_info->profits_label, profits);
}

/* Required for compatibility with Motif code. */
void
gnc_refresh_main_window()
{
  xaccRecomputeGroupBalance(gncGetCurrentGroup());
  gnc_ui_refresh_statusbar();
  gnc_account_tree_refresh_all();
}

static void
gnc_ui_exit_cb ( GtkWidget *widget, gpointer data )
{
  gnc_shutdown(0);
}

static void
gnc_ui_about_cb (GtkWidget *widget, gpointer data)
{
  GtkWidget *about;
  const gchar *copyright = "(C) 1998-2000 Linas Vepstas";
  const gchar *authors[] = {
    "Linas Vepstas <linas@linas.org>",
    NULL
  };

  about = gnome_about_new("GnuCash", VERSION, copyright,
                          authors, ABOUT_MSG, NULL);

  gnome_dialog_run_and_close(GNOME_DIALOG(about));
}

static void
gnc_ui_help_cb ( GtkWidget *widget, gpointer data )
{
  helpWindow(NULL, HELP_STR, HH_MAIN);
}

static void
gnc_ui_add_account ( GtkWidget *widget, gpointer data )
{
  accWindow(NULL);
}

static void
gnc_ui_delete_account ( Account *account )
{
  /* Step 1: Remove the account from all trees */
  gnc_account_tree_remove_account_all(account);

  /* Step 2: Delete associated windows */
  xaccAccountWindowDestroy(account);

  /* Step 3: Delete the actual account */  
  xaccRemoveAccount(account);
  xaccFreeAccount(account);

  /* Step 4: Refresh things */
  gnc_ui_refresh_statusbar();
  gnc_group_ui_refresh(gncGetCurrentGroup());
}

static void
gnc_ui_delete_account_cb ( GtkWidget *widget, gpointer data )
{
  Account *account = gnc_get_current_account();

  if (account)
  {
    gchar *name;
    gchar *message;

    name = xaccAccountGetName(account);
    message = g_strdup_printf(ACC_DEL_SURE_MSG, name);

    if (gnc_verify_dialog(message, GNC_F))
      gnc_ui_delete_account(account);

    g_free(message);
  }
  else
    gnc_error_dialog(ACC_DEL_MSG);
}

static void
gnc_ui_mainWindow_toolbar_open ( GtkWidget *widget, gpointer data )
{
  RegWindow *regData;
  Account *account = gnc_get_current_account();
  
  if (account == NULL)
  {
    gnc_error_dialog(ACC_OPEN_MSG);
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
    gnc_error_dialog(ACC_OPEN_MSG);
    return;
  }

  PINFO ("calling regWindowAccGroup(%p)\n", account);

  regData = regWindowAccGroup(account);
  gnc_register_raise(regData);
}

static void
gnc_ui_mainWindow_toolbar_edit ( GtkWidget *widget, gpointer data )
{
  Account *account = gnc_get_current_account();
  EditAccWindow *edit_window_data;
  
  if (account != NULL)
  {
    edit_window_data = editAccWindow(account);
    gnc_ui_edit_account_window_raise(edit_window_data);
  }
  else
    gnc_error_dialog(ACC_EDIT_MSG);
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
    gnc_error_dialog(ACC_RECONCILE_MSG);
}

static void
gnc_ui_mainWindow_transfer(GtkWidget *widget, gpointer data)
{
  gnc_xfer_dialog(gnc_get_ui_data(), gnc_get_current_account());
}

static void
gnc_ui_mainWindow_adjust_balance(GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account();

  if (account != NULL)
    adjBWindow(account);
  else
    gnc_error_dialog(ACC_ADJUST_MSG);
}

static void
gnc_ui_mainWindow_scrub(GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account();

  if (account == NULL)
  {
    gnc_error_dialog(ACC_SCRUB_MSG);
    return;
  }

  xaccAccountScrubOrphans(account);
  xaccAccountScrubImbalance(account);

  gnc_account_ui_refresh(account);
  gnc_refresh_main_window();
}

static void
gnc_ui_mainWindow_scrub_sub(GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account();

  if (account == NULL)
  {
    gnc_error_dialog(ACC_SCRUB_MSG);
    return;
  }

  xaccAccountTreeScrubOrphans(account);
  xaccAccountTreeScrubImbalance(account);

  gnc_account_ui_refresh(account);
  gnc_refresh_main_window();
}

static void
gnc_ui_mainWindow_scrub_all(GtkWidget *widget, gpointer data)
{
  AccountGroup *group = gncGetCurrentGroup();

  xaccGroupScrubOrphans(group);
  xaccGroupScrubImbalance(group);

  gnc_group_ui_refresh(group);
  gnc_refresh_main_window();
}

static void
gnc_ui_options_cb(GtkWidget *widget, gpointer data)
{
  gnc_show_options_dialog();
}

static void
gnc_ui_filemenu_cb(GtkWidget *widget, gpointer menuItem)
{
  switch(GPOINTER_TO_INT(menuItem))
  {
    case FMB_NEW:
      gncFileNew();
      gnc_refresh_main_window();
      break;
    case FMB_OPEN:
      gncFileOpen();
      gnc_refresh_main_window();
      break;
    case FMB_SAVE:
      gncFileSave();
      break;
    case FMB_SAVEAS:
      gncFileSaveAs();
      break;
    case FMB_IMPORT:
      gncFileQIFImport();
      gnc_refresh_main_window();
      break;
    case FMB_QUIT:
      gnc_shutdown(0);
      break;
    default:
      break;  
  }  
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
gnc_ui_mainWindow_save_size()
{
  GtkWidget *app;
  int width = 0;
  int height = 0;

  app = gnc_get_ui_data();

  gdk_window_get_geometry(GTK_WIDGET(app)->window, NULL, NULL,
                          &width, &height, NULL);

  gnc_save_window_size("main_win", width, height);
}

static void
gnc_ui_mainWindow_destroy_cb(GtkObject *object, gpointer user_data)
{
  GNCMainInfo *main_info = user_data;

  gnc_unregister_option_change_callback_id
    (main_info->main_window_change_callback_id);

  gnc_unregister_option_change_callback_id
    (main_info->toolbar_change_callback_id);

  g_slist_free(main_info->account_sensitives);
  main_info->account_sensitives = NULL;

  g_free(main_info);
}

GNCAccountTree *
gnc_get_current_account_tree()
{
  GNCMainInfo *main_info;

  main_info = gnc_get_main_info();
  if (main_info == NULL)
    return NULL;

  return GNC_ACCOUNT_TREE(main_info->account_tree);
}

Account *
gnc_get_current_account()
{
  GNCAccountTree * tree = gnc_get_current_account_tree();
  return gnc_account_tree_get_current_account(tree);
}

GList *
gnc_get_current_accounts()
{
  GNCAccountTree * tree = gnc_get_current_account_tree();
  return gnc_account_tree_get_current_accounts(tree);
}

static void
gnc_account_tree_activate_cb(GNCAccountTree *tree,
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
      gnc_account_tree_toggle_account_expansion(tree, account);
      return;
    }
  }

  regData = regWindowSimple(account);
  gnc_register_raise(regData);
}

static void
gnc_configure_account_tree(void *data)
{
  GtkObject *app;
  GNCAccountTree *tree;
  AccountViewInfo new_avi;
  AccountViewInfo old_avi;
  GSList *list, *node;

  memset(&new_avi, 0, sizeof(new_avi));

  app = GTK_OBJECT(gnc_get_ui_data());

  tree = gnc_get_current_account_tree();
  if (tree == NULL)
    return;

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

  gnc_free_list_option_value(list);

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
      new_avi.show_field[ACCOUNT_BALANCE] = TRUE;
  }

  gnc_free_list_option_value(list);

  new_avi.show_field[ACCOUNT_NAME] = TRUE;

  gnc_account_tree_get_view_info(tree, &old_avi);

  if (memcmp(&old_avi, &new_avi, sizeof(AccountViewInfo)) != 0)
    gnc_account_tree_set_view_info(tree, &new_avi);
}

static void
gnc_main_create_toolbar(GnomeApp *app, GNCMainInfo *main_info)
{
  GSList *list;

  GnomeUIInfo toolbar[] = 
  {
    { GNOME_APP_UI_ITEM,
      OPEN_STR,
      TOOLTIP_OPEN_FILE,
      gnc_ui_filemenu_cb, 
      GINT_TO_POINTER(FMB_OPEN),
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_OPEN,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      SAVE_STR,
      TOOLTIP_SAVE_FILE,
      gnc_ui_filemenu_cb, 
      GINT_TO_POINTER(FMB_SAVE),
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_SAVE,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      IMPORT_STR,
      TOOLTIP_IMPORT_QIF,
      gnc_ui_filemenu_cb, 
      GINT_TO_POINTER(FMB_IMPORT),
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CONVERT,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM, 
      OPEN_STR,
      TOOLTIP_OPEN,
      gnc_ui_mainWindow_toolbar_open, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_JUMP_TO,
      0, 0, NULL 
    },
    { GNOME_APP_UI_ITEM,
      EDIT_STR,
      TOOLTIP_EDIT,
      gnc_ui_mainWindow_toolbar_edit, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_PROPERTIES,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      NEW_STR,
      TOOLTIP_NEW,
      gnc_ui_add_account, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_ADD,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      DELETE_STR,
      TOOLTIP_DELETE,
      gnc_ui_delete_account_cb, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_REMOVE,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      EXIT_STR,
      TOOLTIP_EXIT,
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

  list = g_slist_prepend(list, toolbar[4].widget);
  list = g_slist_prepend(list, toolbar[5].widget);
  list = g_slist_prepend(list, toolbar[8].widget);

  main_info->account_sensitives = list;
}

static void
gnc_main_create_menus(GnomeApp *app, GtkWidget *account_tree,
                      GNCMainInfo *main_info)
{
  GtkWidget *popup;
  GSList *list;

  GnomeUIInfo filemenu[] = {
    GNOMEUIINFO_MENU_NEW_ITEM(NEW_FILE_STR,
                              TOOLTIP_NEW_FILE,
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
      IMPORT_QIF_E_STR, TOOLTIP_IMPORT_QIF,
      gnc_ui_filemenu_cb, GINT_TO_POINTER(FMB_IMPORT), NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CONVERT,
      'i', GDK_CONTROL_MASK, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    GNOMEUIINFO_MENU_EXIT_ITEM(gnc_ui_filemenu_cb,
                               GINT_TO_POINTER(FMB_QUIT)),
    GNOMEUIINFO_END
  };

  GnomeUIInfo optionsmenu[] = {
    {
      GNOME_APP_UI_ITEM,
      PREFERENCES_MENU_E_STR, TOOLTIP_PREFERENCES,
      gnc_ui_options_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo scrubmenu[] = {
    {
      GNOME_APP_UI_ITEM,
      SCRUB_ACC_MENU_STR, TOOLTIP_SCRUB_ACCT,
      gnc_ui_mainWindow_scrub, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      SCRUB_SUB_MENU_STR, TOOLTIP_SCRUB_SUB,
      gnc_ui_mainWindow_scrub_sub, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      SCRUB_ALL_MENU_STR, TOOLTIP_SCRUB_ALL,
      gnc_ui_mainWindow_scrub_all, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };

  GnomeUIInfo accountsmenu[] = {
    {
      GNOME_APP_UI_ITEM,
      OPEN_ACC_MENU_STR, TOOLTIP_OPEN,
      gnc_ui_mainWindow_toolbar_open, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
      'o', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      OPEN_SUB_MENU_STR, TOOLTIP_OPEN_SUB,
      gnc_ui_mainWindow_toolbar_open_subs, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      EDIT_ACC_MENU_STR, TOOLTIP_EDIT,
      gnc_ui_mainWindow_toolbar_edit, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PROP,
      'e', GDK_CONTROL_MASK, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      RECONCILE_MENU_E_STR, TOOLTIP_RECONCILE,
      gnc_ui_mainWindow_reconcile, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      'r', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      TRANSFER_MENU_E_STR, TOOLTIP_TRANSFER,
      gnc_ui_mainWindow_transfer, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      't', GDK_CONTROL_MASK, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      ADJ_BALN_MENU_E_STR, TOOLTIP_ADJUST,
      gnc_ui_mainWindow_adjust_balance, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      'b', GDK_CONTROL_MASK, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      NEW_ACC_MENU_E_STR, TOOLTIP_NEW,
      gnc_ui_add_account, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_ADD,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      DEL_ACC_MENU_STR, TOOLTIP_DELETE,
      gnc_ui_delete_account_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_REMOVE,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    GNOMEUIINFO_SUBTREE(SCRUB_MENU_STR, scrubmenu),
    GNOMEUIINFO_END
  };  

  GnomeUIInfo helpmenu[] = {
    {
      GNOME_APP_UI_ITEM,
      HELP_MENU_STR, TOOLTIP_HELP,
      gnc_ui_help_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_MENU_ABOUT_ITEM(gnc_ui_about_cb, NULL),
    GNOMEUIINFO_END
  };

  GnomeUIInfo mainmenu[] = {
    GNOMEUIINFO_MENU_FILE_TREE(filemenu),
    GNOMEUIINFO_SUBTREE(ACCOUNTS_MENU_STR, accountsmenu),
    GNOMEUIINFO_MENU_SETTINGS_TREE(optionsmenu),
    GNOMEUIINFO_MENU_HELP_TREE(helpmenu),
    GNOMEUIINFO_END
  };

  gnome_app_create_menus(app, mainmenu);
  gnome_app_install_menu_hints(app, mainmenu);

  list = main_info->account_sensitives;

  list = g_slist_prepend(list, scrubmenu[0].widget);
  list = g_slist_prepend(list, scrubmenu[1].widget);

  list = g_slist_prepend(list, accountsmenu[0].widget);
  list = g_slist_prepend(list, accountsmenu[1].widget);
  list = g_slist_prepend(list, accountsmenu[2].widget);
  list = g_slist_prepend(list, accountsmenu[4].widget);
  list = g_slist_prepend(list, accountsmenu[6].widget);
  list = g_slist_prepend(list, accountsmenu[9].widget);

  popup = gnome_popup_menu_new(accountsmenu);
  gnome_popup_menu_attach(popup, account_tree, NULL);

  list = g_slist_prepend(list, scrubmenu[0].widget);
  list = g_slist_prepend(list, scrubmenu[1].widget);

  list = g_slist_prepend(list, accountsmenu[0].widget);
  list = g_slist_prepend(list, accountsmenu[1].widget);
  list = g_slist_prepend(list, accountsmenu[2].widget);
  list = g_slist_prepend(list, accountsmenu[4].widget);
  list = g_slist_prepend(list, accountsmenu[6].widget);
  list = g_slist_prepend(list, accountsmenu[9].widget);

  main_info->account_sensitives = list;
}

static GNCMainInfo *
gnc_get_main_info()
{
  GtkObject *app = GTK_OBJECT(gnc_get_ui_data());

  return gtk_object_get_data(app, "gnc_main_info");
}

static void
gnc_configure_toolbar(void *data)
{
  GnomeApp *app = GNOME_APP(gnc_get_ui_data());
  GnomeDockItem *di;
  GtkWidget *toolbar;
  GtkToolbarStyle tbstyle;

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
gnc_account_foreach_cb(gpointer widget, gpointer sensitive)
{
  gtk_widget_set_sensitive(GTK_WIDGET(widget), GPOINTER_TO_INT(sensitive));
}

static void
gnc_account_set_sensititives(GNCMainInfo *main_info, gboolean sensitive)
{
  if (main_info == NULL)
    return;

  g_slist_foreach(main_info->account_sensitives,
                  gnc_account_foreach_cb,
                  GINT_TO_POINTER(sensitive));
}

static void
gnc_account_cb(GNCAccountTree *tree, Account *account, gpointer data)
{
  gboolean sensitive;

  account = gnc_account_tree_get_current_account(tree);
  sensitive = account != NULL;

  gnc_account_set_sensititives(gnc_get_main_info(), sensitive);
}

void
mainWindow()
{
  GNCMainInfo *main_info;
  GtkWidget *app = gnc_get_ui_data();
  GtkWidget *scrolled_win;
  GtkWidget *statusbar;
  int width = 0;
  int height = 0;

  gnc_get_window_size("main_win", &width, &height);
  if (height == 0)
    height = 400;
  gtk_window_set_default_size(GTK_WINDOW(app), width, height);

  main_info = g_new0(GNCMainInfo, 1);
  gtk_object_set_data(GTK_OBJECT(app), "gnc_main_info", main_info);

  main_info->account_tree = gnc_account_tree_new();

  main_info->main_window_change_callback_id =
    gnc_register_option_change_callback(gnc_configure_account_tree, NULL,
                                        "Main Window", NULL);

  gtk_signal_connect(GTK_OBJECT(main_info->account_tree), "activate_account",
		     GTK_SIGNAL_FUNC (gnc_account_tree_activate_cb), NULL);

  gtk_signal_connect(GTK_OBJECT(main_info->account_tree), "select_account",
                     GTK_SIGNAL_FUNC(gnc_account_cb), NULL);

  gtk_signal_connect(GTK_OBJECT(main_info->account_tree), "unselect_account",
                     GTK_SIGNAL_FUNC(gnc_account_cb), NULL);

  /* create statusbar and add it to the application. */
  statusbar = gnome_appbar_new(GNC_F, /* no progress bar, maybe later? */
			       GNC_T, /* has status area */
			       GNOME_PREFERENCES_USER /* recommended */);

  gnome_app_set_statusbar(GNOME_APP(app), GTK_WIDGET(statusbar));

  gnc_main_create_menus(GNOME_APP(app), main_info->account_tree, main_info);

  gnc_main_create_toolbar(GNOME_APP(app), main_info);
  gnc_configure_toolbar(NULL);
  main_info->toolbar_change_callback_id =
    gnc_register_option_change_callback(gnc_configure_toolbar, NULL,
                                        "General", "Toolbar Buttons");

  /* create the label containing the account balances */
  {
    GtkWidget *label;
    GtkWidget *hbox;

    hbox = gtk_hbox_new(FALSE, 2);
    gtk_box_pack_end(GTK_BOX(statusbar), hbox, FALSE, FALSE, 5);

    label = gtk_label_new(ASSETS_C_STR);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

    label = gtk_label_new("");
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
    main_info->assets_label = label;

    hbox = gtk_hbox_new(FALSE, 2);
    gtk_box_pack_end(GTK_BOX(statusbar), hbox, FALSE, FALSE, 5);

    label = gtk_label_new(PROFITS_C_STR);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

    label = gtk_label_new("");
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
    main_info->profits_label = label;
  }

  /* create scrolled window */
  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gnome_app_set_contents(GNOME_APP(app), scrolled_win);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                  GTK_POLICY_AUTOMATIC, 
                                  GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(scrolled_win),
                    GTK_WIDGET(main_info->account_tree));

  {
    SCM run_danglers = gh_eval_str("gnc:hook-run-danglers");
    SCM hook = gh_eval_str("gnc:*main-window-opened-hook*");
    SCM window = POINTER_TOKEN_to_SCM(make_POINTER_TOKEN("gncUIWidget", app));
    gh_call2(run_danglers, hook, window); 
  }

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
  gtk_widget_show_all(statusbar);
  gtk_widget_show(main_info->account_tree);
  gtk_widget_show(scrolled_win);

  gnc_configure_account_tree(NULL);

  gnc_refresh_main_window();

  gnc_account_set_sensititives(main_info, FALSE);

  gtk_widget_grab_focus(main_info->account_tree);
} 

/********************* END OF FILE **********************************/
