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

#include <gnome.h>
#include <guile/gh.h>

#include "config.h"

#include "AccWindow.h"
#include "dialog-options.h"
#include "FileDialog.h"
#include "g-wrap.h"
#include "gnucash.h"
#include "MainWindow.h"
#include "messages.h"
#include "RegWindow.h"
#include "top-level.h"
#include "version.h"
#include "window-main.h"
#include "window-mainP.h"
#include "window-help.h"
#include "account-tree.h"
#include "util.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;
static short show_categories = GNC_T;

enum {
  FMB_NEW,
  FMB_OPEN,
  FMB_IMPORT,
  FMB_SAVE,
  FMB_SAVEAS,
  FMB_QUIT,
};

/** Menus ***********************************************************/
static GnomeUIInfo filemenu[] = {
  GNOMEUIINFO_MENU_NEW_ITEM(N_("New"),
			    N_("Create New File."),
			    gnc_ui_filemenu_cb,
			    GINT_TO_POINTER(FMB_NEW)),
  GNOMEUIINFO_MENU_OPEN_ITEM(gnc_ui_filemenu_cb,
			     GINT_TO_POINTER(FMB_OPEN)),
  GNOMEUIINFO_MENU_SAVE_ITEM(gnc_ui_filemenu_cb,
			     GINT_TO_POINTER(FMB_SAVE)),
  {
    GNOME_APP_UI_ITEM,
    N_("Import..."), N_("Import QIF File."),
    gnc_ui_filemenu_cb, GINT_TO_POINTER(FMB_IMPORT), NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CONVERT,
    'i', GDK_CONTROL_MASK, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  GNOMEUIINFO_MENU_EXIT_ITEM(gnc_ui_filemenu_cb,
			     GINT_TO_POINTER(FMB_QUIT)),
  GNOMEUIINFO_END
};

static GnomeUIInfo viewmenu[] = {
  {
    GNOME_APP_UI_ITEM,
    N_("Hide categories"), N_("Hide the income and expense accounts"),
    gnc_ui_view_cb, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
    0, 0, NULL
  },
  GNOMEUIINFO_END
};

static GnomeUIInfo showmenu[] = {
  {
    GNOME_APP_UI_ITEM,
    N_("Show categories"), N_("Show the income and expense accounts"),
    gnc_ui_view_cb, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
    0, 0, NULL
  },
  GNOMEUIINFO_END
};

static GnomeUIInfo reportsmenu[] = {
  {
    GNOME_APP_UI_ITEM,
    N_("_Balance..."), N_("Balance Report"),
    gnc_ui_reports_cb, "report-baln.phtml", NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
    0, 0, NULL
  },
  {
    GNOME_APP_UI_ITEM,
    N_("_Profit & Loss..."), N_("Profit & Loss Report"),
    gnc_ui_reports_cb, "report-pnl.phtml", NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
    0, 0, NULL
  },
  GNOMEUIINFO_END
};

static GnomeUIInfo optionsmenu[] = {
  {
    GNOME_APP_UI_ITEM,
    N_("Preferences"), N_("Preferences"),
    gnc_ui_options_cb, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
    0, 0, NULL
  },
#if 0
  {
    GNOME_APP_UI_ITEM,
    N_("Gnucash News"), N_("News"),
    gnc_ui_news_callback, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
    0, 0, NULL
  },
#endif	 
  GNOMEUIINFO_END
};

static GnomeUIInfo accountsmenu[] = {
  {
    GNOME_APP_UI_ITEM,
    N_("_View..."), N_("View Account"),
    gnc_ui_mainWindow_toolbar_open, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_OPEN,
    'v', GDK_CONTROL_MASK, NULL
  },
  {
    GNOME_APP_UI_ITEM,
    N_("_Edit..."), N_("Edit Account"),
    gnc_ui_mainWindow_toolbar_edit, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PROP,
    'e', GDK_CONTROL_MASK, NULL
  },
  {
    GNOME_APP_UI_ITEM,
    N_("Adjust _Balance..."), N_("Adjust the balance of the account"),
    gnc_ui_mainWindow_adjust_balance, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    'b', GDK_CONTROL_MASK, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  {
    GNOME_APP_UI_ITEM,
    N_("_Add..."), N_("Add Account"),
    gnc_ui_add_account, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_ADD,
    'a', GDK_CONTROL_MASK, NULL
  },
  {
    GNOME_APP_UI_ITEM,
    N_("_Remove"), N_("Remove Account"),
    gnc_ui_delete_account_cb, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_REMOVE,
    'r', GDK_CONTROL_MASK, NULL
  },
  GNOMEUIINFO_END
};  

static GnomeUIInfo helpmenu[] = {
  GNOMEUIINFO_MENU_ABOUT_ITEM(gnc_ui_about_cb, NULL),
  GNOMEUIINFO_SEPARATOR,
  {
    GNOME_APP_UI_ITEM,
    N_("_Help..."), N_("Gnucash Help."),
    gnc_ui_help_cb, NULL, NULL,
    GNOME_APP_PIXMAP_NONE, NULL,
    0, 0, NULL
  },
  GNOMEUIINFO_END
};


static GnomeUIInfo scriptsmenu[] = {
  GNOMEUIINFO_END
};

static GnomeUIInfo mainmenu[] = {
  GNOMEUIINFO_MENU_FILE_TREE(filemenu),
  GNOMEUIINFO_SUBTREE(N_("View"), viewmenu),    
  GNOMEUIINFO_SUBTREE(N_("Accounts"), accountsmenu),
  GNOMEUIINFO_SUBTREE(N_("Reports"), reportsmenu),
  GNOMEUIINFO_SUBTREE(N_("Options"), optionsmenu),
  GNOMEUIINFO_SUBTREE(N_("Extensions"), scriptsmenu),    
  GNOMEUIINFO_MENU_HELP_TREE(helpmenu),
  GNOMEUIINFO_END
};

/** TOOLBAR ************************************************************/
static GnomeUIInfo toolbar[] = 
{
  { GNOME_APP_UI_ITEM,
    N_("Open"), 
    N_("Open File."),
    gnc_ui_filemenu_cb, 
    GINT_TO_POINTER(FMB_OPEN),
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_OPEN,
    0, 0, NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Save"), 
    N_("Save File."),
    gnc_ui_filemenu_cb, 
    GINT_TO_POINTER(FMB_SAVE),
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_SAVE,
    0, 0, NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Import"), 
    N_("Import QIF File."),
    gnc_ui_filemenu_cb, 
    GINT_TO_POINTER(FMB_IMPORT),
    NULL,
    GNOME_APP_PIXMAP_STOCK, 
    GNOME_STOCK_PIXMAP_CONVERT,
    0, 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, 
    N_("View"), 
    N_("View selected account."),
    gnc_ui_mainWindow_toolbar_open, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_JUMP_TO,
    0, 0, NULL 
  },
  { GNOME_APP_UI_ITEM,
    N_("Edit"), 
    N_("Edit account information."), 
    gnc_ui_mainWindow_toolbar_edit, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_PROPERTIES,
    0, 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM,
    N_("Add"),
    N_("Add a new account."),
    gnc_ui_add_account, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_ADD,
    0, 0, NULL
  },
  { GNOME_APP_UI_ITEM,
    N_("Remove"), 
    N_("Remove selected account."), 
    gnc_ui_delete_account_cb, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_REMOVE,
    0, 0, NULL
  },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM,
    N_("Exit"), 
    N_("Exit GnuCash."),
    gnc_ui_exit_cb, 
    NULL,
    NULL,
    GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_QUIT,
    0, 0, NULL
  },
  GNOMEUIINFO_END
};


static void
gnc_ui_refresh_statusbar()
{
  GtkWidget *label;
  guint     context_id;
  int i;
  double  assets  = 0.0;
  double  profits = 0.0;
  char buf[BUFSIZE];
  char *amt;
  AccountGroup *grp;
  Account *acc;
  int nacc;

  grp = gncGetCurrentGroup ();
  nacc = xaccGroupGetNumAccounts (grp);
  for (i=0; i<nacc; i++) {
     int acc_type;
     AccountGroup *acc_children;

     acc = xaccGroupGetAccount (grp,i);
 
     acc_type = xaccAccountGetType (acc);
     acc_children = xaccAccountGetChildren (acc);

     switch (acc_type) {
        case BANK:
        case CASH:
        case ASSET:
        case STOCK:
        case MUTUAL:
        case CREDIT:
        case LIABILITY:
           assets += xaccAccountGetBalance (acc);
           if (acc_children) {
              assets += xaccGroupGetBalance (acc_children);
           }
           break;
        case INCOME:
        case EXPENSE:
           profits -= xaccAccountGetBalance (acc); /* flip the sign !! */
           if (acc_children) {
              profits -= xaccGroupGetBalance (acc_children); /* flip the sign !! */
           }
           break;
        case EQUITY:
        default:
           break;
     }
  }
  
  amt = xaccPrintAmount (assets, PRTSYM | PRTSEP);
  strcpy (buf, "Assets: ");
  strcat (buf, amt);
  strcat (buf, "   Profits: ");
  amt = xaccPrintAmount (profits, PRTSYM | PRTSEP);
  strcat (buf, amt);
  strcat (buf, "  ");
   
  label = gtk_object_get_data(GTK_OBJECT(gnc_get_ui_data()),
			      "balance_label");

  gtk_label_set_text( GTK_LABEL(label), buf);
}

/* Required for compatibility with Motif code... */
void
gnc_refresh_main_window()
{
  gnc_ui_refresh_statusbar();
  gnc_account_tree_refresh(gnc_get_current_account_tree());
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
  const gchar *authors[] = {
    "Linas Vepstas <linas@linas.org>",
    NULL
  };

  about = gnome_about_new("GnuCash", "1.3 (development version)",
			  "(C) 1998-1999 Linas Vepstas",
			  authors,
			  "The GnuCash personal finance manager.\nThe GNU way to manage your money!", NULL);

  gnome_dialog_run_and_close(GNOME_DIALOG(about));
}

static void
gnc_ui_help_cb ( GtkWidget *widget, gpointer data )
{
  helpWindow( GTK_WIDGET(gnc_get_ui_data()), HELP_STR, HH_MAIN );
}

static void
gnc_ui_reports_cb(GtkWidget *widget, gpointer report)
{
  reportWindow (widget, "duuuude", report);  
}

static void
gnc_ui_add_account ( GtkWidget *widget, gpointer data )
{
  accWindow(NULL);
}

static void
gnc_ui_delete_account ( Account *account )
{
  /* Step 1: Remove the account from the tree */
  gnc_account_tree_remove_account(gnc_get_current_account_tree(), account);

  /* Step 2: Delete associated windows */
  xaccAccountWindowDestroy(account);

  /* Step 3: Delete the actual account */  
  xaccRemoveAccount(account);
  xaccFreeAccount(account);

  /* Step 4: Refresh the toolbar */
  gnc_ui_refresh_statusbar();
}

static void
gnc_ui_delete_account_cb ( GtkWidget *widget, gpointer data )
{
  Account *account = gnc_get_current_account();
  
  if (account)
  {
    if (gnc_verify_dialog("Are you sure you want to delete this account?",
			  GNC_F))
      gnc_ui_delete_account(account);
  }
  else
    gnc_error_dialog(ACC_DEL_MSG);
}

static void
gnc_ui_mainWindow_toolbar_open ( GtkWidget *widget, gpointer data )
{
  Account *account = gnc_get_current_account();
  
  if(account)
  {
    PINFO ("calling regWindowSimple(%p)\n", account);
    regWindowSimple ( account );
  }
  else
    gnc_error_dialog("You must select an account to open first.");
}

static void
gnc_ui_mainWindow_toolbar_edit ( GtkWidget *widget, gpointer data )
{
  Account *account = gnc_get_current_account();
  
  if (account != NULL)
    editAccWindow(account);
  else
    gnc_error_dialog(ACC_EDIT_MSG);
}

static void
gnc_ui_mainWindow_adjust_balance(GtkWidget *widget, gpointer data)
{
  Account *account = gnc_get_current_account();

  if (account != NULL)
    adjBWindow(account);
  else
    gnc_error_dialog("You must select an account to adjust first.");
}

static void
gnc_ui_options_cb ( GtkWidget *widget, gpointer data ) {
  gnc_show_options_dialog();
}

/* This function currently just hides/shows the INCOME/EXPENSE
 * accounts. It could and should be extended to allow full
 * customization of the account tree widget. For instance the user
 * should be able to choose which fields get displayed such as
 * balance, description, account type, etc.
 */
static void
gnc_ui_view_cb(GtkWidget *widget, gpointer viewType)
{
  if (show_categories)
  {
    /* Widget label -> Hide Categories */
    gnome_app_remove_menus (GNOME_APP(gnc_get_ui_data()),
			    "View/Hide categories", 1);
    gnome_app_insert_menus (GNOME_APP(gnc_get_ui_data()),
			    "View/", showmenu);
    gnome_app_install_menu_hints(GNOME_APP(gnc_get_ui_data()), showmenu);
    gnc_account_tree_hide_categories(gnc_get_current_account_tree());
    show_categories = GNC_F;
  }
  else
  {
    /* Widget label -> Show Categories */
    gnome_app_remove_menus (GNOME_APP(gnc_get_ui_data()),
			    "View/Show categories", 1);
    gnome_app_insert_menus (GNOME_APP(gnc_get_ui_data()),
			    "View/", viewmenu);
    gnome_app_install_menu_hints(GNOME_APP(gnc_get_ui_data()), viewmenu);
    gnc_account_tree_show_categories(gnc_get_current_account_tree());
    show_categories = GNC_T;
  }
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
gnc_ui_mainWindow_delete_cb(GtkWidget       *widget,
			    GdkEvent        *event,
			    gpointer         user_data)
{
  /* Don't allow deletes if we're in a modal dialog */
  if (gtk_main_level() == 1)
    gnc_shutdown(0);

  /* Don't delete the window, we'll handle things ourselves. */
  return TRUE;
}


static gboolean
gnc_ui_mainWindow_destroy_cb(GtkWidget       *widget,
			     GdkEvent        *event,
			     gpointer         user_data)
{
  return FALSE;
}

GNCAccountTree *
gnc_get_current_account_tree()
{
  return gtk_object_get_data(GTK_OBJECT(gnc_get_ui_data()), "account_tree");
}

Account *
gnc_get_current_account()
{
  GNCAccountTree * tree = gnc_get_current_account_tree();
  return gnc_account_tree_get_current_account(tree);
}

static void
gnc_account_tree_double_click_cb(GNCAccountTree *tree,
				 Account *account,
				 gpointer user_data)
{
  regWindowSimple(account);
}

void
mainWindow()
{
  GtkWidget *app = gnc_get_ui_data();
  GtkWidget *popup;
  GtkWidget *scrolled_win;
  GtkWidget *statusbar;
  GtkWidget *account_tree;
  GtkWidget *label;

  account_tree = gnc_account_tree_new();
  gtk_object_set_data (GTK_OBJECT (app), "account_tree", account_tree);
  gtk_signal_connect (GTK_OBJECT (account_tree), "double_click_account",
                      GTK_SIGNAL_FUNC (gnc_account_tree_double_click_cb),
                      NULL);

  popup = gnome_popup_menu_new(accountsmenu);
  gnome_popup_menu_attach (GTK_WIDGET(popup), GTK_WIDGET(account_tree), NULL);

  gnome_app_create_toolbar(GNOME_APP(app), toolbar);
  gnome_app_create_menus  (GNOME_APP(app), mainmenu);

  /* create statusbar and add it to the application. */
  statusbar = gnome_appbar_new(GNC_F, /* no progress bar, maybe later? */
			       GNC_T, /* has status area */
			       GNOME_PREFERENCES_USER /* recommended */);

  gnome_app_set_statusbar(GNOME_APP(app), GTK_WIDGET(statusbar));

  /* create the label containing the account balances */
  label = gtk_label_new("");
  gtk_object_set_data (GTK_OBJECT (app), "balance_label", label);
  gtk_box_pack_end(GTK_BOX(statusbar), label, GNC_F, GNC_F, 0);

  gnome_app_install_menu_hints(GNOME_APP(app), mainmenu);

  /* create scrolled window */
  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gnome_app_set_contents(GNOME_APP(app), scrolled_win);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                  GTK_POLICY_AUTOMATIC, 
                                  GTK_POLICY_AUTOMATIC);

  gtk_container_add(GTK_CONTAINER(scrolled_win), GTK_WIDGET(account_tree));


  gtk_widget_set_usize ( GTK_WIDGET(app), 500, 400 );

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
                      GTK_SIGNAL_FUNC (gnc_ui_mainWindow_destroy_cb),
                      NULL);

  /* Show everything now that it is created */
  gtk_widget_show(label);
  gtk_widget_show(statusbar);
  gtk_widget_show(account_tree);
  gtk_widget_show(scrolled_win);

  gnc_refresh_main_window();
} 

/********************* END OF FILE **********************************/

/*
  Local Variables:
  indent-tabs-mode: nil
  mode: c
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/
