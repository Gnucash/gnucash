/********************************************************************
 * window-main.c -- open/close/configure GNOME MDI main window      *
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

#include "gnucash.h"
#include "gnc-ui.h"
#include "top-level.h"
#include "extensions.h"

#include "FileBox.h"
#include "FileDialog.h"

#include "dialog-account.h"
#include "dialog-fincalc.h"
#include "dialog-find-transactions.h"
#include "dialog-options.h"
#include "dialog-totd.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"

#include "window-help.h"
#include "window-main.h"
#include "window-main-summarybar.h"
#include "window-acct-tree.h"
#include "window-reconcile.h"
#include "window-register.h"
#include "window-report.h"

#include "mainwindow-account-tree.h"
#include "gnc-component-manager.h"
#include "option-util.h"
#include "global-options.h"
#include "query-user.h"

#define WINDOW_MAIN_CM_CLASS "window-main"

static void gnc_main_window_create_menus(GNCMainInfo * maininfo);

/********************************************************************
 * gnc_main_window_destroy_cb()
 * Shut down the Gnucash ui windows 
 ********************************************************************/

static void
gnc_main_window_destroy_cb(GtkObject * w) {
  gnc_ui_destroy();
}


/********************************************************************
 * gnc_main_window_remove_view_cb()
 * we only have one view per child, so destroy the child
 ********************************************************************/

static gint
gnc_main_window_remove_view_cb(GnomeMDI * mdi, GnomeMDIChild * child, 
                               gpointer data) {
  GNCMainInfo * mainwin = data;
  gtk_object_destroy(GTK_OBJECT(child));
  return TRUE;
}

/********************************************************************
 * gnc_main_window_app_created_cb()
 * called when a new top-level GnomeApp is created.  
 ********************************************************************/

static void
gnc_main_window_app_created_cb(GnomeMDI * mdi, GnomeApp * app, 
                               gpointer data) {
  GNCMainInfo * mainwin = data;
  GtkWidget * summarybar;
  GtkWidget * statusbar;

  /* add the summarybar */
  summarybar = gnc_main_window_summary_new();
  gnome_app_add_docked(GNOME_APP(app), summarybar, "Summary Bar",
                       GNOME_DOCK_ITEM_BEH_EXCLUSIVE,
                       GNOME_DOCK_TOP, 2, 0, 0);

  /* add the statusbar */ 
  statusbar = gnome_appbar_new(FALSE, TRUE, GNOME_PREFERENCES_USER);
  gnome_app_set_statusbar(app, statusbar);

  /* set up extensions menu and hints */
  gnc_extensions_menu_setup(app);  
}


/********************************************************************
 * gnc_main_window_child_changed_cb()
 * called when the active child changes.  Not sure what this means 
 * with top-level windows.  focus, maybe?  
 ********************************************************************/

static void
gnc_main_window_child_changed_cb(GnomeMDI * mdi, GnomeMDIChild * oldchild, 
                                 gpointer data) {
  GNCMainInfo      * mainwin = data;
  GNCMainChildInfo * childwin = 
    gtk_object_get_user_data(GTK_OBJECT(mdi->active_child));
  GNCMainChildInfo * oldchildwin;
  GnomeUIInfo      * hintinfo;
  GnomeApp         * new_app = gnome_mdi_get_app_from_view(childwin->contents);
  
  /* hide the old toolbar, if needed */
  if(oldchild) {
    oldchildwin = gtk_object_get_user_data(GTK_OBJECT(oldchild));
    if(oldchildwin && oldchildwin->toolbar && 
       (oldchildwin->app == new_app)) {
      gtk_widget_hide(GTK_WIDGET(oldchildwin->toolbar)->parent);
    }
  }
  
  /* show the new one */ 
  if(childwin && childwin->toolbar) {
    if(childwin->app) {
      gtk_widget_show(GTK_WIDGET(childwin->toolbar)->parent);
    }
    else {
      childwin->app = new_app;
      gnome_app_add_toolbar(GNOME_APP(childwin->app), 
                            GTK_TOOLBAR(childwin->toolbar),
                            "Toolbar", GNOME_DOCK_ITEM_BEH_NORMAL,
                            GNOME_DOCK_TOP, 1, 0, 0);
      gtk_toolbar_set_style(GTK_TOOLBAR(childwin->toolbar), 
                            gnc_get_toolbar_style());
    }
  }

  /* install menu hints if relevant */
  if(mdi->active_child) {
    
    hintinfo = gnome_mdi_get_menubar_info(new_app);
    if(hintinfo) {
      gnome_app_install_menu_hints(new_app, hintinfo);
    }
    
    hintinfo = gnome_mdi_get_child_menu_info(new_app);
    if(hintinfo) { 
      gnome_app_install_menu_hints(new_app, hintinfo);
    }
  }
}


/********************************************************************
 * gnc_main_window_view_changed_cb()
 * called when the view changes. 
 ********************************************************************/

static void
gnc_main_window_view_changed_cb(GnomeMDI * mdi, GnomeApp * app, 
                                gpointer data) {
  GNCMainInfo * mainwin = data;
  /* don't do anything ATM */
}


/********************************************************************
 * gnc_main_window_configure_toolbar_cb
 * called when the toolbar button style changes 
 ********************************************************************/

static void
gnc_main_window_configure_toolbar_cb (gpointer data)
{
  GNCMainInfo * mi = data; 
  GNCMainChildInfo * mc = NULL; 
  GtkToolbarStyle tbstyle = gnc_get_toolbar_style();
  GList * child;

  for(child = mi->children; child; child = child->next) {
    mc = child->data;
    if(mc && mc->toolbar) {
      gtk_toolbar_set_style(GTK_TOOLBAR(mc->toolbar), tbstyle);
    }
  }
}

/********************************************************************
 * gnc_main_window_create_child()
 * open an MDI child given a config string (URL).  This is used  
 ********************************************************************/

GnomeMDIChild * 
gnc_main_window_create_child(const gchar * configstring) {
  URLType type;
  char * location;
  char * label;
  
  type = gnc_html_parse_url(NULL, configstring, &location, &label);
  g_free(location);
  g_free(label);
  
  switch(type) {
  case URL_TYPE_REPORT:
    return gnc_report_window_create_child(configstring);
    break;
    
  case URL_TYPE_ACCTTREE:
    return gnc_acct_tree_window_create_child(configstring);
    break;
    
  default:
    return NULL;
  }
}


/********************************************************************
 * gnc_main_window_new()
 * initialize the Gnome MDI system
 ********************************************************************/

GNCMainInfo * 
gnc_main_window_new(void) {
  GNCMainInfo * retval = g_new0(GNCMainInfo, 1);
  
  retval->mdi = GNOME_MDI(gnome_mdi_new("GnuCash", "GnuCash"));
  retval->component_id = 
    gnc_register_gui_component (WINDOW_MAIN_CM_CLASS, NULL, NULL, 
                                retval);
  
  /* these menu and toolbar options are the ones that are always 
   * available */ 
  gnc_main_window_create_menus(retval);
  
  /* set up the position where the child menus/toolbars will be 
   * inserted  */
  gnome_mdi_set_child_menu_path(GNOME_MDI(retval->mdi),
                                "_File");

  gnome_mdi_set_mode(retval->mdi, GNOME_MDI_NOTEBOOK);

  retval->toolbar_change_callback_id =
    gnc_register_option_change_callback(gnc_main_window_configure_toolbar_cb, 
                                        retval,
                                        "General", "Toolbar Buttons");
  
  /* handle top-level signals */
  gtk_signal_connect(GTK_OBJECT(retval->mdi), "destroy",
                     GTK_SIGNAL_FUNC(gnc_main_window_destroy_cb),
                     retval);
  gtk_signal_connect(GTK_OBJECT(retval->mdi), "remove_view",
                     GTK_SIGNAL_FUNC(gnc_main_window_remove_view_cb),
                     retval);
  gtk_signal_connect(GTK_OBJECT(retval->mdi), "app_created",
                     GTK_SIGNAL_FUNC(gnc_main_window_app_created_cb),
                     retval);
  gtk_signal_connect(GTK_OBJECT(retval->mdi), "child_changed",
                     GTK_SIGNAL_FUNC(gnc_main_window_child_changed_cb),
                     retval);
  gtk_signal_connect(GTK_OBJECT(retval->mdi), "view_changed",
                     GTK_SIGNAL_FUNC(gnc_main_window_view_changed_cb),
                     retval);
  
  return retval;
}

static char * 
gnc_main_window_child_save_func(GnomeMDIChild * child, gpointer user_data) {
  printf("child save func '%p': '%s'\n", child, child->name);
  return g_strdup(child->name);
}

/********************************************************************
 * gnc_main_window_add_child()
 ********************************************************************/

void
gnc_main_window_add_child(GNCMainInfo * wind, GNCMainChildInfo * child) {
  wind->children = g_list_append(wind->children, child);

  if(GNOME_IS_MDI_GENERIC_CHILD(child->child)) {
    gnome_mdi_generic_child_set_config_func
      (GNOME_MDI_GENERIC_CHILD(child->child),
       gnc_main_window_child_save_func, NULL);
  }
}


/********************************************************************
 * gnc_main_window_remove_child()
 ********************************************************************/

void
gnc_main_window_remove_child(GNCMainInfo * wind, GNCMainChildInfo * child) {
  wind->children = g_list_remove(wind->children, child);
}

/********************************************************************
 * gnc_main_window_destroy()
 * free MDI toplevel resources 
 ********************************************************************/

void
gnc_main_window_destroy(GNCMainInfo * wind) {
  g_free(wind);  
}


/********************************************************************
 * gnc_main_window_save()
 * save the status of the MDI session
 ********************************************************************/

void
gnc_main_window_save(GNCMainInfo * wind) {
  printf("entering gnome_mdi_save_state\n");
  gnome_mdi_save_state(GNOME_MDI(wind->mdi), 
                       "/GnuCash/MDI Session");
  printf("left gnome_mdi_save_state\n");
}

/********************************************************************
 * gnc_main_window_child_refresh(GNCMainChildInfo * child)
 * send an update event to the child 
 ********************************************************************/

void
gnc_main_window_child_refresh(gpointer data) {
  GNCMainChildInfo *child = data;
  gnome_mdi_update_child(gnc_ui_get_data()->mdi, child->child);
}


/********************************************************************
 * gnc_main_window_get_toplevel()
 * get the currently-active top-level widget 
 ********************************************************************/

GtkWidget * 
gnc_main_window_get_toplevel(GNCMainInfo * wind) {
  return GTK_WIDGET(gnome_mdi_get_active_window(GNOME_MDI(wind->mdi)));
}


/********************************************************************
 * menu/toolbar data structures and callbacks 
 * these are the "templates" that are installed in every toplevel
 * MDI window
 ********************************************************************/

static void
gnc_main_window_options_cb(GtkWidget *widget, gpointer data) {
  gnc_show_options_dialog();
}

static void
gnc_main_window_file_new_file_cb(GtkWidget * widget) {
  gncFileNew();
}

static void
gnc_main_window_file_new_window_cb(GtkWidget * widget, GnomeMDI * mdi) {
  if(mdi->active_child) {
    if(!strcmp(mdi->active_child->name, _("Accounts"))) {
      gnc_main_window_open_accounts(TRUE);
    }
    else {
      gnc_main_window_open_report_url(mdi->active_child->name, TRUE);
    }
  }
}

static void
gnc_main_window_file_open_cb(GtkWidget * widget) {
  gncFileOpen();
}

static void
gnc_main_window_file_save_cb(GtkWidget * widget) {
  gncFileSave();
  /* gnc_refresh_main_window_title(); */ 
}

static void
gnc_main_window_file_save_as_cb(GtkWidget * widget) {
  gncFileSaveAs();
  /* gnc_refresh_main_window_title(); */
}

static void
gnc_main_window_file_import_cb(GtkWidget * widget) {
  gncFileQIFImport();
}

static void
gnc_main_window_file_shutdown_cb(GtkWidget * widget) {
  gnc_shutdown(0);
}

static void
gnc_main_window_file_close_cb(GtkWidget * widget, GnomeMDI * mdi) {
  GNCMainChildInfo * inf;

  if(mdi->active_child) {
    inf = gtk_object_get_user_data(GTK_OBJECT(mdi->active_child));
    if(inf->toolbar) {
      gtk_widget_destroy(GTK_WIDGET(inf->toolbar)->parent);
      inf->toolbar = NULL;
    }
    gnome_mdi_remove_child(mdi, mdi->active_child, FALSE);
  }  
  else {
    gnc_warning_dialog(_("Select \"Exit\" to exit GnuCash."));
  }
}

static void
gnc_main_window_fincalc_cb(GtkWidget *widget, gpointer data) {
  gnc_ui_fincalc_dialog_create();
}

static void
gnc_main_window_gl_cb(GtkWidget *widget, gpointer data) {
  xaccLedgerDisplay *ld;
  RegWindow *regData;

  ld = xaccLedgerDisplayGL ();

  regData = regWindowLedger (ld);

  gnc_register_raise (regData);
}

static void
gnc_main_window_prices_cb(GtkWidget *widget, gpointer data) {
  gnc_prices_dialog (NULL);
}


static void
gnc_main_window_find_transactions_cb (GtkWidget *widget, gpointer data) {
  gnc_ui_find_transactions_dialog_create(NULL);
}

static void
gnc_main_window_about_cb (GtkWidget *widget, gpointer data)
{
  GtkWidget *about;
  const gchar *message = _("The GnuCash personal finance manager.\n"
                           "The GNU way to manage your money!");
  const gchar *copyright = "(C) 1998-2001 Linas Vepstas";
  const gchar *authors[] = {
    "Linas Vepstas <linas@linas.org>",
    NULL
  };
  
  about = gnome_about_new ("GnuCash", VERSION, copyright,
                           authors, message, NULL);
  gnome_dialog_set_parent (GNOME_DIALOG(about),
                           GTK_WINDOW(gnc_ui_get_toplevel()));
  
  gnome_dialog_run_and_close (GNOME_DIALOG(about));
}

static void
gnc_main_window_commodities_cb(GtkWidget *widget, gpointer data) {
  gnc_commodities_dialog (NULL);
}


static void
gnc_main_window_totd_cb (GtkWidget *widget, gpointer data)

{
  gnc_ui_totd_dialog_create_and_run();
  return;
}

static void
gnc_main_window_help_cb (GtkWidget *widget, gpointer data)
{
  helpWindow(NULL, NULL, HH_MAIN);
}

static void
gnc_main_window_exit_cb (GtkWidget *widget, gpointer data)
{
  gnc_shutdown(0);
}

static void
gnc_main_window_file_new_account_tree_cb(GtkWidget * w, GnomeMDI * mdi) {
  gnc_main_window_open_accounts(FALSE);
}

static void
gnc_main_window_create_menus(GNCMainInfo * maininfo) {
  static  GnomeUIInfo gnc_report_new_template[] = 
  { 
    GNOMEUIINFO_END 
  };
  
  static  GnomeUIInfo gnc_file_menu_template[] = 
  {
    GNOMEUIINFO_MENU_NEW_ITEM(N_("New _File"),
                              N_("Create a new file"),
                              gnc_main_window_file_new_file_cb, NULL),
    GNOMEUIINFO_MENU_OPEN_ITEM(gnc_main_window_file_open_cb, NULL),
    GNOMEUIINFO_MENU_SAVE_ITEM(gnc_main_window_file_save_cb, NULL),
    GNOMEUIINFO_MENU_SAVE_AS_ITEM(gnc_main_window_file_save_as_cb, NULL),
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Import QIF..."),
      N_("Import a Quicken QIF file"),
      gnc_main_window_file_import_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_CONVERT,
      'i', GDK_CONTROL_MASK, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("New _Window"),
      N_("Open a new top-level GnuCash window"),
      gnc_main_window_file_new_window_cb, NULL, NULL, 
      GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL
    },    
    {
      GNOME_APP_UI_ITEM,
      N_("New _Account Tree"),
      N_("Open a new account tree view"),
      gnc_main_window_file_new_account_tree_cb, NULL, NULL, 
      GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL
    },    
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Close _Window"),
      N_("Close this MDI window"),
      gnc_main_window_file_close_cb, NULL, NULL, 
      GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL
    },    
    GNOMEUIINFO_MENU_EXIT_ITEM(gnc_main_window_file_shutdown_cb, NULL),
    GNOMEUIINFO_END
  };
  
  static  GnomeUIInfo gnc_settings_menu_template[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Preferences..."),
      N_("Open the global preferences dialog"),
      gnc_main_window_options_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_MENU_PREF,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };
  
  static  GnomeUIInfo gnc_tools_menu_template[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_General Ledger"),
      N_("Open a general ledger window"),
      gnc_main_window_gl_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Commodity _Editor"),
      N_("View and edit the commodities for stocks and mutual funds"),
      gnc_main_window_commodities_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Price Editor"),
      N_("View and edit the prices for stocks and mutual funds"),
      gnc_main_window_prices_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("Financial _Calculator"),
      N_("Use the financial calculator"),
      gnc_main_window_fincalc_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("_Find Transactions"),
      N_("Find transactions with a search"),
      gnc_main_window_find_transactions_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };
  
  static  GnomeUIInfo gnc_help_menu_template[] =
  {
    {
      GNOME_APP_UI_ITEM,
      N_("_Manual"),
      N_("Open the GnuCash Manual"),
      gnc_main_window_help_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    {
      GNOME_APP_UI_ITEM,
      N_("_Tips Of The Day"),
      N_("View the Tips of the Day"),
      gnc_main_window_totd_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    
    GNOMEUIINFO_MENU_ABOUT_ITEM(gnc_main_window_about_cb, NULL),
  
    GNOMEUIINFO_END
  };

  static GnomeUIInfo gnc_developer_menu_template[] =
  {
    GNOMEUIINFO_END
  };

  static GnomeUIInfo gnc_main_menu_template[] =
  {
    GNOMEUIINFO_MENU_FILE_TREE(gnc_file_menu_template),
    GNOMEUIINFO_SUBTREE(N_("_Tools"), gnc_tools_menu_template),
    GNOMEUIINFO_SUBTREE(N_("_Settings"), gnc_settings_menu_template),
    GNOMEUIINFO_SUBTREE(N_("_Devel Options"), gnc_developer_menu_template),
    GNOMEUIINFO_MENU_HELP_TREE(gnc_help_menu_template),
    GNOMEUIINFO_END
  };

  gnome_mdi_set_menubar_template(GNOME_MDI(maininfo->mdi),
                                 gnc_main_menu_template);
}

/********************************************************************
 * create_child_toolbar 
 * add the "stock" components to the child's components and return a
 * catenated toolbar.
 ********************************************************************/

void
gnc_main_window_create_child_toolbar(GNCMainInfo * mi, 
                                     GNCMainChildInfo * child) {
  GnomeUIInfo pre_tb[] = 
  {
    { GNOME_APP_UI_ITEM,
      N_("Save"),
      N_("Save the file to disk"),
      gnc_main_window_file_save_cb,
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_SAVE,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR
  };

  GnomeUIInfo post_tb[] = 
  {
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM,
      N_("Exit"),
      N_("Exit GnuCash"),
      gnc_main_window_exit_cb, 
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_QUIT,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };
  GnomeUIInfo end = GNOMEUIINFO_END;
  GnomeUIInfo * tbinfo;
  GnomeUIInfo * cur;
  GtkToolbar  * tb = GTK_TOOLBAR(gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, 
                                                 GTK_TOOLBAR_BOTH));
  tbinfo = g_new0(GnomeUIInfo, 
                  (sizeof(pre_tb) + sizeof(post_tb)) / sizeof(GnomeUIInfo)
                  + child->toolbar_size - 1);
  
  /* splice the pre, post, and child segments together */
  cur = tbinfo; 
  memcpy(cur, pre_tb, sizeof(pre_tb));
  cur += sizeof(pre_tb)/sizeof(GnomeUIInfo);
  memcpy(cur, child->toolbar_info, 
         (child->toolbar_size - 1)*sizeof(GnomeUIInfo));
  cur += child->toolbar_size - 1;
  memcpy(cur, post_tb, sizeof(post_tb));
  
  child->toolbar      = GTK_WIDGET(tb);
  child->toolbar_info = tbinfo;
  child->toolbar_size = 
    (sizeof(pre_tb) + sizeof(post_tb)) / sizeof(GnomeUIInfo) + 
    child->toolbar_size;
  
  gnome_app_fill_toolbar(tb, tbinfo, NULL);

}

