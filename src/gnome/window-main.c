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

#include <errno.h>
#include <gnome.h>
#include <guile/gh.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "dialog-account.h"
#include "dialog-fincalc.h"
#include "dialog-find-transactions.h"
#include "dialog-nextrun.h"
#include "dialog-options.h"
#include "dialog-scheduledxaction.h"
#include "dialog-sxsincelast.h"
#include "dialog-totd.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "druid-qif-import.h"
#include "gfec.h"
#include "global-options.h"
#include "gnc-engine.h"
#include "gnc-file-dialog.h"
#include "gnc-file-history.h"
#include "gnc-file-history-gnome.h"
#include "gnc-file.h"
#include "gnc-gui-query.h"
#include "gnc-menu-extensions.h"
#include "gnc-ui.h"
#include "gnucash.h"
#include "io-utils.h"
#include "mainwindow-account-tree.h"
#include "option-util.h"
#include "top-level.h"
#include "window-acct-tree.h"
#include "window-help.h"
#include "window-main-summarybar.h"
#include "window-main.h"
#include "window-reconcile.h"
#include "window-register.h"
#include "window-report.h"

static void gnc_main_window_create_menus(GNCMDIInfo * maininfo);
static GnomeUIInfo * gnc_main_window_toolbar_prefix (void);
static GnomeUIInfo * gnc_main_window_toolbar_suffix (void);


/********************************************************************
 * gnc_main_window_app_created_cb()
 * called when a new top-level GnomeApp is created.  
 ********************************************************************/

static void
gnc_main_window_app_created_cb(GnomeMDI * mdi, GnomeApp * app, 
                               gpointer data) {
  GNCMDIInfo * mainwin = data;
  GtkWidget * summarybar;
  GtkWidget * statusbar;

  /* add the summarybar */
  summarybar = gnc_main_window_summary_new();

  {
    GnomeDockItemBehavior behavior;
    GtkWidget *item;

    /* This is essentially gnome_app_add_docked, but without using
     * gnome_app_add_dock_item because it emits the layout_changed
     * signal which creates a new layout and writes it over the saved
     * layout config before we've had a chance to read it.
     */

    behavior = (GNOME_DOCK_ITEM_BEH_EXCLUSIVE |
                GNOME_DOCK_ITEM_BEH_NEVER_VERTICAL);
    if (!gnome_preferences_get_toolbar_detachable ())
      behavior |= GNOME_DOCK_ITEM_BEH_LOCKED;

    item = gnome_dock_item_new("Summary Bar", behavior);
    gtk_container_add( GTK_CONTAINER (item), summarybar );

    if (app->layout) {
      gnome_dock_layout_add_item( app->layout,
                                  GNOME_DOCK_ITEM(item),
                                  GNOME_DOCK_TOP,
                                  2, 0, 0 );
    }
    else {
      gnome_dock_add_item( GNOME_DOCK(app->dock),
                           GNOME_DOCK_ITEM(item),
                           GNOME_DOCK_TOP,
                           2, 0, 0, FALSE );
    }
  }

  /* add the statusbar */ 
  statusbar = gnome_appbar_new(FALSE, TRUE, GNOME_PREFERENCES_USER);
  gnome_app_set_statusbar(app, statusbar);

  /* set up extensions menu and hints */
  gnc_extensions_menu_setup (app);

  /* make sure the file history is shown */ 
  gnc_history_update_menu (GTK_WIDGET (app));
}

static void
gnc_refresh_main_window_info (void)
{
  GList *containers = gtk_container_get_toplevels ();

  while (containers)
  {
    GtkWidget *w = containers->data;

    if (GNOME_IS_APP (w))
    {
      gnc_app_set_title (GNOME_APP (w));
      gnc_history_update_menu (w);
    }

    containers = containers->next;
  }
}


/********************************************************************
 * gnc_main_window_create_child()
 * open an MDI child given a config string (URL).  This is used at 
 * MDI session restore time 
 ********************************************************************/

static GnomeMDIChild * 
gnc_main_window_create_child(const gchar * configstring) {
  GnomeMDIChild *child;
  URLType type;
  char * location;
  char * label;

  if (!configstring)
  {
    gnc_main_window_open_accounts (FALSE);
    return NULL;
  }

  type = gnc_html_parse_url(NULL, configstring, &location, &label);
  g_free(location);
  g_free(label);

  switch(type) {
  case URL_TYPE_REPORT:
    child = gnc_report_window_create_child(configstring);
    break;
    
  case URL_TYPE_ACCTTREE:
    child = gnc_acct_tree_window_create_child(configstring);
    break;
    
  default:
    child = NULL;
  }

  return child;
}

/********************************************************************
 * gnc_main_window_can_*()
 ********************************************************************/

gboolean
gnc_main_window_can_save (GNCMDIInfo * wind)
{
  if (!wind) return FALSE;

  return gnc_mdi_has_apps ();
}

gboolean
gnc_main_window_can_cancel_save (GNCMDIInfo *wind)
{
  if (!wind) return FALSE;

  return gnc_mdi_has_apps ();
}

static gboolean
gnc_main_window_can_restore_cb (const char * filename)
{
  return !gnc_commodity_table_has_namespace (gnc_engine_commodities (),
                                             GNC_COMMODITY_NS_LEGACY);
}

/********************************************************************
 * gnc_main_window_new()
 * initialize the Gnome MDI system
 ********************************************************************/

GNCMDIInfo * 
gnc_main_window_new (void)
{
  GNCMDIInfo * retval;

  retval = gnc_mdi_new ("GnuCash", "GnuCash",
                        gnc_main_window_toolbar_prefix (),
                        gnc_main_window_toolbar_suffix (),
                        gnc_shutdown,
                        gnc_main_window_can_restore_cb,
                        gnc_main_window_create_child);

  g_return_val_if_fail (retval != NULL, NULL);

  /* these menu and toolbar options are the ones that are always 
   * available */ 
  gnc_main_window_create_menus (retval);

  /* set up the position where the child menus/toolbars will be 
   * inserted  */
  gnome_mdi_set_child_menu_path(GNOME_MDI(retval->mdi),
                                "_Tools");
  gnome_mdi_set_child_list_path(GNOME_MDI(retval->mdi),
                                "_Windows/");

  /* handle top-level signals */
  gtk_signal_connect(GTK_OBJECT(retval->mdi), "app_created",
                     GTK_SIGNAL_FUNC(gnc_main_window_app_created_cb),
                     retval);

  return retval;
}

/********************************************************************
 * menu/toolbar data structures and callbacks 
 * these are the "templates" that are installed in every toplevel
 * MDI window
 ********************************************************************/

static void
gnc_main_window_options_cb(GtkWidget *widget, gpointer data)
{
  gnc_show_options_dialog();
}

static void
gnc_main_window_file_new_file_cb(GtkWidget * widget, gpointer data)
{
  gnc_file_new ();
  gnc_refresh_main_window_info ();
}

static void
gnc_main_window_file_new_window_cb(GtkWidget * widget, gpointer data)
{
  GNCMDIInfo *main_info;
  GnomeMDI *mdi;

  main_info = gnc_mdi_get_current ();
  if (!main_info) return;

  mdi = main_info->mdi;
  if (!mdi) return;

  if (mdi->active_child && mdi->active_view)
  {
    if (!strcmp(mdi->active_child->name, _("Accounts")))
    {
      gnc_main_window_open_accounts (TRUE);
    }
    else
    {
      GnomeMDIChild * child = mdi->active_child;
      gnome_mdi_remove_view(mdi, mdi->active_view, FALSE);
      gnome_mdi_add_toplevel_view(mdi, child);
    }
  }
}

static void
gnc_main_window_file_open_cb(GtkWidget * widget, gpointer data)
{
  gnc_file_open ();
  gnc_refresh_main_window_info ();
}

static void
gnc_main_window_file_save_cb(GtkWidget * widget, gpointer data)
{
  gnc_file_save ();
  gnc_refresh_main_window_info ();
}

static void
gnc_main_window_file_save_as_cb(GtkWidget * widget, gpointer data)
{
  gnc_file_save_as ();
  gnc_refresh_main_window_info ();
}

static void
gnc_main_window_file_import_cb(GtkWidget * widget, gpointer data)
{
  gnc_file_qif_import ();
}

static void
gnc_main_window_file_export_cb(GtkWidget * widget, gpointer data)
{
  const char *filename;
  struct stat statbuf;
  gboolean ok;
  FILE *file;
  int rc;

  filename =  gnc_file_dialog (_("Export"), NULL, NULL);
  if (!filename)
    return;

  rc = stat (filename, &statbuf);

  /* Check for an error that isn't a non-existant file. */
  if (rc != 0 && errno != ENOENT)
  {
    const char *message = _("You cannot save to that filename.");
    char *string;

    string = g_strconcat (message, "\n\n", strerror (errno), NULL);
    gnc_error_dialog_parented (GTK_WINDOW (gtk_widget_get_toplevel (widget)),
                               string);
    g_free (string);
    return;
  }

  /* Check for a file that isn't a regular file. */
  if (rc == 0 && !S_ISREG (statbuf.st_mode))
  {
    const char *message = _("You cannot save to that file.");

    gnc_error_dialog_parented (GTK_WINDOW (gtk_widget_get_toplevel (widget)),
                               message);
    return;
  }

  if (rc == 0)
  {
    const char *format = _("The file \n    %s\n already exists.\n"
                           "Are you sure you want to overwrite it?");
    char *string;
    gboolean result;

    string = g_strdup_printf (format, filename);
    result = gnc_verify_dialog_parented (gtk_widget_get_toplevel (widget),
                                         string, FALSE);
    g_free (string);

    if (!result)
      return;
  }

  file = fopen (filename, "w");
  if (!file)
  {
    const char *message = _("You cannot save to that file.");
    char *string;

    string = g_strconcat (message, "\n\n", strerror (errno), NULL);
    gnc_error_dialog_parented (GTK_WINDOW (gtk_widget_get_toplevel (widget)),
                               string);
    g_free (string);
    return;
  }

  ok = FALSE;

  do
  {
    rc = fputs ("<?xml version=\"1.0\"?>\n", file);
    if (rc == EOF)
      break;

    rc = fputs ("<gnc-v2>\n", file);
    if (rc == EOF)
      break;

    write_commodities (file, gnc_get_current_book ());
    write_accounts (file, gnc_get_current_book ());

    rc = fputs ("<\\gnc-v2>\n", file);
    if (rc == EOF)
      break;

    write_emacs_trailer (file);

    rc = fclose (file);
    if (rc != 0)
      break;

    ok = TRUE;
  } while (FALSE);

  if (!ok)
  {
    const char *message = _("There was an error saving the file.");
    char *string;

    string = g_strconcat (message, "\n\n", strerror (errno), NULL);
    gnc_error_dialog_parented (GTK_WINDOW (gtk_widget_get_toplevel (widget)),
                               string);
    g_free (string);
    return;
  }
}

static void
gnc_main_window_file_shutdown_cb(GtkWidget * widget, gpointer data)
{
  gnc_shutdown (0);
}

static void
gnc_main_window_file_close_cb(GtkWidget * widget, gpointer data)
{
  GNCMDIInfo *main_info;
  GnomeMDI *mdi;

  main_info = gnc_mdi_get_current ();
  if (!main_info) return;

  mdi = main_info->mdi;
  if (!mdi) return;

  if (mdi->active_child)
  {
    GNCMDIChildInfo * inf;

    inf = gtk_object_get_user_data(GTK_OBJECT(mdi->active_child));
    if (inf->toolbar)
    {
      gtk_widget_destroy (GTK_WIDGET(inf->toolbar)->parent);
      inf->toolbar = NULL;
    }

    gnome_mdi_remove_child (mdi, mdi->active_child, FALSE);
  }  
  else
  {
    gnc_warning_dialog (_("Select \"Exit\" to exit GnuCash."));
  }
}

static void
gnc_main_window_fincalc_cb(GtkWidget *widget, gpointer data)
{
  gnc_ui_fincalc_dialog_create();
}

static void
gnc_ui_mainWindow_scheduled_xaction_cb(GtkWidget *widget, gpointer data)
{
  gnc_ui_scheduled_xaction_dialog_create();
}

static void
gnc_ui_mainWindow_nextrun_cb (GtkWidget *widget, gpointer data)
{
  gnc_ui_nextrun_dialog_create();
}

static void
gnc_main_window_gl_cb(GtkWidget *widget, gpointer data)
{
  GNCLedgerDisplay *ld;
  RegWindow *regData;

  ld = gnc_ledger_display_gl ();

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
gnc_main_window_sched_xaction_cb (GtkWidget *widget, gpointer data) {
  gnc_ui_scheduled_xaction_dialog_create();
}

static void
gnc_main_window_sched_xaction_slr_cb (GtkWidget *widget, gpointer data) {
  gnc_ui_sxsincelast_dialog_create();
}

static void
gnc_main_window_about_cb (GtkWidget *widget, gpointer data)
{
  GtkWidget *about;
  const gchar *message = _("The GnuCash personal finance manager.\n"
                           "The GNU way to manage your money!");
  const gchar *copyright = "(C) 1998-2001 Linas Vepstas";
  const gchar *authors[] = {
    "Rob Browning <rlb@cs.utexas.edu>",
    "Bill Gribble <grib@billgribble.com>",
    "James LewisMoss <dres@debian.org>",
    "Robert Graham Merkel <rgmerk@mira.net>",
    "Dave Peticolas <dave@krondo.com>",
    "Christian Stimming <stimming@tuhh.de>",
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
gnc_main_window_file_new_account_tree_cb(GtkWidget * w, gpointer data)
{
  gnc_main_window_open_accounts(FALSE);
}

static void
gnc_main_window_create_menus(GNCMDIInfo * maininfo)
{
  static GnomeUIInfo gnc_file_menu_template[] = 
  {
    GNOMEUIINFO_MENU_NEW_ITEM(N_("New _File"),
                              N_("Create a new file"),
                              gnc_main_window_file_new_file_cb, NULL),
    GNOMEUIINFO_MENU_OPEN_ITEM(gnc_main_window_file_open_cb, NULL),
    GNOMEUIINFO_MENU_SAVE_ITEM(gnc_main_window_file_save_cb, NULL),
    GNOMEUIINFO_MENU_SAVE_AS_ITEM(gnc_main_window_file_save_as_cb, NULL),
    {
      GNOME_APP_UI_ITEM,
      N_("Export Accounts..."),
      N_("Export the account hierarchy to a new file"),
      gnc_main_window_file_export_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
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
      N_("New _Account Tree"),
      N_("Open a new account tree view"),
      gnc_main_window_file_new_account_tree_cb, NULL, NULL, 
      GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL
    },    
    GNOMEUIINFO_SEPARATOR,
    {
      GNOME_APP_UI_ITEM,
      N_("Move to New Window"),
      N_("Open a new top-level GnuCash window for the current view"),
      gnc_main_window_file_new_window_cb, NULL, NULL, 
      GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL
    },    
    {
      GNOME_APP_UI_ITEM,
      N_("Close _Window"),
      N_("Close the current notebook page"),
      gnc_main_window_file_close_cb, NULL, NULL, 
      GNOME_APP_PIXMAP_NONE, NULL, 0, 0, NULL
    },    
    GNOMEUIINFO_MENU_EXIT_ITEM(gnc_main_window_file_shutdown_cb, NULL),
    GNOMEUIINFO_END
  };
  
  static GnomeUIInfo gnc_settings_menu_template[] =
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
  
  static GnomeUIInfo gnc_tools_menu_template[] =
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
    { GNOME_APP_UI_ITEM,
      N_("Scheduled Transactions List"),
      N_("A list of Scheduled Transactions"),
      gnc_main_window_sched_xaction_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    { GNOME_APP_UI_ITEM,
      N_("Scheduled Transactions Since-Last-Run..."),
      N_("Create Scheduled Transactions since the last-time-run."),
      gnc_main_window_sched_xaction_slr_cb, NULL, NULL,
      GNOME_APP_PIXMAP_NONE, NULL,
      0, 0, NULL
    },
    GNOMEUIINFO_END
  };
  
  static GnomeUIInfo gnc_help_menu_template[] =
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

  static GnomeUIInfo gnc_windows_menu_template[] =
  {
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
    GNOMEUIINFO_SUBTREE(N_("_Windows"), gnc_windows_menu_template),    
    GNOMEUIINFO_MENU_HELP_TREE(gnc_help_menu_template),
    GNOMEUIINFO_END
  };

  gnome_mdi_set_menubar_template(GNOME_MDI(maininfo->mdi),
                                 gnc_main_menu_template);

  gnc_file_history_add_after ("New _Account Tree");
}

static GnomeUIInfo *
gnc_main_window_toolbar_prefix (void)
{
  static GnomeUIInfo prefix[] = 
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
    { GNOME_APP_UI_ITEM,
      N_("Close"),
      N_("Close the current notebook page"),
      gnc_main_window_file_close_cb,
      NULL,
      NULL,
      GNOME_APP_PIXMAP_STOCK, 
      GNOME_STOCK_PIXMAP_CLOSE,
      0, 0, NULL
    },
    GNOMEUIINFO_SEPARATOR,
    GNOMEUIINFO_END
  };

  return prefix;
}

static GnomeUIInfo *
gnc_main_window_toolbar_suffix (void)
{
  static GnomeUIInfo suffix[] = 
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

  return suffix;
}
