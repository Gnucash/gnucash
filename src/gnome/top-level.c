/********************************************************************\
 * top-level.c -- Gnome GUI main for GnuCash                        *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <libguile.h>
#include <popt.h>
#include <stdlib.h>
#include <g-wrap-wct.h>
#include <X11/Xlib.h>

#include "TransLog.h"
#include "combocell.h"
#include "dialog-account.h"
#include "dialog-commodity.h"
#include "dialog-options.h"
#include "dialog-scheduledxaction.h"
#include "dialog-transfer.h"
#include "dialog-totd.h"
#include "dialog-utils.h"
#include "druid-hierarchy.h"
#include "file-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-file.h"
#include "gnc-hooks.h"
#include "gnc-main-window.h"
#include "gnc-menu-extensions.h"
#include "gnc-plugin-menu-additions.h" /* FIXME Remove this line*/
#include "gnc-plugin-account-tree.h" /* FIXME Remove this line*/
#include "gnc-plugin-basic-commands.h" /* FIXME Remove this line*/
#include "gnc-plugin-file-history.h" /* FIXME Remove this line*/
#include "gnc-plugin-register.h" /* FIXME Remove this line*/
#include "gnc-plugin-budget.h"
#include "gnc-plugin-page-register.h"
#include "gnc-plugin-manager.h" /* FIXME Remove this line*/
#include "gnc-icons.h" /* FIXME Remove this line*/
#include "gnc-splash.h"
#include "gnc-html.h"
#include "gnc-gnome-utils.h"
#include "gnc-gpg.h"
#include "gnc-report.h"
#include "gnc-split-reg.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnucash-color.h"
#include "gnucash-sheet.h"
#include "gnucash-style.h"
#include "guile-util.h"
#include "messages.h"
#include "top-level.h"
#include "window-report.h"


/** PROTOTYPES ******************************************************/
static void gnc_configure_date_format(void);


/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static int gnome_is_running = FALSE;
static int splash_is_initialized = FALSE;
static int gnome_is_initialized = FALSE;
static int gnome_is_terminating = FALSE;


gboolean
gnucash_ui_is_running(void)
{
  return gnome_is_running;
}

gboolean 
gnucash_ui_is_terminating(void)
{
  return gnome_is_terminating;
}

static void
gnc_global_options_help_cb (GNCOptionWin *win, gpointer dat)
{
  gnc_gnome_help (HF_CUSTOM, HL_GLOBPREFS);
}

static void
gnc_commodity_help_cb (void)
{
  gnc_gnome_help (HF_USAGE, HL_COMMODITY);
}

/* ============================================================== */
/* HTML Hadler for reports. */

#define IF_TYPE(URL_TYPE_STR,ENTITY_TYPE)                                   \
  if (strncmp (URL_TYPE_STR, location, strlen (URL_TYPE_STR)) == 0)         \
  {                                                                         \
    GUID guid;                                                              \
    QofCollection *col;                                                     \
    QofEntity *entity;                                                      \
    if (!string_to_guid (location + strlen(URL_TYPE_STR), &guid))           \
    {                                                                       \
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location); \
      return FALSE;                                                         \
    }                                                                       \
    col = qof_book_get_collection (book, ENTITY_TYPE);                      \
    entity = qof_collection_lookup_entity (col, &guid);                     \
    if (NULL == entity)                                                     \
    {                                                                       \
      result->error_message = g_strdup_printf (_("Entity Not Found: %s"), location); \
      return FALSE;                                                         \
    }                                                                       \


static gboolean
gnc_html_register_url_cb (const char *location, const char *label,
                          gboolean new_window, GNCURLResult *result)
{
  GncPluginPage *page = NULL;
  GNCSplitReg * gsr   = NULL;
  Split       * split = NULL;
  Account     * account = NULL;
  Transaction * trans;
  GList       * node;
  QofBook     * book = gnc_get_current_book();

  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="gnc-register:account=My Bank Account" */
  if (strncmp("account=", location, 8) == 0)
  {
    account = xaccGetAccountFromFullName (gnc_get_current_group (),
                                          location + 8, 
                                          gnc_get_account_separator ());
  }

  /* href="gnc-register:guid=12345678901234567890123456789012" */
  else IF_TYPE ("acct-guid=", GNC_ID_ACCOUNT)
    account = GNC_ACCOUNT(entity);
  }

  else IF_TYPE ("trans-guid=", GNC_ID_TRANS)
    trans = (Transaction *) entity;

    for (node = xaccTransGetSplitList (trans); node; node = node->next)
    {
      split = node->data;
      account = xaccSplitGetAccount(split);
      if (account) break;
    }

    if (!account)
    {
      result->error_message =
        g_strdup_printf (_("Transaction with no Accounts: %s"), location);
      return FALSE;
    }
  }
  else IF_TYPE ("split-guid=", GNC_ID_SPLIT)
    split = (Split *) entity;
    account = xaccSplitGetAccount(split);
  }
  else
  {
    result->error_message =
          g_strdup_printf (_("Unsupported entity type: %s"), location);
    return FALSE;
  }

  page = gnc_plugin_page_register_new (account, FALSE);
  gnc_main_window_open_page (NULL, page);
  if (split) {
      gsr = gnc_plugin_page_register_get_gsr(page);
      gnc_split_reg_jump_to_split( gsr, split );
  }

  return TRUE;
}

/* ============================================================== */

static gboolean
gnc_html_price_url_cb (const char *location, const char *label,
                       gboolean new_window, GNCURLResult *result)
{
  QofBook * book = gnc_get_current_book();
  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="gnc-register:guid=12345678901234567890123456789012" */
  IF_TYPE ("price-guid=", GNC_ID_PRICE)
    if (!gnc_price_edit_by_guid (NULL, &guid)) 
    {
        result->error_message = g_strdup_printf (_("No such price: %s"),
                                                 location);
        return FALSE;
    }
  }
  else
  {
      result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                               location);
      return FALSE;
  }

  return TRUE;
}

/* ============================================================== */

SCM
gnc_gui_init_splash (SCM command_line)
{
  SCM ret = command_line;

  ENTER (" ");

  if (!splash_is_initialized)
  {
    splash_is_initialized = TRUE;

    ret = gnc_gnome_init ("gnucash", "GnuCash", VERSION, command_line);

    /* put up splash screen */
    gnc_show_splash_screen ();
  }

  LEAVE (" ");

  return ret;
}

SCM
gnc_gui_init (SCM command_line)
{
  SCM ret = command_line;
  GncMainWindow *main_window;

  ENTER (" ");

  if (!gnome_is_initialized)
  {
    /* Make sure the splash (and hense gnome) was initialized */
    if (!splash_is_initialized)
      ret = gnc_gui_init_splash (ret);

    gnome_is_initialized = TRUE;

    gnc_ui_util_init();
    gnc_configure_date_format();
    gnc_gconf_general_register_cb(KEY_DATE_FORMAT,
				  (GncGconfGeneralCb)gnc_configure_date_format, NULL);
    gnc_gconf_general_register_any_cb((GncGconfGeneralAnyCb)gnc_gui_refresh_all, NULL);

    if (!gnucash_style_init())
      gnc_shutdown(1);
    gnucash_color_init();

    gnc_html_register_url_handler (URL_TYPE_REGISTER,
                                   gnc_html_register_url_cb);

    gnc_html_register_url_handler (URL_TYPE_PRICE,
                                   gnc_html_price_url_cb);

    gnc_ui_commodity_set_help_callback (gnc_commodity_help_cb);

    gnc_file_set_shutdown_callback (gnc_shutdown);

    gnc_options_dialog_set_global_help_cb (gnc_global_options_help_cb, NULL);

    gnc_totd_dialog(NULL, TRUE);
    gnc_ui_sx_initialize();

    main_window = gnc_main_window_new ();
    gtk_widget_show (GTK_WIDGET (main_window));

    /* FIXME Remove this test code */
    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), gnc_plugin_account_tree_new ());
    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), gnc_plugin_basic_commands_new ());
    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), gnc_plugin_file_history_new ());
    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), gnc_plugin_menu_additions_new ());
    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (), gnc_plugin_register_new ());
    /* I'm not sure why the FIXME note says to remove this.  Maybe
       each module should be adding its own plugin to the manager?
       Anyway... Oh, maybe... nah */
    gnc_plugin_manager_add_plugin (gnc_plugin_manager_get (),
                                   gnc_plugin_budget_new ());
    gnc_load_stock_icons ();
    gnc_ui_hierarchy_druid_initialize();

    /* Run the ui startup hooks. */
    gnc_hook_run(HOOK_UI_STARTUP, NULL);

    // return ( main_window . command_line )
    {
      SCM gncMainWindowType;
      gncMainWindowType = scm_c_eval_string("<gnc:MainWindow*>");
      ret = scm_cons( gw_wcp_assimilate_ptr(main_window, gncMainWindowType), ret );
    }
  }

  LEAVE (" ");

  return ret;
}

/* ============================================================== */

void
gnc_gui_shutdown (void)
{
  if (gnome_is_running && !gnome_is_terminating)
  {
    gnome_is_terminating = TRUE;

    gtk_main_quit();

    gnc_gnome_shutdown ();
  }
}

/* ============================================================== */

void
gnc_gui_destroy (void)
{
  if (!gnome_is_initialized)
    return;

  gnc_extensions_shutdown ();
}

/* ============================================================== */

static gboolean
gnc_ui_check_events (gpointer not_used)
{
  QofSession *session;
  gboolean force;

  if (gtk_main_level() != 1)
    return TRUE;

  session = qof_session_get_current_session ();
  if (!session)
    return TRUE;

  if (gnc_gui_refresh_suspended ())
    return TRUE;

  if (!qof_session_events_pending (session))
    return TRUE;

  gnc_suspend_gui_refresh ();

  force = qof_session_process_events (session);

  gnc_resume_gui_refresh ();

  if (force)
    gnc_gui_refresh_all ();

  return TRUE;
}

static int
gnc_x_error (Display        *display, XErrorEvent *error)
{
  if (error->error_code)
  {
    char buf[64];

    XGetErrorText (display, error->error_code, buf, 63);

    g_warning ("X-ERROR **: %s\n  serial %ld error_code %d "
               "request_code %d minor_code %d\n", 
               buf, 
               error->serial, 
               error->error_code, 
               error->request_code,
               error->minor_code);
  }

  return 0;
}

int
gnc_ui_start_event_loop (void)
{
  guint id;

  gnome_is_running = TRUE;

  id = g_timeout_add_full (G_PRIORITY_DEFAULT_IDLE, 10000, /* 10 secs */
                           gnc_ui_check_events, NULL, NULL);

  XSetErrorHandler (gnc_x_error);

  /* Enter gnome event loop */
  gtk_main ();

  g_source_remove (id);

  gnome_is_running = FALSE;
  gnome_is_terminating = FALSE;

  return 0;
}

/* ============================================================== */

/* gnc_configure_date_format
 *    sets dateFormat to the current value on the scheme side
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void 
gnc_configure_date_format (void)
{
  char *format_code = gnc_gconf_get_string(GCONF_GENERAL, KEY_DATE_FORMAT, NULL);

  QofDateFormat df;

  if (format_code == NULL)
    format_code = g_strdup("locale");
  if (*format_code == '\0') {
    g_free(format_code);
    format_code = g_strdup("locale");
  }

  if (gnc_date_string_to_dateformat(format_code, &df))
  {
    PERR("Incorrect date format code");
    if (format_code != NULL)
      free(format_code);
    return;
  }

  qof_date_format_set(df);

  if (format_code != NULL)
    free(format_code);
}

/****************** END OF FILE **********************/
