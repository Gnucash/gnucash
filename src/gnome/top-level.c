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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <gnome.h>
#include <guile/gh.h>
#include <popt.h>
#include <stdlib.h>
#include <g-wrap-runtime-guile.h>
#include <X11/Xlib.h>

#include "AccWindow.h"
#include "TransLog.h"
#include "combocell.h"
#include "date.h"
#include "dialog-commodity.h"
#include "dialog-options.h"
#include "dialog-transfer.h"
#include "dialog-utils.h"
#include "file-utils.h"
#include "global-options.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-file.h"
#include "gnc-menu-extensions.h"
#include "gnc-network.h"
#include "gnc-splash.h"
#include "gnc-html.h"
#include "gnc-gnome-utils.h"
#include "gnc-gpg.h"
#include "gnc-report.h"
#include "gnc-ui.h"
#include "gnucash-color.h"
#include "gnucash-sheet.h"
#include "gnucash-style.h"
#include "guile-util.h"
#include "messages.h"
#include "split-register.h"
#include "top-level.h"
#include "window-help.h"
#include "window-main.h"
#include "window-acct-tree.h"
#include "window-register.h"
#include "window-report.h"


/** PROTOTYPES ******************************************************/
static void gnc_configure_date_format_cb(gpointer);
static void gnc_configure_date_format(void);
static void gnc_configure_account_separator_cb(gpointer);
static void gnc_configure_register_colors_cb(gpointer);
static void gnc_configure_register_colors(void);
static void gnc_configure_register_borders_cb(gpointer);
static void gnc_configure_register_borders(void);
static void gnc_configure_auto_raise_cb(gpointer);
static void gnc_configure_auto_raise(void);
static void gnc_configure_negative_color_cb(gpointer);
static void gnc_configure_negative_color(void);
static void gnc_configure_auto_decimal_cb(gpointer);
static void gnc_configure_auto_decimal(void);
static void gnc_configure_auto_decimal_places_cb(gpointer);
static void gnc_configure_auto_decimal_places(void);
static void gnc_configure_register_font_cb(gpointer);
static void gnc_configure_register_font(void);
static void gnc_configure_register_hint_font_cb(gpointer);
static void gnc_configure_register_hint_font(void);


/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static int gnome_is_running = FALSE;
static int gnome_is_initialized = FALSE;
static int gnome_is_terminating = FALSE;

static SCM date_callback_id = SCM_UNDEFINED;
static SCM account_separator_callback_id = SCM_UNDEFINED;
static SCM register_colors_callback_id = SCM_UNDEFINED;
static SCM register_borders_callback_id = SCM_UNDEFINED;
static SCM auto_raise_callback_id = SCM_UNDEFINED;
static SCM negative_color_callback_id = SCM_UNDEFINED;
static SCM auto_decimal_callback_id = SCM_UNDEFINED;
static SCM auto_decimal_places_callback_id = SCM_UNDEFINED;
static SCM register_font_callback_id = SCM_UNDEFINED;
static SCM register_hint_font_callback_id = SCM_UNDEFINED;


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
  helpWindow (NULL, NULL, HH_GLOBPREFS);
}

static gboolean
gnc_html_register_url_cb (const char *location, const char *label,
                          gboolean new_window, GNCURLResult *result)
{
  RegWindow   * reg = NULL;
  Split       * split = NULL;
  Account     * account;
  Transaction * trans;
  GList       * node;

  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="gnc-register:account=My Bank Account" */
  if (strncmp("account=", location, 8) == 0)
  {
    account = xaccGetAccountFromFullName (gnc_get_current_group (),
                                          location + 8, 
                                          gnc_get_account_separator ());
    reg = regWindowSimple (account);
    gnc_register_raise (reg);
  }
  /* href="gnc-register:guid=12345678901234567890123456789012" */
  else if (strncmp ("guid=", location, 5) == 0)
  {
    GUID guid;
    GNCIdType id_type;

    if (!string_to_guid (location + 5, &guid))
    {
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
      return FALSE;
    }

    id_type = xaccGUIDType (&guid, gnc_get_current_book ());
    if (id_type == GNC_ID_NONE || !safe_strcmp (id_type, GNC_ID_NULL))
    {
        result->error_message = g_strdup_printf (_("No such entity: %s"),
                                                 location);
        return FALSE;
    }
    else if (!safe_strcmp (id_type, GNC_ID_ACCOUNT))
    {
        account = xaccAccountLookup (&guid, gnc_get_current_book ());
        reg = regWindowSimple (account);
    }
    else if (!safe_strcmp (id_type, GNC_ID_TRANS))
    {
        trans = xaccTransLookup (&guid, gnc_get_current_book ());
        split = NULL;

        for (node = xaccTransGetSplitList (trans); node; node = node->next)
        {
          split = node->data;
          if (xaccSplitGetAccount (split))
            break;
        }

        if (!split)
        {
          result->error_message =
            g_strdup_printf (_("Transaction with no Accounts: %s"), location);
          return FALSE;
        }

        reg = regWindowSimple (xaccSplitGetAccount (split));
    }
    else if (!safe_strcmp (id_type, GNC_ID_SPLIT))
    {
        split = xaccSplitLookup (&guid, gnc_get_current_book ());
        if (!split)
        {
          result->error_message = g_strdup_printf (_("No such split: %s"),
                                                   location);
          return FALSE;
        }

        reg = regWindowSimple (xaccSplitGetAccount (split));
    }
    else
    {
        result->error_message =
          g_strdup_printf (_("Unsupported entity type: %s"), location);
        return FALSE;
    }

    gnc_register_raise(reg);
    if (split)
      gnc_register_jump_to_split (reg, split);
  }
  else
  {
    result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                             location);
    return FALSE;
  }

  return TRUE;
}

static void
gnc_commodity_help_cb (void)
{
  helpWindow (NULL, _("Help"), HH_COMMODITY);
}

/* ============================================================== */

SCM
gnc_gui_init (SCM command_line)
{
  SCM ret = command_line;

  ENTER (" ");

  if (!gnome_is_initialized)
  {
    ret = gnc_gnome_init ("gnucash", "GnuCash", VERSION, command_line);
    gnome_is_initialized = TRUE;

    /* load default HTML action handlers */ 
    gnc_network_init();

    /* put up splash screen */
    gnc_show_splash_screen ();
    
    /* make sure splash is up */
    while (gtk_events_pending ())
      gtk_main_iteration ();

    gnc_configure_date_format();
    date_callback_id =
      gnc_register_option_change_callback(gnc_configure_date_format_cb, NULL,
                                          "International", "Date Format");

    account_separator_callback_id = 
      gnc_register_option_change_callback(gnc_configure_account_separator_cb,
                                          NULL, "General",
                                          "Account Separator");

    gnc_configure_register_colors();
    register_colors_callback_id = 
      gnc_register_option_change_callback(gnc_configure_register_colors_cb,
                                          NULL, "Register Colors", NULL);

    gnc_configure_register_borders();
    register_borders_callback_id = 
      gnc_register_option_change_callback(gnc_configure_register_borders_cb,
                                          NULL, "Register", NULL);
    
    gnc_configure_auto_raise();
    auto_raise_callback_id = 
      gnc_register_option_change_callback(gnc_configure_auto_raise_cb,
                                          NULL, "Register",
                                          "Auto-Raise Lists");

    gnc_configure_negative_color();
    negative_color_callback_id = 
      gnc_register_option_change_callback(gnc_configure_negative_color_cb,
                                          NULL, "General",
                                          "Display negative amounts in red");

    gnc_configure_auto_decimal();
    auto_decimal_callback_id =
      gnc_register_option_change_callback(gnc_configure_auto_decimal_cb,
                                          NULL, "General",
                                          "Automatic Decimal Point");

    gnc_configure_auto_decimal_places();
    auto_decimal_places_callback_id = 
      gnc_register_option_change_callback(gnc_configure_auto_decimal_places_cb,
                                          NULL, "General",
                                          "Auto Decimal Places");

    gnc_configure_register_font();
    register_font_callback_id =
      gnc_register_option_change_callback(gnc_configure_register_font_cb,
                                          NULL, "Register", "Register font");

    gnc_configure_register_hint_font();
    register_hint_font_callback_id =
      gnc_register_option_change_callback(gnc_configure_register_hint_font_cb,
                                          NULL, "Register",
                                          "Register hint font");

    gnucash_style_init();
    gnucash_color_init();

    gnc_html_register_url_handler (URL_TYPE_REGISTER,
                                   gnc_html_register_url_cb);

    gnc_ui_commodity_set_help_callback (gnc_commodity_help_cb);

    gnc_file_set_can_cancel_callback (gnc_mdi_has_apps);

    gnc_options_dialog_set_global_help_cb (gnc_global_options_help_cb, NULL);

    /* initialize gnome MDI and set up application window defaults  */
    if (!gnc_mdi_get_current ())
      gnc_main_window_new ();

    /* Run the ui startup hooks. */
    {
      SCM run_danglers = gh_eval_str("gnc:hook-run-danglers");
      SCM hook = gh_eval_str("gnc:*ui-startup-hook*");
      gh_call1(run_danglers, hook); 
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

  gnc_unregister_option_change_callback_id(date_callback_id);
  gnc_unregister_option_change_callback_id(account_separator_callback_id);
  gnc_unregister_option_change_callback_id(register_colors_callback_id);
  gnc_unregister_option_change_callback_id(register_borders_callback_id);
  gnc_unregister_option_change_callback_id(auto_raise_callback_id);
  gnc_unregister_option_change_callback_id(negative_color_callback_id);
  gnc_unregister_option_change_callback_id(register_font_callback_id);
  gnc_unregister_option_change_callback_id(register_hint_font_callback_id);

  gnc_mdi_destroy (gnc_mdi_get_current ());

  gnc_extensions_shutdown ();
}

/* ============================================================== */

static gboolean
gnc_ui_check_events (gpointer not_used)
{
  GNCSession *session;
  gboolean force;

  if (gtk_main_level() != 1)
    return TRUE;

  session = gnc_get_current_session ();
  if (!session)
    return TRUE;

  if (gnc_gui_refresh_suspended ())
    return TRUE;

  if (!gnc_session_events_pending (session))
    return TRUE;

  gnc_suspend_gui_refresh ();

  force = gnc_session_process_events (session);

  gnc_resume_gui_refresh ();

  if (force)
    gnc_gui_refresh_all ();

  return TRUE;
}

static int
gnc_x_error (Display	 *display,
	     XErrorEvent *error)
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

/* gnc_configure_date_format_cb
 *    Callback called when options change - sets dateFormat to the current
 *    value on the scheme side and refreshes register windows
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void 
gnc_configure_date_format_cb (gpointer data)
{
  gnc_configure_date_format ();
  gnc_gui_refresh_all ();
}


/* gnc_configure_date_format
 *    sets dateFormat to the current value on the scheme side
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void 
gnc_configure_date_format (void)
{
  char *format_code = gnc_lookup_multichoice_option("International", 
                                                    "Date Format",
                                                    "us");

  DateFormat df;

  if( safe_strcmp(format_code, "us") == 0)
  {
    df = DATE_FORMAT_US;
  }

  else if( safe_strcmp(format_code, "uk") == 0)
  {
    df = DATE_FORMAT_UK;
  }

  else if( safe_strcmp(format_code, "ce") == 0)
  {
    df = DATE_FORMAT_CE;
  }

  else if( safe_strcmp(format_code, "iso") == 0)
  {
    df = DATE_FORMAT_ISO;
  }

  else if( safe_strcmp(format_code, "locale") == 0)
  {
    df = DATE_FORMAT_LOCALE;
  }

  else
  {
    PERR("Incorrect date format code");
    return;
  }

  setDateFormat(df);

  if (format_code != NULL)
    free(format_code);
}

/* gnc_configure_account_separator_cb
 *    Callback called when options change - sets account separator
 *    to the current value on the scheme side
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void 
gnc_configure_account_separator_cb (gpointer data)
{
  gnc_gui_refresh_all ();
}

/* gnc_configure_register_colors_cb
 *    Callback called when options change - sets
 *    register colors to their guile values
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_register_colors_cb (gpointer data)
{
  gnc_configure_register_colors ();
  gnc_gui_refresh_all ();
}

/* gnc_configure_register_colors_cb
 *    sets register colors to their guile values
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_register_colors (void)
{
  SplitRegisterColors reg_colors;

  reg_colors.header_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Header color",
                                 0xffffff);

  reg_colors.primary_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Primary color",
                                 0xffffff);

  reg_colors.secondary_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Secondary color",
                                 0xffffff);

  reg_colors.primary_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Primary active color",
                                 0xffffff);

  reg_colors.secondary_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Secondary active color",
                                 0xffffff);

  reg_colors.split_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Split color",
                                 0xffffff);

  reg_colors.split_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Split active color",
                                 0xffffff);

  reg_colors.double_alternate_virt =
    gnc_lookup_boolean_option("Register Colors",
                              "Double mode colors alternate with transactions",
                              FALSE);

  gnc_split_register_set_colors (reg_colors);
}


/* gnc_configure_register_borders_cb
 *    Callback called when options change - sets
 *    register borders to their guile values
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_register_borders_cb (gpointer data)
{
  gnc_configure_register_borders ();
  gnc_gui_refresh_all ();
}

/* gnc_configure_register_border
 *    sets register borders to their guile values
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_register_borders (void)
{
  gboolean use_vertical_lines;
  gboolean use_horizontal_lines;

  use_vertical_lines = gnc_lookup_boolean_option("Register",
                                                 "Show Vertical Borders",
                                                 FALSE);

  
  use_horizontal_lines = gnc_lookup_boolean_option("Register",
                                                   "Show Horizontal Borders",
                                                   FALSE);

  gnucash_style_config_register_borders (use_vertical_lines,
                                         use_horizontal_lines);
}

/* gnc_configure_auto_raise_cb
 *    Callback called when options change - sets
 *    auto-raise status of combocell class
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_auto_raise_cb (gpointer data)
{
  gnc_configure_auto_raise ();
}

/* gnc_configure_auto_raise
 *    sets combocell auto raise status
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_auto_raise (void)
{
  gboolean auto_pop;

  auto_pop = gnc_lookup_boolean_option("Register", "Auto-Raise Lists", TRUE);

  gnc_combo_cell_set_autopop (auto_pop);
}

/* gnc_configure_negative_color_cb
 *    Callback called when options change - sets
 *    negative amount color flags
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_negative_color_cb (gpointer data)
{
  gnc_configure_negative_color ();

  gnc_gui_refresh_all ();
}

/* gnc_configure_negative_color
 *    sets negative amount color flags
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_negative_color(void)
{
  gboolean use_red;

  use_red = gnc_lookup_boolean_option("General",
                                      "Display negative amounts in red",
                                      TRUE);

  gnc_split_register_colorize_negative (use_red);
}


/* gnc_configure_auto_decimal_cb
 *     Callback called when options change -
 *     sets auto decimal option.
 * 
 *  Args: Nothing
 *  Returns: Nothing
 */
static void
gnc_configure_auto_decimal_cb(gpointer not_used)
{
  gnc_configure_auto_decimal();
}

/* gnc_configure_auto_decimal
 *     Pass the global value for the auto decimal field to the engine.
 * 
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_auto_decimal(void)
{
  gboolean enabled;

  enabled = gnc_lookup_boolean_option("General",
                                      "Automatic Decimal Point",
                                      FALSE);

  gnc_set_auto_decimal_enabled(enabled);
}

/* gnc_configure_auto_decimal_places_cb
 *     Callback called when options change -
 *     sets auto decimal places option.
 * 
 *  Args: Nothing
 *  Returns: Nothing
 */
static void
gnc_configure_auto_decimal_places_cb (gpointer not_used)
{
  gnc_configure_auto_decimal_places ();
}

/* gnc_configure_auto_decimal_places
 *     Pass the global value for the auto decimal places range to the engine.
 * 
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_auto_decimal_places (void)
{
   gnc_set_auto_decimal_places
     (gnc_lookup_number_option("General",
                               "Auto Decimal Places", 2));
}


/* gnc_configure_register_font_cb
 *     Callback called when options change -
 *     sets register font
 * 
 *  Args: unused data
 *  Returns: Nothing
 */
static void
gnc_configure_register_font_cb (gpointer not_used)
{
  gnc_configure_register_font ();
}

/* gnc_configure_register_font
 *     Set up the register font
 * 
 *  Args: Nothing
 *  Returns: Nothing
 */
static void
gnc_configure_register_font(void)
{
  char *font_name;

  font_name = gnc_lookup_font_option("Register", "Register font", NULL);

  gnucash_style_set_register_font_name(font_name);

  if (font_name != NULL)
    free(font_name);
}

/* gnc_configure_register_hint_font_cb
 *     Callback called when options change -
 *     sets register hint font
 * 
 *  Args: unused data
 *  Returns: Nothing
 */
static void
gnc_configure_register_hint_font_cb(gpointer not_used)
{
  gnc_configure_register_hint_font();
}

/* gnc_configure_register_hint_font
 *     Set up the register hint font
 * 
 *  Args: Nothing
 *  Returns: Nothing
 */
static void
gnc_configure_register_hint_font(void)
{
  char *font_name;

  font_name = gnc_lookup_font_option("Register", "Register hint font", NULL);

  gnucash_style_set_register_hint_font_name(font_name);

  if (font_name != NULL)
    free(font_name);
}

/****************** END OF FILE **********************/
