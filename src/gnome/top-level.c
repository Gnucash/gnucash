/********************************************************************\
 * top-level.c -- Gnome GUI main for GnuCash                        *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
 *                                                                  *
\********************************************************************/

#include "top-level.h"

#include <stdlib.h>
#include <guile/gh.h>
#include <gnome.h>

#include "window-main.h"
#include "global-options.h"
#include "gnucash-sheet.h"
#include "gnucash-color.h"
#include "gnucash-style.h"
#include "scripts_menu.h"
#include "window-help.h"
#include "window-report.h"
#include "FileIO.h"
#include "FileBox.h"
#include "FileDialog.h"
#include "MainWindow.h"
#include "Destroy.h"
#include "Refresh.h"
#include "messages.h"
#include "TransLog.h"
#include "util.h"
#include "date.h"
#include "AccWindow.h"


/** PROTOTYPES ******************************************************/
static void gnc_configure_date_format_cb(void *);
static void gnc_configure_date_format(void);
static void gnc_configure_newacc_currency_cb(void *);
static void gnc_configure_newacc_currency(void);

/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GtkWidget *app = NULL;

static int gnome_is_running = FALSE;
static int gnome_is_initialized = FALSE;
static int gnome_is_terminating = FALSE;

static SCM date_callback_id = SCM_UNDEFINED;
static SCM currency_callback_id = SCM_UNDEFINED;

/* ============================================================== */

int 
gnucash_ui_is_running() {
  return gnome_is_running;
}

/* ============================================================== */

int 
gnucash_ui_is_terminating() {
  return gnome_is_terminating;
}

/* ============================================================== */

gncUIWidget
gnc_get_ui_data() {
  return app;
}

/* ============================================================== */

/* These gnucash_ui_init and gnucash_ui functions are just hacks to get
   the guile stuff up and running.  Expect a more formal definition of
   what they should do soon, and expect that the open/select functions
   will be merged with the code in FMB_OPEN in MainWindow.c */

int
gnucash_ui_init()
{
  int fake_argc = 1;
  char *fake_argv[] = {"gnucash"};

  ENTER ("gnucash_ui_init");

  /* We're going to have to have other ways to handle X and GUI
     specific args... */
  if (!gnome_is_initialized)
  {
    gnome_init("GnuCash", NULL, fake_argc, fake_argv);
    gnome_is_initialized = TRUE;

    app = gnome_app_new("GnuCash", "GnuCash");

    gnc_options_init();

    gnc_configure_date_format();
    date_callback_id =
      gnc_register_option_change_callback(gnc_configure_date_format_cb, NULL,
                                          "International", "Date Format");

    gnc_configure_newacc_currency();
    currency_callback_id = 
      gnc_register_option_change_callback(gnc_configure_newacc_currency_cb,
                                          NULL, "International",
                                          "Default Currency");

    mainWindow();

    gnucash_style_init();
    gnucash_color_init();
  }

  LEAVE ("gnucash_ui_init");

  return 0;
}

/* ============================================================== */

void
gnc_ui_shutdown (void)
{
  if (gnome_is_running && !gnome_is_terminating)
  {
    gnome_is_terminating = TRUE;
    gnc_ui_destroy_all_subwindows();
    gtk_widget_hide(app);
    gtk_main_quit();
  }
}

/* ============================================================== */

void
gnc_ui_destroy_all_subwindows (void)
{
  xaccGroupWindowDestroy(gncGetCurrentGroup());
  gnc_ui_destroy_help_windows();
  gnc_ui_destroy_report_windows();
}

/* ============================================================== */

void
gnc_ui_destroy (void)
{
  if (!gnome_is_initialized)
    return;

  gnc_unregister_option_change_callback_id(date_callback_id);
  gnc_unregister_option_change_callback_id(currency_callback_id);

  if (app != NULL)
  {
    gtk_widget_destroy(app);
    app = NULL;
  }

  gnc_options_shutdown();
  gnc_extensions_shutdown();
}

/* ============================================================== */

int
gnc_ui_main()
{
  /* Initialize gnome */
  gnucash_ui_init();

  gnc_refresh_main_window();
  gtk_widget_show(app);

  gnome_is_running = TRUE;

  /* Enter gnome event loop */
  gtk_main();

  gnome_is_running = FALSE;
  gnome_is_terminating = FALSE;

  return 0;
}

/* hack alert -- all we do below is rename some functions ... fix this someday */
/* ============================================================== */

int
gnucash_ui_open_file(const char name[])
{
  gncFileOpenFile(name);
  return (1);
}

/* ============================================================== */

int
gnucash_ui_select_file()
{
  gncFileOpen();
  return (1);
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
gnc_configure_date_format_cb(void *data)
{
  gnc_configure_date_format();
  gnc_group_ui_refresh(gncGetCurrentGroup());
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

/* gnc_configure_date_format_cb
 *    Callback called when options change - sets default currency to
 *    the current value on the scheme size
 *  
 * Args: Nothing
 * Returns: Nothing
 */
static void 
gnc_configure_newacc_currency_cb(void *data)
{
  gnc_configure_newacc_currency();
}

/* gnc_configure_newacc_currency
 *    sets the default currency for new accounts to the 
 *    current value on the scheme side
 *  
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_newacc_currency(void)
{
  char *newacc_def_currency = 
    gnc_lookup_string_option("International",
                             "Default Currency",
                             "USD");
  xaccSetDefaultNewaccountCurrency(newacc_def_currency);

  if (newacc_def_currency != NULL)
    free(newacc_def_currency);
}

/****************** END OF FILE **********************/
