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

#include "top-level.h"

#include <stdlib.h>
#include <guile/gh.h>
#include <gnome.h>

#include "gnome-top-level.h"
#include "window-main.h"
#include "global-options.h"
#include "gnucash-sheet.h"
#include "gnucash-color.h"
#include "gnucash-style.h"
#include "extensions.h"
#include "window-help.h"
#include "window-report.h"
#include "dialog-utils.h"
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
#include "SplitLedger.h"
#include "splitreg.h"


/** PROTOTYPES ******************************************************/
static void gnc_configure_date_format_cb(void *);
static void gnc_configure_date_format(void);
static void gnc_configure_newacc_currency_cb(void *);
static void gnc_configure_newacc_currency(void);
static void gnc_configure_account_separator_cb(void *);
static void gnc_configure_account_separator(void);
static void gnc_configure_register_colors_cb(void *);
static void gnc_configure_register_colors(void);
static void gnc_configure_register_borders_cb(void *);
static void gnc_configure_register_borders(void);
static void gnc_configure_reverse_balance_cb(void *);
static void gnc_configure_reverse_balance(void);
static void gnc_configure_debit_credit_labels_cb(void *);
static void gnc_configure_debit_credit_labels(void);

/** GLOBALS *********************************************************/
/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GtkWidget *app = NULL;

static int gnome_is_running = FALSE;
static int gnome_is_initialized = FALSE;
static int gnome_is_terminating = FALSE;

static SCM date_callback_id = SCM_UNDEFINED;
static SCM currency_callback_id = SCM_UNDEFINED;
static SCM account_separator_callback_id = SCM_UNDEFINED;
static SCM register_colors_callback_id = SCM_UNDEFINED;
static SCM register_borders_callback_id = SCM_UNDEFINED;
static SCM reverse_balance_callback_id = SCM_UNDEFINED;
static SCM debit_credit_labels_callback_id = SCM_UNDEFINED;

/* ============================================================== */

int 
gnucash_ui_is_running()
{
  return gnome_is_running;
}

/* ============================================================== */

int 
gnucash_ui_is_terminating()
{
  return gnome_is_terminating;
}

/* ============================================================== */

gncUIWidget
gnc_get_ui_data()
{
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

    gnc_configure_account_separator();
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
    
    gnc_configure_reverse_balance();
    reverse_balance_callback_id = 
      gnc_register_option_change_callback(gnc_configure_reverse_balance_cb,
                                          NULL, "General",
                                          "Reversed-balance account types");

    gnc_configure_debit_credit_labels();
    debit_credit_labels_callback_id = 
      gnc_register_option_change_callback(gnc_configure_debit_credit_labels_cb,
                                          NULL, "Register",
                                          "Always use debit/credit labels");

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
    gnc_ui_mainWindow_save_size();
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
  gnc_unregister_option_change_callback_id(account_separator_callback_id);
  gnc_unregister_option_change_callback_id(register_colors_callback_id);
  gnc_unregister_option_change_callback_id(register_borders_callback_id);  
  gnc_unregister_option_change_callback_id(reverse_balance_callback_id);  
  gnc_unregister_option_change_callback_id(debit_credit_labels_callback_id);  

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

/* hack alert -- all we do below is rename some functions. fix this someday */
/* ============================================================== */

int
gnucash_ui_open_file(const char name[])
{
  gncFileOpenFile(name);
  return 1;
}

/* ============================================================== */

int
gnucash_ui_select_file()
{
  gncFileOpen();
  return 1;
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
 *    the current value on the scheme side
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

/* gnc_configure_account_separator_cb
 *    Callback called when options change - sets account separator
 *    to the current value on the scheme side
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void 
gnc_configure_account_separator_cb(void *data)
{
  gnc_configure_account_separator();
  gnc_group_ui_refresh(gncGetCurrentGroup());
}

/* gnc_configure_account_separator
 *    sets the account separator to the
 *    current value on the scheme side
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_account_separator(void)
{
  char separator = gnc_get_account_separator();

  xaccSRSetAccountSeparator(separator);
}

/* gnc_configure_register_colors_cb
 *    Callback called when options change - sets
 *    register colors to their guile values
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_register_colors_cb(void *data)
{
  gnc_configure_register_colors();
  gnc_group_ui_refresh(gncGetCurrentGroup());
}

/* gnc_configure_register_colors_cb
 *    sets register colors to their guile values
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_register_colors(void)
{
  SplitRegisterColors reg_colors;

  reg_colors.single_cursor_passive_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Single mode default even row background",
                                 0xccccff);

  reg_colors.single_cursor_passive_bg_color2 =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Single mode default odd row background",
                                 0xccccff);

  reg_colors.single_cursor_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Single mode active background",
                                 0xffdddd);

  reg_colors.double_cursor_passive_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Double mode default even row background",
                                 0xccccff);

  reg_colors.double_cursor_passive_bg_color2 =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Double mode default odd row background",
                                 0xffffff);

  reg_colors.double_alternate_virt =
    gnc_lookup_boolean_option("Register Colors",
                              "Double mode colors alternate with transactions",
                              FALSE);

  reg_colors.double_cursor_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Double mode active background",
                                 0xffdddd);

  reg_colors.trans_cursor_passive_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Multi mode default transaction background",
                                 0xccccff);

  reg_colors.trans_cursor_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Multi mode active transaction background",
                                 0xffdddd);

  reg_colors.split_cursor_passive_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Multi mode default split background",
                                 0xffffff);

  reg_colors.split_cursor_active_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Multi mode active split background",
                                 0xffffdd);

  reg_colors.header_bg_color =
    gnc_lookup_color_option_argb("Register Colors",
                                 "Header background",
                                 0xffffff);

  xaccSetSplitRegisterColors(reg_colors);
}


/* gnc_configure_register_borders_cb
 *    Callback called when options change - sets
 *    register borders to their guile values
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_register_borders_cb(void *data)
{
  gnc_configure_register_borders();
  gnc_group_ui_refresh(gncGetCurrentGroup());
}

/* gnc_configure_register_border
 *    sets register borders to their guile values
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_register_borders(void)
{
  RegisterBorders reg_borders = 0;

  if (gnc_lookup_boolean_option("Register",
                                "Show Vertical Borders",
                                GNC_T))
    reg_borders |= STYLE_BORDER_LEFT | STYLE_BORDER_RIGHT;
  
  if (gnc_lookup_boolean_option("Register",
                                "Show Horizontal Borders",
                                GNC_T))
    reg_borders |= STYLE_BORDER_TOP | STYLE_BORDER_BOTTOM;
  
  gnucash_style_set_register_borders (reg_borders);
}

/* gnc_configure_reverse_balance_cb
 *    Callback called when options change - sets
 *    reverse balance info for the callback
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_reverse_balance_cb(void *not_used)
{
  gnc_configure_reverse_balance();
  gnc_group_ui_refresh(gncGetCurrentGroup());
  gnc_refresh_main_window();
}

static gncBoolean reverse_type[NUM_ACCOUNT_TYPES];

gncBoolean
gnc_reverse_balance_type(int type)
{
  if ((type < 0) || (type >= NUM_ACCOUNT_TYPES))
    return GNC_F;

  return reverse_type[type];
}

gncBoolean
gnc_reverse_balance(Account *account)
{
  int type;

  if (account == NULL)
    return GNC_F;

  type = xaccAccountGetType(account);
  if ((type < 0) || (type >= NUM_ACCOUNT_TYPES))
    return GNC_F;

  return reverse_type[type];
}

/* gnc_configure_reverse_balance
 *    sets reverse balance info for the callback
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_reverse_balance(void)
{
  gchar *choice;
  gint i;

  xaccSRSetReverseBalanceCallback(gnc_reverse_balance);

  for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
    reverse_type[i] = GNC_F;

  choice = gnc_lookup_multichoice_option("General",
                                         "Reversed-balance account types",
                                         "default");

  if (safe_strcmp(choice, "default") == 0)
  {
    reverse_type[INCOME]  = GNC_T;
    reverse_type[EXPENSE] = GNC_T;
  }
  else if (safe_strcmp(choice, "credit") == 0)
  {
    reverse_type[LIABILITY] = GNC_T;
    reverse_type[EQUITY]    = GNC_T;
    reverse_type[INCOME]    = GNC_T;
    reverse_type[CREDIT]    = GNC_T;
  }
  else if (safe_strcmp(choice, "none") == 0)
  {
  }
  else
  {
    PERR("gnc_configure_reverse_balance: bad value\n");

    reverse_type[INCOME]  = GNC_T;
    reverse_type[EXPENSE] = GNC_T;
  }

  if (choice != NULL)
    free(choice);
}

/* gnc_configure_debit_credit_labels_cb
 *    Callback called when options change - sets
 *    register debit/credit labels
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_debit_credit_labels_cb(void *not_used)
{
  gnc_configure_debit_credit_labels();
}

/* gnc_configure_debit_credit_labels
 *    sets usage of debit/credit labels
 *
 * Args: Nothing
 * Returns: Nothing
 */
static void
gnc_configure_debit_credit_labels(void)
{
  gncBoolean only_debit_credit_labels;

  only_debit_credit_labels =
    gnc_lookup_boolean_option("Register",
                              "Always use debit/credit labels",
                              GNC_F);

  xaccSplitRegisterSetLabelMode(only_debit_credit_labels);
}

/****************** END OF FILE **********************/
