/********************************************************************\
 * gnc-ui.h - High level UI functions for GnuCash                   *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1999, 2000 Rob Browning <rlb@cs.utexas.edu>        *
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


/** @addtogroup GUI
    @{ */

/** @defgroup Dialogs Dialogs */
/** @defgroup Druids Druids */

/** @} */


#ifndef GNC_UI_H
#define GNC_UI_H

#include <glib.h>

#include "Account.h"
#include "gnc-pricedb.h"
#include <gtk/gtk.h>


/** Help Files ******************************************************/
#define HF_HELP       "gnucash-help"
#define HF_HELP_HOME  "index.html"
#define HF_GUIDE      "gnucash-guide"
#define HF_GUIDE_HOME "index.html"

/** Links in the Help Files *****************************************/
#define HL_USAGE_BSNSS       "ch07.html#busnss-ar-setup1"
#define HL_USAGE_INVOICE     "ch07s04.html#busnss-ar-invoices1"
#define HL_USAGE_VOUCHER     "ch07s13.html#busnss-emply-newvchr"
#define HL_USAGE_BILL        "ch07s10.html#busnss-ap-bills1"
#define HL_USAGE_CUSTOMER    "ch07s03.html#busnss-ar-customers1"
#define HL_USAGE_VENDOR      "ch07s09.html#busnss-ap-vendors1"
#define HL_USAGE_EMPLOYEE    "ch07s13.html#busnss-emply"
#define HL_USAGE_JOB         "ch07s05.html#busnss-ar-jobs1"
#define HL_ACC               "ch05s04.html#acct-create"
#define HL_ACCEDIT           "ch05s05.html#acct-edit"
#define HL_COMMODITY         "ch08s07.html#tool-commodity"
#define HL_FIND_TRANSACTIONS "ch08.html#tool-find"
#define HL_GLOBPREFS         "ch10s02.html#set-prefs"
#define HL_PRINTCHECK        "ch06s15.html#print-check"
#define HL_RECNWIN           "ch05s08.html#acct-reconcile"
#define HL_SXEDITOR          "ch06s13.html#trans-sched"
#define HL_BOOK_OPTIONS      "ch10s03.html#book-options"
#define HL_STYLE_SHEET       "ch10s04.html#change-style"
#define HL_CLOSE_BOOK        "ch08s09.html#tool-close-book"
#define HL_USAGE_CUSTOMREP   "ch09.html#report-saving"
#define HL_IMPORT_BC         "ch18.html#busnss-imp-bills-invoices"
#define HL_IMPORT_CUST       "ch18s02.html"
/*********************************************************************/


/* GTK Windows - Common Response Codes */

#define GNC_RESPONSE_NEW    1
#define GNC_RESPONSE_DELETE 2
#define GNC_RESPONSE_EDIT   3

/* Dialog windows ***************************************************/

extern gboolean
gnc_verify_dialog (GtkWindow *parent,
                   gboolean yes_is_default,
                   const char *format, ...) G_GNUC_PRINTF (3, 4);

extern gint
gnc_ok_cancel_dialog (GtkWindow *parent,
                      gint default_result,
                      const char *format, ...) G_GNUC_PRINTF (3, 4);

extern void
gnc_warning_dialog (GtkWindow *parent,
                    const char *format, ...) G_GNUC_PRINTF (2, 3);

extern void
gnc_info_dialog (GtkWindow *parent,
                 const char *format, ...) G_GNUC_PRINTF (2, 3);

extern void
gnc_error_dialog (GtkWindow *parent,
                  const char *format, ...) G_GNUC_PRINTF (2, 3);


extern void
gnc_gnome_help (GtkWindow *parent, const char *dir_name, const char *anchor);

int      gnc_choose_radio_option_dialog (GtkWidget *parent,
        const char *title,
        const char *msg,
        const char *button_name,
        int default_value,
        GList *radio_list);

void     gnc_tax_info_dialog (GtkWidget *parent, Account *account);
void     gnc_stock_split_dialog (GtkWidget *parent, Account * initial);

typedef enum
{
    GNC_PRICE_EDIT,
    GNC_PRICE_NEW,
} GNCPriceEditType;

void gnc_price_edit_dialog (GtkWidget *parent, QofSession *session,
                            GNCPrice *price, GNCPriceEditType type);
GNCPrice* gnc_price_edit_by_guid (GtkWidget * parent, const GncGUID * guid);
void     gnc_prices_dialog (GtkWidget *parent);
void     gnc_commodities_dialog (GtkWidget *parent);

/* Open a dialog asking for username and password. The heading and
 * either 'initial_*' arguments may be NULL. If the dialog returns
 * TRUE, the user pressed OK and the entered strings are stored in the
 * output variables. They should be g_freed when no longer needed. If
 * the dialog returns FALSE, the user pressed CANCEL and NULL was
 * stored in username and password. */
gboolean gnc_get_username_password (GtkWidget *parent,
                                    const char *heading,
                                    const char *initial_username,
                                    const char *initial_password,
                                    char **username,
                                    char **password);

/* Managing the GUI Windows *****************************************/

/** Get a pointer to the widget's immediate top level GtkWindow. This can be a dialog
 *  window or a GncMainWindow. If the widget is not a child of
 *  a GtkWindow (yet), NULL is returned.
 *
 *  @param widget the widget to find a GtkWindow for.
 *  @return A pointer to a GtkWindow object or NULL if no toplevel was found. */
GtkWindow *gnc_ui_get_gtk_window (GtkWidget *widget);

/** Get a pointer to the final GncMainWindow widget is rooted
 * in. If widget is a child of a GncMainWindow return that window.
 * If it's a child of a dialog window recursively query the
 * dialog's transient parent until the first parent that's a GncMainWindow
 * and return that. If widget is NULL or not part of any GtkWindow,
 * get a pointer to the first active top level window. If there is
 * none, return the first mapped window. If there's no mapped window
 * return NULL.
 *
 * An example of why searching for a GncMainWindow makes sense: suppose
 * a user has opened a search dialog for vendors and in that dialog has
 * clicked "View vendor invoices". This opens another search window in
 * which the user can click "View/Edit bill". Clicking that button should
 * open a new tab in the GncMainWindow from which the first search dialog
 * was opened.
 *
 * @param widget the widget to find a GncMainWindow for.
 * @return A pointer to a GtkWindow object. */

GtkWindow *gnc_ui_get_main_window (GtkWidget *widget);

/* Changing the GUI Cursor ******************************************/

void gnc_set_busy_cursor(GtkWidget *w, gboolean update_now);
void gnc_unset_busy_cursor(GtkWidget *w);


#endif
