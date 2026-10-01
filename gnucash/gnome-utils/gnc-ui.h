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
/** @defgroup Assistants Assistants */

/** @} */


#ifndef GNC_UI_H
#define GNC_UI_H

#include <glib.h>

#include "Account.h"
#include "gnc-pricedb.h"
#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif


/** Documentation references ****************************************/
#ifdef G_OS_WIN32
#    define DF_GUIDE         "gnucash-guide.chm"
#    define DF_MANUAL        "gnucash-manual.chm"
#elif defined MAC_INTEGRATION
#    define DF_GUIDE         "GnuCash Guide"
#    define DF_MANUAL        "GnuCash Manual"
#else
#    define DF_GUIDE         "gnucash-guide"
#    define DF_MANUAL        "gnucash-manual"
#endif

/** Links in the Manual *********************************************/
#define DL_USAGE_BSNSS       "busnss-ar-setup1"
#define DL_USAGE_INVOICE     "busnss-ar-invoices1"
#define DL_USAGE_VOUCHER     "busnss-emply-newvchr"
#define DL_USAGE_BILL        "busnss-ap-bills1"
#define DL_USAGE_CUSTOMER    "busnss-ar-customers1"
#define DL_USAGE_VENDOR      "busnss-ap-vendors1"
#define DL_USAGE_EMPLOYEE    "busnss-emply"
#define DL_USAGE_JOB         "busnss-ar-jobs1"
#define DL_ACC               "acct-create"
#define DL_ACCEDIT           "acct-edit"
#define DL_COMMODITY         "tool-commodity"
#define DL_FIND_TRANSACTIONS "tool-find"
#define DL_FIN_CALC          "tool-calc"
#define DL_PRICE_DB          "tool-price"
#define DL_PRICE_EDIT        "tool-price-manual"
#define DL_GLOBPREFS         "set-prefs"
#define DL_PRINTCHECK        "print-check"
#define DL_RECNWIN           "acct-reconcile"
#define DL_SXEDITOR          "trans-sched"
#define DL_SX_SLR            "trans-sched-slr"
#define DL_BOOK_OPTIONS      "book-options"
#define DL_STYLE_SHEET       "change-style"
#define DL_CLOSE_BOOK        "tool-close-book"
#define DL_USAGE_CUSTOMREP   "report-saving"
#define DL_IMPORT_BC         "busnss-imp-bills-invoices"
#define DL_IMPORT_CUST       "busnss-imp-customer-vendor"

/* GTK Windows - Common Response Codes */

#define GNC_RESPONSE_NEW    1
#define GNC_RESPONSE_DELETE 2
#define GNC_RESPONSE_EDIT   3

/* Dialog windows ***************************************************/

/** Completion of a non-blocking decision dialog, exactly once.
 * parent is borrowed for the duration of the callback, or NULL if destroyed.
 * Closing or destroying the dialog, destroying its parent, or an unknown
 * response produces the negative response
 * (NO for verify, CANCEL otherwise). The caller must keep user_data valid
 * until completion. Existing synchronous callers retain their old contract.
 */
typedef void (*GncGuiQueryResponseCallback) (GtkWindow *parent,
                                            gint response,
                                            gpointer user_data);

void gnc_ok_cancel_dialog_async (GtkWindow *parent, gint default_result,
                                 GncGuiQueryResponseCallback completed,
                                 gpointer user_data,
                                 const gchar *format, ...) G_GNUC_PRINTF (5, 6);
void gnc_verify_dialog_async (GtkWindow *parent, gboolean yes_is_default,
                              GncGuiQueryResponseCallback completed,
                              gpointer user_data,
                              const gchar *format, ...) G_GNUC_PRINTF (5, 6);
void gnc_action_dialog_async (GtkWindow *parent, const gchar *action,
                              gboolean action_default,
                              GncGuiQueryResponseCallback completed,
                              gpointer user_data,
                              const gchar *format, ...) G_GNUC_PRINTF (6, 7);

extern void
gnc_warning_dialog (GtkWindow *parent,
                    const char *format, ...) G_GNUC_PRINTF (2, 3);

extern void
gnc_info_dialog (GtkWindow *parent,
                 const char *format, ...) G_GNUC_PRINTF (2, 3);

extern void
gnc_error_dialog (GtkWindow *parent,
                  const char *format, ...) G_GNUC_PRINTF (2, 3);

/** Show a modal error notice without waiting for dismissal.
 * Use only when the caller has no work that depends on closing the notice.
 * Formatting arguments are copied before returning; destruction of the
 * parent closes the notice. Callers that depend on dismissal must use a completion callback.
 */
extern void
gnc_error_dialog_async (GtkWindow *parent,
                        const gchar *format, ...) G_GNUC_PRINTF (2, 3);

/** Show ordered error strings in one non-waiting modal notice.
 * The list and its strings are borrowed and may be freed immediately after
 * returning. An empty list produces no notice.
 */
void gnc_error_dialog_async_list (GtkWindow *parent, const GList *errors);

/** Same non-waiting lifetime contract as gnc_error_dialog_async. */
void gnc_warning_dialog_async (GtkWindow *parent,
                               const gchar *format, ...) G_GNUC_PRINTF (2, 3);
void gnc_info_dialog_async (GtkWindow *parent,
                            const gchar *format, ...) G_GNUC_PRINTF (2, 3);
/** Show a modal info notice and continue exactly once after dismissal or
 * destruction. Parent is borrowed during the callback, or NULL if destroyed.
 */
void gnc_info_dialog_async_response (GtkWindow *parent,
                                    GncGuiQueryResponseCallback completed,
                                     gpointer user_data,
                                     const gchar *format, ...) G_GNUC_PRINTF (4, 5);

/** Show a modal notice of the requested type. An optional completion runs
 * exactly once after dismissal or destruction; parent is then borrowed, or
 * NULL if destroyed. With no completion this is a terminal notice. */
void gnc_message_dialog_async_response (GtkWindow *parent, GtkMessageType type,
                                       GncGuiQueryResponseCallback completed,
                                       gpointer user_data,
                                       const gchar *format, ...) G_GNUC_PRINTF (5, 6);

/** Input is owned by the callback; NULL means cancellation or owner loss. */
typedef void (*GncInputDialogCallback) (GtkWindow *parent, gchar *input,
                                       gpointer user_data);
void gnc_input_dialog_async (GtkWidget *parent, const gchar *title,
                             const gchar *msg, const gchar *default_input,
                             GncInputDialogCallback completed, gpointer user_data);
void gnc_input_dialog_with_entry_async (GtkWidget *parent, const gchar *title,
                                        const gchar *msg, const gchar *default_input,
                                        GncInputDialogCallback completed, gpointer user_data);

extern void
gnc_info2_dialog (GtkWidget *parent, const gchar *title, const gchar *msg);

/** Show a modal text-view notice without waiting for dismissal.
 * The title and message are copied before returning. The completion callback
 * runs exactly once after dismissal or destruction; parent is borrowed for
 * the callback duration, or NULL if its window was destroyed.
 */
void gnc_info2_dialog_async (GtkWidget *parent, const gchar *title,
                             const gchar *msg,
                             GncGuiQueryResponseCallback completed,
                             gpointer user_data);

extern void
gnc_gnome_help (GtkWindow *parent, const char *file_name, const char *target_link);

void     gnc_choose_radio_option_dialog_async (GtkWidget *parent,
        const char *title,
        const char *msg,
        const char *button_name,
        int default_value,
        GList *radio_list,
        GncGuiQueryResponseCallback completed, gpointer user_data);

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

/** Completion strings are owned by the callback. Cancellation and owner
 * destruction pass accepted FALSE and NULL strings. */
typedef void (*GncUsernamePasswordCallback) (gboolean accepted,
                                             gchar *username,
                                             gchar *password,
                                             gpointer user_data);
void gnc_get_username_password_async (GtkWindow *parent,
                                      const gchar *heading,
                                      const gchar *initial_username,
                                      const gchar *initial_password,
                                      GncUsernamePasswordCallback completed,
                                      gpointer user_data);

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

#ifdef __cplusplus
}
#endif


#endif
