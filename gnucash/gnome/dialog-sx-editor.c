/********************************************************************\
 * dialog-sx-editor.c : dialog for scheduled transaction editing    *
 * Copyright (C) 2001,2002,2006 Joshua Sled <jsled@asynchronous.org>*
 * Copyright (C) 2011 Robert Fewell                                 *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public License as published by the Free Software     *
 * Foundation.                                                      *
 *                                                                  *
 * As a special exception, permission is granted to link the binary *
 * module resultant from this code with the OpenSSL project's       *
 * "OpenSSL" library (or modified versions of it that use the same  *
 * license as the "OpenSSL" library), and distribute the linked     *
 * executable.  You must obey the GNU General Public License in all *
 * respects for all of the code used other than "OpenSSL". If you   *
 * modify this file, you may extend this exception to your version  *
 * of the file, but you are not obligated to do so. If you do not   *
 * wish to do so, delete this exception statement from your version *
 * of this file.                                                    *
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
\********************************************************************/

#include <config.h>
#include <stdbool.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <locale.h>

#include "qof.h"
#include "Account.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "dialog-preferences.h"
#include "dialog-sx-editor.h"
#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-component-manager.h"
#include "gnc-date.h"
#include "gnc-date-edit.h"
#include "gnc-dense-cal.h"
#include "gnc-dense-cal-store.h"
#include "gnc-embedded-window.h"
#include "gnc-engine.h"
#include "gnc-frequency.h"
#include "gnc-gui-query.h"
#include "gnc-hooks.h"
#include "gnc-ledger-display.h"
#include "gnc-plugin-page.h"
#include "gnc-plugin-page-register.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnucash-sheet.h"
#include "gnc-session.h"
#include <gnc-string-utils.h>

#include "gnc-split-reg.h"

#include "gnc-sx-instance-model.h"
#include "dialog-sx-since-last-run.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.gui.sx.editor"

static QofLogModule log_module = GNC_MOD_GUI_SX;

static gint _sx_engine_event_handler_id = -1;

#define END_NEVER_OPTION 0
#define END_DATE_OPTION  1
#define NUM_OCCUR_OPTION 2

#define NUM_LEDGER_LINES_DEFAULT 6

#define EX_CAL_NUM_MONTHS 12
#define EX_CAL_MO_PER_COL 3

#define GNC_D_WIDTH 25
#define GNC_D_BUF_WIDTH 26

/** Datatypes ***********************************************************/

typedef enum _EndTypeEnum
{
    END_NEVER,
    END_DATE,
    END_OCCUR,
} EndType;

struct _GncSxEditorDialog
{
    GtkWidget    *dialog;
    GtkBuilder   *builder;
    GtkNotebook  *notebook;
    SchedXaction *sx;
    /* If this is a new scheduled transaction or not. */
    int newsxP;

    /* The various widgets in the dialog */
    GNCLedgerDisplay *ledger;

    GncFrequency     *gncfreq;
    GncDenseCalStore *dense_cal_model;
    GncDenseCal      *example_cal;

    GtkEntry        *nameEntry;
    GtkLabel        *lastOccurLabel;

    GtkCheckButton *enabledOpt;
    GtkCheckButton *autocreateOpt;
    GtkCheckButton *notifyOpt;
    GtkCheckButton *advanceOpt;
    GtkSpinButton   *advanceSpin;
    GtkCheckButton *remindOpt;
    GtkSpinButton   *remindSpin;

    GtkCheckButton *optEndDate;
    GtkCheckButton *optEndNone;
    GtkCheckButton *optEndCount;
    EndType          end_type;
    GtkEntry        *endCountSpin;
    GtkEntry        *endRemainSpin;
    GNCDateEdit     *endDateEntry;

    char            *sxGUIDstr;

    GncEmbeddedWindow *embed_window;
    GncPluginPage     *plugin_page;

    /* The editor stays alive until every non-blocking decision finishes. */
    gboolean decision_pending;
    gboolean save_unbalanceable;
    gboolean save_duplicate_name;
    gboolean save_never_runs;
};

/** Prototypes **********************************************************/

static void schedXact_editor_create_freq_sel (GncSxEditorDialog *sxed);
static void schedXact_editor_create_ledger (GncSxEditorDialog *sxed);
static void schedXact_editor_populate (GncSxEditorDialog *);
static void endgroup_rb_toggled_cb (GtkButton *b, gpointer d);
static void set_endgroup_toggle_states (GncSxEditorDialog *sxed, EndType t);
static void advance_toggled_cb (GtkButton *b, GncSxEditorDialog *sxed);
static void remind_toggled_cb (GtkButton *b, GncSxEditorDialog *sxed);
typedef void (*SxedCompletion) (GncSxEditorDialog *sxed);

static gboolean gnc_sxed_check_consistent (GncSxEditorDialog *sxed);
static void gnc_sxed_check_consistent_async (GncSxEditorDialog *sxed);
static gboolean gnc_sxed_check_changed (GncSxEditorDialog *sxed);
static void gnc_sxed_save_sx (GncSxEditorDialog *sxed);
static void gnc_sxed_freq_changed (GncFrequency *gf, gpointer ud);
static void sxed_excal_update_adapt_cb (GtkWidget *o, gpointer ud);
static void gnc_sxed_update_cal (GncSxEditorDialog *sxed);
void on_sx_check_toggled_cb (GtkWidget *togglebutton, gpointer user_data);
static void gnc_sxed_reg_check_close_async (GncSxEditorDialog *sxed,
                                                  SxedCompletion completed);
static void sxed_destroy_window (GncSxEditorDialog *sxed);
static void sxed_request_cancel (GncSxEditorDialog *sxed);
static gboolean sxed_close_request (GtkWindow *window, gpointer user_data);
static gboolean editor_component_sx_equality (gpointer find_data,
                                              gpointer user_data);

static GActionEntry gnc_sxed_menu_entries [] =
{
    { "EditAction", NULL, NULL, NULL, NULL },
    { "TransactionAction", NULL, NULL, NULL, NULL },
    { "ViewAction", NULL, NULL, NULL, NULL },
    { "ActionsAction", NULL, NULL, NULL, NULL },
};
static guint gnc_sxed_menu_n_entries = G_N_ELEMENTS(gnc_sxed_menu_entries);

/** Implementations *****************************************************/

static void
sxed_set_decision_pending (GncSxEditorDialog *sxed, gboolean pending)
{
    sxed->decision_pending = pending;
    if (sxed->dialog)
        gtk_widget_set_sensitive (sxed->dialog, !pending);
}


static void
sxed_destroy_window (GncSxEditorDialog *sxed)
{
    if (!sxed->dialog)
        return;

    gnc_save_window_size (GNC_PREFS_GROUP_SXED, GTK_WINDOW (sxed->dialog));
    gtk_window_destroy (GTK_WINDOW (sxed->dialog));
}


typedef struct
{
    GWeakRef dialog;
    QofBook *book;
    SxedCompletion completed;
} SxedLedgerCloseRequest;

static GncSxEditorDialog *
sxed_ledger_close_request_get_editor (SxedLedgerCloseRequest *request,
                                      GtkWidget **dialog_out)
{
    GtkWidget *dialog = GTK_WIDGET (g_weak_ref_get (&request->dialog));
    GncSxEditorDialog *sxed = dialog ?
        g_object_get_data (G_OBJECT (dialog), "gnc-sxed-dialog-state") : NULL;

    if (dialog_out)
        *dialog_out = dialog;
    else
        g_clear_object (&dialog);
    return sxed;
}

static void
sxed_ledger_close_request_free (SxedLedgerCloseRequest *request)
{
    g_weak_ref_clear (&request->dialog);
    g_free (request);
}

static void
sxed_ledger_save_finished (SplitRegister *reg, gboolean saved, gpointer user_data)
{
    SxedLedgerCloseRequest *request = user_data;
    GtkWidget *dialog = NULL;
    GncSxEditorDialog *sxed = sxed_ledger_close_request_get_editor (request, &dialog);

    if (sxed && dialog && request->book == gnc_get_current_book () && sxed->ledger &&
        reg == gnc_ledger_display_get_split_register (sxed->ledger) && saved)
    {
        gnc_split_register_redraw (reg);
        request->completed (sxed);
    }
    else if (sxed && dialog)
        sxed_set_decision_pending (sxed, FALSE);
    g_clear_object (&dialog);
    sxed_ledger_close_request_free (request);
}

static void
sxed_ledger_close_finished (G_GNUC_UNUSED GtkWindow *parent, gint choice,
                            gpointer user_data)
{
    SxedLedgerCloseRequest *request = user_data;
    GtkWidget *dialog = NULL;
    GncSxEditorDialog *sxed = sxed_ledger_close_request_get_editor (request, &dialog);
    SplitRegister *reg = sxed && sxed->ledger ?
        gnc_ledger_display_get_split_register (sxed->ledger) : NULL;

    if (!sxed || !dialog || !reg || request->book != gnc_get_current_book ())
    {
        if (sxed && dialog)
            sxed_set_decision_pending (sxed, FALSE);
        goto done;
    }

    if (choice == 0)
    {
        gnc_split_register_save_async (reg, TRUE, sxed_ledger_save_finished, request);
        g_clear_object (&dialog);
        return;
    }
    if (choice == 1)
    {
        gnc_split_register_cancel_cursor_trans_changes (reg);
        request->completed (sxed);
    }
    else
        sxed_set_decision_pending (sxed, FALSE);

done:
    g_clear_object (&dialog);
    sxed_ledger_close_request_free (request);
}
/*
 * Preserve the three-way register decision explicitly. The asynchronous
 * choice keeps the ledger and its cursor owned by the editor until the
 * selected continuation has completed.
 */
static void
gnc_sxed_reg_check_close_async (GncSxEditorDialog *sxed, SxedCompletion completed)
{
    SplitRegister *reg = gnc_ledger_display_get_split_register (sxed->ledger);
    GList *choices = NULL;
    SxedLedgerCloseRequest *request;

    if (!gnc_split_register_changed (reg))
    {
        completed (sxed);
        return;
    }

    request = g_new0 (SxedLedgerCloseRequest, 1);
    request->book = gnc_get_current_book ();
    request->completed = completed;
    g_weak_ref_init (&request->dialog, G_OBJECT (sxed->dialog));

    choices = g_list_append (choices, _("Record"));
    choices = g_list_append (choices, _("Don't Record"));
    choices = g_list_append (choices, _("Cancel"));
    gnc_choose_option_dialog_async (
        GTK_WINDOW (sxed->dialog), _("Save changes"),
        _("The current template transaction has been changed. Would you like to "
          "record the changes?"),
        choices, 0, sxed_ledger_close_finished, request);
    g_list_free (choices);
}


static void
sxed_close_handler (gpointer user_data)
{
    GncSxEditorDialog *sxed = user_data;

    if (sxed->decision_pending)
        return;

    sxed_set_decision_pending (sxed, TRUE);
    gnc_sxed_reg_check_close_async (sxed, sxed_destroy_window);
}


static void
sxed_cancel_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncSxEditorDialog *sxed = user_data;

    (void) parent;

    if (response != GTK_RESPONSE_YES)
    {
        sxed_set_decision_pending (sxed, FALSE);
        return;
    }

    gnc_split_register_cancel_cursor_trans_changes (
        gnc_ledger_display_get_split_register (sxed->ledger));
    sxed_destroy_window (sxed);
}


static void
sxed_request_cancel (GncSxEditorDialog *sxed)
{
    const char *message =
        _("This Scheduled Transaction has changed; are you sure you want to cancel?");

    if (sxed->decision_pending)
        return;

    sxed_set_decision_pending (sxed, TRUE);
    if (!gnc_sxed_check_changed (sxed))
    {
        gnc_split_register_cancel_cursor_trans_changes (
            gnc_ledger_display_get_split_register (sxed->ledger));
        sxed_destroy_window (sxed);
        return;
    }

    gnc_verify_dialog_async (GTK_WINDOW (sxed->dialog), FALSE, sxed_cancel_finished, sxed,
                             "%s", message);
}


/**********************************
 * Dialog Action Button functions *
 *********************************/
static void
editor_cancel_button_clicked_cb (GtkButton *button, GncSxEditorDialog *sxed)
{
    (void) button;
    sxed_request_cancel (sxed);
}


static void
editor_help_button_clicked_cb (GtkButton *button, GncSxEditorDialog *sxed)
{
    (void) button;
    gnc_gnome_help (GTK_WINDOW (sxed->dialog), DF_MANUAL, DL_SXEDITOR);
}


static void
editor_ok_button_clicked_cb (GtkButton *button, GncSxEditorDialog *sxed)
{
    (void) button;

    if (sxed->decision_pending)
        return;

    sxed_set_decision_pending (sxed, TRUE);
    gnc_sxed_reg_check_close_async (sxed, gnc_sxed_check_consistent_async);
}

static gboolean
gnc_sxed_check_name_changed (GncSxEditorDialog *sxed)
{
    const char *name = gnc_entry_get_text (sxed->nameEntry);

    if (!name || !name[0])
        return TRUE;

    if (xaccSchedXactionGetName (sxed->sx) == NULL ||
        strcmp (xaccSchedXactionGetName (sxed->sx), name) != 0)
        return TRUE;

    return FALSE;
}

static gboolean
gnc_sxed_check_end_date_changed (GncSxEditorDialog *sxed)
{
    GDate sxEndDate, dlgEndDate;

    if (!xaccSchedXactionHasEndDate (sxed->sx))
        return TRUE;

    sxEndDate = *xaccSchedXactionGetEndDate (sxed->sx);
    gnc_gdate_set_time64 (&dlgEndDate,
                          gnc_date_edit_get_date (sxed-> endDateEntry));

    if (g_date_compare (&sxEndDate, &dlgEndDate) != 0)
        return TRUE;

    return FALSE;
}

static gboolean
gnc_sxed_check_num_occurs_changed (GncSxEditorDialog *sxed)
{
    gint sxNumOccur, sxNumRem, dlgNumOccur, dlgNumRem;

    if (!xaccSchedXactionGetNumOccur (sxed->sx))
        return TRUE;
    dlgNumOccur  =
        gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->endCountSpin));
    dlgNumRem =
        gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->endRemainSpin));
    sxNumOccur = xaccSchedXactionGetNumOccur (sxed->sx);
    sxNumRem = xaccSchedXactionGetRemOccur (sxed->sx);

    if (dlgNumOccur != sxNumOccur || dlgNumRem != sxNumRem)
        return TRUE;

    return FALSE;
}

static gboolean
gnc_sxed_check_creation_changed (GncSxEditorDialog *sxed)
{
    gboolean sxAutoCreate, sxNotify;
    gint dlgAdvance = 0;
    gint dlgRemind = 0;

    gboolean dlgEnabled =
        gtk_check_button_get_active (GTK_CHECK_BUTTON (sxed->enabledOpt));
    gboolean dlgAutoCreate =
        gtk_check_button_get_active (GTK_CHECK_BUTTON (sxed->autocreateOpt));
    gboolean dlgNotify =
        gtk_check_button_get_active (GTK_CHECK_BUTTON (sxed->notifyOpt));

    if (dlgEnabled != xaccSchedXactionGetEnabled (sxed->sx))
        return TRUE;

    xaccSchedXactionGetAutoCreate (sxed->sx, &sxAutoCreate, &sxNotify);
    if (dlgAutoCreate != sxAutoCreate || dlgNotify != sxNotify)
        return TRUE;

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON (sxed->advanceOpt)))
        dlgAdvance = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->advanceSpin));
    if (dlgAdvance != xaccSchedXactionGetAdvanceCreation (sxed->sx))
        return TRUE;

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON (sxed->remindOpt)))
        dlgRemind = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->remindSpin));
    if (dlgRemind != xaccSchedXactionGetAdvanceReminder (sxed->sx))
        return TRUE;

    return FALSE;
}

static gboolean
gnc_sxed_check_dates_changed (GncSxEditorDialog *sxed)
{
    GList *dialog_schedule = NULL;
    GDate dialog_start_date, sx_start_date;
    gchar *dialog_schedule_str, *sx_schedule_str;
    gboolean schedules_are_the_same, start_dates_are_the_same;

    g_date_clear (&dialog_start_date, 1);
    gnc_frequency_save_to_recurrence (sxed->gncfreq, &dialog_schedule,
                                      &dialog_start_date);
    dialog_schedule_str = recurrenceListToString (dialog_schedule);
    recurrenceListFree (&dialog_schedule);

    sx_start_date = *xaccSchedXactionGetStartDate (sxed->sx);
    sx_schedule_str = recurrenceListToString (gnc_sx_get_schedule (sxed->sx));

    DEBUG ("dialog schedule [%s], sx schedule [%s]",
             dialog_schedule_str, sx_schedule_str);

    schedules_are_the_same = (strcmp (dialog_schedule_str,
                                     sx_schedule_str) == 0);
    g_free (dialog_schedule_str);
    g_free (sx_schedule_str);

    start_dates_are_the_same = (g_date_compare (&dialog_start_date,
                                               &sx_start_date) == 0);

    if (schedules_are_the_same && start_dates_are_the_same)
        return FALSE;
    return TRUE;
}

/*************************************************************************
 * Checks to see if the SX has been modified from it's previously-saved
 * state.
 * @return TRUE if this is a 'new' SX, or if the SX has changed from it's
 *   previous configuration.
 ************************************************************************/
static gboolean
gnc_sxed_check_changed (GncSxEditorDialog *sxed)
{
    SplitRegister *sr = NULL;
    if (sxed->newsxP)
        return TRUE;

    /* name */
    if (gnc_sxed_check_name_changed (sxed))
        return TRUE;
    /* end options */
    /* dialog says... no end */
    if (gtk_check_button_get_active (sxed->optEndNone) &&
        (xaccSchedXactionHasEndDate (sxed->sx) ||
         xaccSchedXactionHasOccurDef (sxed->sx)))
        return TRUE;

    /* dialog says... end date */
    if (gtk_check_button_get_active (sxed->optEndDate) &&
        gnc_sxed_check_end_date_changed (sxed))
        return TRUE;

    /* dialog says... num occur */
    if (gtk_check_button_get_active (sxed->optEndCount) &&
        gnc_sxed_check_num_occurs_changed (sxed))
        return TRUE;
    /* SX options [autocreate, notify, reminder, advance] */
    if (gnc_sxed_check_creation_changed (sxed))
        return TRUE;
    /* Dates and Schedules */
    if (gnc_sxed_check_dates_changed (sxed))
        return TRUE;

    /* template transactions */
    sr = gnc_ledger_display_get_split_register (sxed->ledger);
    if (gnc_split_register_changed (sr))
        return TRUE;

    return FALSE;
}


/*****************************************************************************
 * Holds the credit- and debit-sum for a given Transaction, as used in
 * gnc_sxed_check_consistent.
 ****************************************************************************/
typedef struct _txnCreditDebitSums
{
    gnc_numeric    creditSum;
    gnc_numeric    debitSum;
    gnc_commodity *base_cmdty;
    GtkWindow     *window;
    gboolean       multi_commodity;
} txnCreditDebitSums;

static txnCreditDebitSums *
tcds_new (void)
{
    txnCreditDebitSums *tcds = g_new0 (txnCreditDebitSums, 1);
    tcds->creditSum = tcds->debitSum = gnc_numeric_zero ();
    tcds->base_cmdty = NULL;
    tcds->window = NULL;
    tcds->multi_commodity = FALSE;
    return tcds;
}

static void
set_sums_to_zero (gpointer key,
                  gpointer val,
                  gpointer ud)
{
    txnCreditDebitSums *tcds = (txnCreditDebitSums*)val;
    tcds->creditSum = gnc_numeric_zero ();
    tcds->debitSum  = gnc_numeric_zero ();
    tcds->base_cmdty = NULL;
    tcds->multi_commodity = FALSE;
}

inline static gnc_numeric
tcds_difference (txnCreditDebitSums *tcds)
{
    return gnc_numeric_sub_fixed (tcds->debitSum, tcds->creditSum);
}

static void
check_credit_debit_balance (gpointer key, gpointer val, gpointer ud)
{
    txnCreditDebitSums *tcds = (txnCreditDebitSums*)val;
    Transaction *txn = GNC_TRANSACTION(key);
    gboolean *unbalanced = (gboolean*)ud;
    gnc_numeric diff = tcds_difference (tcds);
    const char *result = gnc_numeric_zero_p (diff) ? "true" : "false";
    *unbalanced |= !(gnc_numeric_zero_p (diff));

    DEBUG ("%p | %s [%s - %s = %s]", key, result,
           gnc_num_dbg_to_string (tcds->debitSum),
           gnc_num_dbg_to_string (tcds->creditSum),
           gnc_num_dbg_to_string (diff));

    if (!gnc_numeric_zero_p (diff) && !tcds->multi_commodity)
    {
        char string[32];
        gchar *msg_text;
        const gchar *desc = xaccTransGetDescription (txn);
        GNCPrintAmountInfo print_info = gnc_commodity_print_info (tcds->base_cmdty, TRUE);
        gnc_numeric abs_diff = gnc_numeric_abs (diff);
        xaccSPrintAmount (string, abs_diff, print_info);
        msg_text = g_strdup_printf (_("Transaction with description '%s' can not be balanced.\n"
                                      "The difference is %s"), desc, string);

        gnc_warning_dialog (tcds->window, "%s", msg_text);
        g_free (msg_text);
    }
}

static gboolean
gnc_sxed_check_names (GncSxEditorDialog *sxed)
{
    const gchar *name = gnc_entry_get_text (sxed->nameEntry);
    if (!name || !name[0])
    {
        const char *sx_has_no_name_msg =
            _("Please name the Scheduled Transaction.");
        gnc_error_dialog (GTK_WINDOW (sxed->dialog), "%s", sx_has_no_name_msg);
        return FALSE;

    }

    bool nameExists = FALSE;
    gchar *nameKey = g_utf8_collate_key (name, -1);
    bool nameHasChanged =
        (xaccSchedXactionGetName (sxed->sx) == NULL)
        || (strcmp (xaccSchedXactionGetName (sxed->sx), name) != 0);
    for (GList *sxList = gnc_book_get_schedxactions (gnc_get_current_book ())->sx_list;
         nameHasChanged && !nameExists && sxList;
         sxList = sxList->next)
    {
        const char *existingName = xaccSchedXactionGetName ((SchedXaction*)sxList->data);
        char *existingNameKey = g_utf8_collate_key (existingName, -1);
        nameExists |=  (strcmp (nameKey, existingNameKey) == 0);
        g_free (existingNameKey);
    }
    g_free (nameKey);
    if (nameHasChanged && nameExists)
        sxed->save_duplicate_name = TRUE;
    return TRUE;
}

static gboolean
gnc_sxed_check_endpoint (GncSxEditorDialog *sxed)
{
    GDate startDate, endDate, nextDate;
    GList *schedule = NULL;

    if (!gtk_check_button_get_active (sxed->optEndDate)
         && !gtk_check_button_get_active (sxed->optEndCount)
         && !gtk_check_button_get_active (sxed->optEndNone))
    {
        const char *sx_end_spec_msg =
            _("Please provide a valid end selection.");
        gnc_error_dialog (GTK_WINDOW (sxed->dialog), "%s", sx_end_spec_msg);
        return FALSE;
    }

    if (gtk_check_button_get_active (sxed->optEndCount))
    {
        gint occur  =
            gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->endCountSpin));
        gint rem =
            gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->endRemainSpin));

        if (occur == 0)
        {
            const char *sx_occur_count_zero_msg =
                _("There must be some number of occurrences.");
            gnc_error_dialog (GTK_WINDOW (sxed->dialog), "%s", sx_occur_count_zero_msg);
            return FALSE;
        }

        if (rem > occur)
        {
            const char *sx_occur_counts_wrong_msg =
                _("The number of remaining occurrences (%d) is greater than "
                  "the number of total occurrences (%d).");
            gnc_error_dialog (GTK_WINDOW (sxed->dialog), sx_occur_counts_wrong_msg,
                              rem, occur);
            return FALSE;
        }
        return TRUE;
    }

    g_date_clear (&endDate, 1);
    if (gtk_check_button_get_active (sxed->optEndDate))
    {
        gnc_gdate_set_time64 (&endDate,
                              gnc_date_edit_get_date (sxed-> endDateEntry));
    }

    g_date_clear (&nextDate, 1);
    gnc_frequency_save_to_recurrence (sxed->gncfreq, &schedule, &startDate);
    if (gnc_list_length_cmp (schedule, 0))
    {
        g_date_subtract_days (&startDate, 1);
        recurrenceListNextInstance (schedule, &startDate, &nextDate);
    }
    recurrenceListFree (&schedule);

    if (!g_date_valid (&nextDate) ||
        (g_date_valid (&endDate) && (g_date_compare (&nextDate, &endDate) > 0)))
        sxed->save_never_runs = TRUE;
    return TRUE;
}

static gboolean
gnc_sxed_check_autocreate (GncSxEditorDialog *sxed, int ttVarCount,
                           int splitCount, gboolean multi_commodity)
{
    gboolean autocreateState;

    autocreateState =
        gtk_check_button_get_active (
            GTK_CHECK_BUTTON (sxed->autocreateOpt));

    if (((ttVarCount > 0) || multi_commodity) && autocreateState)
    {
        gnc_warning_dialog (GTK_WINDOW (sxed->dialog), "%s",
                           _("Scheduled Transactions with variables "
                             "or involving more than one commodity "
                             "cannot be automatically created."));
        return FALSE;
    }

    /* Fix for part of Bug#121740 -- auto-create transactions are
     * only valid if there's actually a transaction to create. */
    if (autocreateState && splitCount == 0)
    {
        gnc_warning_dialog (GTK_WINDOW (sxed->dialog), "%s",
                           _("Scheduled Transactions without a template "
                             "transaction cannot be automatically created."));
        return FALSE;
    }
    return TRUE;
}

static gboolean
gnc_sxed_split_check_account (GncSxEditorDialog *sxed, Split *s, txnCreditDebitSums *tcds)
{
    GncGUID *acct_guid = NULL;
    qof_instance_get (QOF_INSTANCE (s), "sx-account", &acct_guid, NULL);
    Account *acct = xaccAccountLookup (acct_guid, gnc_get_current_book ());
    guid_free (acct_guid);
    // If the split is being destroyed always return TRUE.
    if (acct == NULL && !qof_instance_get_destroying (s))
        return FALSE;

    gnc_commodity *split_cmdty = xaccAccountGetCommodity (acct);

    if (!tcds->base_cmdty)
        tcds->base_cmdty = split_cmdty;

    tcds->multi_commodity |= !gnc_commodity_equal (split_cmdty, tcds->base_cmdty);

    return TRUE;
}

static gboolean
gnc_sxed_split_calculate_formula (GncSxEditorDialog *sxed, Split *s,
                                  GHashTable *vars, const char *key,
                                  txnCreditDebitSums *tcds)
{
    gnc_numeric tmp = gnc_numeric_zero ();
    char *str = NULL;
    qof_instance_get (QOF_INSTANCE (s),
                      key, &str,
                      NULL);
    if (!str || !str[0])
    {
        if (str)
            g_free (str);
        return TRUE; /* No formula no foul */
    }
    if (gnc_sx_parse_vars_from_formula (str, vars, &tmp) < 0)
    {
        gchar *err = g_strdup_printf (_("Couldn't parse %s for split \"%s\"."),
                                      key, xaccSplitGetMemo (s));
        gnc_error_dialog (GTK_WINDOW (sxed->dialog), "%s", err);
        g_free (err);
        g_free (str);

        return FALSE;
    }
    if (g_strcmp0 (key, "sx-credit-formula") == 0)
        tcds->creditSum = gnc_numeric_add (tcds->creditSum, tmp, 100,
                                          GNC_DENOM_AUTO | GNC_HOW_DENOM_LCD);
    else
        tcds->debitSum = gnc_numeric_add (tcds->debitSum, tmp, 100,
                                          GNC_DENOM_AUTO | GNC_HOW_DENOM_LCD);
    g_free (str);
    return TRUE;
}

typedef struct
{
    GncSxEditorDialog *sxed;
    GHashTable *txns;
    GHashTable *vars;
    txnCreditDebitSums *tcds;
    gboolean multi_commodity;
    gboolean err;
} CheckTxnSplitData;

static void
split_error_warning_dialog (GtkWidget *parent, const gchar *title,
                            gchar *message)
{
    gnc_error_dialog (GTK_WINDOW (parent), "%s\n\n%s", title, message);
}

static gboolean
check_transaction_splits (Transaction *txn, gpointer data)
{
    GList *splitList = xaccTransGetSplitList (txn);
    CheckTxnSplitData *sd = (CheckTxnSplitData*)data;

    for (; splitList; splitList = splitList->next)
    {
        Split *s = (Split*)splitList->data;

        if (g_hash_table_lookup (sd->txns, (gpointer)txn) == NULL)
        {
            sd->tcds = tcds_new ();
            sd->tcds->window = GTK_WINDOW(sd->sxed->dialog);
            g_hash_table_insert (sd->txns, (gpointer)txn, (gpointer)(sd->tcds));
        }

        if (!gnc_sxed_split_check_account (sd->sxed, s, sd->tcds))
        {
            gchar *message = g_strdup_printf
                (_("Split with memo %s has an invalid account."),
                 xaccSplitGetMemo (s));
            split_error_warning_dialog (sd->sxed->dialog,
                                        _("Invalid Account in Split"),
                                        message);
            g_free (message);
            sd->err = TRUE;
            return FALSE;
        }

        sd->multi_commodity |= sd->tcds->multi_commodity;

        if (!gnc_sxed_split_calculate_formula (sd->sxed, s, sd->vars,
                                               "sx-credit-formula",
                                               sd->tcds))
        {
            gchar *message = g_strdup_printf
                (_("Split with memo %s has an unparsable Credit Formula."),
                 xaccSplitGetMemo (s));
            split_error_warning_dialog (sd->sxed->dialog,
                                        _("Unparsable Formula in Split"),
                                        message);
            g_free (message);
            sd->err = TRUE;
            return FALSE;
        }

        if (!gnc_sxed_split_calculate_formula (sd->sxed, s, sd->vars,
                                               "sx-debit-formula",
                                               sd->tcds))

        {
            gchar *message = g_strdup_printf
                (_("Split with memo %s has an unparsable Debit Formula."),
                 xaccSplitGetMemo (s));
            split_error_warning_dialog (sd->sxed->dialog,
                                        _("Unparsable Formula in Split"),
                                        message);
            g_free (message);
            sd->err = TRUE;
            return FALSE;
        }
    }
    return FALSE; // return FALSE to continue to next transaction
}

/*******************************************************************************
 * Checks to make sure that the SX is in a reasonable state to save.
 * @return true if checks out okay, false otherwise.
 ******************************************************************************/
static gboolean
gnc_sxed_check_consistent (GncSxEditorDialog *sxed)
{
    sxed->save_unbalanceable = FALSE;
    sxed->save_duplicate_name = FALSE;
    sxed->save_never_runs = FALSE;

    /* Do checks on validity and such, interrupting the user if
     * things aren't right.
     *
     * Features...
     * X support formulas [?!]
     * X balancing the SX if contain numeric-only formula data.
     *   X agreement with create-automagically/notification controls
     * X the 'will ever be valid' check should take num-occur vals into
     *   account.
     * X SX name is unique
     * X SX has a name
     * X "weekly" FS has some days set.
     * X "once" with reasonable start/end dates.
     *   X This doesn't work at the time the 'weekly' one was fixed with
     *     user-confirmation, below; the once SX is always valid.
     * [X more generically, creating a "not scheduled" SX is probably not
     *   right... ]
     */

    gint ttVarCount = 0, splitCount = 0;
    static const int NUM_ITERS_WITH_VARS = 5;
    static const int NUM_ITERS_NO_VARS = 1;
    int numIters = NUM_ITERS_NO_VARS, i;
    gboolean unbalanceable = FALSE;
    gpointer unusedKey, unusedValue;

    GHashTable *vars = g_hash_table_new_full (g_str_hash, g_str_equal, g_free,
                                  (GDestroyNotify)gnc_sx_variable_free);
    GHashTable *txns = g_hash_table_new_full (g_direct_hash, g_direct_equal,
                                              NULL, g_free);
    CheckTxnSplitData sd = {sxed, txns, vars, NULL, FALSE, FALSE};

    /**
     * Plan:
     * . Do a first pass to get the variables.
     * . Set each variable to random values.
     * . see if we balance after that
     *   . true: all good
     *   . false: indicate to user, allow decision.
     */

    /* numeric-formulas-get-balanced determination */
    gnc_sx_get_variables (sxed->sx, vars);

    ttVarCount = g_hash_table_size (vars);
    if (ttVarCount != 0)
    {
        /* balance with random variable bindings some number of times in an
         * attempt to ferret out un-balanceable transactions.
         */
        numIters = NUM_ITERS_WITH_VARS;
    }

    for (i = 0; i < numIters && !unbalanceable; i++)
    {
        GList *splitList = xaccSchedXactionGetSplits (sxed->sx);
        Account *tmpl_acct = gnc_sx_get_template_transaction_account (sxed->sx);
        gnc_sx_randomize_variables (vars);
        g_hash_table_foreach (txns, set_sums_to_zero, NULL);

        splitCount += g_list_length (splitList);
        g_list_free (splitList);

        xaccAccountForEachTransaction (tmpl_acct, check_transaction_splits, &sd);

        if (sd.err)
        {
            g_hash_table_destroy (vars);
            g_hash_table_destroy (txns);
            return FALSE;
        }

        g_hash_table_foreach (txns, check_credit_debit_balance, &unbalanceable);
    }

    /* Subtract out pre-defined vars */
    if (g_hash_table_lookup_extended (vars, "i", &unusedKey, &unusedValue))
        ttVarCount -= 1;

    g_hash_table_destroy (vars);
    g_hash_table_destroy (txns);

    sxed->save_unbalanceable = unbalanceable;

    if (!gnc_sxed_check_names (sxed))
        return FALSE;

    if (!gnc_sxed_check_autocreate (sxed, ttVarCount,
                                    splitCount, sd.multi_commodity))
        return FALSE;

    if (!gnc_sxed_check_endpoint (sxed))
        return FALSE;
    return TRUE;
}


static void
sxed_save_complete (GncSxEditorDialog *sxed)
{
    QofBook *book;
    SchedXactions *sxes;

    gnc_sxed_save_sx (sxed);

    if (sxed->newsxP)
    {
        book = gnc_get_current_book ();
        sxes = gnc_book_get_schedxactions (book);
        gnc_sxes_add_sx (sxes, sxed->sx);
        sxed->newsxP = FALSE;
    }

    sxed_destroy_window (sxed);
}


static void
sxed_save_endpoint_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncSxEditorDialog *sxed = user_data;

    (void) parent;
    if (response == GTK_RESPONSE_YES)
        sxed_save_complete (sxed);
    else
        sxed_set_decision_pending (sxed, FALSE);
}


static void
sxed_save_name_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncSxEditorDialog *sxed = user_data;

    (void) parent;
    if (response != GTK_RESPONSE_YES)
    {
        sxed_set_decision_pending (sxed, FALSE);
        return;
    }

    if (sxed->save_never_runs)
    {
        gnc_verify_dialog_async (
            GTK_WINDOW (sxed->dialog), FALSE, sxed_save_endpoint_finished, sxed, "%s",
            _("You have attempted to create a Scheduled Transaction which will "
              "never run. Do you really want to do this?"));
        return;
    }

    sxed_save_complete (sxed);
}


static void
sxed_save_unbalanced_finished (GtkWindow *parent, gint response, gpointer user_data)
{
    GncSxEditorDialog *sxed = user_data;

    (void) parent;
    if (response != GTK_RESPONSE_YES)
    {
        sxed_set_decision_pending (sxed, FALSE);
        return;
    }

    if (sxed->save_duplicate_name)
    {
        const gchar *name = gnc_entry_get_text (sxed->nameEntry);
        gnc_verify_dialog_async (
            GTK_WINDOW (sxed->dialog), FALSE, sxed_save_name_finished, sxed,
            _("A Scheduled Transaction with the name \"%s\" already exists. "
              "Are you sure you want to name this one the same?"), name);
        return;
    }

    if (sxed->save_never_runs)
    {
        gnc_verify_dialog_async (
            GTK_WINDOW (sxed->dialog), FALSE, sxed_save_endpoint_finished, sxed, "%s",
            _("You have attempted to create a Scheduled Transaction which will "
              "never run. Do you really want to do this?"));
        return;
    }

    sxed_save_complete (sxed);
}


static void
gnc_sxed_check_consistent_async (GncSxEditorDialog *sxed)
{
    if (!gnc_sxed_check_consistent (sxed))
    {
        sxed_set_decision_pending (sxed, FALSE);
        return;
    }

    if (sxed->save_unbalanceable)
    {
        gnc_verify_dialog_async (
            GTK_WINDOW (sxed->dialog), FALSE, sxed_save_unbalanced_finished, sxed, "%s",
            _("The Scheduled Transaction Editor cannot automatically balance all "
              "of the transactions in this Scheduled Transaction. Should it still "
              "be entered?"));
        return;
    }

    sxed_save_unbalanced_finished (GTK_WINDOW (sxed->dialog), GTK_RESPONSE_YES, sxed);
}


/******************************************************************************
 * Saves the contents of the SX.  This assumes that gnc_sxed_check_consistent
 * has returned true.
  *****************************************************************************/
static void
gnc_sxed_save_sx (GncSxEditorDialog *sxed)
{
    gnc_sx_begin_edit (sxed->sx);

    /* name */
    const gchar *name = gnc_entry_get_text (sxed->nameEntry);
    if (name && *name)
        xaccSchedXactionSetName (sxed->sx, name);

    /* date */
    {
        GDate gdate;

        if (gtk_check_button_get_active (sxed->optEndDate))
        {
            /* get the end date data */
            gnc_gdate_set_time64(&gdate,
                                  gnc_date_edit_get_date (
                                      sxed->endDateEntry));
            xaccSchedXactionSetEndDate (sxed->sx, &gdate);
            /* set the num occurrences data */
            xaccSchedXactionSetNumOccur (sxed->sx, 0);
        }
        else if (gtk_check_button_get_active (sxed->optEndCount))
        {
            gint num;

            /* get the occurrences data */
            num  =
                gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->endCountSpin));
            xaccSchedXactionSetNumOccur (sxed->sx, num);

            num =
                gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->endRemainSpin));
            xaccSchedXactionSetRemOccur (sxed->sx, num);

            g_date_clear (&gdate, 1);
            xaccSchedXactionSetEndDate (sxed->sx, &gdate);
        }
        else if (gtk_check_button_get_active (sxed->optEndNone))
        {
            xaccSchedXactionSetNumOccur (sxed->sx, 0);
            g_date_clear (&gdate, 1);
            xaccSchedXactionSetEndDate (sxed->sx, &gdate);
        }
        else
        {
            g_critical ("no valid end specified\n");
        }
    }

    /* Enabled states */
    {
        gboolean enabledState;

        enabledState = gtk_check_button_get_active (sxed->enabledOpt);
        xaccSchedXactionSetEnabled (sxed->sx, enabledState);
    }

    /* Auto-create/notification states */
    {
        gboolean autocreateState, notifyState;

        autocreateState = gtk_check_button_get_active (sxed->autocreateOpt);
        notifyState = gtk_check_button_get_active (sxed->notifyOpt);
        /* "Notify" only makes sense if AutoCreate is activated;
         * enforce that here. */
        xaccSchedXactionSetAutoCreate (sxed->sx,
                                       autocreateState,
                                       (autocreateState & notifyState));
    }

    /* days in advance */
    {
        int daysInAdvance;

        daysInAdvance = 0;
        if (gtk_check_button_get_active (sxed->advanceOpt))
        {
            daysInAdvance =
                gtk_spin_button_get_value_as_int (sxed->advanceSpin);
        }
        xaccSchedXactionSetAdvanceCreation (sxed->sx, daysInAdvance);

        daysInAdvance = 0;
        if (gtk_check_button_get_active (sxed->remindOpt))
        {
            daysInAdvance =
                gtk_spin_button_get_value_as_int (sxed->remindSpin);
        }
        xaccSchedXactionSetAdvanceReminder (sxed->sx, daysInAdvance);
    }

    /* start date and freq spec */
    {
        GDate gdate;
        GList *schedule = NULL;

        gnc_frequency_save_to_recurrence (sxed->gncfreq, &schedule, &gdate);
        gnc_sx_set_schedule (sxed->sx, schedule);
        {
            gchar *recurrence_str = recurrenceListToCompactString (schedule);
            DEBUG ("recurrences parsed [%s]", recurrence_str);
            g_free (recurrence_str);
        }

        /* now that we have it, set the start date */
        xaccSchedXactionSetStartDate (sxed->sx, &gdate);
    }

    gnc_sx_commit_edit (sxed->sx);
}

static void
update_sensitivity (GncSxEditorDialog *sxed)
{
    gboolean enabled = gtk_check_button_get_active (sxed->enabledOpt);
    gboolean autocreate = gtk_check_button_get_active (sxed->autocreateOpt);
    gboolean advance = gtk_check_button_get_active (sxed->advanceOpt);
    gboolean remind = gtk_check_button_get_active (sxed->remindOpt);
    gboolean type_date = (sxed->end_type == END_DATE);
    gboolean type_occur = (sxed->end_type == END_OCCUR);

    gnc_suspend_gui_refresh ();

    gtk_widget_set_sensitive (GTK_WIDGET (sxed->autocreateOpt), enabled);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->notifyOpt), enabled && autocreate);

    gtk_widget_set_sensitive (GTK_WIDGET (sxed->advanceOpt), enabled);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->advanceSpin), enabled && advance);

    gtk_widget_set_sensitive (GTK_WIDGET (sxed->remindOpt), enabled);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->remindSpin), enabled && remind);

    gtk_widget_set_sensitive (GTK_WIDGET (sxed->optEndNone), enabled);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->optEndDate), enabled);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->optEndCount), enabled);

    gtk_widget_set_sensitive (GTK_WIDGET (sxed->endDateEntry), enabled && type_date);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->endCountSpin), enabled && type_occur);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->endRemainSpin), enabled && type_occur);

    gtk_widget_set_sensitive (gtk_notebook_get_nth_page (sxed->notebook, 1), enabled);
    gtk_widget_set_sensitive (gtk_notebook_get_nth_page (sxed->notebook, 2), enabled);

    gnc_resume_gui_refresh ();
}

static void
enabled_toggled_cb (GtkCheckButton *o, GncSxEditorDialog *sxed)
{
    update_sensitivity (sxed);
}

static void
autocreate_toggled_cb (GtkCheckButton *o, GncSxEditorDialog *sxed)
{
    update_sensitivity (sxed);
}

static void
advance_toggled_cb (GtkButton *o, GncSxEditorDialog *sxed)
{
    update_sensitivity (sxed);
}

static void
remind_toggled_cb (GtkButton *o, GncSxEditorDialog *sxed)
{
    update_sensitivity (sxed);
}


/* Local destruction of dialog */
static void
scheduledxaction_editor_dialog_destroy (GtkWidget *object, gpointer data)
{
    GncSxEditorDialog *sxed = data;

    if (sxed == NULL)
        return;

    gnc_unregister_gui_component_by_data
        (DIALOG_SCHEDXACTION_EDITOR_CM_CLASS, sxed);

    gnc_embedded_window_close_page (sxed->embed_window, sxed->plugin_page);
    gtk_window_destroy (GTK_WINDOW(sxed->embed_window));
    sxed->embed_window = NULL;
    sxed->plugin_page = NULL;
    sxed->ledger = NULL;

    g_free (sxed->sxGUIDstr);
    sxed->sxGUIDstr = NULL;

    if (sxed->newsxP)
    {
        /* FIXME: WTF???
         *
         * "WTF" explanation: in the "new" click from the caller, we
         * set this flag.  When "ok" is pressed on the dialog, we set
         * this flag to false, and thus leave the SX live.  If
         * "Cancel" is clicked, the flag will still be true, and this
         * SX will be cleaned, here. -- jsled
         */
        gnc_sx_begin_edit (sxed->sx);
        xaccSchedXactionDestroy (sxed->sx);
    }
    sxed->sx = NULL;

    g_free (sxed);
}


static gboolean
sxed_close_request (GtkWindow *window, gpointer user_data)
{
    (void) window;
    sxed_request_cancel (user_data);
    return TRUE;
}

static gboolean
focus_idle_callback(gpointer user_data)
{
    GNCLedgerDisplay  *ledger_display = (GNCLedgerDisplay  *)user_data;

    if (ledger_display)
        gnc_ledger_display_refresh(ledger_display);

    return FALSE;
}

static void
on_notebook_switch_page(GtkNotebook *notebook, GtkWidget *page,
                        guint page_num, gpointer user_data)
{
    GtkWidget *current_page = gtk_notebook_get_nth_page(notebook, page_num);
    if (current_page && page_num == 2)
    {
        GncSxEditorDialog *sxed = (GncSxEditorDialog *)user_data;

        // Wait until Gtk is idle to refresh the display.
        g_idle_add (focus_idle_callback, sxed->ledger);
    }
}

/*************************************
 * Create the Schedule Editor Dialog *
 ************************************/
GncSxEditorDialog *
gnc_ui_scheduled_xaction_editor_dialog_create (GtkWindow *parent,
                                               SchedXaction *sx, gboolean newSX)
{
    GncSxEditorDialog *sxed;
    GtkBuilder *builder;
    GtkWidget *button;
    int i;
    int id;
    GList *dlgExists = NULL;

    static struct widgetSignalCallback
    {
        char     *name;
        char     *signal;
        void     (*fn)();
        gpointer objectData;
    } widgets[] =
        {
            { "ok_button",     "clicked",       G_CALLBACK(editor_ok_button_clicked_cb),     NULL },
            { "cancel_button", "clicked",       G_CALLBACK(editor_cancel_button_clicked_cb), NULL },
            { "help_button",   "clicked",       G_CALLBACK(editor_help_button_clicked_cb),   NULL },
            { "rb_noend",      "toggled",       G_CALLBACK(endgroup_rb_toggled_cb),          GINT_TO_POINTER (END_NEVER_OPTION) },
            { "rb_enddate",    "toggled",       G_CALLBACK(endgroup_rb_toggled_cb),          GINT_TO_POINTER (END_DATE_OPTION) },
            { "rb_num_occur",  "toggled",       G_CALLBACK(endgroup_rb_toggled_cb),          GINT_TO_POINTER (NUM_OCCUR_OPTION) },
            { "remain_spin" ,  "value-changed", G_CALLBACK(sxed_excal_update_adapt_cb),      NULL },
            { "enabled_opt",   "toggled",       G_CALLBACK(enabled_toggled_cb),              NULL },
            { "autocreate_opt", "toggled",       G_CALLBACK(autocreate_toggled_cb),           NULL },
            { "advance_opt",   "toggled",       G_CALLBACK(advance_toggled_cb),              NULL },
            { "remind_opt",    "toggled",       G_CALLBACK(remind_toggled_cb),               NULL },
            { NULL,             NULL,            NULL,                            NULL }
          };

    dlgExists = gnc_find_gui_components (DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                         editor_component_sx_equality,
                                         sx);
    if (dlgExists)
    {
        DEBUG ("dialog already exists; using that one.");
        sxed = (GncSxEditorDialog*)dlgExists->data;
        gtk_window_present (GTK_WINDOW (sxed->dialog));
        g_list_free (dlgExists);
        return sxed;
    }

    sxed = g_new0(GncSxEditorDialog, 1);

    sxed->sx     = sx;
    sxed->newsxP = newSX;

    /* Load up Glade file */
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-sx.ui", "advance_days_adj");
    gnc_builder_add_from_file (builder, "dialog-sx.ui", "remind_days_adj");
    gnc_builder_add_from_file (builder, "dialog-sx.ui", "end_spin_adj");
    gnc_builder_add_from_file (builder, "dialog-sx.ui", "remain_spin_adj");
    gnc_builder_add_from_file (builder, "dialog-sx.ui", "scheduled_transaction_editor_dialog");

    sxed->builder = builder;

    /* Connect the Widgets */
    sxed->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "scheduled_transaction_editor_dialog"));
    g_object_set_data (G_OBJECT (sxed->dialog), "gnc-sxed-dialog-state", sxed);
    sxed->notebook = GTK_NOTEBOOK (gtk_builder_get_object (builder, "editor_notebook"));
    sxed->nameEntry = GTK_ENTRY (gtk_builder_get_object (builder, "sxe_name"));
    sxed->enabledOpt = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "enabled_opt"));
    sxed->autocreateOpt = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "autocreate_opt"));
    sxed->notifyOpt = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "notify_opt"));
    sxed->advanceOpt = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "advance_opt"));
    sxed->advanceSpin = GTK_SPIN_BUTTON (gtk_builder_get_object (builder, "advance_days"));
    sxed->remindOpt = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "remind_opt"));
    sxed->remindSpin = GTK_SPIN_BUTTON (gtk_builder_get_object (builder, "remind_days"));
    sxed->lastOccurLabel = GTK_LABEL (gtk_builder_get_object (builder, "last_occur_label"));
    sxed->optEndNone = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "rb_noend"));
    sxed->optEndDate = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "rb_enddate"));
    sxed->optEndCount = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "rb_num_occur"));
    sxed->endCountSpin = GTK_ENTRY (gtk_builder_get_object (builder, "end_spin"));
    sxed->endRemainSpin = GTK_ENTRY (gtk_builder_get_object (builder, "remain_spin"));

    // Set the name of this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET (sxed->dialog), "gnc-id-sx-editor");
    gnc_widget_style_context_add_class (GTK_WIDGET (sxed->dialog), "gnc-class-sx");

    gtk_window_set_transient_for (GTK_WINDOW (sxed->dialog), parent);

    /* Setup the end-date GNC widget */
    {
        GtkWidget *endDateBox = GTK_WIDGET (gtk_builder_get_object (builder, "editor_end_date_box"));
        sxed->endDateEntry = GNC_DATE_EDIT (gnc_date_edit_new (gnc_time (NULL), FALSE, FALSE));
        gtk_widget_set_visible (GTK_WIDGET (sxed->endDateEntry), TRUE);
        g_signal_connect (sxed->endDateEntry, "date-changed",
                          G_CALLBACK (sxed_excal_update_adapt_cb), sxed);
        gnc_box_append_full (GTK_BOX (endDateBox), GTK_WIDGET (sxed->endDateEntry),
                            TRUE, TRUE, 0);
    }

    id = gnc_register_gui_component (DIALOG_SCHEDXACTION_EDITOR_CM_CLASS,
                                     NULL, /* no refresh handler */
                                     sxed_close_handler,
                                     sxed);
    // This ensure this dialog is closed when the session is closed.
    gnc_gui_component_set_session (id, gnc_get_current_session ());

    g_signal_connect (sxed->dialog, "close-request",
                      G_CALLBACK (sxed_close_request), sxed);
    g_signal_connect (sxed->dialog, "destroy",
                      G_CALLBACK (scheduledxaction_editor_dialog_destroy),
                      sxed);
    g_signal_connect (sxed->notebook, "switch-page",
                      G_CALLBACK(on_notebook_switch_page),
                      sxed);

    for (i = 0; widgets[i].name; i++)
    {
        button = GTK_WIDGET (gtk_builder_get_object (builder, widgets[i].name));
        if (widgets[i].objectData)
        {
            g_object_set_data (G_OBJECT (button), "whichOneAmI",
                               widgets[i].objectData);
        }
        g_signal_connect (button, widgets[i].signal,
                          G_CALLBACK (widgets[i].fn), sxed);
    }

    /* Set sensitivity settings  */
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->notifyOpt), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->advanceSpin), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->remindSpin), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->endCountSpin), FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (sxed->endRemainSpin), FALSE);
    gtk_editable_set_editable (GTK_EDITABLE (sxed->advanceSpin), TRUE);
    gtk_editable_set_editable (GTK_EDITABLE (sxed->remindSpin), TRUE);

    /* Allow resize */
    gtk_window_set_resizable (GTK_WINDOW (sxed->dialog), TRUE);
    gnc_restore_window_size (GNC_PREFS_GROUP_SXED, GTK_WINDOW (sxed->dialog), parent);

    /* create the frequency-selection widget and example [dense-]calendar. */
    schedXact_editor_create_freq_sel (sxed);

    /* create the template-transaction ledger window */
    schedXact_editor_create_ledger (sxed);

    /* populate */
    schedXact_editor_populate (sxed);

    /* Do not call show_all here */
    gtk_window_present (GTK_WINDOW (sxed->dialog));
    gtk_notebook_set_current_page (GTK_NOTEBOOK (sxed->notebook), 0);

    /* Refresh the cal and the ledger */
    gtk_widget_queue_resize (GTK_WIDGET (sxed->example_cal));

    gnc_ledger_display_refresh (sxed->ledger);

    /* Move keyboard focus to the name entry */
    gtk_widget_grab_focus (GTK_WIDGET (sxed->nameEntry));

    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func, sxed);
    g_object_unref (G_OBJECT (builder));

    return sxed;
}


static void
schedXact_editor_create_freq_sel (GncSxEditorDialog *sxed)
{
    GtkBox *b;
    GtkWidget *example_cal_scrolled_win = NULL;

    b = GTK_BOX (gtk_builder_get_object (sxed->builder, "gncfreq_hbox"));

    sxed->gncfreq =
        GNC_FREQUENCY (gnc_frequency_new_from_recurrence (gnc_sx_get_schedule (sxed->sx),
                                                          xaccSchedXactionGetStartDate (sxed->sx)));
    g_assert (sxed->gncfreq);
    g_signal_connect (sxed->gncfreq, "changed",
                      G_CALLBACK (gnc_sxed_freq_changed),
                      sxed);

    gnc_box_append_full (GTK_BOX (b), GTK_WIDGET (sxed->gncfreq), TRUE, TRUE, 0);

    b = GTK_BOX (gtk_builder_get_object (sxed->builder, "example_cal_hbox"));

    example_cal_scrolled_win = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (example_cal_scrolled_win),
                                    GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    gnc_box_append_full (GTK_BOX (b), example_cal_scrolled_win, TRUE, TRUE, 0);

    sxed->dense_cal_model = gnc_dense_cal_store_new (EX_CAL_NUM_MONTHS * 31);
    sxed->example_cal = GNC_DENSE_CAL(gnc_dense_cal_new_with_model (GTK_WINDOW(sxed->dialog),
                                                                    GNC_DENSE_CAL_MODEL(sxed->dense_cal_model)));
    g_assert (sxed->example_cal);
    gnc_dense_cal_set_num_months (sxed->example_cal, EX_CAL_NUM_MONTHS);
    gnc_dense_cal_set_months_per_col (sxed->example_cal, EX_CAL_MO_PER_COL);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(example_cal_scrolled_win),
                                   GTK_WIDGET(sxed->example_cal));


    gtk_widget_set_visible (example_cal_scrolled_win, TRUE);
}


static void
schedXact_editor_create_ledger (GncSxEditorDialog *sxed)
{
    SplitRegister *splitreg;
    GtkWidget *main_vbox;

    /* Create the ledger */
    sxed->sxGUIDstr = guid_to_string (xaccSchedXactionGetGUID (sxed->sx));
    sxed->ledger = gnc_ledger_display_template_gl (sxed->sxGUIDstr);
    splitreg = gnc_ledger_display_get_split_register (sxed->ledger);

    /* First the embedded window */
    main_vbox = GTK_WIDGET (gtk_builder_get_object (sxed->builder, "register_vbox"));
    sxed->embed_window =
        gnc_embedded_window_new ("embedded-win",
                                 gnc_sxed_menu_entries,
                                 gnc_sxed_menu_n_entries,
                                 "gnc-embedded-register-window.ui",
                                 sxed->dialog,
                                 FALSE, /* no accelerators */
                                 sxed);
    gnc_box_append_full (GTK_BOX (main_vbox), GTK_WIDGET (sxed->embed_window),
                        TRUE, TRUE, 0);

    /* Now create the register plugin page. */
    sxed->plugin_page = gnc_plugin_page_register_new_ledger (sxed->ledger);

    gnc_plugin_page_merge_actions (sxed->plugin_page);

    gtk_widget_insert_action_group (GTK_WIDGET(sxed->embed_window),
                                    gnc_plugin_page_get_simple_action_group_name (sxed->plugin_page),
                                    G_ACTION_GROUP(gnc_plugin_page_get_action_group (sxed->plugin_page)));

    gnc_plugin_page_register_set_options (sxed->plugin_page,
                                          NUM_LEDGER_LINES_DEFAULT, FALSE);
    gnc_embedded_window_open_page (sxed->embed_window, sxed->plugin_page);

    /* configure... */
    /* use double-line, so scheduled transaction Notes can be edited */
    gnc_split_register_config (splitreg,
                              splitreg->type, splitreg->style,
                              TRUE);
    gnc_split_register_set_auto_complete (splitreg, FALSE);

    /* don't show present/future divider [by definition, not necessary] */
    gnc_split_register_show_present_divider (splitreg, FALSE);
}


static void
schedXact_editor_populate (GncSxEditorDialog *sxed)
{
    char *name;
    time64 tmpDate;
    SplitRegister *splitReg;
    const GDate *gd;
    gint daysInAdvance;
    gboolean enabledState, autoCreateState, notifyState;

    name = xaccSchedXactionGetName (sxed->sx);
    if (name)
    {
        gnc_entry_set_text (sxed->nameEntry, name);
    }
    {
        gd = xaccSchedXactionGetLastOccurDate (sxed->sx);
        if (g_date_valid (gd))
        {
            gchar dateBuf [MAX_DATE_LENGTH+1];
            qof_print_gdate (dateBuf, MAX_DATE_LENGTH, gd);
            gtk_label_set_text (sxed->lastOccurLabel, dateBuf);
        }
        else
        {
            gtk_label_set_text (sxed->lastOccurLabel, _("(never)"));
        }
        gd = NULL;
    }

    gd = xaccSchedXactionGetEndDate (sxed->sx);
    if (g_date_valid (gd))
    {
        gtk_check_button_set_active (sxed->optEndDate, TRUE);
        tmpDate = gnc_time64_get_day_start_gdate (gd);
        gnc_date_edit_set_time (sxed->endDateEntry, tmpDate);

        set_endgroup_toggle_states (sxed, END_DATE);
    }
    else if (xaccSchedXactionHasOccurDef (sxed->sx))
    {
        gint numOccur = xaccSchedXactionGetNumOccur (sxed->sx);
        gint numRemain = xaccSchedXactionGetRemOccur (sxed->sx);

        gtk_check_button_set_active (sxed->optEndCount, TRUE);

        gtk_spin_button_set_value (GTK_SPIN_BUTTON (sxed->endCountSpin), numOccur);
        gtk_spin_button_set_value (GTK_SPIN_BUTTON (sxed->endRemainSpin), numRemain);

        set_endgroup_toggle_states (sxed, END_OCCUR);
    }
    else
    {
        gtk_check_button_set_active (sxed->optEndNone, TRUE);
        set_endgroup_toggle_states (sxed, END_NEVER);
    }

    enabledState = xaccSchedXactionGetEnabled (sxed->sx);
    gtk_check_button_set_active (sxed->enabledOpt, enabledState);

    /* Do auto-create/notify setup */
    if (sxed->newsxP)
    {
        autoCreateState =
            gnc_prefs_get_bool (GNC_PREFS_GROUP_SXED, GNC_PREF_CREATE_AUTO);
        notifyState =
            gnc_prefs_get_bool (GNC_PREFS_GROUP_SXED, GNC_PREF_NOTIFY);
    }
    else
    {
        xaccSchedXactionGetAutoCreate (sxed->sx,
                                       &autoCreateState,
                                       &notifyState);
    }
    gtk_check_button_set_active (sxed->autocreateOpt, autoCreateState);
    if (!autoCreateState)
    {
        notifyState = FALSE;
    }
    gtk_check_button_set_active (sxed->notifyOpt, notifyState);

    /* Do days-in-advance-to-create widget[s] setup. */
    if (sxed->newsxP)
    {
        daysInAdvance =
            gnc_prefs_get_float (GNC_PREFS_GROUP_SXED, GNC_PREF_CREATE_DAYS);
    }
    else
    {
        daysInAdvance =
            xaccSchedXactionGetAdvanceCreation (sxed->sx);
    }
    if (daysInAdvance != 0)
    {
        gtk_check_button_set_active (sxed->advanceOpt, TRUE);
        gtk_spin_button_set_value (sxed->advanceSpin,
                                   (gfloat)daysInAdvance);
    }

    /* Do days-in-advance-to-remind widget[s] setup. */
    if (sxed->newsxP)
    {
        daysInAdvance =
            gnc_prefs_get_float (GNC_PREFS_GROUP_SXED, GNC_PREF_REMIND_DAYS);
    }
    else
    {
        daysInAdvance =
            xaccSchedXactionGetAdvanceReminder (sxed->sx);
    }
    if (daysInAdvance != 0)
    {
        gtk_check_button_set_active (sxed->remindOpt, TRUE);
        gtk_spin_button_set_value (sxed->remindSpin,
                                   (gfloat)daysInAdvance);
    }

    if (sxed->newsxP)
    {
        gnc_sx_set_instance_count (sxed->sx, 1);
    }

    /* populate the ledger */
    {
        /* create the split list */
        GList *splitList = xaccSchedXactionGetSplits (sxed->sx);
        if (splitList)
        {
            splitReg = gnc_ledger_display_get_split_register (sxed->ledger);
            gnc_split_register_load (splitReg, splitList, NULL, NULL);
        } /* otherwise, use the existing stuff. */
        g_list_free (splitList);
    }

    /* Update the example cal */
    gnc_sxed_update_cal (sxed);
}


static void
set_endgroup_toggle_states (GncSxEditorDialog *sxed, EndType type)
{
    sxed->end_type = type;
    update_sensitivity (sxed);
}


static void
endgroup_rb_toggled_cb (GtkButton *b, gpointer d)
{
    /* figure out which one */
    GncSxEditorDialog *sxed;
    gint id;

    sxed = (GncSxEditorDialog*)d;
    id = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (b), "whichOneAmI"));

    switch (id)
    {
        case END_NEVER_OPTION:
            set_endgroup_toggle_states (sxed, END_NEVER);
            break;
        case END_DATE_OPTION:
            set_endgroup_toggle_states (sxed, END_DATE);
            break;
        case NUM_OCCUR_OPTION:
            set_endgroup_toggle_states (sxed, END_OCCUR);
            break;
        default:
            g_critical ("Unknown id %d", id);
            break;
    }
    gnc_sxed_update_cal (sxed);
}


static gboolean
editor_component_sx_equality (gpointer find_data,
                              gpointer user_data)
{
    return ((SchedXaction*)find_data
            == ((GncSxEditorDialog*)user_data)->sx);
}

static void
gnc_sxed_update_cal (GncSxEditorDialog *sxed)
{
    GList *recurrences = NULL;
    GDate start_date, first_date;

    g_date_clear (&start_date, 1);

    gnc_frequency_save_to_recurrence (sxed->gncfreq, &recurrences, &start_date);
    recurrenceListNextInstance (recurrences, &start_date, &first_date);

    /* Deal with the fact that this SX may have been run before [the
     * calendar should only show upcoming instances]... */
    {
        const GDate *last_sx_inst;

        last_sx_inst = xaccSchedXactionGetLastOccurDate (sxed->sx);
        if (g_date_valid (last_sx_inst)
            && g_date_valid (&first_date)
            && g_date_compare (last_sx_inst, &first_date) > 0)
        {
            /* last occurrence will be passed as initial date to update store
             * later on as well, but only if it's past first_date */
            start_date = *last_sx_inst;
            recurrenceListNextInstance (recurrences, &start_date, &first_date);
        }
        else
            /* move one day back so the store can get the proper first recurrence. */
            g_date_subtract_days (&start_date, 1);

    }

    if (!g_date_valid (&first_date))
    {
        /* Note: There are no recurrences for PERIOD_NONE and on initial setting
         * of PERIOD_WEEKLY (no days set), so still need to 'do nothing' */
        gboolean do_nothing = TRUE;
        if (recurrences)
        {
            Recurrence *r = g_list_nth_data (recurrences, 0);
            if (r && r->ptype == PERIOD_ONCE)
                do_nothing = FALSE;
        }
        /* Nothing to do. */
        if (do_nothing)
        {
            gnc_dense_cal_store_clear (sxed->dense_cal_model);
            goto cleanup;
        }
    }

    gnc_dense_cal_store_update_name (sxed->dense_cal_model, xaccSchedXactionGetName (sxed->sx));
    {
        gchar *schedule_desc = recurrenceListToCompactString (recurrences);
        gnc_dense_cal_store_update_info (sxed->dense_cal_model, schedule_desc);
        g_free (schedule_desc);
    }

    //gnc_dense_cal_set_month (sxed->example_cal, g_date_get_month (&first_date));
    //gnc_dense_cal_set_year (sxed->example_cal, g_date_get_year (&first_date));

    /* figure out the end restriction */
    if (gtk_check_button_get_active (sxed->optEndDate))
    {
        GDate end_date;
        g_date_clear (&end_date, 1);
        gnc_gdate_set_time64 (&end_date, gnc_date_edit_get_date (sxed->endDateEntry));
        gnc_dense_cal_store_update_recurrences_date_end (sxed->dense_cal_model, &start_date, recurrences, &end_date);
    }
    else if (gtk_check_button_get_active (sxed->optEndNone))
    {
        gnc_dense_cal_store_update_recurrences_no_end (sxed->dense_cal_model, &start_date, recurrences);
    }
    else if (gtk_check_button_get_active (sxed->optEndCount))
    {
        gint num_remain
            = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (sxed->endRemainSpin));
        gnc_dense_cal_store_update_recurrences_count_end (sxed->dense_cal_model, &start_date, recurrences, num_remain);
    }
    else
    {
        g_error ("unknown end condition");
    }

 cleanup:
    recurrenceListFree (&recurrences);
}


static void
gnc_sxed_freq_changed (GncFrequency *gf, gpointer ud)
{
    gnc_sxed_update_cal ((GncSxEditorDialog*)ud);
}


static void
sxed_excal_update_adapt_cb (GtkWidget *o, gpointer ud)
{
    gnc_sxed_update_cal ((GncSxEditorDialog*)ud);
}


void
on_sx_check_toggled_cb (GtkWidget *togglebutton, gpointer user_data)
{
    GtkWidget *widget_auto;
    GtkWidget *widget_notify;
    GHashTable *table;

    PINFO ("Togglebutton is %p and user_data is %p", togglebutton, user_data);
    PINFO ("Togglebutton builder name is %s", gtk_buildable_get_buildable_id (GTK_BUILDABLE (togglebutton)));

    /* We need to use the hash table to find the required widget to activate. */
    table = g_object_get_data (G_OBJECT (user_data), "prefs_widget_hash");

    /* "Auto-create" enables "notify before creation" setting */
    widget_auto = g_hash_table_lookup (table, "pref/" GNC_PREFS_GROUP_SXED "/" GNC_PREF_CREATE_AUTO);
    widget_notify = g_hash_table_lookup (table, "pref/" GNC_PREFS_GROUP_SXED "/" GNC_PREF_NOTIFY);

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON (widget_auto)))
        gtk_widget_set_sensitive (widget_notify, TRUE);
    else
        gtk_widget_set_sensitive (widget_notify, FALSE);

    /* "Run when opened" enables "show notification window" setting */
    widget_auto = g_hash_table_lookup (table, "pref/" GNC_PREFS_GROUP_STARTUP "/" GNC_PREF_RUN_AT_FOPEN);
    widget_notify = g_hash_table_lookup (table, "pref/" GNC_PREFS_GROUP_STARTUP "/" GNC_PREF_SHOW_AT_FOPEN);

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON (widget_auto)))
        gtk_widget_set_sensitive (widget_notify, TRUE);
    else
        gtk_widget_set_sensitive (widget_notify, FALSE);
}


/* ------------------------------------------------------------ */
/* sx app engine;  move to somewhere appropriate. :/            */

typedef struct _acct_deletion_handler_data
{
    GList *affected_sx_guids;
    GtkWidget *dialog;
    GtkWindow *parent;
    QofBook *book;
} acct_deletion_handler_data;


static SchedXaction *
sxed_lookup_sx (QofBook *book, const GncGUID *guid)
{
    GList *node;

    if (!book || !guid)
        return NULL;

    for (node = gnc_book_get_schedxactions (book)->sx_list; node; node = node->next)
    {
        SchedXaction *sx = node->data;
        if (guid_equal (xaccSchedXactionGetGUID (sx), guid))
            return sx;
    }
    return NULL;
}


static void
sxed_account_deletion_data_free (acct_deletion_handler_data *data)
{
    if (!data)
        return;

    g_list_free_full (data->affected_sx_guids, (GDestroyNotify) guid_free);
    g_free (data);
}


static void
sxed_account_deletion_destroyed (GtkWidget *widget, gpointer user_data)
{
    (void) widget;
    sxed_account_deletion_data_free (user_data);
}


static void
sxed_account_name_setup (GtkListItemFactory *factory, GtkListItem *item,
                         gpointer user_data)
{
    (void) factory;
    (void) user_data;
    gtk_list_item_set_child (item, gtk_label_new (NULL));
}


static void
sxed_account_name_bind (GtkListItemFactory *factory, GtkListItem *item,
                        gpointer user_data)
{
    GtkStringObject *string_object = GTK_STRING_OBJECT (gtk_list_item_get_item (item));
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (item));

    (void) factory;
    (void) user_data;
    gtk_label_set_label (label, gtk_string_object_get_string (string_object));
}


static void
_open_editors (GtkButton *button, gpointer user_data)
{
    acct_deletion_handler_data *data = user_data;
    GList *node;

    (void) button;
    for (node = data->affected_sx_guids; node; node = node->next)
    {
        SchedXaction *sx = sxed_lookup_sx (data->book, node->data);
        if (sx)
            gnc_ui_scheduled_xaction_editor_dialog_create (data->parent, sx, FALSE);
    }

    gtk_window_destroy (GTK_WINDOW (data->dialog));
}


static void
_sx_engine_event_handler (QofInstance *ent, QofEventId event_type,
                          gpointer user_data, gpointer evt_data)
{
    Account *acct;
    QofBook *book;
    GList *affected_sxes;

    (void) user_data;
    (void) evt_data;

    if (!(event_type & QOF_EVENT_DESTROY) || !GNC_IS_ACCOUNT (ent))
        return;

    acct = GNC_ACCOUNT (ent);
    book = qof_instance_get_book (QOF_INSTANCE (acct));
    affected_sxes = gnc_sx_get_sxes_referencing_account (book, acct);

    if (!gnc_list_length_cmp (affected_sxes, 0))
        return;

    {
        GList *node;
        acct_deletion_handler_data *data;
        GtkBuilder *builder;
        GtkWidget *dialog;
        GtkWindow *parent;
        GtkColumnView *list;
        GtkStringList *names;
        GtkNoSelection *selection;
        GtkListItemFactory *factory;
        GtkColumnViewColumn *column;

        builder = gtk_builder_new ();
        gnc_builder_add_from_file (builder, "dialog-sx.ui", "account_deletion_dialog");

        dialog = GTK_WIDGET (gtk_builder_get_object (builder, "account_deletion_dialog"));
        parent = gnc_ui_get_main_window (NULL);
        gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);

        data = g_new0 (acct_deletion_handler_data, 1);
        data->dialog = dialog;
        data->parent = parent;
        data->book = book;

        names = gtk_string_list_new (NULL);
        for (node = affected_sxes; node; node = node->next)
        {
            SchedXaction *sx = node->data;
            const gchar *name = xaccSchedXactionGetName (sx);

            data->affected_sx_guids = g_list_append (
                data->affected_sx_guids, guid_copy (xaccSchedXactionGetGUID (sx)));
            gtk_string_list_append (names, name ? name : "");
        }
        g_list_free (affected_sxes);

        list = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "sx_list"));
        selection = gtk_no_selection_new (G_LIST_MODEL (names));
        gtk_column_view_set_model (list, GTK_SELECTION_MODEL (selection));
        g_object_unref (selection);
        g_object_unref (names);

        factory = gtk_signal_list_item_factory_new ();
        g_signal_connect (factory, "setup", G_CALLBACK (sxed_account_name_setup), NULL);
        g_signal_connect (factory, "bind", G_CALLBACK (sxed_account_name_bind), NULL);
        column = gtk_column_view_column_new (_("Name"), factory);
        gtk_column_view_append_column (list, column);
        g_object_unref (column);

        g_signal_connect (gtk_builder_get_object (builder, "okbutton1"), "clicked",
                          G_CALLBACK (_open_editors), data);
        g_signal_connect (dialog, "destroy",
                          G_CALLBACK (sxed_account_deletion_destroyed), data);

        gtk_window_present (GTK_WINDOW (dialog));
        g_object_unref (builder);
    }
}

void
gnc_ui_sx_initialize (void)
{
    _sx_engine_event_handler_id = qof_event_register_handler (_sx_engine_event_handler, NULL);

    gnc_hook_add_dangler (HOOK_BOOK_OPENED,
                          (GFunc)gnc_sx_sxsincelast_book_opened, NULL, NULL);

    /* Add page to preferences page for Scheduled Transactions */
    /* The parameters are; glade file, items to add from glade file - last being the dialog, preference tab name */
    gnc_preferences_add_page ("dialog-sx.ui",
                              "create_days_adj,remind_days_adj,sx_prefs",
                              _("Scheduled Transactions"));
}
