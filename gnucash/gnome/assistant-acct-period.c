/********************************************************************\
 * assistant-acct-period.c - accounting period window for GnuCash   *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Copyright (C) 2001 Dave Peticolas <dave@krondo.com>              *
 * Copyright (C) 2003 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2011 Robert Fewell                                 *
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
\********************************************************************/
/*************************************************************************\
 * This is still a work in progress so may damage your data, to enable   *
 * for testing do the following :-                                       *
 * Add a define entry to gnc-plugin-basic-commands.c as below            *
 *     #define CLOSE_BOOKS_ACTUALLY_WORKS 1                              *
 *                                                                       *
 * Add the following to gnc-plugin-basic-commands-ui.xml on line 43      *
 * <menuitem name="ActionsCloseBooks" action="ActionsCloseBooksAction"/> *
\*************************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Recurrence.h"
#include "Query.h"
#include "Scrub.h"
#include "Transaction.h"
#include "dialog-utils.h"
#include "assistant-acct-period.h"
#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-date.h"
#include "gnc-file.h"
#include "gnc-frequency.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "misc-gnome-utils.h"
#include "gnc-session.h"

#define ASSISTANT_ACCT_PERIOD_CM_CLASS "assistant-acct-period"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

typedef enum
{
    AP_PAGE_START,
    AP_PAGE_MENU,
    AP_PAGE_BOOK,
    AP_PAGE_APPLY,
    AP_PAGE_SUMMARY,
    AP_PAGE_COUNT
} AcctPeriodPage;

/** structures *********************************************************/
typedef struct
{
    GtkWindow *window;
    GtkStack *stack;
    GtkWidget *pages[AP_PAGE_COUNT];
    GtkWidget *back_button;
    GtkWidget *next_button;
    GtkWidget *apply_button;
    GtkWidget *cancel_button;
    GtkWidget *close_button;
    guint current_page;

    GncFrequency *period_menu;
    GtkWidget *period_remarks;
    GtkWidget *close_results;
    GtkWidget *book_details;
    GtkWidget *book_title;
    GtkTextView *book_notes;
    GtkWidget *apply_label;
    GtkWidget *summary;

    time64 earliest;
    char *earliest_str;
    GDate closing_date;
    GDate prev_closing_date;
    GList *period;
    int close_status;
} AcctPeriodInfo;

static void ap_window_menu_prepare (AcctPeriodInfo *info);
static void ap_window_book_prepare (AcctPeriodInfo *info);
static void ap_window_apply_prepare (AcctPeriodInfo *info);
static void ap_window_summary_prepare (AcctPeriodInfo *info);
static gboolean ap_validate_menu (AcctPeriodInfo *info);
static void ap_window_show_page (AcctPeriodInfo *info, AcctPeriodPage page);
static void ap_window_update_navigation (AcctPeriodInfo *info);
static void ap_window_cancel (AcctPeriodInfo *info);

/* =============================================================== */
/* Find the earliest date occurring in the book.  Do this by making
 * a query and sorting by date. Since the truncated sort returns
 * only the *last* search results, sort in decreasing order.
 */
static time64
get_earliest_in_book (QofBook *book)
{
    QofQuery *q;
    GSList *p1, *p2;
    GList *res;
    time64 earliest;

    q = qof_query_create_for (GNC_ID_SPLIT);
    qof_query_set_max_results (q, 1);
    qof_query_set_book (q, book);

    /* Sort by transaction date */
    p1 = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    p1 = g_slist_prepend (p1, SPLIT_TRANS);
    p2 = g_slist_prepend (NULL, QUERY_DEFAULT_SORT);
    qof_query_set_sort_order (q, p1, p2, NULL);

    /* Reverse the sort order */
    qof_query_set_sort_increasing (q, FALSE, FALSE, FALSE);

    /* Run the query, find the earliest transaction date */
    res = qof_query_run (q);

    if (res)
        earliest = xaccQueryGetEarliestDateFound (q);
    else
        /* If no results, we don't want to bomb totally */
        earliest = gnc_time (NULL);

    qof_query_destroy (q);
    return earliest;
}

/* =============================================================== */
/* Find the number of transactions occurring before the indicated date.
 * Do this by making a query and counting the results.
 */
static int
get_num_xactions_before_date (QofBook *book, time64 close_date)
{
    QofQuery *q;
    GSList *param;
    QofQueryPredData *pred;
    GList *res, *n;
    int cnt = 0;

    q = qof_query_create_for (GNC_ID_TRANS);
    qof_query_set_max_results (q, -1);
    qof_query_set_book (q, book);

    /* Look for transactions earlier than the closing date */
    param = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    pred = qof_query_date_predicate (QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL,
                                     close_date);
    qof_query_add_term (q, param, pred, QOF_QUERY_FIRST_TERM);

    /* Run the query and count the results. */
    res = qof_query_run (q);
    for (n = res; n; n = n->next)
        cnt++;

    qof_query_destroy (q);
    return cnt;
}

/* =============================================================== */
static const char *
get_close_status_str (AcctPeriodInfo *info)
{
    switch (info->close_status)
    {
    case -1:
        return "";
    case 0:
        return _("The book was closed successfully.");
    default:
        return "";
    }
}

/* =============================================================== */
static void
ap_window_destroy_cb (GtkWidget *object, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    gnc_unregister_gui_component_by_data (ASSISTANT_ACCT_PERIOD_CM_CLASS, info);
    recurrenceListFree (&info->period);
    g_free (info->earliest_str);
    g_free (info);
    (void)object;
}

/* =============================================================== */
static void
ap_window_cancel (AcctPeriodInfo *info)
{
    gnc_close_gui_component_by_data (ASSISTANT_ACCT_PERIOD_CM_CLASS, info);
}

/* =============================================================== */
static void
ap_window_menu_prepare (AcctPeriodInfo *info)
{
    int nperiods;
    GDate period_begin, period_end, date_now;
    char *str, *earliest_str;

    ENTER ("info=%p", info);

    recurrenceListFree (&info->period);
    gnc_frequency_save_to_recurrence (info->period_menu, &info->period,
                                      &info->closing_date);

    /* Count the number of periods that would be generated. */
    g_date_clear (&period_begin, 1);
    g_date_clear (&period_end, 1);
    g_date_clear (&date_now, 1);
    nperiods = 0;
    period_end = info->closing_date;
    gnc_gdate_set_time64 (&date_now, gnc_time (NULL));

    while (0 > g_date_compare (&period_end, &date_now))
    {
        nperiods++;
        PINFO ("Period = %d and End date is %d/%d/%d", nperiods,
               g_date_get_month (&period_end),
               g_date_get_day (&period_end),
               g_date_get_year (&period_end));
        period_begin = period_end;
        recurrenceListNextInstance (info->period, &period_begin, &period_end);

        /* FIXME Check for valid period_end, not sure why it won't be!!! */
        if (!g_date_valid (&period_end))
            break;
    }

    /* The book can have changed since the previous visit to this page. */
    info->earliest = get_earliest_in_book (gnc_get_current_book ());
    g_free (info->earliest_str);
    info->earliest_str = qof_print_date (info->earliest);
    earliest_str = gnc_ctime (&info->earliest);
    PINFO ("Date of earliest transaction is %" G_GINT64_FORMAT " %s",
           info->earliest, earliest_str);
    g_free (earliest_str);

    str = g_strdup_printf (
        /* Translators: %s is a date string. %d is the number of books
           that will be created. This is a ngettext(3) message (but
           only for the %d part). */
        ngettext ("The earliest transaction date found in this book is %s. "
                  "Based on the selection made above, this book will be split "
                  "into %d book.",
                  "The earliest transaction date found in this book is %s. "
                  "Based on the selection made above, this book will be split "
                  "into %d books.",
                  nperiods),
        info->earliest_str, nperiods);
    gtk_label_set_text (GTK_LABEL (info->period_remarks), str);
    g_free (str);
}

/* =============================================================== */
static void
ap_window_book_prepare (AcctPeriodInfo *info)
{
    QofBook *currbook;
    char close_date_str[MAX_DATE_LENGTH];
    char prev_close_date_str[MAX_DATE_LENGTH];
    const char *period_text;
    char *str;
    const char *cstr;
    int ntrans, nacc;
    GtkTextBuffer *buffer;

    ENTER ("info=%p", info);

    cstr = get_close_status_str (info);
    gtk_label_set_text (GTK_LABEL (info->close_results), cstr);
    info->close_status = -1;

    recurrenceListFree (&info->period);
    gnc_frequency_save_to_recurrence (info->period_menu, &info->period,
                                      &info->closing_date);

    qof_print_date_dmy_buff (close_date_str, MAX_DATE_LENGTH,
                             g_date_get_day (&info->closing_date),
                             g_date_get_month (&info->closing_date),
                             g_date_get_year (&info->closing_date));

    currbook = gnc_get_current_book ();
    ntrans = get_num_xactions_before_date (
        currbook, gnc_time64_get_day_end_gdate (&info->closing_date));
    nacc = gnc_account_n_descendants (gnc_book_get_root_account (currbook));

    period_text =
        /* Translators: Run the assistant in your language to see GTK's translation of the button labels. */
        _("You have asked for a book to be created. This book "
          "will contain all transactions up to midnight %s "
          "(for a total of %d transactions spread over %d accounts).\n\n"
          "Amend the Title and Notes or Click on \"Next\" to proceed.\n"
          "Click on \"Back\" to adjust the dates or \"Cancel\".");
    str = g_strdup_printf (period_text, close_date_str, ntrans, nacc);
    gtk_label_set_text (GTK_LABEL (info->book_details), str);
    g_free (str);

    gtk_widget_set_visible (info->book_details, TRUE);

    qof_print_date_dmy_buff (prev_close_date_str, MAX_DATE_LENGTH,
                             g_date_get_day (&info->prev_closing_date),
                             g_date_get_month (&info->prev_closing_date),
                             g_date_get_year (&info->prev_closing_date));

    str = g_strdup_printf (_("Period %s - %s"), prev_close_date_str,
                           close_date_str);
    gnc_entry_set_text (GTK_ENTRY (info->book_title), str);

    buffer = gtk_text_view_get_buffer (info->book_notes);
    gtk_text_buffer_set_text (buffer, str, -1);
    g_free (str);
}

/* =============================================================== */
static void
ap_window_apply_prepare (AcctPeriodInfo *info)
{
    const char *btitle;
    char *str;
    const char *apply_text =
        _("The book will be created with the title %s when you "
          "click on \"Apply\". Click on \"Back\" to adjust, "
          "or \"Cancel\" to not create any book.");

    btitle = gnc_entry_get_text (GTK_ENTRY (info->book_title));
    str = g_strdup_printf (apply_text, btitle);
    gtk_label_set_text (GTK_LABEL (info->apply_label), str);
    g_free (str);
}

/* =============================================================== */
static void
ap_window_summary_prepare (AcctPeriodInfo *info)
{
    const char *msg;
    char *str;

    ENTER ("info=%p", info);

    /* Translation FIXME: Can this %s-containing message please be
       replaced by one single message? Either this closing went
       successfully ("success", "congratulations") or something else
       should be displayed anyway. */
    msg = _("%s\nCongratulations! You are done closing books!\n");

    str = g_strdup_printf (msg, get_close_status_str (info));
    gtk_label_set_text (GTK_LABEL (info->summary), str);
    g_free (str);
}

/* =============================================================== */
static gboolean
ap_validate_menu (AcctPeriodInfo *info)
{
    GDate date_now;

    ENTER ("info=%p", info);

    recurrenceListFree (&info->period);
    gnc_frequency_save_to_recurrence (info->period_menu, &info->period,
                                      &info->closing_date);

    if (0 <= g_date_compare (&info->prev_closing_date, &info->closing_date))
        return FALSE;

    g_date_clear (&date_now, 1);
    gnc_gdate_set_today (&date_now);
    return g_date_compare (&info->closing_date, &date_now) <= 0;
}

/* =============================================================== */
static gboolean
ap_window_page_is_complete (AcctPeriodInfo *info, AcctPeriodPage page)
{
    return page != AP_PAGE_MENU || ap_validate_menu (info);
}

static void
ap_window_update_navigation (AcctPeriodInfo *info)
{
    gboolean summary = info->current_page == AP_PAGE_SUMMARY;
    gboolean apply = info->current_page == AP_PAGE_APPLY;
    gboolean next = info->current_page < AP_PAGE_APPLY;
    gboolean complete = ap_window_page_is_complete (info, info->current_page);
    GtkWidget *default_widget = NULL;

    gtk_widget_set_visible (info->cancel_button, !summary);
    gtk_widget_set_visible (info->back_button,
                            info->current_page > AP_PAGE_START && !summary);
    gtk_widget_set_sensitive (info->back_button,
                              info->current_page > AP_PAGE_START && !summary);
    gtk_widget_set_visible (info->next_button, next);
    gtk_widget_set_sensitive (info->next_button, next && complete);
    gtk_widget_set_visible (info->apply_button, apply);
    gtk_widget_set_sensitive (info->apply_button, apply);
    gtk_widget_set_visible (info->close_button, summary);
    gtk_widget_set_sensitive (info->close_button, summary);

    if (summary)
        default_widget = info->close_button;
    else if (apply)
        default_widget = info->apply_button;
    else if (next && complete)
        default_widget = info->next_button;
    gtk_window_set_default_widget (info->window, default_widget);
}

static void
ap_window_show_page (AcctPeriodInfo *info, AcctPeriodPage page)
{
    g_return_if_fail (page < AP_PAGE_COUNT);

    info->current_page = page;
    switch (page)
    {
    case AP_PAGE_MENU:
        ap_window_menu_prepare (info);
        break;
    case AP_PAGE_BOOK:
        ap_window_book_prepare (info);
        break;
    case AP_PAGE_APPLY:
        ap_window_apply_prepare (info);
        break;
    case AP_PAGE_SUMMARY:
        ap_window_summary_prepare (info);
        break;
    case AP_PAGE_START:
    case AP_PAGE_COUNT:
        break;
    }
    gtk_stack_set_visible_child (info->stack, info->pages[page]);
    ap_window_update_navigation (info);
}

/* =============================================================== */
static void
ap_window_menu_changed_cb (GtkWidget *widget, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    ENTER ("info=%p", info);
    ap_window_menu_prepare (info);
    ap_window_update_navigation (info);
    (void)widget;
}

/* =============================================================== */
static void
ap_window_finish (AcctPeriodInfo *info)
{
    GtkTextBuffer *buffer;
    GtkTextIter startiter, enditer;
    gint len;
    const char *btitle;
    char *bnotes;

    ENTER ("info=%p", info);

    btitle = gnc_entry_get_text (GTK_ENTRY (info->book_title));
    buffer = gtk_text_view_get_buffer (info->book_notes);
    len = gtk_text_buffer_get_char_count (buffer);
    gtk_text_buffer_get_iter_at_offset (buffer, &startiter, 0);
    gtk_text_buffer_get_iter_at_offset (buffer, &enditer, len);

    bnotes = gtk_text_buffer_get_text (buffer, &startiter, &enditer, FALSE);
    PINFO ("Book title is - %s\n", btitle);
    g_free (bnotes);

    /* Report the status back to the user. */
    info->close_status = 0;  /* XXX fixme success or failure? */

    /* Find the next closing date. */
    info->prev_closing_date = info->closing_date;
    recurrenceListNextInstance (info->period, &info->prev_closing_date,
                                &info->closing_date);

    /* FIXME Test for valid closing date, not sure why it won't be!!! */
    if (g_date_valid (&info->closing_date) &&
        gnc_time (NULL) > gnc_time64_get_day_end_gdate (&info->closing_date))
    {
        /* Load the GUI for the next closing period. */
        gnc_frequency_setup_recurrence (info->period_menu, NULL,
                                        &info->closing_date);
        ap_window_show_page (info, AP_PAGE_MENU);
        return;
    }

    ap_window_show_page (info, AP_PAGE_SUMMARY);
}

/* =============================================================== */
static void
ap_window_back_clicked_cb (GtkButton *button, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    if (info->current_page > AP_PAGE_START &&
        info->current_page < AP_PAGE_SUMMARY)
        ap_window_show_page (info, info->current_page - 1);
    (void)button;
}

static void
ap_window_next_clicked_cb (GtkButton *button, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    if (info->current_page < AP_PAGE_APPLY &&
        ap_window_page_is_complete (info, info->current_page))
        ap_window_show_page (info, info->current_page + 1);
    (void)button;
}

static void
ap_window_apply_clicked_cb (GtkButton *button, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    if (info->current_page == AP_PAGE_APPLY)
        ap_window_finish (info);
    (void)button;
}

static void
ap_window_cancel_clicked_cb (GtkButton *button, gpointer user_data)
{
    ap_window_cancel (user_data);
    (void)button;
}

static gboolean
ap_window_close_request_cb (GtkWindow *window, gpointer user_data)
{
    ap_window_cancel (user_data);
    (void)window;
    return TRUE;
}

static gboolean
ap_window_escape_shortcut_cb (GtkWidget *widget, GVariant *args,
                              gpointer user_data)
{
    ap_window_cancel (user_data);
    (void)widget;
    (void)args;
    return TRUE;
}

static gboolean
ap_window_back_shortcut_cb (GtkWidget *widget, GVariant *args,
                            gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    if (info->current_page > AP_PAGE_START &&
        info->current_page < AP_PAGE_SUMMARY)
        ap_window_show_page (info, info->current_page - 1);
    (void)widget;
    (void)args;
    return TRUE;
}

static gboolean
ap_window_next_shortcut_cb (GtkWidget *widget, GVariant *args,
                            gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    if (info->current_page < AP_PAGE_APPLY &&
        ap_window_page_is_complete (info, info->current_page))
        ap_window_show_page (info, info->current_page + 1);
    (void)widget;
    (void)args;
    return TRUE;
}

static void
ap_window_add_shortcuts (AcctPeriodInfo *info)
{
    GtkShortcutController *controller = GTK_SHORTCUT_CONTROLLER (
        gtk_shortcut_controller_new ());

    gtk_shortcut_controller_set_scope (controller, GTK_SHORTCUT_SCOPE_MANAGED);
    gtk_shortcut_controller_add_shortcut (
        controller,
        gtk_shortcut_new (gtk_keyval_trigger_new (GDK_KEY_Escape, 0),
                          gtk_callback_action_new (ap_window_escape_shortcut_cb,
                                                   info, NULL)));
    gtk_shortcut_controller_add_shortcut (
        controller,
        gtk_shortcut_new (gtk_keyval_trigger_new (GDK_KEY_Left, GDK_ALT_MASK),
                          gtk_callback_action_new (ap_window_back_shortcut_cb,
                                                   info, NULL)));
    gtk_shortcut_controller_add_shortcut (
        controller,
        gtk_shortcut_new (gtk_keyval_trigger_new (GDK_KEY_Right, GDK_ALT_MASK),
                          gtk_callback_action_new (ap_window_next_shortcut_cb,
                                                   info, NULL)));
    gtk_widget_add_controller (GTK_WIDGET (info->window),
                               GTK_EVENT_CONTROLLER (controller));
}

/* =============================================================== */
static GtkWidget *
ap_window_create (AcctPeriodInfo *info)
{
    GtkBuilder *builder;
    GtkWidget *box;
    gchar *earliest_str;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "assistant-acct-period.glade",
                               "account_period_window");
    info->window = GTK_WINDOW (gtk_builder_get_object (builder,
                                                        "account_period_window"));
    info->stack = GTK_STACK (gtk_builder_get_object (builder,
                                                     "account_period_stack"));
    info->pages[AP_PAGE_START] = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                       "start_page"));
    info->pages[AP_PAGE_MENU] = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                      "menu_page"));
    info->pages[AP_PAGE_BOOK] = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                      "book_page"));
    info->pages[AP_PAGE_APPLY] = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                       "finish_page"));
    info->pages[AP_PAGE_SUMMARY] = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                         "summary_page"));
    info->back_button = GTK_WIDGET (gtk_builder_get_object (builder,
                                                              "account_period_back"));
    info->next_button = GTK_WIDGET (gtk_builder_get_object (builder,
                                                              "account_period_next"));
    info->apply_button = GTK_WIDGET (gtk_builder_get_object (builder,
                                                               "account_period_apply"));
    info->cancel_button = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                "account_period_cancel"));
    info->close_button = GTK_WIDGET (gtk_builder_get_object (builder,
                                                               "account_period_close"));
    info->period_remarks = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                 "remarks_label"));
    info->close_results = GTK_WIDGET (gtk_builder_get_object (builder,
                                                                "results_label"));
    info->book_details = GTK_WIDGET (gtk_builder_get_object (builder,
                                                               "book_label"));
    info->book_title = GTK_WIDGET (gtk_builder_get_object (builder,
                                                             "book_title_entry"));
    info->book_notes = GTK_TEXT_VIEW (gtk_builder_get_object (builder,
                                                                "book_notes_view"));
    info->apply_label = GTK_WIDGET (gtk_builder_get_object (builder,
                                                              "finish_page"));
    info->summary = GTK_WIDGET (gtk_builder_get_object (builder,
                                                          "summary_label"));

    if (!info->window || !info->stack || !info->pages[AP_PAGE_START] ||
        !info->pages[AP_PAGE_MENU] || !info->pages[AP_PAGE_BOOK] ||
        !info->pages[AP_PAGE_APPLY] || !info->pages[AP_PAGE_SUMMARY] ||
        !info->back_button || !info->next_button || !info->apply_button ||
        !info->cancel_button || !info->close_button || !info->period_remarks ||
        !info->close_results || !info->book_details || !info->book_title ||
        !info->book_notes || !info->apply_label || !info->summary)
    {
        PWARN ("The account period window is incomplete");
        g_object_unref (builder);
        return NULL;
    }

    gtk_widget_set_name (GTK_WIDGET (info->window),
                         "gnc-id-assistant-account-period");
    gtk_window_set_modal (info->window, TRUE);
    gtk_entry_set_activates_default (GTK_ENTRY (info->book_title), TRUE);

    info->close_status = -1;

    /* Find the earliest transaction date and make the first close date a year later. */
    info->earliest = get_earliest_in_book (gnc_get_current_book ());
    info->earliest_str = qof_print_date (info->earliest);
    earliest_str = gnc_ctime (&info->earliest);
    PINFO ("date of earliest transaction is %" G_GINT64_FORMAT " %s",
           info->earliest, earliest_str);
    g_free (earliest_str);

    g_date_clear (&info->closing_date, 1);
    gnc_gdate_set_time64 (&info->closing_date, info->earliest);
    g_date_clear (&info->prev_closing_date, 1);
    info->prev_closing_date = info->closing_date;
    g_date_add_years (&info->closing_date, 1);

    {
        Recurrence *r = g_new0 (Recurrence, 1);
        recurrenceSet (r, 1, PERIOD_MONTH, &info->closing_date,
                       WEEKEND_ADJ_NONE);
        info->period = g_list_append (NULL, r);
    }

    info->period_menu = GNC_FREQUENCY (
        gnc_frequency_new_from_recurrence (info->period, &info->closing_date));
    gnc_frequency_set_frequency_label_text (info->period_menu, _("Period"));
    gnc_frequency_set_date_label_text (info->period_menu, _("Closing Date"));

    box = GTK_WIDGET (gtk_builder_get_object (builder, "period_hbox"));
    gtk_box_append (GTK_BOX (box), GTK_WIDGET (info->period_menu));
    g_signal_connect (info->period_menu, "changed",
                      G_CALLBACK (ap_window_menu_changed_cb), info);

    g_signal_connect (info->back_button, "clicked",
                      G_CALLBACK (ap_window_back_clicked_cb), info);
    g_signal_connect (info->next_button, "clicked",
                      G_CALLBACK (ap_window_next_clicked_cb), info);
    g_signal_connect (info->apply_button, "clicked",
                      G_CALLBACK (ap_window_apply_clicked_cb), info);
    g_signal_connect (info->cancel_button, "clicked",
                      G_CALLBACK (ap_window_cancel_clicked_cb), info);
    g_signal_connect (info->close_button, "clicked",
                      G_CALLBACK (ap_window_cancel_clicked_cb), info);
    g_signal_connect (info->window, "close-request",
                      G_CALLBACK (ap_window_close_request_cb), info);
    g_signal_connect (info->window, "destroy",
                      G_CALLBACK (ap_window_destroy_cb), info);
    ap_window_add_shortcuts (info);

    info->current_page = AP_PAGE_START;
    ap_window_show_page (info, AP_PAGE_START);
    g_object_unref (builder);
    return GTK_WIDGET (info->window);
}

/* =============================================================== */
static void
ap_close_handler (gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    if (info->window)
        gtk_window_destroy (info->window);
}

/********************************************************************\
 * gnc_acct_period_dialog                                           *
 *   opens a window to configure accounting periods                 *
 *                                                                  *
 * Args:   none                                                     *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_acct_period_dialog (void)
{
    AcctPeriodInfo *info = g_new0 (AcctPeriodInfo, 1);

    if (!ap_window_create (info))
    {
        recurrenceListFree (&info->period);
        g_free (info->earliest_str);
        g_free (info);
        return;
    }

    gnc_register_gui_component (ASSISTANT_ACCT_PERIOD_CM_CLASS,
                                NULL, ap_close_handler, info);
    gnc_window_adjust_for_screen (info->window);
    gtk_window_present (info->window);
}
