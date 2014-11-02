/********************************************************************\
 * assistant-acct-period.c - accouting period assistant for GnuCash *
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
 *     #define CLOSE_BOOKS_ACTUALLY_WORKS                                *
 *                                                                       *
 * Add the following to gnc-plugin-basic-commands-ui.xml on line 43      *
 * <menuitem name="ActionsCloseBooks" action="ActionsCloseBooksAction"/> *
\*************************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Recurrence.h"
#include "Query.h"
#include "Scrub.h"
#include "Transaction.h"
#include "dialog-utils.h"
#include "assistant-acct-period.h"
#include "assistant-utils.h"
#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-file.h"
#include "gnc-frequency.h"
#include "gnc-gdate-utils.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "misc-gnome-utils.h"
#include "gnc-session.h"

#define ASSISTANT_ACCT_PERIOD_CM_CLASS "assistant-acct-period"

static QofLogModule log_module = GNC_MOD_ASSISTANT;

/** structures *********************************************************/
typedef struct
{
    GtkWidget * window;
    GtkWidget * assistant;
    GncFrequency *period_menu;
    GtkWidget  * period_remarks;
    GtkWidget  * close_results;
    GtkWidget  * book_details;
    GtkWidget  * book_title;
    GtkTextView  * book_notes;
    GtkWidget  * apply_label;
    GtkWidget  * summary;

    time64 earliest;
    char * earliest_str;
    GDate closing_date;
    GDate prev_closing_date;
    GList *period;
    int close_status;

} AcctPeriodInfo;

/* =============================================================== */

void     ap_assistant_prepare           (GtkAssistant  *assistant, GtkWidget *page,
        gpointer user_data);
void     ap_assistant_menu_prepare      (GtkAssistant *assistant, gpointer user_data);
void	 ap_assistant_book_prepare 	(GtkAssistant *assistant, gpointer user_data);
void	 ap_assistant_apply_prepare 	(GtkAssistant *assistant, gpointer user_data);
void	 ap_assistant_summary_prepare   (GtkAssistant *assistant, gpointer user_data);

gboolean ap_validate_menu 		(GtkAssistant *assistant, gpointer user_data);

void     ap_assistant_finish            (GtkAssistant *gtkassistant, gpointer user_data);
void     ap_assistant_cancel            (GtkAssistant *gtkassistant, gpointer user_data);
void     ap_assistant_close             (GtkAssistant *gtkassistant, gpointer user_data);

/* =============================================================== */
/* Find the earliest date occuring in the book.  Do this by making
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

    q = qof_query_create_for(GNC_ID_SPLIT);
    qof_query_set_max_results(q, 1);
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
    {
        earliest = xaccQueryGetEarliestDateFound (q);
    }
    else
    {
        /* If no results, we don't want to bomb totally */
        earliest = gnc_time (NULL);
    }

    qof_query_destroy (q);
    return earliest;
}

/* =============================================================== */
/* Find the number of transactions occuring before the indicated date.
 * Do this by making a query and counting the results.
 */

static int
get_num_xactions_before_date(QofBook *book, time64 close_date)
{
    QofQuery *q;
    GSList *param;
    QofQueryPredData *pred;
    Timespec ts;
    GList *res, *n;
    int cnt = 0;

    q = qof_query_create_for(GNC_ID_TRANS);
    qof_query_set_max_results(q, -1);
    qof_query_set_book (q, book);

    /* Look for transactions earlier than the closing date */
    param = g_slist_prepend (NULL, TRANS_DATE_POSTED);
    timespecFromTime64 (&ts, close_date);
    pred = qof_query_date_predicate (QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, ts);
    qof_query_add_term (q,  param, pred, QOF_QUERY_FIRST_TERM);

    /* Run the query, find how many transactions there are */
    res = qof_query_run (q);

    cnt = 0;
    for (n = res; n; n = n->next) cnt ++;

    qof_query_destroy (q);
    return cnt;
}

/* =============================================================== */

static const char *
get_close_status_str (AcctPeriodInfo *info)
{
    const char * str;

    /* Tell user about how the previous book closing went. */
    switch (info->close_status)
    {
    case -1:
        str = "";
        break;
    case 0:
        str = _("The book was closed successfully.");
        break;
    default:
        str = "";
        break;
    }
    return str;
}

/* =============================================================== */

static void
ap_assistant_destroy_cb (GtkObject *object, gpointer data)
{
    AcctPeriodInfo *info = data;

    gnc_unregister_gui_component_by_data (ASSISTANT_ACCT_PERIOD_CM_CLASS, info);

    // do we need gnc_frequency_destroy or is this automatic ??
    recurrenceListFree(&info->period);
    g_free (info->earliest_str);
    g_free (info);
}

/* =============================================================== */

void
ap_assistant_cancel (GtkAssistant *assistant, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_ACCT_PERIOD_CM_CLASS, info);
}

/* =============================================================== */

void
ap_assistant_close (GtkAssistant *assistant, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;
    gnc_close_gui_component_by_data (ASSISTANT_ACCT_PERIOD_CM_CLASS, info);
}

/* =============================================================== */

void
ap_assistant_prepare (GtkAssistant *assistant, GtkWidget *page,
                      gpointer user_data)
{
    gint currentpage = gtk_assistant_get_current_page(assistant);

    switch (currentpage)
    {
    case 1:
        /* Current page is Menu page */
        ap_assistant_menu_prepare(assistant, user_data);
        break;
    case 2:
        /* Current page is Book page */
        ap_assistant_book_prepare (assistant, user_data);
        break;
    case 3:
        /* Current page is Apply page */
        ap_assistant_apply_prepare (assistant, user_data);
        break;
    case 4:
        /* Current page is Summary page */
        ap_assistant_summary_prepare (assistant, user_data);
        break;
    }
}

/* =============================================================== */

void
ap_assistant_menu_prepare (GtkAssistant *assistant, gpointer user_data)
{
    int nperiods;
    GDate period_begin, period_end, date_now;
    char * str;

    AcctPeriodInfo *info = user_data;

    ENTER ("info=%p", info);

    /* Pull info from widget, push into freq spec */
    //gnc_frequency_save_state (info->period_menu, info->period, &info->closing_date);
    recurrenceListFree(&info->period);
    gnc_frequency_save_to_recurrence(info->period_menu, &info->period, &info->closing_date);

    /* Count the number of periods that would be generated. */
    g_date_clear (&period_begin, 1);
    g_date_clear (&period_end, 1);
    g_date_clear (&date_now, 1);
    nperiods = 0;
    period_end = info->closing_date;
    gnc_gdate_set_time64 (&date_now, gnc_time (NULL));

    while (0 > g_date_compare(&period_end, &date_now ))
    {
        nperiods ++;
        PINFO ("Period = %d and End date is %d/%d/%d", nperiods,
               g_date_get_month(&period_end),
               g_date_get_day(&period_end),
               g_date_get_year(&period_end));
        period_begin = period_end;
        recurrenceListNextInstance(info->period, &period_begin, &period_end);

        /* FIXME Check for valid period_end, not sure why it wont be!!! */
        if (g_date_valid (&period_end) != TRUE)
            break;
    }

    /* Find the date of the earliest transaction in the current book.
     * Note that this could have changed since last time, since
     * we may have closed books since last time. */
    info->earliest = get_earliest_in_book (gnc_get_current_book());
    info->earliest_str = qof_print_date(info->earliest);
    PINFO ("Date of earliest transaction is %" G_GINT64_FORMAT " %s",
	   info->earliest, gnc_ctime (&info->earliest));

    /* Display the results */
    str = g_strdup_printf (
              /* Translators: %s is a date string. %d is the number of books
               * that will be created. This is a ngettext(3) message (but
               * only for the %d part). */
              ngettext("The earliest transaction date found in this book is %s. "
                       "Based on the selection made above, this book will be split "
                       "into %d book.",
                       "The earliest transaction date found in this book is %s. "
                       "Based on the selection made above, this book will be split "
                       "into %d books.",
                       nperiods),
              info->earliest_str,
              nperiods);
    gtk_label_set_text (GTK_LABEL(info->period_remarks), str);
    g_free (str);
}

/* =============================================================== */

void
ap_assistant_book_prepare (GtkAssistant *assistant, gpointer user_data)
{
    QofBook *currbook;
    char close_date_str[MAX_DATE_LENGTH];
    char prev_close_date_str[MAX_DATE_LENGTH];
    const char *period_text;
    char *str;
    const char *cstr;
    int ntrans, nacc;
    GtkTextBuffer *buffer;

    AcctPeriodInfo *info = user_data;

    ENTER ("info=%p", info);

    /* Tell user about how the previous book closing went. */
    cstr = get_close_status_str (info);
    gtk_label_set_text (GTK_LABEL(info->close_results), cstr);
    info->close_status = -1;

    /* Pull info from widget, push into freq spec */
    //gnc_frequency_save_state (info->period_menu, info->period, &info->closing_date);
    recurrenceListFree(&info->period);
    gnc_frequency_save_to_recurrence(info->period_menu, &info->period, &info->closing_date);

    qof_print_date_dmy_buff (close_date_str, MAX_DATE_LENGTH,
                             g_date_get_day(&info->closing_date),
                             g_date_get_month(&info->closing_date),
                             g_date_get_year(&info->closing_date));

    currbook = gnc_get_current_book();
    ntrans = get_num_xactions_before_date(currbook,
                                          gnc_time64_get_day_end_gdate (&info->closing_date));

    nacc = gnc_account_n_descendants (gnc_book_get_root_account (currbook));

    /* Display the book info */
    period_text =
        _("You have asked for a book to be created. This book "
          "will contain all transactions up to midnight %s "
          "(for a total of %d transactions spread over %d accounts).\n\n "
          "Amend the Title and Notes or Click on 'Forward' to proceed.\n "
          "Click on 'Back' to adjust the dates or 'Cancel'.");
    str = g_strdup_printf (period_text, close_date_str, ntrans, nacc);
    gtk_label_set_text (GTK_LABEL(info->book_details), str);
    g_free (str);

    gtk_widget_show (GTK_WIDGET (info->book_details));

    /* Create default settings for the title, notes fields */
    qof_print_date_dmy_buff (prev_close_date_str, MAX_DATE_LENGTH,
                             g_date_get_day(&info->prev_closing_date),
                             g_date_get_month(&info->prev_closing_date),
                             g_date_get_year(&info->prev_closing_date));

    str = g_strdup_printf (_("Period %s - %s"), prev_close_date_str, close_date_str);
    gtk_entry_set_text (GTK_ENTRY(info->book_title), str);

    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(info->book_notes));
    gtk_text_buffer_set_text(buffer, str, -1);

    g_free (str);
}

/* =============================================================== */

void
ap_assistant_apply_prepare (GtkAssistant *assistant, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;
    const char *btitle;
    char *str;
    const char *apply_text =
        _("The book will be created with the title %s when you "
          "click on 'Apply'. Click on 'Back' to adjust, "
	  "or 'Cancel' to not create any book.");

    btitle = gtk_entry_get_text (GTK_ENTRY(info->book_title));
    str = g_strdup_printf (apply_text, btitle);
    gtk_label_set_text (GTK_LABEL(info->apply_label), str);
    g_free (str);
}

/* =============================================================== */

static void
ap_assistant_menu_changed_cb (GtkWidget *widget, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;
    GtkAssistant *assistant = GTK_ASSISTANT(info->window);
    gint num = gtk_assistant_get_current_page (assistant);
    GtkWidget *page = gtk_assistant_get_nth_page (assistant, num);

    ENTER ("info=%p", info);
    ap_assistant_menu_prepare (assistant, info);
    gtk_assistant_set_page_complete (assistant, page, ap_validate_menu (assistant, user_data));
}

/* =============================================================== */

gboolean
ap_validate_menu (GtkAssistant *assistant, gpointer user_data)
{
    GDate date_now;
    AcctPeriodInfo *info = user_data;
    ENTER("info=%p", info);

    /* Pull info from widget, push into freq spec */
    //gnc_frequency_save_state (info->period_menu, info->period, &info->closing_date);
    recurrenceListFree(&info->period);
    gnc_frequency_save_to_recurrence(info->period_menu, &info->period, &info->closing_date);

    if (0 <= g_date_compare(&info->prev_closing_date, &info->closing_date))
    {
        /* Closing date must be greater than closing date of previous book */
        return FALSE;
    }

    g_date_clear (&date_now, 1);
    gnc_gdate_set_today (&date_now);
    if (0 < g_date_compare(&info->closing_date, &date_now))
    {
        /* Closing date must be in the future */
        return FALSE;
    }
    return TRUE;
}

/* =============================================================== */

void
ap_assistant_finish (GtkAssistant *assistant, gpointer user_data)
{
    AcctPeriodInfo *info = user_data;
    GtkTextBuffer * buffer;
    GtkTextIter startiter, enditer;
    gint len;
    const char *btitle;
    char *bnotes;
    Timespec closing_date;

    ENTER("info=%p", info);

    btitle = gtk_entry_get_text (GTK_ENTRY(info->book_title));
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(info->book_notes));
    len = gtk_text_buffer_get_char_count (buffer);
    gtk_text_buffer_get_iter_at_offset(buffer, &startiter, 0);
    gtk_text_buffer_get_iter_at_offset(buffer, &enditer, len);

    bnotes = gtk_text_buffer_get_text(buffer, &startiter, &enditer , 0);
    PINFO("Book title is - %s\n", btitle);

    timespecFromTime64 (&closing_date,
                        gnc_time64_get_day_end_gdate (&info->closing_date));
    g_free(bnotes);

    /* Report the status back to the user. */
    info->close_status = 0;  /* XXX fixme success or failure? */

    /* Find the next closing date ... */
    info->prev_closing_date = info->closing_date;
    recurrenceListNextInstance (info->period, &info->prev_closing_date,
				&info->closing_date);


    /* FIXME Test for valid closing date, not sure why it wont be!!! */
    if (g_date_valid(&info->closing_date) == TRUE)
    {
        /* If the next closing date is in the future, then we are done. */
        if (gnc_time (NULL) >
	    gnc_time64_get_day_end_gdate (&info->closing_date))
        {
            /* Load up the GUI for the next closing period. */
            gnc_frequency_setup_recurrence (info->period_menu, NULL,
					    &info->closing_date);
            /* Jump back to the Close Book page. */
            gtk_assistant_set_current_page (GTK_ASSISTANT(info->window), 1);
        }
    }
}

/* =============================================================== */

void
ap_assistant_summary_prepare (GtkAssistant *assistant, gpointer user_data)
{
    const char *msg;
    char *str;
    AcctPeriodInfo *info = user_data;
    ENTER ("info=%p", info);

    /* Translation FIXME: Can this %s-containing message please be
       replaced by one single message? Either this closing went
       successfully ("success", "congratulations") or something else
       should be displayed anyway. */
    msg = _("%s\nCongratulations! You are done closing books!\n");

    str = g_strdup_printf (msg, get_close_status_str (info));
    gtk_label_set_text (GTK_LABEL(info->summary), str);
    g_free (str);

}

/* =============================================================== */

static GtkWidget *
ap_assistant_create (AcctPeriodInfo *info)
{
    GtkBuilder *builder;
    GtkWidget *window;
    GtkWidget *box;

    builder = gtk_builder_new();
    gnc_builder_add_from_file  (builder , "assistant-acct-period.glade", "Account Period Assistant");
    window = GTK_WIDGET(gtk_builder_get_object (builder, "Account Period Assistant"));
    info->window = window;

    /* Set the assistant colors */
    gnc_assistant_set_colors (GTK_ASSISTANT (info->window));

    /* Enable all pages except menu page. */
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "start_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "book_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "finish_page")),
                                     TRUE);
    gtk_assistant_set_page_complete (GTK_ASSISTANT (window),
                                     GTK_WIDGET(gtk_builder_get_object(builder, "summary_page")),
                                     TRUE);

    info->close_status = -1;

    /* Find the date of the earliest transaction in the book.
     * Add a year minus a day as the first guess for book closing,
     * and use that to set up the freq spec widget. */
    info->earliest = get_earliest_in_book (gnc_get_current_book());
    info->earliest_str = qof_print_date(info->earliest);
    PINFO ("date of earliest transaction is %" G_GINT64_FORMAT " %s",
           info->earliest, gnc_ctime (&info->earliest));

    g_date_clear (&info->closing_date, 1);
    gnc_gdate_set_time64 (&info->closing_date, info->earliest);
    g_date_clear (&info->prev_closing_date, 1);
    info->prev_closing_date = info->closing_date;
    g_date_add_years (&info->closing_date, 1);

    {
        Recurrence *r = g_new0(Recurrence, 1);
        recurrenceSet(r, 1, PERIOD_MONTH, &info->closing_date, WEEKEND_ADJ_NONE);
        info->period = NULL;
        info->period = g_list_append(info->period, r);
    }

    info->period_menu = GNC_FREQUENCY(
                            gnc_frequency_new_from_recurrence(info->period, &info->closing_date));

    /* Change the text so that its more mainingful for this assistant */
    gnc_frequency_set_frequency_label_text(info->period_menu, _("Period:"));
    gnc_frequency_set_date_label_text(info->period_menu, _("Closing Date:"));

    /* Reparent to the correct location */

    box = GTK_WIDGET(gtk_builder_get_object(builder, "period_hbox"));
    gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET (info->period_menu), TRUE, TRUE, 0);
    g_signal_connect (info->period_menu, "changed",
                      G_CALLBACK (ap_assistant_menu_changed_cb), info);

    /* Get handles to all of the other widgets we'll need */
    info->period_remarks = GTK_WIDGET(gtk_builder_get_object(builder, "remarks_label"));

    info->close_results = GTK_WIDGET(gtk_builder_get_object(builder, "results_label"));

    info->book_details = GTK_WIDGET(gtk_builder_get_object(builder, "book_label"));

    info->book_title = GTK_WIDGET(gtk_builder_get_object(builder, "book_title_entry"));

    info->book_notes = GTK_TEXT_VIEW(gtk_builder_get_object(builder, "book_notes_view"));

    info->apply_label = GTK_WIDGET(gtk_builder_get_object(builder, "finish_page"));

    info->summary = GTK_WIDGET(gtk_builder_get_object(builder, "summary_label"));

    g_signal_connect (G_OBJECT(window), "destroy",
                      G_CALLBACK (ap_assistant_destroy_cb), info);

    gtk_builder_connect_signals(builder, info);
    g_object_unref(G_OBJECT(builder));
    return window;
}

/* =============================================================== */

static void
ap_close_handler (gpointer user_data)
{
    AcctPeriodInfo *info = user_data;

    gtk_widget_destroy (info->window);
}

/********************************************************************\
 * gnc_acct_period_dialog                                           *
 *   opens up a assistant to configure accounting periods           *
 *                                                                  *
 * Args:   none                                                     *
 * Return: nothing                                                  *
\********************************************************************/

void
gnc_acct_period_dialog (void)
{
    AcctPeriodInfo *info;

    info = g_new0 (AcctPeriodInfo, 1);

    ap_assistant_create (info);

    gnc_register_gui_component (ASSISTANT_ACCT_PERIOD_CM_CLASS,
				NULL, ap_close_handler, info);

    gtk_widget_show_all (info->window);

    gnc_window_adjust_for_screen (GTK_WINDOW(info->window));
}
