/*
 * dialog-date-close.c -- Dialog to ask a question and request a date
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>

#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "qof.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-date-edit.h"
#include "gnc-account-sel.h"

#include "business-gnome-utils.h"
#include "dialog-date-close.h"

typedef struct _dialog_date_close_window
{
    GtkWidget *dialog;
    GtkWidget *date;
    GtkWidget *post_date;
    GtkWidget *acct_combo;
    GtkWidget *memo_entry;
    GtkWidget *question_check;
    GncBillTerm *terms;
    time64 *t, *t2;
    GList * acct_types;
    GList * acct_commodities;
    QofBook *book;
    Account *acct;
    char **memo;
    gboolean retval;
    gboolean answer;
} DialogDateClose;

void gnc_dialog_date_close_ok_cb (GtkWidget *widget, gpointer user_data);


void
gnc_dialog_date_close_ok_cb (GtkWidget *widget, gpointer user_data)
{
    DialogDateClose *ddc = user_data;

    if (ddc->acct_combo)
    {
        Account *acc;

        acc = gnc_account_sel_get_account( GNC_ACCOUNT_SEL(ddc->acct_combo) );

        if (!acc)
        {
            gnc_error_dialog (GTK_WINDOW (ddc->dialog), "%s",
                              _("No Account selected. Please try again."));
            return;
        }

        if (xaccAccountGetPlaceholder (acc))
        {
            gnc_error_dialog (GTK_WINDOW (ddc->dialog), "%s",
                              _("Placeholder account selected. Please try again."));
            return;
        }

        ddc->acct = acc;
    }

    if (ddc->post_date)
        *ddc->t2 = gnc_date_edit_get_date (GNC_DATE_EDIT (ddc->post_date));

    if (ddc->date)
    {
        if (ddc->terms)
            *ddc->t = gncBillTermComputeDueDate (ddc->terms, *ddc->t2);
        else
            *ddc->t = gnc_date_edit_get_date (GNC_DATE_EDIT (ddc->date));
    }

    if (ddc->memo_entry && ddc->memo)
        *(ddc->memo) = gtk_editable_get_chars (GTK_EDITABLE (ddc->memo_entry),
                                               0, -1);
    if (ddc->question_check)
        ddc->answer = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ddc->question_check));
    ddc->retval = TRUE;
}

static void
fill_in_acct_info (DialogDateClose *ddc, gboolean set_default_acct)
{
    GNCAccountSel *gas = GNC_ACCOUNT_SEL (ddc->acct_combo);

    /* How do I set the book? */
    gnc_account_sel_set_acct_filters( gas, ddc->acct_types, ddc->acct_commodities );
    gnc_account_sel_set_new_account_ability( gas, TRUE );
    gnc_account_sel_set_new_account_modal( gas, TRUE );
    gnc_account_sel_set_account( gas, ddc->acct, set_default_acct );
}

gboolean
gnc_dialog_date_close_parented (GtkWidget *parent, const char *message,
                                const char *label_message,
                                gboolean ok_is_default,
                                /* Returned data ... */
                                time64 *t)
{
    DialogDateClose *ddc;
    GtkWidget *date_box;
    GtkLabel *label;
    GtkBuilder *builder;
    gboolean retval;

    if (!message || !label_message || !t)
        return FALSE;

    ddc = g_new0 (DialogDateClose, 1);
    ddc->t = t;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-date-close.glade", "date_close_dialog");
    ddc->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "date_close_dialog"));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(ddc->dialog), "GncDateCloseDialog");

    date_box = GTK_WIDGET(gtk_builder_get_object (builder, "date_box"));
    ddc->date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX(date_box), ddc->date, TRUE, TRUE, 0);
    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), *t);

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(ddc->dialog), GTK_WINDOW(parent));

    /* Set the labels */
    label = GTK_LABEL (gtk_builder_get_object (builder, "msg_label"));
    gtk_label_set_text (label, message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "label"));
    gtk_label_set_text (label, label_message);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ddc);

    gtk_widget_show_all (ddc->dialog);

    ddc->retval = FALSE;
    while (gtk_dialog_run (GTK_DIALOG (ddc->dialog)) == GTK_RESPONSE_OK)
    {
        /* If response is OK but flag is not set, try again */
        if (ddc->retval)
            break;
    }

    g_object_unref(G_OBJECT(builder));

    gtk_widget_destroy(ddc->dialog);
    retval = ddc->retval;
    g_list_free (ddc->acct_types);
    g_free (ddc);

    return retval;
}

static void
post_date_changed_cb (GNCDateEdit *gde, gpointer d)
{
    DialogDateClose *ddc = d;
    time64 post_date;
    time64 due_date = 0;

    post_date = gnc_date_edit_get_date (gde);
    due_date = gncBillTermComputeDueDate (ddc->terms, post_date);
    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), due_date);
}

gboolean
gnc_dialog_dates_acct_question_parented (GtkWidget *parent, const char *message,
        const char *ddue_label_message,
        const char *post_label_message,
        const char *acct_label_message,
        const char *question_check_message,
        gboolean ok_is_default,
        gboolean set_default_acct,
        GList * acct_types, GList * acct_commodities,
        QofBook *book, GncBillTerm *terms,
        /* Returned Data... */
        time64 *ddue, time64 *post,
        char **memo, Account **acct, gboolean *answer)
{
    DialogDateClose *ddc;
    GtkLabel *label;
    GtkWidget *date_box;
    GtkWidget *acct_box;
    GtkBuilder *builder;
    gboolean retval;

    if (!message || !ddue_label_message || !post_label_message ||
            !acct_label_message || !acct_types || !book || !ddue || !post || !acct)
        return FALSE;
    if (question_check_message && !answer)
        return FALSE;

    ddc = g_new0 (DialogDateClose, 1);
    ddc->t = ddue;
    ddc->t2 = post;
    ddc->book = book;
    ddc->acct_types = acct_types;
    ddc->acct_commodities = acct_commodities;
    ddc->acct = *acct;
    ddc->memo = memo;
    ddc->terms = terms;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-date-close.glade", "date_account_dialog");
    ddc->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "date_account_dialog"));
    ddc->memo_entry = GTK_WIDGET(gtk_builder_get_object (builder, "memo_entry"));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(ddc->dialog), "GncDateCloseDialog");

    acct_box = GTK_WIDGET(gtk_builder_get_object (builder, "acct_hbox"));
    ddc->acct_combo = gnc_account_sel_new();
    gtk_box_pack_start (GTK_BOX(acct_box), ddc->acct_combo, TRUE, TRUE, 0);

    date_box = GTK_WIDGET(gtk_builder_get_object (builder, "date_hbox"));
    ddc->date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX(date_box), ddc->date, TRUE, TRUE, 0);

    date_box = GTK_WIDGET(gtk_builder_get_object (builder, "post_date_box"));
    ddc->post_date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX(date_box), ddc->post_date, TRUE, TRUE, 0);

    ddc->question_check = GTK_WIDGET(gtk_builder_get_object (builder, "question_check"));

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(ddc->dialog), GTK_WINDOW(parent));


    /* Set the labels */
    label = GTK_LABEL (gtk_builder_get_object (builder, "top_msg_label"));
    gtk_label_set_text (label, message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "date_label"));
    gtk_label_set_text (label, ddue_label_message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "postdate_label"));
    gtk_label_set_text (label, post_label_message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "acct_label"));
    gtk_label_set_text (label, acct_label_message);

    if (question_check_message)
    {
        gtk_label_set_text(GTK_LABEL(gtk_bin_get_child (GTK_BIN(ddc->question_check))), question_check_message);
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ddc->question_check), *answer);
    }
    else
    {
        gtk_widget_hide(ddc->question_check);
        gtk_widget_hide(GTK_WIDGET(gtk_builder_get_object (builder, "hide1")));
    }


    /* Set the post date widget */
    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->post_date), *post);

    /* Deal with the terms handling of the due date */
    if (terms)
    {
        g_signal_connect (G_OBJECT (ddc->post_date), "date_changed",
                          G_CALLBACK (post_date_changed_cb), ddc);
        gtk_widget_set_sensitive (ddc->date, FALSE);
        post_date_changed_cb (GNC_DATE_EDIT (ddc->post_date), ddc);
    }
    else
        gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), *ddue);

    /* Setup the account widget */
    fill_in_acct_info (ddc, set_default_acct);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ddc);

    gtk_widget_show_all (ddc->dialog);

    /* Set the focus on the date widget */
    gnc_date_grab_focus (GNC_DATE_EDIT (ddc->post_date));

    ddc->retval = FALSE;
    while (gtk_dialog_run (GTK_DIALOG (ddc->dialog)) == GTK_RESPONSE_OK)
    {
        /* If response is OK but flag is not set, try again */
        if (ddc->retval)
            break;
    }

    g_object_unref(G_OBJECT(builder));

    gtk_widget_destroy(ddc->dialog);
    retval = ddc->retval;
    *acct = ddc->acct;
    if (question_check_message)
        *answer = ddc->answer;
    g_free (ddc);

    return retval;
}

gboolean
gnc_dialog_date_acct_parented (GtkWidget *parent, const char *message,
                               const char *date_label_message,
                               const char *acct_label_message,
                               gboolean ok_is_default,
                               GList * acct_types, QofBook *book,
                               /* Returned Data... */
                               time64 *date, Account **acct)
{
    DialogDateClose *ddc;
    GtkLabel *label;
    GtkWidget *date_box;
    GtkWidget *acct_box;
    GtkBuilder *builder;
    gboolean retval;

    if (!message || !date_label_message || !acct_label_message ||
            !acct_types || !book || !date || !acct)
        return FALSE;

    ddc = g_new0 (DialogDateClose, 1);
    ddc->t = date;
    ddc->book = book;
    ddc->acct_types = acct_types;
    ddc->acct = *acct;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-date-close.glade", "date_account_dialog");
    ddc->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "date_account_dialog"));

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(ddc->dialog), "GncDateCloseDialog");

    acct_box = GTK_WIDGET(gtk_builder_get_object (builder, "acct_hbox"));
    ddc->acct_combo = gnc_account_sel_new();
    if (*acct)
        gnc_account_sel_set_account (GNC_ACCOUNT_SEL(ddc->acct_combo), *acct, FALSE);
    gtk_box_pack_start (GTK_BOX(acct_box), ddc->acct_combo, TRUE, TRUE, 0);

    date_box = GTK_WIDGET(gtk_builder_get_object (builder, "date_hbox"));
    ddc->date = gnc_date_edit_new (time(NULL), FALSE, FALSE);
    gtk_box_pack_start (GTK_BOX(date_box), ddc->date, TRUE, TRUE, 0);

    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW(ddc->dialog), GTK_WINDOW(parent));


    /* Set the labels */
    label = GTK_LABEL (gtk_builder_get_object (builder, "top_msg_label"));
    gtk_label_set_text (label, message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "date_label"));
    gtk_label_set_text (label, date_label_message);
    label = GTK_LABEL (gtk_builder_get_object (builder, "acct_label"));
    gtk_label_set_text (label, acct_label_message);

    /* Set the date widget */
    gnc_date_edit_set_time (GNC_DATE_EDIT (ddc->date), *date);

    /* Setup the account widget */
    fill_in_acct_info (ddc, FALSE);

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ddc);

    gtk_widget_show_all (ddc->dialog);

    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (builder, "postdate_label")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (builder, "post_date_box")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (builder, "memo_entry")));
    gtk_widget_hide (GTK_WIDGET(gtk_builder_get_object (builder, "memo_label")));

    ddc->retval = FALSE;
    while (gtk_dialog_run (GTK_DIALOG (ddc->dialog)) == GTK_RESPONSE_OK)
    {
        /* If response is OK but flag is not set, try again */
        if (ddc->retval)
            break;
    }

    g_object_unref(G_OBJECT(builder));

    gtk_widget_destroy(ddc->dialog);
    retval = ddc->retval;
    *acct = ddc->acct;
    g_free (ddc);

    return retval;
}
