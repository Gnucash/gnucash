/********************************************************************\
 * dialog-dup-trans.c -- duplicate transaction dialog               *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <time.h>
#include <stdlib.h>

#include "dialog-dup-trans.h"
#include "dialog-utils.h"
#include "gnc-date-edit.h"
#include "qof.h"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

typedef struct
{
    GtkWidget * dialog;

    gboolean focus_out;

    GtkWidget * date_edit;
    GtkWidget * num_edit;
    GtkWidget * tnum_edit;
    GtkWidget * link_edit;

    GtkWidget *duplicate_title_label; // GtkLabel
    GtkWidget *duplicate_table;       // GtkTable
    GtkWidget *date_label;            // GtkLabel
    GtkWidget *num_label;             // GtkLabel
    GtkWidget *tnum_label;            // GtkLabel
    GtkWidget *link_label;            // GtkLabel
} DupTransDialog;

/* Parses the string value and returns true if it is a
 * number. In that case, *num is set to the value parsed.
 * Copied from numcell.c */
static gboolean
parse_num (const char *string, long int *num)
{
    long int number;

    if (string == NULL)
        return FALSE;

    if (!gnc_strisnum (string))
        return FALSE;

    number = strtol (string, NULL, 10);

    if ((number == LONG_MIN) || (number == LONG_MAX))
        return FALSE;

    if (num != NULL)
        *num = number;

    return TRUE;
}

static gboolean
gnc_dup_inc_dec (GtkWidget *widget, const gchar *text, gint inc_dec)
{
    long int num;

    if (parse_num (text, &num))
    {
        gchar *format;
        gchar *out;
        num = num + inc_dec;

        if (num == -1)
            num = 0;

        if (g_str_has_prefix (text, "0"))
            format = g_strdup_printf ("%s%ld%s", "%0", g_utf8_strlen (text, -1), "d");
        else
            format = g_strdup_printf ("%s", "%ld");

        out = g_strdup_printf (format, num);

        gtk_entry_set_text (GTK_ENTRY(widget), out);
        g_free (format);
        g_free (out);
        return TRUE;
    }
    return FALSE;
}

static gboolean
gnc_dup_key_press_event_cb (GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
    const gchar *text = gtk_entry_get_text (GTK_ENTRY(widget));

    if (gnc_strisnum (text))
    {
        GdkModifierType modifiers = gtk_accelerator_get_default_mod_mask ();
        gint increment;

        if ((event->state & modifiers) == GDK_CONTROL_MASK ||
            (event->state & modifiers) == GDK_MOD1_MASK)
            return FALSE;

        /* See https://bugs.gnucash.org/show_bug.cgi?id=798386 for semicolon */
        if (event->keyval == GDK_KEY_plus || event->keyval == GDK_KEY_KP_Add ||
            event->keyval == GDK_KEY_semicolon)
            increment = 1;
        else if (event->keyval == GDK_KEY_minus || event->keyval == GDK_KEY_KP_Subtract)
            increment = -1;
        else
            return FALSE;

        return gnc_dup_inc_dec (widget, text, increment);

    }
    else
        return FALSE;
}

static void
gnc_dup_trans_dialog_create (GtkWidget * parent, DupTransDialog *dt_dialog,
                             gboolean show_date, time64 date,
                             const char *num_str, const char *tnum_str)
{
    GtkWidget *dialog;
    GtkBuilder  *builder;
    const gchar *tt = _("You can type '+' or '-' to increment or decrement the number.");

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade", "num_adjustment");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade", "tnum_adjustment");
    gnc_builder_add_from_file (builder, "gnc-plugin-page-register.glade", "duplicate_transaction_dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "duplicate_transaction_dialog"));
    dt_dialog->dialog = dialog;

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(dialog), "gnc-id-duplicate-transaction");
    gnc_widget_style_context_add_class (GTK_WIDGET(dialog), "gnc-class-transactions");

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW(dialog), GTK_WINDOW(parent));

    /* date widget */
    dt_dialog->date_label = GTK_WIDGET(gtk_builder_get_object (builder, "date_label"));
    if (show_date)
    {
        GtkWidget *date_edit;
        GtkWidget *hbox;

        date_edit = gnc_date_edit_new (date, FALSE, FALSE);
        gnc_date_activates_default (GNC_DATE_EDIT(date_edit), TRUE);
        hbox = GTK_WIDGET(gtk_builder_get_object (builder, "date_hbox"));
        gtk_widget_show (date_edit);

        gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date_edit), dt_dialog->date_label);

        gtk_box_pack_end (GTK_BOX(hbox), date_edit, TRUE, TRUE, 0);
        dt_dialog->date_edit = date_edit;
    }
    else
    {
        GtkWidget *date_edit;
        date_edit = gnc_date_edit_new (date, FALSE, FALSE);
        dt_dialog->date_edit = date_edit;
    }

    dt_dialog->duplicate_title_label = GTK_WIDGET(gtk_builder_get_object (builder, "duplicate_title_label"));
    dt_dialog->duplicate_table = GTK_WIDGET(gtk_builder_get_object (builder, "duplicate_table"));
    dt_dialog->num_label = GTK_WIDGET(gtk_builder_get_object (builder, "num_label"));
    dt_dialog->tnum_label = GTK_WIDGET(gtk_builder_get_object (builder, "tnum_label"));

    dt_dialog->num_edit = GTK_WIDGET(gtk_builder_get_object (builder, "num_entry"));
    dt_dialog->tnum_edit = GTK_WIDGET(gtk_builder_get_object (builder, "tnum_entry"));

    if (num_str)
        gtk_entry_set_text (GTK_ENTRY(dt_dialog->num_edit), num_str);
    if (tnum_str)
        gtk_entry_set_text (GTK_ENTRY(dt_dialog->tnum_edit), tnum_str);

    g_signal_connect (dt_dialog->num_edit, "key-press-event",
                      G_CALLBACK(gnc_dup_key_press_event_cb),
                      dt_dialog);

    g_signal_connect (dt_dialog->tnum_edit, "key-press-event",
                      G_CALLBACK(gnc_dup_key_press_event_cb),
                      dt_dialog);

    if (gnc_strisnum (num_str))
    {
        gtk_widget_set_tooltip_text (GTK_WIDGET(dt_dialog->num_edit), tt);
        gnc_dup_inc_dec (GTK_WIDGET(dt_dialog->num_edit), num_str, 1);
    }
    if (gnc_strisnum (tnum_str))
    {
        gtk_widget_set_tooltip_text (GTK_WIDGET(dt_dialog->tnum_edit), tt);
        gnc_dup_inc_dec (GTK_WIDGET(dt_dialog->tnum_edit), tnum_str, 1);
    }

    /* Transaction Linked Document */
    {
        dt_dialog->link_label = GTK_WIDGET(gtk_builder_get_object (builder, "link_label"));
        dt_dialog->link_edit = GTK_WIDGET(gtk_builder_get_object (builder, "link_check_button"));
    }

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, dt_dialog);

    g_object_unref (G_OBJECT(builder));
}

static gboolean
gnc_dup_trans_dialog_internal (GtkWidget * parent,
                               const char* window_title, const char* title,
                               gboolean show_date, time64 *date_p, GDate *gdate_p,
                               const char *num, char **out_num,
                               const char *tnum, char **out_tnum,
                               const char *tlink, char **out_tlink)
{
    DupTransDialog *dt_dialog;
    GtkWidget *entry;
    gboolean ok;
    gint result;

    dt_dialog = g_new0 (DupTransDialog, 1);

    gnc_dup_trans_dialog_create (parent, dt_dialog, show_date,
                                 *date_p, num, tnum);

    if (!show_date)
    {
        // The "date" field isn't being asked for, so we make the widgets invisible
        gtk_widget_set_visible (dt_dialog->date_label, FALSE);
        if (dt_dialog->date_edit)
            gtk_widget_set_visible (dt_dialog->date_edit, FALSE);
        // If no "date" field, there must be a "num" field, so give it focus
        if (out_num)
            gtk_widget_grab_focus (dt_dialog->num_edit);
    }
    else
    {
        GNCDateEdit *gde;

        gde = GNC_DATE_EDIT(dt_dialog->date_edit);
        entry = gde->date_entry;
        gtk_widget_grab_focus (entry);
    }

    if (window_title)
        gtk_window_set_title (GTK_WINDOW(dt_dialog->dialog), window_title);

    if (title)
    {
        gchar *full_text = g_strdup_printf ("<b>%s</b>", title);
        gtk_label_set_markup (GTK_LABEL(dt_dialog->duplicate_title_label), full_text);
        g_free (full_text);
    }

    if (!out_num)
    {
        // The "num" field isn't being asked for, so we make the widgets invisible
        gtk_widget_set_visible (dt_dialog->num_label, FALSE);
        gtk_widget_set_visible (dt_dialog->num_edit, FALSE);
    }

    if (!tnum)
    {
        // The "tnum" field isn't being asked for, so we make the widgets invisible
        gtk_widget_set_visible (dt_dialog->tnum_label, FALSE);
        gtk_widget_set_visible (dt_dialog->tnum_edit, FALSE);
    }

    if (!show_date && !tnum)
    {
        // The "date" and the "tnum" fields aren't being asked for, this is a split copy
        gtk_label_set_markup (GTK_LABEL(dt_dialog->num_label), _("Action/Number"));
    }

    if (tnum)
    {
        gtk_entry_set_activates_default (GTK_ENTRY(dt_dialog->num_edit), FALSE);
        gtk_entry_set_activates_default (GTK_ENTRY(dt_dialog->tnum_edit), TRUE);
    }

    if (tlink)
    {
        gtk_widget_set_visible (dt_dialog->link_label, TRUE);
        gtk_widget_set_visible (dt_dialog->link_edit, TRUE);
    }
    else
    {
        gtk_widget_set_visible (dt_dialog->link_label, FALSE);
        gtk_widget_set_visible (dt_dialog->link_edit, FALSE);
    }

    result = gtk_dialog_run (GTK_DIALOG(dt_dialog->dialog));

    if (result == GTK_RESPONSE_OK)
    {
        if (date_p)
            *date_p = gnc_date_edit_get_date (GNC_DATE_EDIT(dt_dialog->date_edit));
        if (gdate_p)
            gnc_date_edit_get_gdate (GNC_DATE_EDIT(dt_dialog->date_edit), gdate_p);
        if (out_num)
            *out_num = g_strdup (gtk_entry_get_text (GTK_ENTRY(dt_dialog->num_edit)));
        if (tnum)
            *out_tnum = g_strdup (gtk_entry_get_text (GTK_ENTRY(dt_dialog->tnum_edit)));
        if (tlink)
        {
            if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(dt_dialog->link_edit)))
                *out_tlink = g_strdup (tlink);
        }
        ok = TRUE;
    }
    else
        ok = FALSE;

    gtk_widget_destroy (GTK_WIDGET(dt_dialog->dialog));
    g_free (dt_dialog);

    return ok;
}

gboolean
gnc_dup_trans_dialog (GtkWidget * parent, const char* title,
                      gboolean show_date, time64 *date_p,
                      const char *num, char **out_num,
                      const char *tnum, char **out_tnum,
                      const char *tlink, char **out_tlink)
{
    return gnc_dup_trans_dialog_internal (parent, NULL, title,
                                          show_date, date_p, NULL,
                                          num, out_num, tnum, out_tnum,
                                          tlink, out_tlink);
}

gboolean
gnc_dup_trans_dialog_gdate (GtkWidget * parent, GDate *gdate_p,
                            const char *num, char **out_num)
{
    time64 tmp_time;
    g_assert (gdate_p);

    tmp_time = gdate_to_time64 (*gdate_p);
    return gnc_dup_trans_dialog_internal (parent, NULL, NULL, TRUE,
                                          &tmp_time, gdate_p,
                                          num, out_num, NULL, NULL,
                                          NULL, NULL);
}

gboolean
gnc_dup_time64_dialog (GtkWidget * parent, const char *window_title,
                       const char* title, time64 *date)
{
    return gnc_dup_trans_dialog_internal (parent, window_title, title, TRUE,
                                          date, NULL,
                                          NULL, NULL, NULL, NULL,
                                          NULL, NULL);
}

gboolean
gnc_dup_date_dialog (GtkWidget * parent, const char* title, GDate *gdate_p)
{
    time64 tmp_time;
    g_assert (gdate_p);

    tmp_time = gdate_to_time64 (*gdate_p);
    return gnc_dup_trans_dialog_internal (parent, NULL, title, TRUE,
                                          &tmp_time, gdate_p,
                                          NULL, NULL, NULL, NULL,
                                          NULL, NULL);
}

