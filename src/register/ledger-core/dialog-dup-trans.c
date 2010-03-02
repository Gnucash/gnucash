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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <time.h>
#include <stdlib.h>

#include "dialog-utils.h"
#include "gnc-date-edit.h"
#include "qof.h"
#include "gnc-ui.h"


/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
    GtkWidget * dialog;

    gboolean focus_out;

    GtkWidget * date_edit;
    GtkWidget * num_edit;
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

    if (!gnc_strisnum(string))
        return FALSE;

    number = strtol(string, NULL, 10);

    if ((number == LONG_MIN) || (number == LONG_MAX))
        return FALSE;

    if (num != NULL)
        *num = number;

    return TRUE;
}

static gboolean
gnc_dup_trans_output_cb(GtkSpinButton *spinbutton,
                        gpointer user_data)
{
    gboolean is_number;
    long int num;
    gchar *txt = gtk_editable_get_chars(GTK_EDITABLE(spinbutton), 0, -1);
    is_number = parse_num(txt, &num);
    g_free(txt);
    if (!is_number)
        gtk_entry_set_text(GTK_ENTRY(spinbutton), "");
    return !is_number;
}

static void
gnc_dup_trans_dialog_create (GtkWidget * parent, DupTransDialog *dt_dialog,
                             time_t date, const char *num_str)
{
    GtkWidget *dialog;
    GladeXML  *xml;

    xml = gnc_glade_xml_new ("register.glade", "Duplicate Transaction Dialog");

    dialog = glade_xml_get_widget (xml, "Duplicate Transaction Dialog");
    dt_dialog->dialog = dialog;

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    /* date widget */
    {
        GtkWidget *date_edit;
        GtkWidget *hbox;
        GtkWidget *label;

        date_edit = gnc_date_edit_new (date, FALSE, FALSE);
        gnc_date_activates_default(GNC_DATE_EDIT(date_edit), TRUE);
        hbox = glade_xml_get_widget (xml, "date_hbox");
        gtk_widget_show (date_edit);

        label = glade_xml_get_widget (xml, "date_label");
        gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date_edit), label);

        gtk_box_pack_end (GTK_BOX (hbox), date_edit, TRUE, TRUE, 0);
        dt_dialog->date_edit = date_edit;
    }

    {
        GtkWidget *num_spin;
        long int num;

        num_spin = glade_xml_get_widget (xml, "num_spin");
        dt_dialog->num_edit = num_spin;

        gtk_entry_set_activates_default(GTK_ENTRY(num_spin), TRUE);
        g_signal_connect(num_spin, "output",
                         G_CALLBACK(gnc_dup_trans_output_cb), dt_dialog);

        if (num_str && parse_num (num_str, &num))
            gtk_spin_button_set_value (GTK_SPIN_BUTTON (num_spin), num + 1);
        else
            gtk_entry_set_text (GTK_ENTRY (num_spin), "");
    }
}

/********************************************************************\
 * gnc_dup_trans_dialog                                             *
 *   opens up a window to do an automatic transfer between accounts *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 *         date    - the initial date to use, and the output        *
 *                   parameter for the new date                     *
 *         num     - input num field                                *
 *         out_num - output num field, g_newed string               *
 * Return: TRUE if user closes dialog with 'OK'                     *
\********************************************************************/
gboolean
gnc_dup_trans_dialog (GtkWidget * parent, time_t *date_p,
                      const char *num, char **out_num)
{
    DupTransDialog *dt_dialog;
    GNCDateEdit *gde;
    GtkWidget *entry;
    gboolean ok;
    gint result;

    if (!date_p || !out_num)
        return FALSE;

    dt_dialog = g_new0 (DupTransDialog, 1);

    gnc_dup_trans_dialog_create (parent, dt_dialog, *date_p, num);

    gde = GNC_DATE_EDIT (dt_dialog->date_edit);
    entry = gde->date_entry;

    gtk_widget_grab_focus (entry);

    result = gtk_dialog_run (GTK_DIALOG (dt_dialog->dialog));

    if (result == GTK_RESPONSE_OK)
    {
        *date_p = gnc_date_edit_get_date (GNC_DATE_EDIT (dt_dialog->date_edit));
        *out_num = g_strdup (gtk_entry_get_text (GTK_ENTRY (dt_dialog->num_edit)));
        ok = TRUE;
    }
    else
        ok = FALSE;

    gtk_widget_destroy(GTK_WIDGET(dt_dialog->dialog));
    g_free (dt_dialog);

    return ok;
}
