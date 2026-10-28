/* Copyright (C) 2026 GnuCash contributors
 * SPDX-License-Identifier: GPL-2.0-or-later */
#include <config.h>
#include <glib/gi18n.h>
#include "dialog-date-range-warning.h"

void
gnc_date_range_show_warning (GtkWindow *parent)
{
    // Translators: Use your locale date format here!
    const gchar *message = _("The entered date is out of the range "
                             "01/01/1400 - 31/12/9999, resetting to this year");
    const gchar *title = _("Date out of range");
    GtkWidget *dialog = gtk_message_dialog_new (parent,
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_ERROR,
        GTK_BUTTONS_OK, "%s", title);
    gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), "%s", message);
    g_signal_connect_swapped (dialog, "response", G_CALLBACK (gtk_widget_destroy), dialog);
    gtk_widget_show_all (dialog);
}
