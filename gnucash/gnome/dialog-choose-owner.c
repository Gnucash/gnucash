/*
 * dialog-choose-owner.c -- Dialog to choose an owner for a business Split
 * Copyright (C) 2006 Derek Atkins
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "qof.h"

#include "Transaction.h"
#include "dialog-utils.h"
#include "gncOwner.h"

#include "dialog-choose-owner.h"
#include "business-gnome-utils.h"

struct _choose_owner_dialog
{
    GtkWidget *	dialog;
    GtkWidget *	owner_choice;
    QofBook *	book;
    GncOwner	owner;
    Split *	split;
};

static DialogChooseOwner *
gcoi_create_dialog(Split* split)
{
    DialogChooseOwner* dco;
    GtkBuilder *builder;
    GtkWidget *widget, *box;

    g_return_val_if_fail(split, NULL);

    dco = g_new0(DialogChooseOwner, 1);
    g_assert(dco);
    dco->book = qof_instance_get_book(QOF_INSTANCE(split));
    dco->split = split;

    /* Open the Glade file */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-choose-owner.glade", "choose_owner_dialog");
    g_assert(builder);

    /* Get the dialog handle */
    dco->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "choose_owner_dialog"));
    g_assert(dco->dialog);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(dco->dialog), "GncChooseOwnerDialog");

    /* Get the title widget and set the title */
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "title_label"));
    if (1 == 1)
    {
        gncOwnerInitCustomer(&(dco->owner), NULL);
        gtk_label_set_text(GTK_LABEL(widget),
                           _("This transaction needs to be assigned to a Customer."
                             " Please choose the Customer below."));
    }
    else
    {
        gncOwnerInitVendor(&(dco->owner), NULL);
        gtk_label_set_text(GTK_LABEL(widget),
                           _("This transaction needs to be assigned to a Vendor."
                             " Please choose the Vendor below."));
    }

    /* Get the transaction description and set it */
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "desc_label"));
    gtk_label_set_text(GTK_LABEL(widget),
                       xaccTransGetDescription(xaccSplitGetParent(split)));

    /* Get the owner label and the owner box */
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "owner_label"));
    box = GTK_WIDGET(gtk_builder_get_object (builder, "owner_box"));
    dco->owner_choice = gnc_owner_select_create(widget, box, dco->book,
                        &(dco->owner));

    gtk_widget_show_all(dco->dialog);

    g_object_unref(G_OBJECT(builder));

    return dco;
}


gboolean
gnc_split_assign_owner(GtkWidget* window, Split* split)
{
    if (1 == 0)
        gcoi_create_dialog(split);

    return FALSE;
}
