/*
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

/* Non-blocking owner assignment for business splits. */
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "Transaction.h"
#include "dialog-utils.h"
#include "gnc-gui-query.h"
#include "gnc-component-manager.h"
#include "gncOwner.h"
#include "qof.h"
#include "gnc-session.h"

#include "dialog-choose-owner.h"
#include "business-gnome-utils.h"

struct _choose_owner_dialog
{
    GtkWidget *dialog;
    GtkWidget *owner_choice;
    QofBook *book;
    GncGUID split_guid;
    GncOwner owner;
    GncSplitAssignOwnerCallback callback;
    gpointer user_data;
    GDestroyNotify destroy;
    gint component_id;
};

typedef struct _choose_owner_dialog DialogChooseOwner;

void choose_owner_cancel_cb (GtkButton *button, DialogChooseOwner *dco);
void choose_owner_apply_cb (GtkButton *button, DialogChooseOwner *dco);

static void
choose_owner_finish (DialogChooseOwner *dco, gboolean assigned)
{
    Split *split = xaccSplitLookup (&dco->split_guid, dco->book);
    if (dco->callback)
        dco->callback (split, assigned && split != NULL, dco->user_data);
}

static void
choose_owner_destroy_cb (GtkWidget *widget, DialogChooseOwner *dco)
{
    (void)widget;
    if (dco->component_id)
        gnc_unregister_gui_component (dco->component_id);
    if (dco->destroy)
        dco->destroy (dco->user_data);
    g_free (dco);
}

static void
choose_owner_close_cb (gpointer data)
{
    DialogChooseOwner *dco = data;
    if (dco->dialog)
        gtk_window_destroy (GTK_WINDOW (dco->dialog));
}

void
choose_owner_cancel_cb (GtkButton *button, DialogChooseOwner *dco)
{
    (void)button;
    choose_owner_finish (dco, FALSE);
    gtk_window_destroy (GTK_WINDOW (dco->dialog));
}

void
choose_owner_apply_cb (GtkButton *button, DialogChooseOwner *dco)
{
    Split *split;
    GNCLot *lot;
    (void)button;

    gnc_owner_get_owner (dco->owner_choice, &dco->owner);
    split = xaccSplitLookup (&dco->split_guid, dco->book);
    lot = split ? xaccSplitGetLot (split) : NULL;
    if (!dco->owner.owner.undefined || !lot)
    {
        gnc_error_dialog (GTK_WINDOW (dco->dialog), "%s",
                          !lot ? _("The transaction is no longer available for owner assignment.") :
                                 _("Please choose an owner."));
        return;
    }
    gncOwnerAttachToLot (&dco->owner, lot);
    choose_owner_finish (dco, TRUE);
    gtk_window_destroy (GTK_WINDOW (dco->dialog));
}

void
gnc_split_assign_owner_async (GtkWindow *parent, Split *split,
                              GncSplitAssignOwnerCallback callback,
                              gpointer user_data, GDestroyNotify destroy)
{
    DialogChooseOwner *dco;
    GtkBuilder *builder;
    GtkWidget *widget;
    GtkWidget *box;
    Account *account;
    GncOwnerType owner_type;

    g_return_if_fail (split);
    account = xaccSplitGetAccount (split);
    owner_type = account && xaccAccountGetType (account) == ACCT_TYPE_PAYABLE
                     ? GNC_OWNER_VENDOR : GNC_OWNER_CUSTOMER;

    dco = g_new0 (DialogChooseOwner, 1);
    dco->book = qof_instance_get_book (QOF_INSTANCE (split));
    dco->split_guid = *qof_instance_get_guid (QOF_INSTANCE (split));
    dco->callback = callback;
    dco->user_data = user_data;
    dco->destroy = destroy;
    if (owner_type == GNC_OWNER_VENDOR)
        gncOwnerInitVendor (&dco->owner, NULL);
    else
        gncOwnerInitCustomer (&dco->owner, NULL);

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-choose-owner.glade", "choose_owner_dialog");
    dco->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "choose_owner_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW (dco->dialog), parent);
    gtk_widget_set_name (dco->dialog, "gnc-id-owner");

    widget = GTK_WIDGET (gtk_builder_get_object (builder, "title_label"));
    gtk_label_set_text (GTK_LABEL (widget), owner_type == GNC_OWNER_VENDOR
        ? _("This transaction needs to be assigned to a Vendor. Please choose the Vendor below.")
        : _("This transaction needs to be assigned to a Customer. Please choose the Customer below."));
    widget = GTK_WIDGET (gtk_builder_get_object (builder, "desc_label"));
    gtk_label_set_text (GTK_LABEL (widget),
                        xaccTransGetDescription (xaccSplitGetParent (split)));
    widget = GTK_WIDGET (gtk_builder_get_object (builder, "owner_label"));
    box = GTK_WIDGET (gtk_builder_get_object (builder, "owner_box"));
    dco->owner_choice = gnc_owner_select_create (widget, box, dco->book, &dco->owner);
    gtk_widget_set_visible (dco->owner_choice, TRUE);
    gnc_builder_connect_signals (builder, dco);
    g_signal_connect (dco->dialog, "destroy", G_CALLBACK (choose_owner_destroy_cb), dco);
    dco->component_id = gnc_register_gui_component ("dialog-choose-owner",
                                                     NULL, choose_owner_close_cb, dco);
    gnc_gui_component_set_session (dco->component_id, gnc_get_current_session ());
    g_object_unref (builder);
    gtk_window_present (GTK_WINDOW (dco->dialog));
}
