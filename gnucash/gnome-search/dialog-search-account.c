/*
 * dialog-search-account.c -- Account selection dialog for search criteria
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
 * along with this program; if not, see
 * <https://www.gnu.org/licenses/>.
 */

#include <config.h>
#include <glib/gi18n.h>
#include "gnc-tree-view-account.h"
#include "dialog-search-account.h"

enum
{
    PROP_0,
    PROP_SEARCH_ACCOUNT
};

G_DEFINE_TYPE (GncSearchAccountDialog, gnc_search_account_dialog, GTK_TYPE_DIALOG)

static void
gnc_search_account_dialog_init (GncSearchAccountDialog *self)
{
    GtkWidget *accounts_scroller;
    GtkWidget *label;
    GtkTreeSelection *selection;
    GtkWidget *content_area;

    gtk_window_set_title (GTK_WINDOW (self), _("Select the Accounts to Compare"));

    /* Create the account tree */
    self->account_tree = GTK_WIDGET (gnc_tree_view_account_new (FALSE));
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (self->account_tree), FALSE);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (self->account_tree));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    /* Create the account scroller and put the tree in it */
    accounts_scroller = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (accounts_scroller),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_container_add (GTK_CONTAINER (accounts_scroller), self->account_tree);
    gtk_widget_set_size_request (GTK_WIDGET (accounts_scroller), 300, 300);

    /* Create the label */
    label = gtk_label_new (_("Select Accounts to Match"));

    /* Layout in dialog content area */
    content_area = gtk_dialog_get_content_area (GTK_DIALOG (self));
    gtk_box_pack_start (GTK_BOX (content_area), label, FALSE, FALSE, 3);
    gtk_box_pack_start (GTK_BOX (content_area), accounts_scroller, TRUE, TRUE, 3);

    /* Standard buttons */
    gtk_dialog_add_button (GTK_DIALOG (self), _("_Cancel"), GTK_RESPONSE_CANCEL);
    gtk_dialog_add_button (GTK_DIALOG (self), _("_OK"), GTK_RESPONSE_OK);
    gtk_dialog_set_default_response (GTK_DIALOG (self), GTK_RESPONSE_OK);
}

static void
gnc_search_account_dialog_dispose (GObject *object)
{
    GncSearchAccountDialog *self = GNC_SEARCH_ACCOUNT_DIALOG (object);

    g_clear_object (&self->search_account);

    G_OBJECT_CLASS (gnc_search_account_dialog_parent_class)->dispose (object);
}

static void
gnc_search_account_dialog_set_property (GObject *object,
                                        guint prop_id,
                                        const GValue *value,
                                        GParamSpec *pspec)
{
    GncSearchAccountDialog *self = GNC_SEARCH_ACCOUNT_DIALOG (object);

    switch (prop_id)
    {
    case PROP_SEARCH_ACCOUNT:
        if (self->search_account)
            g_object_unref (self->search_account);
        self->search_account = g_value_dup_object (value);
        if (self->search_account && self->account_tree)
        {
            GList *accounts = gnc_search_account_get_selected_accounts (self->search_account);
            if (accounts)
                gnc_tree_view_account_set_selected_accounts (GNC_TREE_VIEW_ACCOUNT (self->account_tree),
                                                             accounts, FALSE);
        }
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
gnc_search_account_dialog_get_property (GObject *object,
                                        guint prop_id,
                                        GValue *value,
                                        GParamSpec *pspec)
{
    GncSearchAccountDialog *self = GNC_SEARCH_ACCOUNT_DIALOG (object);

    switch (prop_id)
    {
    case PROP_SEARCH_ACCOUNT:
        g_value_set_object (value, self->search_account);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
gnc_search_account_dialog_class_init (GncSearchAccountDialogClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->set_property = gnc_search_account_dialog_set_property;
    object_class->get_property = gnc_search_account_dialog_get_property;
    object_class->dispose = gnc_search_account_dialog_dispose;

    g_object_class_install_property (
        object_class,
        PROP_SEARCH_ACCOUNT,
        g_param_spec_object ("search-account",
                             "Search Account",
                             "Search account criterion being configured",
                             GNC_TYPE_SEARCH_ACCOUNT,
                             G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY | G_PARAM_STATIC_STRINGS));
}

GtkWidget *
gnc_search_account_dialog_new (GtkWindow *parent, GNCSearchAccount *search_account)
{
    return GTK_WIDGET (g_object_new (GNC_TYPE_SEARCH_ACCOUNT_DIALOG,
                                    "transient-for", parent,
                                    "search-account", search_account,
                                    NULL));
}

GList *
gnc_search_account_dialog_get_selected_accounts (GncSearchAccountDialog *dialog)
{
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT_DIALOG (dialog), NULL);
    return gnc_tree_view_account_get_selected_accounts (GNC_TREE_VIEW_ACCOUNT (dialog->account_tree));
}
