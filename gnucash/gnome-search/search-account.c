/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "qof.h"
#include "gnc-tree-view-account.h"
#include "gnc-gtk-utils.h"
#include "gnc-gui-query.h"
#include "dialog-utils.h"

#include "search-account.h"
#include "search-core-utils.h"

#define d(x)

static void pass_parent (GNCSearchCoreType *fe, gpointer parent);
static GNCSearchCoreType *gncs_clone (GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget (GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);
static gboolean selection_is_valid (GNCSearchAccount *fi);

static void gnc_search_account_finalize (GObject *obj);

struct _GNCSearchAccount
{
    GNCSearchCoreType parent;

    QofGuidMatch how;
};

typedef struct _GNCSearchAccountPrivate GNCSearchAccountPrivate;

struct _GNCSearchAccountPrivate
{
    gboolean match_all;
    GList *selected_guids;
    GncGUID book_guid;
    gboolean has_book_guid;
    GWeakRef parent;
};

typedef struct
{
    GNCSearchAccount *search;
    GWeakRef button;
    GWeakRef account_view;
    GncGUID book_guid;
} AccountSelectionWindow;

G_DEFINE_TYPE_WITH_PRIVATE (GNCSearchAccount, gnc_search_account,
                            GNC_TYPE_SEARCH_CORE_TYPE)

#define _PRIVATE(o) \
   ((GNCSearchAccountPrivate*)gnc_search_account_get_instance_private((GNCSearchAccount*)o))

static gpointer
copy_guid (gconstpointer source, gpointer user_data)
{
    (void)user_data;
    return guid_copy (source);
}

static void
selected_guids_clear (GNCSearchAccountPrivate *priv)
{
    g_list_free_full (priv->selected_guids, (GDestroyNotify)guid_free);
    priv->selected_guids = NULL;
    priv->has_book_guid = FALSE;
}

static void
gnc_search_account_class_init (GNCSearchAccountClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class->finalize = gnc_search_account_finalize;

    gnc_search_core_type->pass_parent = pass_parent;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;
}

static void
gnc_search_account_init (GNCSearchAccount *o)
{
    GNCSearchAccountPrivate *priv = _PRIVATE (o);

    o->how = QOF_GUID_MATCH_ANY;
    g_weak_ref_init (&priv->parent, NULL);
}

static void
gnc_search_account_finalize (GObject *obj)
{
    GNCSearchAccount *o = (GNCSearchAccount *)obj;
    GNCSearchAccountPrivate *priv = _PRIVATE (o);

    g_assert (GNC_IS_SEARCH_ACCOUNT (o));
    selected_guids_clear (priv);
    g_weak_ref_clear (&priv->parent);

    G_OBJECT_CLASS (gnc_search_account_parent_class)->finalize(obj);
}

/**
 * gnc_search_account_new:
 *
 * Create a new GNCSearchAccount object.
 *
 * Return value: A new #GNCSearchAccount object.
 **/
GNCSearchAccount *
gnc_search_account_new (void)
{
    return g_object_new (GNC_TYPE_SEARCH_ACCOUNT, NULL);
}

/**
 * gnc_search_account_matchall_new:
 *
 * Create a new GNCSearchAccount object.
 *
 * Return value: A new #GNCSearchAccount object.
 **/
GNCSearchAccount *
gnc_search_account_matchall_new (void)
{
    GNCSearchAccount *o = g_object_new (GNC_TYPE_SEARCH_ACCOUNT, NULL);
    GNCSearchAccountPrivate *priv = _PRIVATE (o);

    priv->match_all = TRUE;
    o->how = QOF_GUID_MATCH_ALL;
    return o;
}

static QofBook *
account_selection_current_book (const GncGUID *book_guid)
{
    QofBook *book = gnc_get_current_book ();

    if (!book || qof_book_shutting_down (book) ||
        !guid_equal (book_guid, qof_book_get_guid (book)))
        return NULL;
    return book;
}

static GList *
selected_accounts_for_current_book (GNCSearchAccount *fi)
{
    GNCSearchAccountPrivate *priv = _PRIVATE (fi);
    QofBook *book;
    GList *accounts = NULL;

    if (!priv->has_book_guid)
        return NULL;
    book = account_selection_current_book (&priv->book_guid);
    if (!book)
        return NULL;

    for (GList *node = priv->selected_guids; node; node = node->next)
    {
        Account *account = xaccAccountLookup (node->data, book);

        if (!account)
        {
            g_list_free (accounts);
            return NULL;
        }
        accounts = g_list_prepend (accounts, account);
    }
    return g_list_reverse (accounts);
}

static gboolean
selection_is_valid (GNCSearchAccount *fi)
{
    GNCSearchAccountPrivate *priv = _PRIVATE (fi);
    GList *accounts;
    gboolean valid;

    if (!priv->selected_guids)
        return FALSE;
    accounts = selected_accounts_for_current_book (fi);
    valid = accounts != NULL;
    g_list_free (accounts);
    return valid;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *priv;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fi), FALSE);

    priv = _PRIVATE (fi);
    if (!selection_is_valid (fi))
    {
        GtkWindow *parent = g_weak_ref_get (&priv->parent);

        gnc_error_dialog (parent, "%s", _("You have not selected any accounts"));
        g_clear_object (&parent);
        return FALSE;
    }
    return TRUE;
}

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *priv = _PRIVATE (fi);
    GtkDropDown *drop_down = GTK_DROP_DOWN (gnc_search_drop_down_new ());
    int initial = QOF_GUID_MATCH_ANY;

    if (priv->match_all)
    {
        gnc_search_drop_down_add (drop_down, _("matches all accounts"), QOF_GUID_MATCH_ALL);
        initial = QOF_GUID_MATCH_ALL;
    }
    else
    {
        gnc_search_drop_down_add (drop_down, _("matches any account"), QOF_GUID_MATCH_ANY);
        gnc_search_drop_down_add (drop_down, _("matches no accounts"), QOF_GUID_MATCH_NONE);
    }

    gnc_search_drop_down_changed (drop_down, &fi->how);
    gnc_search_drop_down_set_active (drop_down, fi->how ? fi->how : initial);

    return GTK_WIDGET (drop_down);
}

static const char *
describe_button (GNCSearchAccount *fi)
{
    GNCSearchAccountPrivate *priv = _PRIVATE (fi);

    return priv->selected_guids ? _("Selected Accounts") : _("Choose Accounts");
}

static gboolean
selected_guids_set_from_accounts (GNCSearchAccount *fi, QofBook *book,
                                  GList *accounts)
{
    GNCSearchAccountPrivate *priv = _PRIVATE (fi);
    GList *guids = NULL;

    for (GList *node = accounts; node; node = node->next)
    {
        Account *account = node->data;

        if (!GNC_IS_ACCOUNT (account) || gnc_account_get_book (account) != book)
        {
            g_list_free_full (guids, (GDestroyNotify)guid_free);
            return FALSE;
        }
        guids = g_list_prepend (guids, guid_copy (xaccAccountGetGUID (account)));
    }

    selected_guids_clear (priv);
    priv->selected_guids = g_list_reverse (guids);
    if (priv->selected_guids)
    {
        priv->book_guid = *qof_book_get_guid (book);
        priv->has_book_guid = TRUE;
    }
    return TRUE;
}

static void
account_selection_window_free (AccountSelectionWindow *selection)
{
    g_clear_object (&selection->search);
    g_weak_ref_clear (&selection->button);
    g_weak_ref_clear (&selection->account_view);
    g_free (selection);
}

static void
account_selection_close (GtkWindow *window)
{
    gtk_window_destroy (window);
}

static void
account_selection_cancel_clicked (GtkButton *button, AccountSelectionWindow *selection)
{
    GtkWindow *window = GTK_WINDOW (gtk_widget_get_root (GTK_WIDGET (button)));

    (void)selection;
    account_selection_close (window);
}

static void
account_selection_apply_clicked (GtkButton *button, AccountSelectionWindow *selection)
{
    GtkWindow *window = GTK_WINDOW (gtk_widget_get_root (GTK_WIDGET (button)));
    GncTreeViewAccount *account_view = g_weak_ref_get (&selection->account_view);
    QofBook *book = account_selection_current_book (&selection->book_guid);
    GList *accounts;
    GtkWidget *choice_button;

    if (!account_view || !book)
    {
        gnc_error_dialog (window, "%s", _("The selected accounts are no longer available."));
        g_clear_object (&account_view);
        return;
    }

    accounts = gnc_tree_view_account_get_selected_accounts (account_view);
    if (!selected_guids_set_from_accounts (selection->search, book, accounts))
    {
        gnc_error_dialog (window, "%s", _("The selected accounts are no longer available."));
        g_list_free (accounts);
        g_clear_object (&account_view);
        return;
    }
    g_list_free (accounts);
    g_clear_object (&account_view);

    choice_button = g_weak_ref_get (&selection->button);
    if (choice_button)
    {
        gtk_button_set_label (GTK_BUTTON (choice_button),
                              describe_button (selection->search));
        g_object_unref (choice_button);
    }
    account_selection_close (window);
}

static gboolean
account_selection_close_request (GtkWindow *window, AccountSelectionWindow *selection)
{
    (void)selection;
    account_selection_close (window);
    return TRUE;
}

static gboolean
account_selection_key_pressed (GtkEventControllerKey *controller,
                               guint keyval, guint keycode,
                               GdkModifierType state,
                               AccountSelectionWindow *selection)
{
    GtkWidget *widget;

    (void)keycode;
    (void)state;
    (void)selection;
    if (keyval != GDK_KEY_Escape)
        return FALSE;
    widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));
    account_selection_close (GTK_WINDOW (widget));
    return TRUE;
}

static void
button_clicked (GtkButton *button, GNCSearchAccount *fi)
{
    GNCSearchAccountPrivate *priv = _PRIVATE (fi);
    GtkWindow *dialog;
    GtkWidget *account_tree;
    GtkWidget *scrolled_window;
    GtkWidget *label;
    GtkWidget *content;
    GtkWidget *button_box;
    GtkWidget *cancel_button;
    GtkWidget *apply_button;
    GtkEventController *key_controller;
    AccountSelectionWindow *selection;
    QofBook *book;
    GtkWindow *parent;
    GList *accounts;

    book = gnc_get_current_book ();
    if (!book || qof_book_shutting_down (book))
    {
        parent = g_weak_ref_get (&priv->parent);
        gnc_error_dialog (parent, "%s", _("The selected accounts are no longer available."));
        g_clear_object (&parent);
        return;
    }

    account_tree = gnc_tree_view_account_new (FALSE);
    gnc_tree_view_account_set_headers_visible (GNC_TREE_VIEW_ACCOUNT (account_tree), FALSE);
    gnc_tree_view_account_set_selection_mode (GNC_TREE_VIEW_ACCOUNT (account_tree),
                                              GTK_SELECTION_MULTIPLE);

    accounts = selected_accounts_for_current_book (fi);
    if (accounts)
    {
        gnc_tree_view_account_set_selected_accounts (GNC_TREE_VIEW_ACCOUNT (account_tree),
                                                      accounts, FALSE);
        g_list_free (accounts);
    }

    scrolled_window = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scrolled_window), account_tree);
    gtk_widget_set_size_request (scrolled_window, 300, 300);

    label = gtk_label_new (_("Select Accounts to Match"));
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);

    dialog = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (dialog);
    gtk_window_set_title (dialog, _("Select the Accounts to Compare"));
    gtk_window_set_modal (dialog, TRUE);
    gtk_window_set_default_size (dialog, 420, 420);
    parent = g_weak_ref_get (&priv->parent);
    if (parent)
    {
        gtk_window_set_transient_for (dialog, parent);
        g_signal_connect_object (parent, "destroy", G_CALLBACK (gtk_window_destroy),
                                 dialog, G_CONNECT_SWAPPED);
        g_object_unref (parent);
    }

    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    gtk_widget_set_margin_start (content, 12);
    gtk_widget_set_margin_end (content, 12);
    gtk_widget_set_margin_top (content, 12);
    gtk_widget_set_margin_bottom (content, 12);
    gtk_window_set_child (dialog, content);
    gtk_box_append (GTK_BOX (content), label);
    gtk_widget_set_vexpand (scrolled_window, TRUE);
    gtk_box_append (GTK_BOX (content), scrolled_window);

    button_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_halign (button_box, GTK_ALIGN_END);
    cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
    apply_button = gtk_button_new_with_mnemonic (_("_OK"));
    gtk_box_append (GTK_BOX (button_box), cancel_button);
    gtk_box_append (GTK_BOX (button_box), apply_button);
    gtk_box_append (GTK_BOX (content), button_box);
    gtk_window_set_default_widget (dialog, apply_button);

    selection = g_new0 (AccountSelectionWindow, 1);
    selection->search = g_object_ref (fi);
    selection->book_guid = *qof_book_get_guid (book);
    g_weak_ref_init (&selection->button, button);
    g_weak_ref_init (&selection->account_view, account_tree);
    g_object_set_data_full (G_OBJECT (dialog), "gnc-search-account-selection",
                            selection, (GDestroyNotify)account_selection_window_free);

    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (account_selection_cancel_clicked), selection);
    g_signal_connect (apply_button, "clicked",
                      G_CALLBACK (account_selection_apply_clicked), selection);
    g_signal_connect (dialog, "close-request",
                      G_CALLBACK (account_selection_close_request), selection);
    key_controller = gtk_event_controller_key_new ();
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (account_selection_key_pressed), selection);
    gtk_widget_add_controller (GTK_WIDGET (dialog), key_controller);
    gtk_window_present (dialog);
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *button;
    GtkWidget *menu;
    GtkWidget *box;
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fi), NULL);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    menu = make_menu (fe);
    gtk_box_append (GTK_BOX (box), menu);

    button = gtk_button_new_with_label (describe_button (fi));
    g_signal_connect_object (button, "clicked", G_CALLBACK (button_clicked), fi, 0);
    gtk_box_append (GTK_BOX (box), button);

    return box;
}

static QofQueryPredData *
gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *priv;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fi), NULL);

    priv = _PRIVATE (fi);
    if (!selection_is_valid (fi))
        return NULL;
    return qof_query_guid_predicate (fi->how, priv->selected_guids);
}

static GNCSearchCoreType *
gncs_clone (GNCSearchCoreType *fe)
{
    GNCSearchAccount *se;
    GNCSearchAccount *fse = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *se_priv;
    GNCSearchAccountPrivate *fse_priv;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fse), NULL);
    fse_priv = _PRIVATE (fse);

    se = gnc_search_account_new ();
    se_priv = _PRIVATE (se);
    se->how = fse->how;
    se_priv->match_all = fse_priv->match_all;
    if (fse_priv->selected_guids)
    {
        se_priv->selected_guids = g_list_copy_deep (fse_priv->selected_guids,
                                                    copy_guid, NULL);
        se_priv->book_guid = fse_priv->book_guid;
        se_priv->has_book_guid = fse_priv->has_book_guid;
    }

    return (GNCSearchCoreType *)se;
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_ACCOUNT (fi));

    priv = _PRIVATE (fi);
    g_weak_ref_set (&priv->parent, GTK_IS_WINDOW (parent) ? parent : NULL);
}
