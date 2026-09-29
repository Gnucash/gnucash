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
#include "gnc-gui-query.h"
#include "dialog-utils.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "guid.h"

#include "search-account.h"
#include "search-core-utils.h"

#define d(x)

static void pass_parent (GNCSearchCoreType *fe, gpointer parent);
static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_account_finalize	(GObject *obj);

struct _GNCSearchAccount
{
    GNCSearchCoreType parent;

    QofGuidMatch        how;
};

typedef struct _GNCSearchAccountPrivate GNCSearchAccountPrivate;

struct _GNCSearchAccountPrivate
{
    gboolean	match_all;
	GList *	selected_guids;
    GncGUID book_guid;
    gboolean has_book_guid;
    GWeakRef parent;
};

G_DEFINE_TYPE_WITH_PRIVATE(GNCSearchAccount, gnc_search_account, GNC_TYPE_SEARCH_CORE_TYPE)

#define _PRIVATE(o) \
   ((GNCSearchAccountPrivate*)gnc_search_account_get_instance_private((GNCSearchAccount*)o))

static gpointer
copy_guid (gconstpointer source, [[maybe_unused]] gpointer user_data)
{
    return guid_copy (source);
}

static void
selected_guids_clear (GNCSearchAccountPrivate *priv)
{
    g_list_free_full (priv->selected_guids, (GDestroyNotify)guid_free);
    priv->selected_guids = NULL;
    priv->has_book_guid = FALSE;
}

static QofBook *
selected_accounts_current_book (const GncGUID *book_guid)
{
    QofBook *book = gnc_get_current_book ();

    if (!book || !qof_book_is_open (book) || qof_book_shutting_down (book) ||
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
    book = selected_accounts_current_book (&priv->book_guid);
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
        guids = g_list_prepend (guids,
                                guid_copy (xaccAccountGetGUID (account)));
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
gnc_search_account_class_init (GNCSearchAccountClass *klass)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)klass;

    object_class = G_OBJECT_CLASS (klass);

    object_class->finalize = gnc_search_account_finalize;

    /* override methods */
    gnc_search_core_type->pass_parent = pass_parent;
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;
}

static void
gnc_search_account_init (GNCSearchAccount *o)
{
    o->how = QOF_GUID_MATCH_ANY;
    g_weak_ref_init (&_PRIVATE (o)->parent, NULL);
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
    GNCSearchAccount *o = g_object_new(GNC_TYPE_SEARCH_ACCOUNT, NULL);
    return o;
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
    GNCSearchAccount *o;
    GNCSearchAccountPrivate *priv;

    o = g_object_new(GNC_TYPE_SEARCH_ACCOUNT, NULL);
    priv = _PRIVATE(o);
    priv->match_all = TRUE;
    o->how = QOF_GUID_MATCH_ALL;
    return o;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *priv;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fi), FALSE);

    priv = _PRIVATE(fi);

    if (!selection_is_valid (fi) && fi->how )
    {
        GtkWindow *parent = g_weak_ref_get (&priv->parent);
        valid = FALSE;
        gnc_error_dialog_async (parent, "%s", _("You have not selected any accounts"));
        g_clear_object (&parent);
    }

    /* XXX */

    return valid;
}

static GtkWidget *
make_menu (GNCSearchCoreType *fe)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *priv;
    GtkComboBox *combo;
    int initial = 0;

    combo = GTK_COMBO_BOX(gnc_combo_box_new_search());

    priv = _PRIVATE(fi);
    if (priv->match_all)
    {
        gnc_combo_box_search_add(combo, _("matches all accounts"), QOF_GUID_MATCH_ALL);
        initial = QOF_GUID_MATCH_ALL;
    }
    else
    {
        gnc_combo_box_search_add(combo, _("matches any account"), QOF_GUID_MATCH_ANY);
        gnc_combo_box_search_add(combo, _("matches no accounts"), QOF_GUID_MATCH_NONE);
        initial = QOF_GUID_MATCH_ANY;
    }

    gnc_combo_box_search_changed(combo, &fi->how);
    gnc_combo_box_search_set_active(combo, fi->how ? fi->how : initial);

    return GTK_WIDGET(combo);
}

static char *
describe_button (GNCSearchAccount *fi)
{
    GNCSearchAccountPrivate *priv;

    priv = _PRIVATE(fi);
    if (priv->selected_guids)
        return (_("Selected Accounts"));
    return (_("Choose Accounts"));
}

typedef struct
{
    GWeakRef search, button, account_view;
    QofBook *book;
    GncGUID book_guid;
    gboolean completed;
} AccountSelectionRequest;

static void
account_selection_request_free (gpointer data)
{
    AccountSelectionRequest *request = data;
    if (request->book)
        g_object_remove_weak_pointer (G_OBJECT (request->book),
                                      (gpointer *)&request->book);
    g_weak_ref_clear (&request->search);
    g_weak_ref_clear (&request->button);
    g_weak_ref_clear (&request->account_view);
    g_free (request);
}

static void
account_selection_dialog_destroyed ([[maybe_unused]] GtkWidget *dialog,
                                    gpointer data)
{
    ((AccountSelectionRequest *)data)->completed = TRUE;
}

static void
account_selection_response (GtkDialog *dialog, gint response, gpointer data)
{
    AccountSelectionRequest *request = data;
    GNCSearchAccount *fi;
    GtkWidget *button, *view;
    GList *accounts = NULL;
    QofBook *book = request->book;

    if (request->completed)
        return;
    request->completed = TRUE;
    /* Label notifications may destroy the dialog. Its data owns request. */
    g_object_ref (dialog);
    fi = g_weak_ref_get (&request->search);
    button = g_weak_ref_get (&request->button);
    view = g_weak_ref_get (&request->account_view);

    if (response == GTK_RESPONSE_OK && fi && button && view && book &&
        !gtk_widget_in_destruction (button) && book == gnc_get_current_book () &&
        qof_book_is_open (book) && !qof_book_shutting_down (book) &&
        guid_equal (&request->book_guid, qof_book_get_guid (book)))
    {
        accounts = gnc_tree_view_account_get_selected_accounts (
            GNC_TREE_VIEW_ACCOUNT (view));
        if (selected_guids_set_from_accounts (fi, book, accounts))
        {
            GtkWidget *label = gtk_bin_get_child (GTK_BIN (button));
            if (GTK_IS_LABEL (label))
                gtk_label_set_text (GTK_LABEL (label), describe_button (fi));
        }
    }
    g_list_free (accounts);
    g_clear_object (&view);
    g_clear_object (&button);
    g_clear_object (&fi);
    gtk_widget_destroy (GTK_WIDGET (dialog));
    g_object_unref (dialog);
}

static void
button_clicked (GtkButton *button, GNCSearchAccount *fi)
{
    GNCSearchAccountPrivate *priv;
    GtkDialog *dialog;
    GtkWidget *account_tree;
    GtkWidget *accounts_scroller;
    GtkWidget *label;
    GtkTreeSelection *selection;
    AccountSelectionRequest *request;
    QofBook *book = gnc_get_current_book ();
    GtkWindow *parent;

    if (!book || !qof_book_is_open (book) || qof_book_shutting_down (book))
        return;

    /* Create the account tree */
    account_tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(account_tree), FALSE);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(account_tree));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    /* Select the currently-selected accounts */
    priv = _PRIVATE(fi);
    GList *selected = selected_accounts_for_current_book (fi);
    if (selected)
        gnc_tree_view_account_set_selected_accounts (GNC_TREE_VIEW_ACCOUNT(account_tree),
                selected, FALSE);
    g_list_free (selected);

    /* Create the account scroller and put the tree in it */
    accounts_scroller = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(accounts_scroller),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_container_add(GTK_CONTAINER(accounts_scroller), account_tree);
    gtk_widget_set_size_request(GTK_WIDGET(accounts_scroller), 300, 300);

    /* Create the label */
    label = gtk_label_new (_("Select Accounts to Match"));

    /* Create the dialog */
    dialog =
        GTK_DIALOG(gtk_dialog_new_with_buttons(_("Select the Accounts to Compare"),
                   NULL,
                   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                   _("_Cancel"), GTK_RESPONSE_CANCEL,
                   _("_OK"), GTK_RESPONSE_OK,
                   NULL));
    parent = g_weak_ref_get (&priv->parent);
    if (parent)
    {
        gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);
        g_object_unref (parent);
    }
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);
    request = g_new0 (AccountSelectionRequest, 1);
    g_weak_ref_init (&request->search, fi);
    g_weak_ref_init (&request->button, button);
    g_weak_ref_init (&request->account_view, account_tree);
    request->book = book;
    request->book_guid = *qof_book_get_guid (book);
    g_object_add_weak_pointer (G_OBJECT (book), (gpointer *)&request->book);
    g_object_set_data_full (G_OBJECT (dialog), "gnc-account-selection-request",
                            request, account_selection_request_free);
    g_signal_connect (dialog, "response",
                      G_CALLBACK (account_selection_response), request);
    g_signal_connect (dialog, "destroy",
                      G_CALLBACK (account_selection_dialog_destroyed), request);

    /* Put the dialog together */
    gtk_box_pack_start ((GtkBox *) gtk_dialog_get_content_area (dialog), label,
                        FALSE, FALSE, 3);
    gtk_box_pack_start ((GtkBox *) gtk_dialog_get_content_area (dialog), accounts_scroller,
                        TRUE, TRUE, 3);

    gtk_widget_show_all (GTK_WIDGET (dialog));

    /* Response handler owns the continuation and destroys the dialog. */
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *button, *label, *menu, *box;
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    char *desc;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fi), NULL);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 3);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    /* Build and connect the option menu */
    menu = make_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

    /* Build and connect the account entry window */
    desc = describe_button (fi);
    label = gtk_label_new (desc);
    gnc_label_set_alignment (label, 0.5, 0.5);

    button = gtk_button_new ();
    gtk_container_add (GTK_CONTAINER (button), label);
    g_signal_connect_object (button, "clicked", G_CALLBACK (button_clicked), fe, 0);
    gtk_box_pack_start (GTK_BOX (box), button, FALSE, FALSE, 3);

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GList *l = NULL, *node, *accounts;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fi), NULL);

    accounts = selected_accounts_for_current_book (fi);
    for (node = accounts; node; node = node->next)
    {
        Account *acc = node->data;
        const GncGUID *guid = xaccAccountGetGUID (acc);
        l = g_list_prepend (l, (gpointer)guid);
    }
    l = g_list_reverse (l);
    g_list_free (accounts);

    return qof_query_guid_predicate (fi->how, l);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchAccount *se, *fse = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *se_priv, *fse_priv;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fse), NULL);
    fse_priv = _PRIVATE(fse);

    se = gnc_search_account_new ();
    se_priv = _PRIVATE(se);
    se->how = fse->how;
    se_priv->match_all = fse_priv->match_all;
    se_priv->selected_guids = g_list_copy_deep (fse_priv->selected_guids,
                                                copy_guid, NULL);
    se_priv->book_guid = fse_priv->book_guid;
    se_priv->has_book_guid = fse_priv->has_book_guid;

    return (GNCSearchCoreType *)se;
}

static void
pass_parent (GNCSearchCoreType *fe, gpointer parent)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *priv;

    g_return_if_fail (fi);
    g_return_if_fail (GNC_IS_SEARCH_ACCOUNT (fi));

    priv = _PRIVATE(fi);
    g_weak_ref_set (&priv->parent, parent);
}
