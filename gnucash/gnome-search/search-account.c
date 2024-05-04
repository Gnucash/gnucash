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
    GList *	selected_accounts;
    GtkWindow *parent;
};

G_DEFINE_TYPE_WITH_PRIVATE(GNCSearchAccount, gnc_search_account, GNC_TYPE_SEARCH_CORE_TYPE)

#define _PRIVATE(o) \
   ((GNCSearchAccountPrivate*)gnc_search_account_get_instance_private((GNCSearchAccount*)o))

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
}

static void
gnc_search_account_finalize (GObject *obj)
{
    GNCSearchAccount *o = (GNCSearchAccount *)obj;
    g_assert (GNC_IS_SEARCH_ACCOUNT (o));

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

    if (priv->selected_accounts == NULL && fi->how )
    {
        valid = FALSE;
        gnc_error_dialog (GTK_WINDOW(priv->parent), "%s", _("You have not selected any accounts"));
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
    if (priv->selected_accounts)
        return (_("Selected Accounts"));
    return (_("Choose Accounts"));
}

static void
button_clicked (GtkButton *button, GNCSearchAccount *fi)
{
    GNCSearchAccountPrivate *priv;
    GtkDialog *dialog;
    GtkWidget *account_tree;
    GtkWidget *scrolled_window;
    GtkWidget *label;
    char *desc;
    GtkTreeSelection *selection;

    /* Create the account tree */
    account_tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(account_tree), FALSE);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(account_tree));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    /* Select the currently-selected accounts */
    priv = _PRIVATE(fi);
    if (priv->selected_accounts)
        gnc_tree_view_account_set_selected_accounts (GNC_TREE_VIEW_ACCOUNT(account_tree),
                priv->selected_accounts, FALSE);

    /* Create the account scroller and put the tree in it */
    scrolled_window = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW(scrolled_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_WIDGET(account_tree));
    gtk_widget_set_size_request(GTK_WIDGET(scrolled_window), 300, 300);

    /* Create the label */
    label = gtk_label_new (_("Select Accounts to Match"));

    /* Create the dialog */
    dialog =
        GTK_DIALOG(gtk_dialog_new_with_buttons(_("Select the Accounts to Compare"),
                   GTK_WINDOW(priv->parent),
                   0,
                   _("_Cancel"), GTK_RESPONSE_CANCEL,
                   _("_OK"), GTK_RESPONSE_OK,
                   NULL));

    /* Put the dialog together */
    gtk_box_append (GTK_BOX(gtk_dialog_get_content_area (dialog)), GTK_WIDGET(label));
    gtk_box_set_spacing (GTK_BOX(gtk_dialog_get_content_area (dialog)), 3);

    gtk_box_append (GTK_BOX(gtk_dialog_get_content_area (dialog)), GTK_WIDGET(scrolled_window));
    gtk_box_set_spacing (GTK_BOX(gtk_dialog_get_content_area (dialog)), 3);

//FIXME gtk4    gtk_widget_show_all (GTK_WIDGET (dialog));

    /* Now run the dialog */

//FIXME gtk4    if (gtk_dialog_run (dialog) == GTK_RESPONSE_OK)
gtk_window_set_modal (GTK_WINDOW(dialog), TRUE); //FIXME gtk4

//    if (gtk_dialog_run (dialog) == GTK_RESPONSE_OK)
//    {
//        if (priv->selected_accounts)
//            g_list_free (priv->selected_accounts);

//        priv->selected_accounts =
//            gnc_tree_view_account_get_selected_accounts (GNC_TREE_VIEW_ACCOUNT (account_tree));

//        desc = describe_button (fi);
//        gtk_label_set_text (GTK_LABEL (gtk_check_button_get_child (GTK_CHECK_BUTTON(button))), desc);
//    }

//FIXME gtk4    gtk_window_destroy (GTK_WINDOW(dialog));
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
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(menu));

    /* Build and connect the account entry window */
    desc = describe_button (fi);
    label = gtk_label_new (desc);
    gnc_label_set_alignment (label, 0.5, 0.5);

    button = gtk_button_new ();
    gtk_box_prepend (GTK_BOX(button), GTK_WIDGET(label));
    g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (button_clicked), fe);
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(button));

    /* And return the box */
    return box;
}

static QofQueryPredData* gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchAccountPrivate *priv;
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GList *l = NULL, *node;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (GNC_IS_SEARCH_ACCOUNT (fi), NULL);

    priv = _PRIVATE(fi);
    for (node = priv->selected_accounts; node; node = node->next)
    {
        Account *acc = node->data;
        const GncGUID *guid = xaccAccountGetGUID (acc);
        l = g_list_prepend (l, (gpointer)guid);
    }
    l = g_list_reverse (l);

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
    se_priv->selected_accounts = g_list_copy (fse_priv->selected_accounts);

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
    priv->parent = GTK_WINDOW(parent);
}
