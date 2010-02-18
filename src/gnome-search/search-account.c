/*
 *  Copyright (C) 2002 Derek Atkins
 *
 *  Authors: Derek Atkins <warlord@MIT.EDU>
 *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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
#include "QueryCore.h"
#include "gnc-tree-view-account.h"
#include "gnc-gui-query.h"

#include "search-account.h"
#include "search-core-utils.h"

#define d(x)

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe);
static gboolean gncs_validate (GNCSearchCoreType *fe);
static GtkWidget *gncs_get_widget(GNCSearchCoreType *fe);
static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe);

static void gnc_search_account_class_init	(GNCSearchAccountClass *class);
static void gnc_search_account_init	(GNCSearchAccount *gspaper);
static void gnc_search_account_finalize	(GObject *obj);

typedef struct _GNCSearchAccountPrivate GNCSearchAccountPrivate;

struct _GNCSearchAccountPrivate
{
    gboolean	match_all;
    GList *	selected_accounts;
};

#define _PRIVATE(o) \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_SEARCH_ACCOUNT, GNCSearchAccountPrivate))

static GNCSearchCoreTypeClass *parent_class;


GType
gnc_search_account_get_type (void)
{
    static GType type = 0;

    if (!type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCSearchAccountClass),    /* class_size */
            NULL,   				/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_search_account_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof(GNCSearchAccount),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_search_account_init,
        };

        type = g_type_register_static (GNC_TYPE_SEARCH_CORE_TYPE,
                                       "GNCSearchAccount",
                                       &type_info, 0);
    }

    return type;
}

static void
gnc_search_account_class_init (GNCSearchAccountClass *class)
{
    GObjectClass *object_class;
    GNCSearchCoreTypeClass *gnc_search_core_type = (GNCSearchCoreTypeClass *)class;

    object_class = G_OBJECT_CLASS (class);
    parent_class = g_type_class_peek_parent (class);

    object_class->finalize = gnc_search_account_finalize;

    /* override methods */
    gnc_search_core_type->validate = gncs_validate;
    gnc_search_core_type->get_widget = gncs_get_widget;
    gnc_search_core_type->get_predicate = gncs_get_predicate;
    gnc_search_core_type->clone = gncs_clone;

    g_type_class_add_private(class, sizeof(GNCSearchAccountPrivate));
}

static void
gnc_search_account_init (GNCSearchAccount *o)
{
    o->how = GUID_MATCH_ANY;
}

static void
gnc_search_account_finalize (GObject *obj)
{
    GNCSearchAccount *o = (GNCSearchAccount *)obj;
    g_assert (IS_GNCSEARCH_ACCOUNT (o));

    G_OBJECT_CLASS (parent_class)->finalize(obj);
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
    o->how = GUID_MATCH_ALL;
    return o;
}

static gboolean
gncs_validate (GNCSearchCoreType *fe)
{
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *priv;
    gboolean valid = TRUE;

    g_return_val_if_fail (fi, FALSE);
    g_return_val_if_fail (IS_GNCSEARCH_ACCOUNT (fi), FALSE);

    priv = _PRIVATE(fi);
    if (priv->selected_accounts == NULL && fi->how )
    {
        valid = FALSE;
        gnc_error_dialog (NULL, "%s", _("You have not selected any accounts"));
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
        gnc_combo_box_search_add(combo, _("matches all accounts"), GUID_MATCH_ALL);
        initial = GUID_MATCH_ALL;
    }
    else
    {
        gnc_combo_box_search_add(combo, _("matches any account"), GUID_MATCH_ANY);
        gnc_combo_box_search_add(combo, _("matches no accounts"), GUID_MATCH_NONE);
        initial = GUID_MATCH_ANY;
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
    GtkWidget *accounts_scroller;
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
                   0,
                   GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                   GTK_STOCK_OK, GTK_RESPONSE_OK,
                   NULL));

    /* Put the dialog together */
    gtk_box_pack_start ((GtkBox *)dialog->vbox, label,
                        FALSE, FALSE, 3);
    gtk_box_pack_start ((GtkBox *)dialog->vbox, accounts_scroller,
                        TRUE, TRUE, 3);

    gtk_widget_show_all (GTK_WIDGET (dialog));

    /* Now run the dialog */
    if (gtk_dialog_run (dialog) == GTK_RESPONSE_OK)
    {
        if (priv->selected_accounts)
            g_list_free (priv->selected_accounts);

        priv->selected_accounts =
            gnc_tree_view_account_get_selected_accounts (GNC_TREE_VIEW_ACCOUNT (account_tree));

        desc = describe_button (fi);
        gtk_label_set_text (GTK_LABEL (GTK_BIN (button)->child), desc);
    }

    gtk_widget_destroy (GTK_WIDGET (dialog));
}

static GtkWidget *
gncs_get_widget (GNCSearchCoreType *fe)
{
    GtkWidget *button, *label, *menu, *box;
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    char *desc;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_ACCOUNT (fi), NULL);

    box = gtk_hbox_new (FALSE, 3);

    /* Build and connect the option menu */
    menu = make_menu (fe);
    gtk_box_pack_start (GTK_BOX (box), menu, FALSE, FALSE, 3);

    /* Build and connect the account entry window */
    desc = describe_button (fi);
    label = gtk_label_new (desc);
    gtk_misc_set_alignment (GTK_MISC (label), 0.5, 0.5);

    button = gtk_button_new ();
    gtk_container_add (GTK_CONTAINER (button), label);
    g_signal_connect (G_OBJECT (button), "clicked", G_CALLBACK (button_clicked), fe);
    gtk_box_pack_start (GTK_BOX (box), button, FALSE, FALSE, 3);

    /* And return the box */
    return box;
}

static QueryPredData_t gncs_get_predicate (GNCSearchCoreType *fe)
{
    GNCSearchAccountPrivate *priv;
    GNCSearchAccount *fi = (GNCSearchAccount *)fe;
    GList *l = NULL, *node;

    g_return_val_if_fail (fi, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_ACCOUNT (fi), NULL);

    priv = _PRIVATE(fi);
    for (node = priv->selected_accounts; node; node = node->next)
    {
        Account *acc = node->data;
        const GUID *guid = xaccAccountGetGUID (acc);
        l = g_list_prepend (l, (gpointer)guid);
    }
    l = g_list_reverse (l);

    return gncQueryGUIDPredicate (fi->how, l);
}

static GNCSearchCoreType *gncs_clone(GNCSearchCoreType *fe)
{
    GNCSearchAccount *se, *fse = (GNCSearchAccount *)fe;
    GNCSearchAccountPrivate *se_priv, *fse_priv;

    g_return_val_if_fail (fse, NULL);
    g_return_val_if_fail (IS_GNCSEARCH_ACCOUNT (fse), NULL);
    fse_priv = _PRIVATE(fse);

    se = gnc_search_account_new ();
    se_priv = _PRIVATE(se);
    se->how = fse->how;
    se_priv->match_all = fse_priv->match_all;
    se_priv->selected_accounts = g_list_copy (fse_priv->selected_accounts);

    return (GNCSearchCoreType *)se;
}
