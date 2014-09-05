/********************************************************************\
 * dialog-commodities.c -- commodities dialog                       *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 * Copyright (C) 2003,2005 David Hampton                            *
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

#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-tree-view-commodity.h"
#include "gnc-prefs.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-gnome-utils.h"
#include "gnc-session.h"
#include "gnome-utils/gnc-warnings.h"


#define DIALOG_COMMODITIES_CM_CLASS "dialog-commodities"
#define STATE_SECTION "dialogs/edit_commodities"
#define GNC_PREFS_GROUP   "dialogs.commodities"
#define GNC_PREF_INCL_ISO "include-iso"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
    GtkWidget * dialog;
    QofSession *session;
    QofBook *book;

    GncTreeViewCommodity * commodity_tree;
    GtkWidget * edit_button;
    GtkWidget * remove_button;
    gboolean    show_currencies;

    gboolean is_new;
} CommoditiesDialog;


void gnc_commodities_window_destroy_cb (GtkObject *object, CommoditiesDialog *cd);
void gnc_commodities_dialog_response (GtkDialog *dialog, gint response, CommoditiesDialog *cd);
void gnc_commodities_show_currencies_toggled (GtkToggleButton *toggle, CommoditiesDialog *cd);



void
gnc_commodities_window_destroy_cb (GtkObject *object,   CommoditiesDialog *cd)
{
    gnc_unregister_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);

    g_free (cd);
}

static void
edit_clicked (CommoditiesDialog *cd)
{
    gnc_commodity *commodity;

    commodity = gnc_tree_view_commodity_get_selected_commodity (cd->commodity_tree);
    if (commodity == NULL)
        return;

    if (gnc_ui_edit_commodity_modal (commodity, cd->dialog))
        gnc_gui_refresh_all ();
}

static void
row_activated_cb (GtkTreeView *view, GtkTreePath *path,
                  GtkTreeViewColumn *column, CommoditiesDialog *cd)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_return_if_fail(view);

    model = gtk_tree_view_get_model(view);
    if (gtk_tree_model_get_iter(model, &iter, path))
    {
        if (gtk_tree_model_iter_has_child(model, &iter))
        {
            /* There are children, so it's not a commodity.
             * Just expand or collapse the row. */
            if (gtk_tree_view_row_expanded(view, path))
                gtk_tree_view_collapse_row(view, path);
            else
                gtk_tree_view_expand_row(view, path, FALSE);
        }
        else
            /* It's a commodity, so click the Edit button. */
            edit_clicked(cd);
    }
}

static void
remove_clicked (CommoditiesDialog *cd)
{
    GNCPriceDB *pdb;
    GList *node;
    GList *prices;
    GList *accounts;
    gboolean can_delete;
    gnc_commodity *commodity;
    GtkWidget *dialog;
    const gchar *message, *warning;
    gint response;

    commodity = gnc_tree_view_commodity_get_selected_commodity (cd->commodity_tree);
    if (commodity == NULL)
        return;

    accounts = gnc_account_get_descendants (gnc_book_get_root_account(cd->book));
    can_delete = TRUE;

    for (node = accounts; node; node = node->next)
    {
        Account *account = node->data;

        if (commodity == xaccAccountGetCommodity (account))
        {
            can_delete = FALSE;
            break;
        }
    }

    /* FIXME check for transaction references */

    if (!can_delete)
    {
        const char *message = _("That commodity is currently used by "
                                "at least one of your accounts. You may "
                                "not delete it.");

        gnc_warning_dialog (cd->dialog, "%s", message);
        g_list_free (accounts);
        return;
    }
    g_list_free (accounts);

    pdb = gnc_pricedb_get_db (cd->book);
    prices = gnc_pricedb_get_prices(pdb, commodity, NULL);
    if (prices)
    {
        message = _("This commodity has price quotes. Are "
                    "you sure you want to delete the selected "
                    "commodity and its price quotes?");
        warning = GNC_PREF_WARN_PRICE_COMM_DEL_QUOTES;
    }
    else
    {
        message = _("Are you sure you want to delete the "
                    "selected commodity?");
        warning = GNC_PREF_WARN_PRICE_COMM_DEL;
    }

    dialog = gtk_message_dialog_new(GTK_WINDOW(cd->dialog),
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_QUESTION,
                                    GTK_BUTTONS_NONE,
                                    "%s", _("Delete commodity?"));
    gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
            "%s", message);
    gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                           GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                           GTK_STOCK_DELETE, GTK_RESPONSE_OK,
                           (gchar *)NULL);
    response = gnc_dialog_run(GTK_DIALOG(dialog), warning);
    gtk_widget_destroy(dialog);

    if (response == GTK_RESPONSE_OK)
    {
        gnc_commodity_table *ct;

        ct = gnc_commodity_table_get_table (cd->book);
        for (node = prices; node; node = node->next)
            gnc_pricedb_remove_price(pdb, node->data);

        gnc_commodity_table_remove (ct, commodity);
        gnc_commodity_destroy (commodity);
        commodity = NULL;
    }

    gnc_price_list_destroy(prices);
    gnc_gui_refresh_all ();
}

static void
add_clicked (CommoditiesDialog *cd)
{
    gnc_commodity *commodity;
    const char *name_space;

    commodity = gnc_tree_view_commodity_get_selected_commodity (cd->commodity_tree);
    if (commodity)
        name_space = gnc_commodity_get_namespace (commodity);
    else
        name_space = NULL;

    commodity = gnc_ui_new_commodity_modal (name_space, cd->dialog);
}

void
gnc_commodities_dialog_response (GtkDialog *dialog,
                                 gint response,
                                 CommoditiesDialog *cd)
{
    switch (response)
    {
    case GNC_RESPONSE_NEW:
        add_clicked (cd);
        return;

    case GNC_RESPONSE_DELETE:
        remove_clicked (cd);
        return;

    case GNC_RESPONSE_EDIT:
        edit_clicked (cd);
        return;

    case GTK_RESPONSE_CLOSE:
    default:
        gnc_close_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);
        return;
    }
}

static void
gnc_commodities_dialog_selection_changed (GtkTreeSelection *selection,
        CommoditiesDialog *cd)
{
    gboolean remove_ok;
    gnc_commodity *commodity;

    commodity = gnc_tree_view_commodity_get_selected_commodity (cd->commodity_tree);
    remove_ok = commodity && !gnc_commodity_is_iso(commodity);
    gtk_widget_set_sensitive (cd->edit_button, commodity != NULL);
    gtk_widget_set_sensitive (cd->remove_button, remove_ok);
}

void
gnc_commodities_show_currencies_toggled (GtkToggleButton *toggle,
        CommoditiesDialog *cd)
{

    cd->show_currencies = gtk_toggle_button_get_active (toggle);
    gnc_tree_view_commodity_refilter (cd->commodity_tree);
}

static gboolean
gnc_commodities_dialog_filter_ns_func (gnc_commodity_namespace *name_space,
                                       gpointer data)
{
    CommoditiesDialog *cd = data;
    const gchar *name;
    GList *list;

    /* Never show the template list */
    name = gnc_commodity_namespace_get_name (name_space);
    if (g_strcmp0 (name, "template") == 0)
        return FALSE;

    /* Check whether or not to show commodities */
    if (!cd->show_currencies && gnc_commodity_namespace_is_iso(name))
        return FALSE;

    /* Show any other namespace that has commodities */
    list = gnc_commodity_namespace_get_commodity_list(name_space);
    return (list != NULL);
}

static gboolean
gnc_commodities_dialog_filter_cm_func (gnc_commodity *commodity,
                                       gpointer data)
{
    CommoditiesDialog *cd = data;

    if (cd->show_currencies)
        return TRUE;
    return !gnc_commodity_is_iso(commodity);
}

static void
gnc_commodities_dialog_create (GtkWidget * parent, CommoditiesDialog *cd)
{
    GtkWidget *button;
    GtkWidget *scrolled_window;
    GtkBuilder *builder;
    GtkTreeView *view;
    GtkTreeSelection *selection;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-commodities.glade", "Securities Dialog");

    cd->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Securities Dialog"));
    cd->session = gnc_get_current_session();
    cd->book = qof_session_get_book(cd->session);
    cd->show_currencies = gnc_prefs_get_bool(GNC_PREFS_GROUP, GNC_PREF_INCL_ISO);

    gtk_builder_connect_signals(builder, cd);

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (cd->dialog), GTK_WINDOW (parent));

    /* buttons */
    cd->remove_button = GTK_WIDGET(gtk_builder_get_object (builder, "remove_button"));
    cd->edit_button = GTK_WIDGET(gtk_builder_get_object (builder, "edit_button"));

    /* commodity tree */

    scrolled_window = GTK_WIDGET(gtk_builder_get_object (builder, "commodity_list_window"));
    view = gnc_tree_view_commodity_new(cd->book,
                                       "state-section", STATE_SECTION,
                                       "show-column-menu", TRUE,
                                       NULL);
    cd->commodity_tree = GNC_TREE_VIEW_COMMODITY(view);
    gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(view));
    gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(cd->commodity_tree), TRUE);
    gnc_tree_view_commodity_set_filter (cd->commodity_tree,
                                        gnc_commodities_dialog_filter_ns_func,
                                        gnc_commodities_dialog_filter_cm_func,
                                        cd, NULL);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_commodities_dialog_selection_changed), cd);

    g_signal_connect (G_OBJECT (cd->commodity_tree), "row-activated",
                      G_CALLBACK (row_activated_cb), cd);

    /* Show currency button */
    button = GTK_WIDGET(gtk_builder_get_object (builder, "show_currencies_button"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), cd->show_currencies);

    g_object_unref(G_OBJECT(builder));
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW(cd->dialog));
}

static void
close_handler (gpointer user_data)
{
    CommoditiesDialog *cd = user_data;

    gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(cd->dialog));

    gnc_prefs_set_bool(GNC_PREFS_GROUP, GNC_PREF_INCL_ISO, cd->show_currencies);

    gtk_widget_destroy(cd->dialog);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    CommoditiesDialog *cd = user_data;

    g_return_if_fail(cd != NULL);

    gnc_tree_view_commodity_refilter (cd->commodity_tree);
}

static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    CommoditiesDialog *cd = user_data;

    if (!cd)
        return(FALSE);
    gtk_window_present (GTK_WINDOW(cd->dialog));
    return(TRUE);
}

/********************************************************************\
 * gnc_commodities_dialog                                           *
 *   opens up a window to edit price information                    *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_commodities_dialog (GtkWidget * parent)
{
    CommoditiesDialog *cd;
    gint component_id;

    if (gnc_forall_gui_components (DIALOG_COMMODITIES_CM_CLASS,
                                   show_handler, NULL))
        return;

    cd = g_new0 (CommoditiesDialog, 1);

    gnc_commodities_dialog_create (parent, cd);

    component_id = gnc_register_gui_component (DIALOG_COMMODITIES_CM_CLASS,
                   refresh_handler, close_handler,
                   cd);
    gnc_gui_component_set_session (component_id, cd->session);

    gtk_widget_grab_focus (GTK_WIDGET(cd->commodity_tree));

    gtk_widget_show (cd->dialog);
}
