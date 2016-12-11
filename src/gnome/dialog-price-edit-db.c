/********************************************************************\
 * dialog-price-editor.c -- price selector dialog                   *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 * Copyright (C) 2003,2005 David Hampton                            *
 * Copyright (C) 2011 Robert Fewell                                 *
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
#include <libguile.h>
#include "guile-mappings.h"
#include <time.h>

#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-general-select.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-pricedb.h"
#include "gnc-session.h"
#include "gnc-tree-view-price.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnome-utils/gnc-warnings.h"
#include "guile-util.h"
#include "engine-helpers-guile.h"
#include "swig-runtime.h"


#define DIALOG_PRICE_DB_CM_CLASS "dialog-price-edit-db"
#define STATE_SECTION "dialogs/edit_prices"
#define GNC_PREFS_GROUP "dialogs.pricedb-editor"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;


void gnc_prices_dialog_window_destroy_cb (GtkWidget *object, gpointer data);
void gnc_prices_dialog_close_cb (GtkDialog *dialog, gpointer data);
void gnc_prices_dialog_response (GtkDialog *dialog, gint response_id, gpointer data);
void gnc_prices_dialog_edit_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_remove_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_remove_old_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_add_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_get_quotes_clicked (GtkWidget *widget, gpointer data);


typedef struct
{
    GtkWidget * dialog;
    QofSession *session;
    QofBook *book;
    GNCPriceDB *price_db;

    GncTreeViewPrice * price_tree;

    GtkWidget * edit_button;
    GtkWidget * remove_button;
} PricesDialog;


void
gnc_prices_dialog_window_destroy_cb (GtkWidget *object, gpointer data)
{
    PricesDialog *pdb_dialog = data;

    ENTER(" ");
    gnc_unregister_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);

    if (pdb_dialog->dialog)
    {
        gtk_widget_destroy(pdb_dialog->dialog);
        pdb_dialog->dialog = NULL;
    }

    g_free (pdb_dialog);
    LEAVE(" ");
}


void
gnc_prices_dialog_close_cb (GtkDialog *dialog, gpointer data)
{
    PricesDialog *pdb_dialog = data;

    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);
    LEAVE(" ");
}


void
gnc_prices_dialog_response (GtkDialog *dialog, gint response_id, gpointer data)
{
    PricesDialog *pdb_dialog = data;

    ENTER(" ");
    gnc_close_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);
    LEAVE(" ");
}


void
gnc_prices_dialog_edit_clicked (GtkWidget *widget, gpointer data)
{
    PricesDialog *pdb_dialog = data;
    GList *price_list;

    ENTER(" ");
    price_list = gnc_tree_view_price_get_selected_prices(pdb_dialog->price_tree);
    if (!price_list)
    {
        LEAVE("no price selected");
        return;
    }
    if (g_list_next(price_list))
    {
        g_list_free(price_list);
        LEAVE("too many prices selected");
        return;
    }

    gnc_price_edit_dialog (pdb_dialog->dialog, pdb_dialog->session,
                           price_list->data, GNC_PRICE_EDIT);
    g_list_free(price_list);
    LEAVE(" ");
}


static void
remove_helper(GNCPrice *price, GNCPriceDB *pdb)
{
    gnc_pricedb_remove_price (pdb, price);
}


void
gnc_prices_dialog_remove_clicked (GtkWidget *widget, gpointer data)
{
    PricesDialog *pdb_dialog = data;
    GList *price_list;
    gint length, response;
    GtkWidget *dialog;

    ENTER(" ");
    price_list = gnc_tree_view_price_get_selected_prices(pdb_dialog->price_tree);
    if (!price_list)
    {
        LEAVE("no price selected");
        return;
    }

    length = g_list_length(price_list);
    if (length > 0)
    {
        gchar *message;

        message = g_strdup_printf
                  (/* Translators: %d is the number of prices. This is a ngettext(3) message. */
                      ngettext("Are you sure you want to delete the selected price?",
                               "Are you sure you want to delete the %d selected prices?",
                               length),
                      length);
        dialog = gtk_message_dialog_new(GTK_WINDOW(pdb_dialog->dialog),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_QUESTION,
                                        GTK_BUTTONS_NONE,
                                        "%s", _("Delete prices?"));
        gtk_message_dialog_format_secondary_text(GTK_MESSAGE_DIALOG(dialog),
                "%s", message);
        g_free(message);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                               GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                               GTK_STOCK_DELETE, GTK_RESPONSE_YES,
                               (gchar *)NULL);
        gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_YES);
        response = gnc_dialog_run(GTK_DIALOG(dialog), GNC_PREF_WARN_PRICE_QUOTES_DEL);
        gtk_widget_destroy(dialog);
    }
    else
    {
        response = GTK_RESPONSE_YES;
    }

    if (response == GTK_RESPONSE_YES)
    {
        g_list_foreach(price_list, (GFunc)remove_helper, pdb_dialog->price_db);
    }
    g_list_free(price_list);
    LEAVE(" ");
}


/** Enumeration for the price delete list-store */
enum GncPriceColumn {PRICED_FULL_NAME, PRICED_COMM, PRICED_DATE, PRICED_COUNT};

static Timespec
gnc_prices_dialog_load_view (GtkTreeView *view, GNCPriceDB *pdb)
{
    GtkTreeModel *model = gtk_tree_view_get_model (view);
    const gnc_commodity_table *commodity_table = gnc_get_current_commodities ();
    gnc_commodity *tmp_commodity = NULL;
    char  *tmp_namespace = NULL;
    GList *commodity_list = NULL;
    GList *namespace_list = gnc_commodity_table_get_namespaces (commodity_table);
    GtkTreeIter iter;

    Timespec oldest_ts = timespec_now ();
    oldest_ts.tv_nsec = 0;

    namespace_list = g_list_first (namespace_list);
    while (namespace_list != NULL)
    {
        tmp_namespace = namespace_list->data;
        DEBUG("Looking at namespace %s", tmp_namespace);
        commodity_list = gnc_commodity_table_get_commodities (commodity_table, tmp_namespace);
        commodity_list  = g_list_first (commodity_list);
        while (commodity_list != NULL)
        {
            gint num = 0;
            tmp_commodity = commodity_list->data;
            DEBUG("Looking at commodity %s, Number of prices %d", gnc_commodity_get_fullname (tmp_commodity),
                    gnc_pricedb_num_prices (pdb, tmp_commodity));

            num = gnc_pricedb_num_prices (pdb, tmp_commodity);

            if (num > 0)
            {
                PriceList *list = gnc_pricedb_get_prices (pdb, tmp_commodity, NULL);
                GList *nodef = g_list_last (list);
                GNCPrice *pricef = (GNCPrice*)nodef->data;
                gchar *name_str, *date_str, *num_str;
                Timespec price_ts;

                name_str = g_strconcat (tmp_namespace, " : ", gnc_commodity_get_fullname (tmp_commodity), NULL);

                price_ts = gnc_price_get_time (pricef);
                if (timespec_cmp(&oldest_ts, &price_ts) >= 0)
                    oldest_ts.tv_sec = price_ts.tv_sec;

                date_str = g_strdup (gnc_print_date (price_ts));
                num_str = g_strdup_printf ("%d", num);

                gtk_list_store_append (GTK_LIST_STORE(model), &iter);

                gtk_list_store_set (GTK_LIST_STORE(model), &iter, PRICED_FULL_NAME, name_str,
                                    PRICED_COMM, tmp_commodity, PRICED_DATE, date_str, PRICED_COUNT, num_str, -1);

                g_free (name_str);
                g_free (date_str);
                g_free (num_str);
                gnc_price_unref (pricef);
            }
            commodity_list = g_list_next (commodity_list);
        }
        namespace_list = g_list_next (namespace_list);
    }
    g_list_free (commodity_list);
    g_list_free (namespace_list);

    return oldest_ts;
}


static GList *
gnc_prices_dialog_get_commodity (GtkTreeView *view)
{
    GList            *list, *row;
    GList            *comm_list = NULL;
    GtkTreeModel     *model;
    GtkTreeIter       iter;
    GtkTreeSelection *selection;
    gnc_commodity    *comm;

    model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(view));

    list = gtk_tree_selection_get_selected_rows (selection, &model);

    // Walk the list
    for (row = g_list_first (list); row; row = g_list_next (row))
    {
	if (gtk_tree_model_get_iter (model, &iter, row->data))
        {
            gtk_tree_model_get (model, &iter, PRICED_COMM, &comm, -1);
            comm_list = g_list_append (comm_list, comm);
        }
    }
    g_list_foreach (list, (GFunc) gtk_tree_path_free, NULL);
    g_list_free (list);

    return comm_list;
}


static gboolean
are_you_sure (PricesDialog *pdb_dialog)
{
    GtkWidget   *dialog;
    gint         response;
    const char  *title = _("Are you sure you want to delete these prices ?");

    dialog = gtk_message_dialog_new (GTK_WINDOW (pdb_dialog->dialog),
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_QUESTION,
                                     GTK_BUTTONS_CANCEL,
                                     "%s", title);

    gtk_dialog_add_button (GTK_DIALOG(dialog), _("_Delete"), GTK_RESPONSE_ACCEPT);

    gtk_widget_grab_focus (gtk_dialog_get_widget_for_response (GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT));

    response = gtk_dialog_run (GTK_DIALOG(dialog));

    gtk_widget_destroy (dialog);

    if (response == GTK_RESPONSE_ACCEPT)
        return TRUE;
    else
        return FALSE;
}


void
gnc_prices_dialog_remove_old_clicked (GtkWidget *widget, gpointer data)
{
    PricesDialog *pdb_dialog = data;
    GtkBuilder *builder;
    GtkWidget *dialog, *date, *label, *box;
    GtkWidget *button, *button1, *button2;
    GtkTreeView *view;
    GtkTreeViewColumn *tree_column;
    GtkCellRenderer   *cr;
    Timespec first_ts;
    gint result;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-price.glade", "liststore4");
    gnc_builder_add_from_file (builder, "dialog-price.glade", "Deletion Date");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Deletion Date"));

    box = GTK_WIDGET(gtk_builder_get_object (builder, "date_hbox"));
    date = gnc_date_edit_new (time (NULL), FALSE, FALSE);

    gtk_box_pack_start (GTK_BOX (box), date, TRUE, TRUE, 0);
    gtk_widget_show (date);
    gtk_entry_set_activates_default(GTK_ENTRY(GNC_DATE_EDIT(date)->date_entry), TRUE);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "date_label"));
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT(date), label);

    // Setup the commodity view
    view = GTK_TREE_VIEW(gtk_builder_get_object (builder, "commodty_treeview"));
    gtk_tree_view_set_rules_hint (view, TRUE);
    gtk_tree_selection_set_mode (gtk_tree_view_get_selection (view), GTK_SELECTION_MULTIPLE);

    // Add Entries column this way as align does not seem to work from builder
    tree_column = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title (tree_column, _("Entries"));
    gtk_tree_view_append_column (GTK_TREE_VIEW(view), tree_column);
    gtk_tree_view_column_set_alignment (tree_column, 0.5);
    gtk_tree_view_column_set_expand (tree_column, TRUE);
    cr = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start (tree_column, cr, TRUE);
    // set 'xalign' property of the cell renderer
    gtk_tree_view_column_set_attributes (tree_column, cr, "text", PRICED_COUNT, NULL);
    gtk_cell_renderer_set_alignment (cr, 0.5, 0.5);

    // Load the view and get the earliest date
    first_ts = gnc_prices_dialog_load_view (view, pdb_dialog->price_db);
    gtk_tree_selection_select_all (gtk_tree_view_get_selection (view));

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, pdb_dialog);
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (pdb_dialog->dialog));

    result = gtk_dialog_run (GTK_DIALOG (dialog));
    if (result == GTK_RESPONSE_OK)
    {
        GList *comm_list = gnc_prices_dialog_get_commodity (view);

        // Are you sure you want to delete the entries and we have commodities
        if ((are_you_sure (pdb_dialog) == TRUE) && (g_list_length (comm_list) != 0))
        {
            Timespec last_ts;
            gchar *source, *leave;
            gboolean fq, user, all;

            DEBUG("deleting prices");
            last_ts.tv_sec = gnc_date_edit_get_date (GNC_DATE_EDIT (date));
            last_ts.tv_nsec = 0;

            // Get the buttons
            button = GTK_WIDGET(gtk_builder_get_object (builder, "checkbutton_all"));
            all = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));
            button = GTK_WIDGET(gtk_builder_get_object (builder, "checkbutton_fq"));
            fq = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));
            button = GTK_WIDGET(gtk_builder_get_object (builder, "checkbutton_user"));
            user = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button));
            if (all)
                source = "all";
            else
            { 
                if (fq && user)
                    source = "fq-user";
                else if (user)
                    source = "user";
                else
                    source = "fq";
            }
            button1 = GTK_WIDGET(gtk_builder_get_object (builder, "radiobutton_month"));
            button2 = GTK_WIDGET(gtk_builder_get_object (builder, "radiobutton_week"));
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button1)))
                leave = "month";
            else if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(button2)))
                leave = "week";
            else
                leave = "none";

            gnc_pricedb_remove_old_prices (pdb_dialog->price_db, comm_list,
                                           first_ts, last_ts, source, leave);
        }
        g_list_free (comm_list);
    }
    gtk_widget_destroy (dialog);
    LEAVE(" ");
}


void
gnc_prices_dialog_add_clicked (GtkWidget *widget, gpointer data)
{
    PricesDialog *pdb_dialog = data;
    GNCPrice *price = NULL;
    GList *price_list;

    ENTER(" ");
    price_list = gnc_tree_view_price_get_selected_prices(pdb_dialog->price_tree);
    if (price_list)
    {
        price = price_list->data;
        g_list_free(price_list);
    }
    gnc_price_edit_dialog (pdb_dialog->dialog, pdb_dialog->session,
                           price, GNC_PRICE_NEW);
    LEAVE(" ");
}


void
gnc_prices_dialog_get_quotes_clicked (GtkWidget *widget, gpointer data)
{
    PricesDialog *pdb_dialog = data;
    SCM quotes_func;
    SCM book_scm;
    SCM scm_window;

    ENTER(" ");
    quotes_func = scm_c_eval_string ("gnc:book-add-quotes");
    if (!scm_is_procedure (quotes_func))
    {
        LEAVE(" no procedure");
        return;
    }

    book_scm = gnc_book_to_scm (pdb_dialog->book);
    if (scm_is_true (scm_not (book_scm)))
    {
        LEAVE("no book");
        return;
    }

    scm_window =  SWIG_NewPointerObj(pdb_dialog->dialog,
                                     SWIG_TypeQuery("_p_GtkWidget"), 0);

    gnc_set_busy_cursor (NULL, TRUE);
    scm_call_2 (quotes_func, scm_window, book_scm);
    gnc_unset_busy_cursor (NULL);

    /* Without this, the summary bar on the accounts tab
     * won't reflect the new prices (bug #522095). */
    gnc_gui_refresh_all ();

    LEAVE(" ");
}


static void
gnc_prices_dialog_selection_changed (GtkTreeSelection *treeselection,
                                     gpointer data)
{
    PricesDialog *pdb_dialog = data;
    GList *price_list;
    gint length;

    ENTER(" ");
    price_list = gnc_tree_view_price_get_selected_prices(pdb_dialog->price_tree);
    length = g_list_length(price_list);

    gtk_widget_set_sensitive (pdb_dialog->edit_button,
                              length == 1);
    gtk_widget_set_sensitive (pdb_dialog->remove_button,
                              length >= 1);
    LEAVE("%d prices selected", length);
}


static gboolean
gnc_price_dialog_filter_ns_func (gnc_commodity_namespace *name_space,
                                 gpointer data)
{
    PricesDialog *pdb_dialog = data;
    const gchar *name;
    static GList *cm_list;
    GList *item;

    /* Never show the template list */
    name = gnc_commodity_namespace_get_name (name_space);
    if (g_strcmp0 (name, "template") == 0)
        return FALSE;

    /* See if this namespace has commodities */
    cm_list = gnc_commodity_namespace_get_commodity_list(name_space);
    for (item = cm_list; item; item = g_list_next(item))
    {

        /* For each commodity, see if there are prices */
        if (gnc_pricedb_has_prices(pdb_dialog->price_db, item->data, NULL))
        {
            return TRUE;
        }
    }

    //  printf("Namespace %s not visible\n", name);
    return FALSE;
}


static gboolean
gnc_price_dialog_filter_cm_func (gnc_commodity *commodity,
                                 gpointer data)
{
    PricesDialog *pdb_dialog = data;

    /* Show any commodity that has prices */
    return gnc_pricedb_has_prices(pdb_dialog->price_db, commodity, NULL);
}


static void
row_activated_cb (GtkTreeView *view, GtkTreePath *path,
                  GtkTreeViewColumn *column, gpointer data)
{
    GtkTreeModel *model;
    GtkTreeIter iter;

    g_return_if_fail(view);

    model = gtk_tree_view_get_model(view);
    if (gtk_tree_model_get_iter(model, &iter, path))
    {
        if (gtk_tree_model_iter_has_child(model, &iter))
        {
            /* There are children, so it's not a price.
             * Just expand or collapse the row. */
            if (gtk_tree_view_row_expanded(view, path))
                gtk_tree_view_collapse_row(view, path);
            else
                gtk_tree_view_expand_row(view, path, FALSE);
        }
        else
            /* It's a price, so click the Edit button. */
            gnc_prices_dialog_edit_clicked(GTK_WIDGET(view), data);
    }
}


static void
gnc_prices_dialog_create (GtkWidget * parent, PricesDialog *pdb_dialog)
{
    GtkWidget *dialog, *scrolled_window;
    GtkBuilder *builder;
    GtkTreeView *view;
    GtkTreeSelection *selection;

    ENTER(" ");
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-price.glade", "Prices Dialog");

    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Prices Dialog"));
    pdb_dialog->dialog = dialog;

    pdb_dialog->session = gnc_get_current_session();
    pdb_dialog->book = qof_session_get_book(pdb_dialog->session);
    pdb_dialog->price_db = gnc_pricedb_get_db(pdb_dialog->book);

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    /* default to 'close' button */
    gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);

    /* price tree */
    scrolled_window = GTK_WIDGET(gtk_builder_get_object (builder, "price_list_window"));
    view = gnc_tree_view_price_new(pdb_dialog->book,
                                   "state-section", STATE_SECTION,
                                   "show-column-menu", TRUE,
                                   NULL);
    pdb_dialog->price_tree = GNC_TREE_VIEW_PRICE(view);
    gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(view));
    gnc_tree_view_price_set_filter (pdb_dialog->price_tree,
                                    gnc_price_dialog_filter_ns_func,
                                    gnc_price_dialog_filter_cm_func,
                                    NULL,
                                    pdb_dialog, NULL);

    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_prices_dialog_selection_changed), pdb_dialog);

    g_signal_connect (G_OBJECT (view), "row-activated",
                      G_CALLBACK (row_activated_cb), pdb_dialog);

    /* buttons */
    {
        GtkWidget *button;

        button = GTK_WIDGET(gtk_builder_get_object (builder, "edit_button"));
        pdb_dialog->edit_button = button;

        button = GTK_WIDGET(gtk_builder_get_object (builder, "remove_button"));
        pdb_dialog->remove_button = button;

        if (!gnc_quote_source_fq_installed())
        {
            button = GTK_WIDGET(gtk_builder_get_object (builder, "get_quotes_button"));
            gtk_widget_set_sensitive(button, FALSE);
        }
    }

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, pdb_dialog);

    g_object_unref(G_OBJECT(builder));

    gnc_restore_window_size(GNC_PREFS_GROUP, GTK_WINDOW(pdb_dialog->dialog));
    LEAVE(" ");
}


static void
close_handler (gpointer user_data)
{
    PricesDialog *pdb_dialog = user_data;

    ENTER(" ");
    gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(pdb_dialog->dialog));

    gtk_widget_destroy (GTK_WIDGET (pdb_dialog->dialog));
    LEAVE(" ");
}


static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
    ENTER(" ");
    LEAVE(" ");
}


static gboolean
show_handler (const char *klass, gint component_id,
              gpointer user_data, gpointer iter_data)
{
    PricesDialog *pdb_dialog = user_data;

    ENTER(" ");
    if (!pdb_dialog)
    {
        LEAVE("no data strucure");
        return(FALSE);
    }

    gtk_window_present (GTK_WINDOW(pdb_dialog->dialog));
    LEAVE(" ");
    return(TRUE);
}


/********************************************************************\
 * gnc_prices_dialog                                                *
 *   opens up a window showing all price information                *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_prices_dialog (GtkWidget * parent)
{
    PricesDialog *pdb_dialog;
    gint component_id;

    ENTER(" ");
    if (gnc_forall_gui_components (DIALOG_PRICE_DB_CM_CLASS, show_handler, NULL))
    {
        LEAVE("existing dialog raised");
        return;
    }

    pdb_dialog = g_new0 (PricesDialog, 1);

    gnc_prices_dialog_create (parent, pdb_dialog);

    component_id = gnc_register_gui_component (DIALOG_PRICE_DB_CM_CLASS,
                   refresh_handler, close_handler,
                   pdb_dialog);
    gnc_gui_component_set_session (component_id, pdb_dialog->session);

    gtk_widget_grab_focus (GTK_WIDGET(pdb_dialog->price_tree));

    gtk_widget_show (pdb_dialog->dialog);
    LEAVE(" ");
}
