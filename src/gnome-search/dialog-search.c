/*
 * dialog-search.c -- Search Dialog
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-query-view.h"
#include "gnc-prefs.h"
#include "gnc-session.h"
#include "qof.h"
#include "engine-helpers.h"
#include "qofbookslots.h"

#include "Transaction.h"	/* for the SPLIT_* and TRANS_* */

#include "dialog-search.h"
#include "search-core-type.h"
#include "search-param.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

#define DIALOG_SEARCH_CM_CLASS "dialog-search"
#define GNC_PREFS_GROUP_SEARCH_GENERAL "dialogs.search"
#define GNC_PREF_NEW_SEARCH_LIMIT  "new-search-limit"
#define GNC_PREF_ACTIVE_ONLY       "search-for-active-only"

typedef enum
{
    GNC_SEARCH_MATCH_ALL = 0,
    GNC_SEARCH_MATCH_ANY = 1
} GNCSearchType;

enum search_cols
{
    SEARCH_COL_NAME = 0,
    SEARCH_COL_POINTER,
    NUM_SEARCH_COLS
};

struct _GNCSearchWindow
{
    GtkWidget               *dialog;
    GtkWidget               *grouping_combo;
    GtkWidget               *match_all_label;
    GtkWidget               *criteria_table;
    GtkWidget               *result_hbox;

    /* The "results" sub-window widgets */
    GtkWidget               *result_view;
    gpointer                 selected_item;
    GList                   *selected_item_list;

    /* The search_type radio-buttons */
    GtkWidget               *new_rb;
    GtkWidget               *narrow_rb;
    GtkWidget               *add_rb;
    GtkWidget               *del_rb;
    GtkWidget               *active_only_check;

    /* The Select button */
    GtkWidget               *select_button;
    GList                   *button_list;

    /* The close/cancel buttons */
    GtkWidget               *close_button;
    GtkWidget               *cancel_button;

    /* Callbacks */
    GNCSearchResultCB        result_cb;
    GNCSearchNewItemCB       new_item_cb;
    GNCSearchCallbackButton *buttons;
    GNCSearchFree            free_cb;
    gpointer                 user_data;

    GNCSearchSelectedCB      selected_cb;
    gpointer                 select_arg;
    gboolean                 allow_clear;

    /* What we're searching for, and how */
    const gchar              *type_label;
    QofIdTypeConst            search_for;
    GNCSearchType             grouping;     /* Match Any, Match All */
    const QofParam           *get_guid;     /* Function to GetGUID from the object */
    int                       search_type;  /* New, Narrow, Add, Delete */

    /* Our query status */
    QofQuery                 *q;
    QofQuery                 *start_q;      /* The query to start from, if any */

    /* The list of criteria */
    GNCSearchParam           *last_param;
    GList                    *params_list;  /* List of GNCSearchParams */
    GList                    *display_list; /* List of GNCSearchParams for Display */
    gint                      num_cols;     /* Number of Display Columns */
    GList                    *crit_list;    /* List of crit_data */

    gint                      component_id;
    const gchar              *prefs_group;
};

struct _crit_data
{
    GNCSearchParam    *param;
    GNCSearchCoreType *element;
    GtkWidget         *elemwidget;
    GtkWidget         *container;
    GtkWidget         *button;
    GtkDialog         *dialog;
};

static void search_clear_criteria (GNCSearchWindow *sw);
static void gnc_search_dialog_display_results (GNCSearchWindow *sw);

static void
gnc_search_callback_button_execute (GNCSearchCallbackButton *cb,
                                    GNCSearchWindow *sw)
{
    GNCQueryView     *qview = GNC_QUERY_VIEW(sw->result_view);
    GtkTreeSelection *selection;
    GtkTreeModel     *model;
    GtkTreeIter       iter;

    // Sanity check
    g_assert(qview);
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(qview));
    g_assert(gtk_tree_selection_get_mode(selection) == GTK_SELECTION_MULTIPLE);
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(qview));

    /* Do we have a callback for multi-selections ? */
    if (cb->cb_multiselect_fn && (!cb->cb_fcn ))
    {
        /* We have allready populated the selected_item_list from the select row callback */
        // We use g_list_prepend (for performance reasons), so we have to reverse once here
        sw->selected_item_list = g_list_reverse(sw->selected_item_list);

        // Call the callback
        (cb->cb_multiselect_fn)(sw->selected_item_list, sw->user_data);
    }
    else
    {
        // No, stick to the single-item callback
        if (cb->cb_fcn)
            (cb->cb_fcn)(&(sw->selected_item), sw->user_data);
    }
}


static void
gnc_search_dialog_result_clicked (GtkButton *button, GNCSearchWindow *sw)
{
    GNCSearchCallbackButton *cb;

    cb = g_object_get_data (G_OBJECT (button), "data");
    gnc_search_callback_button_execute (cb, sw);
}


static void
gnc_search_dialog_select_buttons_enable (GNCSearchWindow *sw, gint selected)
{
    gint i;
    gboolean enable, read_only;
    GList  *blist;

    read_only = qof_book_is_readonly (gnc_get_current_book ());

    for (blist = sw->button_list; blist; blist = blist->next)
    {
        GNCSearchCallbackButton  *button_spec = g_object_get_data (G_OBJECT(blist->data) , "data");

        if(selected == 0)
        {
            gtk_widget_set_sensitive (GTK_WIDGET(blist->data), FALSE);
            continue;
        }

        if(read_only == TRUE)
        {
            if((selected > 1) && (!(button_spec->cb_multiselect_fn == NULL)) && (button_spec->sensitive_if_readonly == TRUE))
                enable = TRUE;
            else
                enable = FALSE;

            if((selected == 1) && (button_spec->sensitive_if_readonly == TRUE))
                enable = TRUE;
        }
        else
        {
            if((selected > 1) && (!(button_spec->cb_multiselect_fn == NULL)))
                enable = TRUE;
            else
                enable = FALSE;

            if(selected == 1)
                enable = TRUE;
        }
        gtk_widget_set_sensitive (GTK_WIDGET(blist->data), enable);
    }
}


static void
gnc_search_dialog_select_cb (GtkButton *button, GNCSearchWindow *sw)
{
    g_return_if_fail (sw->selected_cb);

    if (sw->selected_item == NULL && sw->allow_clear == FALSE)
    {
        char *msg = _("You must select an item from the list");
        gnc_error_dialog (sw->dialog, "%s", msg);
        return;
    }

    (sw->selected_cb)(sw->selected_item, sw->select_arg);
    gnc_search_dialog_destroy (sw);
}


static void
gnc_search_dialog_select_row_cb (GNCQueryView *qview,
                                 gpointer item,
                                 gpointer user_data)
{
    GNCSearchWindow  *sw = user_data;
    gint              number_of_rows;

    sw->selected_item_list = NULL;
    sw->selected_item = NULL;

    number_of_rows = GPOINTER_TO_INT(item);

    gnc_search_dialog_select_buttons_enable(sw, number_of_rows);

    if(number_of_rows == 1)
    {
        sw->selected_item = qview->selected_entry;
        sw->selected_item_list = qview->selected_entry_list;
    }
    else
        sw->selected_item_list = qview->selected_entry_list;
}


static void
gnc_search_dialog_double_click_cb (GNCQueryView *qview,
                                   gpointer item,
                                   gpointer user_data)
{
    GNCSearchWindow  *sw = user_data;

    sw->selected_item = item;
    if (sw->selected_cb)
        /* Select the item */
        gnc_search_dialog_select_cb (NULL, sw);
    else if (sw->buttons)
        /* Call the first button (usually view/edit) */
        gnc_search_callback_button_execute (sw->buttons, sw);

    /* If we get here, then nothing to do for a double-click */
}


static void
gnc_search_dialog_init_result_view (GNCSearchWindow *sw)
{
    GtkTreeSelection *selection;

    sw->result_view = gnc_query_view_new(sw->display_list, sw->q);

    // We want the multi-selection mode of the tree view.
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(sw->result_view));
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);

    /* Set the sort order of the tree view */
    gnc_query_sort_order(GNC_QUERY_VIEW(sw->result_view), 1, GTK_SORT_ASCENDING);

    /* Setup the list callbacks */
    g_signal_connect (GNC_QUERY_VIEW(sw->result_view), "row_selected",
                      G_CALLBACK (gnc_search_dialog_select_row_cb), sw);

    g_signal_connect (GNC_QUERY_VIEW(sw->result_view), "double_click_entry",
                      G_CALLBACK(gnc_search_dialog_double_click_cb), sw);
}


static void
gnc_search_dialog_display_results (GNCSearchWindow *sw)
{
    gdouble max_count;

    /* Check if this is the first time this is called for this window.
     * If so, then build the results sub-window, the scrolled treeview,
     * and the active buttons.
     */
    if (sw->result_view == NULL)
    {
        GtkWidget *scroller, *frame, *button_box, *button;

        /* Create the view */
        gnc_search_dialog_init_result_view (sw);

        frame = gtk_frame_new(NULL);

        /* Create the scroller and add the view to the scroller */
        scroller = gtk_scrolled_window_new (NULL, NULL);
        gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroller),
                                        GTK_POLICY_AUTOMATIC,
                                        GTK_POLICY_AUTOMATIC);
        gtk_widget_set_size_request(GTK_WIDGET(scroller), 300, 100);
        gtk_container_add (GTK_CONTAINER (scroller), sw->result_view);
        gtk_container_add(GTK_CONTAINER(frame), scroller);

        /* Create the button_box */
        button_box = gtk_vbox_new (FALSE, 3);

        /* ... and add all the buttons */
        if (sw->buttons)
        {
            int i;

            button = gtk_button_new_with_label (_("Select"));
            g_signal_connect (G_OBJECT (button), "clicked",
                              G_CALLBACK (gnc_search_dialog_select_cb), sw);
            gtk_box_pack_start (GTK_BOX (button_box), button, FALSE, FALSE, 3);
            sw->select_button = button;

            for (i = 0; sw->buttons[i].label; i++)
            {
                GNCSearchCallbackButton* button_spec = sw->buttons + i;
                button = gtk_button_new_with_label (_(button_spec->label));
                g_object_set_data (G_OBJECT (button), "data", button_spec);

                if (qof_book_is_readonly (gnc_get_current_book ()))
                    gtk_widget_set_sensitive (GTK_WIDGET(button), button_spec->sensitive_if_readonly);

                /* Save the button pointer */
                sw->button_list = g_list_append(sw->button_list, button);

                g_signal_connect (G_OBJECT (button), "clicked",
                                  G_CALLBACK (gnc_search_dialog_result_clicked), sw);
                gtk_box_pack_start (GTK_BOX (button_box), button, FALSE, FALSE, 3);
            }
        }

        /* Add the scrolled-view and button-box to the results_box */
        gtk_box_pack_end (GTK_BOX (sw->result_hbox), button_box, FALSE, FALSE, 3);
        gtk_box_pack_end (GTK_BOX (sw->result_hbox), frame, TRUE, TRUE, 3);

        /* And show the results */
        gtk_widget_show_all (sw->result_hbox);

        /* But may be hide the select button */
        if (!sw->selected_cb)
            gtk_widget_hide (sw->select_button);
    }
    else
        /* Update the query in the view */
        gnc_query_view_reset_query (GNC_QUERY_VIEW(sw->result_view), sw->q);

    /* Deselect all the select buttons and any items */
    gnc_search_dialog_select_buttons_enable (sw, 0);
    gnc_query_view_unselect_all (GNC_QUERY_VIEW(sw->result_view));

    /* set 'new search' if fewer than max_count items is returned. */
    max_count = gnc_prefs_get_float(GNC_PREFS_GROUP_SEARCH_GENERAL, GNC_PREF_NEW_SEARCH_LIMIT);
    if (gnc_query_view_get_num_entries(GNC_QUERY_VIEW(sw->result_view)) < max_count)
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (sw->new_rb), TRUE);
}


static void
match_combo_changed (GtkComboBoxText *combo_box, GNCSearchWindow *sw)
{
    sw->grouping = gtk_combo_box_get_active(GTK_COMBO_BOX(combo_box));
}


static void
search_type_cb (GtkToggleButton *button, GNCSearchWindow *sw)
{
    GSList * buttongroup = gtk_radio_button_get_group (GTK_RADIO_BUTTON(button));

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button)))
    {
        sw->search_type =
            g_slist_length (buttongroup) - g_slist_index (buttongroup, button) - 1;
    }
}


static void
search_active_only_cb (GtkToggleButton *button, GNCSearchWindow *sw)
{

    gnc_prefs_set_bool(sw->prefs_group, GNC_PREF_ACTIVE_ONLY,
                       gtk_toggle_button_get_active (button));
}


static void
search_update_query (GNCSearchWindow *sw)
{
    static GSList *active_params = NULL;
    QofQuery *q, *q2, *new_q;
    GList *node;
    QofQueryOp op;
    QofQueryPredData* pdata;

    if (sw->grouping == GNC_SEARCH_MATCH_ANY)
        op = QOF_QUERY_OR;
    else
        op = QOF_QUERY_AND;

    if (active_params == NULL)
        active_params = g_slist_prepend (NULL, QOF_PARAM_ACTIVE);

    /* Make sure we supply a book! */
    if (sw->start_q == NULL)
    {
        sw->start_q = qof_query_create_for (sw->search_for);
        qof_query_set_book (sw->start_q, gnc_get_current_book ());
    }
    else
    {
        /* We've got a query -- purge it of any "active" parameters */
        qof_query_purge_terms (sw->start_q, active_params);
    }

    /* Now create a new query to work from */
    q = qof_query_create_for (sw->search_for);

    /* Walk the list of criteria */
    for (node = sw->crit_list; node; node = node->next)
    {
        struct _crit_data *data = node->data;

        pdata = gnc_search_core_type_get_predicate (data->element);
        if (pdata)
            qof_query_add_term (q, gnc_search_param_get_param_path (data->param),
                                pdata, op);
    }

    /* Now combine this query with the existing query, depending on
     * what we want to do...  We can assume that cases 1, 2, and 3
     * already have sw->q being valid!
     */

    switch (sw->search_type)
    {
    case 0:			/* New */
        new_q = qof_query_merge (sw->start_q, q, QOF_QUERY_AND);
        qof_query_destroy (q);
        break;
    case 1:			/* Refine */
        new_q = qof_query_merge (sw->q, q, QOF_QUERY_AND);
        qof_query_destroy (q);
        break;
    case 2:			/* Add */
        new_q = qof_query_merge (sw->q, q, QOF_QUERY_OR);
        qof_query_destroy (q);
        break;
    case 3:			/* Delete */
        q2 = qof_query_invert (q);
        new_q = qof_query_merge (sw->q, q2, QOF_QUERY_AND);
        qof_query_destroy (q2);
        qof_query_destroy (q);
        break;
    default:
        g_warning ("bad search type: %d", sw->search_type);
        new_q = q;
        break;
    }

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (sw->active_only_check)))
    {
        qof_query_add_boolean_match (new_q, active_params, TRUE, QOF_QUERY_AND);
        active_params = NULL;
    }

    /* Destroy the old query */
    if (sw->q)
        qof_query_destroy (sw->q);

    /* And save the new one */
    sw->q = new_q;
}


static void
gnc_search_dialog_show_close_cancel (GNCSearchWindow *sw)
{
    if (sw->selected_cb)
    {
        gtk_widget_show (sw->cancel_button);
        gtk_widget_hide (sw->close_button);
    }
    else
    {
        gtk_widget_hide (sw->cancel_button);
        gtk_widget_show (sw->close_button);
    }
}


static void
gnc_search_dialog_reset_widgets (GNCSearchWindow *sw)
{
    gboolean sens = (sw->q != NULL);

    gtk_widget_set_sensitive(GTK_WIDGET(sw->narrow_rb), sens);
    gtk_widget_set_sensitive(GTK_WIDGET(sw->add_rb), sens);
    gtk_widget_set_sensitive(GTK_WIDGET(sw->del_rb), sens);

    if (sw->q)
    {
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (sw->new_rb), FALSE);
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (sw->narrow_rb), TRUE);
    }

    if (sw->crit_list)
    {
        gtk_widget_set_sensitive(sw->grouping_combo, TRUE);
        gtk_widget_hide(sw->match_all_label);
    }
    else
    {
        gtk_widget_set_sensitive(sw->grouping_combo, FALSE);
        gtk_widget_show(sw->match_all_label);
    }
}


static gboolean
gnc_search_dialog_crit_ok (GNCSearchWindow *sw)
{
    struct _crit_data *data;
    GList *l;
    gboolean ret;

    if (!sw->crit_list)
        return TRUE;

    l = g_list_last (sw->crit_list);
    data = l->data;
    ret = gnc_search_core_type_validate (data->element);

    if (ret)
        sw->last_param = data->param;

    return ret;
}


static void
search_find_cb (GtkButton *button, GNCSearchWindow *sw)
{
    if (!gnc_search_dialog_crit_ok (sw))
        return;

    search_update_query (sw);
    search_clear_criteria (sw);
    gnc_search_dialog_reset_widgets (sw);

    if (sw->result_cb)
        (sw->result_cb)(sw->q, sw->user_data, &(sw->selected_item));
    else
        gnc_search_dialog_display_results (sw);
}


static void
search_new_item_cb (GtkButton *button, GNCSearchWindow *sw)
{
    gpointer res;

    g_return_if_fail (sw->new_item_cb);

    res = (sw->new_item_cb)(sw->user_data);

    if (res)
    {
        const GncGUID *guid = (const GncGUID *) ((sw->get_guid->param_getfcn)(res, sw->get_guid));
        QofQueryOp op = QOF_QUERY_OR;

        if (!sw->q)
        {
            if (!sw->start_q)
            {
                sw->start_q = qof_query_create_for (sw->search_for);
                qof_query_set_book (sw->start_q, gnc_get_current_book ());
            }
            sw->q = qof_query_copy (sw->start_q);
            op = QOF_QUERY_AND;
        }

        qof_query_add_guid_match (sw->q, g_slist_prepend (NULL, QOF_PARAM_GUID),
                                  guid, op);

        /* Watch this entity so we'll refresh once it's actually changed */
        gnc_gui_component_watch_entity (sw->component_id, guid, QOF_EVENT_MODIFY);
    }
}


static void
search_cancel_cb (GtkButton *button, GNCSearchWindow *sw)
{
    /* Don't select anything */
    sw->selected_item = NULL;
    gnc_search_dialog_destroy (sw);
}


static void
search_help_cb (GtkButton *button, GNCSearchWindow *sw)
{
    gnc_gnome_help (HF_HELP, HL_FIND_TRANSACTIONS);
}


static void
remove_element (GtkWidget *button, GNCSearchWindow *sw)
{
    GtkWidget *element;
    struct _elem_data *data;

    if (!sw->crit_list)
        return;

    element = g_object_get_data (G_OBJECT (button), "element");
    data = g_object_get_data (G_OBJECT (element), "data");

    /* remove the element from the list */
    sw->crit_list = g_list_remove (sw->crit_list, data);

    /* and from the display */
    gtk_container_remove (GTK_CONTAINER (sw->criteria_table), element);
    gtk_container_remove (GTK_CONTAINER (sw->criteria_table), button);

    /* disable match-type menu when there is no criterion */
    if (!sw->crit_list)
    {
        gtk_widget_set_sensitive(sw->grouping_combo, FALSE);
        gtk_widget_show(sw->match_all_label);
    }
}


static void
attach_element (GtkWidget *element, GNCSearchWindow *sw, int row)
{
    GtkWidget *remove;
    struct _crit_data *data;

    data = g_object_get_data (G_OBJECT (element), "data");

    gtk_table_attach (GTK_TABLE (sw->criteria_table), element, 0, 1, row, row + 1,
                      GTK_EXPAND | GTK_FILL, 0, 0, 0);


    remove = gtk_button_new_from_stock (GTK_STOCK_REMOVE);
    g_object_set_data (G_OBJECT (remove), "element", element);
    g_signal_connect (G_OBJECT (remove), "clicked", G_CALLBACK (remove_element), sw);
    gtk_table_attach (GTK_TABLE (sw->criteria_table), remove, 1, 2, row, row + 1,
                      0, 0, 0, 0);
    gtk_widget_show (remove);
    data->button = remove;	/* Save the button for later */
}


static void
combo_box_changed (GtkComboBox *combo_box, struct _crit_data *data)
{
    GNCSearchParam *param;
    GNCSearchCoreType *newelem;
    GtkTreeModel *model;
    GtkTreeIter iter;

    if (!gtk_combo_box_get_active_iter(combo_box, &iter))
        return;
    model = gtk_combo_box_get_model(combo_box);
    gtk_tree_model_get(model, &iter, SEARCH_COL_POINTER, &param, -1);

    if (gnc_search_param_type_match (param, data->param))
    {
        /* The param type is the same, just save the new param */
        data->param = param;
        return;
    }
    data->param = param;

    /* OK, let's do a widget shuffle, throw away the old widget/element,
     * and create another one here.  No need to change the crit_list --
     * the pointer to data stays the same.
     */
    if (data->elemwidget)
        gtk_container_remove (GTK_CONTAINER (data->container), data->elemwidget);
    g_object_unref (G_OBJECT (data->element));

    newelem = gnc_search_core_type_new_type_name
              (gnc_search_param_get_param_type (param));
    data->element = newelem;
    data->elemwidget = gnc_search_core_type_get_widget (newelem);
    if (data->elemwidget)
    {
        gtk_box_pack_start (GTK_BOX (data->container), data->elemwidget,
                            FALSE, FALSE, 0);
    }

    /* Make sure it's visible */
    gtk_widget_show_all (data->container);

    /* Make sure we widen up if necessary */
    gtk_widget_queue_resize (GTK_WIDGET (data->dialog));

    /* And grab focus */
    gnc_search_core_type_grab_focus (newelem);
    gnc_search_core_type_editable_enters (newelem);
}


static void
search_clear_criteria (GNCSearchWindow *sw)
{
    GList *node;

    for (node = sw->crit_list; node; )
    {
        GList *tmp = node->next;
        struct _crit_data *data = node->data;
        g_object_ref (data->button);
        remove_element (data->button, sw);
        node = tmp;
    }
}


static GtkWidget *
get_comb_box_widget (GNCSearchWindow *sw, struct _crit_data *data)
{
    GtkWidget *combo_box;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkCellRenderer *cell;
    GList *l;
    int index = 0, current = 0;

    store = gtk_list_store_new(NUM_SEARCH_COLS, G_TYPE_STRING, G_TYPE_POINTER);
    combo_box = gtk_combo_box_new_with_model(GTK_TREE_MODEL(store));
    g_object_unref(store);

    cell = gtk_cell_renderer_text_new ();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT (combo_box), cell, TRUE);
    gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo_box), cell,
                                    "text", SEARCH_COL_NAME,
                                    NULL);

    for (l = sw->params_list; l; l = l->next)
    {
        GNCSearchParam *param = l->data;

        gtk_list_store_append(store, &iter);
        gtk_list_store_set(store, &iter,
                           SEARCH_COL_NAME, _(param->title),
                           SEARCH_COL_POINTER, param,
                           -1);

        if (param == sw->last_param) /* is this the right parameter to start? */
            current = index;

        index++;
    }

    gtk_combo_box_set_active (GTK_COMBO_BOX(combo_box), current);
    g_signal_connect (combo_box, "changed", G_CALLBACK (combo_box_changed), data);

    return combo_box;
}

static GtkWidget *
get_element_widget (GNCSearchWindow *sw, GNCSearchCoreType *element)
{
    GtkWidget *combo_box, *hbox, *p;
    struct _crit_data *data;

    data = g_new0 (struct _crit_data, 1);
    data->element = element;
    data->dialog = GTK_DIALOG (sw->dialog);

    hbox = gtk_hbox_new (FALSE, 0);
    /* only set to automaticaly clean up the memory */
    g_object_set_data_full (G_OBJECT (hbox), "data", data, g_free);

    p = gnc_search_core_type_get_widget (element);
    data->elemwidget = p;
    data->container = hbox;
    data->param = sw->last_param;

    combo_box = get_comb_box_widget (sw, data);
    gtk_box_pack_start (GTK_BOX (hbox), combo_box, FALSE, FALSE, 0);
    if (p)
        gtk_box_pack_start (GTK_BOX (hbox), p, FALSE, FALSE, 0);
    gtk_widget_show_all (hbox);

    return hbox;
}

static void
gnc_search_dialog_book_option_changed (gpointer new_val, gpointer user_data)
{
    GList *l;
    GNCSearchWindow *sw = user_data;
    gboolean *new_data = (gboolean*)new_val;
    /* Save current dialog focus */
    GtkWidget *focused_widget = gtk_window_get_focus(GTK_WINDOW(sw->dialog));

    g_return_if_fail (sw);
    if (strcmp (sw->search_for, GNC_ID_SPLIT) != 0)
        return;

    /* Adjust labels for future added search criteria */
    for (l = sw->params_list; l; l = l->next)
    {
        GNCSearchParam *param = l->data;

        if (*new_data)
        {
            if (strcmp (param->title, N_("Action")) == 0)
                gnc_search_param_set_title (param, N_("Number/Action"));
            if (strcmp (param->title, N_("Number")) == 0)
                gnc_search_param_set_title (param, N_("Transaction Number"));
        }
        else
        {
            if (strcmp (param->title, N_("Number/Action")) == 0)
                gnc_search_param_set_title (param, N_("Action"));
            if (strcmp (param->title, N_("Transaction Number")) == 0)
                gnc_search_param_set_title (param, N_("Number"));
        }
    }
    /* Adjust labels for existing search criteria; walk the list of criteria */
    for (l = sw->crit_list; l; l = l->next)
    {
        struct _crit_data *data = l->data;
        GList *children;

        /* For each, walk the list of container children to get combo_box */
        for (children = gtk_container_get_children(GTK_CONTAINER(data->container));
                children; children = children->next)
        {
            GtkWidget *combo_box = children->data;

            /* Get current active item if combo_box */
            if (GTK_IS_COMBO_BOX(combo_box))
            {
                GtkWidget *new_combo_box;
                gint index;

                /* Set index to current active item */
                index = gtk_combo_box_get_active(GTK_COMBO_BOX(combo_box));
                /* Create new combo_box to replace existing one */
                new_combo_box = get_comb_box_widget (sw, data);
                /* If current combo_box has focus, point to new_combo-box */
                if (focused_widget == combo_box)
                    focused_widget = new_combo_box;
                gtk_widget_destroy(combo_box);
                /* Set new combo_box to current active item */
                gtk_combo_box_set_active(GTK_COMBO_BOX(new_combo_box), index);
                gtk_box_pack_start (GTK_BOX (data->container), new_combo_box,
                                                               FALSE, FALSE, 0);
                gtk_box_reorder_child(GTK_BOX (data->container), new_combo_box, 0);
                gtk_widget_show_all (data->container);
            }
        }
    }
    gtk_widget_grab_focus(focused_widget);
}

static void
gnc_search_dialog_add_criterion (GNCSearchWindow *sw)
{
    GNCSearchCoreType *new_sct;

    /* First, make sure that the last criterion is ok */
    if (sw->crit_list)
    {
        if (!gnc_search_dialog_crit_ok (sw))
            return;
    }
    else
    {
        sw->last_param = sw->params_list->data;

        /* no match-all situation anymore */
        gtk_widget_set_sensitive(sw->grouping_combo, TRUE);
        gtk_widget_hide(sw->match_all_label);
    }
    /* create a new criterion element */
    new_sct = gnc_search_core_type_new_type_name
          (gnc_search_param_get_param_type (sw->last_param));

    if (new_sct)
    {
        struct _crit_data *data;
        GtkWidget *w;
        int rows;

        w = get_element_widget (sw, new_sct);
        data = g_object_get_data (G_OBJECT (w), "data");
        sw->crit_list = g_list_append (sw->crit_list, data);

        rows = GTK_TABLE (sw->criteria_table)->nrows;
        gtk_table_resize (GTK_TABLE (sw->criteria_table), rows + 1, 2);
        attach_element (w, sw, rows);

        gnc_search_core_type_grab_focus (new_sct);
        gnc_search_core_type_editable_enters (new_sct);
    }
}


static void
add_criterion (GtkWidget *button, GNCSearchWindow *sw)
{
    gnc_search_dialog_add_criterion (sw);
}


static int
gnc_search_dialog_close_cb (GtkDialog *dialog, GNCSearchWindow *sw)
{
    g_return_val_if_fail (sw, TRUE);

    /* Unregister callback on book option changes originally registered
     * if searching for splits */
    if (strcmp (sw->search_for, GNC_ID_SPLIT) == 0)
        gnc_book_option_remove_cb(OPTION_NAME_NUM_FIELD_SOURCE,
                                    gnc_search_dialog_book_option_changed, sw);

    gnc_unregister_gui_component (sw->component_id);

    /* Clear the crit list */
    g_list_free (sw->crit_list);

    /* Clear the button list */
    g_list_free (sw->button_list);

    /* Destroy the queries */
    if (sw->q) qof_query_destroy (sw->q);
    if (sw->start_q) qof_query_destroy (sw->start_q);

    /* Destroy the user_data */
    if (sw->free_cb)
        (sw->free_cb)(sw->user_data);

    /* Destroy and exit */
    g_free (sw);
    return FALSE;
}


static void
refresh_handler (GHashTable *changes, gpointer data)
{
    GNCSearchWindow * sw = data;

    g_return_if_fail (sw);
    /* This assumes that results_cb will refresh itself which is the case with
     * registers. Also, only refresh if you are already displaying results */
    if (!sw->result_cb && (sw->result_view != NULL))
       gnc_search_dialog_display_results (sw);
}


static void
close_handler (gpointer data)
{
    GNCSearchWindow * sw = data;

    g_return_if_fail (sw);
    gtk_widget_destroy (sw->dialog);
    /* DRH: should sw be freed here? */
}


static const gchar *
type_label_to_new_button(const gchar* type_label)
{
    if (g_strcmp0(type_label, _("Bill")) == 0)
    {
        return _("New Bill");
    }
    else if (g_strcmp0(type_label, _("Customer")) == 0)
    {
        return _("New Customer");
    }
    else if (g_strcmp0(type_label, _("Employee")) == 0)
    {
        return _("New Employee");
    }
    else if (g_strcmp0(type_label, _("Expense Voucher")) == 0)
    {
        return _("New Expense Voucher");
    }
    else if (g_strcmp0(type_label, _("Invoice")) == 0)
    {
        return _("New Invoice");
    }
    else if (g_strcmp0(type_label, _("Job")) == 0)
    {
        return _("New Job");
    }
    else if (g_strcmp0(type_label, _("Order")) == 0)
    {
        return _("New Order");
    }
    else if (g_strcmp0(type_label, _("Transaction")) == 0)
    {
        return _("New Transaction");
    }
    else if (g_strcmp0(type_label, _("Split")) == 0)
    {
        return _("New Split");
    }
    else if (g_strcmp0(type_label, _("Vendor")) == 0)
    {
        return _("New Vendor");
    }
    else
    {
        PWARN("No translatable new-button label found for search type \"%s\", please add one into dialog-search.c!", type_label);
        return Q_("Item represents an unknown object type (in the sense of bill, customer, invoice, transaction, split,...)|New item");
    }
}


static void
gnc_search_dialog_init_widgets (GNCSearchWindow *sw, const gchar *title)
{
    GtkBuilder        *builder;
    GtkWidget         *label, *add, *box;
    GtkComboBoxText   *combo_box;
    GtkWidget         *widget;
    GtkWidget         *new_item_button;
    const char        *type_label;
    gboolean           active;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-search.glade", "Search Dialog");

    /* Grab the dialog, save the dialog info */
    sw->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "Search Dialog"));
    gtk_window_set_title(GTK_WINDOW(sw->dialog), title);
    g_object_set_data (G_OBJECT (sw->dialog), "dialog-info", sw);

    /* Grab the result hbox */
    sw->result_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "result_hbox"));

    /* Grab the search-table widget */
    sw->criteria_table = GTK_WIDGET(gtk_builder_get_object (builder, "criteria_table"));

    /* Set the type label */
    label = GTK_WIDGET(gtk_builder_get_object (builder, "type_label"));
    if (sw->type_label)
        type_label = sw->type_label;
    else
        type_label = _(qof_object_get_type_label (sw->search_for));
    gtk_label_set_text (GTK_LABEL (label), type_label);

    /* Set the 'add criterion' button */
    add = gtk_button_new_from_stock (GTK_STOCK_ADD);

    g_signal_connect (G_OBJECT (add), "clicked", G_CALLBACK (add_criterion), sw);
    box = GTK_WIDGET(gtk_builder_get_object (builder, "add_button_box"));
    gtk_box_pack_start (GTK_BOX (box), add, FALSE, FALSE, 3);
    gtk_widget_show (add);

    /* Set the match-type menu */
    sw->grouping_combo = gtk_combo_box_text_new();
    combo_box = GTK_COMBO_BOX_TEXT(sw->grouping_combo);
    gtk_combo_box_text_append_text(combo_box, _("all criteria are met"));
    gtk_combo_box_text_append_text(combo_box, _("any criteria are met"));
    gtk_combo_box_set_active(GTK_COMBO_BOX(combo_box), sw->grouping);
    g_signal_connect(combo_box, "changed", G_CALLBACK (match_combo_changed), sw);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "type_menu_box"));
    gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET(combo_box), FALSE, FALSE, 3);
    gtk_widget_show(GTK_WIDGET(combo_box));

    /* Grab the 'all items match' label */
    sw->match_all_label = GTK_WIDGET(gtk_builder_get_object (builder, "match_all_label"));

    /* if there's no original query, make the narrow, add, delete buttons inaccessible */
    sw->new_rb = GTK_WIDGET(gtk_builder_get_object (builder, "new_search_radiobutton"));
    g_signal_connect (sw->new_rb, "toggled",
                      G_CALLBACK (search_type_cb), sw);
    sw->narrow_rb = GTK_WIDGET(gtk_builder_get_object (builder, "narrow_search_radiobutton"));
    g_signal_connect (sw->narrow_rb, "toggled",
                      G_CALLBACK (search_type_cb), sw);
    sw->add_rb = GTK_WIDGET(gtk_builder_get_object (builder, "add_search_radiobutton"));
    g_signal_connect (sw->add_rb, "toggled",
                      G_CALLBACK (search_type_cb), sw);
    sw->del_rb = GTK_WIDGET(gtk_builder_get_object (builder, "delete_search_radiobutton"));
    g_signal_connect (sw->del_rb, "toggled",
                      G_CALLBACK (search_type_cb), sw);

    active = gnc_prefs_get_bool(sw->prefs_group, GNC_PREF_ACTIVE_ONLY);
    sw->active_only_check = GTK_WIDGET(gtk_builder_get_object (builder, "active_only_check"));
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (sw->active_only_check), active);
    g_signal_connect (sw->active_only_check, "toggled",
                      G_CALLBACK (search_active_only_cb), sw);

    /* Figure out if we this object-type has an "active" parameter, and
     * if not, then set the active-check button insensitive
     */
    if (qof_class_get_parameter (sw->search_for, QOF_PARAM_ACTIVE) == NULL)
        gtk_widget_set_sensitive (sw->active_only_check, FALSE);

    /* Deal with the find button */
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "find_button"));
    g_signal_connect (widget, "clicked",
                      G_CALLBACK (search_find_cb), sw);

    /* Deal with the cancel button */
    sw->cancel_button = GTK_WIDGET(gtk_builder_get_object (builder, "cancel_button"));
    g_signal_connect (sw->cancel_button, "clicked",
                      G_CALLBACK (search_cancel_cb), sw);

    /* Deal with the close button */
    sw->close_button = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));
    g_signal_connect (sw->close_button, "clicked",
                      G_CALLBACK (search_cancel_cb), sw);

    /* Deal with the new_item button */
    new_item_button = GTK_WIDGET(gtk_builder_get_object (builder, "new_item_button"));
    gtk_button_set_label (GTK_BUTTON(new_item_button),
                          type_label_to_new_button(type_label));
    g_signal_connect (new_item_button, "clicked",
                      G_CALLBACK (search_new_item_cb), sw);

    /* Deal with the help button */
    widget = GTK_WIDGET(gtk_builder_get_object (builder, "help_button"));
    g_signal_connect (widget, "clicked",
                      G_CALLBACK (search_help_cb), sw);

    /* add the first criterion */
    gnc_search_dialog_add_criterion (sw);

    /* register to update criterion/criteria labels based on book option changes
     * if searching for splits */
    if (strcmp (sw->search_for, GNC_ID_SPLIT) == 0)
        gnc_book_option_register_cb(OPTION_NAME_NUM_FIELD_SOURCE,
                                    gnc_search_dialog_book_option_changed, sw);

    /* Hide the 'new' button if there is no new_item_cb */
    if (!sw->new_item_cb)
        gtk_widget_hide (new_item_button);

    /* Connect all the signals */
    gtk_builder_connect_signals (builder, sw);

    /* Register ourselves */
    sw->component_id = gnc_register_gui_component (DIALOG_SEARCH_CM_CLASS,
                       refresh_handler,
                       close_handler, sw);
    gnc_gui_component_set_session (sw->component_id,
                                   gnc_get_current_session());

    /* And setup the close callback */
    g_signal_connect (G_OBJECT (sw->dialog), "destroy",
                      G_CALLBACK (gnc_search_dialog_close_cb), sw);

    gnc_search_dialog_reset_widgets (sw);
    gnc_search_dialog_show_close_cancel (sw);

    g_object_unref(G_OBJECT(builder));
}


void
gnc_search_dialog_destroy (GNCSearchWindow *sw)
{
    if (!sw) return;
    if (sw->prefs_group)
        gnc_save_window_size(sw->prefs_group, GTK_WINDOW(sw->dialog));
    gnc_close_gui_component (sw->component_id);
}


void
gnc_search_dialog_raise (GNCSearchWindow *sw)
{
    if (!sw) return;
    gtk_window_present (GTK_WINDOW(sw->dialog));
}

GNCSearchWindow *
gnc_search_dialog_create (QofIdTypeConst obj_type, const gchar *title,
                          GList *param_list,
                          GList *display_list,
                          QofQuery *start_query, QofQuery *show_start_query,
                          GNCSearchCallbackButton *callbacks,
                          GNCSearchResultCB result_callback,
                          GNCSearchNewItemCB new_item_cb,
                          gpointer user_data, GNCSearchFree free_cb,
                          const gchar *prefs_group,
                          const gchar *type_label)
{
    GNCSearchWindow *sw = g_new0 (GNCSearchWindow, 1);

    g_return_val_if_fail (obj_type, NULL);
    g_return_val_if_fail (*obj_type != '\0', NULL);
    g_return_val_if_fail (param_list, NULL);

    /* Make sure the caller supplies callbacks xor result_callback */
    g_return_val_if_fail ((callbacks && !result_callback) ||
                          (!callbacks && result_callback), NULL);

    if (callbacks)
        g_return_val_if_fail (display_list, NULL);

    sw->search_for = obj_type;
    sw->params_list = param_list;
    sw->display_list = display_list;
    sw->buttons = callbacks;
    sw->result_cb = result_callback;
    sw->new_item_cb = new_item_cb;
    sw->user_data = user_data;
    sw->free_cb = free_cb;
    sw->prefs_group = prefs_group;
    sw->type_label = type_label;

    /* Grab the get_guid function */
    sw->get_guid = qof_class_get_parameter (sw->search_for, QOF_PARAM_GUID);
    if (start_query)
        sw->start_q = qof_query_copy (start_query);
    sw->q = show_start_query;

    gnc_search_dialog_init_widgets (sw, title);
    if (sw->prefs_group)
        gnc_restore_window_size(sw->prefs_group, GTK_WINDOW(sw->dialog));
    gtk_widget_show(sw->dialog);

    /* Maybe display the original query results? */
    if (callbacks && show_start_query)
    {
        gnc_search_dialog_reset_widgets (sw);
        gnc_search_dialog_display_results (sw);
    }

    return sw;
}


/* Register an on-close signal with the Search Dialog */
guint gnc_search_dialog_connect_on_close (GNCSearchWindow *sw,
        GCallback func,
        gpointer user_data)
{
    g_return_val_if_fail (sw, 0);
    g_return_val_if_fail (func, 0);
    g_return_val_if_fail (user_data, 0);

    return g_signal_connect (G_OBJECT (sw->dialog), "destroy",
                             func, user_data);

}


/* Un-register the signal handlers with the Search Dialog */
void gnc_search_dialog_disconnect (GNCSearchWindow *sw, gpointer user_data)
{
    g_return_if_fail (sw);
    g_return_if_fail (user_data);

    g_signal_handlers_disconnect_matched (sw->dialog, G_SIGNAL_MATCH_DATA,
                                          0, 0, NULL, NULL, user_data);
}


/* Clear all callbacks with this Search Window */
void gnc_search_dialog_set_select_cb (GNCSearchWindow *sw,
                                      GNCSearchSelectedCB selected_cb,
                                      gpointer user_data,
                                      gboolean allow_clear)
{
    g_return_if_fail (sw);

    sw->selected_cb = selected_cb;
    sw->select_arg = user_data;
    sw->allow_clear = allow_clear;

    /* Show or hide the select button */
    if (sw->select_button)
    {
        if (selected_cb)
            gtk_widget_show (sw->select_button);
        else
            gtk_widget_hide (sw->select_button);
    }

    /* Show the proper close/cancel button */
    gnc_search_dialog_show_close_cancel (sw);
}


/* TEST CODE BELOW HERE */

static GList *
get_params_list (QofIdTypeConst type)
{
    GList *list = NULL;

    list = gnc_search_param_prepend (list, "Txn: All Accounts",
                                     ACCOUNT_MATCH_ALL_TYPE,
                                     type, SPLIT_TRANS, TRANS_SPLITLIST,
                                     SPLIT_ACCOUNT_GUID, NULL);
    list = gnc_search_param_prepend (list, "Split Account", GNC_ID_ACCOUNT,
                                     type, SPLIT_ACCOUNT, QOF_PARAM_GUID,
                                     NULL);
    list = gnc_search_param_prepend (list, "Split->Txn->Void?", NULL, type,
                                     SPLIT_TRANS, TRANS_VOID_STATUS, NULL);
    list = gnc_search_param_prepend (list, "Split Int64", NULL, type,
                                     "d-share-int64", NULL);
    list = gnc_search_param_prepend (list, "Split Amount (double)", NULL, type,
                                     "d-share-amount", NULL);
    list = gnc_search_param_prepend (list, "Split Value (debcred)", NULL, type,
                                     SPLIT_VALUE, NULL);
    list = gnc_search_param_prepend (list, "Split Amount (numeric)", NULL, type,
                                     SPLIT_AMOUNT, NULL);
    list = gnc_search_param_prepend (list, "Date Reconciled (date)", NULL, type,
                                     SPLIT_DATE_RECONCILED, NULL);
    list = gnc_search_param_prepend (list, "Split Memo (string)", NULL, type,
                                     SPLIT_MEMO, NULL);

    return list;
}


static GList *
get_display_list (QofIdTypeConst type)
{
    GList *list = NULL;

    list = gnc_search_param_prepend (list, "Amount", NULL, type, SPLIT_AMOUNT,
                                     NULL);
    list = gnc_search_param_prepend (list, "Memo", NULL, type, SPLIT_MEMO, NULL);
    list = gnc_search_param_prepend (list, "Date", NULL, type, SPLIT_TRANS,
                                     TRANS_DATE_POSTED, NULL);

    return list;
}


static void
do_nothing (gpointer *a, gpointer b)
{
    return;
}


void
gnc_search_dialog_test (void)
{
    static GList *params = NULL;
    static GList *display = NULL;
    static GNCSearchCallbackButton buttons[] =
    {
        /* Don't mark these as translatable since these are only test strings! */
        { ("View Split"), do_nothing, NULL, TRUE },
        { ("New Split"), do_nothing, NULL, TRUE },
        { ("Do Something"), do_nothing, NULL, TRUE },
        { ("Do Nothing"), do_nothing, NULL, TRUE },
        { ("Who Cares?"), do_nothing, NULL, FALSE },
        { NULL }
    };

    if (params == NULL)
        params = get_params_list (GNC_ID_SPLIT);

    if (display == NULL)
        display = get_display_list (GNC_ID_SPLIT);

/* FIXME: All this does is leak. */
    gnc_search_dialog_create (GNC_ID_SPLIT, _("Find Transaction"),
			      params, display,
			      NULL, NULL, buttons, NULL, NULL, NULL, NULL,
			      NULL, NULL);
}

