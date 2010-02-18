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
#include "gnc-query-list.h"
#include "gnc-gconf-utils.h"
#include "gncObject.h"
#include "QueryNew.h"
#include "QueryObject.h"
#include "QueryCore.h"

#include "Transaction.h"	/* for the SPLIT_* and TRANS_* */

#include "dialog-search.h"
#include "search-core-type.h"
#include "search-param.h"

#define DIALOG_SEARCH_CM_CLASS "dialog-search"
#define KEY_ACTIVE_ONLY "search_for_active_only"

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
    GtkWidget *	dialog;
    GtkWidget *	grouping_combo;
    GtkWidget *	match_all_label;
    GtkWidget *	criteria_table;
    GtkWidget *	result_hbox;

    /* The "results" sub-window widgets */
    GtkWidget *	result_list;
    gpointer	selected_item;

    /* The search_type radio-buttons */
    GtkWidget *	new_rb;
    GtkWidget *	narrow_rb;
    GtkWidget *	add_rb;
    GtkWidget *	del_rb;

    GtkWidget *	active_only_check;

    /* The Select button */
    GtkWidget *	select_button;

    /* The close/cancel buttons */
    GtkWidget *	close_button;
    GtkWidget *	cancel_button;

    /* Callbacks */
    GNCSearchResultCB result_cb;
    GNCSearchNewItemCB new_item_cb;
    GNCSearchCallbackButton *buttons;
    GNCSearchFree	free_cb;
    gpointer		user_data;

    GNCSearchSelectedCB	selected_cb;
    gpointer		select_arg;
    gboolean		allow_clear;

    /* What we're searching for, and how */
    const gchar *  type_label;
    GNCIdTypeConst search_for;
    GNCSearchType	grouping;	/* Match Any, Match All */
    const QofParam * get_guid;	/* Function to GetGUID from the object */
    int		search_type;	/* New, Narrow, Add, Delete */

    /* Our query status */
    QueryNew *	q;
    QueryNew *	start_q;	/* The query to start from, if any */

    /* The list of criteria */
    GNCSearchParam * last_param;
    GList *	params_list;	/* List of GNCSearchParams */
    GList *	display_list;	/* List of GNCSearchParams for Display */
    gint		num_cols;	/* Number of Display Columns */
    GList *	crit_list;	/* list of crit_data */

    gint		component_id;
    const gchar * gconf_section;
};

struct _crit_data
{
    GNCSearchParam *	param;
    GNCSearchCoreType *	element;
    GtkWidget *		elemwidget;
    GtkWidget *		container;
    GtkWidget *		button;
    GtkDialog *		dialog;
};

static void search_clear_criteria (GNCSearchWindow *sw);
static void gnc_search_dialog_display_results (GNCSearchWindow *sw);

static void
gnc_search_callback_button_execute (GNCSearchCallbackButton *cb,
                                    GNCSearchWindow *sw)
{
    if (cb->cb_fcn)
        (cb->cb_fcn)(&(sw->selected_item), sw->user_data);
}

static void
gnc_search_dialog_result_clicked (GtkButton *button, GNCSearchWindow *sw)
{
    GNCSearchCallbackButton *cb;

    cb = g_object_get_data (G_OBJECT (button), "data");
    gnc_search_callback_button_execute (cb, sw);
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

#if 0
static void
gnc_search_dialog_line_toggled (GNCQueryList *list, gpointer item,
                                gpointer user_data)
{
    GNCSearchWindow *sw = user_data;
    if (sw->selected_item == item)
        sw->selected_item = NULL;
    else
        sw->selected_item = item;
}

static void
gnc_search_dialog_double_click_entry (GNCQueryList *list, gpointer item,
                                      gpointer user_data)
{
    GNCSearchWindow *sw = user_data;

    /* Force the selected item */
    sw->selected_item = item;

    /* If we double-click an item, then either "select" it, or run it
     * through the first button (which should be view/edit
     */
    if (sw->selected_cb)
        /* Select the time */
        gnc_search_dialog_select_cb (NULL, sw);
    else if (sw->buttons)
        /* Call the first button (usually view/edit) */
        gnc_search_callback_button_execute (sw->buttons, sw);
}
#endif

static void
gnc_search_dialog_select_row_cb (GtkCList *clist, gint row, gint column,
                                 GdkEventButton *event, gpointer user_data)
{
    GNCSearchWindow *sw = user_data;
    sw->selected_item = gtk_clist_get_row_data (clist, row);

    /* If we double-click an item, then either "select" it, or run it
     * through the first button (which should be view/edit
     */
    if (event && event->type == GDK_2BUTTON_PRESS)
    {
        if (sw->selected_cb)
            /* Select the time */
            gnc_search_dialog_select_cb (NULL, sw);
        else if (sw->buttons)
            /* Call the first button (usually view/edit) */
            gnc_search_callback_button_execute (sw->buttons, sw);

        /* If we get here, then nothing to do for a double-click */
    }
}

static void
gnc_search_dialog_unselect_row_cb (GtkCList *clist, gint row, gint column,
                                   GdkEventButton *event, gpointer user_data)
{
    GNCSearchWindow *sw = user_data;
    gpointer item = gtk_clist_get_row_data (clist, row);

    if (sw->selected_item == item)
        sw->selected_item = NULL;
}

static void
gnc_search_dialog_init_result_list (GNCSearchWindow *sw)
{
    sw->result_list = gnc_query_list_new(sw->display_list, sw->q);

    /* Setup the list callbacks */
    g_signal_connect (G_OBJECT (sw->result_list), "select-row",
                      G_CALLBACK (gnc_search_dialog_select_row_cb), sw);
    g_signal_connect (G_OBJECT (sw->result_list), "unselect-row",
                      G_CALLBACK (gnc_search_dialog_unselect_row_cb), sw);
}

static void
gnc_search_dialog_display_results (GNCSearchWindow *sw)
{
    gdouble max_count;

    /* Check if this is the first time this is called for this window.
     * If so, then build the results sub-window, the scrolled listbox,
     * and the active buttons.
     */
    if (sw->result_list == NULL)
    {
        GtkWidget *scroller, *button_box, *button;

        /* Create the list */
        gnc_search_dialog_init_result_list (sw);

        /* Create the scroller and add the list to the scroller */
        scroller = gtk_scrolled_window_new (NULL, NULL);
        gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroller),
                                        GTK_POLICY_AUTOMATIC,
                                        GTK_POLICY_AUTOMATIC);
        gtk_widget_set_size_request(GTK_WIDGET(scroller), 300, 100);
        gtk_container_add (GTK_CONTAINER (scroller), sw->result_list);

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
                button = gtk_button_new_with_label (_(sw->buttons[i].label));
                g_object_set_data (G_OBJECT (button), "data", &(sw->buttons[i]));
                g_signal_connect (G_OBJECT (button), "clicked",
                                  G_CALLBACK (gnc_search_dialog_result_clicked), sw);
                gtk_box_pack_start (GTK_BOX (button_box), button, FALSE, FALSE, 3);
            }
        }

        /* Add the scrolled-list and button-box to the results_box */
        gtk_box_pack_end (GTK_BOX (sw->result_hbox), button_box, FALSE, FALSE, 3);
        gtk_box_pack_end (GTK_BOX (sw->result_hbox), scroller, TRUE, TRUE, 3);

        /* And show the results */
        gtk_widget_show_all (sw->result_hbox);

        /* But maybe hide the select button */
        if (!sw->selected_cb)
            gtk_widget_hide (sw->select_button);
    }

    /* Update the query in the list */
    gnc_query_list_reset_query (GNC_QUERY_LIST(sw->result_list), sw->q);

    /* set 'new search' if fewer than max_count items is returned. */
    max_count = gnc_gconf_get_float("dialogs/search", "new_search_limit", NULL);
    if (gnc_query_list_get_num_entries(GNC_QUERY_LIST(sw->result_list)) < max_count)
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (sw->new_rb), TRUE);
}

static void
match_combo_changed (GtkComboBox *combo_box, GNCSearchWindow *sw)
{
    sw->grouping = gtk_combo_box_get_active(combo_box);
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

    gnc_gconf_set_bool(sw->gconf_section, KEY_ACTIVE_ONLY,
                       gtk_toggle_button_get_active (button), NULL);
}

static void
search_update_query (GNCSearchWindow *sw)
{
    static GSList *active_params = NULL;
    QueryNew *q, *q2, *new_q;
    GList *node;
    QueryOp op;
    QueryPredData_t pdata;

    if (sw->grouping == GNC_SEARCH_MATCH_ANY)
        op = QUERY_OR;
    else
        op = QUERY_AND;

    if (active_params == NULL)
        active_params = g_slist_prepend (NULL, QUERY_PARAM_ACTIVE);

    /* Make sure we supply a book! */
    if (sw->start_q == NULL)
    {
        sw->start_q = gncQueryCreateFor (sw->search_for);
        gncQuerySetBook (sw->start_q, gnc_get_current_book ());
    }
    else
    {
        /* We've got a query -- purge it of any "active" parameters */
        gncQueryPurgeTerms (sw->start_q, active_params);
    }

    /* Now create a new query to work from */
    q = gncQueryCreateFor (sw->search_for);

    /* Walk the list of criteria */
    for (node = sw->crit_list; node; node = node->next)
    {
        struct _crit_data *data = node->data;

        pdata = gnc_search_core_type_get_predicate (data->element);
        if (pdata)
            gncQueryAddTerm (q, gnc_search_param_get_param_path (data->param),
                             pdata, op);
    }

    /* Now combine this query with the existing query, depending on
     * what we want to do...  We can assume that cases 1, 2, and 3
     * already have sw->q being valid!
     */

    switch (sw->search_type)
    {
    case 0:			/* New */
        new_q = gncQueryMerge (sw->start_q, q, QUERY_AND);
        gncQueryDestroy (q);
        break;
    case 1:			/* Refine */
        new_q = gncQueryMerge (sw->q, q, QUERY_AND);
        gncQueryDestroy (q);
        break;
    case 2:			/* Add */
        new_q = gncQueryMerge (sw->q, q, QUERY_OR);
        gncQueryDestroy (q);
        break;
    case 3:			/* Delete */
        q2 = gncQueryInvert (q);
        new_q = gncQueryMerge (sw->q, q2, QUERY_AND);
        gncQueryDestroy (q2);
        gncQueryDestroy (q);
        break;
    default:
        g_warning ("bad search type: %d", sw->search_type);
        new_q = q;
        break;
    }

    if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (sw->active_only_check)))
    {
        gncQueryAddBooleanMatch (new_q, active_params, TRUE, QUERY_AND);
        active_params = NULL;
    }

    /* Destroy the old query */
    if (sw->q)
        gncQueryDestroy (sw->q);

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
        const GUID *guid = (const GUID *) ((sw->get_guid->param_getfcn)(res, sw->get_guid));
        QueryOp op = QUERY_OR;

        if (!sw->q)
        {
            if (!sw->start_q)
            {
                sw->start_q = gncQueryCreateFor (sw->search_for);
                gncQuerySetBook (sw->start_q, gnc_get_current_book ());
            }
            sw->q = gncQueryCopy (sw->start_q);
            op = QUERY_AND;
        }

        gncQueryAddGUIDMatch (sw->q, g_slist_prepend (NULL, QUERY_PARAM_GUID),
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
get_element_widget (GNCSearchWindow *sw, GNCSearchCoreType *element)
{
    GtkWidget *combo_box, *hbox, *p;
    GtkListStore *store;
    GtkTreeIter iter;
    GtkCellRenderer *cell;
    GList *l;
    struct _crit_data *data;
    int index = 0, current = 0;

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

    gtk_box_pack_start (GTK_BOX (hbox), combo_box, FALSE, FALSE, 0);
    if (p)
        gtk_box_pack_start (GTK_BOX (hbox), p, FALSE, FALSE, 0);
    gtk_widget_show_all (hbox);

    return hbox;
}

static void
gnc_search_dialog_add_criterion (GNCSearchWindow *sw)
{
    GNCSearchCoreType *new;

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

    new = gnc_search_core_type_new_type_name
          (gnc_search_param_get_param_type (sw->last_param));

    if (new)
    {
        struct _crit_data *data;
        GtkWidget *w;
        int rows;

        w = get_element_widget (sw, new);
        data = g_object_get_data (G_OBJECT (w), "data");
        sw->crit_list = g_list_append (sw->crit_list, data);

        rows = GTK_TABLE (sw->criteria_table)->nrows;
        gtk_table_resize (GTK_TABLE (sw->criteria_table), rows + 1, 2);
        attach_element (w, sw, rows);

        gnc_search_core_type_grab_focus (new);
        gnc_search_core_type_editable_enters (new);
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

    gnc_unregister_gui_component (sw->component_id);

    /* XXX: Clear the params_list? */
    g_list_free (sw->crit_list);

    /* Destroy the queries */
    if (sw->q) gncQueryDestroy (sw->q);
    if (sw->start_q) gncQueryDestroy (sw->start_q);

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

static void
gnc_search_dialog_init_widgets (GNCSearchWindow *sw, const gchar *title)
{
    GladeXML *xml;
    GtkWidget *label, *add, *box;
    GtkComboBox *combo_box;
    GtkWidget *new_item_button;
    const char * type_label;
    gboolean active;

    xml = gnc_glade_xml_new ("search.glade", "Search Dialog");

    /* Grab the dialog, save the dialog info */
    sw->dialog = glade_xml_get_widget (xml, "Search Dialog");
    gtk_window_set_title(GTK_WINDOW(sw->dialog), title);
    g_object_set_data (G_OBJECT (sw->dialog), "dialog-info", sw);

    /* grab the result hbox */
    sw->result_hbox = glade_xml_get_widget (xml, "result_hbox");

    /* Grab the search-table widget */
    sw->criteria_table = glade_xml_get_widget (xml, "criteria_table");

    /* Set the type label */
    label = glade_xml_get_widget (xml, "type_label");
    if (sw->type_label)
        type_label = sw->type_label;
    else
        type_label = _(gncObjectGetTypeLabel (sw->search_for));
    gtk_label_set_text (GTK_LABEL (label), type_label);

    /* Set the 'add criterion' button */
    add = gtk_button_new_from_stock (GTK_STOCK_ADD);

    g_signal_connect (G_OBJECT (add), "clicked", G_CALLBACK (add_criterion), sw);
    box = glade_xml_get_widget (xml, "add_button_box");
    gtk_box_pack_start (GTK_BOX (box), add, FALSE, FALSE, 3);
    gtk_widget_show (add);

    /* Set the match-type menu */
    sw->grouping_combo = gtk_combo_box_new_text();
    combo_box = GTK_COMBO_BOX(sw->grouping_combo);
    gtk_combo_box_append_text(combo_box, _("all criteria are met"));
    gtk_combo_box_append_text(combo_box, _("any criteria are met"));
    gtk_combo_box_set_active(combo_box, sw->grouping);
    g_signal_connect(combo_box, "changed", G_CALLBACK (match_combo_changed), sw);

    box = glade_xml_get_widget (xml, "type_menu_box");
    gtk_box_pack_start (GTK_BOX (box), GTK_WIDGET(combo_box), FALSE, FALSE, 3);
    gtk_widget_show(GTK_WIDGET(combo_box));

    /* Grab the 'all items match' label */
    sw->match_all_label = glade_xml_get_widget (xml, "match_all_label");

    /* if there's no original query, make the narrow, add, delete
     * buttons inaccessible */
    sw->new_rb = glade_xml_get_widget (xml, "new_search_radiobutton");
    sw->narrow_rb = glade_xml_get_widget (xml, "narrow_search_radiobutton");
    sw->add_rb = glade_xml_get_widget (xml, "add_search_radiobutton");
    sw->del_rb = glade_xml_get_widget (xml, "delete_search_radiobutton");

    active = gnc_gconf_get_bool(sw->gconf_section, KEY_ACTIVE_ONLY, NULL);
    sw->active_only_check = glade_xml_get_widget (xml, "active_only_check");
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (sw->active_only_check),
                                  active);

    /* Figure out if we this object-type has an "active" parameter, and
     * if not, then set the active-check button insensitive
     */
    if (gncQueryObjectGetParameter (sw->search_for, QUERY_PARAM_ACTIVE) == NULL)
        gtk_widget_set_sensitive (sw->active_only_check, FALSE);

    /* Deal with the cancel button */
    sw->cancel_button = glade_xml_get_widget (xml, "cancel_button");
    sw->close_button = glade_xml_get_widget (xml, "close_button");

    /* Deal with the new_item button */
    new_item_button = glade_xml_get_widget (xml, "new_item_button");
    {
        char *desc =
            /* Translators: %s is either "item" or the name of some other
             * item, e.g. "Customer" or "Invoice". */
            g_strdup_printf (_("New %s"), type_label ? type_label : _("item"));
        gtk_button_set_label (GTK_BUTTON(new_item_button), desc);
        g_free (desc);
    }
    /* add the first criterion */
    gnc_search_dialog_add_criterion (sw);

    /* Hide the 'new' button if there is no new_item_cb */
    if (!sw->new_item_cb)
        gtk_widget_hide (new_item_button);

    /* Connect XML signals */

    glade_xml_signal_connect_data (xml, "gnc_ui_search_type_cb",
                                   G_CALLBACK (search_type_cb), sw);

    glade_xml_signal_connect_data (xml, "gnc_ui_search_active_cb",
                                   G_CALLBACK (search_active_only_cb), sw);

    glade_xml_signal_connect_data (xml, "gnc_ui_search_new_cb",
                                   G_CALLBACK (search_new_item_cb), sw);

    glade_xml_signal_connect_data (xml, "gnc_ui_search_find_cb",
                                   G_CALLBACK (search_find_cb), sw);

    glade_xml_signal_connect_data (xml, "gnc_ui_search_cancel_cb",
                                   G_CALLBACK (search_cancel_cb), sw);

    glade_xml_signal_connect_data (xml, "gnc_ui_search_close_cb",
                                   G_CALLBACK (search_cancel_cb), sw);

    glade_xml_signal_connect_data (xml, "gnc_ui_search_help_cb",
                                   G_CALLBACK (search_help_cb), sw);

    /* Register ourselves */
    sw->component_id = gnc_register_gui_component (DIALOG_SEARCH_CM_CLASS,
                       refresh_handler,
                       close_handler, sw);

    /* And setup the close callback */
    g_signal_connect (G_OBJECT (sw->dialog), "destroy",
                      G_CALLBACK (gnc_search_dialog_close_cb), sw);

    gnc_search_dialog_reset_widgets (sw);
    gnc_search_dialog_show_close_cancel (sw);
}

void
gnc_search_dialog_destroy (GNCSearchWindow *sw)
{
    if (!sw) return;
    if (sw->gconf_section)
        gnc_save_window_size(sw->gconf_section, GTK_WINDOW(sw->dialog));
    gnc_close_gui_component (sw->component_id);
}

void
gnc_search_dialog_raise (GNCSearchWindow *sw)
{
    if (!sw) return;
    gtk_window_present (GTK_WINDOW(sw->dialog));
}

GNCSearchWindow *
gnc_search_dialog_create (GNCIdTypeConst obj_type, const gchar *title,
                          GList *param_list,
                          GList *display_list,
                          QueryNew *start_query, QueryNew *show_start_query,
                          GNCSearchCallbackButton *callbacks,
                          GNCSearchResultCB result_callback,
                          GNCSearchNewItemCB new_item_cb,
                          gpointer user_data, GNCSearchFree free_cb,
                          const gchar *gconf_section,
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
    sw->gconf_section = gconf_section;
    sw->type_label = type_label;

    /* Grab the get_guid function */
    sw->get_guid = qof_class_get_parameter (sw->search_for, QOF_PARAM_GUID);
    if (start_query)
        sw->start_q = gncQueryCopy (start_query);
    sw->q = show_start_query;

    gnc_search_dialog_init_widgets (sw, title);
    if (sw->gconf_section)
        gnc_restore_window_size(sw->gconf_section, GTK_WINDOW(sw->dialog));
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
get_params_list (GNCIdTypeConst type)
{
    GList *list = NULL;

    list = gnc_search_param_prepend (list, "Txn: All Accounts",
                                     ACCOUNT_MATCH_ALL_TYPE,
                                     type, SPLIT_TRANS, TRANS_SPLITLIST,
                                     SPLIT_ACCOUNT_GUID, NULL);
    list = gnc_search_param_prepend (list, "Split Account", GNC_ID_ACCOUNT,
                                     type, SPLIT_ACCOUNT, QUERY_PARAM_GUID,
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
get_display_list (GNCIdTypeConst type)
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
    GNCSearchWindow *sw;
    static GList *params = NULL;
    static GList *display = NULL;
    static GNCSearchCallbackButton buttons[] =
    {
        /* Don't mark these as translatable since these are only test strings! */
        { ("View Split"), do_nothing },
        { ("New Split"), do_nothing },
        { ("Do Something"), do_nothing },
        { ("Do Nothing"), do_nothing },
        { ("Who Cares?"), do_nothing },
        { NULL }
    };

    if (params == NULL)
        params = get_params_list (GNC_ID_SPLIT);

    if (display == NULL)
        display = get_display_list (GNC_ID_SPLIT);

    sw = gnc_search_dialog_create (GNC_ID_SPLIT, _("Find Transaction"),
                                   params, display,
                                   NULL, NULL, buttons, NULL, NULL, NULL, NULL,
                                   NULL, NULL);
}
