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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
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

#include "Transaction.h"    /* for the SPLIT_* and TRANS_* */

#include "dialog-search.h"
#include "search-core-type.h"
#include "search-param.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;
static GQuark search_param_quark = 0;

#define DIALOG_SEARCH_CM_CLASS "dialog-search"
#define GNC_PREFS_GROUP_SEARCH_GENERAL "dialogs.search"
#define GNC_PREF_NEW_SEARCH_LIMIT  "new-search-limit"
#define GNC_PREF_ACTIVE_ONLY       "search-for-active-only"

typedef enum
{
    GNC_SEARCH_MATCH_ALL = 0,
    GNC_SEARCH_MATCH_ANY = 1
} GNCSearchType;

struct _GNCSearchWindow
{
    GtkWidget               *dialog;
    GtkWidget               *grouping_combo;
    GtkWidget               *match_all_label;
    GtkWidget               *criteria_table;
    GtkWidget               *criteria_scroll_window;
    GtkWidget               *result_hbox;

    /* The "results" sub-window widgets */
    GtkWidget               *result_view;

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
    GList                    *display_list; /* List of GNCSearchParamSimples for Display */
    gint                      num_cols;     /* Number of Display Columns */
    GList                    *crit_list;    /* List of crit_data */

    gint                      component_id;
    const gchar              *prefs_group;
    gboolean                  destroying;
};

struct _crit_data
{
    GNCSearchParam    *param;
    GNCSearchCoreType *element;
    GtkWidget         *elemwidget;
    GtkWidget         *container;
    GtkWidget         *button;
    GtkWindow         *dialog;
};

static void search_clear_criteria (GNCSearchWindow *sw);
static void gnc_search_dialog_display_results (GNCSearchWindow *sw);

static void
gnc_search_callback_button_execute (GNCSearchCallbackButton *cb,
                                    GNCSearchWindow *sw)
{
    GNCQueryView     *qview = GNC_QUERY_VIEW(sw->result_view);

    // Sanity check
    g_assert(qview);

    /* Do we have a callback for multi-selections ? */
    if (cb->cb_multiselect_fn && (!cb->cb_fcn ))
    {
        GList *entries = gnc_query_view_get_selected_entry_list (qview);
        // Call the callback
        (cb->cb_multiselect_fn)(GTK_WINDOW (sw->dialog), entries, sw->user_data);
        g_list_free (entries);
    }
    else
    {
        // No, stick to the single-item callback
        gpointer entry = gnc_query_view_get_selected_entry (qview);
        if (cb->cb_fcn)
            (cb->cb_fcn)(GTK_WINDOW (sw->dialog), &entry, sw->user_data);
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
    gpointer entry;
    g_return_if_fail (sw->selected_cb);

    entry = gnc_query_view_get_selected_entry (GNC_QUERY_VIEW (sw->result_view));
    if (!entry && !sw->allow_clear)
    {
        char *msg = _("You must select an item from the list");
        gnc_error_dialog (GTK_WINDOW (sw->dialog), "%s", msg);
        return;
    }

    (sw->selected_cb)(GTK_WINDOW (sw->dialog), entry, sw->select_arg);
    gnc_search_dialog_destroy (sw);
}

static void
gnc_search_dialog_select_row_cb (GNCQueryView *qview,
                                 gpointer item,
                                 gpointer user_data)
{
    GNCSearchWindow  *sw = user_data;
    gint number_of_rows = GPOINTER_TO_INT(item);
    gnc_search_dialog_select_buttons_enable(sw, number_of_rows);
}

static void
gnc_search_dialog_double_click_cb (GNCQueryView *qview,
                                   gpointer item,
                                   gpointer user_data)
{
    GNCSearchWindow  *sw = user_data;

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
    sw->result_view = gnc_query_view_new(sw->display_list, sw->q);

    gnc_query_view_set_selection_mode (GNC_QUERY_VIEW (sw->result_view),
                                       GTK_SELECTION_MULTIPLE);

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
        GtkWidget *scrolled_window, *frame, *button_box, *button;

        /* Create the view */
        gnc_search_dialog_init_result_view (sw);

        frame = gtk_frame_new (NULL);

        /* Create the scrolled_window and add the view to the scrolled_window */
        scrolled_window = gtk_scrolled_window_new ();
        gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                        GTK_POLICY_AUTOMATIC,
                                        GTK_POLICY_AUTOMATIC);
        gtk_widget_set_size_request (GTK_WIDGET(scrolled_window), 300, 100);
        gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(scrolled_window),
                                       GTK_WIDGET(sw->result_view));
        gtk_frame_set_child (GTK_FRAME(frame), GTK_WIDGET(scrolled_window));
        /* Create the button_box */
        button_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 3);
        gtk_box_set_homogeneous (GTK_BOX (button_box), FALSE);

        /* ... and add all the buttons */
        if (sw->buttons)
        {
            int i;

            button = gtk_button_new_with_label (_("Select"));
            g_signal_connect (G_OBJECT (button), "clicked",
                              G_CALLBACK (gnc_search_dialog_select_cb), sw);
            gtk_box_append (GTK_BOX(button_box), GTK_WIDGET(button));
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
                gtk_box_append (GTK_BOX(button_box), GTK_WIDGET(button));
            }
        }

        /* Add the scrolled-view and button-box to the results_box */
        gtk_box_prepend (GTK_BOX(sw->result_hbox), GTK_WIDGET(button_box));
        gtk_box_prepend (GTK_BOX(sw->result_hbox), GTK_WIDGET(frame));
        gtk_box_set_spacing (GTK_BOX(sw->result_hbox), 3);

        /* And show the results */

        /* But may be hide the select button */
        if (!sw->selected_cb)
            gtk_widget_set_visible (GTK_WIDGET(sw->select_button), FALSE);
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
        gtk_check_button_set_active(GTK_CHECK_BUTTON (sw->new_rb), TRUE);

    /* If there are results then select the first, and grab focus */
    if (gnc_query_view_get_num_entries (GNC_QUERY_VIEW(sw->result_view)) > 0)
    {
        gnc_query_view_select_first (GNC_QUERY_VIEW (sw->result_view));
        gnc_query_view_grab_focus (GNC_QUERY_VIEW (sw->result_view));
    }
}

static void
match_combo_changed (GtkDropDown *drop_down, GParamSpec *pspec,
                     GNCSearchWindow *sw)
{
    sw->grouping = gtk_drop_down_get_selected (drop_down);
    (void)pspec;
}

static void
search_type_cb (GtkCheckButton *button, GNCSearchWindow *sw)
{
    if (!gtk_check_button_get_active (button))
        return;

    if (GTK_WIDGET (button) == sw->new_rb)
        sw->search_type = 0;
    else if (GTK_WIDGET (button) == sw->narrow_rb)
        sw->search_type = 1;
    else if (GTK_WIDGET (button) == sw->add_rb)
        sw->search_type = 2;
    else if (GTK_WIDGET (button) == sw->del_rb)
        sw->search_type = 3;
}

static void
search_active_only_cb (GtkCheckButton *button, GNCSearchWindow *sw)
{

    gnc_prefs_set_bool(sw->prefs_group, GNC_PREF_ACTIVE_ONLY,
                       gtk_check_button_get_active (button));
}

static QofQuery *
create_query_fragment (QofIdTypeConst search_for, GNCSearchParam *param, QofQueryPredData *pdata)
{
    GNCSearchParamKind kind = gnc_search_param_get_kind (param);
    QofQuery *q = qof_query_create_for (search_for);

    if (kind == SEARCH_PARAM_ELEM)
    {
        /* The "op" parameter below will be ignored since q has no terms. */
        qof_query_add_term (q, gnc_search_param_get_param_path (GNC_SEARCH_PARAM_SIMPLE (param)),
                            pdata, QOF_QUERY_OR);
    }
    else
    {
        GList *plist = gnc_search_param_get_search (GNC_SEARCH_PARAM_COMPOUND (param));

        for ( ; plist; plist  = plist->next)
        {
            QofQuery *new_q;
            GNCSearchParam *param2 = plist->data;
            QofQuery *q2 = create_query_fragment (search_for, param2,
                                                  qof_query_core_predicate_copy (pdata));
            new_q = qof_query_merge (q, q2, kind == SEARCH_PARAM_ANY ?
                                                    QOF_QUERY_OR : QOF_QUERY_AND);
            qof_query_destroy (q);
            qof_query_destroy (q2);
            q = new_q;
        }
        qof_query_core_predicate_free (pdata);
    }
    return q;
}

static void
search_update_query (GNCSearchWindow *sw)
{
    static GSList *active_params = NULL;
    QofQuery *q, *q2, *new_q;
    GList *node;
    QofQueryOp op;

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
        QofQueryPredData* pdata;

        pdata = gnc_search_core_type_get_predicate (data->element);
        if (pdata)
        {
            q2 = create_query_fragment(sw->search_for, GNC_SEARCH_PARAM (data->param), pdata);
            new_q = qof_query_merge (q, q2, op);
            qof_query_destroy (q);
            qof_query_destroy (q2);
            q = new_q;
        }
    }

    /* Now combine this query with the existing query, depending on
     * what we want to do...  We can assume that cases 1, 2, and 3
     * already have sw->q being valid!
     */

    switch (sw->search_type)
    {
    case 0:         /* New */
        new_q = qof_query_merge (sw->start_q, q, QOF_QUERY_AND);
        qof_query_destroy (q);
        break;
    case 1:         /* Refine */
        new_q = qof_query_merge (sw->q, q, QOF_QUERY_AND);
        qof_query_destroy (q);
        break;
    case 2:         /* Add */
        new_q = qof_query_merge (sw->q, q, QOF_QUERY_OR);
        qof_query_destroy (q);
        break;
    case 3:         /* Delete */
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

    if (gtk_check_button_get_active (GTK_CHECK_BUTTON (sw->active_only_check)))
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
        gtk_widget_set_visible (GTK_WIDGET(sw->cancel_button), TRUE);
        gtk_widget_set_visible (GTK_WIDGET(sw->close_button), FALSE);
    }
    else
    {
        gtk_widget_set_visible (GTK_WIDGET(sw->cancel_button), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(sw->close_button), TRUE);
    }
}

static void
gnc_search_dialog_reset_widgets (GNCSearchWindow *sw)
{
    gboolean sens = (sw->q != NULL);
    gboolean crit_list_vis = FALSE;

    gtk_widget_set_sensitive(GTK_WIDGET(sw->narrow_rb), sens);
    gtk_widget_set_sensitive(GTK_WIDGET(sw->add_rb), sens);
    gtk_widget_set_sensitive(GTK_WIDGET(sw->del_rb), sens);

    if (sw->q)
    {
        gtk_check_button_set_active(GTK_CHECK_BUTTON (sw->new_rb), FALSE);
        gtk_check_button_set_active(GTK_CHECK_BUTTON (sw->narrow_rb), TRUE);
    }

    if (sw->crit_list)
        crit_list_vis = TRUE;

    gtk_widget_set_sensitive(sw->grouping_combo, crit_list_vis);
    gtk_widget_set_visible (sw->criteria_scroll_window, crit_list_vis);
    gtk_widget_set_visible (sw->match_all_label, !crit_list_vis);
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
    {
        gpointer entry = NULL;
        if (sw->result_view)
        {
            GNCQueryView *qview = GNC_QUERY_VIEW (sw->result_view);
            entry = gnc_query_view_get_selected_entry (qview);
        }
        (sw->result_cb)(sw->q, sw->user_data, &entry);
    }
    else
        gnc_search_dialog_display_results (sw);
}

static void
search_new_item_cb (GtkButton *button, GNCSearchWindow *sw)
{
    gpointer res;

    g_return_if_fail (sw->new_item_cb);

    res = (sw->new_item_cb)(GTK_WINDOW (sw->dialog), sw->user_data);

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
    gnc_search_dialog_destroy (sw);
}

static void
search_help_cb (GtkButton *button, GNCSearchWindow *sw)
{
    gnc_gnome_help (GTK_WINDOW(sw->dialog), DF_MANUAL, DL_FIND_TRANSACTIONS);
}

static void
reflow_criteria (GNCSearchWindow *sw)
{
    GPtrArray *widgets;
    GList *node;
    guint row = 0;

    widgets = g_ptr_array_new_with_free_func (g_object_unref);
    for (node = sw->crit_list; node; node = node->next)
    {
        struct _crit_data *data = node->data;

        g_ptr_array_add (widgets, g_object_ref (data->container));
        g_ptr_array_add (widgets, g_object_ref (data->button));
    }

    for (node = sw->crit_list; node; node = node->next)
    {
        struct _crit_data *data = node->data;

        gtk_grid_remove (GTK_GRID (sw->criteria_table), data->container);
        gtk_grid_remove (GTK_GRID (sw->criteria_table), data->button);
    }

    for (node = sw->crit_list; node; node = node->next)
    {
        struct _crit_data *data = node->data;

        gtk_grid_attach (GTK_GRID (sw->criteria_table), data->container,
                         0, row, 1, 1);
        gtk_grid_attach (GTK_GRID (sw->criteria_table), data->button,
                         1, row, 1, 1);
        row++;
    }

    g_ptr_array_unref (widgets);
}

static void
remove_element (GtkWidget *button, GNCSearchWindow *sw)
{
    GtkWidget *element;
    struct _crit_data *data;

    if (!sw->crit_list)
        return;

    element = g_object_get_data (G_OBJECT (button), "element");
    data = g_object_get_data (G_OBJECT (element), "data");

    /* Remove the criterion before unparenting its widgets. */
    sw->crit_list = g_list_remove (sw->crit_list, data);
    gtk_grid_remove (GTK_GRID (sw->criteria_table), element);
    gtk_grid_remove (GTK_GRID (sw->criteria_table), button);

    if (sw->crit_list)
        reflow_criteria (sw);
    else
    {
        gtk_widget_set_sensitive (sw->grouping_combo, FALSE);
        gtk_widget_set_visible (sw->match_all_label, TRUE);
        gtk_widget_set_visible (sw->criteria_scroll_window, FALSE);
    }
}

static void
attach_element (GtkWidget *element, GNCSearchWindow *sw, guint row)
{
    GtkWidget *remove;
    struct _crit_data *data;

    data = g_object_get_data (G_OBJECT (element), "data");
    gnc_search_core_type_pass_parent (data->element, GTK_WINDOW (sw->dialog));

    gtk_grid_attach (GTK_GRID (sw->criteria_table), element, 0, row, 1, 1);
    gtk_widget_set_hexpand (element, TRUE);
    gtk_widget_set_halign (element, GTK_ALIGN_FILL);
    g_object_set (element, "margin", 0, NULL);

    remove = gtk_button_new_with_mnemonic (_("_Remove"));
    g_object_set_data (G_OBJECT (remove), "element", element);
    g_signal_connect (remove, "clicked", G_CALLBACK (remove_element), sw);
    gtk_grid_attach (GTK_GRID (sw->criteria_table), remove, 1, row, 1, 1);
    gtk_widget_set_hexpand (remove, FALSE);
    gtk_widget_set_halign (remove, GTK_ALIGN_CENTER);
    g_object_set (remove, "margin", 0, NULL);

    gtk_widget_set_visible (element, TRUE);
    gtk_widget_set_visible (remove, TRUE);
    data->button = remove;
}

static GNCSearchParam *
search_dropdown_get_param (GtkDropDown *drop_down)
{
    GObject *item;
    GNCSearchParam *param;

    item = gtk_drop_down_get_selected_item (drop_down);
    if (!item)
        return NULL;

    param = g_object_get_qdata (item, search_param_quark);
    return param;
}

static void
combo_box_changed (GtkDropDown *drop_down, GParamSpec *pspec,
                   struct _crit_data *data)
{
    GNCSearchParam *param = search_dropdown_get_param (drop_down);
    GNCSearchCoreType *newelem;

    if (!param)
        return;

    if (gnc_search_param_type_match (param, data->param))
    {
        /* The parameter type is unchanged, so retain the editor. */
        data->param = param;
        return;
    }
    data->param = param;

    /* Recreate only the editor. The criterion record remains stable. */
    if (data->elemwidget)
        gtk_box_remove (GTK_BOX (data->container), data->elemwidget);
    g_object_unref (data->element);

    newelem = gnc_search_core_type_new_type_name
              (gnc_search_param_get_param_type (param));
    data->element = newelem;
    data->elemwidget = gnc_search_core_type_get_widget (newelem);
    if (data->elemwidget)
    {
        gtk_box_append (GTK_BOX (data->container), data->elemwidget);
        gtk_widget_set_visible (data->elemwidget, TRUE);
    }

    gnc_search_core_type_pass_parent (data->element, data->dialog);
    gtk_widget_queue_resize (GTK_WIDGET (data->dialog));
    gnc_search_core_type_grab_focus (newelem);
    gnc_search_core_type_editable_enters (newelem);
    (void)pspec;
}

static void
search_clear_criteria (GNCSearchWindow *sw)
{
    GList *node;

    for (node = sw->crit_list; node; )
    {
        GList *tmp = node->next;
        struct _crit_data *data = node->data;
        GtkWidget *button = g_object_ref (data->button);

        remove_element (button, sw);
        g_object_unref (button);
        node = tmp;
    }
}

static GtkWidget *
get_comb_box_widget (GNCSearchWindow *sw, struct _crit_data *data)
{
    GtkStringList *model;
    GtkWidget *drop_down;
    GList *node;
    guint index = 0;
    guint current = 0;

    if (G_UNLIKELY (search_param_quark == 0))
        search_param_quark = g_quark_from_static_string ("gnc-search-param");

    model = gtk_string_list_new (NULL);
    for (node = sw->params_list; node; node = node->next)
    {
        GNCSearchParam *param = node->data;
        GObject *item;

        gtk_string_list_append (model, _(gnc_search_param_get_title (param)));
        item = g_list_model_get_item (G_LIST_MODEL (model), index);
        g_object_set_qdata (item, search_param_quark, param);
        g_object_unref (item);

        if (param == sw->last_param)
            current = index;
        index++;
    }

    drop_down = GTK_WIDGET (gnc_gtk_drop_down_new (G_LIST_MODEL (model), NULL));
    gtk_drop_down_set_selected (GTK_DROP_DOWN (drop_down), current);
    g_signal_connect (drop_down, "notify::selected",
                      G_CALLBACK (combo_box_changed), data);
    gtk_widget_set_visible (drop_down, TRUE);

    return drop_down;
}

static GtkWidget *
get_element_widget (GNCSearchWindow *sw, GNCSearchCoreType *element)
{
    GtkWidget *combo_box, *hbox, *p;
    struct _crit_data *data;

    data = g_new0 (struct _crit_data, 1);
    data->element = element;
    data->dialog = GTK_WINDOW (sw->dialog);

    hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);

    /* only set to automatically clean up the memory */
    g_object_set_data_full (G_OBJECT (hbox), "data", data, g_free);

    p = gnc_search_core_type_get_widget (element);
    data->elemwidget = p;
    data->container = hbox;
    data->param = sw->last_param;

    combo_box = get_comb_box_widget (sw, data);
    gtk_box_append (GTK_BOX (hbox), combo_box);
    if (p)
    {
        gtk_box_append (GTK_BOX (hbox), p);
        gtk_widget_set_visible (p, TRUE);
    }
    gtk_widget_set_visible (hbox, TRUE);

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
            if (strcmp (gnc_search_param_get_title (param), N_("Action")) == 0)
                gnc_search_param_set_title (param, N_("Number/Action"));
            if (strcmp (gnc_search_param_get_title (param), N_("Number")) == 0)
                gnc_search_param_set_title (param, N_("Transaction Number"));
        }
        else
        {
            if (strcmp (gnc_search_param_get_title (param), N_("Number/Action")) == 0)
                gnc_search_param_set_title (param, N_("Action"));
            if (strcmp (gnc_search_param_get_title (param), N_("Transaction Number")) == 0)
                gnc_search_param_set_title (param, N_("Number"));
        }
    }
    /* Refresh visible criterion labels without changing their values. */
    for (l = sw->crit_list; l; l = l->next)
    {
        struct _crit_data *data = l->data;
        GtkWidget *child;

        for (child = gtk_widget_get_first_child (data->container); child;
             child = gtk_widget_get_next_sibling (child))
        {
            GtkWidget *new_drop_down;
            guint selected;

            if (!GTK_IS_DROP_DOWN (child))
                continue;

            selected = gtk_drop_down_get_selected (GTK_DROP_DOWN (child));
            new_drop_down = get_comb_box_widget (sw, data);
            if (focused_widget == child)
                focused_widget = new_drop_down;

            gtk_box_remove (GTK_BOX (data->container), child);
            gtk_drop_down_set_selected (GTK_DROP_DOWN (new_drop_down), selected);
            gtk_box_prepend (GTK_BOX (data->container), new_drop_down);
            break;
        }
    }
    gtk_widget_grab_focus(focused_widget);
}

static void
gnc_search_dialog_add_criterion (GNCSearchWindow *sw)
{
    GNCSearchCoreType *new_sct;
    guint row = g_list_length (sw->crit_list);

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
        gtk_widget_set_visible (GTK_WIDGET(sw->match_all_label), FALSE);
        gtk_widget_set_visible (GTK_WIDGET(sw->criteria_scroll_window), TRUE);
    }
    /* create a new criterion element */
    new_sct = gnc_search_core_type_new_type_name
          (gnc_search_param_get_param_type (sw->last_param));

    if (new_sct)
    {
        struct _crit_data *data;
        GtkWidget *w;

        w = get_element_widget (sw, new_sct);
        data = g_object_get_data (G_OBJECT (w), "data");
        sw->crit_list = g_list_append (sw->crit_list, data);
        attach_element (w, sw, row);

        gnc_search_core_type_grab_focus (new_sct);
        gnc_search_core_type_editable_enters (new_sct);
    }
}

static void
add_criterion (GtkWidget *button, GNCSearchWindow *sw)
{
    gint number_of_buttons = g_list_length (sw->crit_list) + 1;
    gint button_height = gtk_widget_get_height (button);
    gint min_height = MIN (number_of_buttons * button_height, 5 * button_height);

    // this sets the minimum content height for the criteria scroll
    // window, it is set to a max of 5 buttons visible without scrolling
    gtk_scrolled_window_set_min_content_height (GTK_SCROLLED_WINDOW(
                                                sw->criteria_scroll_window),
                                                min_height + (button_height/2));

    gnc_search_dialog_add_criterion (sw);
}

static void
gnc_search_dialog_destroyed_cb (GtkWidget *dialog, GNCSearchWindow *sw)
{
    g_return_if_fail (sw);

    if (!sw->destroying && sw->prefs_group)
        gnc_save_window_size (sw->prefs_group, GTK_WINDOW (dialog));
    sw->dialog = NULL;

    /* Unregister callback on book option changes originally registered
     * if searching for splits. */
    if (strcmp (sw->search_for, GNC_ID_SPLIT) == 0)
        gnc_book_option_remove_cb (OPTION_NAME_NUM_FIELD_SOURCE,
                                   gnc_search_dialog_book_option_changed, sw);

    if (sw->component_id)
        gnc_unregister_gui_component (sw->component_id);

    g_list_free (sw->crit_list);
    g_list_free (sw->button_list);

    if (sw->q)
        qof_query_destroy (sw->q);
    if (sw->start_q)
        qof_query_destroy (sw->start_q);
    if (sw->free_cb)
        sw->free_cb (sw->user_data);

    g_free (sw);
}

static gboolean
gnc_search_dialog_close_request_cb (GtkWindow *dialog, GNCSearchWindow *sw)
{
    (void)dialog;
    gnc_search_dialog_destroy (sw);
    return TRUE;
}

static gboolean
gnc_search_dialog_key_pressed_cb (GtkEventControllerKey *controller,
                                  guint keyval, guint keycode,
                                  GdkModifierType state,
                                  GNCSearchWindow *sw)
{
    (void)controller;
    (void)keycode;
    (void)state;

    if (keyval != GDK_KEY_Escape)
        return FALSE;

    gnc_search_dialog_destroy (sw);
    return TRUE;
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
    GNCSearchWindow *sw = data;

    g_return_if_fail (sw);
    if (sw->dialog)
        gtk_window_destroy (GTK_WINDOW (sw->dialog));
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
        return C_("Item represents an unknown object type (in the sense of bill, customer, invoice, transaction, split,…)!", "New item");
    }
}

static void
gnc_search_dialog_init_widgets (GNCSearchWindow *sw, const gchar *title)
{
    GtkBuilder        *builder;
    GtkWidget         *label, *add, *box;
    GtkWidget         *widget;
    GtkWidget         *new_item_button;
    const char        *type_label;
    gboolean           active;

    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-search.glade", "search_dialog");

    /* Grab the dialog, save the dialog info */
    sw->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "search_dialog"));
    gtk_window_set_title(GTK_WINDOW(sw->dialog), title);
    g_object_set_data (G_OBJECT (sw->dialog), "dialog-info", sw);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(sw->dialog), "gnc-id-search");
    gnc_widget_style_context_add_class (GTK_WIDGET(sw->dialog), "gnc-class-search");

    /* Grab the result hbox */
    sw->result_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "result_hbox"));

    /* Grab the search-table widget */
    sw->criteria_table = GTK_WIDGET(gtk_builder_get_object (builder, "criteria_table"));
    sw->criteria_scroll_window = GTK_WIDGET(gtk_builder_get_object (builder, "criteria_scroll_window"));

    /* Set the type label */
    label = GTK_WIDGET(gtk_builder_get_object (builder, "type_label"));
    if (sw->type_label)
        type_label = sw->type_label;
    else
        type_label = _(qof_object_get_type_label (sw->search_for));
    gtk_label_set_text (GTK_LABEL (label), type_label);

    /* Set the 'add criterion' button */
    add = gtk_button_new_with_mnemonic (_("_Add"));

    g_signal_connect (G_OBJECT (add), "clicked", G_CALLBACK (add_criterion), sw);
    box = GTK_WIDGET(gtk_builder_get_object (builder, "add_button_box"));
    gtk_box_append (GTK_BOX(box), GTK_WIDGET(add));
    gtk_widget_set_visible (GTK_WIDGET(add), TRUE);

    /* Set the match-type menu. The enum values deliberately equal the
     * stable positions in this model. */
    {
        const char * const grouping_labels[] =
        {
            _("all criteria are met"),
            _("any criteria are met"),
            NULL
        };
        GtkStringList *grouping_model = gtk_string_list_new (grouping_labels);

        sw->grouping_combo = GTK_WIDGET (gnc_gtk_drop_down_new (G_LIST_MODEL (grouping_model), NULL));
        gtk_drop_down_set_selected (GTK_DROP_DOWN (sw->grouping_combo),
                                    sw->grouping);
        g_signal_connect (sw->grouping_combo, "notify::selected",
                          G_CALLBACK (match_combo_changed), sw);

        box = GTK_WIDGET(gtk_builder_get_object (builder, "type_menu_box"));
        gtk_box_append (GTK_BOX(box), sw->grouping_combo);
        gtk_widget_set_visible (sw->grouping_combo, TRUE);
    }

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
    gtk_check_button_set_active (GTK_CHECK_BUTTON (sw->active_only_check), active);
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
    gtk_window_set_default_widget (GTK_WINDOW (sw->dialog), widget);

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
        gtk_widget_set_visible (GTK_WIDGET(new_item_button), FALSE);

    /* Connect all the signals */
gnc_builder_connect_signals (builder, sw);

    /* Register ourselves */
    sw->component_id = gnc_register_gui_component (DIALOG_SEARCH_CM_CLASS,
                       refresh_handler,
                       close_handler, sw);
    gnc_gui_component_set_session (sw->component_id,
                                   gnc_get_current_session());

    g_signal_connect (sw->dialog, "close-request",
                      G_CALLBACK (gnc_search_dialog_close_request_cb), sw);
    g_signal_connect (sw->dialog, "destroy",
                      G_CALLBACK (gnc_search_dialog_destroyed_cb), sw);
    {
        GtkEventController *key_controller = gtk_event_controller_key_new ();

        gtk_widget_add_controller (sw->dialog, key_controller);
        g_signal_connect (key_controller, "key-pressed",
                          G_CALLBACK (gnc_search_dialog_key_pressed_cb), sw);
    }

    gnc_search_dialog_reset_widgets (sw);
    gnc_search_dialog_show_close_cancel (sw);

    g_object_unref(G_OBJECT(builder));
}

void
gnc_search_dialog_destroy (GNCSearchWindow *sw)
{
    if (!sw || sw->destroying)
        return;

    sw->destroying = TRUE;
    if (sw->prefs_group && sw->dialog)
        gnc_save_window_size (sw->prefs_group, GTK_WINDOW (sw->dialog));
    if (sw->component_id)
        gnc_close_gui_component (sw->component_id);
    else if (sw->dialog)
        gtk_window_destroy (GTK_WINDOW (sw->dialog));
}

void
gnc_search_dialog_raise (GNCSearchWindow *sw)
{
    if (!sw) return;
    gtk_window_present (GTK_WINDOW(sw->dialog));
}

GNCSearchWindow *
gnc_search_dialog_create (GtkWindow *parent,
                          QofIdTypeConst obj_type, const gchar *title,
                          GList *param_list,
                          GList *display_list,
                          QofQuery *start_query, QofQuery *show_start_query,
                          GNCSearchCallbackButton *callbacks,
                          GNCSearchResultCB result_callback,
                          GNCSearchNewItemCB new_item_cb,
                          gpointer user_data, GNCSearchFree free_cb,
                          const gchar *prefs_group,
                          const gchar *type_label,
                          const gchar *style_class)
{
    GNCSearchWindow *sw = g_new0 (GNCSearchWindow, 1);
    gint grouping_minimum_height;
    gint grouping_natural_height;

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
    gtk_widget_measure (GTK_WIDGET(sw->grouping_combo),
                        GTK_ORIENTATION_VERTICAL, -1,
                        &grouping_minimum_height,
                        &grouping_natural_height, NULL, NULL);
    gtk_scrolled_window_set_min_content_height (
        GTK_SCROLLED_WINDOW(sw->criteria_scroll_window),
        MAX (grouping_minimum_height, grouping_natural_height) * 3 / 2);
    if (sw->prefs_group)
        gnc_restore_window_size(sw->prefs_group, GTK_WINDOW(sw->dialog), parent);
    gtk_window_set_transient_for(GTK_WINDOW(sw->dialog), parent);
    gtk_widget_set_visible (GTK_WIDGET(sw->dialog), TRUE);

    // Add a style context for this dialog so it can be easily manipulated with css
    if (style_class != NULL)
        gnc_widget_style_context_add_class (GTK_WIDGET(sw->dialog), style_class);

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
        if (sw->selected_cb)
            gtk_widget_set_visible (GTK_WIDGET(sw->select_button), TRUE);
        else
            gtk_widget_set_visible (GTK_WIDGET(sw->select_button), FALSE);
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
do_nothing (GtkWindow *dialog, gpointer *a, gpointer b)
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
    gnc_search_dialog_create (NULL, GNC_ID_SPLIT,
                  _("Find Transaction"),
                  params, display,
                  NULL, NULL, buttons, NULL, NULL, NULL, NULL,
                  NULL, NULL, NULL);
}
