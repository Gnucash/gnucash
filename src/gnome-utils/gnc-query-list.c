/********************************************************************\
 * gnc-query-list.c -- A query display list.                        *
 * Copyright (C) 2003 Derek Atkins <derek@ihtfp.com>                *
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

#include "dialog-utils.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-component-manager.h"
#include "gnc-query-list.h"
#include "search-param.h"
#include "QueryCore.h"
#include "QueryNew.h"
#include "QueryObject.h"

/* Signal codes */
enum
{
    LINE_TOGGLED,
    DOUBLE_CLICK_ENTRY,
    LAST_SIGNAL
};

typedef struct _GNCQueryListPriv  GNCQueryListPriv;

struct _GNCQueryListPriv
{
    const QofParam * get_guid;
    gint	          component_id;
};

#define GNC_QUERY_LIST_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_QUERY_LIST, GNCQueryListPriv))

/* Impossible to get at runtime. Assume this is a reasonable number */
#define ARROW_SIZE      14
#define VSCROLLBAR_SLOP 40


/** Static Globals ****************************************************/
static GtkCListClass *parent_class = NULL;
static guint query_list_signals[LAST_SIGNAL] = {0};


/** Static function declarations **************************************/
static void gnc_query_list_init(GNCQueryList *list);
static void gnc_query_list_init_clist (GNCQueryList *list);
static void gnc_query_list_class_init(GNCQueryListClass *klass);
static void gnc_query_list_select_row(GtkCList *clist, gint row,
                                      gint column, GdkEvent *event);
static void gnc_query_list_unselect_row(GtkCList *clist, gint row,
                                        gint column, GdkEvent *event);
static void gnc_query_list_destroy(GtkObject *object);
static void gnc_query_list_fill(GNCQueryList *list);
static void gnc_query_list_click_column_cb(GtkWidget *w, gint column,
        gpointer data);
static void gnc_query_list_size_allocate_cb(GtkWidget *w,
        GtkAllocation *allocation,
        gpointer data);

static void gnc_query_list_set_query_sort (GNCQueryList *list, gboolean new_column);

GType
gnc_query_list_get_type (void)
{
    static GType gnc_query_list_type = 0;

    if (!gnc_query_list_type)
    {
        GTypeInfo type_info =
        {
            sizeof(GNCQueryListClass),        /* class_size */
            NULL,   				/* base_init */
            NULL,				/* base_finalize */
            (GClassInitFunc)gnc_query_list_class_init,
            NULL,				/* class_finalize */
            NULL,				/* class_data */
            sizeof (GNCQueryList),		/* */
            0,				/* n_preallocs */
            (GInstanceInitFunc)gnc_query_list_init,
        };

        gnc_query_list_type = g_type_register_static(GTK_TYPE_CLIST,
                              "GNCQueryList",
                              &type_info, 0);
    }

    return gnc_query_list_type;
}


/********************************************************************\
 * gnc_query_list_new                                               *
 *   creates the query list                                         *
 *                                                                  *
 * Args: param_list - the list of params                            *
 *       query      - the query to use to find entries              *
 * Returns: the query list widget, or NULL if there was a problem.  *
\********************************************************************/
void
gnc_query_list_construct (GNCQueryList *list, GList *param_list, Query *query)
{
    GNCQueryListPriv *priv;

    g_return_if_fail(list);
    g_return_if_fail(param_list);
    g_return_if_fail(query);
    g_return_if_fail(IS_GNC_QUERY_LIST(list));

    /* more configuration */
    list->query = gncQueryCopy(query);
    list->column_params = param_list;

    /* cache the function to get the guid of this query type */
    priv = GNC_QUERY_LIST_GET_PRIVATE(list);
    priv->get_guid =
        qof_class_get_parameter (qof_query_get_search_for(query), QOF_PARAM_GUID);

    /* Initialize the CList */
    gnc_query_list_init_clist(list);

    /* Set initial sort order */
    gnc_query_list_set_query_sort(list, TRUE);
}


GtkWidget *
gnc_query_list_new(GList *param_list, Query *query)
{
    GNCQueryList *list;
    gint columns;

    g_return_val_if_fail(param_list, NULL);
    g_return_val_if_fail(query, NULL);

    columns = g_list_length(param_list);
    list = GNC_QUERY_LIST(g_object_new(gnc_query_list_get_type(),
                                       "n_columns", columns,
                                       NULL));

    gnc_query_list_construct(list, param_list, query);

    return GTK_WIDGET(list);
}

void gnc_query_list_reset_query (GNCQueryList *list, Query *query)
{
    g_return_if_fail(list);
    g_return_if_fail(query);
    g_return_if_fail (IS_GNC_QUERY_LIST(list));

    gncQueryDestroy(list->query);
    list->query = gncQueryCopy(query);
    gnc_query_list_set_query_sort(list, TRUE);
}

static void
update_booleans (GNCQueryList *list, gint row)
{
    GtkCList *clist = GTK_CLIST(list);
    gpointer entry;
    GList *node;
    gint i;
    gboolean result;

    entry = gtk_clist_get_row_data (clist, row);
    for (i = 0, node = list->column_params; node; node = node->next, i++)
    {
        GNCSearchParam *param = node->data;
        const char *type = gnc_search_param_get_param_type (param);

        /* if this is a boolean, ignore it now -- we'll use a checkmark later */
        if (safe_strcmp (type, QUERYCORE_BOOLEAN))
            continue;

        result = (gboolean) GPOINTER_TO_INT(gnc_search_param_compute_value(param, entry));
        gnc_clist_set_check (clist, row, i, result);
    }
}

static void
gnc_query_list_column_title (GNCQueryList *list, gint column, const gchar *title)
{
    GtkWidget *hbox, *label, *arrow;

    hbox = gtk_hbox_new(FALSE, 2);
    gtk_widget_show(hbox);
    gtk_clist_set_column_widget(GTK_CLIST(list), column, hbox);

    label = gtk_label_new(title);
    gtk_widget_show(label);
    gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

    arrow = gtk_arrow_new(GTK_ARROW_DOWN, GTK_SHADOW_ETCHED_IN);
    list->title_arrows[column] = arrow;
    if (column == 0)
        gtk_widget_show(arrow);
    gtk_box_pack_end(GTK_BOX(hbox), arrow, FALSE, FALSE, 0);
}

static void
gnc_query_list_refresh_handler (GHashTable *changes, gpointer user_data)
{
    GNCQueryList *list = (GNCQueryList *)user_data;
    g_return_if_fail (list);
    g_return_if_fail (IS_GNC_QUERY_LIST(list));

    gnc_query_list_refresh (list);
}

static void
gnc_query_list_init (GNCQueryList *list)
{
    GNCQueryListPriv *priv;

    list->query = NULL;
    list->no_toggle = FALSE;
    list->always_unselect = FALSE;

    list->num_columns = 0;
    list->column_params = NULL;

    list->sort_column = 0;
    list->increasing = TRUE;
    list->title_arrows = NULL;

    list->prev_allocation = -1;
    list->title_widths = NULL;

    list->numeric_abs = FALSE;
    list->numeric_inv_sort = FALSE;

    priv = GNC_QUERY_LIST_GET_PRIVATE(list);
    priv->component_id =
        gnc_register_gui_component ("gnc-query-list-cm-class",
                                    gnc_query_list_refresh_handler,
                                    NULL, list);
}

static void
gnc_query_list_init_clist (GNCQueryList *list)
{
    GtkCList *clist = GTK_CLIST (list);
    GtkStyle *style;
    GList *node;
    gchar **titles;
    gint i;

    /* compute the number of columns and fill in the rest of the list */
    list->num_columns = g_list_length(list->column_params);
    list->title_arrows = g_new0(GtkWidget*, list->num_columns);
    list->title_widths = g_new0(gint, list->num_columns);

    /* build an array of titles */
    titles = g_new0(gchar*, list->num_columns);
    for (i = 0, node = list->column_params; node; node = node->next, i++)
    {
        GNCSearchParam *param = node->data;
        titles[i] = (gchar *)param->title;
    }

    gtk_clist_column_titles_show (clist);
    gtk_clist_set_shadow_type (clist, GTK_SHADOW_IN);

    /* build all the column titles */
    for (i = 0; i < list->num_columns; i++)
        gnc_query_list_column_title(list, i, titles[i]);

    /* set the column justification */
    for (i = 0, node = list->column_params; node; node = node->next, i++)
    {
        GNCSearchParam *param = node->data;
        gtk_clist_set_column_justification (clist, i, param->justify);

        if (param->passive)
            gtk_clist_column_title_passive (clist, i);

        if (param->non_resizeable)
            gtk_clist_set_column_resizeable (clist, i, FALSE);
    }

    g_signal_connect (clist, "click_column",
                      G_CALLBACK(gnc_query_list_click_column_cb),
                      NULL);
    g_signal_connect (clist, "size_allocate",
                      G_CALLBACK(gnc_query_list_size_allocate_cb),
                      NULL);

    style = gtk_widget_get_style (GTK_WIDGET(list));

#if 0
    {
        GdkFont *font = NULL;
        gint width;

        font = style->font;
        if (font != NULL)
        {
            for (i = 0, node = list->column_params; node; node = node->next, i++)
            {
                GNCSearchParam *param = node->data;
                width = gdk_string_width (font, titles[i]) + 5;
                if (!param->passive)
                    width += ARROW_SIZE;
                gtk_clist_set_column_min_width (clist, i, width);
                list->title_widths[i] = width;
            }
        }
    }
#endif
    g_free(titles);
}

static void
gnc_query_list_class_init (GNCQueryListClass *klass)
{
    GtkObjectClass    *object_class;
    GtkWidgetClass    *widget_class;
    GtkContainerClass *container_class;
    GtkCListClass     *clist_class;

    object_class =    (GtkObjectClass*) klass;
    widget_class =    (GtkWidgetClass*) klass;
    container_class = (GtkContainerClass*) klass;
    clist_class =     (GtkCListClass*) klass;

    parent_class = gtk_type_class(GTK_TYPE_CLIST);

    g_type_class_add_private(klass, sizeof(GNCQueryListPriv));

    query_list_signals[LINE_TOGGLED] =
        g_signal_new("line_toggled",
                     G_TYPE_FROM_CLASS (object_class),
                     G_SIGNAL_RUN_FIRST,
                     G_STRUCT_OFFSET(GNCQueryListClass, line_toggled),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__POINTER,
                     G_TYPE_NONE,
                     1,
                     G_TYPE_POINTER);

    query_list_signals[DOUBLE_CLICK_ENTRY] =
        g_signal_new("double_click_entry",
                     G_TYPE_FROM_CLASS (object_class),
                     G_SIGNAL_RUN_FIRST,
                     G_STRUCT_OFFSET(GNCQueryListClass, double_click_entry),
                     NULL, NULL,
                     g_cclosure_marshal_VOID__POINTER,
                     G_TYPE_NONE,
                     1,
                     G_TYPE_POINTER);

    object_class->destroy = gnc_query_list_destroy;

    clist_class->select_row = gnc_query_list_select_row;
    clist_class->unselect_row = gnc_query_list_unselect_row;

    klass->line_toggled = NULL;
    klass->double_click_entry = NULL;
}

static void
gnc_query_list_toggle (GNCQueryList *list)
{
    gpointer entry;
    gint row;

    g_return_if_fail (IS_GNC_QUERY_LIST(list));

    if (list->no_toggle)
        return;

    row = list->current_row;
    entry = gtk_clist_get_row_data (GTK_CLIST(list), row);
    list->current_entry = entry;

    g_signal_emit (list, query_list_signals[LINE_TOGGLED], 0, entry);

    update_booleans (list, row);
}

static void
gnc_query_list_select_row (GtkCList *clist, gint row, gint column,
                           GdkEvent *event)
{
    GNCQueryList *list = GNC_QUERY_LIST(clist);

    list->current_row = row;

    gnc_query_list_toggle (list);
    if (event == NULL)
    {
        /* User pressed the space key */
        parent_class->scroll_vertical(clist, GTK_SCROLL_STEP_FORWARD, 0.0);
    }

    /* This will trigger an unselect event for the currently selected row */
    parent_class->select_row (clist, row, column, event);

    if (event && (event->type == GDK_2BUTTON_PRESS))
    {
        gpointer entry;

        entry = gtk_clist_get_row_data (clist, row);

        g_signal_emit(list, query_list_signals[DOUBLE_CLICK_ENTRY], 0, entry);
    }
}

static void
gnc_query_list_unselect_row (GtkCList *clist, gint row, gint column,
                             GdkEvent *event)
{
    GNCQueryList *list = GNC_QUERY_LIST(clist);

    if (row == list->current_row)
    {
        gnc_query_list_toggle (list);
        if (event == NULL)
        {
            /* User pressed the space key */
            parent_class->scroll_vertical(clist, GTK_SCROLL_STEP_FORWARD, 0.0);
        }
        if (!list->always_unselect)
            return;
    }

    parent_class->unselect_row (clist, row, column, event);

    if (event && (event->type == GDK_2BUTTON_PRESS))
    {
        gpointer entry;

        entry = gtk_clist_get_row_data (clist, row);

        g_signal_emit (list, query_list_signals[DOUBLE_CLICK_ENTRY], 0, entry);
    }
}

static void
gnc_query_list_destroy (GtkObject *object)
{
    GNCQueryList *list = GNC_QUERY_LIST(object);
    GNCQueryListPriv *priv;

    priv = GNC_QUERY_LIST_GET_PRIVATE(list);
    if (priv->component_id > 0)
    {
        gnc_unregister_gui_component (priv->component_id);
        priv->component_id = 0;
    }
    if (list->query)
    {
        xaccFreeQuery(list->query);
        list->query = NULL;
    }
    if (list->column_params)
    {
        /* XXX: free the params list??? */
    }
    if (list->title_arrows)
    {
        g_free(list->title_arrows);
        list->title_arrows = NULL;
    }
    if (list->title_widths)
    {
        g_free(list->title_widths);
        list->title_widths = NULL;
    }

    if (GTK_OBJECT_CLASS(parent_class)->destroy)
        GTK_OBJECT_CLASS(parent_class)->destroy (object);
}

gint
gnc_query_list_get_needed_height (GNCQueryList *list, gint num_rows)
{
    GtkCList *clist;
    gint list_height;
    gint title_height;

    g_return_val_if_fail (list != NULL, 0);
    g_return_val_if_fail (IS_GNC_QUERY_LIST(list), 0);

    if (!GTK_WIDGET_REALIZED (list))
        return 0;

    clist = GTK_CLIST (list);

    /* sync with gtkclist.c */
    title_height = (clist->column_title_area.height +
                    (GTK_WIDGET(list)->style->ythickness +
                     GTK_CONTAINER(list)->border_width) * 2);
    list_height = (clist->row_height * num_rows) + (num_rows + 1);

    return title_height + list_height;
}

gint
gnc_query_list_get_num_entries (GNCQueryList *list)
{
    g_return_val_if_fail (list != NULL, 0);
    g_return_val_if_fail (IS_GNC_QUERY_LIST(list), 0);

    return list->num_entries;
}

gpointer
gnc_query_list_get_current_entry (GNCQueryList *list)
{
    g_return_val_if_fail (list != NULL, NULL);
    g_return_val_if_fail (IS_GNC_QUERY_LIST(list), NULL);

    return list->current_entry;
}

/********************************************************************\
 * gnc_query_list_recompute_widths                              *
 *   Given a new widget width, recompute the widths of each column. *
 *   Give any excess allocation to the description field. This also *
 *   handles the case of allocating column widths when the list is  *
 *   first filled with data.                                        *
 *                                                                  *
 * Args: list - a GncQueryList widget                           *
 *       allocated - the allocated width for this list              *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_query_list_recompute_widths (GNCQueryList *list, gint allocated)
{
    GtkCList *clist = GTK_CLIST(list);
    gint total_width, desc_width = 0, excess, i;

    /* Prevent loops when allocation is bigger than total widths */
    if (allocated == list->prev_allocation)
        return;

    /* Enforce column minimum widths */
    total_width = 0;
    for (i = 0; i < list->num_columns; i++)
    {
        gint width;

        width = gtk_clist_optimal_column_width(clist, i);
        if (width < list->title_widths[i])
            width = list->title_widths[i];
        total_width += width;
        gtk_clist_set_column_width (clist, i, width);
        if (i == 2)
            desc_width = width;
    }

    /* Did the list use its full allocation?
     *
     * Add/subtract any underage/overage to/from the description column
     */
    if (allocated <= 1)
        allocated = list->prev_allocation;
    list->prev_allocation = allocated;
    excess = allocated - total_width - VSCROLLBAR_SLOP;

    /* XXX: Choose a generic column to resize */
    gtk_clist_set_column_width (clist, 2, desc_width + excess);
}

/********************************************************************\
 * gnc_query_list_size_allocate_cb                              *
 *   The allocated size has changed. Need to recompute the          *
 *   column widths                                                  *
 *                                                                  *
 * Args: w - a GncQueryList widget                              *
 *       allocation - a widget allocation desctiption               *
 *       data - unused                                              *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_query_list_size_allocate_cb (GtkWidget *w,
                                 GtkAllocation *allocation,
                                 gpointer data)
{
    GNCQueryList *list = GNC_QUERY_LIST(w);

    g_return_if_fail (list != NULL);
    gnc_query_list_recompute_widths(list, allocation->width);
}

/********************************************************************\
 * gnc_query_list_refresh                                       *
 *   refreshes the list                                             *
 *                                                                  *
 * Args: list - list to refresh                                     *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_query_list_refresh (GNCQueryList *list)
{
    GtkCList *clist = GTK_CLIST(list);
    GtkAdjustment *adjustment;
    gfloat save_value = 0.0;
    gpointer *old_focus_entry;
    gpointer *old_entry;
    gint old_focus_row;
    gint new_row;

    g_return_if_fail (list != NULL);
    g_return_if_fail (IS_GNC_QUERY_LIST(list));

    adjustment = gtk_clist_get_vadjustment (GTK_CLIST(list));
    if (adjustment != NULL)
        save_value = adjustment->value;

    old_focus_row = clist->focus_row;
    old_focus_entry = gtk_clist_get_row_data (clist, old_focus_row);

    gtk_clist_freeze (clist);
    gtk_clist_clear (clist);

    old_entry = list->current_entry;
    list->num_entries = 0;
    list->current_row = -1;
    list->current_entry = NULL;

    gnc_query_list_fill (list);

    gnc_query_list_recompute_widths (list, -1);

    if (adjustment)
    {
        save_value = CLAMP (save_value, adjustment->lower, adjustment->upper);
        gtk_adjustment_set_value (adjustment, save_value);
    }

    if (old_entry)
    {
        new_row = gtk_clist_find_row_from_data (clist, old_entry);
        if (new_row >= 0)
        {
            list->no_toggle = TRUE;
            gtk_clist_select_row (clist, new_row, 0);
            list->no_toggle = FALSE;
            list->current_entry = old_entry;
        }
    }

    if (old_focus_entry)
    {
        new_row = gtk_clist_find_row_from_data (clist, old_focus_entry);

        if (new_row < 0)
            new_row = old_focus_row;

        if (new_row >= 0)
            clist->focus_row = new_row;
    }

    gtk_clist_thaw (clist);
}

/********************************************************************\
 * gnc_query_list_set_query_sort                                    *
 *   sets the sorting order of entries in the list                  *
 *                                                                  *
 * Args: list       - list to change the sort order for             *
 *	 new_column - is this a new column (so should we set the    *
 *                    query sort order or just set the 'increasing' *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_query_list_set_query_sort (GNCQueryList *list, gboolean new_column)
{
    gboolean sort_order = list->increasing;
    GList *node;
    GNCSearchParam *param;

    /* Find the column parameter definition */
    node = g_list_nth(list->column_params, list->sort_column);
    param = node->data;

    /* If we're asked to invert numerics, and if this is a numeric or
     * debred column, then invert the sort order.
     */
    if (list->numeric_inv_sort)
    {
        const char *type = gnc_search_param_get_param_type (param);
        if (!safe_strcmp(type, QUERYCORE_NUMERIC) ||
                !safe_strcmp(type, QUERYCORE_DEBCRED))
            sort_order = !sort_order;
    }

    /* Set the sort order for the engine, if the key changed */
    if (new_column)
    {
        GSList *p1, *p2;

        p1 = gnc_search_param_get_param_path(param);
        p2 = g_slist_prepend(NULL, QUERY_DEFAULT_SORT);
        gncQuerySetSortOrder (list->query, p1, p2, NULL);
    }

    xaccQuerySetSortIncreasing (list->query,
                                sort_order,
                                sort_order,
                                sort_order);

    /*
     * Recompute the list. Is this really necessary? Why not just sort
     * the rows already in the clist?  Answer: it would be an n-squared
     * algorithm to get the clist to match the resulting list.
     */
    gnc_query_list_refresh(list);
}

/********************************************************************\
 * gnc_query_list_set_sort_column                                   *
 *   sets the sorting order of entries in the list                  *
 *                                                                  *
 * Args: list   - list to change the sort order for                 *
 *	 column - the column to sort on                             *
 * Returns: nothing                                                 *
\********************************************************************/
static void
gnc_query_list_set_sort_column (GNCQueryList *list, gint sort_column)
{
    gint column;
    gboolean new_column = FALSE;

    g_return_if_fail (list != NULL);
    g_return_if_fail (IS_GNC_QUERY_LIST(list));
    g_return_if_fail (list->query != NULL);

    /* Clear all arrows */
    for (column = 0; column < list->num_columns; column++)
    {
        if (list->title_arrows[column])
            gtk_widget_hide(list->title_arrows[column]);
    }

    /* Is this a new column or a re-click on the existing column? */
    column = sort_column;
    new_column = (list->sort_column != sort_column);

    list->increasing = new_column ? TRUE : !list->increasing;
    list->sort_column = sort_column;

    /* Set the appropriate arrow */
    gtk_arrow_set(GTK_ARROW(list->title_arrows[column]),
                  list->increasing ? GTK_ARROW_DOWN : GTK_ARROW_UP,
                  GTK_SHADOW_ETCHED_IN);
    gtk_widget_show(list->title_arrows[column]);

    gnc_query_list_set_query_sort (list, new_column);
}

static void
gnc_query_list_click_column_cb(GtkWidget *w, gint column, gpointer data)
{
    GNCQueryList *list = GNC_QUERY_LIST(w);
    gnc_query_list_set_sort_column(list, column);
}

static void
gnc_query_list_fill(GNCQueryList *list)
{
    GNCQueryListPriv *priv;
    gchar *strings[list->num_columns + 1];
    GList *entries, *item;
    const GUID *guid;
    gint i;

    /* Clear all watches */
    priv = GNC_QUERY_LIST_GET_PRIVATE(list);
    gnc_gui_component_clear_watches (priv->component_id);

    /* Reverse the list now because 'append()' takes too long */
    entries = gncQueryRun(list->query);

    for (item = entries; item; item = item->next)
    {
        GList *node;
        gint row;
        const QofParam *gup;
        QofParam *qp = NULL;

        for (i = 0, node = list->column_params; node; node = node->next)
        {
            GNCSearchParam *param = node->data;
            GSList *converters = gnc_search_param_get_converters (param);
            const char *type = gnc_search_param_get_param_type (param);
            gpointer res = item->data;

            /* if this is a boolean, ignore it now -- we'll use a checkmark later */
            if (!safe_strcmp (type, QUERYCORE_BOOLEAN))
            {
                strings[i++] = g_strdup("");
                continue;
            }

            /* Do all the object conversions */
            for (; converters; converters = converters->next)
            {
                qp = converters->data;
                if (converters->next)
                {
                    res = (qp->param_getfcn)(res, qp);
                }
            }

            /* Now convert this to a text value for the row */
            if (!safe_strcmp(type, QUERYCORE_DEBCRED) ||
                    !safe_strcmp(type, QUERYCORE_NUMERIC))
            {
                gnc_numeric (*nfcn)(gpointer, QofParam *) =
                    (gnc_numeric(*)(gpointer, QofParam *))(qp->param_getfcn);
                gnc_numeric value = nfcn(res, qp);
                if (list->numeric_abs)
                    value = gnc_numeric_abs (value);
                strings[i++] = g_strdup(xaccPrintAmount(value, gnc_default_print_info(FALSE)));
            }
            else
                strings[i++] = gncQueryCoreToString (type, res, qp);
        }

        row = gtk_clist_append (GTK_CLIST(list), (gchar **) strings);
        gtk_clist_set_row_data (GTK_CLIST(list), row, item->data);

        /* Free up our strings */
        for (i = 0; i < list->num_columns; i++)
        {
            if (strings[i])
                g_free (strings[i]);
        }

        /* Now update any checkmarks */
        update_booleans (list, row);

        /* and set a watcher on this item */
        gup = priv->get_guid;
        guid = (const GUID*)((gup->param_getfcn)(item->data, gup));
        gnc_gui_component_watch_entity (priv->component_id, guid,
                                        QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

        list->num_entries++;
    }
}

/********************************************************************\
 * gnc_query_list_unselect_all                                      *
 *   unselect all items in the list                                 *
 *                                                                  *
 * Args: list - list to unselect all                                *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_query_list_unselect_all(GNCQueryList *list)
{
    g_return_if_fail (list != NULL);
    g_return_if_fail (IS_GNC_QUERY_LIST(list));

    list->no_toggle = TRUE;
    list->always_unselect = TRUE;

    gtk_clist_unselect_all (GTK_CLIST(list));

    list->always_unselect = FALSE;
    list->no_toggle = FALSE;

    list->current_entry = NULL;
}


gboolean gnc_query_list_item_in_list (GNCQueryList *list, gpointer item)
{
    g_return_val_if_fail(list, FALSE);
    g_return_val_if_fail(item, FALSE);
    g_return_val_if_fail(IS_GNC_QUERY_LIST(list), FALSE);

    return (gtk_clist_find_row_from_data(GTK_CLIST(list), item) != -1);
}

void gnc_query_list_refresh_item (GNCQueryList *list, gpointer item)
{
    gint row;

    g_return_if_fail(list);
    g_return_if_fail(item);
    g_return_if_fail(IS_GNC_QUERY_LIST(list));

    row = gtk_clist_find_row_from_data(GTK_CLIST(list), item);
    if (row != -1)
        update_booleans (list, row);
}

void
gnc_query_list_set_numerics (GNCQueryList *list, gboolean abs, gboolean inv_sort)
{
    g_return_if_fail(list);
    g_return_if_fail(IS_GNC_QUERY_LIST(list));

    list->numeric_abs = abs;
    list->numeric_inv_sort = inv_sort;
}
