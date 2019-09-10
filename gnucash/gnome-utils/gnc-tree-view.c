/*
 * gnc-tree-view.c -- new GtkTreeView with extra features used by
 *                    all the tree views in gnucash
 *
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
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

/** @addtogroup GUI
    @{ */
/** @addtogroup GncTreeView
 * @{ */
/** @file gnc-tree-view.c
    @brief Base GncTreeView implementation for gnucash trees.
    @author David Hampton <hampton@employees.org>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>

#include "gnc-tree-view.h"
#include "gnc-engine.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-cell-renderer-date.h"
#include "gnc-state.h"
#include "gnc-prefs.h"
#include "dialog-utils.h"

/* The actual state key for a particular column visibility.  This is
 * attached to the menu items that are in the column selection menu.
 * Makes it very easy to update saved state when a menu item is toggled. */
#define STATE_KEY  "state-key"

/* State keys within this particular saved state section. */
#define STATE_KEY_SORT_COLUMN  "sort_column"
#define STATE_KEY_SORT_ORDER   "sort_order"
#define STATE_KEY_COLUMN_ORDER "column_order"

/* Partial state keys within this particular saved state section. These
   are appended to the various column names to create the actual
   keys. */
#define STATE_KEY_SUFF_VISIBLE "visible"
#define STATE_KEY_SUFF_WIDTH   "width"

enum
{
    PROP_0,
    PROP_STATE_SECTION,
    PROP_SHOW_COLUMN_MENU,
};

/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/**** Declarations ******************************************************/
static void gnc_tree_view_class_init (GncTreeViewClass *klass);
static void gnc_tree_view_init (GncTreeView *view, void *data);
static void gnc_tree_view_finalize (GObject *object);
static void gnc_tree_view_destroy (GtkWidget *widget);
static void gnc_tree_view_set_property (GObject         *object,
                                        guint            prop_id,
                                        const GValue    *value,
                                        GParamSpec      *pspec);
static void gnc_tree_view_get_property (GObject         *object,
                                        guint            prop_id,
                                        GValue          *value,
                                        GParamSpec      *pspec);
static gboolean gnc_tree_view_drop_ok_cb (GtkTreeView *view,
        GtkTreeViewColumn *column,
        GtkTreeViewColumn *prev_column,
        GtkTreeViewColumn *next_column,
        gpointer data);
static void gnc_tree_view_build_column_menu (GncTreeView *view);
static void gnc_tree_view_select_column_cb (GtkTreeViewColumn *column,
        GncTreeView *view);
static gchar *gnc_tree_view_get_sort_order (GncTreeView *view);
static gchar *gnc_tree_view_get_sort_column (GncTreeView *view);
static gchar **gnc_tree_view_get_column_order (GncTreeView *view,
        gsize *length);

/** Private Data Structure ***********************************************/

typedef struct GncTreeViewPrivate
{
    /* Column selection menu related values */
    GtkTreeViewColumn *column_menu_column;
    GtkWidget         *column_menu;
    gboolean           show_column_menu;
    GtkWidget         *column_menu_icon_box;

    /* Sort callback model */
    GtkTreeModel      *sort_model;

    /* Editing callback functions */
    GFunc editing_started_cb;
    GFunc editing_finished_cb;
    gpointer editing_cb_data;

    /* State related values */
    gchar             *state_section;
    gboolean           seen_state_visibility;
    gulong             columns_changed_cb_id;
    gulong             sort_column_changed_cb_id;
    gulong             size_allocate_cb_id;
} GncTreeViewPrivate;

GNC_DEFINE_TYPE_WITH_CODE(GncTreeView, gnc_tree_view, GTK_TYPE_TREE_VIEW,
                          G_ADD_PRIVATE(GncTreeView))

#define GNC_TREE_VIEW_GET_PRIVATE(o)  \
   ((GncTreeViewPrivate*)g_type_instance_get_private((GTypeInstance*)o, GNC_TYPE_TREE_VIEW))


/************************************************************/
/*               g_object required functions                */
/************************************************************/

/** @name Gnc Tree View Object Implementation
 @{ */

static GObjectClass *parent_class = NULL;

/** Initialize the class for the new base gnucash tree view.  This
 *  will set up any function pointers that override functions in the
 *  parent class, and also installs the properties that are unique to
 *  this class.
 *
 *  @param klass The new class structure created by the object system.
 *
 *  @internal
 */
static void
gnc_tree_view_class_init (GncTreeViewClass *klass)
{
    GObjectClass *gobject_class;
    GtkWidgetClass *gtkwidget_class;

    parent_class = g_type_class_peek_parent (klass);

    gobject_class = G_OBJECT_CLASS(klass);
    gtkwidget_class = GTK_WIDGET_CLASS(klass);

    gobject_class->set_property = gnc_tree_view_set_property;
    gobject_class->get_property = gnc_tree_view_get_property;

    g_object_class_install_property (gobject_class,
                                     PROP_STATE_SECTION,
                                     g_param_spec_string ("state-section",
                                             "State Section",
                                             "The section name in the saved state to use for (re)storing the treeview's visual state (visible columns, sort order,...",
                                             NULL,
                                             G_PARAM_READWRITE));
    g_object_class_install_property (gobject_class,
                                     PROP_SHOW_COLUMN_MENU,
                                     g_param_spec_boolean ("show-column-menu",
                                             "Show Column Menu",
                                             "Show the column menu so user can change what columns are visible.",
                                             FALSE,
                                             G_PARAM_READWRITE));

    /* GObject signals */
    gobject_class->finalize = gnc_tree_view_finalize;

    /* GtkWidget signals */
    gtkwidget_class->destroy = gnc_tree_view_destroy;
}

static void
gnc_tree_view_update_grid_lines (gpointer prefs, gchar* pref, gpointer user_data)
{
    GncTreeView *view = user_data;
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), gnc_tree_view_get_grid_lines_pref ());
}

static gboolean
gnc_tree_view_select_column_icon_cb (GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    GncTreeView *view = user_data;
    GncTreeViewPrivate *priv;
    GtkStyleContext *stylectxt = gtk_widget_get_style_context (widget);
    GtkBorder padding;

    // if the event button is not the right one, leave.
    if (event->button != 1)
        return FALSE;

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    gtk_style_context_get_padding (stylectxt, GTK_STATE_FLAG_NORMAL, &padding);

    if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL)
    {
        if (event->x < (gtk_widget_get_allocated_width (priv->column_menu_icon_box) + padding.left))
            gnc_tree_view_select_column_cb (priv->column_menu_column, view);
    }
    else
    {
        if (event->x > (gtk_widget_get_allocated_width (widget) -
                       (gtk_widget_get_allocated_width (priv->column_menu_icon_box) + padding.right)))
            gnc_tree_view_select_column_cb (priv->column_menu_column, view);
    }
    return FALSE;
}

/** Initialize a new instance of a base gnucash tree view.  This
 *  function allocates and initializes the object private storage
 *  space.  It also adds the new object to a list (for memory tracking
 *  purposes), and sets up a callback for the column drag function.
 *
 *  @param view The new object instance created by the object system.
 *
 *  @internal
 */
static void
gnc_tree_view_init (GncTreeView *view, void *data)
{
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column;
    GtkWidget *sep, *icon;

    GncTreeViewClass *klass = (GncTreeViewClass*)data;

    gnc_gobject_tracking_remember (G_OBJECT(view),
                                   G_OBJECT_CLASS(klass));

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    priv->column_menu = NULL;
    priv->show_column_menu = FALSE;
    priv->sort_model = NULL;
    priv->state_section = NULL;
    priv->seen_state_visibility = FALSE;
    priv->columns_changed_cb_id = 0;
    priv->sort_column_changed_cb_id = 0;
    priv->size_allocate_cb_id = 0;

    // Set the style context for this page so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(view), "GncTreeView");

    /* Handle column drag and drop */
    gtk_tree_view_set_column_drag_function (GTK_TREE_VIEW(view),
                                            gnc_tree_view_drop_ok_cb, NULL, NULL);

    // Set grid lines option to preference
    gtk_tree_view_set_grid_lines (GTK_TREE_VIEW(view), gnc_tree_view_get_grid_lines_pref ());
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL,
                           gnc_tree_view_update_grid_lines, view);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL,
                           gnc_tree_view_update_grid_lines, view);

    /* Create the last column which contains the column selection
     * widget.  gnc_tree_view_add_text_column will do most of the
     * work. */
    icon = gtk_image_new_from_icon_name ("go-down", GTK_ICON_SIZE_SMALL_TOOLBAR);

    priv->column_menu_icon_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX(priv->column_menu_icon_box), FALSE);

#if GTK_CHECK_VERSION(3,12,0)
    gtk_widget_set_margin_start (GTK_WIDGET(icon), 5);
#else
    gtk_widget_set_margin_left (GTK_WIDGET(icon), 5);
#endif
    gtk_box_pack_end (GTK_BOX(priv->column_menu_icon_box), icon, FALSE, FALSE, 0);

    sep = gtk_separator_new (GTK_ORIENTATION_VERTICAL);
    gtk_box_pack_end (GTK_BOX(priv->column_menu_icon_box), sep, FALSE, FALSE, 0);

    gtk_widget_show_all (priv->column_menu_icon_box);

    g_signal_connect (G_OBJECT(icon), "draw",
                      G_CALLBACK(gnc_draw_arrow_cb), GINT_TO_POINTER(1));

    column = gnc_tree_view_add_text_column (view, NULL, NULL, NULL, NULL,
                                            -1, -1, NULL);
    g_object_set (G_OBJECT(column),
                  "clickable", TRUE,
                  "widget", priv->column_menu_icon_box,
                  "alignment", 1.0,
                  "expand", TRUE,
                  (gchar *)NULL);

    priv->column_menu_column = column;

    // get the actual column button by looking at the parents of the column_menu_icon
    {
        GtkWidget *mybox = gtk_widget_get_parent (icon);
        GtkWidget *walign = gtk_widget_get_parent (mybox);
        GtkWidget *box = gtk_widget_get_parent (walign);
        GtkWidget *button = gtk_widget_get_parent (box);

        if (!GTK_IS_BUTTON(button)) // just in case this order changes.
        {
            // this will fire for the whole column header
            g_signal_connect (G_OBJECT(column), "clicked",
                              G_CALLBACK(gnc_tree_view_select_column_cb),
                              view);
        }
        else
        {
            /* this part will restrict the mouse click to just where the
               icon is, tried using an eventbox but it would only work
               some of the time */
            gtk_widget_set_events (button, GDK_BUTTON_PRESS_MASK);

            g_signal_connect (G_OBJECT(button), "button_press_event",
                              G_CALLBACK(gnc_tree_view_select_column_icon_cb),
                              view);
        }
    }
    gtk_tree_view_column_set_sizing (column, GTK_TREE_VIEW_COLUMN_FIXED);
}

/** Finalize the GncTreeView object.  This function is called from the
 *  G_Object level to complete the destruction of the object.  It
 *  should release any memory not previously released by the destroy
 *  function (i.e. the private data structure), then chain up to the
 *  parent's destroy function.
 *
 *  @param object The object being destroyed.
 *
 *  @internal
 */
static void
gnc_tree_view_finalize (GObject *object)
{
    ENTER("view %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_VIEW(object));

    gnc_gobject_tracking_forget (object);

    if (G_OBJECT_CLASS(parent_class)->finalize)
        G_OBJECT_CLASS(parent_class)->finalize (object);
    LEAVE(" ");
}

/** Destroy the GncTreeView widget.  This function is called (possibly
 *  multiple times) from the Gtk_Object level to destroy the widget.
 *  It should release any memory owned by the widget that isn't
 *  fundamental to the implementation.  In this function any active
 *  callbacks are disconnected, all memory other than the private data
 *  structure are freed.
 *
 *  @param widget The widget being destroyed.
 *
 *  @internal
 */
static void
gnc_tree_view_destroy (GtkWidget *widget)
{
    GncTreeView *view;
    GncTreeViewPrivate *priv;

    ENTER("view %p", widget);
    g_return_if_fail (widget != NULL);
    g_return_if_fail (GNC_IS_TREE_VIEW(widget));

    view = GNC_TREE_VIEW(widget);

    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_HORIZONTAL,
                                 gnc_tree_view_update_grid_lines, view);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL, GNC_PREF_GRID_LINES_VERTICAL,
                                 gnc_tree_view_update_grid_lines, view);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    if (priv->state_section)
    {
        gnc_tree_view_save_state (view);
    }
    g_free (priv->state_section);
    priv->state_section = NULL;

    if (priv->column_menu)
    {
        DEBUG("removing column selection menu");
        g_object_unref (priv->column_menu);
        priv->column_menu = NULL;
    }

    if (GTK_WIDGET_CLASS(parent_class)->destroy)
        GTK_WIDGET_CLASS(parent_class)->destroy (widget);
    LEAVE(" ");
}

/** @} */

/************************************************************/
/*                g_object other functions                  */
/************************************************************/

/** @name Gnc Tree View Object Implementation
 @{ */

/** Retrieve a property specific to this GncTreeView object.  This is
 *  nothing more than a dispatch function for routines that can be
 *  called directly.  It has the nice feature of allowing a single
 *  function call to retrieve multiple properties.
 *
 *  @internal
 */
static void
gnc_tree_view_get_property (GObject     *object,
                            guint        prop_id,
                            GValue      *value,
                            GParamSpec  *pspec)
{
    GncTreeView *view = GNC_TREE_VIEW(object);
    GncTreeViewPrivate *priv;

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    switch (prop_id)
    {
    case PROP_STATE_SECTION:
        g_value_set_string (value, priv->state_section);
        break;
    case PROP_SHOW_COLUMN_MENU:
        g_value_set_boolean (value, priv->show_column_menu);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}


/** Set a property specific to this GncTreeView object.  This is
 *  nothing more than a dispatch function for routines that can be
 *  called directly.  It has the nice feature of allowing a new view
 *  to be created with a varargs list specifying the properties,
 *  instead of having to explicitly call each property function.
 *
 *  @internal
 */
static void
gnc_tree_view_set_property (GObject      *object,
                            guint         prop_id,
                            const GValue *value,
                            GParamSpec   *pspec)
{
    GncTreeView *view = GNC_TREE_VIEW(object);

    switch (prop_id)
    {
    case PROP_STATE_SECTION:
        gnc_tree_view_set_state_section (view, g_value_get_string (value));
        break;
    case PROP_SHOW_COLUMN_MENU:
        gnc_tree_view_set_show_column_menu (view, g_value_get_boolean (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

/** @} */

/************************************************************/
/*                   Auxiliary Functions                    */
/************************************************************/
/** @name Gnc Tree View Auxiliary Functions
 @{ */

/** Find a tree column given a column id number from the underlying
 *  data model.  This function should only be called by code that has
 *  visibility into the data model.  The column id numbers shouldn't
 *  be used for any other purpose.
 *
 *  This function simply runs the list of all (visible and invisible)
 *  columns looking for a match.  Column id numbers were attached to
 *  each column at the time the column was created.
 *
 *  @param view The visible tree widget.
 *
 *  @param wanted The column id number to find.
 *
 *  @internal
 */
static GtkTreeViewColumn *
view_column_find_by_model_id (GncTreeView *view,
                              const gint wanted)
{
    GtkTreeViewColumn *column, *found = NULL;
    GList *column_list, *tmp;
    gint id;

    // ENTER("view %p, name %s", view, name);
    column_list = gtk_tree_view_get_columns (GTK_TREE_VIEW(view));
    for (tmp = column_list; tmp; tmp = g_list_next (tmp))
    {
        column = tmp->data;
        id = GPOINTER_TO_INT(g_object_get_data (G_OBJECT(column), MODEL_COLUMN));
        if (id != wanted)
            continue;
        found = column;
        break;
    }
    g_list_free (column_list);

    // LEAVE("column %p", found);
    return found;
}

/** Find a tree column given the "pref name" used with saved state.  This
 *  function simply runs the list of all (visible and invisible)
 *  columns looking for a match.  Column names were attached to each
 *  column at the time the column was created.
 *
 *  @param view The visible tree widget.
 *
 *  @param wanted The "pref name" to find.
 *
 */
GtkTreeViewColumn *
gnc_tree_view_find_column_by_name (GncTreeView *view,
                                   const gchar *wanted)
{
    GtkTreeViewColumn *column, *found = NULL;
    GList *column_list, *tmp;
    const gchar *name;

    // ENTER("view %p, wanted %s", view, wanted);
    column_list = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
    for (tmp = column_list; tmp; tmp = g_list_next (tmp))
    {
        column = tmp->data;
        name = g_object_get_data (G_OBJECT(column), PREF_NAME);
        if (!name || (strcmp(name, wanted) != 0))
            continue;
        found = column;
        break;
    }
    g_list_free (column_list);

    // LEAVE("column %p", found);
    return found;
}

/** @} */

/************************************************************/
/*                     Tree Callbacks                       */
/************************************************************/

/** @name Gnc Tree View Callback Functions
 @{ */

/** This function is called to determine whether it is acceptable to
 *  drop a dragged tree column at a given location.  This function
 *  will be called multiple times by the GtkTreeView code while the
 *  user is dragging a column.  Each call is a check to see if the
 *  proposed location is an acceptable place to drop the column.  In
 *  the case of a GncTreeView, the only drop locations that are
 *  prohibited are on the right side of the visible tree.
 *
 *  @param view The visible tree widget.
 *
 *  @param column The column being dragged.
 *
 *  @param prev_column The column before (left) of the proposed drop
 *  location.
 *
 *  @param next_column The column after (right) of the proposed drop
 *  location.
 *
 *  @param data Unused.
 *
 *  @return TRUE if this drop location is acceptable. FALSE if not
 *  acceptable.
 *
 *  @internal
 */
static gboolean
gnc_tree_view_drop_ok_cb (GtkTreeView *view,
                          GtkTreeViewColumn *column,
                          GtkTreeViewColumn *prev_column,
                          GtkTreeViewColumn *next_column,
                          gpointer data)
{
    const gchar *pref_name;

    /* Should we allow a drop at the left side of the tree view before
     * the widget to open a new display level?  I can think of cases
     * where the user might want to do this with a checkbox column. */
    if (prev_column == NULL)
        return TRUE;

    /* Do not allow a drop at the right side of the tree view after the
     * column selection widget.  */
    if (next_column == NULL)
        return FALSE;

    /* Columns without pref names are considered fixed at the right hand
     * side of the view.  At the time of this writing, the only two are
     * the column where the "column selection widget" is stored, and the
     * "padding" column to the left of that where extra view space ends
     * up. */
    pref_name = g_object_get_data (G_OBJECT(prev_column), PREF_NAME);
    if (!pref_name)
        return FALSE;

    /* Everything else is allowed. */
    return TRUE;
}

/** @} */

/************************************************************/
/*                  State Setup / Callbacks                 */
/************************************************************/

/** @name Gnc Tree View state Callback / Related Functions
 @{ */

/** Determine the visibility of a column.  This function first looks
 *  for columns specially marked to be always visible, or columns
 *  without a preference name.  These are always shown.  Next, this
 *  function checks if visibility is stored in saved state. If so
 *  use the value found there. If none of the above the default
 *  visibility for the column is used.
 *
 *  @param view A GncTreeView.
 *
 *  @param column The GtkTreeViewColumn in question.  Either this
 *  value or the pref_name parameter must be non-NULL.
 *
 *  @param pref_name The name of the column in question.  Either this
 *  value or the column parameter must be non-NULL.
 *
 *  @return TRUE if the column should be visible.  FALSE otherwise.
 *
 *  @internal
 */
static gboolean
gnc_tree_view_column_visible (GncTreeView *view,
                              GtkTreeViewColumn *column,
                              const gchar *pref_name)
{
    GncTreeViewPrivate *priv;
    gboolean visible;
    const gchar *col_name = pref_name;

    ENTER("column %p, name %s", column, pref_name ? pref_name : "(null)");
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (column)
    {
        if (g_object_get_data (G_OBJECT(column), ALWAYS_VISIBLE))
        {
            LEAVE("1, first column");
            return TRUE;
        }
        col_name = g_object_get_data (G_OBJECT(column), PREF_NAME);
        DEBUG("col_name is %s", col_name ? col_name : "(null)");
    }

    if (!col_name)
    {
        LEAVE("1, no pref name");
        return TRUE;
    }

    /* Using saved state ? */
    if (priv->state_section)
    {
        GKeyFile *state_file = gnc_state_get_current ();
        gchar *key = g_strdup_printf ("%s_%s", col_name, STATE_KEY_SUFF_VISIBLE);

        if (g_key_file_has_key (state_file, priv->state_section, key, NULL))
        {
            visible = g_key_file_get_boolean (state_file, priv->state_section, key, NULL);
            g_free (key);
            LEAVE("%d, state defined visibility", visible);
            return visible;
        }
    }

    /* Check the default columns list */
    visible = column ?
              (g_object_get_data (G_OBJECT(column), DEFAULT_VISIBLE) != NULL) : FALSE;
    LEAVE("defaults says %d", visible);
    return visible;
}

/** This function updates the visibility of a single column.  It
 *  checks if the column should be visible, and if so tells the view
 *  to show the column.
 *
 *  @param column The column whose visibility should be updated.
 *
 *  @param view The GncTreeView containing the column.
 *
 *  @internal
 */
static void
gnc_tree_view_update_visibility (GtkTreeViewColumn *column,
                                 GncTreeView *view)
{
    gboolean visible;

    g_return_if_fail (GTK_IS_TREE_VIEW_COLUMN(column));
    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    ENTER(" ");
    visible = gnc_tree_view_column_visible (view, column, NULL);
    gtk_tree_view_column_set_visible (column, visible);
    LEAVE("made %s", visible ? "visible" : "invisible");
}

/** Get the sort order for the sort column, converted into a string.
 *
 *  @param view The tree view.
 *
 *  @return a string representing the sort order. NULL if not sorted, else
 *          either "ascending" or "descending".
 *          Should be freed with g_free if no longer needed.
 *
 *  @internal
 */
static gchar *
gnc_tree_view_get_sort_order (GncTreeView *view)
{
    GtkTreeModel *s_model;
    GtkSortType order;
    gint current;
    gchar *order_str = NULL;

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    if (!s_model)
        return NULL; /* no model, so sort order doesn't make sense */

    if (!gtk_tree_sortable_get_sort_column_id (GTK_TREE_SORTABLE(s_model),
            &current, &order))
        return NULL; /* Model is not sorted, return */

    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(s_model),
                                         current, order);
    order_str = g_strdup (order == GTK_SORT_ASCENDING ? "ascending" : "descending");
    DEBUG("current sort_order is %s", order_str);
    return order_str;
}

/** Get the current sort column.
 *
 *  @param view The tree view.
 *
 *  @return a string with the name of the sort column, or NULL if not sorted.
 *          Should be freed with g_free if no longer needed.
 *
 *  @internal
 */
static gchar *
gnc_tree_view_get_sort_column (GncTreeView *view)
{
    GtkTreeModel *s_model;
    GtkTreeViewColumn *column;
    GtkSortType order;
    gint current;
    const gchar *name;

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    if (!s_model)
        return NULL; /* no model -> no sort column */

    if (!gtk_tree_sortable_get_sort_column_id (GTK_TREE_SORTABLE(s_model),
            &current, &order))
        return NULL; /* model not sorted */

    column = view_column_find_by_model_id (view, current);
    if (!column)
        return NULL; /* column not visible, can't be used for sorting */

    name = g_object_get_data (G_OBJECT(column), PREF_NAME);
    DEBUG("current sort column is %s", name ? name : "(NULL)");
    return g_strdup (name);
}



/** Get the current column order.
 *
 *  @param view The tree view.
 *
 *  @return an array of strings with the names of the columns in the order
 *          they are currently ordered.
 *          Should be freed with g_free if no longer needed.
 *
 *  @internal
 */
static gchar **
gnc_tree_view_get_column_order (GncTreeView *view,
                                gsize *length)
{
    const GList *tmp;
    GList *columns;
    gulong num_cols = 0;
    gchar *col_names = NULL;
    gchar **col_str_list;

    /* First, convert from names to pointers */
    ENTER(" ");

    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW(view));
    for (tmp = columns; tmp; tmp = g_list_next(tmp))
    {
        GtkTreeViewColumn *column = tmp->data;
        const gchar *name = g_object_get_data (G_OBJECT(column), PREF_NAME);
        if (!col_names)
            col_names = g_strdup (name);
        else
        {
            gchar *col_names_prev = col_names;
            col_names = g_strjoin (";", col_names_prev, name, NULL);
            g_free (col_names_prev);
        }
        num_cols++;
    }
    //DEBUG ("got %lu columns: %s", num_cols, col_names);
    col_str_list = g_strsplit (col_names, ";", 0);

    /* Clean up */
    g_list_free (columns);
    g_free (col_names);

    LEAVE("column order get");
    *length = num_cols;
    return col_str_list;
}

/** Set the sort order for the sort column (if there is one)
 *  of this tree view.
 *
 *  @param view The tree view.
 *
 *  @param name The sort order enum (in string form). Either
 *  "ascending" or "descending".
 *
 *  @internal
 */
static void
gnc_tree_view_set_sort_order (GncTreeView *view,
                              const gchar *name)
{
    GtkTreeModel *s_model;
    GtkSortType order = GTK_SORT_ASCENDING;
    gint current;

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    if (!s_model)
        return;
    if (g_strcmp0 (name, "descending") == 0)
        order = GTK_SORT_DESCENDING;
    if (!gtk_tree_sortable_get_sort_column_id (GTK_TREE_SORTABLE(s_model),
            &current, NULL))
        current = GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID;
    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(s_model),
                                          current, order);
    DEBUG("sort_order set to %s", order == GTK_SORT_ASCENDING ? "ascending" : "descending");
}

/** Set the sort column for this tree view.
 *
 *  @param view The tree view.
 *
 *  @param name The name of the column that should be made the sort column.
 *
 *  @internal
 */
static void
gnc_tree_view_set_sort_column (GncTreeView *view,
                               const gchar *name)
{
    GtkTreeModel *s_model;
    GtkTreeViewColumn *column;
    GtkSortType order;
    gint model_column, current;

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    if (!s_model)
        return;

    column = gnc_tree_view_find_column_by_name (view, name);
    if (!column)
    {
        gtk_tree_sortable_set_sort_column_id (
            GTK_TREE_SORTABLE(s_model), GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID,
            GTK_SORT_ASCENDING);
        return;
    }

    model_column =
        GPOINTER_TO_INT(g_object_get_data (G_OBJECT(column), MODEL_COLUMN));
    if (model_column == GNC_TREE_VIEW_COLUMN_DATA_NONE)
        return;

    if (!gtk_tree_sortable_get_sort_column_id (GTK_TREE_SORTABLE(s_model),
                                               &current, &order))
        order = GTK_SORT_ASCENDING;

    gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE(s_model),
                                          model_column, order);
    DEBUG("sort column set to %s", name);
}

/** Set the order of the columns (visible and invisible) for this
 *  tree view.
 *
 *  @param view The tree view.
 *
 *  @param column_names An array of strings.  These strings are the
 *  names of the columns in the order they should appear.
 *
 *  @param length The number of strings in the array.
 *
 *  @internal
 */
static void
gnc_tree_view_set_column_order (GncTreeView *view,
                                gchar **column_names,
                                gsize length)
{
    GtkTreeViewColumn *column, *prev;
    const GSList *tmp;
    GSList *columns;
    gsize idx;

    /* First, convert from names to pointers */
    ENTER(" ");
    columns = NULL;
    for (idx = 0; idx < length; idx++)
    {
        const gchar *name = column_names [idx];
        column = gnc_tree_view_find_column_by_name (view, name);
        if (!column)
            continue;
        columns = g_slist_append (columns, column);
    }

    /* Then reorder the columns */
    for (prev = NULL, tmp = columns; tmp; tmp = g_slist_next (tmp))
    {
        column = tmp->data;
        gtk_tree_view_move_column_after (GTK_TREE_VIEW(view), column, prev);
        prev = column;
    }

    /* Clean up */
    g_slist_free (columns);
    LEAVE("column order set");
}

/** Completely wipe the treeview's state information (column visibility, width,
 *  sorting order,..).  This function may be called at any time;
 *  either when the user wants to disconnect or
 *  when the view object is being destroyed.
 *
 *  @param view The tree view.
 */

void gnc_tree_view_remove_state_information (GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GKeyFile *state_file = gnc_state_get_current ();

    ENTER(" ");
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (!priv->state_section)
    {
        LEAVE("no state section");
        return;
    }

    g_key_file_remove_group (state_file, priv->state_section, NULL);
    g_free (priv->state_section);
    priv->state_section = NULL;
    LEAVE(" ");
}

/** Set up or remove an association between a saved state section
 *  and the display of a view.  It will first remove any existing association,
 *  and then install the new one. If the new section has state
 *  information, update the view with this information.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
void
gnc_tree_view_set_state_section (GncTreeView *view,
                                 const gchar *section)
{
    GncTreeViewPrivate *priv;
    GKeyFile *state_file;

    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    ENTER("view %p, section %s", view, section);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    /* Drop any previous state section */
    if (priv->state_section)
        gnc_tree_view_remove_state_information (view);

    if (!section)
    {
        LEAVE("cleared state section");
        return;
    }

    /* Catch changes in state. Propagate to view. */
    priv->state_section = g_strdup (section);

    state_file = gnc_state_get_current ();
    if (g_key_file_has_group (state_file, priv->state_section))
    {
        gsize num_keys, idx;
        gchar **keys = g_key_file_get_keys (state_file, priv->state_section, &num_keys, NULL);
        for (idx = 0; idx < num_keys; idx++)
        {
            gchar *key = keys[idx];
            if (g_strcmp0 (key, STATE_KEY_SORT_COLUMN) == 0)
            {
                gnc_tree_view_set_sort_column (view,
                                               g_key_file_get_string (state_file, priv->state_section, key, NULL));
            }
            else if (g_strcmp0 (key, STATE_KEY_SORT_ORDER) == 0)
            {
                gnc_tree_view_set_sort_order (view,
                                              g_key_file_get_string (state_file, priv->state_section, key, NULL));
            }
            else if (g_strcmp0 (key, STATE_KEY_COLUMN_ORDER) == 0)
            {
                gsize length;
                gchar **columns = g_key_file_get_string_list (state_file, priv->state_section,
                                                              key, &length, NULL);
                gnc_tree_view_set_column_order (view, columns, length);
                g_strfreev (columns);
            }
            else
            {
                /* Make a copy of the local part of the key so it can be split
                 * into column name and key type */
                gboolean known = FALSE;
                gchar *column_name = g_strdup (key);
                gchar *type_name = g_strrstr (column_name, "_");

                if (type_name != NULL) //guard against not finding '_'
                {
                    *type_name++ = '\0';

                    if (g_strcmp0 (type_name, STATE_KEY_SUFF_VISIBLE) == 0)
                    {
                        GtkTreeViewColumn *column = gnc_tree_view_find_column_by_name (view, column_name);
                        if (column)
                        {
                            known = TRUE;
                            if (!g_object_get_data (G_OBJECT (column), ALWAYS_VISIBLE))
                            {
                                gtk_tree_view_column_set_visible (column,
                                                                  g_key_file_get_boolean (state_file, priv->state_section, key, NULL));
                            }
                        }
                    }
                    else if (g_strcmp0 (type_name, STATE_KEY_SUFF_WIDTH) == 0)
                    {
                        gint width = g_key_file_get_integer (state_file, priv->state_section, key, NULL);
                        GtkTreeViewColumn *column = gnc_tree_view_find_column_by_name (view, column_name);
                        if (column)
                        {
                            known = TRUE;
                            if (width && (width != gtk_tree_view_column_get_width (column)))
                            {
                                gtk_tree_view_column_set_fixed_width (column, width);
                            }
                        }
                    }
                    if (!known)
                        DEBUG ("Ignored key %s", key);

                    g_free (column_name);
                }
            }
        }
        g_strfreev (keys);
    }

    /* Rebuild the column visibility menu */
    gnc_tree_view_build_column_menu (view);
    LEAVE ("set state section");
}

/** Get the name of the state section this tree view is associated with.
 *  It returns the same value passed to gnc_tree_view_set_state_section();
 *  i.e. a string like "dialogs/edit_prices".
 *
 *  Parameters are defined in gnc-tree-view.h
 */
const gchar *
gnc_tree_view_get_state_section (GncTreeView *view)
{
    GncTreeViewPrivate *priv;

    g_return_val_if_fail (GNC_IS_TREE_VIEW(view), NULL);

    priv = GNC_TREE_VIEW_GET_PRIVATE (view);
    return priv->state_section;
}

void gnc_tree_view_save_state (GncTreeView *view)
{
    GncTreeViewPrivate *priv;

    ENTER("view %p", view);
    g_return_if_fail (view != NULL);
    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    if (priv->state_section)
    {
        /* Save state. Only store non-default values when possible. */
        GList *column_list, *tmp;
        GKeyFile *state_file = gnc_state_get_current();
        gsize num_cols = 0;
        gchar *sort_column = gnc_tree_view_get_sort_column (view);
        gchar *sort_order = gnc_tree_view_get_sort_order (view);
        gchar **col_order = gnc_tree_view_get_column_order (view, &num_cols);

        /* Default sort column is the name column */
        if (sort_column && (g_strcmp0 (sort_column, "name") != 0))
            g_key_file_set_string (state_file, priv->state_section, STATE_KEY_SORT_COLUMN, sort_column);
        else if (g_key_file_has_key (state_file, priv->state_section, STATE_KEY_SORT_COLUMN, NULL))
            g_key_file_remove_key (state_file, priv->state_section, STATE_KEY_SORT_COLUMN, NULL);
        g_free (sort_column);


        /* Default sort order is "ascending" */
        if (g_strcmp0 (sort_order, "descending") == 0)
            g_key_file_set_string (state_file, priv->state_section, STATE_KEY_SORT_ORDER, sort_order);
        else if (g_key_file_has_key (state_file, priv->state_section, STATE_KEY_SORT_ORDER, NULL))
            g_key_file_remove_key (state_file, priv->state_section, STATE_KEY_SORT_ORDER, NULL);
        g_free (sort_order);

        if (col_order && (num_cols > 0))
            g_key_file_set_string_list (state_file, priv->state_section, STATE_KEY_COLUMN_ORDER,
                                        (const gchar**) col_order, num_cols);
        else if (g_key_file_has_key (state_file, priv->state_section, STATE_KEY_COLUMN_ORDER, NULL))
            g_key_file_remove_key (state_file, priv->state_section, STATE_KEY_COLUMN_ORDER, NULL);


        // ENTER("view %p, wanted %s", view, wanted);
        column_list = gtk_tree_view_get_columns (GTK_TREE_VIEW(view));
        for (tmp = column_list; tmp; tmp = g_list_next (tmp))
        {
            GtkTreeViewColumn *column = tmp->data;
            gchar *key=NULL;
            const gchar *name = g_object_get_data (G_OBJECT(column), PREF_NAME);
            if (!name)
                continue;

            if (!g_object_get_data (G_OBJECT(column), ALWAYS_VISIBLE))
            {
                key = g_strjoin ("_", name, STATE_KEY_SUFF_VISIBLE, NULL);
                g_key_file_set_boolean (state_file, priv->state_section, key,
                                        gtk_tree_view_column_get_visible (column));
                g_free (key);
            }

            key = g_strjoin ("_", name, STATE_KEY_SUFF_WIDTH, NULL);
            if (g_object_get_data (G_OBJECT(column), "default-width") &&
                (GPOINTER_TO_INT((g_object_get_data (G_OBJECT(column), "default-width")))
                    != gtk_tree_view_column_get_width (column)))
            {
                g_key_file_set_integer (state_file, priv->state_section, key,
                                        gtk_tree_view_column_get_width (column));
            }
            else if (g_key_file_has_key (state_file, priv->state_section, key, NULL))
                g_key_file_remove_key (state_file, priv->state_section, key, NULL);
            g_free (key);
        }
        g_list_free (column_list);
    }

    LEAVE(" ");
}


/** @} */

/************************************************************/
/*                   Column Selection Menu                  */
/************************************************************/

/** @name Gnc Tree View Column Selection Menu Related Functions
 @{ */

/** This function is called to create a single checkmenuitem in the
 *  column selection menu.  It is called once for each column in the
 *  view.  It creates a menu item for the corresponding column, and
 *  attaches to it a copy of the state key for this column's
 *  visibility.  This makes the toggle callback function trivial.
 *
 *  This function will create the column selection menu if one doesn't
 *  already exist.
 *
 *  @param column Create a menu item for this column.
 *
 *  @param view The tree view.
 *
 *  @internal
 */
static void
gnc_tree_view_create_menu_item (GtkTreeViewColumn *column,
                                GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GtkWidget *widget;
    const gchar *column_name, *pref_name;
    gchar *key;
    GBinding *binding;

    // ENTER("view %p, column %p", view, column);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (!priv->state_section)
    {
        // LEAVE("no state section");
        return;
    }

    pref_name = g_object_get_data (G_OBJECT(column), PREF_NAME);
    if (!pref_name)
    {
        // LEAVE("column has no pref_name");
        return;
    }

    /* Create the menu if we don't have one already */
    if (!priv->column_menu)
    {
        priv->column_menu = gtk_menu_new();
        g_object_ref_sink (priv->column_menu);
    }

    /* Create the check menu item */
    column_name = g_object_get_data (G_OBJECT(column), REAL_TITLE);
    if (!column_name)
        column_name = gtk_tree_view_column_get_title (column);
    widget = gtk_check_menu_item_new_with_label (column_name);
    gtk_menu_shell_append (GTK_MENU_SHELL(priv->column_menu), widget);

    /* Should never be able to hide the first column */
    if (g_object_get_data (G_OBJECT(column), ALWAYS_VISIBLE))
    {
        g_object_set_data (G_OBJECT(widget), ALWAYS_VISIBLE, GINT_TO_POINTER(1));
        gtk_widget_set_sensitive (widget, FALSE);
    }

    binding = g_object_bind_property (G_OBJECT(widget), "active", G_OBJECT(column), "visible", 0);
    g_object_set_data (G_OBJECT(widget), "column-binding", binding);

    /* Store data on the widget for callbacks */
    key = g_strdup_printf ("%s_%s", pref_name, STATE_KEY_SUFF_VISIBLE);
    g_object_set_data_full (G_OBJECT(widget), STATE_KEY, key, g_free);
    // LEAVE(" ");
}

/** This function is called to build the column selection menu.  It
 *  first destroys any old column selection menu, then checks to see
 *  if a new menu should be built.  If so, it calls the
 *  gnc_tree_view_create_menu_item() for each column in the view.
 *  This function is invoked when either the "state-section" or the
 *  "show-column-menu" property is changed on the view.
 *
 *  @param view Build a selection menu for this tree view.
 *
 *  @internal
 */
static void
gnc_tree_view_build_column_menu (GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GList *column_list;

    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    ENTER("view %p", view);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    /* Destroy any old menu */
    if (priv->column_menu)
    {
        g_object_unref (priv->column_menu);
        priv->column_menu = NULL;
    }

    if (priv->show_column_menu && priv->state_section)
    {
        /* Show the menu popup button */
        if (priv->column_menu_column)
            gtk_tree_view_column_set_visible (priv->column_menu_column, TRUE);

        /* Now build a new menu */
        column_list = gtk_tree_view_get_columns (GTK_TREE_VIEW(view));
        g_list_foreach (column_list, (GFunc)gnc_tree_view_create_menu_item, view);
        g_list_free (column_list);
    }
    else
    {
        /* Hide the menu popup button */
        if (priv->column_menu_column)
            gtk_tree_view_column_set_visible (priv->column_menu_column, FALSE);
    }
    LEAVE("menu: show %d, section %s", priv->show_column_menu,
          priv->state_section ? priv->state_section : "(null)");
}

/** This function is called to synchronize the checkbox on a menu item
 *  with the current visibility for the corresponding column.
 *
 *  @param checkmenuitem The menu item to update.
 *
 *  @param view The tree view.
 *
 *  @internal
 */
static void
gnc_tree_view_update_column_menu_item (GtkCheckMenuItem *checkmenuitem,
                                       GncTreeView *view)
{
    gboolean visible;

    g_return_if_fail (GTK_IS_CHECK_MENU_ITEM(checkmenuitem));
    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    if (g_object_get_data (G_OBJECT(checkmenuitem), ALWAYS_VISIBLE))
    {
        visible = TRUE;
    }
    else
    {
        GBinding *binding = g_object_get_data (G_OBJECT(checkmenuitem), "column-binding");
        GtkTreeViewColumn *column = GTK_TREE_VIEW_COLUMN(g_binding_get_target (binding));

        visible = gtk_tree_view_column_get_visible (column);
    }
    gtk_check_menu_item_set_active (checkmenuitem, visible);
}

/** This function when the user clicks on the button to show the
 *  column selection menu.  It first synchronize the checkboxes on all
 *  menu item with the state visibility values.  It then pops up the
 *  menu for the user to choose from.
 *
 *  @param column The tree column containing the column selection
 *  button.
 *
 *  @param view The tree view.
 *
 *  @internal
 */
static void
gnc_tree_view_select_column_cb (GtkTreeViewColumn *column,
                                GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GtkWidget *menu;

    g_return_if_fail (GTK_IS_TREE_VIEW_COLUMN(column));
    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    menu = priv->column_menu;
    if (!menu)
        return;

    /* Synchronize the menu before display */
    gtk_container_foreach (GTK_CONTAINER(menu),
                           (GtkCallback)gnc_tree_view_update_column_menu_item,
                           view);

    /* Ensure all components are visible */
    gtk_widget_show_all (menu);

    /* Pop the menu up at the button */
#if GTK_CHECK_VERSION(3,22,0)
    gtk_menu_popup_at_pointer (GTK_MENU(priv->column_menu), NULL);
#else
    gtk_menu_popup (GTK_MENU(priv->column_menu), NULL, GTK_WIDGET(menu),
                    NULL, NULL, 0, gtk_get_current_event_time ());
#endif
}


void gnc_tree_view_expand_columns (GncTreeView *view,
                                   gchar *first_column_name,
                                   ...)
{
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column;
    gboolean hide_spacer;
    GList *columns, *tmp;
    gchar *name, *pref_name;
    va_list args;

    g_return_if_fail (GNC_IS_TREE_VIEW(view));
    ENTER(" ");
    va_start (args, first_column_name);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    name = first_column_name;
    hide_spacer = FALSE;

    /* First disable the expand property on all (non-infrastructure) columns. */
    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW(view));
    for (tmp = columns; tmp; tmp = g_list_next (tmp))
    {
        column = tmp->data;
        pref_name = g_object_get_data (G_OBJECT(column), PREF_NAME);
        if (pref_name != NULL)
            gtk_tree_view_column_set_expand (column, FALSE);
    }
    g_list_free(columns);

    /* Now enable it on the requested columns. */
    while (name != NULL)
    {
        column = gnc_tree_view_find_column_by_name (view, name);
        if (column != NULL)
        {
            gtk_tree_view_column_set_expand (column, TRUE);
            hide_spacer = TRUE;
        }
        name = va_arg (args, gchar*);
    }
    va_end (args);

    LEAVE(" ");
}


/* Links the cell backgrounds of the two control columns to the model or
   cell data function */
static void
update_control_cell_renderers_background (GncTreeView *view, GtkTreeViewColumn *col,
                                          gint column, GtkTreeCellDataFunc func )
{
    GList *renderers;
    GtkCellRenderer *cell;
    GList *node;

    renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT(col));

    /* Update the cell background in the list of renderers */
    for (node = renderers; node; node = node->next)
    {
        cell = node->data;
        if (func == NULL)
            gtk_tree_view_column_add_attribute (col, cell, "cell-background", column);
        else
            gtk_tree_view_column_set_cell_data_func (col, cell, func, view, NULL);
    }
    g_list_free (renderers);
}


/* This function links the cell backgrounds of the two control columns to a column
   in the model that has color strings or a cell data function */
void
gnc_tree_view_set_control_column_background (GncTreeView *view, gint column, GtkTreeCellDataFunc func )
{
    GncTreeViewPrivate *priv;

    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    ENTER("view %p, column %d, func %p", view, column, func);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    update_control_cell_renderers_background (view, priv->column_menu_column, column, func);

    LEAVE(" ");
}


/* This allows the columns to be setup without the model connected */
//FIXME I think this should be specified as a parameter to the add columns functions...
void
gnc_tree_view_set_sort_user_data (GncTreeView *view, GtkTreeModel *s_model)
{
    GncTreeViewPrivate *priv;

    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    ENTER("view %p, sort_model %p", view, s_model);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    priv->sort_model = s_model;
    LEAVE(" ");
}


/** This function is called to set the "show-column-menu" property on
 *  this view.  This function has no visible effect if the
 *  "state-section" property has not been set.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
void
gnc_tree_view_set_show_column_menu (GncTreeView *view,
                                    gboolean visible)
{
    GncTreeViewPrivate *priv;

    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    ENTER("view %p, show menu %d", view, visible);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    priv->show_column_menu = visible;
    gnc_tree_view_build_column_menu (view);
    LEAVE(" ");
}

/** This function is called to get the current value of the
 *  "show-column-menu" property.  It returns the same value passed to
 *  gnc_tree_view_set_show_menu_column().
 *
 *  Parameters are defined in gnc-tree-view.h
 */
gboolean
gnc_tree_view_get_show_column_menu (GncTreeView *view)
{
    GncTreeViewPrivate *priv;

    g_return_val_if_fail (GNC_IS_TREE_VIEW(view), FALSE);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    return (priv->show_column_menu);
}

/** @} */

/************************************************************/
/*                    Tree View Creation                    */
/************************************************************/

static gint
gnc_tree_view_count_visible_columns (GncTreeView *view)
{
    GList *columns, *node;
    gint count = 0;

    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW(view));
    for (node = columns; node; node = node->next)
    {
        GtkTreeViewColumn *col = GTK_TREE_VIEW_COLUMN(node->data);

        if (g_object_get_data (G_OBJECT(col), DEFAULT_VISIBLE) ||
                g_object_get_data (G_OBJECT(col), ALWAYS_VISIBLE))
            count++;
    }
    g_list_free (columns);
    return count;
}

void
gnc_tree_view_configure_columns (GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column;
    GList *columns;
    gboolean hide_menu_column;

    g_return_if_fail (GNC_IS_TREE_VIEW(view));

    ENTER(" ");

    /* Update the view and saved state */
    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW(view));
    g_list_foreach (columns, (GFunc)gnc_tree_view_update_visibility, view);
    g_list_free (columns);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->state_section)
        priv->seen_state_visibility = TRUE;

    /* If only the first column is visible, hide the spacer and make that
     * column expand. */
    hide_menu_column = (gnc_tree_view_count_visible_columns (view) == 1);
    column = gtk_tree_view_get_column (GTK_TREE_VIEW(view), 0);
    gtk_tree_view_column_set_expand (column, hide_menu_column);
    gtk_tree_view_column_set_visible (priv->column_menu_column, !hide_menu_column);

    LEAVE(" ");
}


/** This internal function sets a variety of common properties on a
 *  newly created GtkTreeViewColumn and its renderer.
 *
 *  @param view A pointer to a generic GncTreeView.
 *
 *  @param column The newly created tree view column.
 *
 *  @param pref_name The internal name of this column.  This name is
 *  used in several functions to look up the column, and it is also
 *  used to create the keys used to record the column width and
 *  visibility in saved state.
 *
 *  @param data_column The index of the GtkTreeModel data column used
 *  to determine the data that will be displayed in this column for
 *  each row in the view.  Use GNC_TREE_VIEW_COLUMN_DATA_NONE if you
 *  plan on using an non-model data source for this column.
 *
 *  @param default_width The width this column should be if not
 *  specified by saved state.  If the this value is zero, the column will
 *  be marked as automatically sized.
 *
 *  @param resizable Whether to mark the column as user resizable.
 *  This marking is only relevant for fixed width columns.
 *
 *  @param column_sort_fn The function that GtkTreeModelSort
 *  will call to compare two rows to determine their displayed order.
 *
 *  @internal
 */
static void
gnc_tree_view_column_properties (GncTreeView *view,
                                 GtkTreeViewColumn *column,
                                 const gchar *pref_name,
                                 gint data_column,
                                 gint default_width,
                                 gboolean resizable,
                                 GtkTreeIterCompareFunc column_sort_fn)
{
    GncTreeViewPrivate *priv;
    GtkTreeModel *s_model;
    gboolean visible;
    int width = 0;

    /* Set data used by other functions */
    if (pref_name)
        g_object_set_data (G_OBJECT(column), PREF_NAME, (gpointer)pref_name);
    if (data_column == 0)
        g_object_set_data (G_OBJECT(column), ALWAYS_VISIBLE, GINT_TO_POINTER(1));
    g_object_set_data (G_OBJECT(column), MODEL_COLUMN,
                       GINT_TO_POINTER(data_column));

    /* Get visibility */
    visible = gnc_tree_view_column_visible (view, NULL, pref_name);

    /* Set column attributes (without the sizing) */
    g_object_set (G_OBJECT(column),
                  "visible", visible,
                  "resizable", resizable && pref_name != NULL,
                  "reorderable", pref_name != NULL,
                  NULL);

    /* Get width */
    if (default_width == 0)
    {
        /* Set the sizing column attributes */
        g_object_set (G_OBJECT(column),
                      "sizing", GTK_TREE_VIEW_COLUMN_AUTOSIZE,
                      NULL);
    }
    else
    {

        /* If saved state comes back with a width of zero (or there is no saved
         * state width) the use the default width for the column.  Allow for
         * padding L and R of the displayed data. */
        if (width == 0)
            width = default_width + 10;
        if (width == 0)
            width = 10;

        /* Set the sizing column attributes (including fixed-width) */
        g_object_set (G_OBJECT(column),
                      "sizing", GTK_TREE_VIEW_COLUMN_FIXED,
                      "fixed-width", width,
                      NULL);
        /* Save the initially calculated preferred width for later
         * comparison to the actual width when saving state. Can't
         * use the "fixed-width" property for that because it changes
         * when the user resizes the column.
         */
        g_object_set_data (G_OBJECT(column),
                     "default-width", GINT_TO_POINTER(width));
    }

    s_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    if (GTK_IS_TREE_SORTABLE(s_model))
    {
        gtk_tree_view_column_set_sort_column_id (column, data_column);
        if (column_sort_fn)
        {
            gtk_tree_sortable_set_sort_func (GTK_TREE_SORTABLE(s_model),
                                             data_column, column_sort_fn,
                                             GINT_TO_POINTER(data_column),
                                             NULL /* destroy fn */);
        }
    }

    // Used in registers, sort model not connected to view yet
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->sort_model != NULL)
    {
        gtk_tree_view_column_set_sort_column_id (column, data_column);
        if (column_sort_fn)
        {
            gtk_tree_sortable_set_sort_func (GTK_TREE_SORTABLE(priv->sort_model),
                                             data_column, column_sort_fn,
                                             view,
                                             NULL /* destroy fn */);
        }
    }

    /* Add to the column selection menu */
    if (pref_name)
    {
        gnc_tree_view_create_menu_item (column, view);
    }
}

/** This function adds a new toggle column to a GncTreeView base view.
 *  It takes all the parameters necessary to hook a GtkTreeModel
 *  column to a GtkTreeViewColumn.  It handles creating a tooltip to
 *  show the full title name, and setting the sort and edit callback
 *  functions.  If the tree has a state section associated with it,
 *  this function also wires up the column so that its visibility and
 *  width are remembered.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
GtkTreeViewColumn *
gnc_tree_view_add_toggle_column (GncTreeView *view,
                                 const gchar *column_title,
                                 const gchar *column_short_title,
                                 const gchar *pref_name,
                                 gint model_data_column,
                                 gint model_visibility_column,
                                 GtkTreeIterCompareFunc column_sort_fn,
                                 renderer_toggled toggle_edited_cb)
{
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;

    g_return_val_if_fail (GNC_IS_TREE_VIEW(view), NULL);

    renderer = gtk_cell_renderer_toggle_new ();
    if (!toggle_edited_cb)
    {
        gtk_cell_renderer_toggle_set_activatable (GTK_CELL_RENDERER_TOGGLE(renderer), FALSE);
    }
    column =
        gtk_tree_view_column_new_with_attributes (column_short_title,
                renderer,
                "active", model_data_column,
                NULL);

    /* Add the full title to the object for menu creation */
    g_object_set_data_full (G_OBJECT(column), REAL_TITLE,
                            g_strdup(column_title), g_free);
    if (toggle_edited_cb)
        g_signal_connect (G_OBJECT(renderer), "toggled",
                          G_CALLBACK(toggle_edited_cb), view);

    if (model_visibility_column != GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "visible", model_visibility_column);


    gnc_tree_view_column_properties (view, column, pref_name, model_data_column,
                                     0, FALSE, column_sort_fn);

    gnc_tree_view_append_column (view, column);

    /* Also add the full title to the object as a tooltip */
    gtk_widget_set_tooltip_text (gtk_tree_view_column_get_button (column), column_title);

    return column;
}

static void
renderer_editing_canceled_cb (GtkCellRenderer *renderer, gpointer user_data)
{
    GncTreeView *view = user_data;
    GncTreeViewPrivate *priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->editing_finished_cb)
        (priv->editing_finished_cb)(view, priv->editing_cb_data);
}

static void
renderer_editing_started_cb (GtkCellRenderer *renderer,
               GtkCellEditable *editable, gchar *path, gpointer user_data)
{
    GncTreeView *view = user_data;
    GncTreeViewPrivate *priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->editing_started_cb)
        (priv->editing_started_cb)(view, priv->editing_cb_data);
}

static void
renderer_edited_cb (GtkCellRendererText *renderer, gchar *path,
                    gchar *new_text, gpointer user_data)
{
    GncTreeView *view = user_data;
    GncTreeViewPrivate *priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->editing_finished_cb)
        (priv->editing_finished_cb)(view, priv->editing_cb_data);
}

/** This function adds a new text column to a GncTreeView base view.
 *  It takes all the parameters necessary to hook a GtkTreeModel
 *  column to a GtkTreeViewColumn.  If the tree has a state section
 *  associated with it, this function also wires up the column so that
 *  its visibility and width are remembered.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
GtkTreeViewColumn *
gnc_tree_view_add_text_column (GncTreeView *view,
                               const gchar *column_title,
                               const gchar *pref_name,
                               const gchar *icon_name,
                               const gchar *sizing_text,
                               gint model_data_column,
                               gint model_visibility_column,
                               GtkTreeIterCompareFunc column_sort_fn)
{
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;
    PangoLayout* layout;
    int default_width, title_width;

    g_return_val_if_fail (GNC_IS_TREE_VIEW(view), NULL);

    column = gtk_tree_view_column_new ();
    gtk_tree_view_column_set_title (column, column_title);

    /* Set up an icon renderer if requested */
    if (icon_name)
    {
        renderer = gtk_cell_renderer_pixbuf_new ();
        g_object_set (renderer, "icon-name", icon_name, NULL);
        gtk_tree_view_column_pack_start (column, renderer, FALSE);
    }

    /* Set up a text renderer and attributes */
    renderer = gtk_cell_renderer_text_new ();
    gtk_tree_view_column_pack_start (column, renderer, TRUE);

    /* Set up the callbacks for when editing */
    g_signal_connect (G_OBJECT(renderer), "editing-canceled",
                          (GCallback)renderer_editing_canceled_cb, view);

    g_signal_connect (G_OBJECT(renderer), "editing-started",
                          (GCallback)renderer_editing_started_cb, view);

    g_signal_connect (G_OBJECT(renderer), "edited",
                          (GCallback)renderer_edited_cb, view);

    /* Set renderer attributes controlled by the model */
    if (model_data_column != GNC_TREE_VIEW_COLUMN_DATA_NONE)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "text", model_data_column);
    if (model_visibility_column != GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "visible", model_visibility_column);

    /* Default size is the larger of the column title and the sizing text */
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), column_title);
    pango_layout_get_pixel_size (layout, &title_width, NULL);
    g_object_unref (layout);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), sizing_text);
    pango_layout_get_pixel_size (layout, &default_width, NULL);
    g_object_unref (layout);
    default_width = MAX(default_width, title_width);
    if (default_width)
        default_width += 10; /* padding on either side */
    gnc_tree_view_column_properties (view, column, pref_name, model_data_column,
                                     default_width, TRUE, column_sort_fn);

    gnc_tree_view_append_column (view, column);
    return column;
}



/** This function adds a new date column to a GncTreeView base view.
 *  It takes all the parameters necessary to hook a GtkTreeModel
 *  column to a GtkTreeViewColumn.  If the tree has a state section
 *  associated with it, this function also wires up the column so that
 *  its visibility and width are remembered.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
GtkTreeViewColumn *
gnc_tree_view_add_date_column (GncTreeView *view,
                               const gchar *column_title,
                               const gchar *pref_name,
                               const gchar *icon_name,
                               const gchar *sizing_text,
                               gint model_data_column,
                               gint model_visibility_column,
                               GtkTreeIterCompareFunc column_sort_fn)
{
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;
    PangoLayout* layout;
    int default_width, title_width;

    g_return_val_if_fail (GNC_IS_TREE_VIEW(view), NULL);

    column = gtk_tree_view_column_new ();
    gtk_tree_view_column_set_title (column, column_title);

    /* Set up an icon renderer if requested */
    if (icon_name)
    {
        renderer = gtk_cell_renderer_pixbuf_new ();
        g_object_set (renderer, "icon-name", icon_name, NULL);
        gtk_tree_view_column_pack_start (column, renderer, FALSE);
    }

    /* Set up a text renderer and attributes */
    renderer = gnc_cell_renderer_date_new (TRUE);
    gtk_tree_view_column_pack_start (column, renderer, TRUE);

    /* Set renderer attributes controlled by the model */
    if (model_data_column != GNC_TREE_VIEW_COLUMN_DATA_NONE)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "text", model_data_column);
    if (model_visibility_column != GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "visible", model_visibility_column);

    /* Default size is the larger of the column title and the sizing text */
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), column_title);
    pango_layout_get_pixel_size (layout, &title_width, NULL);
    g_object_unref (layout);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), sizing_text);
    pango_layout_get_pixel_size (layout, &default_width, NULL);
    g_object_unref (layout);
    default_width = MAX(default_width, title_width);
    if (default_width)
        default_width += 10; /* padding on either side */
    gnc_tree_view_column_properties (view, column, pref_name, model_data_column,
                                     default_width, TRUE, column_sort_fn);

    gnc_tree_view_append_column (view, column);
    return column;
}


GtkTreeViewColumn *
gnc_tree_view_add_combo_column (GncTreeView *view,
                                const gchar *column_title,
                                const gchar *pref_name,
                                const gchar *sizing_text,
                                gint model_data_column,
                                gint model_visibility_column,
                                GtkTreeModel *combo_tree_model,
                                gint combo_model_text_column,
                                GtkTreeIterCompareFunc column_sort_fn)
{
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;
    PangoLayout* layout;
    int default_width, title_width;

    g_return_val_if_fail (GNC_IS_TREE_VIEW(view), NULL);

    column = gtk_tree_view_column_new ();
    gtk_tree_view_column_set_title (column, gettext(column_title));

    /* Set up a renderer and attributes */
    renderer = gtk_cell_renderer_combo_new ();
    gtk_tree_view_column_pack_start (column, renderer, TRUE);

    /* Set renderer attributes controlled by the model */
    if (model_data_column != GNC_TREE_VIEW_COLUMN_DATA_NONE)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "text", model_data_column);
    if (model_visibility_column != GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "visible", model_visibility_column);

    /* Default size is the larger of the column title and the sizing text */
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), column_title);
    pango_layout_get_pixel_size (layout, &title_width, NULL);
    g_object_unref (layout);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), sizing_text);
    pango_layout_get_pixel_size (layout, &default_width, NULL);
    g_object_unref (layout);
    default_width = MAX(default_width, title_width);
    if (default_width)
        default_width += 10; /* padding on either side */

    gnc_tree_view_column_properties (view, column, pref_name, model_data_column,
                                     default_width, TRUE, column_sort_fn);

    /* Stuff specific to combo */
    if (combo_tree_model)
    {
        g_object_set (G_OBJECT(renderer), "model", combo_tree_model,
                      "text-column", combo_model_text_column, NULL);
    }
    /* TODO: has-entry? */

    gnc_tree_view_append_column (view, column);
    return column;
}

GtkCellRenderer *
gnc_tree_view_column_get_renderer (GtkTreeViewColumn *column)
{
    GList *renderers;
    GtkCellRenderer *cr = NULL;

    g_return_val_if_fail (GTK_TREE_VIEW_COLUMN(column), NULL);

    /* Get the list of one renderer */
    renderers = gtk_cell_layout_get_cells (GTK_CELL_LAYOUT(column));
    if (g_list_length (renderers) > 0)
        cr = GTK_CELL_RENDERER(renderers->data);
    g_list_free (renderers);

    return cr;
}

/** This function adds a new numeric column to a GncTreeView base
 *  view.  It takes all the parameters necessary to hook a
 *  GtkTreeModel column to a GtkTreeViewColumn.  If the tree has a
 *  state section associated with it, this function also wires up the
 *  column so that its visibility and width are remembered.  A numeric
 *  column is nothing more then a text column with a few extra
 *  attributes.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
GtkTreeViewColumn *
gnc_tree_view_add_numeric_column (GncTreeView *view,
                                  const gchar *column_title,
                                  const gchar *pref_name,
                                  const gchar *sizing_text,
                                  gint model_data_column,
                                  gint model_color_column,
                                  gint model_visibility_column,
                                  GtkTreeIterCompareFunc column_sort_fn)
{
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;

    column = gnc_tree_view_add_text_column (view, column_title, pref_name,
                                            NULL, sizing_text, model_data_column,
                                            model_visibility_column,
                                            column_sort_fn);

    renderer = gnc_tree_view_column_get_renderer (column);

    /* Right align the column title and data */
    g_object_set (G_OBJECT(column), "alignment",   1.0, NULL);
    g_object_set (G_OBJECT(renderer), "xalign",   1.0, NULL);

    /* Change the text color */
    if (model_color_column != GNC_TREE_VIEW_COLUMN_COLOR_NONE)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "foreground", model_color_column);

    return column;
}

/** Add a column to a view based upon a GncTreeView.  This function
 *  knows about the two special columns on the right side of this type
 *  of view, and adds the new column before these two columns.  You
 *  could say that it appends to the data columns and ignores the
 *  infrastructure columns.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
gint
gnc_tree_view_append_column (GncTreeView *view,
                             GtkTreeViewColumn *column)
{
    GList *columns;
    int n;

    /* There's no easy way to get this number. */
    columns = gtk_tree_view_get_columns (GTK_TREE_VIEW(view));
    n = g_list_length (columns);
    g_list_free (columns);

    /* Ignore the initial column, the selection menu */
    if (n >= 1)
        n -= 1;
    return gtk_tree_view_insert_column (GTK_TREE_VIEW(view), column, n);
}

static gboolean
get_column_next_to (GtkTreeView *tv, GtkTreeViewColumn **col, gboolean backward)
{
    GList *cols, *node;
    GtkTreeViewColumn *c = NULL;
    gint seen = 0;
    gboolean wrapped = FALSE;

    cols = gtk_tree_view_get_columns (tv);
    g_return_val_if_fail (g_list_length (cols) > 0, FALSE);

    node = g_list_find (cols, *col);
    g_return_val_if_fail (node, FALSE);
    do
    {
        node = backward ? node->prev : node->next;
        if (!node)
        {
            wrapped = TRUE;
            node = backward ? g_list_last (cols) : cols;
        }
        c = GTK_TREE_VIEW_COLUMN (node->data);
        if (c && gtk_tree_view_column_get_visible (c))
            seen++;
        if (c == *col) break;
    }
    while (!seen);

    g_list_free (cols);
    *col = c;
    return wrapped;
}

gboolean
gnc_tree_view_path_is_valid (GncTreeView *view, GtkTreePath *path)
{
    GtkTreeView *tv = GTK_TREE_VIEW(view);
    GtkTreeModel *s_model;
    GtkTreeIter iter;

    s_model = gtk_tree_view_get_model (tv);
    return gtk_tree_model_get_iter (s_model, &iter, path);
}

void
gnc_tree_view_keynav (GncTreeView *view, GtkTreeViewColumn **col,
                      GtkTreePath *path, GdkEventKey *event)
{
    GtkTreeView *tv = GTK_TREE_VIEW(view);
    gint depth;
    gboolean shifted;

    if (event->type != GDK_KEY_PRESS) return;

    switch (event->keyval)
    {
    case GDK_KEY_Tab:
    case GDK_KEY_ISO_Left_Tab:
    case GDK_KEY_KP_Tab:
        shifted = event->state & GDK_SHIFT_MASK;
        if (get_column_next_to (tv, col, shifted))
        {
            /* This is the end (or beginning) of the line, buddy. */
            depth = gtk_tree_path_get_depth (path);
            if (shifted)
            {
                if (!gtk_tree_path_prev (path) && depth > 1)
                {
                    gtk_tree_path_up (path);
                }
            }
            else if (gtk_tree_view_row_expanded (tv, path))
            {
                gtk_tree_path_down (path);
            }
            else
            {
                gtk_tree_path_next (path);
                if (!gnc_tree_view_path_is_valid (view, path) && depth > 2)
                {
                    gtk_tree_path_prev (path);
                    gtk_tree_path_up (path);
                    gtk_tree_path_next (path);
                }
                if (!gnc_tree_view_path_is_valid (view, path) && depth > 1)
                {
                    gtk_tree_path_prev (path);
                    gtk_tree_path_up (path);
                    gtk_tree_path_next (path);
                }
            }
        }
        break;

    case GDK_KEY_Return:
    case GDK_KEY_KP_Enter:
        if (gtk_tree_view_row_expanded (tv, path))
        {
            gtk_tree_path_down (path);
        }
        else
        {
            depth = gtk_tree_path_get_depth (path);
            gtk_tree_path_next (path);
            if (!gnc_tree_view_path_is_valid (view, path) && depth > 1)
            {
                gtk_tree_path_prev (path);
                gtk_tree_path_up (path);
                gtk_tree_path_next (path);
            }
        }
        break;
    }
    return;
}

void
gnc_tree_view_set_editing_started_cb (GncTreeView *view, GFunc editing_started_cb, gpointer editing_cb_data)
{
    GncTreeViewPrivate *priv;

    if (!view && !editing_started_cb)
        return;

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    priv->editing_started_cb = editing_started_cb;
    priv->editing_cb_data = editing_cb_data;
}

void
gnc_tree_view_set_editing_finished_cb (GncTreeView *view, GFunc editing_finished_cb, gpointer editing_cb_data)
{
    GncTreeViewPrivate *priv;

    if (!view && !editing_finished_cb)
        return;

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    priv->editing_finished_cb = editing_finished_cb;
    priv->editing_cb_data = editing_cb_data;
}

/** @} */
/** @} */
