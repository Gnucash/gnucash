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

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>

#include "gnc-tree-view.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"

/* The column id refers to a specific column in the tree model.  It is
 * also attached to the side of the tree column to allow lookup of a
 * GtkTreeViewColumn when passed a column id from the underlying
 * model. By convention, negative column numbers are used when the
 * visible column has no association with the underlying model.*/
#define MODEL_COLUMN "model_column"

/* For checkbox columns, this contains the real title for the column. */
#define REAL_TITLE  "real_title"

/* The name of this column as it should appear in gconf.  This is
 * attached to the column when it is created, and used to map back and
 * forth to gconf keys.  The actual gconf keys are built from these
 * strings. */
#define PREF_NAME  "pref-name"

/* The actual gconf key for a particular column visibility.  This is
 * attached to the menu items that are in the column selection menu.
 * Makes it very easy to update gconf when a menu item is toggled. */
#define GCONF_KEY  "gconf-key"

/* Gconf keys within this particular section of gconf. */
#define GCONF_KEY_SORT_COLUMN  "sort_column"
#define GCONF_KEY_SORT_ORDER   "sort_order"
#define GCONF_KEY_COLUMN_ORDER "column_order"

/* Partial gconf keys within this particular section of gconf. These
   are appended to the various column names to create the actual
   keys. */
#define GCONF_KEY_VISIBLE      "visible"
#define GCONF_KEY_WIDTH        "width"

enum
{
    PROP_0,
    PROP_GCONF_SECTION,
    PROP_SHOW_COLUMN_MENU,
};

/** Static Globals *******************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/**** Declarations ******************************************************/
static void gnc_tree_view_class_init (GncTreeViewClass *klass);
static void gnc_tree_view_init (GncTreeView *view, GncTreeViewClass *klass);
static void gnc_tree_view_finalize (GObject *object);
static void gnc_tree_view_destroy (GtkObject *object);
static void gnc_tree_view_set_property (GObject         *object,
                                        guint            prop_id,
                                        const GValue    *value,
                                        GParamSpec      *pspec);
static void gnc_tree_view_get_property (GObject         *object,
                                        guint            prop_id,
                                        GValue          *value,
                                        GParamSpec      *pspec);
static void gnc_tree_view_remove_gconf(GncTreeView *view);
static gboolean gnc_tree_view_drop_ok_cb (GtkTreeView *view,
        GtkTreeViewColumn *column,
        GtkTreeViewColumn *prev_column,
        GtkTreeViewColumn *next_column,
        gpointer data);
static void gtk_tree_view_sort_column_changed_cb (GtkTreeSortable *treesortable,
        GncTreeView *view);
static void gnc_tree_view_build_column_menu (GncTreeView *view);
static void gnc_tree_view_select_column_cb (GtkTreeViewColumn *column,
        GncTreeView *view);

/** Private Data Structure ***********************************************/

typedef struct GncTreeViewPrivate
{
    GtkTooltips       *title_tips;

    /*  Spacer column */
    GtkTreeViewColumn *spacer_column;
    GtkTreeViewColumn *selection_column;

    /* Column selection menu related values */
    GtkTreeViewColumn *column_menu_column;
    GtkWidget         *column_menu;
    gboolean           show_column_menu;

    /* Gconf related values */
    gchar             *gconf_section;
    gboolean           seen_gconf_visibility;
    gulong             columns_changed_cb_id;
    gulong             sort_column_changed_cb_id;
    gulong             size_allocate_cb_id;
} GncTreeViewPrivate;

#define GNC_TREE_VIEW_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_TREE_VIEW, GncTreeViewPrivate))


/************************************************************/
/*               g_object required functions                */
/************************************************************/

/** @name Gnc Tree View Object Implementation
 @{ */

static GObjectClass *parent_class = NULL;

/** Create a new glib type for the base gnucash tree view.
 *
 *  @internal
 *
 *  @return The new type value.
 */
GType
gnc_tree_view_get_type (void)
{
    static GType gnc_tree_view_type = 0;

    if (gnc_tree_view_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncTreeViewClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_tree_view_class_init,
            NULL,
            NULL,
            sizeof (GncTreeView),
            0,
            (GInstanceInitFunc) gnc_tree_view_init
        };

        gnc_tree_view_type = g_type_register_static (GTK_TYPE_TREE_VIEW,
                             GNC_TREE_VIEW_NAME,
                             &our_info, 0);
    }

    return gnc_tree_view_type;
}

/** Initialize the class for the new base gnucash tree view.  This
 *  will set up any function pointers that override functions in the
 *  parent class, and also installs the proprieties that are unique to
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
    GtkObjectClass *gtkobject_class;

    parent_class = g_type_class_peek_parent (klass);

    gobject_class = G_OBJECT_CLASS (klass);
    gtkobject_class = GTK_OBJECT_CLASS (klass);

    gobject_class->set_property = gnc_tree_view_set_property;
    gobject_class->get_property = gnc_tree_view_get_property;

    g_type_class_add_private(klass, sizeof(GncTreeViewPrivate));

    g_object_class_install_property (gobject_class,
                                     PROP_GCONF_SECTION,
                                     g_param_spec_string ("gconf-section",
                                             "Gconf Section",
                                             "The Gconf section to use for storing settings",
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

    /* GtkObject signals */
    gtkobject_class->destroy = gnc_tree_view_destroy;
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
gnc_tree_view_init (GncTreeView *view, GncTreeViewClass *klass)
{
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column;
    GtkWidget *icon;
    GtkRequisition requisition;

    gnc_gobject_tracking_remember(G_OBJECT(view), G_OBJECT_CLASS(klass));

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    priv->column_menu = NULL;
    priv->show_column_menu = FALSE;
    priv->gconf_section = NULL;
    priv->seen_gconf_visibility = FALSE;
    priv->columns_changed_cb_id = 0;
    priv->sort_column_changed_cb_id = 0;
    priv->size_allocate_cb_id = 0;

    /* Ask gtk to help the user keep track of rows. */
    g_object_set(view, "rules-hint", TRUE, NULL);

    /* Handle column drag and drop */
    gtk_tree_view_set_column_drag_function(GTK_TREE_VIEW(view),
                                           gnc_tree_view_drop_ok_cb, NULL, NULL);

    /* Create the next to last column which is always present, visible,
     * and empty. Override the defaults and make this a one pixel wide
     * column, but have it take up any extra space in the window. */
    column = gnc_tree_view_add_text_column (view, NULL, NULL, NULL, NULL,
                                            -1, -1, NULL);
    g_object_set(G_OBJECT(column),
                 "fixed-width", 1,
                 "expand", TRUE,
                 (gchar *)NULL);
    priv->spacer_column = column;

    /* Create the last column which contains the column selection
     * widget.  gnc_tree_view_add_text_column will do most of the
     * work. */
    icon = gtk_image_new_from_stock(GTK_STOCK_GO_DOWN,
                                    GTK_ICON_SIZE_SMALL_TOOLBAR);
    gtk_widget_show(icon);
    gtk_widget_size_request(icon, &requisition);
    column = gnc_tree_view_add_text_column (view, NULL, NULL, NULL, NULL,
                                            -1, -1, NULL);
    g_object_set(G_OBJECT(column),
                 "clickable", TRUE,
                 "widget", icon,
                 "fixed-width", requisition.width + 10,
                 (gchar *)NULL);
    priv->selection_column = column;
    g_signal_connect(G_OBJECT(column), "clicked",
                     G_CALLBACK (gnc_tree_view_select_column_cb),
                     view);
    priv->column_menu_column = column;
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
    GncTreeView *view;
    GncTreeViewPrivate *priv;

    ENTER("view %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_VIEW (object));

    gnc_gobject_tracking_forget(object);

    view = GNC_TREE_VIEW (object);
    priv = GNC_TREE_VIEW_GET_PRIVATE (view);

    if (G_OBJECT_CLASS (parent_class)->finalize)
        G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}

/** Destroy the GncTreeView object.  This function is called (possibly
 *  multiple times) from the Gtk_Object level to destroy the object.
 *  It should release any memory owned by the object that isn't
 *  fundamental to the implementation.  In this function any active
 *  callbacks are disconnected, all memory other than the private data
 *  structure are freed.
 *
 *  @param object The object being destroyed.
 *
 *  @internal
 */
static void
gnc_tree_view_destroy (GtkObject *object)
{
    GncTreeView *view;
    GncTreeViewPrivate *priv;

    ENTER("view %p", object);
    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_TREE_VIEW (object));

    view = GNC_TREE_VIEW (object);

    gnc_tree_view_remove_gconf(view);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    if (priv->column_menu)
    {
        DEBUG("removing column selection menu");
        gtk_widget_unref(priv->column_menu);
        priv->column_menu = NULL;
    }

    if (GTK_OBJECT_CLASS (parent_class)->destroy)
        GTK_OBJECT_CLASS (parent_class)->destroy (object);
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
    GncTreeView *view = GNC_TREE_VIEW (object);
    GncTreeViewPrivate *priv;

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    switch (prop_id)
    {
    case PROP_GCONF_SECTION:
        g_value_set_string (value, priv->gconf_section);
        break;
    case PROP_SHOW_COLUMN_MENU:
        g_value_set_boolean (value, priv->show_column_menu);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
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
    GncTreeView *view = GNC_TREE_VIEW (object);

    switch (prop_id)
    {
    case PROP_GCONF_SECTION:
        gnc_tree_view_set_gconf_section (view, g_value_get_string (value));
        break;
    case PROP_SHOW_COLUMN_MENU:
        gnc_tree_view_set_show_column_menu (view, g_value_get_boolean (value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
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
    column_list = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
    for (tmp = column_list; tmp; tmp = g_list_next(tmp))
    {
        column = tmp->data;
        id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(column), MODEL_COLUMN));
        if (id != wanted)
            continue;
        found = column;
        break;
    }
    g_list_free(column_list);

    // LEAVE("column %p", found);
    return found;
}

/** Find a tree column given the "pref name" used with gconf.  This
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
    for (tmp = column_list; tmp; tmp = g_list_next(tmp))
    {
        column = tmp->data;
        name = g_object_get_data(G_OBJECT(column), PREF_NAME);
        if (!name || (strcmp(name, wanted) != 0))
            continue;
        found = column;
        break;
    }
    g_list_free(column_list);

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
    pref_name = g_object_get_data(G_OBJECT(prev_column), PREF_NAME);
    if (!pref_name)
        return FALSE;

    /* Everything else is allowed. */
    return TRUE;
}

/** This function is called when the sort order has changed on the
 *  underlying GtkTreeModel.  It propagates these changes to gconf to
 *  keep it in sync with the user's view of the tree.
 *
 *  @param sortable The underlying sortable model.
 *
 *  @param view A pointer to the GncTreeView that displays the model
 *  data.
 *
 *  @internal
 */
static void
gtk_tree_view_sort_column_changed_cb (GtkTreeSortable *treesortable,
                                      GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column;
    const gchar  	    *gconf_section;
    gchar        	    *column_pref_name;
    GtkSortType  	     order;
    gint         	     id;

    g_return_if_fail(GTK_IS_TREE_SORTABLE(treesortable));
    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    ENTER(" ");
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (!priv->gconf_section)
    {
        LEAVE("no gconf section");
        return;
    }

    /* Set defaults, then extract data from the model */
    if (!gtk_tree_sortable_get_sort_column_id(treesortable, &id, &order))
    {
        order = GTK_SORT_ASCENDING;
        id = GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID;
    }
    column = view_column_find_by_model_id (view, id);
    column_pref_name = g_object_get_data(G_OBJECT(column), PREF_NAME);

    if (!column_pref_name)
        column_pref_name = "none";

    /* Store the values in gconf */
    gconf_section = priv->gconf_section;
    gnc_gconf_set_string(gconf_section, GCONF_KEY_SORT_COLUMN,
                         column_pref_name, NULL);
    gnc_gconf_set_string(gconf_section, GCONF_KEY_SORT_ORDER,
                         gnc_enum_to_nick(GTK_TYPE_SORT_TYPE, order), NULL);
    LEAVE(" ");
}

/** This function is called when the column order has changed in the
 *  gtk tree view.  It propagates these changes to gconf to keep it in
 *  sync with the user's view of the tree.  This function runs the
 *  list of all tree column data structures, building a second list of
 *  column names.  This second list is passed to gconf for storing.
 *  Only columns that have names can be saved in this fashion.
 *
 *  @param view The tree view.
 *
 *  @param data Unused.
 *
 *  @internal
 */
static void
gtk_tree_view_columns_changed_cb (GncTreeView *view,
                                  gpointer data)
{
    GncTreeViewPrivate *priv;
    GList *column_list, *tmp;
    GSList *column_names = NULL;
    gchar *name;

    //ENTER("view %p, data %p", view, data);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    column_list = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
    for (tmp = column_list; tmp; tmp = g_list_next(tmp))
    {
        name = g_object_get_data(tmp->data, PREF_NAME);
        if (!name)
            continue;
        column_names = g_slist_append(column_names, name);
        //DEBUG("%s", name);
    }
    g_list_free(column_list);

    gnc_gconf_set_list(priv->gconf_section, GCONF_KEY_COLUMN_ORDER,
                       GCONF_VALUE_STRING, column_names, NULL);
    g_slist_free(column_names);
    //LEAVE(" ");
}

/** This is the helper function for gtk_tree_view_size_allocate_cb().
 *  It compares the actual column width to the width as stored in
 *  gconf.  If the two are different, it the updates gconf with the
 *  actual width.  This will trigger a notification from gconf that
 *  the size has changes, and that code must be smart enough to
 *  prevent an infinite loop.  Not storing unchanged values prevents
 *  spurious callbacks from gconf and just saves processing time.
 *
 *  @param column The column whose size should be updated in gconf..
 *
 *  @param view The tree view.
 *
 *  @internal
 */
static void
gtk_tree_view_size_allocate_helper (GtkTreeViewColumn *column,
                                    GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    const gchar *column_pref_name;
    gint new_width, current_width;
    gchar *key;

    g_return_if_fail(GTK_IS_TREE_VIEW_COLUMN(column));
    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    /* Any updates to be made? */
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    column_pref_name = g_object_get_data(G_OBJECT(column), PREF_NAME);
    if (!column_pref_name)
        return;
    new_width = gtk_tree_view_column_get_width (column);
    if (!new_width)
        return;

    /* Do it */
    key = g_strdup_printf("%s_%s", column_pref_name, GCONF_KEY_WIDTH);
    current_width = gnc_gconf_get_int(priv->gconf_section, key, NULL);
    if (new_width != current_width)
    {
        gnc_gconf_set_int(priv->gconf_section, key, new_width, NULL);
        DEBUG("set %s width to %d", column_pref_name, new_width);
    }
    g_free(key);
}

/** This function is called when the tree size has been reallocated.
 *  When called, one of two events has occurred.  1) The column sizes
 *  within the tree have changed.  2) The window containing the tree
 *  has been resized.  This code is concerned with case one.  Case two
 *  is a don't care, unless the window resizing has caused a column
 *  resizing.
 *
 *  This function simply runs the list of all tree columns, calling the
 *  gtk_tree_view_size_allocate_helper() function on each column.
 *  There is no callback that can be used for notification of an
 *  individual column change.  Its this or nothing.
 *
 *  @param widget The tree view.
 *
 *  @param allocation Unused.
 *
 *  @param data Unused.
 *
 *  @internal
 */
static void
gtk_tree_view_size_allocate_cb (GtkWidget *widget,
                                GtkAllocation *allocation,
                                gpointer data)
{
    GncTreeView *view;
    GList *column_list;

    g_return_if_fail(GNC_IS_TREE_VIEW(widget));

    view = GNC_TREE_VIEW(widget);
    column_list = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
    g_list_foreach(column_list, (GFunc)gtk_tree_view_size_allocate_helper, view);
    g_list_free(column_list);
}

/** @} */

/************************************************************/
/*                  Gconf Setup / Callbacks                 */
/************************************************************/

/** @name Gnc Tree View Gconf Callback / Related Functions
 @{ */

/** Determine the visibility of a column.  This function first looks
 *  for columns specially marked to be always visible, or columns
 *  without a preference name.  These are always shown.  Next, this
 *  function checks to see if gconf is responsible for this view *and*
 *  that gconf visibility keys have been seen.  (This handles the
 *  'first run' case where gconf should manage a view but no keys yet
 *  exist in gconf.)  If so, the gconf visibility key is returned.
 *  Otherwise the "'default visible column' list is checked and a
 *  value of TRUE returned if the pref name is found, otherwise FALSE.
 *
 *  @param view A GncTreeView.
 *
 *  @param column The GtkTreeViewColumn in question.  Either this
 *  value or the pref_name parameter must be non-NULL.
 *
 *  @param pref_name The name of the column in question.  Either this
 *  value or the column parameter must be non-NULL.
 *
 *  @returns TRUE if the column should be visible.  FALSE otherwise.
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
    gchar *key;

    ENTER("column %p, name %s", column, pref_name ? pref_name : "(null)");
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (column)
    {
        if (g_object_get_data(G_OBJECT(column), ALWAYS_VISIBLE))
        {
            LEAVE("1, first column");
            return TRUE;
        }
        pref_name = (gchar *)g_object_get_data(G_OBJECT(column), PREF_NAME);
        DEBUG("pref_name is %s", pref_name ? pref_name : "(null)");
    }

    if (!pref_name)
    {
        LEAVE("1, no pref name");
        return TRUE;
    }

    /* Using gconf? */
    if (priv->gconf_section)
    {
        if (priv->seen_gconf_visibility)
        {
            key = g_strdup_printf("%s_%s", pref_name, GCONF_KEY_VISIBLE);
            visible = gnc_gconf_get_bool(priv->gconf_section, key, NULL);
            g_free(key);
            LEAVE("%d, gconf visibility", visible);
            return visible;
        }

        visible = column ?
                  (g_object_get_data(G_OBJECT(column), DEFAULT_VISIBLE) != NULL) : FALSE;
        LEAVE("%d, gconf but using defaults", visible);
        return visible;
    }

    /* Check the default columns list */
    visible = column ?
              (g_object_get_data(G_OBJECT(column), DEFAULT_VISIBLE) != NULL) : FALSE;
    LEAVE("defaults says %d", visible);
    return visible;
}

/** This function updates the visibility of a single column.  It
 *  checks if the column should be visible, and if so tells the view
 *  to show the column and (if needed) updates the gconf database.
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
    GncTreeViewPrivate *priv;
    gchar *name, *key;
    gboolean visible;

    g_return_if_fail(GTK_IS_TREE_VIEW_COLUMN(column));
    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    ENTER(" ");
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    visible = gnc_tree_view_column_visible(view, column, NULL);
    gtk_tree_view_column_set_visible(column, visible);
    if (priv->gconf_section)
    {
        name = (gchar *)g_object_get_data(G_OBJECT(column), PREF_NAME);
        if (!name)
        {
            LEAVE("no pref name");
            return;
        }
        key = g_strdup_printf("%s_%s", name, GCONF_KEY_VISIBLE);
        gnc_gconf_set_bool(priv->gconf_section, key, visible, NULL);
        g_free(key);
        LEAVE("made %s, set gconf key", visible ? "visible" : "invisible");
        return;
    }
    LEAVE("made %s", visible ? "visible" : "invisible");
}

/** This function is called via the gconf notification callback when
 *  the sort order has changed in gconf.  It must update the model
 *  sort order to match what is specified in gconf.  The sort column
 *  used will be whatever is the current sort column.  This function
 *  is careful to prevent an infinite loop by blocking the
 *  "sort-column-changed" callback that is attached to the model.
 *
 *  @param widget The tree view.
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
    GncTreeViewPrivate *priv;
    GtkTreeModel *s_model;
    GtkSortType order;
    gint current;

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    if (!s_model)
        return;
    order = gnc_enum_from_nick(GTK_TYPE_SORT_TYPE, name, GTK_SORT_ASCENDING);
    if (!gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(s_model),
            &current, NULL))
        current = GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID;
    g_signal_handler_block(s_model, priv->sort_column_changed_cb_id);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(s_model),
                                         current, order);
    g_signal_handler_unblock(s_model, priv->sort_column_changed_cb_id);
    DEBUG("sort_order set to %s", gnc_enum_to_nick(GTK_TYPE_SORT_TYPE, order));
}

/** This function is called via the gconf notification callback when
 *  the sort column has changed in gconf.  It must update the model
 *  sort column to match what is specified in gconf.  The sort order
 *  used for this column will be whatever sort order is in use on the
 *  current sort column.  This function is careful to prevent an
 *  infinite loop by blocking the "sort-column-changed" callback that
 *  is attached to the model.
 *
 *  @param widget The tree view.
 *
 *  @param name The name of the column that should be made the sort column.
 *
 *  @internal
 */
static void
gnc_tree_view_set_sort_column (GncTreeView *view,
                               const gchar *name)
{
    GncTreeViewPrivate *priv;
    GtkTreeModel *s_model;
    GtkTreeViewColumn *column;
    GtkSortType order;
    gint model_column, current;

    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
    if (!s_model)
        return;
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    column = gnc_tree_view_find_column_by_name(view, name);

    if (!column)
    {
        g_signal_handler_block(s_model, priv->sort_column_changed_cb_id);
        gtk_tree_sortable_set_sort_column_id(
            GTK_TREE_SORTABLE(s_model), GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID,
            GTK_SORT_ASCENDING);
        g_signal_handler_unblock(s_model, priv->sort_column_changed_cb_id);
        return;
    }

    model_column =
        GPOINTER_TO_INT(g_object_get_data(G_OBJECT(column), MODEL_COLUMN));
    if (model_column == GNC_TREE_VIEW_COLUMN_DATA_NONE)
        return;

    if (!gtk_tree_sortable_get_sort_column_id(GTK_TREE_SORTABLE(s_model),
            &current, &order))
        order = GTK_SORT_ASCENDING;
    g_signal_handler_block(s_model, priv->sort_column_changed_cb_id);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(s_model),
                                         model_column, order);
    g_signal_handler_unblock(s_model, priv->sort_column_changed_cb_id);
    DEBUG("sort column set to %s", name);
}

/** This function is called via the gconf notification callback when
 *  the column order has changed in gconf.  It must update the order
 *  of the columns in the model to match what is specified in gconf.
 *  This function is careful to prevent an infinite loop by blocking
 *  the "columns-changed" callback that is attached to the view.
 *
 *  @param widget The tree view.
 *
 *  @param name A list of pointers to strings.  These strings are the
 *  names of the columns in the order they should appear.
 *
 *  @internal
 */
static void
gnc_tree_view_set_column_order (GncTreeView *view,
                                const GSList *column_names)
{
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column, *prev;
    const gchar *name;
    const GSList *tmp;
    GSList *columns;

    /* First, convert from names to pointers */
    ENTER(" ");
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    columns = NULL;
    for (tmp = column_names; tmp; tmp = g_slist_next(tmp))
    {
        name = gconf_value_get_string(tmp->data);
        column = gnc_tree_view_find_column_by_name(view, name);
        if (!column)
            continue;
        columns = g_slist_append(columns, column);
    }

    /* Then reorder the columns */
    g_signal_handler_block(view, priv->columns_changed_cb_id);
    for (prev = NULL, tmp = columns; tmp; tmp = g_slist_next(tmp))
    {
        column = tmp->data;
        gtk_tree_view_move_column_after(GTK_TREE_VIEW(view), column, prev);
        prev = column;
    }
    g_signal_handler_unblock(view, priv->columns_changed_cb_id);

    /* Clean up */
    g_slist_free(columns);
    LEAVE("column order set");
}

/** This function is called via the gconf notification callback when
 *  any change has occurred.  It must update the appropriate aspect of
 *  the model/view to match what is specified in gconf.  Some of the
 *  updates are handled in this function, while others are handled by
 *  other helper functions.
 *
 *  @param client A pointer to the gconf client object from which
 *  detected a change in gconf.  Unused by this function.
 *
 *  @cnxn_id The identifier for the notification that this callback
 *  represents.  Unused since this code only has a single notification
 *  request.
 *
 *  @entry A pointer to the key/value pair in gconf that changed.
 *
 *  @param data The tree view.
 *
 *  @internal
 */
static void
gnc_tree_view_gconf_changed (GConfClient *client,
                             guint cnxn_id,
                             GConfEntry *entry,
                             gpointer data)
{
    GncTreeView *view;
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column;
    GConfValue *value;
    const gchar *key, *local;
    gchar *column_name, *type_name;
    gboolean known;
    gint width;

    g_return_if_fail(GNC_IS_TREE_VIEW(data));

    ENTER(" ");
    view = GNC_TREE_VIEW(data);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    key = gconf_entry_get_key(entry);
    value = gconf_entry_get_value(entry);
    if (!value)
    {
        /* Values can be unset */
        LEAVE("Unset valued for %s", key);
        return;
    }

    DEBUG("Key %s, value %p", key, value);
    local = strrchr(key, '/') + 1;
    if (strcmp(local, GCONF_KEY_SORT_COLUMN) == 0)
    {
        gnc_tree_view_set_sort_column(view, gconf_value_get_string(value));
    }
    else if (strcmp(local, GCONF_KEY_SORT_ORDER) == 0)
    {
        gnc_tree_view_set_sort_order(view, gconf_value_get_string(value));
    }
    else if (strcmp(local, GCONF_KEY_COLUMN_ORDER) == 0)
    {
        gnc_tree_view_set_column_order(view, gconf_value_get_list(value));
    }
    else
    {
        /* Make a copy of the local part of the key so it can be split
         * into column name and key type */
        known = FALSE;
        column_name = strdup(local);
        type_name = strrchr(column_name, '_');
        *type_name++ = '\0';

        if (strcmp(type_name, GCONF_KEY_VISIBLE) == 0)
        {
            priv->seen_gconf_visibility = TRUE;
            column = gnc_tree_view_find_column_by_name(view, column_name);
            if (column)
            {
                known = TRUE;
                if (!g_object_get_data(G_OBJECT(column), ALWAYS_VISIBLE))
                {
                    gtk_tree_view_column_set_visible(column, gconf_value_get_bool(value));
                }
            }
        }
        else if (strcmp(type_name, GCONF_KEY_WIDTH) == 0)
        {
            width = gconf_value_get_int(value);
            column = gnc_tree_view_find_column_by_name(view, column_name);
            if (column)
            {
                known = TRUE;
                if (width && (width != gtk_tree_view_column_get_width(column)))
                {
                    gtk_tree_view_column_set_fixed_width(column, width);
                }
            }
        }
        if (!known)
        {
            DEBUG("Ignored key %s", key);
        }
        g_free(column_name);
    }
    LEAVE(" ");
}

/** This function is called force synchronization between the gconf
 *  database and a tree view.  It will be called for major events like
 *  assigning a gconf section to a tree view.  All values in gconf
 *  will be propagated into the view.  This function simply requests a
 *  list of all key in the section from gconf, then feeds them one at
 *  a time to gnc_tree_view_gconf_changed().
 *
 *  @param view The tree view.
 *
 *  @internal
 */
static void
gnc_tree_view_gconf_force_update (GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GSList *all_entries, *etmp;
    GList *columns;

    ENTER("view %p", view);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    all_entries = gnc_gconf_client_all_entries(priv->gconf_section);

    /* Set a flag indicating that the gconf data section may be empty.
     * It will be checked later on and appropriate action taken if its
     * still set. */
    priv->seen_gconf_visibility = FALSE;

    /* Pull all the entries from gconf */
    for (etmp = all_entries; etmp; etmp = g_slist_next(etmp))
    {
        gnc_tree_view_gconf_changed(NULL, 0, etmp->data, view);
        gconf_entry_free(etmp->data);
    }
    g_slist_free(all_entries);

    /* No visibilities seen yet.  Write out any columns we may have */
    if (!priv->seen_gconf_visibility)
    {
        columns = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
        g_list_foreach(columns, (GFunc)gnc_tree_view_update_visibility, view);
        g_list_free(columns);
    }

    LEAVE(" ");
}

/** This internal function is called to disconnect a tree view from a
 *  gconf section.  It first disconnects any signals that are in
 *  effect on the model/view, then removes the gconf notification
 *  callback, then clears the gconf section string.  This function may
 *  be called at any time; either when the user wants to disconnect or
 *  when the view object is being destroyed.
 *
 *  @param view The tree view.
 *
 *  @internal
 */
static void
gnc_tree_view_remove_gconf(GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GtkTreeModel *model;

    ENTER(" ");
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (!priv->gconf_section)
    {
        LEAVE("no gconf section");
        return;
    }

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->sort_column_changed_cb_id)
    {
        model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
        if (model)
        {
            DEBUG("removing sort_column_changed callback (# %ld)",
                  priv->sort_column_changed_cb_id);
            g_signal_handler_disconnect(GTK_TREE_SORTABLE(model),
                                        priv->sort_column_changed_cb_id);
            priv->sort_column_changed_cb_id = 0;
        }
    }

    if (priv->columns_changed_cb_id)
    {
        DEBUG("removing columns_changed callback (# %ld)",
              priv->columns_changed_cb_id);
        g_signal_handler_disconnect(view, priv->columns_changed_cb_id);
        priv->columns_changed_cb_id = 0;
    }

    if (priv->size_allocate_cb_id)
    {
        DEBUG("removing size_allocate callback (# %ld)",
              priv->size_allocate_cb_id);
        g_signal_handler_disconnect(view, priv->size_allocate_cb_id);
        priv->size_allocate_cb_id = 0;
    }

    DEBUG("removing gconf notification");
    gnc_gconf_remove_notification(G_OBJECT(view), priv->gconf_section,
                                  GNC_TREE_VIEW_NAME);
    g_free(priv->gconf_section);
    priv->gconf_section = NULL;
    LEAVE(" ");
}

/** This function is called to set up or remove an association between
 *  a gconf section and the display of a view.  It will first remove
 *  any existing association, and then install the new one.  This
 *  involves storing the gconf section value, requesting notification
 *  from gconf of any changes to keys in that section, then attaching
 *  several signals to catch user changes to the view.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
void
gnc_tree_view_set_gconf_section (GncTreeView *view,
                                 const gchar *section)
{
    GncTreeViewPrivate *priv;
    GtkTreeModel *model;
    gulong id;

    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    ENTER("view %p, section %s", view, section);
    gnc_tree_view_remove_gconf(view);

    if (!section)
    {
        LEAVE("cleared gconf section");
        return;
    }

    /* Catch changes in gconf. Propagate to view. */
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    priv->gconf_section = g_strdup(section);
    gnc_gconf_add_notification(G_OBJECT(view), section,
                               gnc_tree_view_gconf_changed,
                               GNC_TREE_VIEW_NAME);

    /* Catch changes to the sort column. Propagate to gconf. The key can
     * be set before the model, so the code must handle that case. */
    model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
    if (model)
        priv->sort_column_changed_cb_id =
            g_signal_connect(GTK_TREE_SORTABLE(model), "sort-column-changed",
                             (GCallback)gtk_tree_view_sort_column_changed_cb, view);

    /* Catch changes to the column order. Propagate to gconf */
    id = g_signal_connect(view, "columns-changed",
                          (GCallback)gtk_tree_view_columns_changed_cb, NULL);
    priv->columns_changed_cb_id = id;

    /* Catch changes to the column width. Propagate to gconf */
    id = g_signal_connect(view, "size-allocate",
                          (GCallback)gtk_tree_view_size_allocate_cb, NULL);
    priv->size_allocate_cb_id = id;

    /* Force an update of the view with all items from gconf. */
    gnc_tree_view_gconf_force_update(view);

    /* Rebuild the column visibility menu */
    gnc_tree_view_build_column_menu(view);
    LEAVE("set gconf section");
}

/** This function is called to get the current association between a
 *  gconf section and the display of a view.  It returns the same
 *  value passed to gnc_tree_view_set_gconf_section(); i.e. a string
 *  like "dialogs/edit_prices".
 *
 *  Parameters are defined in gnc-tree-view.h
 */
const gchar *
gnc_tree_view_get_gconf_section (GncTreeView *view)
{
    GncTreeViewPrivate *priv;

    g_return_val_if_fail(GNC_IS_TREE_VIEW(view), NULL);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    return(priv->gconf_section);
}


/** @} */

/************************************************************/
/*                   Column Selection Menu                  */
/************************************************************/

/** @name Gnc Tree View Column Selection Menu Related Functions
 @{ */

/** This function is called when an entry in the column selection menu
 *  is toggled.  It must update gconf with the current value of the
 *  checkbox.  This will trigger a callback to other functions which
 *  will then change the actual state of the view.
 *
 *  @param checkmenuitem A checkbox visible in the menu.
 *
 *  @param view The tree view.
 *
 *  @internal
 */
static void
gnc_tree_view_menu_item_toggled (GtkCheckMenuItem *checkmenuitem,
                                 GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    gboolean value;
    gchar *key;

    g_return_if_fail(GTK_IS_CHECK_MENU_ITEM(checkmenuitem));
    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    ENTER("checkmenuitem %p, view %p", checkmenuitem, view);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (!priv->gconf_section)
    {
        LEAVE("no gconf section");
        return;
    }

    key = g_object_get_data(G_OBJECT(checkmenuitem), GCONF_KEY);
    value = gtk_check_menu_item_get_active(checkmenuitem);
    gnc_gconf_set_bool(priv->gconf_section, key, value, NULL);
    LEAVE("set gconf section %s, key %s, visible %d",
          priv->gconf_section, key, value);
}

/** This function is called to create a single checkmenuitem in the
 *  column selection menu.  It is called once for each column in the
 *  view.  It creates a menu item for the corresponding column, and
 *  attaches to it a copy of the gconf key for this column's
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
    gulong id;
    gchar *key;

    // ENTER("view %p, column %p", view, column);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (!priv->gconf_section)
    {
        // LEAVE("no gconf section");
        return;
    }

    pref_name = g_object_get_data(G_OBJECT(column), PREF_NAME);
    if (!pref_name)
    {
        // LEAVE("column has no pref_name");
        return;
    }

    /* Create the menu if we don't have one already */
    if (!priv->column_menu)
    {
        priv->column_menu = gtk_menu_new();
        g_object_ref_sink(priv->column_menu);
    }

    /* Create the check menu item */
    column_name = g_object_get_data(G_OBJECT(column), REAL_TITLE);
    if (!column_name)
        column_name = gtk_tree_view_column_get_title(column);
    widget = gtk_check_menu_item_new_with_label(column_name);
    gtk_menu_shell_append(GTK_MENU_SHELL(priv->column_menu), widget);

    /* Should never be able to hide the first column */
    if (g_object_get_data(G_OBJECT(column), ALWAYS_VISIBLE))
    {
        g_object_set_data(G_OBJECT(widget), ALWAYS_VISIBLE, GINT_TO_POINTER(1));
        gtk_widget_set_sensitive(widget, FALSE);
    }

    /* Set up the callback  */
    id = g_signal_connect(widget, "toggled",
                          (GCallback)gnc_tree_view_menu_item_toggled, view);

    /* Store data on the widget for callbacks */
    key = g_strdup_printf("%s_%s", pref_name, GCONF_KEY_VISIBLE);
    g_object_set_data_full(G_OBJECT(widget), GCONF_KEY, key, g_free);
    // LEAVE(" ");
}

/** This function is called to build the column selection menu.  It
 *  first destroys any old column selection menu, then checks to see
 *  if a new menu should be built.  If so, it calls the
 *  gnc_tree_view_create_menu_item() for each column in the view.
 *  This function is invoked then either the "gconf-section" or the
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

    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    ENTER("view %p", view);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);

    /* Destroy any old menu */
    if (priv->column_menu)
    {
        gtk_widget_unref(priv->column_menu);
        priv->column_menu = NULL;
    }

    if (priv->show_column_menu && priv->gconf_section)
    {
        /* Show the menu popup button */
        if (priv->column_menu_column)
            gtk_tree_view_column_set_visible(priv->column_menu_column, TRUE);

        /* Now build a new menu */
        column_list = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
        g_list_foreach(column_list, (GFunc)gnc_tree_view_create_menu_item, view);
        g_list_free(column_list);
    }
    else
    {
        /* Hide the menu popup button */
        if (priv->column_menu_column)
            gtk_tree_view_column_set_visible(priv->column_menu_column, FALSE);
    }
    LEAVE("menu: show %d, section %s", priv->show_column_menu,
          priv->gconf_section ? priv->gconf_section : "(null)");
}

/** This function is called to synchronize the checkbox on a menu item
 *  with the current gconf visibility value for the corresponding
 *  column (which will be the visibility of the column if the rest of
 *  the code in this file is working correctly).  It simply takes the
 *  gconf key attached to the menu item, reads it, and sets the menu
 *  item to the retrieved value.  It does take care to block signals
 *  from the menu item to prevent updating gconf (since the value was
 *  just read from gconf).
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
    GncTreeViewPrivate *priv;
    gchar *key;
    gboolean visible;

    g_return_if_fail(GTK_IS_CHECK_MENU_ITEM(checkmenuitem));
    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    key = g_object_get_data(G_OBJECT(checkmenuitem), GCONF_KEY);
    if (g_object_get_data(G_OBJECT(checkmenuitem), ALWAYS_VISIBLE))
    {
        visible = TRUE;
    }
    else
    {
        visible = gnc_gconf_get_bool(priv->gconf_section, key, NULL);
    }

    g_signal_handlers_block_by_func(checkmenuitem,
                                    gnc_tree_view_menu_item_toggled, view);
    gtk_check_menu_item_set_active(checkmenuitem, visible);
    g_signal_handlers_unblock_by_func(checkmenuitem,
                                      gnc_tree_view_menu_item_toggled, view);
}

/** This function when the user clicks on the button to show the
 *  column selection menu.  It first synchronize the checkboxes on all
 *  menu item with the gconf visibility values.  It then pops up the
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
    GtkWidget *widget, *menu;

    g_return_if_fail(GTK_IS_TREE_VIEW_COLUMN(column));
    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    menu = priv->column_menu;
    if (!menu)
        return;

    /* Synchronize the menu before display */
    gtk_container_foreach(GTK_CONTAINER(menu),
                          (GtkCallback)gnc_tree_view_update_column_menu_item,
                          view);

    /* Ensure all components are visible */
    gtk_widget_show_all(menu);

    /* Pop the menu up at the button */
    widget = gtk_tree_view_column_get_widget(column);
    gtk_menu_popup(GTK_MENU(priv->column_menu), NULL, GTK_WIDGET(menu),
                   NULL, NULL, 0, gtk_get_current_event_time());
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

    g_return_if_fail (GNC_IS_TREE_VIEW (view));
    ENTER(" ");
    va_start (args, first_column_name);
    priv = GNC_TREE_VIEW_GET_PRIVATE (view);
    name = first_column_name;
    hide_spacer = FALSE;

    /* First disable the expand property on all (non-infrastructure) columns. */
    columns = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
    for (tmp = columns; tmp; tmp = g_list_next(tmp))
    {
        column = tmp->data;
        pref_name = g_object_get_data(G_OBJECT(column), PREF_NAME);
        if (pref_name != NULL)
            gtk_tree_view_column_set_expand(column, FALSE);
    }
    g_list_free(columns);

    /* Now enable it on the requested columns. */
    while (name != NULL)
    {
        column = gnc_tree_view_find_column_by_name(view, name);
        if (column != NULL)
        {
            gtk_tree_view_column_set_expand(column, TRUE);
            hide_spacer = TRUE;
        }
        name = va_arg (args, gchar*);
    }
    va_end (args);

    gtk_tree_view_column_set_visible (priv->spacer_column, !hide_spacer);
    gtk_tree_view_column_set_visible (priv->selection_column, !hide_spacer);

    LEAVE(" ");
}

/** This function is called to set the "show-column-menu" property on
 *  this view.  This function has no visible effect if the
 *  "gconf-section" property has not been set.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
void
gnc_tree_view_set_show_column_menu (GncTreeView *view,
                                    gboolean visible)
{
    GncTreeViewPrivate *priv;

    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    ENTER("view %p, show menu %d", view, visible);
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    priv->show_column_menu = visible;
    gnc_tree_view_build_column_menu(view);
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

    g_return_val_if_fail(GNC_IS_TREE_VIEW(view), FALSE);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    return(priv->show_column_menu);
}

/** @} */

/************************************************************/
/*                    Tree View Creation                    */
/************************************************************/

/** Attach a data model to a visible GncTreeView widget.  Users of
 *  this view object must use this function instead of directly
 *  calling the gtk_tree_view_set_model function.  This function takes
 *  the additional step of attaching a callback function to the model
 *  to catch any changes to the sorting of the model.  These changes
 *  are propagated into gconf by the callback function.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
void
gnc_tree_view_set_model(GncTreeView *view, GtkTreeModel *model)
{
    GncTreeViewPrivate *priv;
    GtkTreeModel *old_model;

    /* Remove existing callback */
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->sort_column_changed_cb_id)
    {
        old_model = gtk_tree_view_get_model (GTK_TREE_VIEW(view));
        g_signal_handler_disconnect (old_model, priv->sort_column_changed_cb_id);
        priv->sort_column_changed_cb_id = 0;
    }

    gtk_tree_view_set_model (GTK_TREE_VIEW(view), model);

    /* Maybe add a new callback */
    if (model && priv->gconf_section)
    {
        priv->sort_column_changed_cb_id =
            g_signal_connect(GTK_TREE_SORTABLE(model), "sort-column-changed",
                             (GCallback)gtk_tree_view_sort_column_changed_cb, view);
    }
}

static gint
gnc_tree_view_count_visible_columns(GncTreeView *view)
{
    GList *columns, *node;
    gint count = 0;

    columns = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
    for (node = columns; node; node = node->next)
    {
        GtkTreeViewColumn *col = GTK_TREE_VIEW_COLUMN(node->data);

        if (g_object_get_data(G_OBJECT(col), DEFAULT_VISIBLE) ||
                g_object_get_data(G_OBJECT(col), ALWAYS_VISIBLE))
            count++;
    }
    g_list_free(columns);
    return count;
}

void
gnc_tree_view_configure_columns (GncTreeView *view)
{
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column;
    GList *columns;
    gboolean hide_spacer;

    g_return_if_fail(GNC_IS_TREE_VIEW(view));

    ENTER(" ");

    /* Update the view and gconf */
    columns = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
    g_list_foreach(columns, (GFunc)gnc_tree_view_update_visibility, view);
    g_list_free(columns);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->gconf_section)
        priv->seen_gconf_visibility = TRUE;

    /* If only the first column is visible, hide the spacer and make that
     * column expand. */
    hide_spacer = (gnc_tree_view_count_visible_columns(view) == 1);
    column = gtk_tree_view_get_column(GTK_TREE_VIEW(view), 0);
    gtk_tree_view_column_set_expand(column, hide_spacer);
    gtk_tree_view_column_set_visible(priv->spacer_column, !hide_spacer);
    gtk_tree_view_column_set_visible(priv->selection_column, !hide_spacer);

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
 *  visibility in gconf.
 *
 *  @param data_column The index of the GtkTreeModel data column used
 *  to determine the data that will be displayed in this column for
 *  each row in the view.  Use GNC_TREE_VIEW_COLUMN_DATA_NONE if you
 *  plan on using an non-model data source for this column.
 *
 *  @param default_width The width this column should be if not
 *  specified by gconf.  If the this value is zero, the column will
 *  be marked as automatically sized.
 *
 *  @param resizable Whether to mark the column as user resizable.
 *  This marking is only relevant for fixed width columns.
 *
 *  @param column_sort_function The function that GtkTreeModelSort
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
    gchar *key;

    /* Set data used by other functions */
    if (pref_name)
        g_object_set_data(G_OBJECT(column), PREF_NAME, (gpointer)pref_name);
    if (data_column == 0)
        g_object_set_data(G_OBJECT(column), ALWAYS_VISIBLE, GINT_TO_POINTER(1));
    g_object_set_data(G_OBJECT(column), MODEL_COLUMN,
                      GINT_TO_POINTER(data_column));

    /* Get visibility */
    visible = gnc_tree_view_column_visible(view, NULL, pref_name);

    /* Get width */
    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    if (priv->gconf_section)
    {
        key = g_strdup_printf("%s_%s", pref_name, GCONF_KEY_WIDTH);
        width = gnc_gconf_get_int(priv->gconf_section, key, NULL);
        g_free(key);
    }

    /* If gconf comes back with a width of zero (or there is no gconf
     * width) the use the default width for the column.  Allow for
     * padding L and R of the displayed data. */
    if (width == 0)
        width = default_width + 10;
    if (width == 0)
        width = 10;

    /* Set column attributes */
    g_object_set(G_OBJECT(column),
                 "visible",     visible,
                 "sizing",      GTK_TREE_VIEW_COLUMN_FIXED,
                 "fixed-width", width,
                 "resizable",   resizable && pref_name != NULL,
                 "reorderable", pref_name != NULL,
                 NULL);

    s_model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
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

    /* Add to the column selection menu */
    if (pref_name)
    {
        gnc_tree_view_create_menu_item(column, view);
    }
}

/** This function adds a new toggle column to a GncTreeView base view.
 *  It takes all the parameters necessary to hook a GtkTreeModel
 *  column to a GtkTreeViewColumn.  It handles creating a tooltip to
 *  show the full title name, and setting the sort and edit callback
 *  functions.  If the tree has a gconf section associated with it,
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
    GncTreeViewPrivate *priv;
    GtkTreeViewColumn *column;
    GtkCellRenderer *renderer;
    PangoLayout* layout;
    int title_width;

    g_return_val_if_fail (GNC_IS_TREE_VIEW(view), NULL);

    priv = GNC_TREE_VIEW_GET_PRIVATE(view);
    renderer = gtk_cell_renderer_toggle_new ();
    column =
        gtk_tree_view_column_new_with_attributes (column_short_title,
                renderer,
                "active", model_data_column,
                NULL);

    /* Add the full title to the object for menu creation */
    g_object_set_data_full(G_OBJECT(column), REAL_TITLE,
                           g_strdup(column_title), g_free);
    if (toggle_edited_cb)
        g_signal_connect(G_OBJECT(renderer), "toggled",
                         G_CALLBACK(toggle_edited_cb), view);

    if (model_visibility_column != GNC_TREE_VIEW_COLUMN_VISIBLE_ALWAYS)
        gtk_tree_view_column_add_attribute (column, renderer,
                                            "visible", model_visibility_column);

    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view),
             column_short_title);
    pango_layout_get_pixel_size(layout, &title_width, NULL);
    g_object_unref(layout);

    gnc_tree_view_column_properties (view, column, pref_name, model_data_column,
                                     title_width, FALSE, column_sort_fn);

    gnc_tree_view_append_column (view, column);

    /* Also add the full title to the object as a tooltip */
    if (!priv->title_tips)
        priv->title_tips = gtk_tooltips_new();
    gtk_tooltips_set_tip(priv->title_tips, column->button, column_title, NULL);

    return column;
}

/** This function adds a new text column to a GncTreeView base view.
 *  It takes all the parameters necessary to hook a GtkTreeModel
 *  column to a GtkTreeViewColumn.  If the tree has a gconf section
 *  associated with it, this function also wires up the column so that
 *  its visibility and width are remembered.
 *
 *  Parameters are defined in gnc-tree-view.h
 */
GtkTreeViewColumn *
gnc_tree_view_add_text_column (GncTreeView *view,
                               const gchar *column_title,
                               const gchar *pref_name,
                               const gchar *stock_icon_name,
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
    if (stock_icon_name)
    {
        renderer = gtk_cell_renderer_pixbuf_new ();
        g_object_set (renderer, "stock-id", stock_icon_name, NULL);
        gtk_tree_view_column_pack_start (column, renderer, FALSE);
    }

    /* Set up a text renderer and attributes */
    renderer = gtk_cell_renderer_text_new ();
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
    pango_layout_get_pixel_size(layout, &title_width, NULL);
    g_object_unref(layout);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), sizing_text);
    pango_layout_get_pixel_size(layout, &default_width, NULL);
    g_object_unref(layout);
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
    pango_layout_get_pixel_size(layout, &title_width, NULL);
    g_object_unref(layout);
    layout = gtk_widget_create_pango_layout (GTK_WIDGET(view), sizing_text);
    pango_layout_get_pixel_size(layout, &default_width, NULL);
    g_object_unref(layout);
    default_width = MAX(default_width, title_width);
    if (default_width)
        default_width += 10; /* padding on either side */

    gnc_tree_view_column_properties (view, column, pref_name, model_data_column,
                                     default_width, TRUE, column_sort_fn);

    /* Stuff specific to combo */
    if (combo_tree_model)
    {
        g_object_set(G_OBJECT(renderer), "model", combo_tree_model,
                     "text-column", combo_model_text_column, NULL);
    }
    /* TODO: has-entry? */

    gnc_tree_view_append_column (view, column);
    return column;
}

GtkCellRenderer *
gnc_tree_view_column_get_renderer(GtkTreeViewColumn *column)
{
    GList *renderers;
    GtkCellRenderer *cr = NULL;

    g_return_val_if_fail(GTK_TREE_VIEW_COLUMN(column), NULL);

    /* Get the list of one renderer */
    renderers = gtk_tree_view_column_get_cell_renderers(column);
    if (g_list_length(renderers) > 0)
        cr = GTK_CELL_RENDERER(renderers->data);
    g_list_free(renderers);

    return cr;
}

/** This function adds a new numeric column to a GncTreeView base
 *  view.  It takes all the parameters necessary to hook a
 *  GtkTreeModel column to a GtkTreeViewColumn.  If the tree has a
 *  gconf section associated with it, this function also wires up the
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

    renderer = gnc_tree_view_column_get_renderer(column);

    /* Right align the column title and data */
    g_object_set(G_OBJECT(column), "alignment",   1.0, NULL);
    g_object_set(G_OBJECT(renderer), "xalign",   1.0, NULL);

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
    columns = gtk_tree_view_get_columns(GTK_TREE_VIEW(view));
    n = g_list_length(columns);
    g_list_free(columns);

    /* Ignore the initial two columns (the spacer and the selection menu) */
    if (n >= 2)
        n -= 2;
    return gtk_tree_view_insert_column (GTK_TREE_VIEW(view), column, n);
}

static gboolean
get_column_next_to(GtkTreeView *tv, GtkTreeViewColumn **col, gboolean backward)
{
    GList *cols, *node;
    GtkTreeViewColumn *c = NULL;
    gint seen = 0;
    gboolean wrapped = FALSE;

    cols = gtk_tree_view_get_columns(tv);
    g_return_val_if_fail(g_list_length(cols) > 0, FALSE);

    node = g_list_find(cols, *col);
    g_return_val_if_fail(node, FALSE);
    do
    {
        node = backward ? node->prev : node->next;
        if (!node)
        {
            wrapped = TRUE;
            node = backward ? g_list_last(cols) : cols;
        }
        c = GTK_TREE_VIEW_COLUMN(node->data);
        if (c && gtk_tree_view_column_get_visible(c))
            seen++;
        if (c == *col) break;
    }
    while (!seen);

    g_list_free(cols);
    *col = c;
    return wrapped;
}

gboolean
gnc_tree_view_path_is_valid(GncTreeView *view, GtkTreePath *path)
{
    GtkTreeView *tv = GTK_TREE_VIEW(view);
    GtkTreeModel *s_model;
    GtkTreeIter iter;

    s_model = gtk_tree_view_get_model(tv);
    return gtk_tree_model_get_iter(s_model, &iter, path);
}

void
gnc_tree_view_keynav(GncTreeView *view, GtkTreeViewColumn **col,
                     GtkTreePath *path, GdkEventKey *event)
{
    GtkTreeView *tv = GTK_TREE_VIEW(view);
    gint depth;
    gboolean shifted;

    if (event->type != GDK_KEY_PRESS) return;

    switch (event->keyval)
    {
    case GDK_Tab:
    case GDK_ISO_Left_Tab:
    case GDK_KP_Tab:
        shifted = event->state & GDK_SHIFT_MASK;
        if (get_column_next_to(tv, col, shifted))
        {
            /* This is the end (or beginning) of the line, buddy. */
            depth = gtk_tree_path_get_depth(path);
            if (shifted)
            {
                if (!gtk_tree_path_prev(path) && depth > 1)
                {
                    gtk_tree_path_up(path);
                }
            }
            else if (gtk_tree_view_row_expanded(tv, path))
            {
                gtk_tree_path_down(path);
            }
            else
            {
                gtk_tree_path_next(path);
                if (!gnc_tree_view_path_is_valid(view, path) && depth > 1)
                {
                    gtk_tree_path_prev(path);
                    gtk_tree_path_up(path);
                    gtk_tree_path_next(path);
                }
            }
        }
        break;

    case GDK_Return:
    case GDK_KP_Enter:
        if (gtk_tree_view_row_expanded(tv, path))
        {
            gtk_tree_path_down(path);
        }
        else
        {
            depth = gtk_tree_path_get_depth(path);
            gtk_tree_path_next(path);
            if (!gnc_tree_view_path_is_valid(view, path) && depth > 1)
            {
                gtk_tree_path_prev(path);
                gtk_tree_path_up(path);
                gtk_tree_path_next(path);
            }
        }
        break;
    }
    return;
}

/** @} */
/** @} */
