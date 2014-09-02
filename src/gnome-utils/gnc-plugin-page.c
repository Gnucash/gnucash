/*
 * gnc-plugin_page.c --
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
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

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup ContentPluginBase Common object and functions
    @{ */
/** @file gnc-plugin-page.c
    @brief Functions for adding plugins to a GnuCash window.
    @author Copyright (C) 2003 Jan Arne Petersen
    @author Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#include "gnc-engine.h"
#include "gnc-plugin.h"
#include "gnc-plugin-page.h"
#include "gnc-gobject-utils.h"

/** The debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;
/** A pointer to the parent class of a plugin page. */
static gpointer parent_class = NULL;

static void gnc_plugin_page_class_init (GncPluginPageClass *klass);
static void gnc_plugin_page_init       (GncPluginPage *plugin_page,
                                        GncPluginPageClass *klass);
static void gnc_plugin_page_finalize   (GObject *object);
static void gnc_plugin_page_set_property (GObject         *object,
        guint            prop_id,
        const GValue    *value,
        GParamSpec      *pspec);
static void gnc_plugin_page_get_property (GObject         *object,
        guint            prop_id,
        GValue          *value,
        GParamSpec      *pspec);

enum
{
    INSERTED,
    REMOVED,
    SELECTED,
    UNSELECTED,
    LAST_SIGNAL
};

enum
{
    PROP_0,
    PROP_PAGE_NAME,
    PROP_PAGE_COLOR,
    PROP_PAGE_URI,
    PROP_BOOK,
    PROP_STATUSBAR_TEXT,
    PROP_USE_NEW_WINDOW,
    PROP_UI_DESCRIPTION,
    PROP_UI_MERGE,
    PROP_ACTION_GROUP,
};

static guint signals[LAST_SIGNAL] = { 0 };


/** The instance private data for a content plugin. */
typedef struct _GncPluginPagePrivate
{
    /** The group of all actions provided by this plugin. */
    GtkActionGroup *action_group;
    GtkUIManager *ui_merge;
    guint merge_id;
    char *ui_description;

    GList *books;

    gboolean use_new_window;

    gchar *page_name;
    gchar *page_long_name;
    gchar *page_color;
    gchar *uri;
    gchar *statusbar_text;
} GncPluginPagePrivate;

#define GNC_PLUGIN_PAGE_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE, GncPluginPagePrivate))

GType
gnc_plugin_page_get_type (void)
{
    static GType gnc_plugin_page_type = 0;

    if (gnc_plugin_page_type == 0)
    {
        static const GTypeInfo our_info =
        {

            sizeof (GncPluginPageClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_plugin_page_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPluginPage),
            0,		/* n_preallocs */
            (GInstanceInitFunc) gnc_plugin_page_init,
        };

        gnc_plugin_page_type = g_type_register_static (G_TYPE_OBJECT,
                               "GncPluginPage",
                               &our_info, 0);
    }

    return gnc_plugin_page_type;
}


/*  Create the display widget that corresponds to this plugin.  This
 *  function will be called by the main/embedded window manipulation
 *  code to create a widget that they can display.  The returned
 *  widget should encompass all information that goes with this page,
 *  including scroll bars, a summary bar, etc. */
GtkWidget *
gnc_plugin_page_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageClass *klass;
    GtkWidget *widget;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);

    klass = GNC_PLUGIN_PAGE_GET_CLASS (plugin_page);
    g_return_val_if_fail (klass != NULL, NULL);
    g_return_val_if_fail (klass->create_widget != NULL, NULL);

    widget = klass->create_widget (plugin_page);

    /*
     * If there is a destroy function, add a ref so that the
     * widgets will exists when the destroy function is called.
     * Otherwise it will be destroyed when it is removed from the
     * main notebook for the window.
     */
    if (klass->destroy_widget)
        g_object_ref(widget);

    return widget;
}


/*  Destroy the display widget that corresponds to this plugin.  This
 *  function will be called by the main/embedded window manipulation
 *  code when a page is closed. */
void
gnc_plugin_page_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageClass *klass;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

    klass = GNC_PLUGIN_PAGE_GET_CLASS (plugin_page);
    g_return_if_fail (klass != NULL);
    g_return_if_fail (klass->destroy_widget != NULL);

    klass->destroy_widget (plugin_page);
}


/*  Show/hide the summarybar associated with this page. */
void
gnc_plugin_page_show_summarybar (GncPluginPage *page,
                                 gboolean visible)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));

    if (!page->summarybar)
        return;

    if (visible)
    {
        gtk_widget_show(page->summarybar);
    }
    else
    {
        gtk_widget_hide(page->summarybar);
    }
}


/*  Call the plugin specific function that will save the state of a
 *  content page to a disk.  That function must save enough
 *  information about the page that it can be recreated next time the
 *  user starts gnucash. */
void
gnc_plugin_page_save_page (GncPluginPage *page,
                           GKeyFile *key_file,
                           const gchar *group_name)
{
    GncPluginPageClass *klass;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER(" ");
    klass = GNC_PLUGIN_PAGE_GET_CLASS (page);
    g_return_if_fail (klass != NULL);
    g_return_if_fail (klass->save_page != NULL);

    klass->save_page(page, key_file, group_name);
    LEAVE(" ");
}


/*  This function looks up a specific plugin type by name, and then
 *  calls a plugin specific function to create a new page and restore
 *  its content to a previous state. */
GncPluginPage *
gnc_plugin_page_recreate_page(GtkWidget *window,
                              const gchar *page_type,
                              GKeyFile *key_file,
                              const gchar *page_group)
{
    GncPluginPageClass *klass;
    GncPluginPage *page = NULL;
    GType type;

    ENTER("type %s, keyfile %p, group %s", page_type, key_file, page_group);
    type = g_type_from_name(page_type);
    if (type == 0)
    {
        LEAVE("Cannot find type named %s", page_type);
        return NULL;
    }

    klass = g_type_class_ref(type);
    if (klass == NULL)
    {
        const gchar *type_name = g_type_name(type);
        LEAVE("Cannot create class %s(%s)", page_type, type_name ? type_name : "invalid type");
        return NULL;
    }

    if (!klass->recreate_page)
    {
        LEAVE("Class %shas no recreate function.", page_type);
        g_type_class_unref(klass);
        return NULL;
    }

    page = (klass->recreate_page)(window, key_file, page_group);
    g_type_class_unref(klass);
    LEAVE(" ");
    return page;
}


/*  Add the actions for a content page to the specified window. */
void
gnc_plugin_page_merge_actions (GncPluginPage *page,
                               GtkUIManager *ui_merge)
{
    GncPluginPagePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE(page));

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    priv->ui_merge = ui_merge;
    priv->merge_id = gnc_plugin_add_actions(priv->ui_merge,
                                            priv->action_group,
                                            priv->ui_description);
}


/*  Remove the actions for a content page from the specified window. */
void
gnc_plugin_page_unmerge_actions (GncPluginPage *page,
                                 GtkUIManager *ui_merge)
{
    GncPluginPagePrivate *priv;

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));
    g_return_if_fail (priv->merge_id != 0);
    g_return_if_fail (priv->action_group != NULL);

    gtk_ui_manager_remove_ui(ui_merge, priv->merge_id);
    gtk_ui_manager_remove_action_group(ui_merge, priv->action_group);

    priv->ui_merge = NULL;
    priv->merge_id = 0;
}


GtkAction *
gnc_plugin_page_get_action (GncPluginPage *page, const gchar *name)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), NULL);
    g_return_val_if_fail(name != NULL, NULL);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    if (!priv->action_group)
        return NULL;
    return gtk_action_group_get_action (priv->action_group, name);
}


/*  Retrieve the textual name of a plugin. */
const gchar *
gnc_plugin_page_get_plugin_name (GncPluginPage *plugin_page)
{
    GncPluginPageClass *klass;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page), NULL);

    klass = GNC_PLUGIN_PAGE_GET_CLASS (plugin_page);
    g_return_val_if_fail (klass != NULL, NULL);

    return (klass->plugin_name);
}


/* Signals */
void
gnc_plugin_page_inserted (GncPluginPage *plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

    g_signal_emit (G_OBJECT (plugin_page), signals[INSERTED], 0);
}

void
gnc_plugin_page_removed (GncPluginPage *plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

    g_signal_emit (G_OBJECT (plugin_page), signals[REMOVED], 0);
}

void
gnc_plugin_page_selected (GncPluginPage *plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

    g_signal_emit (G_OBJECT (plugin_page), signals[SELECTED], 0);
}

void
gnc_plugin_page_unselected (GncPluginPage *plugin_page)
{
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (plugin_page));

    g_signal_emit (G_OBJECT (plugin_page), signals[UNSELECTED], 0);
}


/** Initialize the class for a new generic plugin page.  This will set
 *  up any function pointers that override functions in the parent
 *  class, set up all properties and signals, and also configure the
 *  private data storage for this widget.
 *
 *  @param klass The new class structure created by the object system.
 */
static void
gnc_plugin_page_class_init (GncPluginPageClass *klass)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);
    gobject_class->finalize = gnc_plugin_page_finalize;
    gobject_class->set_property = gnc_plugin_page_set_property;
    gobject_class->get_property = gnc_plugin_page_get_property;

    klass->tab_icon    = NULL;
    klass->plugin_name = NULL;

    g_type_class_add_private(klass, sizeof(GncPluginPagePrivate));

    g_object_class_install_property
    (gobject_class,
     PROP_PAGE_NAME,
     g_param_spec_string ("page-name",
                          "Page Name",
                          "The name of this page.  This value is "
                          "used to generate the notebook tab and "
                          "menu items, and also the window title "
                          "when this page is visible.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_PAGE_COLOR,
     g_param_spec_string ("page-color",
                          "Page Color",
                          "The color of this page.  This value is "
                          "used to generate the notebook tab color "
                          "when this page is visible.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_PAGE_URI,
     g_param_spec_string ("page-uri",
                          "Page URI",
                          "The uri for this page.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_STATUSBAR_TEXT,
     g_param_spec_string ("statusbar-text",
                          "Statusbar Text",
                          "The text to be displayed in the statusbar "
                          "at the bottom of the window when this page "
                          "is visible.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_USE_NEW_WINDOW,
     g_param_spec_boolean ("use-new-window",
                           "Use New Window",
                           "When TRUE a new top level window will be "
                           "created to hold this page.",
                           FALSE,
                           G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_UI_DESCRIPTION,
     g_param_spec_string ("ui-description",
                          "UI Description File",
                          "The filename containing the XML data that "
                          "describes this pages menus and toolbars.",
                          NULL,
                          G_PARAM_READWRITE));

    g_object_class_install_property
    (gobject_class,
     PROP_UI_MERGE,
     g_param_spec_object ("ui-merge",
                          "UI Merge",
                          "A pointer to the GtkUIManager object that "
                          "represents this pages menu hierarchy.",
                          GTK_TYPE_UI_MANAGER,
                          G_PARAM_READABLE));

    g_object_class_install_property
    (gobject_class,
     PROP_ACTION_GROUP,
     g_param_spec_object ("action-group",
                          "Action Group",
                          "A pointer to the GtkActionGroup object that "
                          "represents this pages available menu/toolbar "
                          "actions.",
                          GTK_TYPE_ACTION_GROUP,
                          G_PARAM_READABLE));




    signals[INSERTED] = g_signal_new ("inserted",
                                      G_OBJECT_CLASS_TYPE (klass),
                                      G_SIGNAL_RUN_FIRST,
                                      G_STRUCT_OFFSET (GncPluginPageClass, inserted),
                                      NULL, NULL,
                                      g_cclosure_marshal_VOID__VOID,
                                      G_TYPE_NONE,
                                      0);
    signals[REMOVED] = g_signal_new ("removed",
                                     G_OBJECT_CLASS_TYPE (klass),
                                     G_SIGNAL_RUN_FIRST,
                                     G_STRUCT_OFFSET (GncPluginPageClass, removed),
                                     NULL, NULL,
                                     g_cclosure_marshal_VOID__VOID,
                                     G_TYPE_NONE,
                                     0);
    signals[SELECTED] = g_signal_new ("selected",
                                      G_OBJECT_CLASS_TYPE (klass),
                                      G_SIGNAL_RUN_FIRST,
                                      G_STRUCT_OFFSET (GncPluginPageClass, selected),
                                      NULL, NULL,
                                      g_cclosure_marshal_VOID__VOID,
                                      G_TYPE_NONE,
                                      0);
    signals[UNSELECTED] = g_signal_new ("unselected",
                                        G_OBJECT_CLASS_TYPE (klass),
                                        G_SIGNAL_RUN_FIRST,
                                        G_STRUCT_OFFSET (GncPluginPageClass, unselected),
                                        NULL, NULL,
                                        g_cclosure_marshal_VOID__VOID,
                                        G_TYPE_NONE,
                                        0);
}


/** Initialize a new instance of a gnucash content plugin.  This
 *  function initializes the object private storage space, and adds
 *  the object to the tracking system.
 *
 *  @param page The new object instance created by the object system.
 *
 *  @param klass A pointer to the class data structure for this
 *  object. */
static void
gnc_plugin_page_init (GncPluginPage *page, GncPluginPageClass *klass)
{
    GncPluginPagePrivate *priv;

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    priv->page_name   = NULL;
    priv->page_color  = NULL;
    priv->uri         = NULL;

    page->window      = NULL;
    page->summarybar  = NULL;

    gnc_gobject_tracking_remember(G_OBJECT(page), G_OBJECT_CLASS(klass));
}


/** Finalize the gnucash plugin object.  This function is called from
 *  the G_Object level to complete the destruction of the object.  It
 *  should release any memory not previously released by the destroy
 *  function (i.e. the private data structure), then chain up to the
 *  parent's destroy function.
 *
 *  @param object The object being destroyed. */
static void
gnc_plugin_page_finalize (GObject *object)
{
    GncPluginPagePrivate *priv;
    GncPluginPage *page;

    page = GNC_PLUGIN_PAGE (object);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    if (priv->page_name)
        g_free(priv->page_name);
    if (priv->page_color)
        g_free(priv->page_color);
    if (priv->uri)
        g_free(priv->uri);
    if (priv->statusbar_text)
        g_free(priv->statusbar_text);

    if (priv->books)
    {
        g_list_free(priv->books);
        priv->books = NULL;
    }

    page->window = NULL; // Don't need to free it.

    gnc_gobject_tracking_forget(object);
    G_OBJECT_CLASS (parent_class)->finalize (object);
}

/************************************************************/
/*                g_object other functions                  */
/************************************************************/


/** Retrieve a property specific to this GncPluginPage object.  This is
 *  nothing more than a dispatch function for routines that can be
 *  called directly.  It has the nice feature of allowing a single
 *  function call to retrieve multiple properties.
 *
 *  @param object The object whose property should be retrieved.
 *
 *  @param prop_id The numeric identifier of the property.  This
 *  should be a PROP_XXX constant as specified at the beginning of
 *  this file.
 *
 *  @param value A pointer to where this property value should be
 *  stored.
 *
 *  @param pspec A pointer to the meta data that described the property
 *  being retrieved. */
/* Note that g_value_set_object() refs the object, as does
 * g_object_get(). But g_object_get() only unrefs once when it disgorges
 * the object, leaving an unbalanced ref, which leaks. So instead of
 * using g_value_set_object(), use g_value_take_object() which doesn't
 * ref the object when used in get_property().
 */
static void
gnc_plugin_page_get_property (GObject     *object,
                              guint        prop_id,
                              GValue      *value,
                              GParamSpec  *pspec)
{
    GncPluginPage *page;
    GncPluginPagePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE(object));

    page = GNC_PLUGIN_PAGE(object);
    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    switch (prop_id)
    {
    case PROP_PAGE_NAME:
        g_value_set_string (value, priv->page_name);
        break;
    case PROP_PAGE_COLOR:
        g_value_set_string (value, priv->page_color);
        break;
    case PROP_PAGE_URI:
        g_value_set_string (value, priv->uri);
        break;
    case PROP_STATUSBAR_TEXT:
        g_value_set_string (value, priv->statusbar_text);
        break;
    case PROP_USE_NEW_WINDOW:
        g_value_set_boolean (value, priv->use_new_window);
        break;
    case PROP_UI_DESCRIPTION:
        g_value_set_string (value, priv->ui_description);
        break;
    case PROP_UI_MERGE:
        g_value_take_object (value, priv->ui_merge);
        break;
    case PROP_ACTION_GROUP:
        g_value_take_object (value, priv->action_group);
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}


/** Set a property specific to this GncPluginPage object.  This is
 *  nothing more than a dispatch function for routines that can be
 *  called directly.  It has the nice feature of allowing a new view
 *  to be created with a varargs list specifying the properties,
 *  instead of having to explicitly call each property function.
 *
 *  @param object The object whose property should be set.
 *
 *  @param prop_id The numeric identifier of the property.  This
 *  should be a PROP_XXX constant as specified at the beginning of
 *  this file.
 *
 *  @param value A pointer to then new value for this property value.
 *
 *  @param pspec A pointer to the meta data that described the property
 *  being retrieved. */
static void
gnc_plugin_page_set_property (GObject      *object,
                              guint         prop_id,
                              const GValue *value,
                              GParamSpec   *pspec)
{
    GncPluginPage *page;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE(object));

    page = GNC_PLUGIN_PAGE(object);

    switch (prop_id)
    {
    case PROP_PAGE_NAME:
        gnc_plugin_page_set_page_name(page, g_value_get_string(value));
        break;
    case PROP_PAGE_COLOR:
        gnc_plugin_page_set_page_color(page, g_value_get_string(value));
        break;
    case PROP_PAGE_URI:
        gnc_plugin_page_set_uri(page, g_value_get_string(value));
        break;
    case PROP_STATUSBAR_TEXT:
        gnc_plugin_page_set_statusbar_text(page, g_value_get_string(value));
        break;
    case PROP_USE_NEW_WINDOW:
        gnc_plugin_page_set_use_new_window(page, g_value_get_boolean(value));
        break;
    case PROP_UI_DESCRIPTION:
        gnc_plugin_page_set_ui_description(page, g_value_get_string(value));
        break;
    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

/************************************************************/
/*                                                          */
/************************************************************/

/*  Add a book reference to the specified page. */
void
gnc_plugin_page_add_book (GncPluginPage *page, QofBook *book)
{
    GncPluginPagePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));
    g_return_if_fail (book != NULL);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    priv->books = g_list_append(priv->books, book);
}


/*  Query a page to see if it has a reference to a given book. */
gboolean
gnc_plugin_page_has_book (GncPluginPage *page, QofBook *book)
{
    GncPluginPagePrivate *priv;
    GList *item;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), FALSE);
    g_return_val_if_fail (book != NULL, FALSE);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    for (item = priv->books; item; item = g_list_next(item))
    {
        if (item->data == book)
        {
            return TRUE;
        }
    }
    return FALSE;
}


/*  Query a page to see if it has a reference to any book. */
gboolean
gnc_plugin_page_has_books (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), FALSE);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return (priv->books != NULL);
}


/*  Retrieve a pointer to the GncMainWindow (GtkWindow) containing
 *  this page. */
GtkWidget *
gnc_plugin_page_get_window (GncPluginPage *page)
{
    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), NULL);

    return page->window;
}


/*  Retrieve the name of this page.  This is the string used in the
 *  window title, and in the notebook tab and page selection menus. */
const gchar *
gnc_plugin_page_get_page_name (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), NULL);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->page_name;
}


/*  Set the name of this page.  This is the string used in the window
 *  title, and in the notebook tab and page selection menus. */
void
gnc_plugin_page_set_page_name (GncPluginPage *page, const gchar *name)
{
    GncPluginPagePrivate *priv;
    GncPluginPageClass *klass;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    if (priv->page_name)
        g_free(priv->page_name);
    priv->page_name = g_strdup(name);

    /* Perform page specific actions */
    klass = GNC_PLUGIN_PAGE_GET_CLASS (page);
    if (klass->page_name_changed)
    {
        klass->page_name_changed(page, name);
    }
}


/*  Retrieve the long name of this page.  This is the string used in
 *  the tooltip that is attached to the page name in the notebook
 *  tab. */
const gchar *
gnc_plugin_page_get_page_long_name (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), NULL);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->page_long_name;
}


/*  Set the long name of this page.  This is the string used in the
 *  tooltip that is attached to the page name in the notebook tab. */
void
gnc_plugin_page_set_page_long_name (GncPluginPage *page, const gchar *name)
{
    GncPluginPagePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    if (priv->page_long_name)
        g_free(priv->page_long_name);
    priv->page_long_name = g_strdup(name);
}


/*  Get the color of this page.  This is the string used in the notebook tab. */
const gchar *
gnc_plugin_page_get_page_color (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), NULL);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->page_color;
}


/*  Set the color of this page.  This is the string used in the notebook tab. */
void
gnc_plugin_page_set_page_color (GncPluginPage *page, const gchar *color)
{
    GncPluginPagePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    if (priv->page_color)
        g_free(priv->page_color);
    if (color)
        priv->page_color = g_strdup(color);
}


/*  Retrieve the Uniform Resource Identifier for this page. */
const gchar *
gnc_plugin_page_get_uri (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), NULL);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->uri;
}


/*  Set the Uniform Resource Identifier for this page. */
void
gnc_plugin_page_set_uri (GncPluginPage *page, const gchar *name)
{
    GncPluginPagePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    if (priv->uri)
        g_free(priv->uri);
    priv->uri = g_strdup(name);
}


/*  Retrieve the statusbar text associated with this page. */
const gchar *
gnc_plugin_page_get_statusbar_text (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), NULL);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->statusbar_text;
}


/*  Set the statusbar text associated with this page. */
void
gnc_plugin_page_set_statusbar_text (GncPluginPage *page, const gchar *message)
{
    GncPluginPagePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    if (priv->statusbar_text)
        g_free(priv->statusbar_text);
    priv->statusbar_text = g_strdup(message);
}


/*  Retrieve the "use new window" setting associated with this page. */
gboolean
gnc_plugin_page_get_use_new_window (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), FALSE);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->use_new_window;
}


/*  Set the "use new window" setting associated with this page.  If
 *  this setting is TRUE, the page will be installed into a new
 *  window.  Otherwise the page will be installed into an existing
 *  window. */
void
gnc_plugin_page_set_use_new_window (GncPluginPage *page, gboolean use_new)
{
    GncPluginPagePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    priv->use_new_window = use_new;
}


/*  Retrieve the name of the XML UI file associated with this page. */
const gchar *
gnc_plugin_page_get_ui_description (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), FALSE);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->ui_description;
}


/*  Set an alternate UI for the specified page.  This alternate ui
 *  may only use actions specified in the source for the page. */
void
gnc_plugin_page_set_ui_description (GncPluginPage *page,
                                    const char *ui_filename)
{
    GncPluginPagePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE(page));

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    if (priv->ui_description)
        g_free(priv->ui_description);
    priv->ui_description = g_strdup(ui_filename);
}


/*  Retrieve the GtkUIManager object associated with this page. */
GtkUIManager *
gnc_plugin_page_get_ui_merge (GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), NULL);

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->ui_merge;
}


/*  Retrieve the GtkActionGroup object associated with this page. */
GtkActionGroup *
gnc_plugin_page_get_action_group(GncPluginPage *page)
{
    GncPluginPagePrivate *priv;

    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE (page), NULL);
    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    return priv->action_group;
}


/*  Create the GtkActionGroup object associated with this page. */
GtkActionGroup *
gnc_plugin_page_create_action_group (GncPluginPage *page, const gchar *group_name)
{
    GncPluginPagePrivate *priv;
    GtkActionGroup *group;

    priv = GNC_PLUGIN_PAGE_GET_PRIVATE(page);
    group = gtk_action_group_new(group_name);
    gnc_gtk_action_group_set_translation_domain(group, GETTEXT_PACKAGE);
    priv->action_group = group;
    return group;
}

gboolean
gnc_plugin_page_finish_pending (GncPluginPage *page)
{
    if (!page)
        return TRUE;
    if (!GNC_IS_PLUGIN_PAGE(page))
        return TRUE;

    if (!GNC_PLUGIN_PAGE_GET_CLASS(page)->finish_pending)
        return TRUE;
    return (GNC_PLUGIN_PAGE_GET_CLASS(page)->finish_pending)(page);
}

/** @} */
/** @} */
