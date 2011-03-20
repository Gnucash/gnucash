/*
 * gnc-main-window.c -- GtkWindow which represents the
 *	GnuCash main window.
 *
 * Copyright (C) 2003 Jan Arne Petersen <jpetersen@uni-bonn.de>
 * Copyright (C) 2003 David Hampton <hampton@employees.org>
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

#include "gnc-embedded-window.h"

#include "gnc-engine.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-gui-query.h"
#include "gnc-plugin.h"
#include "gnc-plugin-manager.h"
#include "gnc-ui.h"
#include "gnc-window.h"

/* Static Globals *******************************************************/

/** The debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;
/** A pointer to the parent class of an embedded window. */
static GObjectClass *parent_class = NULL;


/* Declarations *********************************************************/
static void gnc_embedded_window_class_init (GncEmbeddedWindowClass *klass);
static void gnc_embedded_window_init (GncEmbeddedWindow *window, GncEmbeddedWindowClass *klass);
static void gnc_embedded_window_finalize (GObject *object);
static void gnc_embedded_window_dispose (GObject *object);

static void gnc_window_embedded_window_init (GncWindowIface *iface);

static void gnc_embedded_window_setup_window (GncEmbeddedWindow *window);


/** The instance private data for an embedded window object. */
typedef struct GncEmbeddedWindowPrivate
{
    /** The dock (vbox) at the top of the window containing the menubar
     *  and toolbar.  These items are generated bu the UI manager and
     *  stored here when the UI manager provides them to the main
     *  window. */
    GtkWidget *menu_dock;
    /* The toolbar created by the UI manager.  This pointer provides
     * easy access for showing/hiding the toolbar. */
    GtkWidget *toolbar;
    /** A pointer to the status bar at the bottom edge of the window.
     *  This pointer provides easy access for updating/showing/hiding
     *  the status bar. */
    GtkWidget *statusbar;

    /** The group of all actions provided by the main window itself.
     *  This does not include any action provided by menu or content
     *  plugins. */
    GtkActionGroup *action_group;

    /** The currently selected page. */
    GncPluginPage *page;
    /** The parent of this embedded "window".  This points to a real
     *  GtkWindow widget. */
    GtkWidget *parent_window;
} GncEmbeddedWindowPrivate;

#define GNC_EMBEDDED_WINDOW_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_EMBEDDED_WINDOW, GncEmbeddedWindowPrivate))



/*  Get the type of a gnc embedded window. */
GType
gnc_embedded_window_get_type (void)
{
    static GType gnc_embedded_window_type = 0;

    if (gnc_embedded_window_type == 0)
    {
        static const GTypeInfo our_info =
        {
            sizeof (GncEmbeddedWindowClass),
            NULL,
            NULL,
            (GClassInitFunc) gnc_embedded_window_class_init,
            NULL,
            NULL,
            sizeof (GncEmbeddedWindow),
            0,
            (GInstanceInitFunc) gnc_embedded_window_init
        };

        static const GInterfaceInfo plugin_info =
        {
            (GInterfaceInitFunc) gnc_window_embedded_window_init,
            NULL,
            NULL
        };

        gnc_embedded_window_type = g_type_register_static (GTK_TYPE_VBOX,
                                   "GncEmbeddedWindow",
                                   &our_info, 0);
        g_type_add_interface_static (gnc_embedded_window_type,
                                     GNC_TYPE_WINDOW,
                                     &plugin_info);
    }

    return gnc_embedded_window_type;
}


/*  Display a data plugin page in a window. */
void
gnc_embedded_window_open_page (GncEmbeddedWindow *window,
                               GncPluginPage *page)
{
    GncEmbeddedWindowPrivate *priv;

    g_return_if_fail (GNC_IS_EMBEDDED_WINDOW (window));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);
    g_return_if_fail (priv->page == NULL);

    ENTER("window %p, page %p", window, page);
    priv->page = page;
    page->window = GTK_WIDGET(window);
    page->notebook_page = gnc_plugin_page_create_widget (page);

    gtk_box_pack_end(GTK_BOX(window), page->notebook_page, TRUE, TRUE, 2);
    gnc_plugin_page_inserted (page);

    gnc_plugin_page_merge_actions (page, window->ui_merge);
    LEAVE(" ");
}


/*  Remove a data plugin page from a window. */
void
gnc_embedded_window_close_page (GncEmbeddedWindow *window,
                                GncPluginPage *page)
{
    GncEmbeddedWindowPrivate *priv;

    g_return_if_fail (GNC_IS_EMBEDDED_WINDOW (window));
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);
    g_return_if_fail (priv->page == page);

    ENTER("window %p, page %p", window, page);

    if (!page->notebook_page)
    {
        LEAVE("no displayed widget");
        return;
    }

    gtk_container_remove (GTK_CONTAINER(window), GTK_WIDGET(page->notebook_page));
    priv->page = NULL;
    gnc_plugin_page_removed (page);

    gnc_plugin_page_unmerge_actions (page, window->ui_merge);
    gtk_ui_manager_ensure_update (window->ui_merge);

    gnc_plugin_page_destroy_widget (page);
    g_object_unref(page);
    LEAVE(" ");
}


/*  Retrieve the plugin that is embedded in the specified window. */
GncPluginPage *
gnc_embedded_window_get_page (GncEmbeddedWindow *window)
{
    GncEmbeddedWindowPrivate *priv;

    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);
    return priv->page;
}


/** Initialize the class for a new gnucash embedded window.  This will
 *  set up any function pointers that override functions in the parent
 *  class.
 *
 *  @param klass The new class structure created by the object system.
 */
static void
gnc_embedded_window_class_init (GncEmbeddedWindowClass *klass)
{
    GObjectClass *object_class;
    ENTER("klass %p", klass);
    object_class = G_OBJECT_CLASS (klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_embedded_window_finalize;
    object_class->dispose = gnc_embedded_window_dispose;

    g_type_class_add_private(klass, sizeof(GncEmbeddedWindowPrivate));
    LEAVE(" ");
}


/** Initialize a new instance of a gnucash embedded window.  This
 *  function initializes the object private storage space.  It also
 *  adds the new object to a list (for memory tracking purposes).
 *
 *  @param view The new object instance created by the object system.
 *
 *  @param klass A pointer to the class data structure for this
 *  object. */
static void
gnc_embedded_window_init (GncEmbeddedWindow *window,
                          GncEmbeddedWindowClass *klass)
{
    ENTER("window %p", window);

    gnc_embedded_window_setup_window (window);

    gnc_gobject_tracking_remember(G_OBJECT(window),
                                  G_OBJECT_CLASS(klass));
    LEAVE(" ");
}


/** Finish destruction of an embedded window.
 *
 *  @object The window being destroyed. */
static void
gnc_embedded_window_finalize (GObject *object)
{
    GncEmbeddedWindow *window;
    GncEmbeddedWindowPrivate *priv;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_EMBEDDED_WINDOW (object));

    ENTER("object %p", object);
    window = GNC_EMBEDDED_WINDOW (object);
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);

    gnc_gobject_tracking_forget(object);
    G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}


/** Begin destruction of an embedded window.  This function should
 *  release all objects referenced by the window (i.e. the page).
 *
 *  @object The window being destroyed. */
static void
gnc_embedded_window_dispose (GObject *object)
{
    GncEmbeddedWindow *window;
    GncEmbeddedWindowPrivate *priv;

    g_return_if_fail (object != NULL);
    g_return_if_fail (GNC_IS_EMBEDDED_WINDOW (object));

    ENTER("object %p", object);
    window = GNC_EMBEDDED_WINDOW (object);
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);
    if (priv->page)
    {
        DEBUG("unreffing page %p (count currently %d)", priv->page,
              G_OBJECT(priv->page)->ref_count);
        g_object_unref(priv->page);
        priv->page = NULL;
    }

    G_OBJECT_CLASS (parent_class)->dispose (object);
    LEAVE(" ");
}


static void
gnc_embedded_window_add_widget (GtkUIManager *merge,
                                GtkWidget *widget,
                                GncEmbeddedWindow *window)
{
    GncEmbeddedWindowPrivate *priv;

    ENTER("merge %p, new widget %p, window %p", merge, widget, window);
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);
    if (GTK_IS_TOOLBAR (widget))
    {
        priv->toolbar = widget;
    }

    gtk_box_pack_start (GTK_BOX (priv->menu_dock), widget, FALSE, FALSE, 0);
    gtk_widget_show (widget);
    LEAVE(" ");
}


/** Initialize the data structures of a gnucash embedded window.
 *
 *  @param window The object to initialize. */
static void
gnc_embedded_window_setup_window (GncEmbeddedWindow *window)
{
    GncEmbeddedWindowPrivate *priv;

    ENTER("window %p", window);
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);

    /* Create widgets and add them to the window */
    gtk_widget_show (GTK_WIDGET(window));

    priv->menu_dock = gtk_vbox_new (FALSE, 0);
    gtk_widget_show (priv->menu_dock);
    gtk_box_pack_start (GTK_BOX (window), priv->menu_dock, TRUE, TRUE, 0);

    priv->statusbar = gtk_statusbar_new ();
    gtk_statusbar_set_has_resize_grip (GTK_STATUSBAR(priv->statusbar), FALSE);
    gtk_widget_show (priv->statusbar);
    gtk_box_pack_end (GTK_BOX (window), priv->statusbar, FALSE, TRUE, 0);

    window->ui_merge = gtk_ui_manager_new ();
    g_signal_connect (G_OBJECT (window->ui_merge), "add_widget",
                      G_CALLBACK (gnc_embedded_window_add_widget), window);

    priv->action_group = NULL;
    LEAVE(" ");
}


/** Create a new gnc embedded window plugin. */
GncEmbeddedWindow *
gnc_embedded_window_new (const gchar *action_group_name,
                         GtkActionEntry *action_entries,
                         gint n_action_entries,
                         const gchar *ui_filename,
                         GtkWidget *enclosing_win,
                         gboolean add_accelerators,
                         gpointer user_data)
{
    GncEmbeddedWindowPrivate *priv;
    GncEmbeddedWindow *window;
    gchar *ui_fullname;
    GError *error = NULL;
    guint merge_id;

    ENTER("group %s, first %p, num %d, ui file %s, parent %p, add accelerators %d, user data %p",
          action_group_name, action_entries, n_action_entries, ui_filename,
          enclosing_win, add_accelerators, user_data);
    window = g_object_new (GNC_TYPE_EMBEDDED_WINDOW, NULL);
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);

    /* Determine the full pathname of the ui file */
    ui_fullname = gnc_gnome_locate_ui_file(ui_filename);

    priv->parent_window = enclosing_win;

    /* Create menu and toolbar information */
    priv->action_group = gtk_action_group_new (action_group_name);
    gnc_gtk_action_group_set_translation_domain(priv->action_group, GETTEXT_PACKAGE);
    gtk_action_group_add_actions (priv->action_group, action_entries,
                                  n_action_entries, user_data);
    gtk_ui_manager_insert_action_group (window->ui_merge, priv->action_group, 0);
    merge_id = gtk_ui_manager_add_ui_from_file (window->ui_merge, ui_fullname,
               &error);

    /* Error checking */
    g_assert(merge_id || error);
    if (error)
    {
        g_critical("Failed to load ui file.\n  Filename %s\n  Error %s",
                   ui_fullname, error->message);
        g_error_free(error);
        g_free(ui_fullname);
        LEAVE("window %p", window);
        return window;
    }

    /* Add accelerators (if wanted) */
    if (add_accelerators)
        gtk_window_add_accel_group (GTK_WINDOW(enclosing_win),
                                    gtk_ui_manager_get_accel_group(window->ui_merge));

    gtk_ui_manager_ensure_update (window->ui_merge);
    g_free(ui_fullname);
    LEAVE("window %p", window);
    return window;
}


/** Retrieve the gtk window associated with an embedded window object.
 *  This function is called via a vector off a generic window
 *  interface.
 *
 *  @param window_in A pointer to a generic window. */
static GtkWindow *
gnc_embedded_window_get_gtk_window (GncWindow *window_in)
{
    GncEmbeddedWindow *window;
    GncEmbeddedWindowPrivate *priv;

    g_return_val_if_fail (GNC_IS_EMBEDDED_WINDOW (window_in), NULL);

    window = GNC_EMBEDDED_WINDOW(window_in);
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);
    return GTK_WINDOW(priv->parent_window);
}


/** Retrieve the status bar associated with an embedded window object.
 *  This function is called via a vector off a generic window
 *  interface.
 *
 *  @param window_in A pointer to a generic window. */
static GtkWidget *
gnc_embedded_window_get_statusbar (GncWindow *window_in)
{
    GncEmbeddedWindowPrivate *priv;
    GncEmbeddedWindow *window;

    g_return_val_if_fail (GNC_IS_EMBEDDED_WINDOW (window_in), NULL);

    window = GNC_EMBEDDED_WINDOW(window_in);
    priv = GNC_EMBEDDED_WINDOW_GET_PRIVATE(window);
    return priv->statusbar;
}


/** Initialize the generic window interface for an embedded window.
 *
 *  @param iface A pointer to the interface data structure to
 *  populate. */
static void
gnc_window_embedded_window_init (GncWindowIface *iface)
{
    iface->get_gtk_window = gnc_embedded_window_get_gtk_window;
    iface->get_statusbar = gnc_embedded_window_get_statusbar;
}
