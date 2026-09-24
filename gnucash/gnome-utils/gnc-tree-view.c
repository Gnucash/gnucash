/*
 * Copyright (C) 2003,2005 David Hampton <hampton@employees.org>
 * Author: David Hampton <hampton@employees.org>
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
#include "gnc-tree-view.h"
#include "gnc-prefs.h"

#define GNC_PREFS_GROUP_GENERAL "general"
#define GNC_PREF_GRID_LINES_HORIZONTAL "grid-lines-horizontal"
#define GNC_PREF_GRID_LINES_VERTICAL "grid-lines-vertical"
typedef struct
{
    GtkColumnView *column_view;
    gchar *state_section;
    gboolean show_column_menu;
} GncTreeViewPrivate;
enum
{
    PROP_0, PROP_STATE_SECTION, PROP_SHOW_COLUMN_MENU, N_PROPERTIES
};
static GParamSpec *properties[N_PROPERTIES];
G_DEFINE_TYPE_WITH_PRIVATE (GncTreeView, gnc_tree_view, GTK_TYPE_BOX)

static void
update_grid_lines (gpointer prefs, gchar *pref_name, gpointer user_data)
{
    GtkColumnView *view = GTK_COLUMN_VIEW (user_data);

    gtk_column_view_set_show_row_separators
        (view, gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                   GNC_PREF_GRID_LINES_HORIZONTAL));
    gtk_column_view_set_show_column_separators
        (view, gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL,
                                   GNC_PREF_GRID_LINES_VERTICAL));
    (void)prefs;
    (void)pref_name;
}

static void
remove_grid_line_preferences (GtkColumnView *view)
{
    /* This helper is also used from a weak-notify callback. At that point the
     * pointer is only an identity key for the preferences backend and must not
     * be validated or dereferenced. */
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_GRID_LINES_HORIZONTAL,
                                 update_grid_lines, view);
    gnc_prefs_remove_cb_by_func (GNC_PREFS_GROUP_GENERAL,
                                 GNC_PREF_GRID_LINES_VERTICAL,
                                 update_grid_lines, view);
}

static GQuark
grid_line_preferences_quark (void)
{
    return g_quark_from_static_string ("gnc-column-view-grid-line-preferences");
}

static void
grid_line_preferences_destroyed (gpointer user_data, GObject *where_the_object_was)
{
    /* A weak-notify pointer is only valid as the callback user-data key. */
    remove_grid_line_preferences ((GtkColumnView *)where_the_object_was);
    (void)user_data;
}

void
gnc_column_view_unbind_grid_line_preferences (GtkColumnView *view)
{
    g_return_if_fail (GTK_IS_COLUMN_VIEW (view));

    remove_grid_line_preferences (view);
    if (g_object_get_qdata (G_OBJECT (view), grid_line_preferences_quark ()))
    {
        g_object_set_qdata (G_OBJECT (view), grid_line_preferences_quark (), NULL);
        g_object_weak_unref (G_OBJECT (view), grid_line_preferences_destroyed, NULL);
    }
}

void
gnc_column_view_bind_grid_line_preferences (GtkColumnView *view)
{
    g_return_if_fail (GTK_IS_COLUMN_VIEW (view));

    /* Make repeated construction/setup safe and apply the current setting now. */
    gnc_column_view_unbind_grid_line_preferences (view);
    update_grid_lines (NULL, NULL, view);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_GRID_LINES_HORIZONTAL,
                           update_grid_lines, view);
    gnc_prefs_register_cb (GNC_PREFS_GROUP_GENERAL,
                           GNC_PREF_GRID_LINES_VERTICAL,
                           update_grid_lines, view);
    if (!g_object_get_qdata (G_OBJECT (view), grid_line_preferences_quark ()))
    {
        g_object_set_qdata (G_OBJECT (view), grid_line_preferences_quark (),
                            GINT_TO_POINTER (TRUE));
        g_object_weak_ref (G_OBJECT (view), grid_line_preferences_destroyed, NULL);
    }
}

static void
get_property (GObject *object, guint id, GValue *value, GParamSpec *pspec)
{
    GncTreeViewPrivate *p = gnc_tree_view_get_instance_private (GNC_TREE_VIEW (object));
    if (id == PROP_STATE_SECTION) g_value_set_string (value, p->state_section);
    else if (id == PROP_SHOW_COLUMN_MENU) g_value_set_boolean (value, p->show_column_menu);
    else G_OBJECT_WARN_INVALID_PROPERTY_ID (object, id, pspec);
}
static void
set_property (GObject *object, guint id, const GValue *value, GParamSpec *pspec)
{
    if (id == PROP_STATE_SECTION) gnc_tree_view_set_state_section (GNC_TREE_VIEW (object), g_value_get_string (value));
    else if (id == PROP_SHOW_COLUMN_MENU) gnc_tree_view_set_show_column_menu (GNC_TREE_VIEW (object), g_value_get_boolean (value));
    else G_OBJECT_WARN_INVALID_PROPERTY_ID (object, id, pspec);
}
static void
dispose (GObject *object)
{
    GncTreeViewPrivate *p = gnc_tree_view_get_instance_private (GNC_TREE_VIEW (object));
    if (p->column_view)
        gnc_column_view_unbind_grid_line_preferences (p->column_view);
    g_clear_pointer (&p->state_section, g_free);
    /* gtk_box_append() owns this child; the GtkBox dispose path unparents it. */
    p->column_view = NULL;
    G_OBJECT_CLASS (gnc_tree_view_parent_class)->dispose (object);
}
static void
gnc_tree_view_class_init (GncTreeViewClass *klass)
{
    GObjectClass *c = G_OBJECT_CLASS (klass);
    c->get_property = get_property;
    c->set_property = set_property;
    c->dispose = dispose;
    properties[PROP_STATE_SECTION] = g_param_spec_string ("state-section", "State section", "Preference section used to persist this view", NULL, G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
    properties[PROP_SHOW_COLUMN_MENU] = g_param_spec_boolean ("show-column-menu", "Show column menu", "Expose the GTK4 column chooser affordance", FALSE, G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);
    g_object_class_install_properties (c, N_PROPERTIES, properties);
}
static void
gnc_tree_view_init (GncTreeView *view)
{
    GncTreeViewPrivate *p = gnc_tree_view_get_instance_private (view);
    gtk_orientable_set_orientation (GTK_ORIENTABLE (view), GTK_ORIENTATION_VERTICAL);
    p->column_view = GTK_COLUMN_VIEW (gtk_column_view_new (NULL));
    gnc_column_view_bind_grid_line_preferences (p->column_view);
    gtk_column_view_set_reorderable (p->column_view, TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET (p->column_view), TRUE);
    gtk_widget_set_vexpand (GTK_WIDGET (p->column_view), TRUE);
    gtk_box_append (GTK_BOX (view), GTK_WIDGET (p->column_view));
}
GtkColumnView *
gnc_tree_view_get_column_view (GncTreeView *view)
{
    GncTreeViewPrivate *p;
    g_return_val_if_fail (GNC_IS_TREE_VIEW (view), NULL);
    p = gnc_tree_view_get_instance_private (view);
    return p->column_view;
}
void
gnc_tree_view_set_state_section (GncTreeView *view, const gchar *section)
{
    GncTreeViewPrivate *p;
    g_return_if_fail (GNC_IS_TREE_VIEW (view));
    p = gnc_tree_view_get_instance_private (view);
    if (g_strcmp0 (p->state_section, section) == 0) return;
    g_free (p->state_section);
    p->state_section = g_strdup (section);
    g_object_notify_by_pspec (G_OBJECT (view), properties[PROP_STATE_SECTION]);
}
const gchar *
gnc_tree_view_get_state_section (GncTreeView *view)
{
    GncTreeViewPrivate *p;
    g_return_val_if_fail (GNC_IS_TREE_VIEW (view), NULL);
    p = gnc_tree_view_get_instance_private (view);
    return p->state_section;
}
void
gnc_tree_view_set_show_column_menu (GncTreeView *view, gboolean visible)
{
    GncTreeViewPrivate *p;
    g_return_if_fail (GNC_IS_TREE_VIEW (view));
    p = gnc_tree_view_get_instance_private (view);
    visible = !!visible;
    if (p->show_column_menu == visible) return;
    p->show_column_menu = visible;
    g_object_notify_by_pspec (G_OBJECT (view), properties[PROP_SHOW_COLUMN_MENU]);
}
gboolean
gnc_tree_view_get_show_column_menu (GncTreeView *view)
{
    GncTreeViewPrivate *p;
    g_return_val_if_fail (GNC_IS_TREE_VIEW (view), FALSE);
    p = gnc_tree_view_get_instance_private (view);
    return p->show_column_menu;
}
