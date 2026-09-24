/********************************************************************\
 * gnc-gtk-utils.h -- utility functions based on glib functions     *
 * Copyright (C) 2006 David Hampton <hampton@employees.org>         *
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
 *                                                                  *
\********************************************************************/

/** @addtogroup Gtk Gtk Utilities

    The API in this file is designed to provide support functions that
    wrap the base gtk functions and make them easier to use.

    @{ */
/** @file gnc-gtk-utils.h
 *  @brief gtk helper routines.
 *  @author Copyright (C) 2006 David Hampton <hampton@employees.org>
 */

#ifndef GNC_GTK_UTILS_H
#define GNC_GTK_UTILS_H

#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif

/** @name gtk Miscellaneous Functions
 @{
*/

#define GNC_MENU_ATTRIBUTE_ACCELERATOR  "accel"
#define GNC_MENU_ATTRIBUTE_TOOLTIP      "tooltip"
#define GNC_MENU_ATTRIBUTE_TEMPORARY    "temp"


/**
 * Append or prepend a child while retaining the GtkBox packing intent from
 * the pre-GTK4 layout.  GTK4 stores expansion and alignment on the child,
 * so callers provide the former GtkBox packing flags explicitly.
 */
void gnc_box_append_full (GtkBox *box, GtkWidget *child, gboolean expand,
                          gboolean fill, guint padding);
void gnc_box_prepend_full (GtkBox *box, GtkWidget *child, gboolean expand,
                           gboolean fill, guint padding);

/** Set the four GTK4 widget margins corresponding to a legacy container border. */
void gnc_widget_set_all_margins (GtkWidget *widget, gint margin);

/**
 * Returns whether a double-click landed on a selectable GtkCalendar day.
 * GtkCalendar exposes day labels through its documented "day-number" CSS
 * class; headers, weekday names and week numbers deliberately do not match.
 */
static inline gboolean
gnc_gtk_calendar_double_clicks_day (GtkCalendar *calendar, gint n_press,
                                    gdouble x, gdouble y)
{
    GtkWidget *target;

    g_return_val_if_fail (GTK_IS_CALENDAR (calendar), FALSE);

    if (n_press != 2)
        return FALSE;

    target = gtk_widget_pick (GTK_WIDGET (calendar), x, y, GTK_PICK_DEFAULT);
    return target && gtk_widget_has_css_class (target, "day-number") &&
           gtk_widget_is_sensitive (target);
}

/**
 * Reserve enough horizontal space for the widest text displayed by a
 * GtkDropDown. The reservation follows the current theme, font and scale;
 * it is refreshed when the drop-down's model, expression or selection
 * changes.
 */
void gnc_gtk_drop_down_normalize_width (GtkDropDown *drop_down);

/** Construct a GtkDropDown that keeps its width normalized. */
GtkDropDown *gnc_gtk_drop_down_new (GListModel *model,
                                    GtkExpression *expression);

/** Construct a string GtkDropDown that keeps its width normalized. */
GtkDropDown *gnc_gtk_drop_down_new_from_strings (const char * const *strings);

/**
 * Bind a newly created top-level window to the running GtkApplication.
 *
 * A transient parent does not replace the application association: GTK needs
 * the latter to include the window in application lifecycle management. The
 * helper is intentionally a no-op outside the GUI process, so utility code
 * remains usable in tests and early startup.
 */
void gnc_window_bind_to_application (GtkWindow *window);

/** Create a GdkTexture from a pixbuf without using deprecated GDK APIs. */
GdkTexture *gnc_texture_new_from_pixbuf (GdkPixbuf *pixbuf);

/* GTK4 status bar replacement retaining GtkStatusbar's message stack semantics. */
GtkWidget *gnc_statusbar_new (void);
gboolean gnc_statusbar_is (GtkWidget *statusbar);
guint gnc_statusbar_push (GtkWidget *statusbar, guint context_id,
                           const gchar *message);
void gnc_statusbar_pop (GtkWidget *statusbar, guint context_id);
void gnc_statusbar_remove (GtkWidget *statusbar, guint context_id,
                           guint message_id);

gboolean gnc_is_dark_theme (GdkRGBA *fg_color);

GtkWidget *gnc_get_widget_from_id (GtkWidget *root, const gchar *id);

void gnc_disable_all_actions_in_group (GSimpleActionGroup *action_group);

/** Load existing legacy accelerator-map entries as action overrides. */
void gnc_accelerator_overrides_load_legacy_map (const gchar *filename);
void gnc_accelerator_overrides_clear (void);
gboolean gnc_accelerator_overrides_lookup (const gchar *action_name,
                                            const gchar **accelerator);

/** Interpret the legacy Primary modifier according to the current platform. */
GtkShortcutTrigger *gnc_accelerator_trigger_parse (const gchar *accelerator);
void gnc_menu_model_apply_accelerators (GMenuModel *model);

void gnc_add_accelerator_keys_for_menu (GtkWidget *menu, GMenuModel *model, GtkEventController *shortcut_controller);

GtkWidget *gnc_find_toolbar_item (GtkWidget *toolbar, const gchar *action_name);

void gnc_menubar_setup_tooltip_to_statusbar_callbacks (GtkWidget *menubar,
                                                   GMenuModel *menu_model,
                                                   GtkWidget *statusbar);

void gnc_tool_item_setup_tooltip_to_statusbar_callback (GtkWidget *tool_item,
                                                        GtkWidget *statusbar);

struct _GncMenuModelSearch
{
    const gchar *search_action_name;
    const gchar *search_action_label;
    const gchar *search_action_target;
    const gchar *tooltip;
    GMenuModel  *model;
    gint         index;
};

typedef struct _GncMenuModelSearch GncMenuModelSearch;

gboolean gnc_menubar_model_find_item (GMenuModel *menu_model, GncMenuModelSearch *gsm);
gboolean gnc_menubar_model_set_item_visible (GMenuModel *menu_model,
                                            const gchar *action_name,
                                            gboolean visible);

gboolean gnc_menubar_model_update_item (GMenuModel *menu_model, const gchar *action_name,
                                        const gchar *target, const gchar *label,
                                        const gchar *accel_name, const gchar *tooltip);

void gnc_menubar_model_remove_items_with_attrib (GMenuModel *menu_model, const gchar *attrib);

/** @} */

#ifdef __cplusplus
}
#endif

#endif /* GNC_GTK_UTILS_H */
/** @} */
