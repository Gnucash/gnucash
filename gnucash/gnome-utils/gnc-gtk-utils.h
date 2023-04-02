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

void gnc_cbwe_set_by_string(GtkComboBox *cbwe, const gchar *text);
void gnc_cbwe_add_completion (GtkComboBox *cbwe);
void gnc_cbwe_require_list_item (GtkComboBox *cbwe);

gboolean gnc_is_dark_theme (GdkRGBA *fg_color);
void gnc_style_context_get_background_color (GtkStyleContext *context,
                                             GtkStateFlags    state,
                                             GdkRGBA         *color);
void gnc_style_context_get_border_color (GtkStyleContext *context,
                                         GtkStateFlags    state,
                                         GdkRGBA         *color);

GtkWidget *gnc_get_dialog_widget_from_id (GtkDialog *dialog, const gchar *id);

void gnc_disable_all_actions_in_group (GSimpleActionGroup *action_group);

void gnc_add_accelerator_keys_for_menu (GtkWidget *menu, GMenuModel *model, GtkAccelGroup *accel_group);

GtkWidget *gnc_find_menu_item_by_action_name (GtkWidget *menu, const gchar *action_name);
GtkWidget *gnc_find_menu_item_by_action_label (GtkWidget *menu, const gchar *action_label);
GList *gnc_menu_get_items (GtkWidget *menu);

GtkWidget *gnc_find_toolbar_item (GtkWidget *toolbar, const gchar *action_name);

void gnc_menu_item_setup_tooltip_to_statusbar_callback (GtkWidget *menu_item,
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
GtkWidget *gnc_menubar_model_find_menu_item (GMenuModel *menu_model, GtkWidget *menu, const gchar *action_name);

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
