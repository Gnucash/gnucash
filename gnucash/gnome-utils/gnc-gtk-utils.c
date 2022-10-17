/********************************************************************\
 * gnc-gtk-utils.c -- utility functions based on glib functions     *
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

#include <config.h>

#include "gnc-gtk-utils.h"

#define LAST_INDEX "last_index"
#define CHANGED_ID "changed_id"


/** Find an entry in the GtkComboBox by its text value, and set
 *  the widget to that value.  This function also records the index of
 *  that text value for use when the user leaves the widget.
 *
 *  @param cbwe A pointer to a GtkComboBox with entry widget.
 *
 *  @param text The entry text to find in the model of the combo box
 *  entry. */
void
gnc_cbwe_set_by_string(GtkComboBox *cbwe,
                      const gchar *text)
{
    GtkTreeModel *model;
    GtkTreeIter iter;
    gchar *tree_string;
    gint column, index, id;
    gboolean match;

    model = gtk_combo_box_get_model(GTK_COMBO_BOX(cbwe));
    if (!gtk_tree_model_get_iter_first(model, &iter))
    {
        /* empty tree */
        gtk_combo_box_set_active(GTK_COMBO_BOX(cbwe), -1);
        return;
    }

    column = gtk_combo_box_get_entry_text_column(cbwe);
    do
    {
        gtk_tree_model_get(model, &iter, column, &tree_string, -1);
        match = g_utf8_collate(text, tree_string) == 0;
        g_free(tree_string);
        if (!match)
            continue;

        /* Found a matching string */
        id = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cbwe), CHANGED_ID));
        g_signal_handler_block(cbwe, id);
        gtk_combo_box_set_active_iter(GTK_COMBO_BOX(cbwe), &iter);
        g_signal_handler_unblock(cbwe, id);

        index = gtk_combo_box_get_active(GTK_COMBO_BOX(cbwe));
        g_object_set_data(G_OBJECT(cbwe), LAST_INDEX, GINT_TO_POINTER(index));
        return;
    }
    while (gtk_tree_model_iter_next(model, &iter));
}


/**  The GtkComboBox with entry widget has changed its value.  If the widget
 *   now points to another valid entry string then record the index of
 *   that string for use when the user leaves the widget.
 *
 *   @param widget Unused.
 *
 *   @param cbwe A pointer to a GtkComboBox widget. */
static void
gnc_cbwe_changed_cb (GtkComboBox *widget,
                    GtkComboBox *cbwe)
{
    gint index;

    index = gtk_combo_box_get_active(widget);
    if (index == -1)
        return;
    g_object_set_data(G_OBJECT(cbwe), LAST_INDEX, GINT_TO_POINTER(index));
}


/**  The completion attached to currency edit widget has selected a
 *   match.  This function extracts the completed string from the
 *   completion code's temporary model, and uses that to set the index
 *   of that currency name for use when the user leaves the widget.
 *   This should always point to a valid currency name since the user
 *   made the selection from a list of currency names.
 *
 *   @param completion Unused.
 *
 *   @param comp_model A temporary model used by completion code that
 *   contains only the current matches.
 *
 *   @param comp_iter The iter in the completion's temporary model
 *   that represents the user selected match.
 *
 *   @param cbwe A pointer to a currency entry widget. */
static gboolean
gnc_cbwe_match_selected_cb (GtkEntryCompletion *completion,
                            GtkTreeModel       *comp_model,
                            GtkTreeIter        *comp_iter,
                            GtkComboBox        *cbwe)
{
    gint column;
    gchar *text;

    column = gtk_combo_box_get_entry_text_column(cbwe);
    gtk_tree_model_get(comp_model, comp_iter, column, &text, -1);
    gnc_cbwe_set_by_string(cbwe, text);
    g_free(text);
    return FALSE;
}


/**  The focus left the currency edit widget, so reset the widget to
 *   its last known good value.  If the widget value contained a valid
 *   currency then this is a noop.  Otherwise the widget will be reset
 *   to the last user selected currency.  This latter state will occur
 *   if the user has typed characters directly into the widget but not
 *   selected a completion.
 *
 *   @param entry Unused.
 *
 *   @param event Unused.
 *
 *   @param cbwe A pointer to a currency entry widget. */
static gboolean
gnc_cbwe_focus_out_cb (GtkEntry *entry,
                       GdkEventFocus *event,
                       GtkComboBox *cbwe)
{
    const gchar *text;
    gint index;

    /* Make a final attempt to match the current text. */
    text = gtk_entry_get_text(entry);
    gnc_cbwe_set_by_string(cbwe, text);

    /* Get the last known index (which may have just been set). */
    index = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cbwe), LAST_INDEX));
    gtk_combo_box_set_active(GTK_COMBO_BOX(cbwe), index);
    return FALSE;
}

void
gnc_cbwe_add_completion (GtkComboBox *cbwe)
{
    GtkEntry *entry;
    GtkEntryCompletion *completion;
    GtkTreeModel *model;

    entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN(cbwe)));
    completion = gtk_entry_get_completion(entry);
    if (completion)
        return;

    /* No completion yet? Set one up. */
    completion = gtk_entry_completion_new();
    model = gtk_combo_box_get_model(GTK_COMBO_BOX(cbwe));
    gtk_entry_completion_set_model(completion, model);
    gtk_entry_completion_set_text_column(completion, 0);
    gtk_entry_set_completion(entry, completion);
    g_object_unref(completion);
}

void
gnc_cbwe_require_list_item (GtkComboBox *cbwe)
{
    GtkEntry *entry;
    GtkEntryCompletion *completion;
    GtkTreeModel *model;
    GtkTreeIter iter;
    gint index, id;

    /* Ensure completion is set up. */
    gnc_cbwe_add_completion(cbwe);

    /* If an item in the combo box isn't already selected, then force
     * select the first item. Take care, the combo box may not have been
     * filled yet.  */
    entry = GTK_ENTRY(gtk_bin_get_child(GTK_BIN(cbwe)));
    completion = gtk_entry_get_completion(entry);
    index = gtk_combo_box_get_active(GTK_COMBO_BOX(cbwe));
    if (index == -1)
    {
        model = gtk_entry_completion_get_model(completion);
        if (gtk_tree_model_get_iter_first(model, &iter))
        {
            gtk_combo_box_set_active(GTK_COMBO_BOX(cbwe), 0);
            index = 0;
        }
    }
    g_object_set_data(G_OBJECT(cbwe), LAST_INDEX, GINT_TO_POINTER(index));

    /* Now the signals to make sure the user can't leave the
       widget without a valid match. */
    id = g_signal_connect(cbwe, "changed",
                          G_CALLBACK(gnc_cbwe_changed_cb), cbwe);
    g_signal_connect(completion, "match_selected",
                     G_CALLBACK(gnc_cbwe_match_selected_cb), cbwe);
    g_signal_connect(entry, "focus-out-event",
                     G_CALLBACK(gnc_cbwe_focus_out_cb), cbwe);

    g_object_set_data(G_OBJECT(cbwe), CHANGED_ID, GINT_TO_POINTER(id));
}

/** Return whether the current gtk theme is a dark one. A theme is considered "dark" if
 *  it has a dark background color with a light foreground color (used for text and so on).
 *  We only test on the foreground color assuming a sane theme chooses enough contrast between
 *  foreground and background colors.
 *
 *  @param fg_color The foreground color to test.
 *
 *  @returns TRUE if the theme is considered dark, FALSE otherwise.
 */
gboolean
gnc_is_dark_theme (GdkRGBA *fg_color)
{
    gboolean is_dark = FALSE;

    // Counting the perceptive luminance - human eye favors green color...
    double lightness = (0.299 * fg_color->red + 0.587 * fg_color->green + 0.114 * fg_color->blue);

    if (lightness > 0.5)
        is_dark = TRUE;

    return is_dark;
}

/** Wrapper to get the background color of a widget for a given state
 *
 *  @param context Style context of widget.
 *
 *  @param state The stateflag of the widget.
 *
 *  @param color The returned background color of the widget.
 */
void
gnc_style_context_get_background_color (GtkStyleContext *context,
                                        GtkStateFlags    state,
                                        GdkRGBA         *color)
{
    GdkRGBA *c;

    g_return_if_fail (color != NULL);
    g_return_if_fail (GTK_IS_STYLE_CONTEXT (context));

    gtk_style_context_get (context,
                           state,
                           GTK_STYLE_PROPERTY_BACKGROUND_COLOR, &c,
                           NULL);
    *color = *c;
    gdk_rgba_free (c);
}

/** Wrapper to get the border color of a widget for a given state
 *
 *  @param context Style context of widget.
 *
 *  @param state The stateflag of the widget.
 *
 *  @param color The returned border color of the widget.
 */
void
gnc_style_context_get_border_color (GtkStyleContext *context,
                                    GtkStateFlags    state,
                                    GdkRGBA         *color)
{
    GdkRGBA *c;

    g_return_if_fail (color != NULL);
    g_return_if_fail (GTK_IS_STYLE_CONTEXT (context));

    gtk_style_context_get (context,
                           state,
                           GTK_STYLE_PROPERTY_BORDER_COLOR, &c,
                           NULL);
    *color = *c;
    gdk_rgba_free (c);
}

static gpointer
find_widget_func (GtkWidget *widget, const gchar *id)
{
    const gchar *name = gtk_buildable_get_name (GTK_BUILDABLE(widget));
    GtkWidget *ret = NULL;

    if (g_strcmp0 (name, id) == 0)
        return widget;

    if (GTK_IS_CONTAINER(widget))
    {
        GList *container_list = gtk_container_get_children (GTK_CONTAINER(widget));
        for (GList *n = container_list; !ret && n; n = n->next)
            ret = find_widget_func (n->data, id);
        g_list_free (container_list);
    }

    return ret;
}

/** Find the Widget defined by 'id' in the dialog
 *
 *  @param dialog The dialog to search for 'id'.
 *
 *  @param id The widget name to find in the dialog.
 *
 *  @returns The widget defined by id in the dialog or NULL.
 */
GtkWidget *
gnc_get_dialog_widget_from_id (GtkDialog *dialog, const gchar *id)
{
    GtkWidget *content_area = gtk_dialog_get_content_area (dialog);
    return find_widget_func (content_area, id);
}


/** Disable all the actions in a simple action group
 *
 *  @param action_group The GSimpleActionGroup
 */
void
gnc_disable_all_actions_in_group (GSimpleActionGroup *action_group)
{
    gchar **actions;
    gint num_actions;

    g_return_if_fail (action_group != NULL);

    actions = g_action_group_list_actions (G_ACTION_GROUP(action_group));
    num_actions = g_strv_length (actions);

    // Disable the actions
    for (gint i = 0; i < num_actions; i++)
    {
        GAction *action = g_action_map_lookup_action (G_ACTION_MAP(action_group),
                                                      actions[i]);
        g_simple_action_set_enabled (G_SIMPLE_ACTION(action), FALSE);
    }
    g_strfreev (actions);
}


static void
add_accel_for_menu_lookup (GtkWidget *widget, gpointer user_data)
{
    if (GTK_IS_MENU_ITEM(widget))
    {
        GtkMenuItem* menuItem = GTK_MENU_ITEM(widget);
        GtkWidget* subMenu = gtk_menu_item_get_submenu (menuItem);
        GtkWidget *accel_label = gtk_bin_get_child (GTK_BIN(widget));

        if (accel_label)
        {
            guint key;
            GdkModifierType mods;

            gtk_accel_label_get_accel (GTK_ACCEL_LABEL(accel_label), &key, &mods);

            if (key > 0)
                gtk_widget_add_accelerator (GTK_WIDGET(widget), "activate",
                                            GTK_ACCEL_GROUP(user_data),
                                            key, mods, GTK_ACCEL_VISIBLE);
        }
        if (GTK_IS_CONTAINER(subMenu))
            gtk_container_foreach (GTK_CONTAINER(subMenu),
                                   add_accel_for_menu_lookup, user_data);
    }
}

/** Add accelerator keys for menu item widgets
 *
 *  @param menu The menu widget.
 *
 *  @param accel_group The accelerator group to use.
 */
void
gnc_add_accelerator_keys_for_menu (GtkWidget *menu, GtkAccelGroup *accel_group)
{
    g_return_if_fail (GTK_IS_WIDGET(menu));
    g_return_if_fail (accel_group != NULL);

    gtk_container_foreach (GTK_CONTAINER(menu), add_accel_for_menu_lookup, accel_group);
}


static gpointer
find_menu_item_func (GtkWidget *widget, const gchar *action_name)
{
    GtkWidget *ret = NULL;

    if (GTK_IS_MENU_ITEM(widget))
    {
        const gchar *a_name = g_object_get_data (G_OBJECT(widget), "myaction-name");

        GtkWidget* subMenu;

        if (g_strcmp0 (a_name, action_name) == 0)
            return widget;

        subMenu = gtk_menu_item_get_submenu (GTK_MENU_ITEM(widget));

        if (GTK_IS_CONTAINER(subMenu))
        {
            GList *container_list = gtk_container_get_children (GTK_CONTAINER(subMenu));
            for (GList *n = container_list; !ret && n; n = n->next)
                ret = find_menu_item_func (n->data, action_name);
            g_list_free (container_list);
        }
    }
    return ret;
}

/** Search the menu for the menu item based on the label or action name
 *
 *  @param menu The menu widget.
 *
 *  @param action_name The GAction name.
 *
 *  @return The menu item widget or NULL.
 */
GtkWidget *
gnc_find_menu_item (GtkWidget *menu, const gchar *action_name)
{
    GtkWidget *ret = NULL;

    g_return_val_if_fail (GTK_IS_WIDGET(menu), NULL);
    g_return_val_if_fail (action_name != NULL, NULL);

    if (GTK_IS_CONTAINER(menu))
    {
        GList *container_list = gtk_container_get_children (GTK_CONTAINER(menu));
        for (GList *n = container_list; !ret && n; n = n->next)
            ret = find_menu_item_func (n->data, action_name);
        g_list_free (container_list);
    }
    return ret;
}


static void
search_menu_item_list (GtkWidget *widget, gpointer user_data)
{
    GList **list = user_data;

    if (GTK_IS_MENU_ITEM(widget))
    {
        GtkWidget* subMenu = gtk_menu_item_get_submenu (GTK_MENU_ITEM(widget));
        const gchar *a_name = g_object_get_data (G_OBJECT(widget), "myaction-name");

        *list = g_list_prepend (*list, widget);

        if (!a_name)
        {
            GtkWidget *accel_label = gtk_bin_get_child (GTK_BIN(widget));

            if (accel_label)
            {
                // use gtk_label_get_text to get text with no underlines
                const gchar *al_name = gtk_label_get_label (GTK_LABEL(accel_label));

                g_object_set_data_full (G_OBJECT(widget), "myaction-name",
                                        g_strdup (al_name), g_free);
            }
        }

        if (GTK_IS_CONTAINER(subMenu))
            gtk_container_foreach (GTK_CONTAINER(subMenu),
                                   search_menu_item_list, user_data);
    }
}

/** Return a list of menu items
 *
 *  @param menu The menu widget.
 *
 *  @return A GList of menu items or NULL.
 */
GList *
gnc_menu_get_items (GtkWidget *menu)
{
    GList *list = NULL;

    g_return_val_if_fail (GTK_IS_WIDGET(menu), NULL);

    gtk_container_foreach (GTK_CONTAINER(menu), search_menu_item_list, &list);

    return list;
}

/** Search the toolbar for the tool item based on the action name
 *
 *  @param toolbar The toolbar widget.
 *
 *  @param action_name The GAction name.
 *
 *  @return The tool item widget or NULL.
 */
GtkWidget *
gnc_find_toolbar_item (GtkWidget *toolbar, const gchar *action_name)
{
    GtkWidget *found = NULL;

    g_return_val_if_fail (GTK_IS_TOOLBAR(toolbar), NULL);
    g_return_val_if_fail (action_name != NULL, NULL);

    for (gint i = 0; i < gtk_toolbar_get_n_items (GTK_TOOLBAR(toolbar)); i++)
    {
        GtkToolItem *item = gtk_toolbar_get_nth_item (GTK_TOOLBAR(toolbar), i);

        if (GTK_IS_ACTIONABLE(item))
        {
            const gchar *item_action_name = gtk_actionable_get_action_name (GTK_ACTIONABLE(item));

            if (g_str_has_suffix (item_action_name, action_name))
            {
                found = GTK_WIDGET(item);
                break;
            }
        }
    }
    return found;
}
