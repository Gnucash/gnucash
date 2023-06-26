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
accel_map_foreach_func (gpointer user_data, const gchar* accel_path, guint accel_key,
                        GdkModifierType accel_mods, gboolean changed)
{
    GMenuModel *menu_model = user_data;
    gchar **accel_path_parts = NULL;
    guint  accel_size = 0;
    gchar *target = NULL;
    gchar *accel_name_tmp = gtk_accelerator_name (accel_key, accel_mods);
    gchar *accel_name = g_strescape (accel_name_tmp, NULL);

    accel_path_parts = g_strsplit (accel_path, "/", -1);
    accel_size = g_strv_length (accel_path_parts);

    if (accel_size == 4)
        target = g_strdup (accel_path_parts[3]);

    if (accel_size >=3)
        gnc_menubar_model_update_item (menu_model, accel_path_parts[2],
                                       target, NULL, accel_name, NULL);

    g_strfreev (accel_path_parts);
    g_free (target);
    g_free (accel_name_tmp);
    g_free (accel_name);
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
            gboolean added = GPOINTER_TO_INT(g_object_get_data (G_OBJECT(menuItem),
                                                                "accel-added"));
            guint key;
            GdkModifierType mods;

            gtk_accel_label_get_accel (GTK_ACCEL_LABEL(accel_label), &key, &mods);

            if (key > 0 && !added)
            {
                g_object_set_data (G_OBJECT(menuItem), "accel-added", GINT_TO_POINTER(1));
                gtk_widget_add_accelerator (GTK_WIDGET(widget), "activate",
                                            GTK_ACCEL_GROUP(user_data),
                                            key, mods, GTK_ACCEL_VISIBLE);
            }
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
 *  @param model The menu bar model.
 *
 *  @param accel_group The accelerator group to use.
 */
void
gnc_add_accelerator_keys_for_menu (GtkWidget *menu, GMenuModel *model, GtkAccelGroup *accel_group)
{
    g_return_if_fail (GTK_IS_WIDGET(menu));
    g_return_if_fail (model != NULL);
    g_return_if_fail (accel_group != NULL);

    // this updates the menu accelerators based on accelerator-map
    gtk_accel_map_foreach (model, (GtkAccelMapForeach)accel_map_foreach_func);

    gtk_container_foreach (GTK_CONTAINER(menu), add_accel_for_menu_lookup, accel_group);
}


static gpointer
find_menu_item_func (GtkWidget *widget, const gchar *action_name, const gchar *action_label)
{
    GtkWidget *ret = NULL;

    if (GTK_IS_MENU_ITEM(widget))
    {
        GtkWidget* subMenu;

        if (action_name)
        {
            if (GTK_IS_ACTIONABLE(widget))
            {
                const gchar *a_name = gtk_actionable_get_action_name (GTK_ACTIONABLE(widget));

                if (g_strcmp0 (a_name, action_name) == 0)
                    return widget;
            }
        }

        if (action_label)
        {
            GtkWidget *accel_label = gtk_bin_get_child (GTK_BIN(widget));

            if (accel_label)
            {
                // use gtk_label_get_text to get text with no underlines
                const gchar *al_name = gtk_label_get_label (GTK_LABEL(accel_label));

                if (g_strcmp0 (al_name, action_label) == 0)
                    return widget;
             }
        }

        subMenu = gtk_menu_item_get_submenu (GTK_MENU_ITEM(widget));

        if (GTK_IS_CONTAINER(subMenu))
        {
            GList *container_list = gtk_container_get_children (GTK_CONTAINER(subMenu));
            for (GList *n = container_list; !ret && n; n = n->next)
                ret = find_menu_item_func (n->data, action_name, action_label);
            g_list_free (container_list);
        }
    }
    return ret;
}

/** Search the menu for the menu item based on action name
 *
 *  @param menu The menu widget.
 *
 *  @param action_name The GAction name.
 *
 *  @return The menu item widget or NULL.
 */
GtkWidget *
gnc_find_menu_item_by_action_name (GtkWidget *menu, const gchar *action_name)
{
    GtkWidget *ret = NULL;
    const gchar *action_label = NULL;

    g_return_val_if_fail (GTK_IS_WIDGET(menu), NULL);
    g_return_val_if_fail (action_name != NULL, NULL);

    if (GTK_IS_CONTAINER(menu))
    {
        GList *container_list = gtk_container_get_children (GTK_CONTAINER(menu));
        for (GList *n = container_list; !ret && n; n = n->next)
            ret = find_menu_item_func (n->data, action_name, action_label);
        g_list_free (container_list);
    }
    return ret;
}


/** Search the menu for the menu item based on the action label
 *
 *  @param menu The menu widget.
 *
 *  @param action_label The GtkMenuItem label.
 *
 *  @return The menu item widget or NULL.
 */
GtkWidget *
gnc_find_menu_item_by_action_label (GtkWidget *menu, const gchar *action_label)
{
    GtkWidget *ret = NULL;
    const gchar *action_name = NULL;

    g_return_val_if_fail (GTK_IS_WIDGET(menu), NULL);
    g_return_val_if_fail (action_label != NULL, NULL);

    if (GTK_IS_CONTAINER(menu))
    {
        GList *container_list = gtk_container_get_children (GTK_CONTAINER(menu));
        for (GList *n = container_list; !ret && n; n = n->next)
            ret = find_menu_item_func (n->data, action_name, action_label);
        g_list_free (container_list);
    }
    return ret;
}


static void
menu_item_list (GtkWidget *widget, gpointer user_data)
{
    GList **list = user_data;

    if (GTK_IS_MENU_ITEM(widget))
    {
        GtkWidget* subMenu = gtk_menu_item_get_submenu (GTK_MENU_ITEM(widget));

        *list = g_list_prepend (*list, widget);

        if (GTK_IS_CONTAINER(subMenu))
            gtk_container_foreach (GTK_CONTAINER(subMenu),
                                   menu_item_list, user_data);
    }
}

/** Return a list of GtkMenuItems
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

    gtk_container_foreach (GTK_CONTAINER(menu), menu_item_list, &list);

    return list;
}


struct find_tool_item_struct
{
    GtkWidget   *found_tool_item;
    const gchar *action_name;
};

static void
find_tool_action (GtkWidget *widget, gpointer user_data)
{
    struct find_tool_item_struct *ftis = user_data;

    if (GTK_IS_ACTIONABLE(widget))
    {
        // this returns the full action name
        const gchar *item_action_name = gtk_actionable_get_action_name (GTK_ACTIONABLE(widget));

        if (g_str_has_suffix (item_action_name, ftis->action_name))
            ftis->found_tool_item = GTK_WIDGET(widget);
    }
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
    struct find_tool_item_struct ftis;

    g_return_val_if_fail (GTK_IS_TOOLBAR(toolbar), NULL);
    g_return_val_if_fail (action_name != NULL, NULL);

    ftis.action_name = action_name;
    ftis.found_tool_item = NULL;

    gtk_container_foreach (GTK_CONTAINER(toolbar), find_tool_action, &ftis);

    return ftis.found_tool_item;
}


static void
extract_items_from_model (GMenuModel *model,
                          gint        item,
                          gpointer    user_data)
{
    GMenuAttributeIter *iter;
    const gchar *key;
    GVariant *value;
    GncMenuModelSearch *gsm = user_data;
    const gchar *action = NULL;
    const gchar *label = NULL;
    const gchar *tooltip = NULL;
    const gchar *target_char = NULL;
    gint         target_int = -1;

    iter = g_menu_model_iterate_item_attributes (model, item);
    while (g_menu_attribute_iter_get_next (iter, &key, &value))
    {
        if (g_str_equal (key, GNC_MENU_ATTRIBUTE_TOOLTIP) &&
            g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
            tooltip = g_variant_get_string (value, NULL);
        else if (g_str_equal (key, G_MENU_ATTRIBUTE_LABEL) &&
                 g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
            label = g_variant_get_string (value, NULL);
        else if (g_str_equal (key, G_MENU_ATTRIBUTE_ACTION) &&
                 g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
            action = g_variant_get_string (value, NULL);
        else if (g_str_equal (key, G_MENU_ATTRIBUTE_TARGET) &&
                 g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
            target_char = g_variant_get_string (value, NULL);
        else if (g_str_equal (key, G_MENU_ATTRIBUTE_TARGET) &&
                 g_variant_is_of_type (value, G_VARIANT_TYPE_INT32))
            target_int = g_variant_get_int32 (value);
        g_variant_unref (value);
    }

    if (gsm->search_action_target)
    {
        gboolean target_test = FALSE;

        if (target_int != -1 && target_int == atoi (gsm->search_action_target))
            target_test = TRUE;

        if (target_char && g_strcmp0 (target_char, gsm->search_action_target) == 0)
            target_test = TRUE;

        if (!target_test)
        {
            g_object_unref (iter);
            return;
        }
    }

    if (action && gsm->search_action_name)
    {
        if (g_str_has_suffix (action, gsm->search_action_name))
        {
            gsm->model = model;
            gsm->index = item;
            gsm->tooltip = tooltip;
            gsm->search_action_label = label;
        }
    }
    if (label && gsm->search_action_label)
    {
        if (g_strcmp0 (label, gsm->search_action_label) == 0)
        {
            gsm->model = model;
            gsm->index = item;
            gsm->tooltip = tooltip;
            gsm->search_action_name = action;
        }
    }
    g_object_unref (iter);
}

static void
items_from_model (GMenuModel *model,
                  gpointer user_data)
{
    GncMenuModelSearch *gsm = user_data;

    for (gint i = 0; i < g_menu_model_get_n_items (model); i++)
    {
        GMenuLinkIter *iter;
        GMenuModel *sub_model;

        if (gsm->model)
            return;

        extract_items_from_model (model, i, user_data);

        iter = g_menu_model_iterate_item_links (model, i);
        while (g_menu_link_iter_get_next (iter, NULL, &sub_model))
        {
            items_from_model (sub_model, user_data);
            g_object_unref (sub_model);
        }
        g_object_unref (iter);
    }
}

/** Find a GtkMenu item from the action name. This is done by first finding
 *  the action name in the GMenuModel and then doing a search for the
 *  label text in the GtkMenu.
 *
 *  NOTE: This is done this way as the action_name field of the GtkMenuItem
 *  is not populated from the model.
 *
 *  @param menu_model The GMenuModel of the menu.
 *
 *  @param gsm The GncMenuModelSearch structure.
 *
 *  @return TRUE if GMenuModel item found or FALSE if not.
 */
gboolean
gnc_menubar_model_find_item (GMenuModel *menu_model, GncMenuModelSearch *gsm)
{

    g_return_val_if_fail (menu_model != NULL, FALSE);
    g_return_val_if_fail (gsm != NULL, FALSE);

    gsm->model = NULL;

    items_from_model (menu_model, gsm);

    if (gsm->model)
       return TRUE;

    return FALSE;
}


/** Find a GtkMenu item from the action name. This is done by first finding
 *  the action name in the GMenuModel and then doing a search for the
 *  label text in the GtkMenu.
 *
 *  NOTE: This is done this way as the action_name field of the GtkMenuItem
 *  is not populated from the model.
 *
 *  @param menu_model The GMenuModel of the menu.
 *
 *  @param menu The GtkMenu built from the model.
 *
 *  @param action_name The action name of the menu item to find.
 *
 *  @return The GtkMenuItem if found or NULL
 */
GtkWidget *
gnc_menubar_model_find_menu_item (GMenuModel *menu_model, GtkWidget *menu, const gchar *action_name)
{
    GncMenuModelSearch *gsm;
    GtkWidget *menu_item = NULL;

    g_return_val_if_fail (menu_model != NULL, NULL);
    g_return_val_if_fail (menu != NULL, NULL);
    g_return_val_if_fail (action_name != NULL, NULL);

    gsm = g_new0 (GncMenuModelSearch, 1);

    gsm->search_action_label = NULL;
    gsm->search_action_name = action_name;
    gsm->search_action_target = NULL;

    if (gnc_menubar_model_find_item (menu_model, gsm))
        menu_item = gnc_find_menu_item_by_action_label (menu, gsm->search_action_label);

    g_free (gsm);
    return menu_item;
}


/** Update the GMenuModel item based on the action name by copying
 *  existing item, removing it and inserting a new one in same location.
 *
 *  @param menu_model The GMenuModel of the menu.
 *
 *  @param action_name The action name to update.
 * 
 *  @param target The action target if required, else NULL.
 *
 *  @param label The new menu label text.
 *
 *  @param accel_name The accelerator string
 *
 *  @param tooltip The new tooltip text if any.
 *
 *  @return TRUE if item found and updated or FALSE if not.
 */
gboolean
gnc_menubar_model_update_item (GMenuModel *menu_model, const gchar *action_name,
                               const gchar *target, const gchar *label,
                               const gchar *accel_name, const gchar *tooltip)
{
    GncMenuModelSearch *gsm;
    gboolean found = FALSE;

    g_return_val_if_fail (menu_model != NULL, FALSE);
    g_return_val_if_fail (action_name != NULL, FALSE);

    gsm = g_new0 (GncMenuModelSearch, 1);

    gsm->search_action_label = NULL;
    gsm->search_action_name = action_name;
    gsm->search_action_target = target;

    if (gnc_menubar_model_find_item (menu_model, gsm))
    {
        GMenuAttributeIter *iter;
        const gchar *key;
        GVariant *value;
        GVariant *old_target = NULL;
        const gchar *old_action = NULL;
        const gchar *old_temp = NULL;
        const gchar *old_accel = NULL;
        const gchar *old_tooltip = NULL;

        iter = g_menu_model_iterate_item_attributes (gsm->model, gsm->index);
        while (g_menu_attribute_iter_get_next (iter, &key, &value))
        {
            if (g_str_equal (key, GNC_MENU_ATTRIBUTE_TEMPORARY) &&
                g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
                old_temp = g_variant_get_string (value, NULL);
            else if (g_str_equal (key, G_MENU_ATTRIBUTE_ACTION) &&
                     g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
                old_action = g_variant_get_string (value, NULL);
            else if (g_str_equal (key, GNC_MENU_ATTRIBUTE_ACCELERATOR) &&
                     g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
                old_accel = g_variant_get_string (value, NULL);
            else if (g_str_equal (key, GNC_MENU_ATTRIBUTE_TOOLTIP) &&
                     g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
                old_tooltip = g_variant_get_string (value, NULL);
            else if (g_str_equal (key, G_MENU_ATTRIBUTE_TARGET))
                old_target = g_variant_ref (value);

            g_variant_unref (value);
        }
        g_object_unref (iter);

        if (!label && !gsm->search_action_label)
        {
            if (old_target)
                g_variant_unref (old_target);

            g_free (gsm);
            return found;
        }

        if ((accel_name && g_strcmp0 (old_accel, accel_name) != 0) ||
            (tooltip && g_strcmp0 (old_tooltip, tooltip) != 0) ||
            (label && g_strcmp0 (gsm->search_action_label, label) != 0))
        {
            GMenuItem *item = NULL;

            if (label)
                item = g_menu_item_new (label, old_action);
            else
                item = g_menu_item_new (gsm->search_action_label, old_action);

            if (tooltip)
                g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_TOOLTIP, "s", tooltip);
            else
            {
                if (old_tooltip)
                   g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_TOOLTIP, "s", old_tooltip);
            }
            if (accel_name)
                g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_ACCELERATOR, "s", accel_name);
            else
            {
                if (old_accel)
                    g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_ACCELERATOR, "s", old_accel);
            }
            if (old_temp)
                g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_TEMPORARY, "s", old_temp);

            if (old_target)
                g_menu_item_set_attribute_value (item, G_MENU_ATTRIBUTE_TARGET, old_target);

            g_menu_remove (G_MENU(gsm->model), gsm->index);
            g_menu_insert_item (G_MENU(gsm->model), gsm->index, item);
            g_object_unref (item);
            found = TRUE;
        }
        if (old_target)
            g_variant_unref (old_target);
    }
    g_free (gsm);
    return found;
}


typedef struct
{
    GMenuModel *model;
    gint        index;
} to_remove;

static void
item_to_remove_from_model (GMenuModel  *model,
                           gint         item,
                           GList      **remove_list,
                           const gchar *attrib)
{
    GVariant *value = g_menu_model_get_item_attribute_value (model, item,
                                                             attrib, NULL);

    if (value && g_variant_is_of_type (value, G_VARIANT_TYPE_STRING))
    {
        to_remove *tr = g_new0 (to_remove, 1);
        tr->model = model;
        tr->index = item;

        // to keep the order append
        *remove_list = g_list_append (*remove_list, tr);
        g_variant_unref (value);
    }
}

static void
remove_items_from_model (GMenuModel *model,
                         GList **remove_list,
                         const gchar *attrib)
{
    // Note: item high to low
    for (gint i = g_menu_model_get_n_items (model) -1; i >= 0; i--)
    {
        GMenuLinkIter *iter;
        GMenuModel *sub_model;

        item_to_remove_from_model (model, i, remove_list, attrib);

        iter = g_menu_model_iterate_item_links (model, i);
        while (g_menu_link_iter_get_next (iter, NULL, &sub_model))
        {
            remove_items_from_model (sub_model, remove_list, attrib);
            g_object_unref (sub_model);
        }
        g_object_unref (iter);
    }
}

static void
remove_items (gpointer data, gpointer user_data)
{
    to_remove *tr = (to_remove*)data;
    g_menu_remove (G_MENU(tr->model), tr->index);
    g_free (tr);
}

/** Remove GMenuModel entries based on having an attribute value equal
 *  to attrib, it does not matter what the value is.
 *
 *  @param menu_model The GMenuModel of the menu.
 *
 *  @param attrib The attribute to look for.
 */
void
gnc_menubar_model_remove_items_with_attrib (GMenuModel *menu_model, const gchar *attrib)
{
    GList *remove_list = NULL;

    g_return_if_fail (menu_model != NULL);
    g_return_if_fail (attrib != NULL);

    remove_items_from_model (menu_model, &remove_list, attrib);

    g_list_foreach (remove_list, (GFunc)remove_items, NULL);
    g_list_free (remove_list);
}


static void
statusbar_push (GtkWidget *statusbar, const gchar *text)
{
    gtk_statusbar_push (GTK_STATUSBAR(statusbar), 0,
                        text ? text : " ");
}

static void
statusbar_pop (GtkWidget *statusbar)
{
    gtk_statusbar_pop (GTK_STATUSBAR(statusbar), 0);
}

static void
menu_item_select_cb (GtkWidget *menu_item, GtkWidget *statusbar)
{
    GtkWidget *accel_label = gtk_bin_get_child (GTK_BIN(menu_item));
    GMenuModel *menubar_model = g_object_get_data (G_OBJECT(statusbar), "menu-model");

    if (!menubar_model)
        return;

    if (accel_label)
    {
        GncMenuModelSearch *gsm = g_new0 (GncMenuModelSearch, 1);

        gsm->search_action_label = gtk_label_get_label (GTK_LABEL(accel_label));
        gsm->search_action_name = NULL;

        if (gnc_menubar_model_find_item (menubar_model, gsm))
        {
            if (gsm->model)
                statusbar_push (statusbar, gsm->tooltip);
        }
        g_free (gsm);
    }
}

static void
menu_item_deselect_cb (GtkWidget *menu_item, GtkWidget *statusbar)
{
    statusbar_pop (statusbar);
}

/** Setup the callbacks for menu bar items so the tooltip can be
 *  displayed in the status bar.
 *
 *  @param menu_item The menubar menu item widget.
 *
 *  @param statusbar The statusbar widget to display the tooltip.
 */
void
gnc_menu_item_setup_tooltip_to_statusbar_callback (GtkWidget *menu_item,
                                                   GtkWidget *statusbar)
{
    g_return_if_fail (menu_item != NULL);
    g_return_if_fail (statusbar != NULL);

    if (GPOINTER_TO_INT(g_object_get_data(G_OBJECT(menu_item), "added-callbacks")))
        return;

    g_signal_connect (menu_item, "select",
                      G_CALLBACK(menu_item_select_cb),
                      statusbar);
    g_signal_connect (menu_item, "deselect",
                      G_CALLBACK(menu_item_deselect_cb),
                      statusbar);
    g_object_set (G_OBJECT(menu_item), "has-tooltip", FALSE, NULL);

    g_object_set_data (G_OBJECT(menu_item), "added-callbacks", GINT_TO_POINTER(1));
}


static gboolean
tool_item_enter_event (GtkWidget *button, GdkEvent *event,
                       gpointer user_data)
{
    GtkWidget *tool_item = gtk_widget_get_parent (button);
    gchar *tooltip = gtk_widget_get_tooltip_text (tool_item);
    statusbar_push (user_data, tooltip);
    g_free (tooltip);
    return FALSE;
}

static gboolean
tool_item_leave_event (GtkWidget *button, GdkEvent *event,
                       gpointer user_data)
{
    statusbar_pop (user_data);
    return FALSE;
}

/** Setup the callbacks for tool bar items so the tooltip can be
 *  displayed in the status bar.
 *
 *  @param tool_item The toolbar tool item widget.
 *
 *  @param statusbar The statusbar widget to display the tooltip.
 */
void
gnc_tool_item_setup_tooltip_to_statusbar_callback (GtkWidget *tool_item,
                                                   GtkWidget *statusbar)
{
    GtkWidget *child;

    g_return_if_fail (tool_item != NULL);
    g_return_if_fail (statusbar != NULL);

    child = gtk_bin_get_child (GTK_BIN(tool_item));

    gtk_widget_add_events (GTK_WIDGET(child),
                           GDK_ENTER_NOTIFY_MASK | GDK_LEAVE_NOTIFY_MASK
                           | GDK_FOCUS_CHANGE_MASK);

    g_signal_connect (child, "enter-notify-event",
                      G_CALLBACK (tool_item_enter_event),
                      statusbar);

    g_signal_connect (child, "leave-notify-event",
                      G_CALLBACK (tool_item_leave_event),
                      statusbar);

    g_object_set (G_OBJECT(tool_item), "has-tooltip", FALSE, NULL);
}
