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

#include <string.h>

#include "gnc-gtk-utils.h"
#include "gnc-engine.h"

static QofLogModule log_module = GNC_MOD_GUI;

static void
gnc_box_pack_full (GtkBox *box, GtkWidget *child, gboolean expand,
                   gboolean fill, guint padding, gboolean prepend)
{
    GtkOrientation orientation;

    g_return_if_fail (GTK_IS_BOX (box));
    g_return_if_fail (GTK_IS_WIDGET (child));

    orientation = gtk_orientable_get_orientation (GTK_ORIENTABLE (box));
    if (orientation == GTK_ORIENTATION_HORIZONTAL)
    {
        gtk_widget_set_hexpand (child, expand);
        if (expand && !fill)
            gtk_widget_set_halign (child, GTK_ALIGN_CENTER);
        if (padding)
        {
            gtk_widget_set_margin_start
                (child, gtk_widget_get_margin_start (child) + padding);
            gtk_widget_set_margin_end
                (child, gtk_widget_get_margin_end (child) + padding);
        }
    }
    else
    {
        gtk_widget_set_vexpand (child, expand);
        if (expand && !fill)
            gtk_widget_set_valign (child, GTK_ALIGN_CENTER);
        if (padding)
        {
            gtk_widget_set_margin_top
                (child, gtk_widget_get_margin_top (child) + padding);
            gtk_widget_set_margin_bottom
                (child, gtk_widget_get_margin_bottom (child) + padding);
        }
    }

    if (prepend)
        gtk_box_prepend (box, child);
    else
        gtk_box_append (box, child);
}

void
gnc_box_append_full (GtkBox *box, GtkWidget *child, gboolean expand,
                     gboolean fill, guint padding)
{
    gnc_box_pack_full (box, child, expand, fill, padding, FALSE);
}

void
gnc_box_prepend_full (GtkBox *box, GtkWidget *child, gboolean expand,
                      gboolean fill, guint padding)
{
    gnc_box_pack_full (box, child, expand, fill, padding, TRUE);
}

void
gnc_widget_set_all_margins (GtkWidget *widget, gint margin)
{
    g_return_if_fail (GTK_IS_WIDGET (widget));

    gtk_widget_set_margin_start (widget, margin);
    gtk_widget_set_margin_end (widget, margin);
    gtk_widget_set_margin_top (widget, margin);
    gtk_widget_set_margin_bottom (widget, margin);
}

#define GNC_DROP_DOWN_WIDTH_DATA "gnc-drop-down-width-data"

typedef struct
{
    GListModel *model;
    gulong model_items_changed_id;
    GtkSettings *settings;
    gulong settings_theme_changed_id;
    gulong settings_dark_theme_changed_id;
    gulong settings_font_changed_id;
    gint original_min_width;
    gint original_min_height;
    gint requested_min_width;
    gint requested_min_height;
    gboolean width_content_changed;
} GncDropDownWidthData;

static void
drop_down_width_data_free (GncDropDownWidthData *data)
{
    if (!data)
        return;

    if (data->model && data->model_items_changed_id)
        g_signal_handler_disconnect (data->model, data->model_items_changed_id);
    if (data->settings && data->settings_theme_changed_id)
        g_signal_handler_disconnect (data->settings, data->settings_theme_changed_id);
    if (data->settings && data->settings_dark_theme_changed_id)
        g_signal_handler_disconnect (data->settings, data->settings_dark_theme_changed_id);
    if (data->settings && data->settings_font_changed_id)
        g_signal_handler_disconnect (data->settings, data->settings_font_changed_id);
    g_clear_object (&data->model);
    g_clear_object (&data->settings);
    g_free (data);
}

static GncDropDownWidthData *
drop_down_width_data (GtkDropDown *drop_down)
{
    return g_object_get_data (G_OBJECT (drop_down), GNC_DROP_DOWN_WIDTH_DATA);
}

static gboolean
drop_down_item_text (GtkDropDown *drop_down, GObject *item, GValue *value)
{
    GtkExpression *expression = gtk_drop_down_get_expression (drop_down);

    if (expression)
    {
        if (!gtk_expression_evaluate (expression, item, value) ||
            !G_VALUE_HOLDS_STRING (value))
        {
            if (G_IS_VALUE (value))
                g_value_unset (value);
            return FALSE;
        }
        if (!g_value_get_string (value))
        {
            g_value_unset (value);
            return FALSE;
        }
        return TRUE;
    }

    if (!GTK_IS_STRING_OBJECT (item))
        return FALSE;

    g_value_init (value, G_TYPE_STRING);
    g_value_set_string (value, gtk_string_object_get_string (GTK_STRING_OBJECT (item)));
    return TRUE;
}

static gint
drop_down_text_width (PangoLayout *layout, const gchar *text)
{
    gint width = 0;

    pango_layout_set_text (layout, text, -1);
    pango_layout_get_pixel_size (layout, &width, NULL);
    return width;
}

static void
drop_down_width_items_changed (GListModel *model, guint position,
                               guint removed, guint added,
                               GtkDropDown *drop_down)
{
    GncDropDownWidthData *data = drop_down_width_data (drop_down);

    if (data)
        data->width_content_changed = TRUE;
    gnc_gtk_drop_down_normalize_width (drop_down);
    (void)model;
    (void)position;
    (void)removed;
    (void)added;
}

static void
drop_down_width_content_changed (GtkDropDown *drop_down, GParamSpec *pspec,
                                 gpointer user_data)
{
    GncDropDownWidthData *data = drop_down_width_data (drop_down);

    if (data)
        data->width_content_changed = TRUE;
    gnc_gtk_drop_down_normalize_width (drop_down);
    (void)pspec;
    (void)user_data;
}

static void
drop_down_width_selection_changed (GtkDropDown *drop_down, GParamSpec *pspec,
                                   gpointer user_data)
{
    gnc_gtk_drop_down_normalize_width (drop_down);
    (void)pspec;
    (void)user_data;
}

static void
drop_down_width_theme_changed (GtkSettings *settings, GParamSpec *pspec,
                               GtkDropDown *drop_down)
{
    GncDropDownWidthData *data = drop_down_width_data (drop_down);

    if (data)
        data->width_content_changed = TRUE;
    gnc_gtk_drop_down_normalize_width (drop_down);
    (void)settings;
    (void)pspec;
}

static void
drop_down_width_connect_model (GtkDropDown *drop_down,
                                GncDropDownWidthData *data)
{
    GListModel *model = gtk_drop_down_get_model (drop_down);

    if (data->model == model)
        return;

    if (data->model && data->model_items_changed_id)
        g_signal_handler_disconnect (data->model, data->model_items_changed_id);
    data->model_items_changed_id = 0;
    g_set_object (&data->model, model);
    if (data->model)
        data->model_items_changed_id = g_signal_connect
            (data->model, "items-changed", G_CALLBACK (drop_down_width_items_changed),
             drop_down);
}

static void
drop_down_width_restore_original_request (GtkDropDown *drop_down,
                                          GncDropDownWidthData *data)
{
    data->requested_min_width = data->original_min_width;
    data->requested_min_height = data->original_min_height;
    gtk_widget_set_size_request (GTK_WIDGET (drop_down),
                                 data->requested_min_width,
                                 data->requested_min_height);
}

void
gnc_gtk_drop_down_normalize_width (GtkDropDown *drop_down)
{
    GncDropDownWidthData *data;
    GListModel *model;
    PangoLayout *layout;
    GObject *selected_item;
    gint selected_width;
    gint widest_width = 0;
    gint current_min_width;
    gint current_min_height;
    gint minimum;
    gint natural;
    gint chrome;
    gboolean have_widest_text = FALSE;

    g_return_if_fail (GTK_IS_DROP_DOWN (drop_down));

    data = drop_down_width_data (drop_down);
    if (!data)
    {
        data = g_new0 (GncDropDownWidthData, 1);
        gtk_widget_get_size_request (GTK_WIDGET (drop_down),
                                     &data->original_min_width,
                                     &data->original_min_height);
        data->requested_min_width = data->original_min_width;
        data->requested_min_height = data->original_min_height;
        g_object_set_data_full (G_OBJECT (drop_down), GNC_DROP_DOWN_WIDTH_DATA,
                                data, (GDestroyNotify)drop_down_width_data_free);
        g_signal_connect (drop_down, "notify::model",
                          G_CALLBACK (drop_down_width_content_changed), NULL);
        g_signal_connect (drop_down, "notify::expression",
                          G_CALLBACK (drop_down_width_content_changed), NULL);
        g_signal_connect (drop_down, "notify::selected",
                          G_CALLBACK (drop_down_width_selection_changed), NULL);
        g_signal_connect (drop_down, "notify::scale-factor",
                          G_CALLBACK (drop_down_width_content_changed), NULL);
        g_set_object (&data->settings, gtk_settings_get_for_display
                      (gtk_widget_get_display (GTK_WIDGET (drop_down))));
        if (data->settings)
        {
            GObjectClass *settings_class = G_OBJECT_GET_CLASS (data->settings);

            if (g_object_class_find_property (settings_class, "gtk-theme-name"))
                data->settings_theme_changed_id = g_signal_connect
                    (data->settings, "notify::gtk-theme-name",
                     G_CALLBACK (drop_down_width_theme_changed), drop_down);
            if (g_object_class_find_property
                (settings_class, "gtk-application-prefer-dark-theme"))
                data->settings_dark_theme_changed_id = g_signal_connect
                    (data->settings, "notify::gtk-application-prefer-dark-theme",
                     G_CALLBACK (drop_down_width_theme_changed), drop_down);
            if (g_object_class_find_property (settings_class, "gtk-font-name"))
                data->settings_font_changed_id = g_signal_connect
                    (data->settings, "notify::gtk-font-name",
                     G_CALLBACK (drop_down_width_theme_changed), drop_down);
        }
    }

    gtk_widget_get_size_request (GTK_WIDGET (drop_down), &current_min_width,
                                 &current_min_height);
    if (current_min_width != data->requested_min_width)
    {
        data->original_min_width = current_min_width;
        data->width_content_changed = TRUE;
    }
    if (current_min_height != data->requested_min_height)
    {
        data->original_min_height = current_min_height;
        data->width_content_changed = TRUE;
    }
    drop_down_width_connect_model (drop_down, data);
    model = data->model;
    if (!model || !g_list_model_get_n_items (model))
    {
        drop_down_width_restore_original_request (drop_down, data);
        return;
    }

    selected_item = gtk_drop_down_get_selected_item (drop_down);
    if (!selected_item)
    {
        drop_down_width_restore_original_request (drop_down, data);
        return;
    }

    layout = gtk_widget_create_pango_layout (GTK_WIDGET (drop_down), NULL);
    if (!layout)
        return;

    {
        GValue value = G_VALUE_INIT;

        if (!drop_down_item_text (drop_down, selected_item, &value))
        {
            g_object_unref (layout);
            drop_down_width_restore_original_request (drop_down, data);
            return;
        }
        selected_width = drop_down_text_width (layout, g_value_get_string (&value));
        g_value_unset (&value);
    }

    for (guint position = 0; position < g_list_model_get_n_items (model); position++)
    {
        GObject *item = g_list_model_get_item (model, position);
        GValue value = G_VALUE_INIT;

        if (item && drop_down_item_text (drop_down, item, &value))
        {
            widest_width = MAX (widest_width,
                                drop_down_text_width (layout, g_value_get_string (&value)));
            have_widest_text = TRUE;
        }
        if (G_IS_VALUE (&value))
            g_value_unset (&value);
        g_clear_object (&item);
    }
    g_object_unref (layout);

    if (!have_widest_text)
    {
        drop_down_width_restore_original_request (drop_down, data);
        return;
    }

    gtk_widget_set_size_request (GTK_WIDGET (drop_down), -1, -1);
    gtk_widget_measure (GTK_WIDGET (drop_down), GTK_ORIENTATION_HORIZONTAL, -1,
                        &minimum, &natural, NULL, NULL);
    chrome = MAX (0, natural - selected_width);
    data->requested_min_width = MAX (data->original_min_width,
                                     widest_width + chrome);
    if (!data->width_content_changed)
        data->requested_min_width = MAX (data->requested_min_width,
                                         current_min_width);
    data->requested_min_height = data->original_min_height;
    gtk_widget_set_size_request (GTK_WIDGET (drop_down), data->requested_min_width,
                                 data->requested_min_height);
    data->width_content_changed = FALSE;
}

GtkDropDown *
gnc_gtk_drop_down_new (GListModel *model, GtkExpression *expression)
{
    GtkDropDown *drop_down = GTK_DROP_DOWN (gtk_drop_down_new (model, expression));

    gnc_gtk_drop_down_normalize_width (drop_down);
    return drop_down;
}

GtkDropDown *
gnc_gtk_drop_down_new_from_strings (const char * const *strings)
{
    GtkDropDown *drop_down = GTK_DROP_DOWN (gtk_drop_down_new_from_strings (strings));

    gnc_gtk_drop_down_normalize_width (drop_down);
    return drop_down;
}

void
gnc_window_bind_to_application (GtkWindow *window)
{
    GApplication *application;

    g_return_if_fail (GTK_IS_WINDOW (window));

    application = g_application_get_default ();
    if (GTK_IS_APPLICATION (application))
        gtk_window_set_application (window, GTK_APPLICATION (application));
}

GdkTexture *
gnc_texture_new_from_pixbuf (GdkPixbuf *pixbuf)
{
    GBytes *bytes;
    GError *error = NULL;
    GdkTexture *texture;
    gchar *encoded = NULL;
    gsize encoded_size = 0;

    g_return_val_if_fail (GDK_IS_PIXBUF (pixbuf), NULL);

    if (!gdk_pixbuf_save_to_buffer (pixbuf, &encoded, &encoded_size,
                                    "png", &error, NULL))
    {
        PWARN ("Unable to encode pixbuf as PNG: %s",
               error ? error->message : "unknown error");
        g_clear_error (&error);
        return NULL;
    }

    bytes = g_bytes_new_take (encoded, encoded_size);
    texture = gdk_texture_new_from_bytes (bytes, &error);
    g_bytes_unref (bytes);

    if (!texture)
    {
        PWARN ("Unable to create texture from PNG data: %s",
               error ? error->message : "unknown error");
        g_clear_error (&error);
    }

    return texture;
}

typedef struct
{
    guint context_id;
    guint message_id;
    gchar *text;
} GncStatusbarMessage;

typedef struct
{
    GtkLabel *label;
    GPtrArray *messages;
    guint next_message_id;
} GncStatusbarData;

#define GNC_STATUSBAR_DATA_KEY "gnc-statusbar-data"

static void
statusbar_message_free (GncStatusbarMessage *message)
{
    if (!message)
        return;

    g_free (message->text);
    g_free (message);
}

static void
statusbar_data_free (GncStatusbarData *data)
{
    if (!data)
        return;

    g_clear_pointer (&data->messages, g_ptr_array_unref);
    g_free (data);
}

static GncStatusbarData *
statusbar_data (GtkWidget *statusbar)
{
    return GTK_IS_WIDGET (statusbar)
        ? g_object_get_data (G_OBJECT (statusbar), GNC_STATUSBAR_DATA_KEY)
        : NULL;
}

static void
statusbar_refresh (GncStatusbarData *data)
{
    const gchar *text = " ";

    if (data->messages->len)
    {
        GncStatusbarMessage *message =
            g_ptr_array_index (data->messages, data->messages->len - 1);
        text = message->text;
    }
    gtk_label_set_text (data->label, text);
}

GtkWidget *
gnc_statusbar_new (void)
{
    GncStatusbarData *data = g_new0 (GncStatusbarData, 1);
    GtkWidget *statusbar = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    GtkWidget *label = gtk_label_new (" ");

    data->label = GTK_LABEL (label);
    data->messages = g_ptr_array_new_with_free_func (
        (GDestroyNotify)statusbar_message_free);
    gtk_widget_add_css_class (statusbar, "statusbar");
    gtk_widget_set_hexpand (label, TRUE);
    gtk_label_set_xalign (data->label, 0.0);
    gtk_box_append (GTK_BOX (statusbar), label);
    g_object_set_data_full (G_OBJECT (statusbar), GNC_STATUSBAR_DATA_KEY,
                            data, (GDestroyNotify)statusbar_data_free);
    return statusbar;
}

gboolean
gnc_statusbar_is (GtkWidget *statusbar)
{
    return statusbar_data (statusbar) != NULL;
}

guint
gnc_statusbar_push (GtkWidget *statusbar, guint context_id, const gchar *text)
{
    GncStatusbarData *data = statusbar_data (statusbar);
    GncStatusbarMessage *message;

    g_return_val_if_fail (data != NULL, 0);

    message = g_new0 (GncStatusbarMessage, 1);
    message->context_id = context_id;
    message->message_id = ++data->next_message_id;
    if (message->message_id == 0)
        message->message_id = ++data->next_message_id;
    message->text = g_strdup (text ? text : " ");
    g_ptr_array_add (data->messages, message);
    statusbar_refresh (data);
    return message->message_id;
}

void
gnc_statusbar_pop (GtkWidget *statusbar, guint context_id)
{
    GncStatusbarData *data = statusbar_data (statusbar);

    g_return_if_fail (data != NULL);

    for (guint index = data->messages->len; index > 0; index--)
    {
        GncStatusbarMessage *message =
            g_ptr_array_index (data->messages, index - 1);
        if (message->context_id == context_id)
        {
            g_ptr_array_remove_index (data->messages, index - 1);
            break;
        }
    }
    statusbar_refresh (data);
}

void
gnc_statusbar_remove (GtkWidget *statusbar, guint context_id, guint message_id)
{
    GncStatusbarData *data = statusbar_data (statusbar);

    g_return_if_fail (data != NULL);

    for (guint index = 0; index < data->messages->len; index++)
    {
        GncStatusbarMessage *message = g_ptr_array_index (data->messages, index);
        if (message->context_id == context_id && message->message_id == message_id)
        {
            g_ptr_array_remove_index (data->messages, index);
            break;
        }
    }
    statusbar_refresh (data);
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

static gpointer
find_widget_func (GtkWidget *widget, const gchar *id)
{
    const gchar *name;

    if (!widget)
        return NULL;

    name = gtk_buildable_get_buildable_id (GTK_BUILDABLE(widget));
    GtkWidget *ret = NULL;

    if (g_strcmp0 (name, id) == 0)
        return widget;

    GtkWidget *child;
    for (child = gtk_widget_get_first_child (GTK_WIDGET(widget));
         child != NULL;
         child = gtk_widget_get_next_sibling (GTK_WIDGET(child)))
    {
            ret = find_widget_func (child, id);
            if (ret)
                break;
    }
    return ret;
}

/** Find the Widget defined by id below a widget root.
 *
 *  @param root The root widget to search.
 *  @param id The GTK Builder ID to find.
 *  @returns The widget defined by id or NULL.
 */
GtkWidget *
gnc_get_widget_from_id (GtkWidget *root, const gchar *id)
{
    g_return_val_if_fail (GTK_IS_WIDGET (root), NULL);
    g_return_val_if_fail (id != NULL, NULL);

    return find_widget_func (root, id);
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


/* The controller owns registered shortcuts. Keep a second reference in this
 * array so that a menu-model update can remove all previous bindings before
 * recreating them. */
#define GNC_MENU_SHORTCUTS "gnc-menu-shortcuts"
#define GNC_ACCELERATOR_MAP_PREFIX "<Actions>/"
#define GNC_MENU_ORIGINAL_ACCELERATOR "gnc-original-accel"

static GHashTable *accelerator_overrides;

static gchar *
legacy_accelerator_action_to_menu_action (const gchar *legacy_action)
{
    const gchar *action;
    const gchar *separator;

    if (!g_str_has_prefix (legacy_action, GNC_ACCELERATOR_MAP_PREFIX))
        return NULL;

    action = legacy_action + strlen (GNC_ACCELERATOR_MAP_PREFIX);
    separator = strrchr (action, '/');
    if (!separator || separator == action || !separator[1])
        return NULL;

    return g_strdup_printf ("%.*s.%s", (gint)(separator - action), action,
                            separator + 1);
}

void
gnc_accelerator_overrides_clear (void)
{
    g_clear_pointer (&accelerator_overrides, g_hash_table_unref);
}

void
gnc_accelerator_overrides_load_legacy_map (const gchar *filename)
{
    GRegex *entry_regex;
    gchar *contents = NULL;
    gchar **lines;
    GError *error = NULL;

    gnc_accelerator_overrides_clear ();

    if (!filename || !*filename || !g_file_test (filename, G_FILE_TEST_EXISTS))
        return;

    if (!g_file_get_contents (filename, &contents, NULL, &error))
    {
        PWARN ("Unable to load accelerator map '%s': %s", filename,
               error->message);
        g_clear_error (&error);
        return;
    }

    accelerator_overrides = g_hash_table_new_full (g_str_hash, g_str_equal,
                                                     g_free, g_free);
    entry_regex = g_regex_new ("^\\s*\\(gtk_accel_path\\s+\"([^\"]+)\"\\s+\"([^\"]*)\"\\s*\\)\\s*$",
                               G_REGEX_OPTIMIZE, 0, NULL);
    lines = g_strsplit (contents, "\n", -1);

    for (gchar **line = lines; *line; line++)
    {
        GMatchInfo *match_info = NULL;

        if (g_regex_match (entry_regex, *line, 0, &match_info))
        {
            gchar *legacy_action = g_match_info_fetch (match_info, 1);
            gchar *action_name = legacy_accelerator_action_to_menu_action (legacy_action);

            if (action_name)
                g_hash_table_replace (accelerator_overrides, action_name,
                                      g_match_info_fetch (match_info, 2));
            g_free (legacy_action);
        }
        if (match_info)
            g_match_info_free (match_info);
    }

    g_strfreev (lines);
    g_regex_unref (entry_regex);
    g_free (contents);
}

gboolean
gnc_accelerator_overrides_lookup (const gchar *action_name,
                                  const gchar **accelerator)
{
    if (accelerator)
        *accelerator = NULL;

    g_return_val_if_fail (action_name != NULL, FALSE);

    if (!accelerator_overrides ||
        !g_hash_table_contains (accelerator_overrides, action_name))
        return FALSE;

    if (accelerator)
        *accelerator = g_hash_table_lookup (accelerator_overrides, action_name);
    return TRUE;
}

static gchar *
accelerator_for_platform (const gchar *accelerator)
{
#ifdef MAC_INTEGRATION
    if (g_strstr_len (accelerator, -1, "<Primary>"))
    {
        gchar **parts = g_strsplit (accelerator, "<Primary>", -1);
        gchar *mac_accelerator = g_strjoinv ("<Meta>", parts);

        g_strfreev (parts);
        return mac_accelerator;
    }
#endif
    return g_strdup (accelerator);
}

GtkShortcutTrigger *
gnc_accelerator_trigger_parse (const gchar *accelerator)
{
    GtkShortcutTrigger *trigger;
    gchar *platform_accelerator;

    g_return_val_if_fail (accelerator != NULL, NULL);

    platform_accelerator = accelerator_for_platform (accelerator);
    trigger = gtk_shortcut_trigger_parse_string (platform_accelerator);
    g_free (platform_accelerator);
    return trigger;
}

void
gnc_menu_model_apply_accelerators (GMenuModel *model)
{
    g_return_if_fail (G_IS_MENU_MODEL (model));

    for (gint index = 0; index < g_menu_model_get_n_items (model); index++)
    {
        gchar *action_name = NULL;
        gchar *original = NULL;
        gchar *current = NULL;
        const gchar *override = NULL;
        gboolean saved_original;

        saved_original = g_menu_model_get_item_attribute (model, index,
                                                           GNC_MENU_ORIGINAL_ACCELERATOR,
                                                           "s", &original);
        g_menu_model_get_item_attribute (model, index,
                                         GNC_MENU_ATTRIBUTE_ACCELERATOR, "s", &current);
        if (!saved_original)
            original = g_strdup (current);

        g_menu_model_get_item_attribute (model, index, G_MENU_ATTRIBUTE_ACTION,
                                         "s", &action_name);
        if (action_name)
            gnc_accelerator_overrides_lookup (action_name, &override);

        if ((override || original) && G_IS_MENU (model))
        {
            gchar *displayed = accelerator_for_platform (override ? override : original);

            if (g_strcmp0 (current, displayed) != 0)
            {
                GMenuItem *item = g_menu_item_new_from_model (model, index);

                if (!saved_original)
                    g_menu_item_set_attribute (item, GNC_MENU_ORIGINAL_ACCELERATOR,
                                               "s", original ? original : "");
                g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_ACCELERATOR,
                                           "s", displayed);
                g_menu_remove (G_MENU (model), index);
                g_menu_insert_item (G_MENU (model), index, item);
                g_object_unref (item);
            }
            g_free (displayed);
        }

        const gchar *link_names[] = { G_MENU_LINK_SECTION, G_MENU_LINK_SUBMENU };
        for (guint link_index = 0; link_index < G_N_ELEMENTS (link_names); link_index++)
        {
            GMenuModel *linked_model = g_menu_model_get_item_link (model, index,
                                                                    link_names[link_index]);
            if (linked_model)
            {
                gnc_menu_model_apply_accelerators (linked_model);
                g_object_unref (linked_model);
            }
        }

        g_free (action_name);
        g_free (original);
        g_free (current);
    }
}

static void
clear_menu_shortcuts (GtkShortcutController *shortcut_controller,
                      GPtrArray *shortcuts)
{
    for (guint index = 0; index < shortcuts->len; index++)
        gtk_shortcut_controller_remove_shortcut (shortcut_controller,
                                                 GTK_SHORTCUT (g_ptr_array_index (shortcuts, index)));
    g_ptr_array_set_size (shortcuts, 0);
}

static void
add_menu_shortcuts (GMenuModel *model,
                    GtkShortcutController *shortcut_controller,
                    GPtrArray *shortcuts)
{
    for (gint index = 0; index < g_menu_model_get_n_items (model); index++)
    {
        gchar *action_name = NULL;

        if (g_menu_model_get_item_attribute (model, index, G_MENU_ATTRIBUTE_ACTION,
                                              "s", &action_name))
        {
            const gchar *override = NULL;
            gchar *attr_accelerator = NULL;
            const gchar *accelerator = NULL;

            if (gnc_accelerator_overrides_lookup (action_name, &override))
                accelerator = override;
            else if (g_menu_model_get_item_attribute (model, index,
                                                      GNC_MENU_ATTRIBUTE_ACCELERATOR,
                                                      "s", &attr_accelerator))
                accelerator = attr_accelerator;

            if (accelerator && *accelerator)
            {
                GtkShortcutTrigger *trigger = gnc_accelerator_trigger_parse (accelerator);
                if (trigger)
                {
                    GtkShortcutAction *action = gtk_named_action_new (action_name);
                    GtkShortcut *shortcut = gtk_shortcut_new (trigger, action);
                    g_ptr_array_add (shortcuts, g_object_ref (shortcut));
                    gtk_shortcut_controller_add_shortcut (shortcut_controller, shortcut);
                }
                else
                    PWARN ("Ignoring invalid accelerator '%s' for action '%s'", accelerator,
                           action_name);
            }

            g_free (attr_accelerator);
            g_free (action_name);
        }

        const gchar *link_names[] = { G_MENU_LINK_SECTION, G_MENU_LINK_SUBMENU };
        for (guint link_index = 0; link_index < G_N_ELEMENTS (link_names); link_index++)
        {
            GMenuModel *linked_model = g_menu_model_get_item_link (model, index,
                                                                    link_names[link_index]);
            if (linked_model)
            {
                add_menu_shortcuts (linked_model, shortcut_controller, shortcuts);
                g_object_unref (linked_model);
            }
        }
    }
}

/** Add accelerator keys for menu item widgets
 *
 *  @param menu The menu widget.
 *
 *  @param model The menu bar model.
 *
 *  @param shortcut_controller The window shortcut controller to update.
 */
void
gnc_add_accelerator_keys_for_menu (GtkWidget *menu, GMenuModel *model, GtkEventController *shortcut_controller)
{
    GPtrArray *shortcuts;

    g_return_if_fail (GTK_IS_WIDGET(menu));
    g_return_if_fail (model != NULL);
    g_return_if_fail (GTK_IS_SHORTCUT_CONTROLLER (shortcut_controller));

    shortcuts = g_object_get_data (G_OBJECT (shortcut_controller), GNC_MENU_SHORTCUTS);
    if (!shortcuts)
    {
        shortcuts = g_ptr_array_new_with_free_func (g_object_unref);
        g_object_set_data_full (G_OBJECT (shortcut_controller), GNC_MENU_SHORTCUTS,
                                shortcuts, (GDestroyNotify)g_ptr_array_unref);
    }

    gnc_menu_model_apply_accelerators (model);
    clear_menu_shortcuts (GTK_SHORTCUT_CONTROLLER (shortcut_controller), shortcuts);
    add_menu_shortcuts (model, GTK_SHORTCUT_CONTROLLER (shortcut_controller), shortcuts);
}


static gchar *
menu_model_find_tooltip_by_action (GMenuModel  *model,
                                   const gchar *action_name)
{
    g_return_val_if_fail (G_IS_MENU_MODEL (model), NULL);
    g_return_val_if_fail (action_name != NULL, NULL);

    for (gint index = 0; index < g_menu_model_get_n_items (model); index++)
    {
        g_autofree gchar *item_action = NULL;

        if (g_menu_model_get_item_attribute (model, index, G_MENU_ATTRIBUTE_ACTION,
                                             "s", &item_action) &&
            g_str_has_suffix (item_action, action_name))
        {
            gchar *tooltip = NULL;

            g_menu_model_get_item_attribute (model, index,
                                             GNC_MENU_ATTRIBUTE_TOOLTIP,
                                             "s", &tooltip);
            return tooltip;
        }

        GMenuLinkIter *iter = g_menu_model_iterate_item_links (model, index);
        GMenuModel *sub_model;

        while (g_menu_link_iter_get_next (iter, NULL, &sub_model))
        {
            gchar *tooltip = menu_model_find_tooltip_by_action (sub_model, action_name);

            g_object_unref (sub_model);
            if (tooltip)
            {
                g_object_unref (iter);
                return tooltip;
            }
        }
        g_object_unref (iter);
    }
    return NULL;
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
    GtkWidget *ret = NULL;

    g_return_val_if_fail (action_name != NULL, NULL);

    GtkWidget *child;
    for (child = gtk_widget_get_first_child (GTK_WIDGET(toolbar));
         child != NULL;
         child = gtk_widget_get_next_sibling (GTK_WIDGET(child)))
    {
        if (GTK_IS_ACTIONABLE(child))
        {
            // this returns the full action name
            const gchar *item_action_name = gtk_actionable_get_action_name (GTK_ACTIONABLE(child));

            if (g_str_has_suffix (item_action_name, action_name))
            {
                ret = child;
                break;
            }
        }
    }
    return ret;
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

/** Find a GMenuModel item by action name, action label, and optional target.
 *
 *  The matching model and item index are stored in @a gsm. The model remains
 *  authoritative; callers must not depend on implementation-specific menu
 *  widgets generated by GtkPopoverMenuBar.
 *
 *  @param gsm The GncMenuModelSearch structure.
 *
 *  @return TRUE if a model item was found or FALSE otherwise.
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


typedef struct
{
    gchar     *action_name;
    GMenuItem *item;
    gboolean   visible;
} GncMenuVisibilitySlot;

typedef struct
{
    GMenu     *model;
    GPtrArray *slots;
} GncMenuVisibilityOrder;

typedef struct
{
    GHashTable *orders;
    GHashTable *hidden_slots;
} GncMenuVisibilityState;

#define GNC_MENU_VISIBILITY_STATE "gnc-menu-visibility-state"

static void
menu_visibility_slot_free (GncMenuVisibilitySlot *slot)
{
    if (!slot)
        return;

    g_clear_object (&slot->item);
    g_free (slot->action_name);
    g_free (slot);
}

static void
menu_visibility_order_free (GncMenuVisibilityOrder *order)
{
    if (!order)
        return;

    g_clear_object (&order->model);
    g_clear_pointer (&order->slots, g_ptr_array_unref);
    g_free (order);
}

static void
menu_visibility_state_free (GncMenuVisibilityState *state)
{
    if (!state)
        return;

    g_clear_pointer (&state->orders, g_hash_table_unref);
    g_clear_pointer (&state->hidden_slots, g_hash_table_unref);
    g_free (state);
}

static GncMenuVisibilityState *
menu_visibility_state_new (void)
{
    GncMenuVisibilityState *state = g_new0 (GncMenuVisibilityState, 1);

    state->orders = g_hash_table_new_full (g_direct_hash, g_direct_equal, NULL,
                                           (GDestroyNotify)menu_visibility_order_free);
    state->hidden_slots = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    return state;
}

static GncMenuVisibilityOrder *
menu_visibility_state_get_order (GncMenuVisibilityState *state,
                                 GMenu                  *model)
{
    GncMenuVisibilityOrder *order = g_hash_table_lookup (state->orders, model);

    if (order)
        return order;

    order = g_new0 (GncMenuVisibilityOrder, 1);
    order->model = g_object_ref (model);
    order->slots = g_ptr_array_new_with_free_func ((GDestroyNotify)menu_visibility_slot_free);

    for (gint index = 0; index < g_menu_model_get_n_items (G_MENU_MODEL (model)); index++)
    {
        GncMenuVisibilitySlot *slot = g_new0 (GncMenuVisibilitySlot, 1);

        g_menu_model_get_item_attribute (G_MENU_MODEL (model), index,
                                         G_MENU_ATTRIBUTE_ACTION, "s",
                                         &slot->action_name);
        slot->item = g_menu_item_new_from_model (G_MENU_MODEL (model), index);
        slot->visible = TRUE;
        g_ptr_array_add (order->slots, slot);
    }

    g_hash_table_insert (state->orders, order->model, order);
    return order;
}

static GncMenuVisibilitySlot *
menu_visibility_order_get_visible_slot (GncMenuVisibilityOrder *order,
                                        gint                    visible_index)
{
    gint index = 0;

    for (guint slot_index = 0; slot_index < order->slots->len; slot_index++)
    {
        GncMenuVisibilitySlot *slot = g_ptr_array_index (order->slots, slot_index);

        if (!slot->visible)
            continue;
        if (index == visible_index)
            return slot;
        index++;
    }
    return NULL;
}

static GncMenuVisibilityOrder *
menu_visibility_state_find_slot_order (GncMenuVisibilityState *state,
                                       GncMenuVisibilitySlot  *needle)
{
    GHashTableIter iter;
    gpointer value;

    g_hash_table_iter_init (&iter, state->orders);
    while (g_hash_table_iter_next (&iter, NULL, &value))
    {
        GncMenuVisibilityOrder *order = value;

        for (guint index = 0; index < order->slots->len; index++)
        {
            if (g_ptr_array_index (order->slots, index) == needle)
                return order;
        }
    }
    return NULL;
}

static gint
menu_visibility_order_insertion_index (GncMenuVisibilityOrder *order,
                                       GncMenuVisibilitySlot  *needle)
{
    gint insertion_index = 0;

    for (guint index = 0; index < order->slots->len; index++)
    {
        GncMenuVisibilitySlot *slot = g_ptr_array_index (order->slots, index);

        if (slot == needle)
            return insertion_index;
        if (slot->visible)
            insertion_index++;
    }
    return -1;
}

static void
menu_visibility_state_restore (GncMenuVisibilityState *state)
{
    GHashTableIter order_iter;
    gpointer value;

    g_hash_table_iter_init (&order_iter, state->orders);
    while (g_hash_table_iter_next (&order_iter, NULL, &value))
    {
        GncMenuVisibilityOrder *order = value;

        for (guint index = 0; index < order->slots->len; index++)
        {
            GncMenuVisibilitySlot *slot = g_ptr_array_index (order->slots, index);

            if (!slot->visible)
            {
                gint insertion_index = menu_visibility_order_insertion_index (order, slot);

                g_menu_insert_item (order->model, insertion_index, slot->item);
                slot->visible = TRUE;
            }
        }
    }
}

static void
menu_visibility_state_clear (GMenuModel *menu_model)
{
    GncMenuVisibilityState *state = g_object_get_data (G_OBJECT (menu_model),
                                                        GNC_MENU_VISIBILITY_STATE);

    if (!state)
        return;

    menu_visibility_state_restore (state);
    g_object_set_data (G_OBJECT (menu_model), GNC_MENU_VISIBILITY_STATE, NULL);
}

static gboolean
menu_model_has_action (GMenuModel  *menu_model,
                       const gchar *action_name)
{
    GncMenuModelSearch gsm = { 0 };

    gsm.search_action_name = action_name;
    return gnc_menubar_model_find_item (menu_model, &gsm);
}

gboolean
gnc_menubar_model_set_item_visible (GMenuModel  *menu_model,
                                    const gchar *action_name,
                                    gboolean     visible)
{
    GncMenuVisibilityState *state;
    GncMenuVisibilitySlot *slot;

    g_return_val_if_fail (G_IS_MENU_MODEL (menu_model), FALSE);
    g_return_val_if_fail (action_name != NULL, FALSE);

    state = g_object_get_data (G_OBJECT (menu_model), GNC_MENU_VISIBILITY_STATE);
    if (visible)
    {
        if (!state)
            return menu_model_has_action (menu_model, action_name);

        slot = g_hash_table_lookup (state->hidden_slots, action_name);
        if (!slot)
            return menu_model_has_action (menu_model, action_name);

        GncMenuVisibilityOrder *order = menu_visibility_state_find_slot_order (state, slot);
        gint insertion_index = order ? menu_visibility_order_insertion_index (order, slot) : -1;

        if (!order || insertion_index < 0)
            return FALSE;

        g_menu_insert_item (order->model, insertion_index, slot->item);
        slot->visible = TRUE;
        g_hash_table_remove (state->hidden_slots, action_name);
        return TRUE;
    }

    if (state && g_hash_table_contains (state->hidden_slots, action_name))
        return TRUE;

    GncMenuModelSearch gsm = { 0 };
    gsm.search_action_name = action_name;

    if (!gnc_menubar_model_find_item (menu_model, &gsm) || !G_IS_MENU (gsm.model))
        return FALSE;

    if (!state)
    {
        state = menu_visibility_state_new ();
        g_object_set_data_full (G_OBJECT (menu_model), GNC_MENU_VISIBILITY_STATE, state,
                                (GDestroyNotify)menu_visibility_state_free);
    }

    GncMenuVisibilityOrder *order = menu_visibility_state_get_order (state, G_MENU (gsm.model));
    slot = menu_visibility_order_get_visible_slot (order, gsm.index);
    if (!slot)
        return FALSE;

    g_menu_remove (G_MENU (gsm.model), gsm.index);
    slot->visible = FALSE;
    g_hash_table_insert (state->hidden_slots, g_strdup (action_name), slot);
    return TRUE;
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
    GncMenuModelSearch gsm = { 0 };
    gboolean changed = FALSE;

    g_return_val_if_fail (G_IS_MENU_MODEL (menu_model), FALSE);
    g_return_val_if_fail (action_name != NULL, FALSE);

    menu_visibility_state_clear (menu_model);
    gsm.search_action_name = action_name;
    gsm.search_action_target = target;

    if (!gnc_menubar_model_find_item (menu_model, &gsm) || !G_IS_MENU (gsm.model))
        return FALSE;

    g_autofree gchar *old_label = NULL;
    g_autofree gchar *old_accel = NULL;
    g_autofree gchar *old_tooltip = NULL;

    if (label)
    {
        g_menu_model_get_item_attribute (gsm.model, gsm.index,
                                         G_MENU_ATTRIBUTE_LABEL, "s", &old_label);
        changed |= g_strcmp0 (old_label, label) != 0;
    }
    if (accel_name)
    {
        g_menu_model_get_item_attribute (gsm.model, gsm.index,
                                         GNC_MENU_ATTRIBUTE_ACCELERATOR,
                                         "s", &old_accel);
        changed |= g_strcmp0 (old_accel, accel_name) != 0;
    }
    if (tooltip)
    {
        g_menu_model_get_item_attribute (gsm.model, gsm.index,
                                         GNC_MENU_ATTRIBUTE_TOOLTIP,
                                         "s", &old_tooltip);
        changed |= g_strcmp0 (old_tooltip, tooltip) != 0;
    }

    if (!changed)
        return FALSE;

    GMenuItem *item = g_menu_item_new_from_model (gsm.model, gsm.index);

    if (label)
        g_menu_item_set_label (item, label);
    if (accel_name)
    {
        g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_ACCELERATOR, "s", accel_name);
        g_menu_item_set_attribute (item, GNC_MENU_ORIGINAL_ACCELERATOR, "s", accel_name);
    }
    if (tooltip)
        g_menu_item_set_attribute (item, GNC_MENU_ATTRIBUTE_TOOLTIP, "s", tooltip);

    g_menu_remove (G_MENU (gsm.model), gsm.index);
    g_menu_insert_item (G_MENU (gsm.model), gsm.index, item);
    g_object_unref (item);
    return TRUE;
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

    menu_visibility_state_clear (menu_model);

    remove_items_from_model (menu_model, &remove_list, attrib);

    g_list_foreach (remove_list, (GFunc)remove_items, NULL);
    g_list_free (remove_list);
}


static void
statusbar_push (GtkWidget *statusbar, const gchar *text)
{
    gnc_statusbar_push (statusbar, 0, text);
}

static void
statusbar_pop (GtkWidget *statusbar)
{
    gnc_statusbar_pop (statusbar, 0);
}

typedef struct
{
    GWeakRef    statusbar;
    GMenuModel *menu_model;
    gboolean    pushed;
} GncMenuTooltipBinding;

static void
menu_tooltip_binding_free (GncMenuTooltipBinding *binding)
{
    g_weak_ref_clear (&binding->statusbar);
    g_clear_object (&binding->menu_model);
    g_free (binding);
}

static void
menu_item_pointer_enter (GtkEventControllerMotion *controller,
                         double                    x,
                         double                    y,
                         GncMenuTooltipBinding    *binding)
{
    GtkWidget *widget = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));

    while (widget && !GTK_IS_ACTIONABLE (widget))
        widget = gtk_widget_get_parent (widget);

    if (!widget || binding->pushed)
        return;

    const gchar *action_name = gtk_actionable_get_action_name (GTK_ACTIONABLE (widget));
    g_autofree gchar *tooltip = action_name
        ? menu_model_find_tooltip_by_action (binding->menu_model, action_name)
        : NULL;

    if (tooltip)
    {
        GObject *statusbar = g_weak_ref_get (&binding->statusbar);

        if (statusbar)
        {
            statusbar_push (GTK_WIDGET (statusbar), tooltip);
            binding->pushed = TRUE;
            g_object_unref (statusbar);
        }
    }
    (void)x;
    (void)y;
}

static void
menu_item_pointer_leave (GtkEventControllerMotion *controller,
                         GncMenuTooltipBinding    *binding)
{
    if (binding->pushed)
    {
        GObject *statusbar = g_weak_ref_get (&binding->statusbar);

        if (statusbar)
        {
            statusbar_pop (GTK_WIDGET (statusbar));
            g_object_unref (statusbar);
        }
        binding->pushed = FALSE;
    }
    (void)controller;
}

static void
menu_widget_setup_tooltip_callback (GtkWidget  *widget,
                                    GMenuModel *menu_model,
                                    GtkWidget  *statusbar)
{
    if (!GTK_IS_ACTIONABLE (widget) ||
        g_object_get_data (G_OBJECT (widget), "gnc-menu-tooltip-controller") ||
        !gtk_actionable_get_action_name (GTK_ACTIONABLE (widget)))
        return;

    GncMenuTooltipBinding *binding = g_new0 (GncMenuTooltipBinding, 1);
    GtkEventController *motion = gtk_event_controller_motion_new ();

    g_weak_ref_init (&binding->statusbar, G_OBJECT (statusbar));
    binding->menu_model = g_object_ref (menu_model);
    g_signal_connect (motion, "enter", G_CALLBACK (menu_item_pointer_enter), binding);
    g_signal_connect_data (motion, "leave", G_CALLBACK (menu_item_pointer_leave), binding,
                           (GClosureNotify)menu_tooltip_binding_free, 0);
    gtk_widget_add_controller (widget, motion);
    g_object_set_data (G_OBJECT (widget), "gnc-menu-tooltip-controller",
                       GINT_TO_POINTER (1));
}

static void
menu_widget_setup_tooltip_callbacks (GtkWidget  *widget,
                                     GMenuModel *menu_model,
                                     GtkWidget  *statusbar)
{
    menu_widget_setup_tooltip_callback (widget, menu_model, statusbar);

    for (GtkWidget *child = gtk_widget_get_first_child (widget);
         child;
         child = gtk_widget_get_next_sibling (child))
        menu_widget_setup_tooltip_callbacks (child, menu_model, statusbar);
}

void
gnc_menubar_setup_tooltip_to_statusbar_callbacks (GtkWidget  *menubar,
                                                   GMenuModel *menu_model,
                                                   GtkWidget  *statusbar)
{
    g_return_if_fail (GTK_IS_WIDGET (menubar));
    g_return_if_fail (G_IS_MENU_MODEL (menu_model));
    g_return_if_fail (gnc_statusbar_is (statusbar));

    menu_widget_setup_tooltip_callbacks (menubar, menu_model, statusbar);
}

typedef struct
{
    GWeakRef statusbar;
    gboolean pushed;
} GncToolItemTooltipBinding;

static void
tool_item_tooltip_binding_free (GncToolItemTooltipBinding *binding)
{
    g_weak_ref_clear (&binding->statusbar);
    g_free (binding);
}

static void
tool_item_pointer_enter (GtkEventControllerMotion *controller,
                         double                    x,
                         double                    y,
                         GncToolItemTooltipBinding *binding)
{
    GtkWidget *child = gtk_event_controller_get_widget (GTK_EVENT_CONTROLLER (controller));
    GtkWidget *tool_item = child ? gtk_widget_get_parent (child) : NULL;
    const gchar *tooltip = tool_item ? gtk_widget_get_tooltip_text (tool_item) : NULL;
    GObject *statusbar = g_weak_ref_get (&binding->statusbar);

    if (statusbar)
    {
        statusbar_push (GTK_WIDGET (statusbar), tooltip);
        binding->pushed = TRUE;
        g_object_unref (statusbar);
    }
    (void)x;
    (void)y;
}

static void
tool_item_pointer_leave (GtkEventControllerMotion *controller,
                         GncToolItemTooltipBinding *binding)
{
    if (binding->pushed)
    {
        GObject *statusbar = g_weak_ref_get (&binding->statusbar);

        if (statusbar)
        {
            statusbar_pop (GTK_WIDGET (statusbar));
            g_object_unref (statusbar);
        }
        binding->pushed = FALSE;
    }
    (void)controller;
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
    g_return_if_fail (gnc_statusbar_is (statusbar));

    child = gtk_widget_get_first_child (GTK_WIDGET(tool_item));
    if (!child || g_object_get_data (G_OBJECT (child), "gnc-tool-item-tooltip-controller"))
        return;

    GncToolItemTooltipBinding *binding = g_new0 (GncToolItemTooltipBinding, 1);
    GtkEventController *motion = gtk_event_controller_motion_new ();

    g_weak_ref_init (&binding->statusbar, G_OBJECT (statusbar));
    g_signal_connect (motion, "enter", G_CALLBACK (tool_item_pointer_enter), binding);
    g_signal_connect_data (motion, "leave", G_CALLBACK (tool_item_pointer_leave), binding,
                           (GClosureNotify)tool_item_tooltip_binding_free, 0);
    gtk_widget_add_controller (child, motion);
    g_object_set_data (G_OBJECT (child), "gnc-tool-item-tooltip-controller",
                       GINT_TO_POINTER (1));

    g_object_set (G_OBJECT(tool_item), "has-tooltip", FALSE, NULL);
}
