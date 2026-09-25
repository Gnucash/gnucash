/********************************************************************\
 * dialog-utils.c -- utility functions for creating dialogs         *
 *                   for GnuCash                                    *
 * Copyright (C) 1999-2000 Linas Vepstas                            *
 * Copyright (C) 2005 David Hampton <hampton@employees.org>         *
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gmodule.h>
#ifdef HAVE_DLFCN_H
# include <dlfcn.h>
#endif

#include "dialog-utils.h"
#include "gnc-gtk-utils.h"
#include "gnc-commodity.h"
#include "gnc-date.h"
#include "gnc-path.h"
#include "gnc-engine.h"
#include "gnc-euro.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-prefs.h"
#include "gnc-main-window.h"

/* This static indicates the debugging module that this .o belongs to. */
static QofLogModule log_module = GNC_MOD_GUI;

#define GNC_PREF_LAST_GEOMETRY "last-geometry"

static GdkMonitor *
gnc_window_get_monitor (GtkWindow *window)
{
    GdkDisplay *display;
    GdkSurface *surface;
    GdkMonitor *monitor;
    GListModel *monitors;

    g_return_val_if_fail (GTK_IS_WINDOW (window), NULL);

    display = gtk_widget_get_display (GTK_WIDGET (window));
    surface = gtk_native_get_surface (GTK_NATIVE (window));
    if (surface != NULL)
    {
        monitor = gdk_display_get_monitor_at_surface (display, surface);
        return monitor ? GDK_MONITOR (g_object_ref (monitor)) : NULL;
    }

    monitors = gdk_display_get_monitors (display);
    return GDK_MONITOR (g_list_model_get_item (monitors, 0));
}

static void
gnc_window_constrain_size (GtkWindow *window, gint *width, gint *height)
{
    GdkMonitor *monitor;
    GdkRectangle geometry;

    g_return_if_fail (GTK_IS_WINDOW (window));
    g_return_if_fail (width != NULL);
    g_return_if_fail (height != NULL);

    if (*width <= 0 || *height <= 0)
        return;

    monitor = gnc_window_get_monitor (window);
    if (monitor == NULL)
        return;

    gdk_monitor_get_geometry (monitor, &geometry);
    *width = MIN (*width, MAX (1, geometry.width - 10));
    *height = MIN (*height, MAX (1, geometry.height - 10));
    g_object_unref (monitor);
}

/********************************************************************\
 * gnc_set_label_color                                              *
 *   sets the color of the label given the value                    *
 *                                                                  *
 * Args: label - gtk label widget                                   *
 *       value - value to use to set color                          *
 * Returns: none                                                    *
 \*******************************************************************/
void
gnc_set_label_color(GtkWidget *label, gnc_numeric value)
{
    gboolean deficit;

    if (!gnc_prefs_get_bool(GNC_PREFS_GROUP_GENERAL, GNC_PREF_NEGATIVE_IN_RED))
        return;

    deficit = gnc_numeric_negative_p (value);

    if (deficit)
    {
        gnc_widget_style_context_remove_class (GTK_WIDGET(label), "gnc-class-default-color");
        gnc_widget_style_context_add_class (GTK_WIDGET(label), "gnc-class-negative-numbers");
    }
    else
    {
        gnc_widget_style_context_remove_class (GTK_WIDGET(label), "gnc-class-negative-numbers");
        gnc_widget_style_context_add_class (GTK_WIDGET(label), "gnc-class-default-color");
    }
}


/********************************************************************\
 * gnc_restore_window_size                                          *
 *   restores the saved size of a window. GTK4 delegates top-level  *
 *   placement to the window system, so legacy saved coordinates are *
 *   deliberately ignored.                                          *
 *                                                                  *
 * Args: group - the preferences group to look in for saved size    *
 *       window - the window for which the size is restored         *
 *       parent - optional parent used to choose a monitor          *
 *                                                                  *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_restore_window_size (const char *group, GtkWindow *window, GtkWindow *parent)
{
    gint saved_x;
    gint saved_y;
    gint width;
    gint height;
    GVariant *geometry;

    ENTER ("");

    g_return_if_fail (group != NULL);
    g_return_if_fail (GTK_IS_WINDOW (window));

    if (!gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
        return;

    geometry = gnc_prefs_get_value (group, GNC_PREF_LAST_GEOMETRY);
    if (geometry == NULL ||
        !g_variant_is_of_type (geometry, G_VARIANT_TYPE ("(iiii)")))
    {
        g_clear_pointer (&geometry, g_variant_unref);
        return;
    }

    g_variant_get (geometry, "(iiii)", &saved_x, &saved_y, &width, &height);
    g_variant_unref (geometry);

    DEBUG ("geometry from preferences - group %s, x %d, y %d, width %d, height %d",
           group, saved_x, saved_y, width, height);

    if (width <= 0 || height <= 0)
        return;

    gnc_window_constrain_size (parent != NULL ? parent : window, &width, &height);
    gtk_window_set_default_size (window, width, height);
    LEAVE ("");
}

/********************************************************************\
 * gnc_save_window_size                                             *
 *   saves the current window size. GTK4 top-level positions are    *
 *   owned by the window system, so only width and height persist.  *
 *                                                                  *
 * Args: group - the preferences group to update                    *
 *       window - the window whose size is saved                    *
 *                                                                  *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_save_window_size (const char *group, GtkWindow *window)
{
    gint width;
    gint height;
    GVariant *geometry;

    ENTER ("");

    g_return_if_fail (group != NULL);
    g_return_if_fail (GTK_IS_WINDOW (window));

    if (!gnc_prefs_get_bool (GNC_PREFS_GROUP_GENERAL, GNC_PREF_SAVE_GEOMETRY))
        return;

    gtk_window_get_default_size (window, &width, &height);

    if (width <= 0 || height <= 0)
        return;

    DEBUG ("save geometry - width %d, height %d", width, height);
    geometry = g_variant_new ("(iiii)", -1, -1, width, height);
    gnc_prefs_set_value (group, GNC_PREF_LAST_GEOMETRY, geometry);
    LEAVE ("");
}

/********************************************************************\
 * gnc_window_adjust_for_screen                                     *
 *   constrains a window's default size to its current monitor.     *
 *                                                                  *
 * Args: window - the window to constrain                           *
 *                                                                  *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_window_adjust_for_screen (GtkWindow *window)
{
    gint width;
    gint height;
    gint adjusted_width;
    gint adjusted_height;

    ENTER ("");

    g_return_if_fail (GTK_IS_WINDOW (window));

    width = gtk_widget_get_width (GTK_WIDGET (window));
    height = gtk_widget_get_height (GTK_WIDGET (window));
    if (width <= 0 || height <= 0)
        gtk_window_get_default_size (window, &width, &height);

    if (width <= 0 || height <= 0)
        return;

    adjusted_width = width;
    adjusted_height = height;
    gnc_window_constrain_size (window, &adjusted_width, &adjusted_height);

    if (adjusted_width != width || adjusted_height != height)
        gtk_window_set_default_size (window, adjusted_width, adjusted_height);

    LEAVE ("");
}
/********************************************************************\
 * Sets the alignment of a Label Widget.                           *
 *                                                                  *
 * Args: widget - the label widget to set alignment on              *
 *       xalign - x alignment                                       *
 *       yalign - y alignment                                       *
 * Returns: nothing                                                 *
\********************************************************************/
void
gnc_label_set_alignment (GtkWidget *widget, gfloat xalign, gfloat yalign)
{
    gtk_label_set_xalign (GTK_LABEL (widget), xalign);
    gtk_label_set_yalign (GTK_LABEL (widget), yalign);
}


/********************************************************************\
 * Add a style context to a Widget so it can be altered with css    *
 *                                                                  *
 * Args:    widget - widget to add css style too                    *
 *       gnc_class - character string for css class name            *
 * Returns:  nothing                                                *
\********************************************************************/
void
gnc_widget_style_context_add_class (GtkWidget *widget, const char *gnc_class)
{
    gtk_widget_add_css_class (widget, gnc_class);
}

/********************************************************************\
 * Remove a style context class from a Widget                       *
 *                                                                  *
 * Args:    widget - widget to remove style class from              *
 *       gnc_class - character string for css class name            *
 * Returns:  nothing                                                *
\********************************************************************/
void
gnc_widget_style_context_remove_class (GtkWidget *widget, const char *gnc_class)
{
    gtk_widget_remove_css_class (widget, gnc_class);
}

/********************************************************************\
 * Draw an arrow on a Widget so it can be altered with css          *
 *                                                                  *
 * Args:     widget - widget to add arrow to in the draw callback   *
 *               cr - cairo context for the draw callback           *
 *        direction - 0 for up, 1 for down                          *
 * Returns:  TRUE, stop other handlers being invoked for the event  *
\********************************************************************/
gboolean
gnc_gdate_in_valid_range (GDate *test_date, gboolean warn)
{
    gboolean use_autoreadonly = qof_book_uses_autoreadonly (gnc_get_current_book());
    GDate *max_date = g_date_new_dmy (1,1,10000);
    GDate *min_date;
    gboolean ret = FALSE;
    gboolean max_date_ok = FALSE;
    gboolean min_date_ok = FALSE;

    if (use_autoreadonly)
        min_date = qof_book_get_autoreadonly_gdate (gnc_get_current_book());
    else
        min_date = g_date_new_dmy (1,1,1400);

    // max date
    if (g_date_compare (max_date, test_date) > 0)
        max_date_ok = TRUE;

    // min date
    if (g_date_compare (min_date, test_date) <= 0)
        min_date_ok = TRUE;

    if (use_autoreadonly && warn)
        ret = max_date_ok;
    else
        ret = min_date_ok & max_date_ok;

    if (warn && !ret)
    {
            // Translators: Use your locale date format here!
        gchar *dialog_msg = _("The entered date is out of the range "
                  "01/01/1400 - 31/12/9999, resetting to this year");
        gchar *dialog_title = _("Date out of range");
        GtkAlertDialog *dialog = gtk_alert_dialog_new ("%s", dialog_title);
        gtk_alert_dialog_set_detail (dialog, dialog_msg);
        gtk_alert_dialog_show (dialog, gnc_ui_get_main_window (NULL));
        g_object_unref (dialog);
    }
    g_date_free (max_date);
    g_date_free (min_date);
    return ret;
}


gboolean
gnc_handle_date_accelerator_input (const GncRegisterInput *input,
                                    struct tm *tm,
                                    const char *date_str)
{
    GDate gdate;
    gunichar character;

    g_return_val_if_fail (input != NULL, FALSE);
    g_return_val_if_fail (tm != NULL, FALSE);
    g_return_val_if_fail (date_str != NULL, FALSE);

    if (!input->pressed)
        return FALSE;

    if ((tm->tm_mday <= 0) || (tm->tm_mon == -1) || (tm->tm_year == -1))
        return FALSE;

    if (!g_date_valid_dmy (tm->tm_mday, tm->tm_mon + 1, tm->tm_year + 1900))
        return FALSE;

    g_date_set_dmy (&gdate,
                    tm->tm_mday,
                    tm->tm_mon + 1,
                    tm->tm_year + 1900);

    switch (input->key)
    {
    case GNC_REGISTER_KEY_KEYPAD_ADD:
    case GNC_REGISTER_KEY_PLUS:
    case GNC_REGISTER_KEY_EQUAL:
    case GNC_REGISTER_KEY_SEMICOLON:
        if (input->modifiers & GNC_REGISTER_MODIFIER_SHIFT)
            g_date_add_days (&gdate, 7);
        else if (input->modifiers & GNC_REGISTER_MODIFIER_ALT)
            g_date_add_months (&gdate, 1);
        else if (input->modifiers & GNC_REGISTER_MODIFIER_CONTROL)
            g_date_add_years (&gdate, 1);
        else
            g_date_add_days (&gdate, 1);

        if (gnc_gdate_in_valid_range (&gdate, FALSE))
            g_date_to_struct_tm (&gdate, tm);
        return TRUE;

    case GNC_REGISTER_KEY_MINUS:
    case GNC_REGISTER_KEY_KEYPAD_SUBTRACT:
    case GNC_REGISTER_KEY_UNDERSCORE:
        if ((strlen (date_str) != 0) && (dateSeparator () == '-'))
        {
            const char *cursor = date_str;
            gint separators = 0;

            while (*cursor)
            {
                if (g_utf8_get_char (cursor) == '-')
                    separators++;
                cursor = g_utf8_next_char (cursor);
            }

            if (separators < 2)
                return FALSE;
        }

        if (input->modifiers & GNC_REGISTER_MODIFIER_SHIFT)
            g_date_subtract_days (&gdate, 7);
        else if (input->modifiers & GNC_REGISTER_MODIFIER_ALT)
            g_date_subtract_months (&gdate, 1);
        else if (input->modifiers & GNC_REGISTER_MODIFIER_CONTROL)
            g_date_subtract_years (&gdate, 1);
        else
            g_date_subtract_days (&gdate, 1);

        if (gnc_gdate_in_valid_range (&gdate, FALSE))
            g_date_to_struct_tm (&gdate, tm);
        return TRUE;

    default:
        break;
    }

    if (input->modifiers & GNC_REGISTER_MODIFIER_DEFAULT)
        return FALSE;

    switch (input->key)
    {
    case GNC_REGISTER_KEY_RIGHT_BRACKET:
    case GNC_REGISTER_KEY_RIGHT_BRACE:
        g_date_add_months (&gdate, 1);
        break;
    case GNC_REGISTER_KEY_LEFT_BRACKET:
    case GNC_REGISTER_KEY_LEFT_BRACE:
        g_date_subtract_months (&gdate, 1);
        break;
    case GNC_REGISTER_KEY_OTHER:
        character = g_unichar_tolower (input->unicode_value);
        switch (character)
        {
        case 'm':
            g_date_set_day (&gdate, 1);
            break;
        case 'h':
            g_date_set_day (&gdate, 1);
            g_date_add_months (&gdate, 1);
            g_date_subtract_days (&gdate, 1);
            break;
        case 'y':
            g_date_set_day (&gdate, 1);
            g_date_set_month (&gdate, 1);
            break;
        case 'r':
            g_date_set_day (&gdate, 1);
            g_date_set_month (&gdate, 1);
            g_date_add_years (&gdate, 1);
            g_date_subtract_days (&gdate, 1);
            break;
        case 't':
            gnc_gdate_set_today (&gdate);
            break;
        default:
            return FALSE;
        }
        break;
    default:
        return FALSE;
    }

    if (gnc_gdate_in_valid_range (&gdate, FALSE))
        g_date_to_struct_tm (&gdate, tm);

    return TRUE;
}

/*--------------------------------------------------------------------------
 *   GtkBuilder support functions
 *-------------------------------------------------------------------------*/

GModule *allsymbols = NULL;

typedef struct
{
    gchar *object_id;
    gchar *signal_name;
    gchar *handler_name;
    gchar *connect_object_id;
    GConnectFlags flags;
} GncBuilderSignal;

typedef struct
{
    GPtrArray *signals;
    GHashTable *seen;
} GncBuilderSignalRegistry;

typedef struct
{
    GncBuilderSignalRegistry *registry;
    GPtrArray *object_stack;
} GncBuilderSignalCollector;

static GQuark builder_signal_registry_quark;

static void
gnc_builder_signal_free (GncBuilderSignal *signal)
{
    g_free (signal->object_id);
    g_free (signal->signal_name);
    g_free (signal->handler_name);
    g_free (signal->connect_object_id);
    g_free (signal);
}

static void
gnc_builder_signal_registry_free (GncBuilderSignalRegistry *registry)
{
    g_ptr_array_unref (registry->signals);
    g_hash_table_unref (registry->seen);
    g_free (registry);
}

static GncBuilderSignalRegistry *
gnc_builder_get_signal_registry (GtkBuilder *builder)
{
    GncBuilderSignalRegistry *registry;

    if (G_UNLIKELY (builder_signal_registry_quark == 0))
        builder_signal_registry_quark =
            g_quark_from_static_string ("gnc-builder-signal-registry");

    registry = g_object_get_qdata (G_OBJECT (builder),
                                   builder_signal_registry_quark);
    if (registry)
        return registry;

    registry = g_new0 (GncBuilderSignalRegistry, 1);
    registry->signals = g_ptr_array_new_with_free_func
        ((GDestroyNotify)gnc_builder_signal_free);
    registry->seen = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    g_object_set_qdata_full (G_OBJECT (builder), builder_signal_registry_quark,
                             registry,
                             (GDestroyNotify)gnc_builder_signal_registry_free);
    return registry;
}

static const gchar *
gnc_builder_attribute (const gchar **attribute_names,
                       const gchar **attribute_values,
                       const gchar *name)
{
    for (guint i = 0; attribute_names[i] != NULL; i++)
        if (g_str_equal (attribute_names[i], name))
            return attribute_values[i];
    return NULL;
}

static void
gnc_builder_collect_start_element (GMarkupParseContext *context,
                                   const gchar *element_name,
                                   const gchar **attribute_names,
                                   const gchar **attribute_values,
                                   gpointer user_data,
                                   GError **error)
{
    (void)context;
    (void)error;
    GncBuilderSignalCollector *collector = user_data;

    if (g_str_equal (element_name, "object"))
    {
        const gchar *id = gnc_builder_attribute (attribute_names,
                                                 attribute_values, "id");
        g_ptr_array_add (collector->object_stack, g_strdup (id ? id : ""));
        return;
    }

    if (!g_str_equal (element_name, "signal") ||
        collector->object_stack->len == 0)
        return;

    const gchar *signal_name = gnc_builder_attribute (attribute_names,
                                                      attribute_values, "name");
    const gchar *handler_name = gnc_builder_attribute (attribute_names,
                                                       attribute_values, "handler");
    if (!signal_name || !handler_name)
        return;

    const gchar *object_id = g_ptr_array_index
        (collector->object_stack, collector->object_stack->len - 1);
    const gchar *connect_object_id = gnc_builder_attribute (attribute_names,
                                                             attribute_values,
                                                             "object");
    const gchar *swapped = gnc_builder_attribute (attribute_names,
                                                  attribute_values, "swapped");
    const gchar *after = gnc_builder_attribute (attribute_names,
                                                attribute_values, "after");
    GConnectFlags flags = 0;
    if (g_strcmp0 (swapped, "yes") == 0 || g_strcmp0 (swapped, "true") == 0)
        flags |= G_CONNECT_SWAPPED;
    if (g_strcmp0 (after, "yes") == 0 || g_strcmp0 (after, "true") == 0)
        flags |= G_CONNECT_AFTER;

    gchar *key = g_strdup_printf ("%s\x1f%s\x1f%s\x1f%s\x1f%u", object_id,
                                  signal_name, handler_name,
                                  connect_object_id ? connect_object_id : "", flags);
    if (g_hash_table_contains (collector->registry->seen, key))
    {
        g_free (key);
        return;
    }

    GncBuilderSignal *signal = g_new0 (GncBuilderSignal, 1);
    signal->object_id = g_strdup (object_id);
    signal->signal_name = g_strdup (signal_name);
    signal->handler_name = g_strdup (handler_name);
    signal->connect_object_id = g_strdup (connect_object_id);
    signal->flags = flags;
    g_hash_table_add (collector->registry->seen, key);
    g_ptr_array_add (collector->registry->signals, signal);
}

static void
gnc_builder_collect_end_element (GMarkupParseContext *context,
                                 const gchar *element_name,
                                 gpointer user_data,
                                 GError **error)
{
    (void)context;
    (void)error;
    GncBuilderSignalCollector *collector = user_data;

    if (g_str_equal (element_name, "object") && collector->object_stack->len)
        g_ptr_array_remove_index (collector->object_stack,
                                  collector->object_stack->len - 1);
}

static gboolean
gnc_builder_collect_signals (GtkBuilder *builder, const gchar *contents,
                             gssize length, GError **error)
{
    GncBuilderSignalCollector collector =
    {
        .registry = gnc_builder_get_signal_registry (builder),
        .object_stack = g_ptr_array_new_with_free_func (g_free),
    };
    GMarkupParser parser =
    {
        .start_element = gnc_builder_collect_start_element,
        .end_element = gnc_builder_collect_end_element,
    };
    GMarkupParseContext *context = g_markup_parse_context_new
        (&parser, G_MARKUP_TREAT_CDATA_AS_TEXT, &collector, NULL);
    gboolean result = g_markup_parse_context_parse (context, contents, length, error) &&
        g_markup_parse_context_end_parse (context, error);

    g_markup_parse_context_free (context);
    g_ptr_array_unref (collector.object_stack);
    return result;
}

static gchar *
gnc_builder_without_signals (const gchar *contents, GError **error)
{
    GRegex *signal_pattern = g_regex_new
        ("(?s)<signal\\b[^>]*(?:/>|>.*?</signal>)", 0, 0, error);
    if (!signal_pattern)
        return NULL;

    gchar *without_signals = g_regex_replace_literal (signal_pattern, contents,
                                                      -1, 0, "", 0, error);
    g_regex_unref (signal_pattern);
    return without_signals;
}

/* gnc_builder_add_from_file:
 *
 *   a convenience wrapper for gtk_builder_add_objects_from_string.
 *   It takes care of finding the directory for glade files and prints a
 *   warning message in case of an error.
 */
gboolean
gnc_builder_add_from_file (GtkBuilder *builder, const char *filename, const char *root)
{
    GError* error = NULL;
    char *fname;
    gchar *gnc_builder_dir;
    gchar *contents = NULL;
    gchar *without_signals = NULL;
    gsize contents_length = 0;
    gboolean result;

    g_return_val_if_fail (builder != NULL, FALSE);
    g_return_val_if_fail (filename != NULL, FALSE);
    g_return_val_if_fail (root != NULL, FALSE);

    gnc_builder_dir = gnc_path_get_gtkbuilderdir ();
    fname = g_build_filename(gnc_builder_dir, filename, (char *)NULL);
    g_free (gnc_builder_dir);

    if (!g_file_get_contents (fname, &contents, &contents_length, &error))
    {
        PWARN ("Couldn't load builder file: %s", error->message);
        g_error_free (error);
        g_free (fname);
        return FALSE;
    }

    if (!gnc_builder_collect_signals (builder, contents, contents_length, &error))
    {
        PWARN ("Couldn't parse builder signals: %s", error->message);
        g_error_free (error);
        g_free (contents);
        g_free (fname);
        return FALSE;
    }

    without_signals = gnc_builder_without_signals (contents, &error);
    g_free (contents);
    if (!without_signals)
    {
        PWARN ("Couldn't prepare builder file: %s", error->message);
        g_error_free (error);
        g_free (fname);
        return FALSE;
    }

    {
        const char *objects[] = { root, NULL };
        result = gtk_builder_add_objects_from_string (builder, without_signals,
                                                      -1, objects, &error);
        if (!result)
        {
            PWARN ("Couldn't load builder file: %s", error->message);
            g_error_free (error);
        }
    }

    g_free (without_signals);
    g_free (fname);

    if (result)
    {
        GSList *objects = gtk_builder_get_objects (builder);

        for (GSList *node = objects; node; node = node->next)
            if (GTK_IS_WINDOW (node->data))
                gnc_window_bind_to_application (GTK_WINDOW (node->data));
            else if (GTK_IS_DROP_DOWN (node->data))
                gnc_gtk_drop_down_normalize_width (GTK_DROP_DOWN (node->data));
        g_slist_free (objects);
    }

    return result;
}

void
gnc_builder_connect_signals_full (GtkBuilder *builder,
                                  GncBuilderConnectFunc connect_func,
                                  gpointer user_data)
{
    GncBuilderSignalRegistry *registry = gnc_builder_get_signal_registry (builder);

    g_return_if_fail (connect_func != NULL);
    for (guint i = 0; i < registry->signals->len; i++)
    {
        GncBuilderSignal *signal = g_ptr_array_index (registry->signals, i);
        GObject *signal_object = gtk_builder_get_object (builder, signal->object_id);
        if (!signal_object)
            continue;

        GObject *connect_object = NULL;
        if (signal->connect_object_id)
        {
            connect_object = gtk_builder_get_object (builder,
                                                     signal->connect_object_id);
            if (!connect_object)
            {
                PWARN ("Couldn't find signal object '%s' for handler '%s'.",
                       signal->connect_object_id, signal->handler_name);
                continue;
            }
        }

        connect_func (builder, signal_object, signal->signal_name,
                      signal->handler_name, connect_object,
                      signal->flags, user_data);
    }
}

void
gnc_builder_connect_signals (GtkBuilder *builder, gpointer user_data)
{
    gnc_builder_connect_signals_full (builder, gnc_builder_connect_full_func,
                                      user_data);
}


/*---------------------------------------------------------------------
 * The following function is built from a couple of glade functions.
 *--------------------------------------------------------------------*/
void
gnc_builder_connect_full_func(GtkBuilder *builder,
                              GObject *signal_object,
                              const gchar *signal_name,
                              const gchar *handler_name,
                              GObject *connect_object,
                              GConnectFlags flags,
                              gpointer user_data)
{
    GCallback func;
    GCallback *p_func = &func;

    if (allsymbols == NULL)
    {
        /* get a handle on the main executable -- use this to find symbols */
        allsymbols = g_module_open(NULL, 0);
    }

    if (!g_module_symbol(allsymbols, handler_name, (gpointer *)p_func))
    {
#ifdef HAVE_DLSYM
        /* Fallback to dlsym -- necessary for *BSD linkers */
        func = dlsym(RTLD_DEFAULT, handler_name);
#else
        func = NULL;
#endif
        if (func == NULL)
        {
            PWARN("ggaff: could not find signal handler '%s'.", handler_name);
            return;
        }
    }

    if (connect_object)
        g_signal_connect_object (signal_object, signal_name, func,
                                 connect_object, flags);
    else
        g_signal_connect_data(signal_object, signal_name, func,
                              user_data, NULL , flags);
}
/*--------------------------------------------------------------------------
 * End of GtkBuilder utilities
 *-------------------------------------------------------------------------*/


static void
gnc_perm_button_cb (GtkButton *perm, gpointer user_data)
{
    gboolean perm_active;

    perm_active = gtk_check_button_get_active(GTK_CHECK_BUTTON(perm));
    gtk_widget_set_sensitive(user_data, !perm_active);
}

typedef struct
{
    GtkWindow *window;
    GWeakRef parent;
    GWeakRef perm;
    GWeakRef temp;
    gchar *pref_name;
    GncWarningDialogResponseCallback completed;
    gpointer user_data;
    gulong close_handler;
    gulong destroy_handler;
    gulong parent_destroy_handler;
    gint action_response;
    gint alternate_response;
    gint stored_response;
} GncWarningDialogRequest;

static void
gnc_warning_dialog_request_free (GncWarningDialogRequest *request)
{
    GtkWindow *parent = g_weak_ref_get (&request->parent);

    if (parent && request->parent_destroy_handler)
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    g_clear_object (&parent);
    g_clear_object (&request->window);
    g_weak_ref_clear (&request->parent);
    g_weak_ref_clear (&request->perm);
    g_weak_ref_clear (&request->temp);
    g_free (request->pref_name);
    g_free (request);
}

static void
gnc_warning_dialog_request_complete (GncWarningDialogRequest *request,
                                     gint response, gboolean destroy_window)
{
    GtkWindow *parent;

    if (request->close_handler)
    {
        g_signal_handler_disconnect (request->window, request->close_handler);
        request->close_handler = 0;
    }
    if (request->destroy_handler)
    {
        g_signal_handler_disconnect (request->window, request->destroy_handler);
        request->destroy_handler = 0;
    }
    parent = g_weak_ref_get (&request->parent);
    if (parent && request->parent_destroy_handler)
        g_signal_handler_disconnect (parent, request->parent_destroy_handler);
    request->parent_destroy_handler = 0;
    g_clear_object (&parent);

    if (destroy_window)
        gtk_window_destroy (request->window);

    request->completed (response, request->user_data);
    gnc_warning_dialog_request_free (request);
}

static void
gnc_warning_dialog_save_response (GncWarningDialogRequest *request,
                                  gint response)
{
    GtkWidget *perm = g_weak_ref_get (&request->perm);
    GtkWidget *temp = g_weak_ref_get (&request->temp);

    if (response != GTK_RESPONSE_CANCEL && perm &&
        gtk_check_button_get_active (GTK_CHECK_BUTTON (perm)))
        gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_PERM, request->pref_name,
                           response);
    else if (response != GTK_RESPONSE_CANCEL && temp &&
             gtk_check_button_get_active (GTK_CHECK_BUTTON (temp)))
        gnc_prefs_set_int (GNC_PREFS_GROUP_WARNINGS_TEMP, request->pref_name,
                           response);
    g_clear_object (&perm);
    g_clear_object (&temp);
}

static void
gnc_warning_dialog_respond (GncWarningDialogRequest *request, gint response)
{
    gnc_warning_dialog_save_response (request, response);
    gnc_warning_dialog_request_complete (request, response, TRUE);
}

static void
gnc_warning_dialog_cancel_clicked_cb (GtkButton *button, gpointer user_data)
{
    (void)button;
    gnc_warning_dialog_respond (user_data, GTK_RESPONSE_CANCEL);
}

static void
gnc_warning_dialog_action_clicked_cb (GtkButton *button, gpointer user_data)
{
    GncWarningDialogRequest *request = user_data;

    (void)button;
    gnc_warning_dialog_respond (request, request->action_response);
}
static void
gnc_warning_dialog_alternate_clicked_cb (GtkButton *button, gpointer user_data)
{
    GncWarningDialogRequest *request = user_data;

    (void)button;
    gnc_warning_dialog_respond (request, request->alternate_response);
}

static gboolean
gnc_warning_dialog_close_request_cb (GtkWindow *window, gpointer user_data)
{
    (void)window;
    gnc_warning_dialog_respond (user_data, GTK_RESPONSE_CANCEL);
    return TRUE;
}

static void
gnc_warning_dialog_destroyed_cb (GtkWidget *window, gpointer user_data)
{
    GncWarningDialogRequest *request = user_data;

    (void)window;
    request->destroy_handler = 0;
    gnc_warning_dialog_request_complete (request, GTK_RESPONSE_CANCEL, FALSE);
}

static void
gnc_warning_dialog_parent_destroyed_cb (GtkWidget *parent, gpointer user_data)
{
    GncWarningDialogRequest *request = user_data;

    (void)parent;
    request->parent_destroy_handler = 0;
    gnc_warning_dialog_request_complete (request, GTK_RESPONSE_CANCEL, TRUE);
}

static gboolean
gnc_warning_dialog_stored_response_cb (gpointer user_data)
{
    GncWarningDialogRequest *request = user_data;

    request->completed (request->stored_response, request->user_data);
    return G_SOURCE_REMOVE;
}

static void
gnc_warning_dialog_async_full (GtkWindow *parent, const gchar *pref_name,
                               const gchar *title, const gchar *message,
                               const gchar *alternate_action,
                               gint alternate_response,
                               const gchar *action, gint action_response,
                               gboolean action_is_default,
                               GncWarningDialogResponseCallback completed,
                               gpointer user_data)
{
    GncWarningDialogRequest *request;
    GtkWidget *content;
    GtkWidget *label;
    GtkWidget *perm;
    GtkWidget *temp;
    GtkWidget *button_box;
    GtkWidget *cancel_button;
    GtkWidget *action_button;
    GtkWidget *alternate_button;
    gint response;

    g_return_if_fail (!parent || GTK_IS_WINDOW (parent));
    g_return_if_fail (pref_name != NULL);
    g_return_if_fail (title != NULL);
    g_return_if_fail (message != NULL);
    g_return_if_fail (action != NULL);
    g_return_if_fail (completed != NULL);

    request = g_new0 (GncWarningDialogRequest, 1);
    g_weak_ref_init (&request->parent, parent);
    g_weak_ref_init (&request->perm, NULL);
    g_weak_ref_init (&request->temp, NULL);
    request->pref_name = g_strdup (pref_name);
    request->completed = completed;
    request->user_data = user_data;
    request->action_response = action_response;
    request->alternate_response = alternate_response;

    response = gnc_prefs_get_int (GNC_PREFS_GROUP_WARNINGS_PERM, pref_name);
    if (response == 0)
        response = gnc_prefs_get_int (GNC_PREFS_GROUP_WARNINGS_TEMP, pref_name);
    if (response != 0)
    {
        request->stored_response = response;
        g_idle_add_full (G_PRIORITY_DEFAULT, gnc_warning_dialog_stored_response_cb,
                         request, (GDestroyNotify)gnc_warning_dialog_request_free);
        return;
    }

    request->window = GTK_WINDOW (g_object_ref_sink (gtk_window_new ()));
    gnc_window_bind_to_application (request->window);
    gtk_window_set_title (request->window, title);
    gtk_window_set_modal (request->window, TRUE);
    gtk_window_set_resizable (request->window, FALSE);
    if (parent)
    {
        gtk_window_set_transient_for (request->window, parent);
        request->parent_destroy_handler = g_signal_connect (
            parent, "destroy", G_CALLBACK (gnc_warning_dialog_parent_destroyed_cb), request);
    }

    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    gtk_widget_set_margin_start (content, 18);
    gtk_widget_set_margin_end (content, 18);
    gtk_widget_set_margin_top (content, 18);
    gtk_widget_set_margin_bottom (content, 18);
    gtk_window_set_child (request->window, content);

    label = gtk_label_new (title);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_widget_add_css_class (label, "title-2");
    gtk_box_append (GTK_BOX (content), label);
    gtk_widget_set_visible (label, TRUE);

    label = gtk_label_new (message);
    gtk_label_set_wrap (GTK_LABEL (label), TRUE);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_max_width_chars (GTK_LABEL (label), 72);
    gtk_box_append (GTK_BOX (content), label);
    gtk_widget_set_visible (label, TRUE);

    perm = gtk_check_button_new_with_mnemonic (_("Remember and don't _ask me again."));
    temp = gtk_check_button_new_with_mnemonic (
        _("Remember and don't ask me again this _session."));
    gtk_box_append (GTK_BOX (content), perm);
    gtk_box_append (GTK_BOX (content), temp);
    gtk_widget_set_visible (perm, TRUE);
    gtk_widget_set_visible (temp, TRUE);
    g_weak_ref_set (&request->perm, perm);
    g_weak_ref_set (&request->temp, temp);
    g_signal_connect (perm, "clicked", G_CALLBACK (gnc_perm_button_cb), temp);

    button_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    gtk_widget_set_halign (button_box, GTK_ALIGN_END);
    cancel_button = gtk_button_new_with_mnemonic (_("_Cancel"));
    alternate_button = alternate_action
        ? gtk_button_new_with_mnemonic (alternate_action) : NULL;
    action_button = gtk_button_new_with_mnemonic (action);
    gtk_box_append (GTK_BOX (button_box), cancel_button);
    if (alternate_button)
        gtk_box_append (GTK_BOX (button_box), alternate_button);
    gtk_box_append (GTK_BOX (button_box), action_button);
    gtk_box_append (GTK_BOX (content), button_box);
    gtk_widget_set_visible (cancel_button, TRUE);
    if (alternate_button)
        gtk_widget_set_visible (alternate_button, TRUE);
    gtk_widget_set_visible (action_button, TRUE);
    gtk_widget_set_visible (button_box, TRUE);
    gtk_widget_set_visible (content, TRUE);

    g_signal_connect (cancel_button, "clicked",
                      G_CALLBACK (gnc_warning_dialog_cancel_clicked_cb), request);
    if (alternate_button)
        g_signal_connect (alternate_button, "clicked",
                          G_CALLBACK (gnc_warning_dialog_alternate_clicked_cb), request);
    g_signal_connect (action_button, "clicked",
                      G_CALLBACK (gnc_warning_dialog_action_clicked_cb), request);
    request->close_handler = g_signal_connect (
        request->window, "close-request", G_CALLBACK (gnc_warning_dialog_close_request_cb), request);
    request->destroy_handler = g_signal_connect (
        request->window, "destroy", G_CALLBACK (gnc_warning_dialog_destroyed_cb), request);
    gtk_window_set_default_widget (request->window,
                                   action_is_default ? action_button : cancel_button);
    gtk_window_present (request->window);
}

void
gnc_warning_dialog_async (GtkWindow *parent, const gchar *pref_name,
                          const gchar *title, const gchar *message,
                          const gchar *action, gint action_response,
                          gboolean action_is_default,
                          GncWarningDialogResponseCallback completed,
                          gpointer user_data)
{
    gnc_warning_dialog_async_full (parent, pref_name, title, message,
                                   NULL, GTK_RESPONSE_NONE, action,
                                   action_response, action_is_default,
                                   completed, user_data);
}

void
gnc_warning_dialog_choice_async (GtkWindow *parent, const gchar *pref_name,
                                 const gchar *title, const gchar *message,
                                 const gchar *alternate_action,
                                 gint alternate_response,
                                 const gchar *action, gint action_response,
                                 gboolean action_is_default,
                                 GncWarningDialogResponseCallback completed,
                                 gpointer user_data)
{
    g_return_if_fail (alternate_action != NULL);
    gnc_warning_dialog_async_full (parent, pref_name, title, message,
                                   alternate_action, alternate_response,
                                   action, action_response, action_is_default,
                                   completed, user_data);
}
#define GNC_OK_TO_CLOSE_REQUEST "gnc-ok-to-close-window-request"

typedef struct
{
    GncOkToCloseWindowCallback completed;
    gpointer user_data;
} GncOkToCloseWindowCallbackData;

typedef struct
{
    gint ref_count;
    GWeakRef window;
    GCancellable *cancellable;
    GPtrArray *callbacks;
    gchar **buttons;
    gulong destroy_handler;
    gboolean completed;
} GncOkToCloseWindowRequest;

static GncOkToCloseWindowRequest *
gnc_ok_to_close_window_request_ref (GncOkToCloseWindowRequest *request)
{
    g_atomic_int_inc (&request->ref_count);
    return request;
}

static void
gnc_ok_to_close_window_request_free (GncOkToCloseWindowRequest *request)
{
    g_assert (request->destroy_handler == 0);
    g_weak_ref_clear (&request->window);
    g_clear_object (&request->cancellable);
    g_clear_pointer (&request->callbacks, g_ptr_array_unref);
    g_strfreev (request->buttons);
    g_free (request);
}

static void
gnc_ok_to_close_window_request_unref (GncOkToCloseWindowRequest *request)
{
    if (g_atomic_int_dec_and_test (&request->ref_count))
        gnc_ok_to_close_window_request_free (request);
}

static void
gnc_ok_to_close_window_request_destroy_notify (gpointer data,
                                                GClosure *closure)
{
    (void)closure;
    gnc_ok_to_close_window_request_unref (data);
}

static void
gnc_ok_to_close_window_request_complete (GncOkToCloseWindowRequest *request,
                                         GtkWindow *window,
                                         gboolean close_allowed)
{
    GPtrArray *callbacks;
    guint index;

    if (request->completed)
        return;

    request->completed = TRUE;
    if (window && g_object_get_data (G_OBJECT (window),
                                     GNC_OK_TO_CLOSE_REQUEST) == request)
        g_object_set_data (G_OBJECT (window), GNC_OK_TO_CLOSE_REQUEST, NULL);

    if (request->destroy_handler)
    {
        g_signal_handler_disconnect (window, request->destroy_handler);
        request->destroy_handler = 0;
    }

    callbacks = g_steal_pointer (&request->callbacks);
    for (index = 0; index < callbacks->len; index++)
    {
        GncOkToCloseWindowCallbackData *callback =
            g_ptr_array_index (callbacks, index);
        callback->completed (window, close_allowed, callback->user_data);
    }
    g_ptr_array_unref (callbacks);
}

static void
gnc_ok_to_close_window_request_window_destroyed (GtkWidget *widget,
                                                  gpointer user_data)
{
    GncOkToCloseWindowRequest *request = user_data;

    (void)widget;
    request->destroy_handler = 0;
    gnc_ok_to_close_window_request_complete (request, NULL, FALSE);
    g_cancellable_cancel (request->cancellable);
}

static void
gnc_ok_to_close_window_request_finished (GObject *source,
                                          GAsyncResult *result,
                                          gpointer user_data)
{
    GncOkToCloseWindowRequest *request = user_data;
    GError *error = NULL;
    GtkWindow *window = GTK_WINDOW (g_weak_ref_get (&request->window));
    gint choice = gtk_alert_dialog_choose_finish (GTK_ALERT_DIALOG (source), result,
                                                  &error);
    gboolean close_allowed = !error && choice == 1 && window != NULL;

    if (error && !g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED))
        g_warning ("Close confirmation failed: %s", error->message);

    gnc_ok_to_close_window_request_complete (request, window, close_allowed);
    g_clear_error (&error);
    g_clear_object (&window);
    gnc_ok_to_close_window_request_unref (request);
}

void
gnc_ok_to_close_window_async (GtkWindow *window,
                              GncOkToCloseWindowCallback completed,
                              gpointer user_data)
{
    GncOkToCloseWindowRequest *request;
    GncOkToCloseWindowCallbackData *callback;
    GtkAlertDialog *dialog;

    g_return_if_fail (GTK_IS_WINDOW (window));
    g_return_if_fail (completed != NULL);

    request = g_object_get_data (G_OBJECT (window), GNC_OK_TO_CLOSE_REQUEST);
    if (!request)
    {
        request = g_new0 (GncOkToCloseWindowRequest, 1);
        request->ref_count = 1;
        g_weak_ref_init (&request->window, window);
        request->cancellable = g_cancellable_new ();
        request->callbacks = g_ptr_array_new_with_free_func (g_free);
        request->buttons = g_new0 (gchar *, 3);
        request->buttons[0] = g_strdup (_("No"));
        request->buttons[1] = g_strdup (_("Yes"));
        g_object_set_data (G_OBJECT (window), GNC_OK_TO_CLOSE_REQUEST, request);
        gnc_ok_to_close_window_request_ref (request);
        request->destroy_handler = g_signal_connect_data (
            window, "destroy",
            G_CALLBACK (gnc_ok_to_close_window_request_window_destroyed), request,
            gnc_ok_to_close_window_request_destroy_notify, 0);
    }

    callback = g_new0 (GncOkToCloseWindowCallbackData, 1);
    callback->completed = completed;
    callback->user_data = user_data;
    g_ptr_array_add (request->callbacks, callback);

    if (request->callbacks->len != 1)
        return;

    dialog = gtk_alert_dialog_new ("%s", _("Close Window ?"));
    gtk_alert_dialog_set_buttons (dialog, (const char * const *)request->buttons);
    gtk_alert_dialog_set_default_button (dialog, 0);
    gtk_alert_dialog_set_cancel_button (dialog, 0);
    gnc_ok_to_close_window_request_ref (request);
    gtk_alert_dialog_choose (dialog, window, request->cancellable,
                             gnc_ok_to_close_window_request_finished, request);
    g_object_unref (dialog);
    gnc_ok_to_close_window_request_unref (request);
}

/* If this is a new book, this function can be used to display book options
 * dialog so user can specify options, before any transactions can be
 * imported/entered, since the book options can affect how transactions are
 * created. Note: This dialog is modal! */
typedef struct
{
    GWeakRef parent;
    GncNewBookOptionsFinishedCB callback;
    gpointer user_data;
    gboolean completed;
} GncNewBookOptionsRequest;

static void
new_book_options_destroyed (GtkWidget *window, gpointer user_data)
{
    GncNewBookOptionsRequest *request = user_data;
    GtkWindow *parent;
    gboolean applied;

    if (!request || request->completed)
        return;
    request->completed = TRUE;
    parent = GTK_WINDOW (g_weak_ref_get (&request->parent));
    applied = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (window),
                                                   "gnc-options-dialog-applied"));
    if (request->callback)
        request->callback (parent, applied, request->user_data);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

void
gnc_new_book_option_display_async (GtkWidget *parent,
                                   GncNewBookOptionsFinishedCB callback,
                                   gpointer user_data)
{
    GtkWidget *window = gnc_book_options_dialog_cb (
        TRUE, _( "New Book Options"), parent ? GTK_WINDOW (parent) : NULL);
    if (!window)
    {
        if (callback)
            callback (parent ? GTK_WINDOW (parent) : NULL, FALSE, user_data);
        return;
    }

    GncNewBookOptionsRequest *request = g_new0 (GncNewBookOptionsRequest, 1);
    g_weak_ref_init (&request->parent, parent);
    request->callback = callback;
    request->user_data = user_data;
    g_signal_connect (window, "destroy", G_CALLBACK (new_book_options_destroyed), request);
    if (parent)
        g_signal_connect_object (parent, "destroy", G_CALLBACK (gtk_window_destroy),
                                 window, G_CONNECT_SWAPPED);
    gtk_window_set_modal (GTK_WINDOW (window), TRUE);
    gtk_window_present (GTK_WINDOW (window));
}

void
gnc_entry_set_text (GtkEntry *entry, const gchar *text)
{
    g_return_if_fail (GTK_IS_ENTRY (entry));

    gtk_editable_set_text (GTK_EDITABLE (entry), text ? text : "");
}

const gchar *
gnc_entry_get_text (GtkEntry *entry)
{
    g_return_val_if_fail (GTK_IS_ENTRY (entry), "");

    return gtk_editable_get_text (GTK_EDITABLE (entry));
}

void
gnc_box_set_all_margins (GtkBox *box, gint margin)
{
    g_return_if_fail (GTK_IS_BOX (box));

    gtk_widget_set_margin_start (GTK_WIDGET (box), margin);
    gtk_widget_set_margin_end (GTK_WIDGET (box), margin);
    gtk_widget_set_margin_top (GTK_WIDGET (box), margin);
    gtk_widget_set_margin_bottom (GTK_WIDGET (box), margin);
}

void
gnc_owner_window_set_title (GtkWindow *window, const char *header,
                            GtkWidget *owner_entry, GtkWidget *id_entry)
{
    const char *name = gtk_editable_get_text (GTK_EDITABLE (owner_entry));
    if (!name || *name == '\0')
        name = _("<No name>");

    const char *id = gtk_editable_get_text (GTK_EDITABLE (id_entry));

    char *title = (id && *id) ?
        g_strdup_printf ("%s - %s (%s)", header, name, id) :
        g_strdup_printf ("%s - %s", header, name);

    gtk_window_set_title (window, title);

    g_free (title);
}
