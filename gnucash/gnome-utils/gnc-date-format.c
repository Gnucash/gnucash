/********************************************************************
 * gnc-date-format.c -- Date formator widget                        *
 *                       (GnuCash)                                  *
 * Copyright (C) 2003 Derek Atkins  <derek@ihtfp.com>               *
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
 ********************************************************************/

/*
  @NOTATION@
*/

/*
 * Date format widget
 *
 * Authors: Derek Atkins <derek@ihtfp.com>
 */

#include <config.h>

#include <gtk/gtk.h>
#include <string.h>
#include <stdio.h>

#include "gnc-date-format.h"
#include "dialog-utils.h"
#include "gnc-engine.h"

/* Perhaps it's better just to use MAX_DATE_LENGTH defined in gnc-date.h */
#define MAX_DATE_LEN 80

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;

enum
{
    FORMAT_CHANGED,
    LAST_SIGNAL
};

/**
 **/
struct _GNCDateFormat
{
    GtkBox hbox;

    GtkWidget*	format_combobox;

    GtkWidget*  label;
    GtkWidget*  table;

    GtkWidget*	months_label;
    GtkWidget*	months_number;
    GtkWidget*	months_abbrev;
    GtkWidget*	months_name;

    GtkWidget*	years_label;
    GtkWidget*	years_button;

    GtkWidget*	custom_label;
    GtkWidget*	custom_entry;

    GtkWidget*	sample_label;
};

static guint date_format_signals [LAST_SIGNAL] = { 0 };

static void gnc_date_format_finalize     (GObject            *object);
static void gnc_date_format_compute_format(GNCDateFormat *gdf);

void gnc_ui_date_format_changed_cb(GtkWidget *unused, gpointer user_data);

G_DEFINE_TYPE(GNCDateFormat, gnc_date_format, GTK_TYPE_BOX)

static void
gnc_date_format_class_init (GNCDateFormatClass *klass)
{
    GObjectClass   *gobject_class = (GObjectClass *) klass;

    gobject_class->finalize = gnc_date_format_finalize;

    date_format_signals [FORMAT_CHANGED] =
        g_signal_new ("format_changed",
                      G_OBJECT_CLASS_TYPE (gobject_class),
                      G_SIGNAL_RUN_FIRST,
                      0,
                      NULL,
                      NULL,
                      g_cclosure_marshal_VOID__VOID,
                      G_TYPE_NONE,
                      0);
}


static void
gnc_date_format_init (GNCDateFormat *gdf)
{
    GtkBuilder *builder;
    GtkWidget *dialog;

    g_return_if_fail(gdf);
    g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

    gtk_orientable_set_orientation (GTK_ORIENTABLE(gdf), GTK_ORIENTATION_HORIZONTAL);

    // Set the name for this widget so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(gdf), "gnc-id-date-format");

    /* Open up the Glade and set the signals */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "gnc-date-format.glade", "format-liststore");
    gnc_builder_add_from_file (builder, "gnc-date-format.glade", "gnc_date_format_window");

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, gdf);

    /* pull in all the child widgets */
    gdf->label = GTK_WIDGET(gtk_builder_get_object (builder, "widget_label"));
    gdf->format_combobox = GTK_WIDGET(gtk_builder_get_object (builder, "format_combobox"));

    gdf->months_label = GTK_WIDGET(gtk_builder_get_object (builder, "months_label"));
    gdf->months_number = GTK_WIDGET(gtk_builder_get_object (builder, "month_number_button"));
    gdf->months_abbrev = GTK_WIDGET(gtk_builder_get_object (builder, "month_abbrev_button"));
    gdf->months_name = GTK_WIDGET(gtk_builder_get_object (builder, "month_name_button"));

    gdf->years_label = GTK_WIDGET(gtk_builder_get_object (builder, "years_label"));
    gdf->years_button = GTK_WIDGET(gtk_builder_get_object (builder, "years_button"));

    gdf->custom_label = GTK_WIDGET(gtk_builder_get_object (builder, "format_label"));
    gdf->custom_entry = GTK_WIDGET(gtk_builder_get_object (builder, "format_entry"));

    gdf->sample_label = GTK_WIDGET(gtk_builder_get_object (builder, "sample_label"));

    /* Set initial format to gnucash default */
    gnc_date_format_set_format(gdf, QOF_DATE_FORMAT_UNSET);

    /* pull in the dialog and table widgets and play the reconnect game */
    dialog = GTK_WIDGET(gtk_builder_get_object (builder, "gnc_date_format_window"));

    gdf->table = GTK_WIDGET(gtk_builder_get_object (builder, "date_format_table"));
    g_object_ref (G_OBJECT(gdf->table));
    gtk_container_remove (GTK_CONTAINER(dialog), gdf->table);
    gtk_container_add (GTK_CONTAINER(gdf), gdf->table);
    g_object_unref (G_OBJECT(gdf->table));

    g_object_unref(G_OBJECT(builder));

    /* Destroy the now empty window */
    gtk_widget_destroy(dialog);
}


static void
gnc_date_format_finalize (GObject *object)
{
    g_return_if_fail(object != NULL);
    g_return_if_fail(GNC_IS_DATE_FORMAT(object));

    G_OBJECT_CLASS(gnc_date_format_parent_class)->finalize (object);
}


/**
 * gnc_date_format_new:
 *
 * Creates a new GNCDateFormat widget which can be used to provide
 * an easy to use way for entering date formats and seeing the sample.
 *
 * Returns a GNCDateFormat widget.
 */
GtkWidget *
gnc_date_format_new (void)
{
    return gnc_date_format_new_with_label (NULL);
}


GtkWidget *
gnc_date_format_new_without_label (void)
{
    GtkWidget *widget = gnc_date_format_new_with_label(NULL);
    GNCDateFormat *gdf = GNC_DATE_FORMAT(widget);

    // remove the first column which has the label
    gtk_grid_remove_column (GTK_GRID(gdf->table), 0);
    gdf->label = NULL;
    return widget;
}


/**
 * gnc_date_format_new_with_label:
 * @label: the label to use to define the widget.
 *
 * Creates a new GNCDateFormat widget which can be used to provide
 * an easy to use way for entering date formats and seeing the sample.
 *
 * Returns a GNCDateFormat widget.
 */
GtkWidget *
gnc_date_format_new_with_label (const char *label)
{
    GNCDateFormat *gdf;

    gdf = g_object_new(GNC_TYPE_DATE_FORMAT, NULL);

    if (label)
        gtk_label_set_text(GTK_LABEL(gdf->label), label);

    gnc_date_format_compute_format(gdf);
    return GTK_WIDGET(gdf);
}


void
gnc_date_format_set_format (GNCDateFormat *gdf, QofDateFormat format)
{
    g_return_if_fail(gdf);
    g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

    gtk_combo_box_set_active(GTK_COMBO_BOX(gdf->format_combobox), format);
    gnc_date_format_compute_format(gdf);
}


QofDateFormat
gnc_date_format_get_format (GNCDateFormat *gdf)
{
    g_return_val_if_fail (gdf, QOF_DATE_FORMAT_LOCALE);
    g_return_val_if_fail (GNC_IS_DATE_FORMAT(gdf), QOF_DATE_FORMAT_LOCALE);

    return gtk_combo_box_get_active(GTK_COMBO_BOX(gdf->format_combobox));
}


void
gnc_date_format_set_months (GNCDateFormat *gdf, GNCDateMonthFormat months)
{
    GtkWidget *button = NULL;

    g_return_if_fail(gdf);
    g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

    switch (months)
    {
    case GNCDATE_MONTH_NUMBER:
        button = gdf->months_number;
        break;
    case GNCDATE_MONTH_ABBREV:
        button = gdf->months_abbrev;
        break;
    case GNCDATE_MONTH_NAME:
        button = gdf->months_name;
        break;
    default:
        break;
    }

    g_return_if_fail(button);

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), TRUE);
    gnc_date_format_compute_format(gdf);
}


GNCDateMonthFormat
gnc_date_format_get_months (GNCDateFormat *gdf)
{
    g_return_val_if_fail(gdf, GNCDATE_MONTH_NUMBER);
    g_return_val_if_fail(GNC_IS_DATE_FORMAT(gdf), GNCDATE_MONTH_NUMBER);

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->months_number)))
        return GNCDATE_MONTH_NUMBER;
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->months_abbrev)))
        return GNCDATE_MONTH_ABBREV;
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->months_name)))
        return GNCDATE_MONTH_NAME;

    /* We should never reach this point */
    g_assert(FALSE);
    return GNCDATE_MONTH_NUMBER;
}


void
gnc_date_format_set_years (GNCDateFormat *gdf, gboolean include_century)
{
    g_return_if_fail(gdf);
    g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gdf->years_button),
                                 include_century);
    gnc_date_format_compute_format(gdf);
}


gboolean
gnc_date_format_get_years (GNCDateFormat *gdf)
{
    g_return_val_if_fail(gdf, FALSE);
    g_return_val_if_fail(GNC_IS_DATE_FORMAT(gdf), FALSE);

    return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->years_button));
}


void
gnc_date_format_set_custom (GNCDateFormat *gdf, const char *format)
{
    g_return_if_fail(gdf);
    g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

    if (format == NULL || *format == '\0')
        return;

    gtk_entry_set_text(GTK_ENTRY(gdf->custom_entry), format);
    gnc_date_format_compute_format(gdf);
}


const char *
gnc_date_format_get_custom (GNCDateFormat *gdf)
{
    g_return_val_if_fail(gdf, "");
    g_return_val_if_fail(GNC_IS_DATE_FORMAT(gdf), "");

    return gtk_entry_get_text(GTK_ENTRY(gdf->custom_entry));
}


void
gnc_ui_date_format_changed_cb(GtkWidget *unused, gpointer user_data)
{
    GNCDateFormat * gdf = user_data;

    gnc_date_format_compute_format(gdf);
}


static void
gnc_date_format_enable_month (GNCDateFormat *gdf, gboolean sensitive)
{
    gtk_widget_set_sensitive(gdf->months_label, sensitive);
    gtk_widget_set_sensitive(gdf->months_number, sensitive);
    gtk_widget_set_sensitive(gdf->months_abbrev, sensitive);
    gtk_widget_set_sensitive(gdf->months_name, sensitive);
}


static void
gnc_date_format_enable_year (GNCDateFormat *gdf, gboolean sensitive)
{
    gtk_widget_set_sensitive(gdf->years_label, sensitive);
    gtk_widget_set_sensitive(gdf->years_button, sensitive);
}


static void
gnc_date_format_enable_format (GNCDateFormat *gdf, gboolean sensitive)
{
    gtk_widget_set_sensitive(gdf->custom_label, sensitive);
    gtk_widget_set_sensitive(gdf->custom_entry, sensitive);
}


void
gnc_date_format_refresh (GNCDateFormat *gdf)
{
    int sel_option;
    gboolean enable_year, enable_month, enable_custom, check_modifiers;
    static gchar *format, *c;
    gchar date_string[MAX_DATE_LEN];
    time64 secs_now;
    struct tm today;

    g_return_if_fail(gdf);
    g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

    sel_option =
        gtk_combo_box_get_active(GTK_COMBO_BOX(gdf->format_combobox));

    switch (sel_option)
    {
    case QOF_DATE_FORMAT_CUSTOM:
        format = g_strdup(gtk_entry_get_text(GTK_ENTRY(gdf->custom_entry)));
        enable_year = enable_month = check_modifiers = FALSE;
        enable_custom = TRUE;
        break;

    case QOF_DATE_FORMAT_UNSET:
    case QOF_DATE_FORMAT_LOCALE:
    case QOF_DATE_FORMAT_UTC:
        format = g_strdup(qof_date_format_get_string(sel_option));
        enable_year = enable_month = check_modifiers = enable_custom = FALSE;
        break;

    case QOF_DATE_FORMAT_ISO:
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gdf->months_number), TRUE);
        enable_year = check_modifiers = TRUE;
        enable_month = enable_custom = FALSE;
        break;

    default:
        enable_year = enable_month = check_modifiers = TRUE;
        enable_custom = FALSE;
        break;
    }

    /* Tweak widget sensitivities, as appropriate. */
    gnc_date_format_enable_year(gdf, enable_year);
    gnc_date_format_enable_month(gdf, enable_month);
    gnc_date_format_enable_format(gdf, enable_custom);

    /* Update the format string based upon the user's preferences */
    if (check_modifiers)
    {
        if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->months_number)))
        {
            format = g_strdup(qof_date_format_get_string(sel_option));
        }
        else
        {
            format = g_strdup(qof_date_text_format_get_string(sel_option));
            if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->months_name)))
            {
                c = strchr(format, 'b');
                if (c)
                    *c = 'B';
            }
        }
        if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->years_button)))
        {
            c = strchr(format, 'y');
            if (c)
                *c = 'Y';
        }
    }

    /*
     * Give feedback on the format string so users can see how it works
     * without having to read the strftime man page. Prevent recursive
     * signals.
     */
    g_signal_handlers_block_matched(gdf->custom_entry, G_SIGNAL_MATCH_DATA,
                                    0, 0, NULL, NULL, gdf);
    gtk_entry_set_text(GTK_ENTRY(gdf->custom_entry), format);
    g_signal_handlers_unblock_matched(gdf->custom_entry, G_SIGNAL_MATCH_DATA,
                                      0, 0, NULL, NULL, gdf);

    /* Visual feedback on what the date will look like. */
    secs_now = gnc_time (NULL);
    gnc_localtime_r (&secs_now, &today);
    qof_strftime(date_string, MAX_DATE_LEN, format, &today);
    gtk_label_set_text(GTK_LABEL(gdf->sample_label), date_string);
    g_free(format);
}


static void
gnc_date_format_compute_format(GNCDateFormat *gdf)
{
    g_return_if_fail(gdf);
    g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

    /* refresh the widget */
    gnc_date_format_refresh(gdf);

    /* Emit a signal that we've changed */
    g_signal_emit(G_OBJECT(gdf), date_format_signals[FORMAT_CHANGED], 0);
}
