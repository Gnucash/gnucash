/*
 * gnc-date-format.c -- Date formator widget
 *
 * Copyright (C) 2003 Derek Atkins  <derek@ihtfp.com>
 * All rights reserved.
 *
 * Gnucash is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 *
 */
/*
  @NOTATION@
*/

/*
 * Date format widget
 *
 * Authors: Derek Atkins <derek@ihtfp.com>
 */

#include "config.h"

#include <string.h>
#include <stdio.h>

#include "gnc-date-format.h"
#include "dialog-utils.h"

#define MAX_DATE_LEN 80

enum {
  FORMAT_CHANGED,
  LAST_SIGNAL
};

struct _GNCDateFormatPriv {
  GtkWidget*	format_omenu;

  GtkWidget*	months_label;
  GtkWidget*	months_number;
  GtkWidget*	months_abbrev;
  GtkWidget*	months_name;

  GtkWidget*	years_label;
  GtkWidget*	years_button;

  GtkWidget*	custom_label;
  GtkWidget*	custom_entry;

  GtkWidget*	sample_label;

  GtkWidget*	table;
  GtkWidget*	label_box;
};

static gint date_format_signals [LAST_SIGNAL] = { 0 };


static void gnc_date_format_init         (GNCDateFormat      *gdf);
static void gnc_date_format_class_init   (GNCDateFormatClass *class);
static void gnc_date_format_destroy      (GtkObject          *object);
static void gnc_date_format_compute_format(GNCDateFormat *gdf);

/* Used by glade_xml_signal_autoconnect_full */
void gnc_ui_date_format_changed_cb(GtkWidget *unused, gpointer user_data);

static GtkHBoxClass *parent_class;

/**
 * gnc_date_format_get_type:
 *
 * Returns the GtkType for the GNCDateFormat widget
 */
guint
gnc_date_format_get_type (void)
{
  static guint date_format_type = 0;

  if (!date_format_type){
    GtkTypeInfo date_format_info = {
      "GNCDateFormat",
      sizeof (GNCDateFormat),
      sizeof (GNCDateFormatClass),
      (GtkClassInitFunc) gnc_date_format_class_init,
      (GtkObjectInitFunc) gnc_date_format_init,
      NULL,
      NULL,
    };

    date_format_type = gtk_type_unique (gtk_hbox_get_type (),
					&date_format_info);
  }
	
  return date_format_type;
}


static void
gnc_date_format_class_init (GNCDateFormatClass *class)
{
  GtkObjectClass *object_class = (GtkObjectClass *) class;

  object_class = (GtkObjectClass*) class;

  parent_class = gtk_type_class (gtk_hbox_get_type ());

  date_format_signals [FORMAT_CHANGED] =
    gtk_signal_new ("format_changed",
		    GTK_RUN_FIRST, object_class->type, 
		    GTK_SIGNAL_OFFSET (GNCDateFormatClass,
				       format_changed),
		    gtk_signal_default_marshaller,
		    GTK_TYPE_NONE, 0);

  gtk_object_class_add_signals (object_class, date_format_signals,
				LAST_SIGNAL);

  object_class->destroy = gnc_date_format_destroy;

  class->format_changed = NULL;
}

static void
gnc_date_format_init (GNCDateFormat *gdf)
{
  GladeXML *xml;
  GtkWidget *dialog;

  g_return_if_fail(gdf);
  g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

  gdf->priv = g_new0(GNCDateFormatPriv, 1);

  /* Open up the Glade and set the signals */
  xml = gnc_glade_xml_new("gnc-date-format.glade", "GNC Date Format");
  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, gdf);

  /* pull in all the child widgets */
  gdf->label = glade_xml_get_widget(xml, "widget_label");
  gdf->priv->format_omenu = glade_xml_get_widget(xml, "format_omenu");

  gdf->priv->months_label = glade_xml_get_widget(xml, "months_label");
  gdf->priv->months_number = glade_xml_get_widget(xml, "month_number_button");
  gdf->priv->months_abbrev = glade_xml_get_widget(xml, "month_abbrev_button");
  gdf->priv->months_name = glade_xml_get_widget(xml, "month_name_button");

  gdf->priv->years_label = glade_xml_get_widget(xml, "years_label");
  gdf->priv->years_button = glade_xml_get_widget(xml, "years_button");

  gdf->priv->custom_label = glade_xml_get_widget(xml, "format_label");
  gdf->priv->custom_entry = glade_xml_get_widget(xml, "format_entry");

  gdf->priv->sample_label = glade_xml_get_widget(xml, "sample_label");

  gdf->priv->table = glade_xml_get_widget(xml, "date_format_table");
  gdf->priv->label_box = glade_xml_get_widget(xml, "label_box");

  /* Initialize the format menu */
  gnc_option_menu_init_w_signal(gdf->priv->format_omenu,
				gnc_ui_date_format_changed_cb, gdf);

  /* Set initial format to gnucash default */
  gnc_date_format_set_format(gdf, getDateFormat());

  /* pull in the dialog and table widgets and play the reconnect game */
  dialog = glade_xml_get_widget(xml, "GNC Date Format");

  gtk_object_ref(GTK_OBJECT(gdf->priv->table));
  gtk_container_remove(GTK_CONTAINER(dialog), gdf->priv->table);
  gtk_container_add(GTK_CONTAINER(gdf), gdf->priv->table);
  /* XXX: do I need to unref the table? */
  gtk_widget_destroy(dialog);
}

static void
gnc_date_format_destroy (GtkObject *object)
{
  GNCDateFormat *gdf;

  g_return_if_fail(object != NULL);
  g_return_if_fail(GNC_IS_DATE_FORMAT(object));

  gdf = GNC_DATE_FORMAT(object);

  g_free(gdf->priv);

  if (GTK_OBJECT_CLASS(parent_class)->destroy)
    (* GTK_OBJECT_CLASS(parent_class)->destroy) (object);
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

GtkWidget
*gnc_date_format_new_without_label (void)
{
  GtkWidget *widget = gnc_date_format_new_with_label(NULL);
  GNCDateFormat *gdf = GNC_DATE_FORMAT(widget);

  gtk_container_remove(GTK_CONTAINER(gdf->priv->table), gdf->priv->label_box);
  gdf->label = NULL;
  gtk_widget_queue_resize(gdf->priv->table);

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

  gdf = gtk_type_new (gnc_date_format_get_type ());

  if (label)
    gtk_label_set_text(GTK_LABEL(gdf->label), label);

  gnc_date_format_compute_format(gdf);
  return GTK_WIDGET(gdf);
}

void
gnc_date_format_set_format (GNCDateFormat *gdf, DateFormat format)
{
  g_return_if_fail(gdf);
  g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

  gtk_option_menu_set_history(GTK_OPTION_MENU(gdf->priv->format_omenu), format);
  gnc_date_format_compute_format(gdf);
}

DateFormat
gnc_date_format_get_format (GNCDateFormat *gdf)
{
  g_return_val_if_fail (gdf, DATE_FORMAT_LOCALE);
  g_return_val_if_fail (GNC_IS_DATE_FORMAT(gdf), DATE_FORMAT_LOCALE);

  return gnc_option_menu_get_active(gdf->priv->format_omenu);
}

void
gnc_date_format_set_months (GNCDateFormat *gdf, GNCDateMonthFormat months)
{
  GtkWidget *button = NULL;

  g_return_if_fail(gdf);
  g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

  switch (months) {
  case GNCDATE_MONTH_NUMBER:
    button = gdf->priv->months_number;
    break;
  case GNCDATE_MONTH_ABBREV:
    button = gdf->priv->months_abbrev;
    break;
  case GNCDATE_MONTH_NAME:
    button = gdf->priv->months_name;
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

  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->priv->months_number)))
    return GNCDATE_MONTH_NUMBER;
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->priv->months_abbrev)))
    return GNCDATE_MONTH_ABBREV;
  if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->priv->months_name)))
    return GNCDATE_MONTH_ABBREV;

  /* We should never reach this point */
  g_assert(FALSE);
  return GNCDATE_MONTH_NUMBER;
}

void
gnc_date_format_set_years (GNCDateFormat *gdf, gboolean include_century)
{
  g_return_if_fail(gdf);
  g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gdf->priv->years_button),
			       include_century);
  gnc_date_format_compute_format(gdf);
}

gboolean
gnc_date_format_get_years (GNCDateFormat *gdf)
{
  g_return_val_if_fail(gdf, FALSE);
  g_return_val_if_fail(GNC_IS_DATE_FORMAT(gdf), FALSE);

  return gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->priv->years_button));
}

void
gnc_date_format_set_custom (GNCDateFormat *gdf, const char *format)
{
  g_return_if_fail(gdf);
  g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

  if (format == NULL || *format == '\0')
    return;

  gtk_entry_set_text(GTK_ENTRY(gdf->priv->custom_entry), format);
  gnc_date_format_compute_format(gdf);
}

const char *
gnc_date_format_get_custom (GNCDateFormat *gdf)
{
  g_return_val_if_fail(gdf, "");
  g_return_val_if_fail(GNC_IS_DATE_FORMAT(gdf), "");

  return gtk_entry_get_text(GTK_ENTRY(gdf->priv->custom_entry));
}

/**
 * gnc_date_format_editable_enters:
 * @dialog: The gnome dialog this date formator lives in
 * @gdf: The date formator to modity
 * 
 * Extracts the formatable field from a GNCDateFormat widget, and sets it
 * up so that pressing the Enter key in this field as the same as
 * clicking the button that has the default.
 **/
void
gnc_date_format_editable_enters (GnomeDialog *dialog, GNCDateFormat *gdf)
{
  g_return_if_fail(dialog);
  g_return_if_fail(gdf);
  g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

  gnome_dialog_editable_enters(GNOME_DIALOG(dialog),
			       GTK_EDITABLE(gdf->priv->custom_entry));
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
  gtk_widget_set_sensitive(gdf->priv->months_label, sensitive);
  gtk_widget_set_sensitive(gdf->priv->months_number, sensitive);
  gtk_widget_set_sensitive(gdf->priv->months_abbrev, sensitive);
  gtk_widget_set_sensitive(gdf->priv->months_name, sensitive);
}

static void
gnc_date_format_enable_year (GNCDateFormat *gdf, gboolean sensitive)
{
  gtk_widget_set_sensitive(gdf->priv->years_label, sensitive);
  gtk_widget_set_sensitive(gdf->priv->years_button, sensitive);
}

static void
gnc_date_format_enable_format (GNCDateFormat *gdf, gboolean sensitive)
{
  gtk_widget_set_sensitive(gdf->priv->custom_label, sensitive);
  gtk_widget_set_sensitive(gdf->priv->custom_entry, sensitive);
}

void
gnc_date_format_refresh (GNCDateFormat *gdf)
{
  int sel_option;
  gboolean enable_year, enable_month, enable_custom, check_modifiers;
  static gchar *format, *c;
  gchar date_string[MAX_DATE_LEN];
  time_t secs_now;
  struct tm today;

  g_return_if_fail(gdf);
  g_return_if_fail(GNC_IS_DATE_FORMAT(gdf));

  sel_option = gnc_option_menu_get_active(gdf->priv->format_omenu);

  switch (sel_option) {
   case DATE_FORMAT_CUSTOM:
    format = g_strdup(gtk_entry_get_text(GTK_ENTRY(gdf->priv->custom_entry)));
    enable_year = enable_month = check_modifiers = FALSE;
    enable_custom = TRUE;
    break;

   case DATE_FORMAT_LOCALE:
    format = g_strdup(getDateFormatString(DATE_FORMAT_LOCALE));
    enable_year = enable_month = check_modifiers = enable_custom = FALSE;
    break;

   case DATE_FORMAT_ISO:
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(gdf->priv->months_number), TRUE);
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
  if (check_modifiers) {
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->priv->months_number))) {
      format = g_strdup(getDateFormatString(sel_option));
    } else {
      format = g_strdup(getDateTextFormatString(sel_option));
      if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->priv->months_name))) {
	c = strchr(format, 'b');
	if (c)
	  *c = 'B';
      }
    }
    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(gdf->priv->years_button))){
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
  gtk_signal_handler_block_by_data(GTK_OBJECT(gdf->priv->custom_entry), gdf);
  gtk_entry_set_text(GTK_ENTRY(gdf->priv->custom_entry), format);
  gtk_signal_handler_unblock_by_data(GTK_OBJECT(gdf->priv->custom_entry), gdf);
  
  /* Visual feedback on what the date will look like. */
  secs_now = time(NULL);
  localtime_r(&secs_now, &today);
  strftime(date_string, MAX_DATE_LEN, format, &today);
  gtk_label_set_text(GTK_LABEL(gdf->priv->sample_label), date_string);
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
  gtk_signal_emit(GTK_OBJECT(gdf), date_format_signals[FORMAT_CHANGED]);
}
