/********************************************************************\
 * dialog-options.h -- GNOME option handling                        *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include <gnome.h>

#include "dialog-options.h"
#include "dialog-utils.h"
#include "option-util.h"
#include "query-user.h"
#include "util.h"


/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GnomePropertyBox *options_dialog = NULL;


static void
gnc_option_set_ui_value(GNCOption *option, gboolean use_default)
{
  gboolean bad_value = FALSE;
  char *type;
  SCM getter;
  SCM value;

  if ((option == NULL) || (option->widget == NULL))
    return;

  type = gnc_option_type(option);

  if (use_default)
    getter = gnc_option_default_getter(option);
  else
    getter = gnc_option_getter(option);

  value = gh_call0(getter);

  if (safe_strcmp(type, "boolean") == 0)
  {
    if (gh_boolean_p(value))
      gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(option->widget),
				  gh_scm2bool(value));
    else
      bad_value = TRUE;
  }
  else if (safe_strcmp(type, "string") == 0)
  {
    if (gh_string_p(value))
    {
      char *string = gh_scm2newstr(gh_call0(getter), NULL);
      gtk_entry_set_text(GTK_ENTRY(option->widget), string);
      free(string);
    }
    else
      bad_value = TRUE;
  }
  else
  {
    PERR("gnc_option_set_ui_value: Unknown type. Ignoring.\n");
  }

  if (bad_value)
  {
    PERR("gnc_option_set_ui_value: bad value\n");
  }

  free(type);
}

void
_gnc_option_refresh_ui(SCM guile_option)
{
  gnc_option_set_ui_value(gnc_get_option_by_SCM(guile_option), FALSE);
}

SCM
gnc_option_get_ui_value(GNCOption *option)
{
  SCM result = SCM_UNDEFINED;
  char *type;

  if (option->widget == NULL)
    return result;

  type = gnc_option_type(option);

  if (safe_strcmp(type, "boolean") == 0)
  {
    gboolean active;

    active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(option->widget));
    result = gh_bool2scm(active);
  }
  else if (safe_strcmp(type, "string") == 0)
  {
    char * string;

    string = gtk_editable_get_chars(GTK_EDITABLE(option->widget), 0, -1);
    result = gh_str02scm(string);
    g_free(string);
  }
  else
  {
    PERR("_gnc_option_get_guile_value: "
	 "Unknown type for refresh. Ignoring.\n");
  }

  free(type);

  return result;
}

static void
default_button_cb(GtkButton *button, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_ui_value(option, TRUE);

  option->changed = TRUE;
  gnome_property_box_changed(options_dialog);
}

static GtkWidget *
gnc_option_create_default_button(GNCOption *option)
{
  GtkWidget *default_button = gtk_button_new_with_label("Set to default");

  gtk_widget_show(default_button);
  gtk_container_set_border_width(GTK_CONTAINER(default_button), 2);

  gtk_signal_connect(GTK_OBJECT(default_button), "clicked",
		     GTK_SIGNAL_FUNC(default_button_cb), option);

  return default_button;
}

static void
gnc_option_toggled_cb(GtkToggleButton *button, gpointer data)
{
  GNCOption *option = data;

  option->changed = TRUE;
  gnome_property_box_changed(options_dialog);
}

static void
gnc_option_changed_cb(GtkEditable *editable, gpointer data)
{
  GNCOption *option = data;

  option->changed = TRUE;
  gnome_property_box_changed(options_dialog);
}

static GtkWidget *
gnc_option_set_ui_widget(GNCOption *option)
{
  GtkWidget *enclosing = NULL;
  GtkWidget *value = NULL;
  char *name, *documentation;
  char *type;

  type = gnc_option_type(option);
  name = gnc_option_name(option);
  documentation = gnc_option_documentation(option);

  if (safe_strcmp(type, "boolean") == 0)
  {
    enclosing = gtk_hbox_new(FALSE, 5);
    value = gtk_check_button_new_with_label(name);

    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);

    gtk_signal_connect(GTK_OBJECT(value), "toggled",
		       GTK_SIGNAL_FUNC(gnc_option_toggled_cb), option);

    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option),
		     FALSE, FALSE, 0);
  }
  else if (safe_strcmp(type, "string") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);
    value = gtk_entry_new();

    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);

    gtk_signal_connect(GTK_OBJECT(value), "changed",
		       GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option),
		     FALSE, FALSE, 0);
  }
  else
  {
    PERR("gnc_option_set_ui_widget: Unknown type. Ignoring.\n");
  }

  if (value != NULL)
    gnc_set_tooltip(value, documentation);

  if (enclosing != NULL)
    gtk_widget_show_all(enclosing);

  free(documentation);
  free(name);
  free(type);

  return enclosing;
}

static void
gnc_options_dialog_add_option(GtkWidget *page, GNCOption *option)
{
  gtk_box_pack_start(GTK_BOX(page), gnc_option_set_ui_widget(option),
		     FALSE, FALSE, 0);
}

static void
gnc_options_dialog_append_page(GNCOptionSection *section)
{
  GNCOption *option;
  GtkWidget *page_label;
  GtkWidget *page_content_box;
  gint num_options;
  gint i;

  page_label = gtk_label_new(section->section_name);
  gtk_widget_show(page_label);

  page_content_box = gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(page_content_box), 5);
  gtk_widget_show(page_content_box);
  
  gnome_property_box_append_page(options_dialog, page_content_box, page_label);

  num_options = gnc_option_section_num_options(section);
  for (i = 0; i < num_options; i++)
  {
    option = gnc_get_option_section_option(section, i);
    gnc_options_dialog_add_option(page_content_box, option);
  }
}

static void
build_options_dialog_contents()
{
  GNCOptionSection *section;
  gint num_sections = gnc_num_option_sections();
  gint i;

  for (i = 0; i < num_sections; i++)
  {
    section = gnc_get_option_section(i);
    gnc_options_dialog_append_page(section);
  }
}

static void
gnc_options_dialog_apply_cb(GnomePropertyBox *propertybox,
			    gint arg1, gpointer user_data)
{
  if (arg1 == -1)
    gnc_options_commit();
}

static void
gnc_options_dialog_help_cb(GnomePropertyBox *propertybox,
			   gint arg1, gpointer user_data)
{
  gnome_ok_dialog("Help on properties");
}

/* Options dialog... this should house all of the config options     */
/* like where the docs reside, and whatever else is deemed necessary */
void
gnc_show_options_dialog()
{
  if (gnc_num_option_sections() == 0)
  {
    gnc_warning_dialog("No options!");
    return;
  }

  if (gnc_options_dirty())
  {
    if (options_dialog != NULL)
      gtk_widget_destroy(GTK_WIDGET(options_dialog));

    options_dialog = NULL;
  }

  if (options_dialog == NULL)
  {
    options_dialog = GNOME_PROPERTY_BOX(gnome_property_box_new());
    gnome_dialog_close_hides(GNOME_DIALOG(options_dialog), TRUE);

    build_options_dialog_contents();
    gnc_options_clean();

    gtk_signal_connect(GTK_OBJECT(options_dialog), "apply",
		       GTK_SIGNAL_FUNC(gnc_options_dialog_apply_cb),
		       NULL);

    gtk_signal_connect(GTK_OBJECT(options_dialog), "help",
		       GTK_SIGNAL_FUNC(gnc_options_dialog_help_cb),
		       NULL);
  }

  gtk_widget_show(GTK_WIDGET(options_dialog));  
  gdk_window_raise(GTK_WIDGET(options_dialog)->window);
}
