/********************************************************************\
 * dialog-options.c -- GNOME option handling                        *
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


/********************************************************************\
 * gnc_option_set_ui_value                                          *
 *   sets the GUI representation of an option with either its       *
 *   current guile value, or its default value                      *
 *                                                                  *
 * Args: option      - option structure containing option           *
 *       use_default - if true, use the default value, otherwise    *
 *                     use the current value                        *
 * Return: nothing                                                  *
\********************************************************************/
void
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
      char *string = gh_scm2newstr(value, NULL);
      gtk_entry_set_text(GTK_ENTRY(option->widget), string);
      free(string);
    }
    else
      bad_value = TRUE;
  }
  else if (safe_strcmp(type, "multichoice") == 0)
  {
    int index;

    index = gnc_option_value_permissible_value_index(option, value);
    if (index < 0)
      bad_value = TRUE;
    else
    {
      gtk_option_menu_set_history(GTK_OPTION_MENU(option->widget), index);
      gtk_object_set_data(GTK_OBJECT(option->widget), "gnc_multichoice_index",
                          GINT_TO_POINTER(index));
    }
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


/********************************************************************\
 * gnc_option_get_ui_value                                          *
 *   returns the SCM representation of the GUI option value         *
 *                                                                  *
 * Args: option - option structure containing option                *
 * Return: SCM handle to GUI option value                           *
\********************************************************************/
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
  else if (safe_strcmp(type, "multichoice") == 0)
  {
    gpointer _index;
    int index;

    _index = gtk_object_get_data(GTK_OBJECT(option->widget),
                                 "gnc_multichoice_index");
    index = GPOINTER_TO_INT(_index);

    result = gnc_option_value_permissible_value(option, index);
  }
  else
  {
    PERR("gnc_option_get_ui_value: "
	 "Unknown type for refresh. Ignoring.\n");
  }

  free(type);

  return result;
}

static void
default_button_cb(GtkButton *button, gpointer data)
{
  GtkWidget *pbox;
  GNCOption *option = data;

  gnc_option_set_ui_value(option, TRUE);

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(button));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
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
  GtkWidget *pbox;
  GNCOption *option = data;

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(button));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static void
gnc_option_changed_cb(GtkEditable *editable, gpointer data)
{
  GtkWidget *pbox;
  GNCOption *option = data;

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(editable));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static void
gnc_option_multichoice_cb(GtkWidget *w, gint index, gpointer data)
{
  GtkWidget *pbox, *omenu;
  GNCOption *option = data;
  gpointer _current;
  gint current;

  _current = gtk_object_get_data(GTK_OBJECT(option->widget),
                                 "gnc_multichoice_index");
  current = GPOINTER_TO_INT(_current);

  if (current == index)
    return;

  gtk_option_menu_set_history(GTK_OPTION_MENU(option->widget), index);
  gtk_object_set_data(GTK_OBJECT(option->widget), "gnc_multichoice_index",
                      GINT_TO_POINTER(index));

  option->changed = TRUE;

  omenu = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_menu");
  pbox = gtk_widget_get_toplevel(omenu);
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static GtkWidget *
gnc_option_create_multichoice_widget(GNCOption *option)
{
  GtkWidget *widget;
  GNCOptionInfo *info;
  int num_values;
  int i;

  num_values = gnc_option_value_num_permissible_values(option);

  g_return_val_if_fail(num_values >= 0, NULL);

  info = g_new0(GNCOptionInfo, num_values);

  for (i = 0; i < num_values; i++)
  {
    info[i].name = gnc_option_value_permissible_value_name(option, i);
    info[i].tip  = gnc_option_value_permissible_value_description(option, i);
    info[i].callback = gnc_option_multichoice_cb;
    info[i].user_data = option;
  }

  widget = gnc_build_option_menu(info, num_values);

  for (i = 0; i < num_values; i++)
  {
    free(info[i].name);
    free(info[i].tip);
  }
  g_free(info);

  return widget;
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
  else if (safe_strcmp(type, "multichoice") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label= gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);

    value = gnc_option_create_multichoice_widget(option);
    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);
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
gnc_options_dialog_append_page(GnomePropertyBox *propertybox,
                               GNCOptionSection *section)
{
  GNCOption *option;
  GtkWidget *page_label;
  GtkWidget *page_content_box;
  gint num_options;
  gint i;

  page_label = gtk_label_new(gnc_option_section_name(section));
  gtk_widget_show(page_label);

  page_content_box = gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(page_content_box), 5);
  gtk_widget_show(page_content_box);
  
  gnome_property_box_append_page(propertybox, page_content_box, page_label);

  num_options = gnc_option_section_num_options(section);
  for (i = 0; i < num_options; i++)
  {
    option = gnc_get_option_section_option(section, i);
    gnc_options_dialog_add_option(page_content_box, option);
  }
}


/********************************************************************\
 * gnc_build_options_dialog_contents                                *
 *   builds an options dialog given a property box and an options   *
 *   database                                                       *
 *                                                                  *
 * Args: propertybox - gnome property box to use                    *
 *       odb         - option database to use                       *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_build_options_dialog_contents(GnomePropertyBox *propertybox,
                                  GNCOptionDB *odb)
{
  GNCOptionSection *section;
  gint num_sections = gnc_option_db_num_sections(odb);
  gint i;

  for (i = 0; i < num_sections; i++)
  {
    section = gnc_option_db_get_section(odb, i);
    gnc_options_dialog_append_page(propertybox, section);
  }
}
