/********************************************************************\
 * dialog-options.c -- GNOME option handling                        *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
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

#include <top-level.h>

#include <gnome.h>

#include "dialog-options.h"
#include "dialog-utils.h"
#include "option-util.h"
#include "query-user.h"
#include "gnc-helpers.h"
#include "account-tree.h"
#include "global-options.h"
#include "messages.h"
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

    index = gnc_option_permissible_value_index(option, value);
    if (index < 0)
      bad_value = TRUE;
    else
    {
      gtk_option_menu_set_history(GTK_OPTION_MENU(option->widget), index);
      gtk_object_set_data(GTK_OBJECT(option->widget), "gnc_multichoice_index",
                          GINT_TO_POINTER(index));
    }
  }
  else if (safe_strcmp(type, "date") == 0)
  {
    Timespec ts;

    if (gnc_timepair_p(value))
    {
      ts = gnc_timepair2timespec(value);
      gnome_date_edit_set_time(GNOME_DATE_EDIT(option->widget), ts.tv_sec);
    }
    else
      bad_value = TRUE;
  }
  else if (safe_strcmp(type, "account-list") == 0)
  {
    GList *list;

    list = gnc_scm_to_account_list(value);

    gtk_clist_unselect_all(GTK_CLIST(option->widget));
    gnc_account_tree_select_accounts(GNC_ACCOUNT_TREE(option->widget),
                                     list, TRUE);

    g_list_free(list);
  }
  else if (safe_strcmp(type, "list") == 0)
  {
    gint num_rows, row;

    gtk_clist_unselect_all(GTK_CLIST(option->widget));

    num_rows = gnc_option_num_permissible_values(option);
    for (row = 0; row < num_rows; row++)
      gtk_clist_set_row_data(GTK_CLIST(option->widget),
                             row, GINT_TO_POINTER(FALSE));

    while (gh_list_p(value) && !gh_null_p(value))
    {
      SCM item;

      item = gh_car(value);
      value = gh_cdr(value);

      row = gnc_option_permissible_value_index(option, item);
      if (index < 0)
      {
        bad_value = TRUE;
        break;
      }

      gtk_clist_select_row(GTK_CLIST(option->widget), row, 0);
      gtk_clist_set_row_data(GTK_CLIST(option->widget),
                             row, GINT_TO_POINTER(TRUE));
    }

    if (!gh_list_p(value) || !gh_null_p(value))
      bad_value = TRUE;
  }
  else if (safe_strcmp(type, "number-range") == 0)
  {
    GtkSpinButton *spinner;
    gdouble d_value;;

    spinner = GTK_SPIN_BUTTON(option->widget);

    if (gh_number_p(value))
    {
      d_value = gh_scm2double(value);
      gtk_spin_button_set_value(spinner, d_value);
    }
    else
      bad_value = TRUE;
  }
  else if (safe_strcmp(type, "color") == 0)
  {
    gdouble red, green, blue, alpha;

    if (gnc_option_get_color_info(option, use_default,
                                  &red, &green, &blue, &alpha))
    {
      GnomeColorPicker *picker;

      picker = GNOME_COLOR_PICKER(option->widget);

      gnome_color_picker_set_d(picker, red, green, blue, alpha);
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

    result = gnc_option_permissible_value(option, index);
  }
  else if (safe_strcmp(type, "date") == 0)
  {
    Timespec ts;

    ts.tv_sec  = gnome_date_edit_get_date(GNOME_DATE_EDIT(option->widget));
    ts.tv_nsec = 0;

    result = gnc_timespec2timepair(ts);
  }
  else if (safe_strcmp(type, "account-list") == 0)
  {
    GNCAccountTree *tree;
    GList *list;

    tree = GNC_ACCOUNT_TREE(option->widget);
    list = gnc_account_tree_get_current_accounts(tree);

    result = gnc_account_list_to_scm(list);

    g_list_free(list);
  }
  else if (safe_strcmp(type, "list") == 0)
  {
    gboolean selected;
    GtkCList *clist;
    gint num_rows;
    gint row;

    clist = GTK_CLIST(option->widget);
    num_rows = gnc_option_num_permissible_values(option);
    result = gh_eval_str("()");

    for (row = 0; row < num_rows; row++)
    {
      selected = GPOINTER_TO_INT(gtk_clist_get_row_data(clist, row));
      if (selected)
        result = gh_cons(gnc_option_permissible_value(option, row), result);
    }

    result = gh_reverse(result);
  }
  else if (safe_strcmp(type, "number-range") == 0)
  {
    GtkSpinButton *spinner;
    gdouble value;

    spinner = GTK_SPIN_BUTTON(option->widget);

    value = gtk_spin_button_get_value_as_float(spinner);

    result = gh_double2scm(value);
  }
  else if (safe_strcmp(type, "color") == 0)
  {
    GnomeColorPicker *picker;
    gdouble red, green, blue, alpha;
    gdouble scale;

    picker = GNOME_COLOR_PICKER(option->widget);

    gnome_color_picker_get_d(picker, &red, &green, &blue, &alpha);

    scale = gnc_option_color_range(option);

    result = gh_eval_str("()");
    result = gh_cons(gh_double2scm(alpha * scale), result);
    result = gh_cons(gh_double2scm(blue * scale), result);
    result = gh_cons(gh_double2scm(green * scale), result);
    result = gh_cons(gh_double2scm(red * scale), result);
  }
  else
  {
    PERR("gnc_option_get_ui_value: Unknown type for refresh. Ignoring.\n");
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
gnc_option_create_default_button(GNCOption *option, GtkTooltips *tooltips)
{
  GtkWidget *default_button = gtk_button_new_with_label(SET_TO_DEFAULT_STR);

  gtk_container_set_border_width(GTK_CONTAINER(default_button), 2);

  gtk_signal_connect(GTK_OBJECT(default_button), "clicked",
		     GTK_SIGNAL_FUNC(default_button_cb), option);

  gtk_tooltips_set_tip(tooltips, default_button, TOOLTIP_SET_DEFAULT, NULL);

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
  char **raw_strings;
  char **raw;
  int i;

  num_values = gnc_option_num_permissible_values(option);

  g_return_val_if_fail(num_values >= 0, NULL);

  info = g_new0(GNCOptionInfo, num_values);
  raw_strings = g_new0(char *, num_values * 2);
  raw = raw_strings;

  for (i = 0; i < num_values; i++)
  {
    *raw = gnc_option_permissible_value_name(option, i);
    if (*raw != NULL)
      info[i].name = _(*raw);
    else
      info[i].name = "";

    raw++;

    *raw = gnc_option_permissible_value_description(option, i);
    if (*raw != NULL)
      info[i].tip = _(*raw);
    else
      info[i].tip = "";

    info[i].callback = gnc_option_multichoice_cb;
    info[i].user_data = option;
  }

  widget = gnc_build_option_menu(info, num_values);

  for (i = 0; i < num_values * 2; i++)
    if (raw_strings[i] != NULL)
      free(raw_strings[i]);

  g_free(raw_strings);
  g_free(info);

  return widget;
}

static void
gnc_option_account_cb(GNCAccountTree *tree, Account * account, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(tree));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static void
gnc_option_account_select_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  gtk_clist_select_all(GTK_CLIST(option->widget));

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static void
gnc_option_account_clear_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  gtk_clist_unselect_all(GTK_CLIST(option->widget));

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}


static GtkWidget *
gnc_option_create_account_widget(GNCOption *option, char *name)
{
  gboolean multiple_selection;
  GtkWidget *scroll_win;
  GtkWidget *button;
  GtkWidget *frame;
  GtkWidget *tree;
  GtkWidget *vbox;
  GtkWidget *bbox;

  multiple_selection = gnc_option_multiple_selection(option);

  frame = gtk_frame_new(name);

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(frame), vbox);

  tree = gnc_account_tree_new();
  gtk_clist_column_titles_hide(GTK_CLIST(tree));
  gnc_account_tree_hide_all_but_name(GNC_ACCOUNT_TREE(tree));
  gnc_account_tree_refresh(GNC_ACCOUNT_TREE(tree));
  if (multiple_selection)
    gtk_clist_set_selection_mode(GTK_CLIST(tree), GTK_SELECTION_MULTIPLE);
  else
    gtk_clist_set_selection_mode(GTK_CLIST(tree), GTK_SELECTION_BROWSE);

  scroll_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                 GTK_POLICY_AUTOMATIC, 
                                 GTK_POLICY_AUTOMATIC);

  gtk_box_pack_start(GTK_BOX(vbox), scroll_win, FALSE, FALSE, 0);
  gtk_container_border_width(GTK_CONTAINER(scroll_win), 5);
  gtk_container_add(GTK_CONTAINER(scroll_win), tree);

  bbox = gtk_hbutton_box_new();
  gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
  gtk_box_pack_start(GTK_BOX(vbox), bbox, FALSE, FALSE, 10);

  if (multiple_selection)
  {
    button = gtk_button_new_with_label(SELECT_ALL_STR);
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

    gtk_signal_connect(GTK_OBJECT(button), "clicked",
                       GTK_SIGNAL_FUNC(gnc_option_account_select_all_cb),
                       option);

    button = gtk_button_new_with_label(CLEAR_ALL_STR);
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

    gtk_signal_connect(GTK_OBJECT(button), "clicked",
                       GTK_SIGNAL_FUNC(gnc_option_account_clear_all_cb),
                       option);
  }

  button = gtk_button_new_with_label(SELECT_DEFAULT_STR);
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(button), "clicked",
		     GTK_SIGNAL_FUNC(default_button_cb), option);

  option->widget = tree;

  return frame;
}

static void
gnc_option_list_select_cb(GtkCList *clist, gint row, gint column,
                          GdkEventButton *event, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  option->changed = TRUE;

  gtk_clist_set_row_data(clist, row, GINT_TO_POINTER(TRUE));

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(clist));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static void
gnc_option_list_unselect_cb(GtkCList *clist, gint row, gint column,
                            GdkEventButton *event, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  option->changed = TRUE;

  gtk_clist_set_row_data(clist, row, GINT_TO_POINTER(FALSE));

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(clist));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static void
gnc_option_list_select_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  gtk_clist_select_all(GTK_CLIST(option->widget));

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static void
gnc_option_list_clear_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  gtk_clist_unselect_all(GTK_CLIST(option->widget));

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static GtkWidget *
gnc_option_create_list_widget(GNCOption *option, char *name)
{
  GtkWidget *scroll_win;
  GtkWidget *top_hbox;
  GtkWidget *button;
  GtkWidget *frame;
  GtkWidget *clist;
  GtkWidget *hbox;
  GtkWidget *bbox;
  gint num_values;
  gint width;
  gint i;

  top_hbox = gtk_hbox_new(FALSE, 0);

  frame = gtk_frame_new(name);
  gtk_box_pack_start(GTK_BOX(top_hbox), frame, FALSE, FALSE, 0);

  hbox = gtk_hbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(frame), hbox);

  clist = gtk_clist_new(1);
  gtk_clist_column_titles_hide(GTK_CLIST(clist));
  gtk_clist_set_selection_mode(GTK_CLIST(clist), GTK_SELECTION_MULTIPLE);

  num_values = gnc_option_num_permissible_values(option);
  for (i = 0; i < num_values; i++)
  {
    gchar *text[1];
    gchar *string;

    string = gnc_option_permissible_value_name(option, i);
    if (string != NULL)
    {
      text[0] = _(string);
      gtk_clist_append(GTK_CLIST(clist), text);
      gtk_clist_set_row_data(GTK_CLIST(clist), i, GINT_TO_POINTER(FALSE));
      free(string);
    }
    else
    {
      PERR("gnc_option_create_list_widget: bad value name\n");
    }
  }

  scroll_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                 GTK_POLICY_NEVER, 
                                 GTK_POLICY_AUTOMATIC);

  width = gtk_clist_columns_autosize(GTK_CLIST(clist));
  gtk_widget_set_usize(scroll_win, width + 50, 0);

  gtk_box_pack_start(GTK_BOX(hbox), scroll_win, FALSE, FALSE, 0);
  gtk_container_border_width(GTK_CONTAINER(scroll_win), 5);
  gtk_container_add(GTK_CONTAINER(scroll_win), clist);

  bbox = gtk_vbutton_box_new();
  gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
  gtk_box_pack_start(GTK_BOX(hbox), bbox, FALSE, FALSE, 10);

  button = gtk_button_new_with_label(SELECT_ALL_STR);
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_option_list_select_all_cb),
                     option);

  button = gtk_button_new_with_label(CLEAR_ALL_STR);
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_option_list_clear_all_cb),
                     option);

  button = gtk_button_new_with_label(SELECT_DEFAULT_STR);
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(button), "clicked",
		     GTK_SIGNAL_FUNC(default_button_cb), option);

  option->widget = clist;

  return top_hbox;
}

static void
gnc_option_color_changed_cb(GnomeColorPicker *picker, guint arg1, guint arg2,
                            guint arg3, guint arg4, gpointer data)
{
  GtkWidget *pbox;
  GNCOption *option = data;

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(picker));
  gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));
}

static void
gnc_option_set_ui_widget(GNCOption *option,
                         GtkBox *page_box,
                         GtkTooltips *tooltips)
{
  GtkWidget *enclosing = NULL;
  GtkWidget *value = NULL;
  gboolean packed = FALSE;
  char *raw_name, *raw_documentation;
  char *name, *documentation;
  char *type;

  type = gnc_option_type(option);
  if (type == NULL)
    return;

  raw_name = gnc_option_name(option);
  if (raw_name != NULL)
    name = _(raw_name);
  else
    name = NULL;

  raw_documentation = gnc_option_documentation(option);
  if (raw_documentation != NULL)
    documentation = _(raw_documentation);
  else
    documentation = NULL;

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
		     gnc_option_create_default_button(option, tooltips),
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
		     gnc_option_create_default_button(option, tooltips),
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
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);
  }
  else if (safe_strcmp(type, "date") == 0)
  {
    GtkWidget *entry;
    GtkWidget *label;
    gchar *colon_name;
    gboolean show_time;
    gboolean use24;

    colon_name = g_strconcat(name, ":", NULL);
    label= gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);

    show_time = gnc_option_show_time(option);
    use24 = gnc_lookup_boolean_option("International",
                                      "Use 24-hour time format", FALSE);

    value = gnome_date_edit_new(time(NULL), show_time, use24);

    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);

    entry = GNOME_DATE_EDIT(value)->date_entry;
    gtk_tooltips_set_tip(tooltips, entry, documentation, NULL);
    gtk_signal_connect(GTK_OBJECT(entry), "changed",
		       GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

    if (show_time)
    {
      entry = GNOME_DATE_EDIT(value)->time_entry;
      gtk_tooltips_set_tip(tooltips, entry, documentation, NULL);
      gtk_signal_connect(GTK_OBJECT(entry), "changed",
                         GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);
    }

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);
  }
  else if (safe_strcmp(type, "account-list") == 0)
  {
    enclosing = gnc_option_create_account_widget(option, name);
    value = option->widget;

    gtk_tooltips_set_tip(tooltips, enclosing, documentation, NULL);

    gtk_box_pack_start(page_box, enclosing, FALSE, FALSE, 5);
    packed = TRUE;

    gtk_widget_realize(value);

    gnc_option_set_ui_value(option, FALSE);

    gtk_signal_connect(GTK_OBJECT(value), "select_account",
                       GTK_SIGNAL_FUNC(gnc_option_account_cb), option);
    gtk_signal_connect(GTK_OBJECT(value), "unselect_account",
                       GTK_SIGNAL_FUNC(gnc_option_account_cb), option);

    gtk_clist_set_row_height(GTK_CLIST(value), 0);
    gtk_widget_set_usize(value, 0, GTK_CLIST(value)->row_height * 10);
  }
  else if (safe_strcmp(type, "list") == 0)
  {
    gint num_lines;

    enclosing = gnc_option_create_list_widget(option, name);
    value = option->widget;

    gtk_tooltips_set_tip(tooltips, enclosing, documentation, NULL);

    gtk_box_pack_start(page_box, enclosing, FALSE, FALSE, 5);
    packed = TRUE;

    gtk_widget_realize(value);

    gnc_option_set_ui_value(option, FALSE);

    gtk_signal_connect(GTK_OBJECT(value), "select_row",
                       GTK_SIGNAL_FUNC(gnc_option_list_select_cb), option);
    gtk_signal_connect(GTK_OBJECT(value), "unselect_row",
                       GTK_SIGNAL_FUNC(gnc_option_list_unselect_cb), option);

    num_lines = gnc_option_num_permissible_values(option);
    num_lines = MIN(num_lines, 9) + 1;

    gtk_clist_set_row_height(GTK_CLIST(value), 0);
    gtk_widget_set_usize(value, 0, GTK_CLIST(value)->row_height * num_lines);
  }
  else if (safe_strcmp(type, "number-range") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;
    GtkAdjustment *adj;
    gdouble lower_bound = G_MINDOUBLE;
    gdouble upper_bound = G_MAXDOUBLE;
    gdouble step_size = 1.0;
    int num_decimals = 0;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);

    gnc_option_get_range_info(option, &lower_bound, &upper_bound,
                              &num_decimals, &step_size);
    adj = GTK_ADJUSTMENT(gtk_adjustment_new(lower_bound, lower_bound,
                                            upper_bound, step_size,
                                            step_size * 5.0,
                                            step_size * 5.0));
    value = gtk_spin_button_new(adj, step_size, num_decimals);
    gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(value), TRUE);

    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);

    gtk_signal_connect(GTK_OBJECT(value), "changed",
		       GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);
  }
  else if (safe_strcmp(type, "color") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;
    gboolean use_alpha;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 0.95, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);

    use_alpha = gnc_option_use_alpha(option);

    value = gnome_color_picker_new();
    gnome_color_picker_set_title(GNOME_COLOR_PICKER(value), name);
    gnome_color_picker_set_use_alpha(GNOME_COLOR_PICKER(value), use_alpha);

    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);

    gtk_signal_connect(GTK_OBJECT(value), "color-set",
		       GTK_SIGNAL_FUNC(gnc_option_color_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);
  }
  else
  {
    PERR("gnc_option_set_ui_widget: Unknown type. Ignoring.\n");
  }

  if (!packed && (enclosing != NULL))
    gtk_box_pack_start(page_box, enclosing, FALSE, FALSE, 0);

  if (value != NULL)
    gtk_tooltips_set_tip(tooltips, value, documentation, NULL);

  if (enclosing != NULL)
    gtk_widget_show_all(enclosing);

  if (raw_name != NULL)
    free(raw_name);
  if (raw_documentation != NULL)
    free(raw_documentation);
  free(type);
}

static void
gnc_options_dialog_add_option(GtkWidget *page,
                              GNCOption *option,
                              GtkTooltips *tooltips)
{
  gnc_option_set_ui_widget(option, GTK_BOX(page), tooltips);
}

static void
gnc_options_dialog_append_page(GnomePropertyBox *propertybox,
                               GNCOptionSection *section,
                               GtkTooltips *tooltips)
{
  GNCOption *option;
  GtkWidget *page_label;
  GtkWidget *page_content_box;
  gint num_options;
  gchar *name;
  gint i;

  name = gnc_option_section_name(section);
  if (strncmp(name, "__", 2) == 0)
    return;

  page_label = gtk_label_new(_(name));
  gtk_widget_show(page_label);

  page_content_box = gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(page_content_box), 5);
  gtk_widget_show(page_content_box);

  gnome_property_box_append_page(propertybox, page_content_box, page_label);

  num_options = gnc_option_section_num_options(section);
  for (i = 0; i < num_options; i++)
  {
    option = gnc_get_option_section_option(section, i);
    gnc_options_dialog_add_option(page_content_box, option, tooltips);
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
  GtkTooltips *tooltips;
  GNCOptionSection *section;
  gint num_sections;
  gint i;

  tooltips = gtk_tooltips_new();

  num_sections = gnc_option_db_num_sections(odb);

  for (i = 0; i < num_sections; i++)
  {
    section = gnc_option_db_get_section(odb, i);
    gnc_options_dialog_append_page(propertybox, section, tooltips);
  }
}
