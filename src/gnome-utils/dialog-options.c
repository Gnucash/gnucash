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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <gnome.h>

#include "dialog-options.h"
#include "dialog-utils.h"
#include "engine-helpers.h"
#include "glib-helpers.h"
#include "global-options.h"
#include "gnc-account-tree.h"
#include "gnc-commodity-edit.h"
#include "gnc-general-select.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-engine-util.h"
#include "gnc-engine.h"
#include "gnc-gui-query.h"
#include "gnc-ui.h"
#include "guile-util.h"
#include "messages.h"
#include "option-util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

static GHashTable *optionTable = NULL;

struct gnc_option_win
{
  GtkWidget  * container;
  GtkWidget  * notebook;

  gboolean toplevel;

  GtkTooltips * tips;

  GNCOptionWinCallback apply_cb;
  gpointer             apply_cb_data;
  
  GNCOptionWinCallback help_cb;
  gpointer             help_cb_data;

  GNCOptionWinCallback close_cb;
  gpointer             close_cb_data;
};

typedef enum {
  GNC_RD_WID_AB_BUTTON_POS = 0,
  GNC_RD_WID_AB_WIDGET_POS,
  GNC_RD_WID_REL_BUTTON_POS,
  GNC_RD_WID_REL_WIDGET_POS} GNCRdPositions;


static GNCOptionWinCallback global_help_cb = NULL;
gpointer global_help_cb_data = NULL;


static GtkWidget *
gnc_options_dialog_get_apply_button (GtkWidget *widget)
{
  while (widget)
  {
    GtkWidget *button;

    button = gtk_object_get_data (GTK_OBJECT (widget),
                                  "gnc_option_apply_button");
    if (button)
      return button;

    widget = widget->parent;
  }

  return NULL;
}

static void
gnc_options_dialog_changed_internal (GtkWidget *widget)
{
  GtkWidget *button;

  button = gnc_options_dialog_get_apply_button (widget);
  if (!button)
    return;

  gtk_widget_set_sensitive (button, TRUE);
}

static void
gnc_options_dialog_clear_changed (GtkWidget *widget)
{
  GtkWidget *button;

  button = gnc_options_dialog_get_apply_button (widget);
  if (!button)
    return;

  gtk_widget_set_sensitive (button, FALSE);
}

void
gnc_options_dialog_changed (GNCOptionWin *win)
{
  if (!win) return;

  gnc_options_dialog_changed_internal (win->container);
}

static void
gnc_option_changed_cb(GtkEditable *editable, gpointer data)
{
  GtkWidget *raw;
  GNCOption *option = data;

  raw = GTK_WIDGET(editable);
  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gnc_options_dialog_changed_internal (raw);
}

static void 
gnc_date_option_changed_cb(GtkWidget *dummy, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gnc_options_dialog_changed_internal (gnc_option_get_widget (option));
}

static void
gnc_date_option_set_select_method(GNCOption *option, gboolean use_absolute,
                                  gboolean set_buttons)
{
  GList* widget_list;
  GtkWidget *ab_button, *rel_button, *rel_widget, *ab_widget;
  GtkWidget *widget;

  widget = gnc_option_get_widget (option);

  widget_list = gtk_container_children(GTK_CONTAINER(widget));
  ab_button = g_list_nth_data(widget_list, GNC_RD_WID_AB_BUTTON_POS);
  ab_widget = g_list_nth_data(widget_list, GNC_RD_WID_AB_WIDGET_POS);
  rel_button = g_list_nth_data(widget_list, GNC_RD_WID_REL_BUTTON_POS);
  rel_widget = g_list_nth_data(widget_list, GNC_RD_WID_REL_WIDGET_POS);

  if(use_absolute)
  {
    gtk_widget_set_sensitive(ab_widget, TRUE);
    gtk_widget_set_sensitive(rel_widget, FALSE);
    if(set_buttons)
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(ab_button), TRUE);
    }
  }
  else
  {
    gtk_widget_set_sensitive(rel_widget, TRUE);
    gtk_widget_set_sensitive(ab_widget, FALSE);
    if (set_buttons)
    {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(rel_button), TRUE);
    }
  }
}

static void 
gnc_rd_option_ab_set_cb(GtkWidget *widget, gpointer *raw_option)
{
  GNCOption *option = (GNCOption *) raw_option;
  gnc_date_option_set_select_method(option, TRUE, FALSE);
  gnc_date_option_changed_cb(widget, option);
}

static void
gnc_rd_option_rel_set_cb(GtkWidget *widget, gpointer *raw_option)
{
  GNCOption *option = (GNCOption *) raw_option;
  gnc_date_option_set_select_method(option, FALSE, FALSE);
  gnc_date_option_changed_cb(widget, option);
  return;
}

/********************************************************************\
 * gnc_option_set_ui_value_internal                                 *
 *   sets the GUI representation of an option with either its       *
 *   current guile value, or its default value                      *
 *                                                                  *
 * Args: option      - option structure containing option           *
 *       use_default - if true, use the default value, otherwise    *
 *                     use the current value                        *
 * Return: nothing                                                  *
\********************************************************************/

static void
gnc_option_set_ui_value_internal (GNCOption *option, gboolean use_default)
{
  gboolean bad_value = FALSE;
  GtkWidget *widget;
  char *type;
  SCM getter;
  SCM value;
  GNCOptionDef_t *option_def;

  widget = gnc_option_get_widget (option);
  if (!widget)
    return;

  type = gnc_option_type(option);

  if (use_default)
    getter = gnc_option_default_getter(option);
  else
    getter = gnc_option_getter(option);

  value = gh_call0(getter);

  option_def = gnc_options_ui_get_option (type);
  if (option_def && option_def->set_value)
  {
    bad_value = option_def->set_value (option, use_default, widget, value);
  }
  else
  {
    PERR("Unknown type. Ignoring.\n");
  }

  if (bad_value)
  {
    PERR("bad value\n");
  }

  free(type);
}


/********************************************************************\
 * gnc_option_get_ui_value_internal                                 *
 *   returns the SCM representation of the GUI option value         *
 *                                                                  *
 * Args: option - option structure containing option                *
 * Return: SCM handle to GUI option value                           *
\********************************************************************/
static SCM
gnc_option_get_ui_value_internal (GNCOption *option)
{
  SCM result = SCM_UNDEFINED;
  GtkWidget *widget;
  char *type;
  GNCOptionDef_t *option_def;

  widget = gnc_option_get_widget (option);
  if (!widget)
    return result;

  type = gnc_option_type(option);

  option_def = gnc_options_ui_get_option (type);
  if (option_def && option_def->get_value)
  {
    result = option_def->get_value (option, widget);
  }
  else
  {
    PERR("Unknown type for refresh. Ignoring.\n");
  }

  free(type);

  return result;
}


/********************************************************************\
 * gnc_option_set_selectable_internal                               *
 *   Change the selectable state of the widget that represents a    *
 *   GUI option.                                                    *
 *                                                                  *
 * Args: option      - option to change widget state for            *
 *       selectable  - if false, update the widget so that it       *
 *                     cannot be selected by the user.  If true,    *
 *                     update the widget so that it can be selected.*
 * Return: nothing                                                  *
\********************************************************************/
static void
gnc_option_set_selectable_internal (GNCOption *option, gboolean selectable)
{
  GtkWidget *widget;

  widget = gnc_option_get_widget (option);
  if (!widget)
    return;

  gtk_widget_set_sensitive (widget, selectable);
}


static void
default_button_cb(GtkButton *button, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_ui_value (option, TRUE);

  gnc_option_set_changed (option, TRUE);

  gnc_options_dialog_changed_internal (GTK_WIDGET(button));
}

static GtkWidget *
gnc_option_create_default_button(GNCOption *option, GtkTooltips *tooltips)
{
  GtkWidget *default_button = gtk_button_new_with_label(_("Set to default"));

  gtk_container_set_border_width(GTK_CONTAINER(default_button), 2);

  gtk_signal_connect(GTK_OBJECT(default_button), "clicked",
		     GTK_SIGNAL_FUNC(default_button_cb), option);

  gtk_tooltips_set_tip(tooltips, default_button,
                       _("Set the option to its default value"), NULL);

  return default_button;
}

static void
gnc_option_toggled_cb(GtkToggleButton *button, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc (option);

  gnc_options_dialog_changed_internal (GTK_WIDGET(button));
}


static void
gnc_option_multichoice_cb(GtkWidget *w, gint index, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *widget;
  GtkWidget *omenu;
  gpointer _current;
  gint current;

  widget = gnc_option_get_widget (option);

  _current = gtk_object_get_data(GTK_OBJECT(widget), "gnc_multichoice_index");
  current = GPOINTER_TO_INT(_current);

  if (current == index)
    return;

  gtk_option_menu_set_history(GTK_OPTION_MENU(widget), index);
  gtk_object_set_data(GTK_OBJECT(widget), "gnc_multichoice_index",
                      GINT_TO_POINTER(index));

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  omenu = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_menu");
  gnc_options_dialog_changed_internal (omenu);
}

static void
gnc_option_radiobutton_cb(GtkWidget *w, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *widget;
  gpointer _current, _new_value;
  gint current, new_value;

  widget = gnc_option_get_widget (option);

  _current = gtk_object_get_data(GTK_OBJECT(widget), "gnc_radiobutton_index");
  current = GPOINTER_TO_INT (_current);

  _new_value = gtk_object_get_data (GTK_OBJECT(w), "gnc_radiobutton_index");
  new_value = GPOINTER_TO_INT (_new_value);

  if (current == new_value)
    return;

  gtk_object_set_data (GTK_OBJECT(widget), "gnc_radiobutton_index",
		       GINT_TO_POINTER(new_value));

  gnc_option_set_changed (option, TRUE);
  gnc_option_call_option_widget_changed_proc(option);
  gnc_options_dialog_changed_internal (widget);
}

static void
gnc_option_rd_combo_cb(GtkWidget *w, gint index, gpointer data)
{
  GtkWidget *widget, *omenu;
  GList *children;
  GNCOption *option = data;
  gpointer _current;
  gint current;

  children =
    gtk_container_children(GTK_CONTAINER(gnc_option_get_widget (option)));

  widget = g_list_nth_data(children, GNC_RD_WID_REL_WIDGET_POS);
				     
  _current = gtk_object_get_data(GTK_OBJECT(widget),
                                 "gnc_multichoice_index");
  current = GPOINTER_TO_INT(_current);

  if (current == index)
    return;

  gtk_option_menu_set_history(GTK_OPTION_MENU(widget), index);
  gtk_object_set_data(GTK_OBJECT(widget), "gnc_multichoice_index",
                      GINT_TO_POINTER(index));

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  omenu = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_menu");
  gnc_options_dialog_changed_internal (omenu);
}

static GtkWidget *
gnc_option_create_date_widget (GNCOption *option)
{
  GtkWidget * box = NULL;
  GtkWidget *rel_button= NULL, *ab_button=NULL;
  GtkWidget *rel_widget=NULL, *ab_widget=NULL;
  GtkWidget *entry;
  gboolean show_time, use24;
  GNCOptionInfo *info;
  char *type;
  char **raw_strings;
  char **raw;
  int num_values;

  type = gnc_option_date_option_get_subtype(option);
  show_time = gnc_option_show_time(option);
  use24 = gnc_lookup_boolean_option("International", 
				    "Use 24-hour time format", FALSE);

  if (safe_strcmp(type, "relative") != 0)
  {
    ab_widget = gnc_date_edit_new(time(NULL), show_time, use24);
    entry = GNC_DATE_EDIT(ab_widget)->date_entry;
    gtk_signal_connect(GTK_OBJECT(entry), "changed",
		       GTK_SIGNAL_FUNC(gnc_date_option_changed_cb), option);
    if (show_time)
    {
      entry = GNC_DATE_EDIT(ab_widget)->time_entry;
      gtk_signal_connect(GTK_OBJECT(entry), "changed",
		       GTK_SIGNAL_FUNC(gnc_date_option_changed_cb), option);
    }
  }
    
  if (safe_strcmp(type, "absolute") != 0)
  { 
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
      if(safe_strcmp(type, "both") == 0)
      {
        info[i].callback = gnc_option_rd_combo_cb;
      }
      else 
      {
        info[i].callback = gnc_option_multichoice_cb;
      }
      info[i].user_data = option;

      raw++;
    }

    rel_widget = gnc_build_option_menu(info, num_values);
  
    for (i = 0; i < num_values * 2; i++)
      if (raw_strings[i] != NULL)
        free(raw_strings[i]);

    g_free(raw_strings);
    g_free(info);
  }

  if(safe_strcmp(type, "absolute") == 0)
  {
    free(type);
    gnc_option_set_widget (option, ab_widget);
    return ab_widget;
  }
  else if (safe_strcmp(type, "relative") == 0)
  {
    gnc_option_set_widget (option, rel_widget);
    free(type);

    return rel_widget;
  }
  else if (safe_strcmp(type, "both") == 0)
  {
    box = gtk_hbox_new(FALSE, 5);

    ab_button = gtk_radio_button_new(NULL);
    gtk_signal_connect(GTK_OBJECT(ab_button), "toggled",
		       GTK_SIGNAL_FUNC(gnc_rd_option_ab_set_cb), option);

    rel_button = gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(ab_button));
    gtk_signal_connect(GTK_OBJECT(rel_button), "toggled",
		       GTK_SIGNAL_FUNC(gnc_rd_option_rel_set_cb), option);

    gtk_box_pack_start(GTK_BOX(box), ab_button, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(box), ab_widget, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(box), rel_button, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(box), rel_widget, FALSE, FALSE, 0);

    free(type);

    gnc_option_set_widget (option, box);

    return box;
  }
  else /* can't happen */
  {
    return NULL;
  }
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
    raw++;
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
radiobutton_destroy_cb (GtkObject *obj, gpointer data)
{
  GtkTooltips *tips = data;

  gtk_object_unref (GTK_OBJECT (tips));
}

static GtkWidget *
gnc_option_create_radiobutton_widget(char *name, GNCOption *option)
{
  GtkTooltips *tooltips;
  GtkWidget *frame, *box;
  GtkWidget *widget = NULL;
  int num_values;
  char *label;
  char *tip;
  int i;

  num_values = gnc_option_num_permissible_values(option);

  g_return_val_if_fail(num_values >= 0, NULL);

  /* Create our button frame */
  frame = gtk_frame_new (name);

  /* Create the button box */
  box = gtk_vbutton_box_new ();
  gtk_container_add (GTK_CONTAINER (frame), box);

  /* Create the tooltips */
  tooltips = gtk_tooltips_new ();
  gtk_object_ref (GTK_OBJECT (tooltips));
  gtk_object_sink (GTK_OBJECT (tooltips));

  /* Iterate over the options and create a radio button for each one */
  for (i = 0; i < num_values; i++)
  {
    label = gnc_option_permissible_value_name(option, i);
    tip = gnc_option_permissible_value_description(option, i);

    widget =
      gtk_radio_button_new_with_label_from_widget (widget ?
						   GTK_RADIO_BUTTON (widget) :
						   NULL,
						   label ? _(label) : "");
    gtk_object_set_data (GTK_OBJECT (widget), "gnc_radiobutton_index",
			 GINT_TO_POINTER (i));
    gtk_tooltips_set_tip(tooltips, widget, tip ? _(tip) : "", NULL);
    gtk_signal_connect(GTK_OBJECT(widget), "toggled",
		       GTK_SIGNAL_FUNC(gnc_option_radiobutton_cb), option);
    gtk_box_pack_start (GTK_BOX (box), widget, FALSE, FALSE, 0);

    if (label)
      free (label);
    if (tip)
      free (tip);
  }

  gtk_signal_connect (GTK_OBJECT (frame), "destroy",
                      GTK_SIGNAL_FUNC (radiobutton_destroy_cb), tooltips);

  return frame;
}

static void
gnc_option_account_cb(GNCAccountTree *tree, Account * account, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc (option);

  gnc_options_dialog_changed_internal (GTK_WIDGET(tree));
}

static void
gnc_option_account_select_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;

  gtk_clist_select_all(GTK_CLIST(gnc_option_get_widget (option)));

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gnc_options_dialog_changed_internal (widget);
}

static void
gnc_option_account_clear_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;

  gtk_clist_unselect_all(GTK_CLIST(gnc_option_get_widget (option)));

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gnc_options_dialog_changed_internal (widget);
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
    button = gtk_button_new_with_label(_("Select All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

    gtk_signal_connect(GTK_OBJECT(button), "clicked",
                       GTK_SIGNAL_FUNC(gnc_option_account_select_all_cb),
                       option);

    button = gtk_button_new_with_label(_("Clear All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

    gtk_signal_connect(GTK_OBJECT(button), "clicked",
                       GTK_SIGNAL_FUNC(gnc_option_account_clear_all_cb),
                       option);
  }

  button = gtk_button_new_with_label(_("Select Default"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(button), "clicked",
		     GTK_SIGNAL_FUNC(default_button_cb), option);

  gnc_option_set_widget (option, tree);

  return frame;
}

static void
gnc_option_list_select_cb(GtkCList *clist, gint row, gint column,
                          GdkEventButton *event, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gtk_clist_set_row_data(clist, row, GINT_TO_POINTER(TRUE));

  gnc_options_dialog_changed_internal (GTK_WIDGET(clist));
}

static void
gnc_option_list_unselect_cb(GtkCList *clist, gint row, gint column,
                            GdkEventButton *event, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gtk_clist_set_row_data(clist, row, GINT_TO_POINTER(FALSE));

  gnc_options_dialog_changed_internal (GTK_WIDGET(clist));
}

static void
gnc_option_list_select_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;

  gtk_clist_select_all(GTK_CLIST(gnc_option_get_widget (option)));

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gnc_options_dialog_changed_internal (widget);
}

static void
gnc_option_list_clear_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;

  gtk_clist_unselect_all(GTK_CLIST(gnc_option_get_widget (option)));

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gnc_options_dialog_changed_internal (widget);
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
      PERR("bad value name\n");
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

  button = gtk_button_new_with_label(_("Select All"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_option_list_select_all_cb),
                     option);

  button = gtk_button_new_with_label(_("Clear All"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_option_list_clear_all_cb),
                     option);

  button = gtk_button_new_with_label(_("Select Default"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);

  gtk_signal_connect(GTK_OBJECT(button), "clicked",
		     GTK_SIGNAL_FUNC(default_button_cb), option);

  gnc_option_set_widget (option, clist);

  return top_hbox;
}

static void
gnc_option_color_changed_cb(GnomeColorPicker *picker, guint arg1, guint arg2,
                            guint arg3, guint arg4, gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gnc_options_dialog_changed_internal (GTK_WIDGET(picker));
}

static void
gnc_option_font_changed_cb(GnomeFontPicker *picker, gchar *font_name,
                           gpointer data)
{
  GNCOption *option = data;

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  gnc_options_dialog_changed_internal (GTK_WIDGET(picker));
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
  GNCOptionDef_t *option_def;

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

  option_def = gnc_options_ui_get_option (type);
  if (option_def && option_def->set_widget)
  {
    value = option_def->set_widget (option, page_box,
				    tooltips, name, documentation,
				    /* Return values */
				    &enclosing, &packed);
  }
  else
  {
    PERR("Unknown type. Ignoring.\n");
  }

  if (!packed && (enclosing != NULL))
    gtk_box_pack_start(page_box, enclosing, FALSE, FALSE, 0);

  if (value != NULL)
    gtk_tooltips_set_tip(tooltips, value, documentation, NULL);

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

static gint
gnc_options_dialog_append_page(GNCOptionWin * propertybox,
                               GNCOptionSection *section)
{
  GNCOption *option;
  GtkWidget *page_label;
  GtkWidget *page_content_box;
  gint num_options;
  const char *name;
  gint i;

  name = gnc_option_section_name(section);
  if (!name)
    return -1;

  if (strncmp(name, "__", 2) == 0)
    return -1;

  page_label = gtk_label_new(_(name));
  gtk_widget_show(page_label);

  page_content_box = gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(page_content_box), 5);
  gtk_widget_show(page_content_box);
  
  gtk_notebook_append_page(GTK_NOTEBOOK(propertybox->notebook), 
                           page_content_box, page_label);
  
  num_options = gnc_option_section_num_options(section);
  for (i = 0; i < num_options; i++)
  {
    option = gnc_get_option_section_option(section, i);
    gnc_options_dialog_add_option(page_content_box, option,
                                  propertybox->tips);
  }
  
  return gtk_notebook_page_num(GTK_NOTEBOOK(propertybox->notebook),
                               page_content_box);
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
gnc_build_options_dialog_contents(GNCOptionWin *propertybox,
                                  GNCOptionDB  *odb)
{
  GNCOptionSection *section;
  gchar *default_section_name;
  gint default_page = -1;
  gint num_sections;
  gint page;
  gint i, j;

  g_return_if_fail (propertybox != NULL);
  g_return_if_fail (odb != NULL);

  gnc_option_db_set_ui_callbacks (odb,
                                  gnc_option_get_ui_value_internal,
                                  gnc_option_set_ui_value_internal,
                                  gnc_option_set_selectable_internal);

  propertybox->tips = gtk_tooltips_new();

  gtk_object_ref (GTK_OBJECT (propertybox->tips));
  gtk_object_sink (GTK_OBJECT (propertybox->tips));

  num_sections = gnc_option_db_num_sections(odb);
  default_section_name = gnc_option_db_get_default_section(odb);

  for (i = 0; i < num_sections; i++)
  {
    const char *section_name;

    section = gnc_option_db_get_section(odb, i);
    page = gnc_options_dialog_append_page(propertybox, section);

    section_name = gnc_option_section_name(section);
    if (safe_strcmp(section_name, default_section_name) == 0)
      default_page = page;
  }

  if (default_page >= 0)
    gtk_notebook_set_page(GTK_NOTEBOOK(propertybox->notebook), default_page);

  if (default_section_name != NULL)
    free(default_section_name);

  /* call each option widget changed callbacks once at this point,
   * now that all options widgets exist.
   */
  for (i = 0; i < num_sections; i++)
  {
    section = gnc_option_db_get_section(odb, i);

    for (j = 0; j < gnc_option_section_num_options(section); j++)
    {
      gnc_option_call_option_widget_changed_proc(
              gnc_get_option_section_option(section, j) );
    }
  }

  gnc_options_dialog_clear_changed(propertybox->container);
}


GtkWidget *
gnc_options_dialog_widget(GNCOptionWin * win) {
  return win->container;
}

GtkWidget *
gnc_options_dialog_notebook(GNCOptionWin * win) {
  return win->notebook;
}

static void
gnc_options_dialog_apply_stub_cb(GtkWidget * w, gpointer data)
{
  GNCOptionWin * window = data;
  GtkWidget *button;

  if (window->apply_cb)
    window->apply_cb (window, window->apply_cb_data);

  button = gnc_options_dialog_get_apply_button (window->container);
  if (button)
    gtk_widget_set_sensitive (button, FALSE);
}

static void
gnc_options_dialog_help_stub_cb(GtkWidget * w, gpointer data) {
  GNCOptionWin * window = data;

  if(window->help_cb)
    (window->help_cb)(window, window->help_cb_data);
}

static void
gnc_options_dialog_destroy_stub_cb(GtkObject * obj, gpointer data) {
  GNCOptionWin * window = data;

  if (window->close_cb)
    (window->close_cb)(window, window->close_cb_data);
}

static void
gnc_options_dialog_close_stub_cb(GtkWidget * w, gpointer data) {
  GNCOptionWin * window = data;
  GtkWidget *container;

  container = window->container;

  gtk_widget_ref (container);

  gtk_signal_handler_block_by_func(GTK_OBJECT(container),
                                   GTK_SIGNAL_FUNC
                                   (gnc_options_dialog_destroy_stub_cb),
                                   data);

  if (window->close_cb)
    (window->close_cb)(window, window->close_cb_data);
  else
    gtk_widget_hide(container);

  /* at this point, window may point to freed data */
  if (!GTK_OBJECT_DESTROYED (container))
    gtk_signal_handler_unblock_by_func(GTK_OBJECT(container),
                                       GTK_SIGNAL_FUNC
                                       (gnc_options_dialog_destroy_stub_cb),
                                       data);

  gtk_widget_unref (container);
}

static void
gnc_options_dialog_ok_cb(GtkWidget * w, gpointer data) {
  gnc_options_dialog_apply_stub_cb(w, data);
  gnc_options_dialog_close_stub_cb(w, data);
}

GNCOptionWin *
gnc_options_dialog_new(gboolean make_toplevel, gchar *title) {
  GNCOptionWin * retval = g_new0(GNCOptionWin, 1);
  GtkWidget * vbox;
  GtkWidget * buttonbox;

  GtkWidget * ok_button=NULL;
  GtkWidget * apply_button=NULL;
  GtkWidget * help_button=NULL;
  GtkWidget * close_button=NULL;

  retval->toplevel = make_toplevel;

  vbox     =  gtk_vbox_new(FALSE, 5);

  if(make_toplevel) {
    retval->container = gtk_window_new(GDK_WINDOW_TOPLEVEL);
    if(title)
    {
      gtk_window_set_title(GTK_WINDOW(retval->container), title);
    }
  }
  else {
    retval->container = vbox;
    gtk_widget_ref(vbox);
    gtk_object_sink(GTK_OBJECT(vbox));
  }

  buttonbox = gtk_hbutton_box_new ();

  gtk_button_box_set_layout (GTK_BUTTON_BOX (buttonbox),
                             GTK_BUTTONBOX_SPREAD);

  gtk_button_box_set_spacing (GTK_BUTTON_BOX (buttonbox), 
			      GNOME_PAD);

  gtk_container_set_border_width(GTK_CONTAINER (buttonbox), 5);

  apply_button = gnome_stock_button (GNOME_STOCK_BUTTON_APPLY);
  help_button  = gnome_stock_button (GNOME_STOCK_BUTTON_HELP);
  ok_button    = gnome_stock_button (GNOME_STOCK_BUTTON_OK);
  close_button = gnome_stock_button (GNOME_STOCK_BUTTON_CLOSE);

  gtk_widget_set_sensitive (apply_button, FALSE);

  gtk_object_set_data (GTK_OBJECT (retval->container),
                       "gnc_option_apply_button", apply_button);

  gtk_signal_connect(GTK_OBJECT(apply_button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_apply_stub_cb),
                     retval);

  gtk_signal_connect(GTK_OBJECT(help_button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_help_stub_cb),
                     retval);

  gtk_signal_connect(GTK_OBJECT(ok_button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_ok_cb),
                     retval);

  gtk_signal_connect(GTK_OBJECT(close_button), "clicked",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_close_stub_cb),
                     retval);

  gtk_signal_connect(GTK_OBJECT(retval->container), "destroy",
                     GTK_SIGNAL_FUNC(gnc_options_dialog_destroy_stub_cb),
                     retval);

  gtk_box_pack_start(GTK_BOX(buttonbox), ok_button, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(buttonbox), apply_button, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(buttonbox), help_button, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(buttonbox), close_button, TRUE, TRUE, 0);

  retval->notebook = gtk_notebook_new();
  gtk_box_pack_start(GTK_BOX(vbox), retval->notebook, TRUE, TRUE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), GTK_WIDGET(buttonbox), FALSE, TRUE, 0);

  if(make_toplevel) {
    gtk_container_add(GTK_CONTAINER(retval->container), vbox);
  }

  gtk_widget_show_all(vbox);

  if(make_toplevel) {
    gtk_widget_show_all(retval->container);
  }
  
  return retval;
}

void 
gnc_options_dialog_set_apply_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                                gpointer data) {
  win->apply_cb = cb;
  win->apply_cb_data = data;
}

void 
gnc_options_dialog_set_help_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                               gpointer data) {
  win->help_cb = cb;
  win->help_cb_data = data;
}

void 
gnc_options_dialog_set_close_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                                gpointer data) {
  win->close_cb = cb;
  win->close_cb_data = data;
}

void
gnc_options_dialog_set_global_help_cb(GNCOptionWinCallback thunk,
                                      gpointer cb_data)
{
  global_help_cb = thunk;
  global_help_cb_data = cb_data;
}

void
gnc_options_dialog_destroy(GNCOptionWin * win) {

  if (!win) return;

  gtk_signal_disconnect_by_func(GTK_OBJECT(win->container), 
                                GTK_SIGNAL_FUNC
                                (gnc_options_dialog_destroy_stub_cb),
                                win);
  if(!win->toplevel) {
    gtk_widget_unref(win->container);
  }
  else {
    gtk_widget_destroy(win->container);
  }

  if(win->tips) {
    gtk_object_unref (GTK_OBJECT(win->tips));
  }

  win->container = NULL;
  win->notebook = NULL;
  win->apply_cb = NULL;
  win->help_cb = NULL;
  win->tips = NULL;

  g_free(win);
}


/* Global options dialog... this should house all of the config
 * options like where the docs reside, and whatever else is deemed
 * necessary */

static void
gnc_options_dialog_apply_cb(GNCOptionWin *propertybox,
                            gpointer user_data) {
  GNCOptionDB *global_options = user_data;
  gnc_option_db_commit(global_options);
}

static void
gnc_options_dialog_help_cb(GNCOptionWin *propertybox,
			   gpointer user_data) {
  if (global_help_cb)
    global_help_cb (propertybox, global_help_cb_data);
}

static void
gnc_options_dialog_close_cb(GNCOptionWin *propertybox,
                            gpointer user_data) {
  GNCOptionWin **options_dialog = user_data;

  if (!GTK_OBJECT_DESTROYED (GTK_OBJECT (propertybox->container)))
    gtk_widget_destroy (propertybox->container);

  *options_dialog = NULL;
}


void
gnc_show_options_dialog(void)
{
  static GNCOptionWin *options_dialog = NULL;
  GNCOptionDB *global_options;

  global_options = gnc_get_global_options();

  if (gnc_option_db_num_sections(global_options) == 0)
  {
    gnc_warning_dialog(_("No options!"));
    return;
  }

  if (gnc_option_db_dirty(global_options))
  {
    if (options_dialog != NULL)
      gnc_options_dialog_destroy(options_dialog);

    options_dialog = NULL;
  }

  if (options_dialog == NULL)
  {
    options_dialog = gnc_options_dialog_new(TRUE, NULL);

    gnc_build_options_dialog_contents(options_dialog, global_options);
    gnc_option_db_clean(global_options);

    gtk_window_set_title(GTK_WINDOW(options_dialog->container), 
                         _("GnuCash Preferences"));
    
    gnc_options_dialog_set_apply_cb(options_dialog, 
                                    gnc_options_dialog_apply_cb,
                                    global_options);

    gnc_options_dialog_set_help_cb(options_dialog, 
                                   gnc_options_dialog_help_cb,
                                   global_options);

    gnc_options_dialog_set_close_cb (options_dialog,
                                     gnc_options_dialog_close_cb,
                                     &options_dialog);
  }

  gtk_window_present(GTK_WINDOW(options_dialog->container));
}

/*****************************************************************/
/* Option Registration                                           */

/* SET WIDGET */

static GtkWidget *
gnc_option_set_ui_widget_boolean (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;

  *enclosing = gtk_hbox_new(FALSE, 5);
  value = gtk_check_button_new_with_label(name);

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  gtk_signal_connect(GTK_OBJECT(value), "toggled",
		     GTK_SIGNAL_FUNC(gnc_option_toggled_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);

  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_string (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  gchar *colon_name;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);
  value = gtk_entry_new();

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  gtk_signal_connect(GTK_OBJECT(value), "changed",
		     GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_text (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *frame;
  GtkWidget *scroll;

  frame = gtk_frame_new(name);

  scroll = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
				 GTK_POLICY_NEVER, 
				 GTK_POLICY_AUTOMATIC);
  gtk_container_border_width(GTK_CONTAINER(scroll), 2);

  gtk_container_add(GTK_CONTAINER(frame), scroll);

  *enclosing = gtk_hbox_new(FALSE, 10);
  value = gtk_text_new(NULL, NULL);
  gtk_text_set_word_wrap(GTK_TEXT(value), TRUE);
  gtk_text_set_editable(GTK_TEXT(value), TRUE);

  gtk_container_add (GTK_CONTAINER (scroll), value);

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  gtk_signal_connect(GTK_OBJECT(value), "changed",
		     GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), frame, TRUE, TRUE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_currency (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  gchar *colon_name;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);
  value = gnc_currency_edit_new();

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  if (documentation != NULL)
    gtk_tooltips_set_tip(tooltips, GTK_COMBO(value)->entry,
			 documentation, NULL);

  gtk_signal_connect(GTK_OBJECT(GTK_COMBO(value)->entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_commodity (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  gchar *colon_name;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);
  value = gnc_general_select_new(GNC_GENERAL_SELECT_TYPE_SELECT,
				 gnc_commodity_edit_get_string,
				 gnc_commodity_edit_new_select,
				 NULL);

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  if (documentation != NULL)
    gtk_tooltips_set_tip(tooltips, GNC_GENERAL_SELECT(value)->entry,
			 documentation, NULL);

  gtk_signal_connect(GTK_OBJECT(GNC_GENERAL_SELECT(value)->entry), "changed",
		     GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_multichoice (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  gchar *colon_name;

  colon_name = g_strconcat(name, ":", NULL);
  label= gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);

  value = gnc_option_create_multichoice_widget(option);
  gnc_option_set_widget (option, value);

  gnc_option_set_ui_value(option, FALSE);
  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_date (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  gchar *colon_name;

  colon_name = g_strconcat(name, ":", NULL);
  label= gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);

  value = gnc_option_create_date_widget(option);

  gnc_option_set_widget (option, value);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);  

  gtk_box_pack_start(page_box, *enclosing, FALSE, FALSE, 5);
  *packed = TRUE;  
  gnc_option_set_ui_value(option, FALSE);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_account_list (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;

  *enclosing = gnc_option_create_account_widget(option, name);
  value = gnc_option_get_widget (option);

  gtk_tooltips_set_tip(tooltips, *enclosing, documentation, NULL);

  gtk_box_pack_start(page_box, *enclosing, FALSE, FALSE, 5);
  *packed = TRUE;

  gtk_widget_realize(value);

  gnc_option_set_ui_value(option, FALSE);

  gtk_signal_connect(GTK_OBJECT(value), "select_account",
		     GTK_SIGNAL_FUNC(gnc_option_account_cb), option);
  gtk_signal_connect(GTK_OBJECT(value), "unselect_account",
		     GTK_SIGNAL_FUNC(gnc_option_account_cb), option);

  gtk_clist_set_row_height(GTK_CLIST(value), 0);
  gtk_widget_set_usize(value, 0, GTK_CLIST(value)->row_height * 10);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_list (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  gint num_lines;

  *enclosing = gnc_option_create_list_widget(option, name);
  value = gnc_option_get_widget (option);

  gtk_tooltips_set_tip(tooltips, *enclosing, documentation, NULL);

  gtk_box_pack_start(page_box, *enclosing, FALSE, FALSE, 5);
  *packed = TRUE;

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
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_number_range (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  gchar *colon_name;
  GtkAdjustment *adj;
  gdouble lower_bound = G_MINDOUBLE;
  gdouble upper_bound = G_MAXDOUBLE;
  gdouble step_size = 1.0;
  int num_decimals = 0;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);

  gnc_option_get_range_info(option, &lower_bound, &upper_bound,
			    &num_decimals, &step_size);
  adj = GTK_ADJUSTMENT(gtk_adjustment_new(lower_bound, lower_bound,
					  upper_bound, step_size,
					  step_size * 5.0,
					  step_size * 5.0));
  value = gtk_spin_button_new(adj, step_size, num_decimals);
  gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(value), TRUE);

  {
    GtkStyle *style;
    gdouble biggest;
    gint num_digits;

    biggest = ABS(lower_bound);
    biggest = MAX(biggest, ABS(upper_bound));

    num_digits = 0;
    while (biggest >= 1)
    {
      num_digits++;
      biggest = biggest / 10;
    }

    if (num_digits == 0)
      num_digits = 1;

    num_digits += num_decimals + 1;

    style = gtk_widget_get_style(value);
    if (style != NULL)
    {
      gchar *string;
      gint width;

      string = g_strnfill(num_digits, '8');
      
      width = gdk_text_measure(style->font, string, num_digits);

      /* sync with gtkspinbutton.c. why doesn't it do this itself? */
      width += 11 + (2 * style->klass->xthickness);

      g_free(string);

      gtk_widget_set_usize(value, width, 0);
    }
  }

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  gtk_signal_connect(GTK_OBJECT(value), "changed",
		     GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);
  
  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_color (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  gchar *colon_name;
  gboolean use_alpha;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);

  use_alpha = gnc_option_use_alpha(option);

  value = gnome_color_picker_new();
  gnome_color_picker_set_title(GNOME_COLOR_PICKER(value), name);
  gnome_color_picker_set_use_alpha(GNOME_COLOR_PICKER(value), use_alpha);

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  gtk_signal_connect(GTK_OBJECT(value), "color-set",
		     GTK_SIGNAL_FUNC(gnc_option_color_changed_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_font (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  gchar *colon_name;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);
  value = gnome_font_picker_new();
  gnome_font_picker_set_mode(GNOME_FONT_PICKER(value),
			     GNOME_FONT_PICKER_MODE_FONT_INFO);

  gnc_option_set_widget (option, value);

  gnc_option_set_ui_value(option, FALSE);

  gtk_signal_connect(GTK_OBJECT(value), "font-set",
		     GTK_SIGNAL_FUNC(gnc_option_font_changed_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_pixmap (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  GtkWidget *default_button;
  gchar *colon_name;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);
  value = gnome_pixmap_entry_new(NULL, _("Select pixmap"),
				 FALSE);
  gnome_pixmap_entry_set_preview(GNOME_PIXMAP_ENTRY(value), FALSE);

  gtk_signal_connect(GTK_OBJECT
		     (gnome_pixmap_entry_gtk_entry
		      (GNOME_PIXMAP_ENTRY(value))),
		     "changed",
		     GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);
    
  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);

  default_button = gnc_option_create_default_button(option, tooltips);
  gtk_box_pack_end(GTK_BOX(*enclosing), default_button, FALSE, FALSE, 0);

  gtk_widget_show(value);
  gtk_widget_show(label);
  gtk_widget_show(default_button);
  gtk_widget_show(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_radiobutton (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;

  *enclosing = gtk_hbox_new(FALSE, 5);

  value = gnc_option_create_radiobutton_widget(name, option);
  gnc_option_set_widget (option, value);

  gnc_option_set_ui_value(option, FALSE);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing),
		   gnc_option_create_default_button(option, tooltips),
		   FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

/* SET VALUE */

static gboolean
gnc_option_set_ui_value_boolean (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  if (gh_boolean_p(value))
  {
    gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(widget),
				gh_scm2bool(value));
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_string (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  if (gh_string_p(value))
  {
    char *string = gh_scm2newstr(value, NULL);
    gtk_entry_set_text(GTK_ENTRY(widget), string);
    free(string);
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_text (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  if (gh_string_p(value))
  {
    char *string = gh_scm2newstr(value, NULL);
    gint pos = 0;
    gtk_editable_delete_text(GTK_EDITABLE(widget), 0, -1);
    gtk_editable_insert_text(GTK_EDITABLE(widget),
			     string, strlen (string), &pos);
    free(string);
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_currency (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  gnc_commodity *commodity;

  commodity = gnc_scm_to_commodity (value);
  if (commodity)
  {
    gnc_currency_edit_set_currency(GNC_CURRENCY_EDIT(widget), commodity);
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_commodity (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  gnc_commodity *commodity;

  commodity = gnc_scm_to_commodity (value);
  if (commodity)
  {
    gnc_general_select_set_selected(GNC_GENERAL_SELECT (widget), commodity);
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_multichoice (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  int index;

  index = gnc_option_permissible_value_index(option, value);
  if (index < 0)
    return TRUE;
  else
  {
    gtk_option_menu_set_history(GTK_OPTION_MENU(widget), index);
    gtk_object_set_data(GTK_OBJECT(widget), "gnc_multichoice_index",
			GINT_TO_POINTER(index));
    return FALSE;
  }
}

static gboolean
gnc_option_set_ui_value_date (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  int index;
  char *date_option_type;
  char *symbol_str;
  gboolean bad_value = FALSE;

  date_option_type = gnc_option_date_option_get_subtype(option);

  if (gh_pair_p(value))
  {
    symbol_str = gnc_date_option_value_get_type (value);
    if (symbol_str)
    {
      if (safe_strcmp(symbol_str, "relative") == 0)
      {
	SCM relative = gnc_date_option_value_get_relative (value);

	index = gnc_option_permissible_value_index(option, relative);
	if (safe_strcmp(date_option_type, "relative") == 0)
	{
	  gtk_object_set_data(GTK_OBJECT(widget),
			      "gnc_multichoice_index",
			      GINT_TO_POINTER(index));
	  gtk_option_menu_set_history(GTK_OPTION_MENU(widget), index);
	}
	else if (safe_strcmp(date_option_type, "both") == 0)
	{
	  GList *widget_list;
	  GtkWidget *rel_date_widget;

	  widget_list = gtk_container_children(GTK_CONTAINER(widget));
	  rel_date_widget = g_list_nth_data(widget_list,
					    GNC_RD_WID_REL_WIDGET_POS);
	  gnc_date_option_set_select_method(option, FALSE, TRUE);
	  gtk_object_set_data(GTK_OBJECT(rel_date_widget),
			      "gnc_multichoice_index",
			      GINT_TO_POINTER(index));
	  gtk_option_menu_set_history(GTK_OPTION_MENU(rel_date_widget),
				      index);
	}
	else
	{
	  bad_value = TRUE;
	}
      }
      else if (safe_strcmp(symbol_str, "absolute") == 0)
      { 
	Timespec ts;

	ts = gnc_date_option_value_get_absolute (value);

	if (safe_strcmp(date_option_type, "absolute") == 0)
        {
	  gnc_date_edit_set_time(GNC_DATE_EDIT(widget), ts.tv_sec);
	}
	else if (safe_strcmp(date_option_type, "both") == 0)
        {
	  GList *widget_list;
	  GtkWidget *ab_widget;

	  widget_list = gtk_container_children(GTK_CONTAINER(widget));
	  ab_widget = g_list_nth_data(widget_list,
				      GNC_RD_WID_AB_WIDGET_POS);
	  gnc_date_option_set_select_method(option, TRUE, TRUE);
	  gnc_date_edit_set_time(GNC_DATE_EDIT(ab_widget), ts.tv_sec);
	}
	else
        {
	  bad_value = TRUE;
	}
      }
      else
      {
	bad_value = TRUE;
      }

      if (symbol_str)
	free(symbol_str);
    }
  }
  else
  {
    bad_value = TRUE;
  }

  if (date_option_type)
    free(date_option_type);

  return bad_value;
}

static gboolean
gnc_option_set_ui_value_account_list (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  GList *list;

  list = gnc_scm_list_to_glist(value);

  gtk_clist_unselect_all(GTK_CLIST(widget));
  gnc_account_tree_select_accounts(GNC_ACCOUNT_TREE(widget), list, TRUE);

  g_list_free(list);
  return FALSE;
}

static gboolean
gnc_option_set_ui_value_list (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  gint num_rows, row;

  gtk_clist_unselect_all(GTK_CLIST(widget));

  num_rows = gnc_option_num_permissible_values(option);
  for (row = 0; row < num_rows; row++)
    gtk_clist_set_row_data(GTK_CLIST(widget), row, GINT_TO_POINTER(FALSE));

  while (gh_list_p(value) && !gh_null_p(value))
  {
    SCM item;

    item = gh_car(value);
    value = gh_cdr(value);

    row = gnc_option_permissible_value_index(option, item);
    if (row < 0)
    {
      return TRUE;
    }

    gtk_clist_select_row(GTK_CLIST(widget), row, 0);
    gtk_clist_set_row_data(GTK_CLIST(widget), row, GINT_TO_POINTER(TRUE));
  }

  if (!gh_list_p(value) || !gh_null_p(value))
    return TRUE;

  return FALSE;
}

static gboolean
gnc_option_set_ui_value_number_range (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  GtkSpinButton *spinner;
  gdouble d_value;;

  spinner = GTK_SPIN_BUTTON(widget);

  if (gh_number_p(value))
  {
    d_value = gh_scm2double(value);
    gtk_spin_button_set_value(spinner, d_value);
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_color (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  gdouble red, green, blue, alpha;

  if (gnc_option_get_color_info(option, use_default,
				&red, &green, &blue, &alpha))
  {
    GnomeColorPicker *picker;

    picker = GNOME_COLOR_PICKER(widget);

    gnome_color_picker_set_d(picker, red, green, blue, alpha);
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_font (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  if (gh_string_p(value))
  {
    char *string = gh_scm2newstr(value, NULL);
    if ((string != NULL) && (*string != '\0'))
    {
      GnomeFontPicker *picker = GNOME_FONT_PICKER(widget);
      gnome_font_picker_set_font_name(picker, string);
    }
    if(string)
      free(string);
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_pixmap (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  if (gh_string_p(value))
  {
    char * string = gh_scm2newstr(value, NULL);

    if (string && *string)
    {
      GnomeFileEntry *fentry = 
	GNOME_FILE_ENTRY(gnome_pixmap_entry_gnome_file_entry
			 (GNOME_PIXMAP_ENTRY(widget)));
      gnome_file_entry_set_default_path(fentry, string);
    }
    if(string)
      free(string);
    return FALSE;
  }
  else 
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_radiobutton (GNCOption *option, gboolean use_default,
				     GtkWidget *widget, SCM value)
{
  int index;

  index = gnc_option_permissible_value_index(option, value);
  if (index < 0)
    return TRUE;
  else
  {
    GtkWidget *box, *button;
    GList *list;
    int i;
    gpointer val;

    list = gtk_container_children (GTK_CONTAINER (widget));
    box = list->data;

    list = gtk_container_children (GTK_CONTAINER (box));
    for (i = 0; i < index && list; i++)
      list = list->next;
    g_return_val_if_fail (list, TRUE);

    button = list->data;
    val = gtk_object_get_data (GTK_OBJECT (button), "gnc_radiobutton_index");
    g_return_val_if_fail (GPOINTER_TO_INT (val) == index, TRUE);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
    //    gtk_object_set_data(GTK_OBJECT(widget), "gnc_radiobutton_index",
    //			GINT_TO_POINTER(index));
    return FALSE;
  }
}

/* GET VALUE */

static SCM
gnc_option_get_ui_value_boolean (GNCOption *option, GtkWidget *widget)
{
  gboolean active;

  active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
  return gh_bool2scm(active);
}

static SCM
gnc_option_get_ui_value_string (GNCOption *option, GtkWidget *widget)
{
  char * string;
  SCM result;

  string = gtk_editable_get_chars(GTK_EDITABLE(widget), 0, -1);
  result = gh_str02scm(string);
  g_free(string);
  return result;
}

static SCM
gnc_option_get_ui_value_text (GNCOption *option, GtkWidget *widget)
{
  char * string;
  SCM result;

  string = gtk_editable_get_chars(GTK_EDITABLE(widget), 0, -1);
  result = gh_str02scm(string);
  g_free(string);
  return result;
}

static SCM
gnc_option_get_ui_value_currency (GNCOption *option, GtkWidget *widget)
{
  gnc_commodity *commodity;

  commodity =
    gnc_currency_edit_get_currency(GNC_CURRENCY_EDIT(widget));

  return (gnc_commodity_to_scm (commodity));
}

static SCM
gnc_option_get_ui_value_commodity (GNCOption *option, GtkWidget *widget)
{
  gnc_commodity *commodity;

  commodity =
    gnc_general_select_get_selected(GNC_GENERAL_SELECT(widget));

  return (gnc_commodity_to_scm(commodity));
}

static SCM
gnc_option_get_ui_value_multichoice (GNCOption *option, GtkWidget *widget)
{
  gpointer _index;
  int index;

  _index = gtk_object_get_data(GTK_OBJECT(widget), "gnc_multichoice_index");
  index = GPOINTER_TO_INT(_index);

  return (gnc_option_permissible_value(option, index));
}

static SCM
gnc_option_get_ui_value_date (GNCOption *option, GtkWidget *widget)
{
  int index;
  SCM type, val,result = SCM_UNDEFINED;
  char *subtype = gnc_option_date_option_get_subtype(option);

  if(safe_strcmp(subtype, "relative") == 0)
  {
    index = GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(widget),
						"gnc_multichoice_index"));
    type = gh_symbol2scm("relative");
    val = gnc_option_permissible_value(option, index);
    result = gh_cons(type, val);
  }
  else if (safe_strcmp(subtype, "absolute") == 0)
  { 		      
    Timespec ts;

    ts.tv_sec  = gnc_date_edit_get_date(GNC_DATE_EDIT(widget));
    ts.tv_nsec = 0;

    result = gh_cons(gh_symbol2scm("absolute"), gnc_timespec2timepair(ts));
  }
  else if (safe_strcmp(subtype, "both") == 0)
  {
    Timespec ts;
    int index;
    SCM val;
    GList *widget_list;
    GtkWidget *ab_button, *rel_widget, *ab_widget;

    widget_list = gtk_container_children(GTK_CONTAINER(widget));
    ab_button = g_list_nth_data(widget_list,  GNC_RD_WID_AB_BUTTON_POS);
    ab_widget = g_list_nth_data(widget_list,  GNC_RD_WID_AB_WIDGET_POS);
    rel_widget = g_list_nth_data(widget_list, GNC_RD_WID_REL_WIDGET_POS);

    /* if it's an absolute date */
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ab_button)))
    {
      ts.tv_sec = gnc_date_edit_get_date(GNC_DATE_EDIT(ab_widget));
      ts.tv_nsec = 0;
      result = gh_cons(gh_symbol2scm("absolute"), gnc_timespec2timepair(ts));
    }
    else 
    {
      index = GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(rel_widget),
						  "gnc_multichoice_index"));
      val = gnc_option_permissible_value(option, index);
      result = gh_cons(gh_symbol2scm("relative"), val);
    }
  }
  g_free(subtype);
  return result;
}

static SCM
gnc_option_get_ui_value_account_list (GNCOption *option, GtkWidget *widget)
{
  GNCAccountTree *tree;
  GList *list;
  SCM result;

  tree = GNC_ACCOUNT_TREE(widget);
  list = gnc_account_tree_get_current_accounts(tree);

  /* handover list */
  result = gnc_glist_to_scm_list(list, gh_eval_str("<gnc:Account*>"));
  g_list_free(list);
  return result;
}

static SCM
gnc_option_get_ui_value_list (GNCOption *option, GtkWidget *widget)
{
  SCM result;
  gboolean selected;
  GtkCList *clist;
  gint num_rows;
  gint row;

  clist = GTK_CLIST(widget);
  num_rows = gnc_option_num_permissible_values(option);
  result = gh_eval_str("()");

  for (row = 0; row < num_rows; row++)
  {
    selected = GPOINTER_TO_INT(gtk_clist_get_row_data(clist, row));
    if (selected)
      result = gh_cons(gnc_option_permissible_value(option, row), result);
  }

  return (gh_reverse(result));
}

static SCM
gnc_option_get_ui_value_number_range (GNCOption *option, GtkWidget *widget)
{
  GtkSpinButton *spinner;
  gdouble value;

  spinner = GTK_SPIN_BUTTON(widget);

  value = gtk_spin_button_get_value_as_float(spinner);

  return (gh_double2scm(value));
}

static SCM
gnc_option_get_ui_value_color (GNCOption *option, GtkWidget *widget)
{
  SCM result;
  GnomeColorPicker *picker;
  gdouble red, green, blue, alpha;
  gdouble scale;

  picker = GNOME_COLOR_PICKER(widget);

  gnome_color_picker_get_d(picker, &red, &green, &blue, &alpha);

  scale = gnc_option_color_range(option);

  result = SCM_EOL;
  result = gh_cons(gh_double2scm(alpha * scale), result);
  result = gh_cons(gh_double2scm(blue * scale), result);
  result = gh_cons(gh_double2scm(green * scale), result);
  result = gh_cons(gh_double2scm(red * scale), result);
  return result;
}

static SCM
gnc_option_get_ui_value_font (GNCOption *option, GtkWidget *widget)
{
  GnomeFontPicker *picker = GNOME_FONT_PICKER(widget);
  char * string;

  string = gnome_font_picker_get_font_name(picker);
  return (gh_str02scm(string));
}

static SCM
gnc_option_get_ui_value_pixmap (GNCOption *option, GtkWidget *widget)
{
  GnomePixmapEntry * p = GNOME_PIXMAP_ENTRY(widget);
  char             * string = gnome_pixmap_entry_get_filename(p);

  return (gh_str02scm(string ? string : ""));
}

static SCM
gnc_option_get_ui_value_radiobutton (GNCOption *option, GtkWidget *widget)
{
  gpointer _index;
  int index;

  _index = gtk_object_get_data(GTK_OBJECT(widget), "gnc_radiobutton_index");
  index = GPOINTER_TO_INT(_index);

  return (gnc_option_permissible_value(option, index));
}

/* INITIALIZATION */

static void gnc_options_initialize_options (void)
{
  static GNCOptionDef_t options[] = {
    { "boolean", gnc_option_set_ui_widget_boolean,
      gnc_option_set_ui_value_boolean, gnc_option_get_ui_value_boolean },
    { "string", gnc_option_set_ui_widget_string,
      gnc_option_set_ui_value_string, gnc_option_get_ui_value_string },
    { "text", gnc_option_set_ui_widget_text,
      gnc_option_set_ui_value_text, gnc_option_get_ui_value_text },
    { "currency", gnc_option_set_ui_widget_currency,
      gnc_option_set_ui_value_currency, gnc_option_get_ui_value_currency },
    { "commodity", gnc_option_set_ui_widget_commodity,
      gnc_option_set_ui_value_commodity, gnc_option_get_ui_value_commodity },
    { "multichoice", gnc_option_set_ui_widget_multichoice,
      gnc_option_set_ui_value_multichoice, gnc_option_get_ui_value_multichoice },
    { "date", gnc_option_set_ui_widget_date,
      gnc_option_set_ui_value_date, gnc_option_get_ui_value_date },
    { "account-list", gnc_option_set_ui_widget_account_list,
      gnc_option_set_ui_value_account_list, gnc_option_get_ui_value_account_list },
    { "list", gnc_option_set_ui_widget_list,
      gnc_option_set_ui_value_list, gnc_option_get_ui_value_list },
    { "number-range", gnc_option_set_ui_widget_number_range,
      gnc_option_set_ui_value_number_range, gnc_option_get_ui_value_number_range },
    { "color", gnc_option_set_ui_widget_color,
      gnc_option_set_ui_value_color, gnc_option_get_ui_value_color },
    { "font", gnc_option_set_ui_widget_font,
      gnc_option_set_ui_value_font, gnc_option_get_ui_value_font },
    { "pixmap", gnc_option_set_ui_widget_pixmap,
      gnc_option_set_ui_value_pixmap, gnc_option_get_ui_value_pixmap },
    { "radiobutton", gnc_option_set_ui_widget_radiobutton,
      gnc_option_set_ui_value_radiobutton, gnc_option_get_ui_value_radiobutton },
    { NULL, NULL, NULL, NULL }
  };
  int i;

  for (i = 0; options[i].option_name; i++)
    gnc_options_ui_register_option (&(options[i]));
}

/* Register a new option type in the UI */
void gnc_options_ui_register_option (GNCOptionDef_t *option)
{
  g_return_if_fail (optionTable);
  g_return_if_fail (option);

  g_hash_table_insert (optionTable, (gpointer)(option->option_name), option);
}

GNCOptionDef_t * gnc_options_ui_get_option (const char *option_name)
{
  g_return_val_if_fail (optionTable, NULL);
  g_return_val_if_fail (option_name, NULL);

  return g_hash_table_lookup (optionTable, option_name);
}

void gnc_options_ui_initialize (void)
{
  g_return_if_fail (optionTable == NULL);
  optionTable = g_hash_table_new (g_str_hash, g_str_equal);

  /* add known types */
  gnc_options_initialize_options ();
}
