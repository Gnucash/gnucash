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
#include "option-util.h"
#include "guile-util.h"
#include "query-user.h"
#include "gnc-helpers.h"
#include "account-tree.h"
#include "gnc-dateedit.h"
#include "gnc-commodity-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-engine.h"
#include "global-options.h"
#include "query-user.h"
#include "window-help.h"
#include "messages.h"
#include "gnc-engine-util.h"
#include "gnc-ui.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

struct _gnc_option_win {
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

static void
gnc_option_changed_cb(GtkEditable *editable, gpointer data)
{
  GtkWidget *raw, *pbox;
  GNCOption *option = data;

  raw = GTK_WIDGET(editable);
  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(raw);
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
}

static void 
gnc_date_option_changed_cb(GtkWidget *dummy, gpointer data)
{
  GtkWidget *pbox;
  GNCOption *option = data;
  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(option->widget);
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
}

static void
gnc_date_option_set_select_method(GNCOption *option, gboolean use_absolute,
                                  gboolean set_buttons)
{
  GList* widget_list;
  GtkWidget *ab_button, *rel_button, *rel_widget, *ab_widget;

  widget_list = gtk_container_children(GTK_CONTAINER(option->widget));
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
  else if (safe_strcmp(type, "currency") == 0)
  {
    gnc_commodity *commodity;

    commodity = gnc_scm_to_commodity (value);
    if (commodity)
      gnc_currency_edit_set_currency(GNC_CURRENCY_EDIT(option->widget),
                                     commodity);
    else
      bad_value = TRUE;
  }
  else if (safe_strcmp(type, "commodity") == 0)
  {
    gnc_commodity *commodity;

    commodity = gnc_scm_to_commodity (value);
    if (commodity)
      gnc_commodity_edit_set_commodity(GNC_COMMODITY_EDIT (option->widget),
                                       commodity);
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
    SCM symbol;
    int index;
    char *date_option_type;
    char *symbol_str;

    date_option_type = gnc_option_date_option_get_subtype(option);

    if (gh_vector_p(value))
    {
      symbol = gh_vector_ref(value, gh_int2scm(0));
      if(gh_symbol_p(symbol))
      {
	symbol_str = gh_symbol2newstr(symbol, NULL);
	if (safe_strcmp(symbol_str, "relative") == 0)
	{
	  index =
            gnc_option_permissible_value_index(option,
                                               gh_vector_ref(value,
                                                             gh_int2scm(2)));
	  if (safe_strcmp(date_option_type, "relative") == 0)
	  {      
	    gtk_object_set_data(GTK_OBJECT(option->widget),
                                "gnc_multichoice_index",
				GINT_TO_POINTER(index));
	    gtk_option_menu_set_history(GTK_OPTION_MENU(option->widget),
                                        index);
	  }
	  else if (safe_strcmp(date_option_type, "both") == 0)
	  {
	    GList *widget_list;
	    GtkWidget *rel_date_widget;
	    widget_list =
              gtk_container_children(GTK_CONTAINER(option->widget));
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
	  SCM tp;
	  tp = gh_vector_ref(value, gh_int2scm(1));
	  if (gnc_timepair_p(tp))
	  {
	    ts = gnc_timepair2timespec(tp);

	    if (safe_strcmp(date_option_type, "absolute") == 0)
	    {
	      gnc_date_edit_set_time(GNC_DATE_EDIT(option->widget), ts.tv_sec);
	    }
	    else if (safe_strcmp(date_option_type, "both") == 0)
	    {
	      GList *widget_list;
	      GtkWidget *ab_widget;
	      widget_list =
                gtk_container_children(GTK_CONTAINER(option->widget));
	      ab_widget = g_list_nth_data(widget_list,
                                          GNC_RD_WID_AB_WIDGET_POS);
	      gnc_date_option_set_select_method(option, TRUE, TRUE);
	      gnc_date_edit_set_time(GNC_DATE_EDIT(option->widget), ts.tv_sec);
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
	}
	else
	{
	  bad_value = TRUE;
	}
	free(symbol_str);
      }
    }
    else
    {
      bad_value = TRUE;
    }
    g_free(date_option_type);
  }
  else if (safe_strcmp(type, "account-list") == 0)
  {
    GList *list;

    list = gnc_scm_to_glist_account_ptr(value);

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
      if (row < 0)
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
  else if (safe_strcmp(type, "font") == 0)
  {
    if (gh_string_p(value))
    {
      char *string = gh_scm2newstr(value, NULL);
      if ((string != NULL) && (*string != '\0'))
      {
        GnomeFontPicker *picker = GNOME_FONT_PICKER(option->widget);
        gnome_font_picker_set_font_name(picker, string);
      }
      if(string) {
        free(string);
      }
    }
    else
      bad_value = TRUE;
  }
  else if(safe_strcmp(type, "pixmap") == 0) {
    /* PIXMAP */
    if(gh_string_p(value)) {
      char * string = gh_scm2newstr(value, NULL);
      if (string && *string) {
        GnomeFileEntry *fentry = 
          GNOME_FILE_ENTRY(gnome_pixmap_entry_gnome_file_entry
          (GNOME_PIXMAP_ENTRY(option->widget)));
        gnome_file_entry_set_default_path(fentry, string);
      }
      if(string) {
        free(string);
      }
    }
    else 
      bad_value = TRUE;
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
  else if (safe_strcmp(type, "currency") == 0)
  {
    gnc_commodity *commodity;

    commodity =
      gnc_currency_edit_get_currency(GNC_CURRENCY_EDIT(option->widget));

    result = gnc_commodity_to_scm (commodity);
  }
  else if (safe_strcmp(type, "commodity") == 0)
  {
    gnc_commodity *commodity;

    commodity =
      gnc_commodity_edit_get_commodity(GNC_COMMODITY_EDIT(option->widget));

    result = gnc_commodity_to_scm(commodity);
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
    int index;
    SCM type, val;
    char *subtype = gnc_option_date_option_get_subtype(option);

    if(safe_strcmp(subtype, "relative") == 0)
    {
      index = GPOINTER_TO_INT(gtk_object_get_data(GTK_OBJECT(option->widget),
						  "gnc_multichoice_index"));
      type = gh_symbol2scm("relative");
      val = gnc_option_permissible_value(option, index);
      result = gh_cons(type, val);
    }
    else if (safe_strcmp(subtype, "absolute") == 0)
    { 		      
      Timespec ts;

      ts.tv_sec  = gnc_date_edit_get_date(GNC_DATE_EDIT(option->widget));
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
      widget_list = gtk_container_children(GTK_CONTAINER(option->widget));
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
  }
  else if (safe_strcmp(type, "account-list") == 0)
  {
    GNCAccountTree *tree;
    GList *list;

    tree = GNC_ACCOUNT_TREE(option->widget);
    list = gnc_account_tree_get_current_accounts(tree);

    /* handover list */
    result = gnc_glist_account_ptr_to_scm(list);
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
  else if (safe_strcmp(type, "font") == 0)
  {
    GnomeFontPicker *picker = GNOME_FONT_PICKER(option->widget);
    char * string;

    string = gnome_font_picker_get_font_name(picker);
    result = gh_str02scm(string);
  }
  else if (safe_strcmp(type, "pixmap") == 0) 
  {
    /* PIXMAP */
    GnomePixmapEntry * p = GNOME_PIXMAP_ENTRY(option->widget);
    char             * string = gnome_pixmap_entry_get_filename(p);

    result = gh_str02scm(string);
  }  
  else
    {
    PERR("Unknown type for refresh. Ignoring.\n");
  }

  free(type);

  return result;
}


/********************************************************************\
 * gnc_set_option_selectable_by_name                                *
 *   Change the selectable state of the widget that represents a    *
 *   GUI option.                                                    *
 *                                                                  *
 * Args: section     - section of option                            *
 *       name        - name of option                               *
 *       selectable  - if false, update the widget so that it       *
 *                     cannot be selected by the user.  If true,    *
 *                     update the widget so that it can be selected.*
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_set_option_selectable_by_name( const char *section,
                                   const char *name,
                                   gboolean selectable)
{
  GNCOption *option = gnc_get_option_by_name( section, name );

  if ((option == NULL) || (option->widget == NULL))
    return;

  gtk_widget_set_sensitive( GTK_WIDGET(option->widget),
                            selectable                  );

  return;
} /* gnc_set_option_selectable_by_name */


static void
default_button_cb(GtkButton *button, gpointer data)
{
  GtkWidget *pbox;
  GNCOption *option = data;

  gnc_option_set_ui_value(option, TRUE);

  option->changed = TRUE;

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(button));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
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
  GtkWidget *pbox;
  GNCOption *option = data;

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(button));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
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

  gnc_option_call_option_widget_changed_proc(option);

  omenu = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_menu");
  pbox = gtk_widget_get_toplevel(omenu);
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
}

static void
gnc_option_rd_combo_cb(GtkWidget *w, gint index, gpointer data)
{
  
  GtkWidget *widget, *pbox, *omenu;
  GList *children;
  GNCOption *option = data;
  gpointer _current;
  gint current;

  children = gtk_container_children(GTK_CONTAINER(option->widget));
  widget = g_list_nth_data(children, GNC_RD_WID_REL_WIDGET_POS);
				     
  _current = gtk_object_get_data(GTK_OBJECT(widget),
                                 "gnc_multichoice_index");
  current = GPOINTER_TO_INT(_current);

  if (current == index)
    return;

  gtk_option_menu_set_history(GTK_OPTION_MENU(widget), index);
  gtk_object_set_data(GTK_OBJECT(widget), "gnc_multichoice_index",
                      GINT_TO_POINTER(index));

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  omenu = gtk_object_get_data(GTK_OBJECT(w), "gnc_option_menu");
  pbox = gtk_widget_get_toplevel(omenu);
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
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
  if(safe_strcmp(type, "relative") != 0)
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
    
  if(safe_strcmp(type, "absolute") != 0)
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
    option->widget = ab_widget;
    return ab_widget;
  }
  else if (safe_strcmp(type, "relative") == 0)
  {
    option->widget = rel_widget;
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
    option->widget = box;
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
gnc_option_account_cb(GNCAccountTree *tree, Account * account, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(tree));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
}

static void
gnc_option_account_select_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  gtk_clist_select_all(GTK_CLIST(option->widget));

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));*/
}

static void
gnc_option_account_clear_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  gtk_clist_unselect_all(GTK_CLIST(option->widget));

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
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

  gnc_option_call_option_widget_changed_proc(option);

  gtk_clist_set_row_data(clist, row, GINT_TO_POINTER(TRUE));

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(clist));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
}

static void
gnc_option_list_unselect_cb(GtkCList *clist, gint row, gint column,
                            GdkEventButton *event, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  gtk_clist_set_row_data(clist, row, GINT_TO_POINTER(FALSE));

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(clist));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
}

static void
gnc_option_list_select_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  gtk_clist_select_all(GTK_CLIST(option->widget));

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));*/
}

static void
gnc_option_list_clear_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *pbox;

  gtk_clist_unselect_all(GTK_CLIST(option->widget));

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(widget));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
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

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(picker));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox));*/
}

static void
gnc_option_font_changed_cb(GnomeFontPicker *picker, gchar *font_name,
                           gpointer data)
{
  GtkWidget *pbox;
  GNCOption *option = data;

  option->changed = TRUE;

  gnc_option_call_option_widget_changed_proc(option);

  pbox = gtk_widget_get_toplevel(GTK_WIDGET(picker));
  /* gnome_property_box_changed(GNOME_PROPERTY_BOX(pbox)); */
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
    gtk_widget_show_all(enclosing);
  }
  else if (safe_strcmp(type, "string") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
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
    gtk_widget_show_all(enclosing);
  }
  else if (safe_strcmp(type, "currency") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);
    value = gnc_currency_edit_new();

    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);

    if (documentation != NULL)
      gtk_tooltips_set_tip(tooltips, GTK_COMBO(value)->entry,
                           documentation, NULL);

    gtk_signal_connect(GTK_OBJECT(GTK_COMBO(value)->entry), "changed",
		       GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);
    gtk_widget_show_all(enclosing);
  }
  else if (safe_strcmp(type, "commodity") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);
    value = gnc_commodity_edit_new();

    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);

    if (documentation != NULL)
      gtk_tooltips_set_tip(tooltips, GNC_COMMODITY_EDIT(value)->entry,
                           documentation, NULL);

    gtk_signal_connect(GTK_OBJECT(GNC_COMMODITY_EDIT(value)->entry), "changed",
		       GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);
    gtk_widget_show_all(enclosing);
  }
  else if (safe_strcmp(type, "multichoice") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label= gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
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
    gtk_widget_show_all(enclosing);
  }
  else if (safe_strcmp(type, "date") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label= gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);

    value = gnc_option_create_date_widget(option);

    option->widget = value;

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);  

    gtk_box_pack_start(page_box, enclosing, FALSE, FALSE, 5);
    packed = TRUE;  
    gnc_option_set_ui_value(option, FALSE);
    gtk_widget_show_all(enclosing);
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
    gtk_widget_show_all(enclosing);
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
    gtk_widget_show_all(enclosing);
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
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
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

    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);

    gtk_signal_connect(GTK_OBJECT(value), "changed",
		       GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);
    gtk_widget_show_all(enclosing);
  }
  else if (safe_strcmp(type, "color") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;
    gboolean use_alpha;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
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
    gtk_widget_show_all(enclosing);
  }
  else if (safe_strcmp(type, "font") == 0)
  {
    GtkWidget *label;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);
    value = gnome_font_picker_new();
    gnome_font_picker_set_mode(GNOME_FONT_PICKER(value),
                               GNOME_FONT_PICKER_MODE_FONT_INFO);

    option->widget = value;

    gnc_option_set_ui_value(option, FALSE);

    gtk_signal_connect(GTK_OBJECT(value), "font-set",
		       GTK_SIGNAL_FUNC(gnc_option_font_changed_cb), option);

    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(enclosing),
		     gnc_option_create_default_button(option, tooltips),
		     FALSE, FALSE, 0);
    gtk_widget_show_all(enclosing);
  }
  else if (safe_strcmp(type, "pixmap") == 0)
  {
    GtkWidget *label;
    GtkWidget *default_button;
    gchar *colon_name;

    colon_name = g_strconcat(name, ":", NULL);
    label = gtk_label_new(colon_name);
    gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
    g_free(colon_name);

    enclosing = gtk_hbox_new(FALSE, 5);
    value = gnome_pixmap_entry_new(NULL, _("Select pixmap"),
                                   FALSE);
    gnome_pixmap_entry_set_preview(GNOME_PIXMAP_ENTRY(value), FALSE);

    gtk_signal_connect(GTK_OBJECT
                       (gnome_pixmap_entry_gtk_entry
                        (GNOME_PIXMAP_ENTRY(value))),
                       "changed",
		       GTK_SIGNAL_FUNC(gnc_option_changed_cb), option);
    
    option->widget = value;
    gnc_option_set_ui_value(option, FALSE);
    gtk_box_pack_start(GTK_BOX(enclosing), label, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), value, FALSE, FALSE, 0);
    default_button = gnc_option_create_default_button(option, tooltips);
    gtk_box_pack_end(GTK_BOX(enclosing), default_button, FALSE, FALSE, 0);

    gtk_widget_show(value);
    gtk_widget_show(label);
    gtk_widget_show(default_button);
    gtk_widget_show(enclosing);
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

}


GtkWidget *
gnc_options_dialog_widget(GNCOptionWin * win) {
  return win->container;
}

GtkWidget *
gnc_options_dialog_notebook(GNCOptionWin * win) {
  return win->notebook;
}

static int
gnc_options_dialog_apply_stub_cb(GtkWidget * w, gpointer data) {
  GNCOptionWin * window = data;
  if(window->apply_cb) {
    (window->apply_cb)(window, window->apply_cb_data);
    return TRUE;
  }
  else {
    return FALSE;
  }
}

static int
gnc_options_dialog_help_stub_cb(GtkWidget * w, gpointer data) {
  GNCOptionWin * window = data;
  if(window->help_cb) {
    (window->help_cb)(window, window->help_cb_data);
    return TRUE;
  }
  else {
    return FALSE;
  }
}

static int
gnc_options_dialog_close_stub_cb(GtkWidget * w, gpointer data) {
  GNCOptionWin * window = data;
  if(window->close_cb) {
    (window->close_cb)(window, window->close_cb_data);
    return TRUE;
  }
  else {
    gtk_widget_hide(window->container);
    return FALSE;
  }
}

static void
gnc_options_dialog_ok_cb(GtkWidget * w, gpointer data) {
  gnc_options_dialog_apply_stub_cb(w, data);
  gnc_options_dialog_close_stub_cb(w, data);
}

GNCOptionWin *
gnc_options_dialog_new(gboolean make_toplevel) {
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
gnc_options_dialog_destroy(GNCOptionWin * win) {

  if (!win) return;

  gtk_widget_destroy(win->container);

  if(!win->toplevel) {
    gtk_widget_unref(win->container);
  }

  gtk_object_unref (GTK_OBJECT(win->tips));

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
  helpWindow(NULL, NULL, HH_GLOBPREFS);
}


void
gnc_show_options_dialog(void)
{
  static GNCOptionWin *options_dialog = NULL;
  GNCOptionDB *global_options;
  
  global_options = gnc_get_global_options();
  
  if (gnc_option_db_num_sections(global_options) == 0)
  {
    gnc_warning_dialog("No options!");
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
    options_dialog = gnc_options_dialog_new(TRUE);
    
    /*
      gnome_dialog_close_hides(GNOME_DIALOG(options_dialog), TRUE);
      gnome_dialog_set_parent(GNOME_DIALOG(options_dialog),
      GTK_WINDOW (gnc_get_ui_data()));
    */
    
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
  }
  
  gtk_widget_show(options_dialog->container);
  gdk_window_raise(options_dialog->container->window);
}
