/********************************************************************\
 * dialog-options.c -- GNOME option handling                        *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
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
\********************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <glib/gi18n.h>
#include "swig-runtime.h"

#include "gnc-tree-model-budget.h" //FIXME?
#include "gnc-budget.h"

#include "dialog-options.h"
#include "dialog-utils.h"
#include "engine-helpers.h"
#include "glib-helpers.h"
#include "gnc-account-sel.h"
#include "gnc-tree-view-account.h"
#include "gnc-commodity-edit.h"
#include "gnc-component-manager.h"
#include "gnc-general-select.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-gconf-utils.h"
#include "gnc-gui-query.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "guile-util.h"
#include "option-util.h"
#include "guile-mappings.h"
#include "gnc-date-format.h"
#include "misc-gnome-utils.h"

#define FUNC_NAME G_STRFUNC
/* TODO: clean up "register-stocks" junk
 */


/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define DIALOG_OPTIONS_CM_CLASS "dialog-options"

/*
 * Point where preferences switch control method from a set of
 * notebook tabs to a list.
 */
#define MAX_TAB_COUNT 4

/* A pointer to the last selected filename */
#define LAST_SELECTION "last-selection"

/* A Hash-table of GNCOptionDef_t keyed with option names. */
static GHashTable *optionTable = NULL;

struct gnc_option_win
{
  GtkWidget  * dialog;
  GtkWidget  * notebook;
  GtkWidget  * page_list;

  gboolean toplevel;

  GtkTooltips * tips;

  GNCOptionWinCallback apply_cb;
  gpointer             apply_cb_data;
  
  GNCOptionWinCallback help_cb;
  gpointer             help_cb_data;

  GNCOptionWinCallback close_cb;
  gpointer             close_cb_data;

  /* Hold onto this for a complete reset */
  GNCOptionDB *		option_db;
};

typedef enum {
  GNC_RD_WID_AB_BUTTON_POS = 0,
  GNC_RD_WID_AB_WIDGET_POS,
  GNC_RD_WID_REL_BUTTON_POS,
  GNC_RD_WID_REL_WIDGET_POS} GNCRdPositions;


static GNCOptionWinCallback global_help_cb = NULL;
gpointer global_help_cb_data = NULL;

void gnc_options_dialog_response_cb(GtkDialog *dialog, gint response,
				    GNCOptionWin *window);
static void gnc_options_dialog_reset_cb(GtkWidget * w, gpointer data);
void gnc_options_dialog_list_select_cb(GtkWidget * list, GtkWidget * item,
				       gpointer data);


static inline gint
color_d_to_i16 (double d)
{
  return (d * 0xFFFF);
}

static inline double
color_i16_to_d (gint i16)
{
  return ((double)i16 / 0xFFFF);
}

static void
gnc_options_dialog_changed_internal (GtkWidget *widget, gboolean sensitive)
{
  GtkDialog *dialog;

  while (widget && !GTK_IS_DIALOG(widget))
    widget = gtk_widget_get_parent(widget);
  if (widget == NULL)
    return;

  dialog = GTK_DIALOG(widget);
  gtk_dialog_set_response_sensitive (dialog, GTK_RESPONSE_OK, sensitive);
  gtk_dialog_set_response_sensitive (dialog, GTK_RESPONSE_APPLY, sensitive);
}

void
gnc_options_dialog_changed (GNCOptionWin *win)
{
  if (!win) return;

  gnc_options_dialog_changed_internal (win->dialog, TRUE);
}

void 
gnc_option_changed_widget_cb(GtkWidget *widget, GNCOption *option)
{
  gnc_option_set_changed (option, TRUE);
  gnc_option_call_option_widget_changed_proc(option);
  gnc_options_dialog_changed_internal (widget, TRUE);
}

void 
gnc_option_changed_option_cb(GtkWidget *dummy, GNCOption *option)
{
  GtkWidget *widget;

  widget = gnc_option_get_widget (option);
  gnc_option_changed_widget_cb(widget, option);
}

static void
gnc_date_option_set_select_method(GNCOption *option, gboolean use_absolute,
                                  gboolean set_buttons)
{
  GList* widget_list;
  GtkWidget *ab_button, *rel_button, *rel_widget, *ab_widget;
  GtkWidget *widget;

  widget = gnc_option_get_widget (option);

  widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
  ab_button = g_list_nth_data(widget_list, GNC_RD_WID_AB_BUTTON_POS);
  ab_widget = g_list_nth_data(widget_list, GNC_RD_WID_AB_WIDGET_POS);
  rel_button = g_list_nth_data(widget_list, GNC_RD_WID_REL_BUTTON_POS);
  rel_widget = g_list_nth_data(widget_list, GNC_RD_WID_REL_WIDGET_POS);
  g_list_free(widget_list);

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
  gnc_option_changed_option_cb(widget, option);
}

static void
gnc_rd_option_rel_set_cb(GtkWidget *widget, gpointer *raw_option)
{
  GNCOption *option = (GNCOption *) raw_option;
  gnc_date_option_set_select_method(option, FALSE, FALSE);
  gnc_option_changed_option_cb(widget, option);
  return;
}

static void
gnc_image_option_update_preview_cb (GtkFileChooser *chooser,
				    GNCOption *option)
{
  gchar *filename;
  GtkImage *image;
  GdkPixbuf *pixbuf;
  gboolean have_preview;

  g_return_if_fail(chooser != NULL);

  ENTER("chooser %p, option %p", chooser, option);
  filename = gtk_file_chooser_get_preview_filename(chooser);
  DEBUG("chooser preview name is %s.", filename ? filename : "(null)");
  if (filename == NULL) {
    filename = g_strdup(g_object_get_data(G_OBJECT(chooser), LAST_SELECTION));
    DEBUG("using last selection of %s", filename ? filename : "(null)");
    if (filename == NULL) {
      LEAVE("no usable name");
      return;
    }
  }

  image = GTK_IMAGE(gtk_file_chooser_get_preview_widget(chooser));
  pixbuf = gdk_pixbuf_new_from_file_at_size(filename, 128, 128, NULL);
  g_free(filename);
  have_preview = (pixbuf != NULL);

  gtk_image_set_from_pixbuf(image, pixbuf);
  if (pixbuf)
    g_object_unref(pixbuf);

  gtk_file_chooser_set_preview_widget_active(chooser, have_preview);
  LEAVE("preview visible is %d", have_preview);
}

static void
gnc_image_option_selection_changed_cb (GtkFileChooser *chooser,
				       GNCOption *option)
{
  gchar *filename;

  filename = gtk_file_chooser_get_preview_filename(chooser);
  if (!filename)
    return;
  g_object_set_data_full(G_OBJECT(chooser), LAST_SELECTION, filename, g_free);
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

  value = scm_call_0(getter);

  option_def = gnc_options_ui_get_option (type);
  if (option_def && option_def->set_value)
  {
    bad_value = option_def->set_value (option, use_default, widget, value);
    if (bad_value)
    {
      PERR("bad value\n");
    }
  }
  else
  {
    PERR("Unknown type. Ignoring.\n");
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
gnc_option_default_cb(GtkWidget *widget, GNCOption *option)
{
  gnc_option_set_ui_value (option, TRUE);
  gnc_option_set_changed (option, TRUE);
  gnc_options_dialog_changed_internal (widget, TRUE);
}

static void
gnc_option_show_hidden_toggled_cb(GtkWidget *widget, GNCOption* option)
{
	AccountViewInfo avi;
    GncTreeViewAccount *tree_view;

    tree_view = GNC_TREE_VIEW_ACCOUNT(gnc_option_get_widget (option));
    gnc_tree_view_account_get_view_info (tree_view, &avi);
	avi.show_hidden = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
    gnc_tree_view_account_set_view_info (tree_view, &avi);
    gnc_option_changed_widget_cb(widget, option);
}

#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
static void
gnc_option_multichoice_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  gnc_option_changed_widget_cb(widget, option);
}
#else
static void
gnc_option_multichoice_cb(GtkWidget *w, gint index, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *widget;
  GtkWidget *omenu;
  gpointer _current;
  gint current;
  char *type;

  widget = gnc_option_get_widget (option);

  /* the option menu may be part of a date option widget, so we need to
     decomposit the enclosing hbox then */
  type = gnc_option_type (option);
  if (safe_strcmp (type, "date") == 0) {
    char *date_type = gnc_option_date_option_get_subtype(option);
    GList *children;

    if (safe_strcmp (date_type, "both") == 0) {
      children = gtk_container_get_children (GTK_CONTAINER (widget));
      widget = g_list_nth_data (children, GNC_RD_WID_REL_WIDGET_POS);
      g_list_free(children);
    }
    free (date_type);
  }
  free (type);

  _current = g_object_get_data(G_OBJECT(widget), "gnc_multichoice_index");
  current = GPOINTER_TO_INT(_current);

  if (current == index)
    return;

  gtk_option_menu_set_history(GTK_OPTION_MENU(widget), index);
  g_object_set_data(G_OBJECT(widget), "gnc_multichoice_index",
                    GINT_TO_POINTER(index));

  gnc_option_set_changed (option, TRUE);

  gnc_option_call_option_widget_changed_proc(option);

  omenu = g_object_get_data(G_OBJECT(w), "gnc_option_menu");
  gnc_options_dialog_changed_internal (omenu, TRUE);
}
#endif

static void
gnc_option_radiobutton_cb(GtkWidget *w, gpointer data)
{
  GNCOption *option = data;
  GtkWidget *widget;
  gpointer _current, _new_value;
  gint current, new_value;

  widget = gnc_option_get_widget (option);

  _current = g_object_get_data(G_OBJECT(widget), "gnc_radiobutton_index");
  current = GPOINTER_TO_INT (_current);

  _new_value = g_object_get_data (G_OBJECT(w), "gnc_radiobutton_index");
  new_value = GPOINTER_TO_INT (_new_value);

  if (current == new_value)
    return;

  g_object_set_data (G_OBJECT(widget), "gnc_radiobutton_index",
		     GINT_TO_POINTER(new_value));
  gnc_option_changed_widget_cb(widget, option);
}

static GtkWidget *
gnc_option_create_date_widget (GNCOption *option)
{
  GtkWidget * box = NULL;
  GtkWidget *rel_button= NULL, *ab_button=NULL;
  GtkWidget *rel_widget=NULL, *ab_widget=NULL;
  GtkWidget *entry;
  gboolean show_time, use24;
  char *type;
  int num_values;

  type = gnc_option_date_option_get_subtype(option);
  show_time = gnc_option_show_time(option);
  use24 = gnc_gconf_get_bool(GCONF_GENERAL, "24hour_time", FALSE);

  if (safe_strcmp(type, "relative") != 0)
  {
    ab_widget = gnc_date_edit_new(time(NULL), show_time, use24);
    entry = GNC_DATE_EDIT(ab_widget)->date_entry;
    g_signal_connect(G_OBJECT(entry), "changed",
		     G_CALLBACK(gnc_option_changed_option_cb), option);
    if (show_time)
    {
      entry = GNC_DATE_EDIT(ab_widget)->time_entry;
      g_signal_connect(G_OBJECT(entry), "changed",
		       G_CALLBACK(gnc_option_changed_option_cb), option);
    }
  }
    
  if (safe_strcmp(type, "absolute") != 0)
  { 
    int i;
    num_values = gnc_option_num_permissible_values(option);
    
    g_return_val_if_fail(num_values >= 0, NULL);
    
#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
    {
      /* New code for GtkComboBox. Is not used because it is missing
       * the feature of per-item tooltips. Not yet implemented in gtk,
       * see http://bugzilla.gnome.org/show_bug.cgi?id=303717 , see
       * also gnc_option_create_multichoice_widget() below. */
      char *string;
      rel_widget = gtk_combo_box_new_text();
      for (i = 0; i < num_values; i++)
      {
	string = gnc_option_permissible_value_name(option, i);
	gtk_combo_box_append_text(GTK_COMBO_BOX(rel_widget), string);
	g_free(string);
      }

      g_signal_connect(G_OBJECT(rel_widget), "changed",
		       G_CALLBACK(gnc_option_multichoice_cb), option);
    }
#else
    {
      /* Old 1-8-branch code for a GtkOptionMenu. We use this one
       * because it has a per-item tooltip which the GtkComboBox still
       * doesn't have. - cstim, 2006-02-25 */
      GNCOptionInfo *info;
      char **raw_strings;
      char **raw;
      
      info = g_new0(GNCOptionInfo, num_values);
      raw_strings = g_new0(char *, num_values * 2);
      raw = raw_strings;
    
      for (i = 0; i < num_values; i++)
      {
	*raw = gnc_option_permissible_value_name(option, i);
	info[i].name = *raw; /* (*raw && **raw) ? _(*raw) : ""; */
      
	raw++;

	*raw = gnc_option_permissible_value_description(option, i);
	info[i].tip = *raw; /* (*raw && **raw) ? _(*raw) : ""; */

	if(safe_strcmp(type, "both") == 0)
	{
	  info[i].callback = gnc_option_multichoice_cb; /* gnc_option_rd_combo_cb */
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
	if (raw_strings[i])
	  free(raw_strings[i]);
  
      g_free(raw_strings);
      g_free(info);
    }
#endif
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
    g_signal_connect(G_OBJECT(ab_button), "toggled",
		     G_CALLBACK(gnc_rd_option_ab_set_cb), option);

    rel_button = gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(ab_button));
    g_signal_connect(G_OBJECT(rel_button), "toggled",
		     G_CALLBACK(gnc_rd_option_rel_set_cb), option);

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
gnc_option_create_budget_widget(GNCOption *option)
{
    GtkTreeModel *tm;
    GtkComboBox *cb;
    GtkCellRenderer *cr;

    tm = gnc_tree_model_budget_new(gnc_get_current_book());
    cb = GTK_COMBO_BOX(gtk_combo_box_new_with_model(tm));
    g_object_unref(tm);
    cr = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(cb), cr, TRUE);

    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(cb), cr, "text",
                                   BUDGET_NAME_COLUMN, NULL);
    return GTK_WIDGET(cb);
}

static GtkWidget *
gnc_option_create_multichoice_widget(GNCOption *option, GtkTooltips *tooltips)
{
  GtkWidget *widget;
  int num_values;
  int i;

  num_values = gnc_option_num_permissible_values(option);

  g_return_val_if_fail(num_values >= 0, NULL);

#ifndef GTKCOMBOBOX_TOOLTIPS_WORK
  {
    /* Old 1-8-branch code for a GtkOptionMenu. We use this one
       because it has a per-item tooltip which the GtkComboBox still
       doesn't have. - cstim, 2006-02-25 */
    GNCOptionInfo *info;
    char **raw_strings;
    char **raw;

    info = g_new0(GNCOptionInfo, num_values);
    raw_strings = g_new0(char *, num_values * 2);
    raw = raw_strings;

    for (i = 0; i < num_values; i++)
    {
      *raw = gnc_option_permissible_value_name(option, i);
      info[i].name = (*raw && **raw) ? _(*raw) : "";
      
      raw++;

      *raw = gnc_option_permissible_value_description(option, i);
      info[i].tip = (*raw && **raw) ? _(*raw) : "";

      info[i].callback = gnc_option_multichoice_cb;
      info[i].user_data = option;
      raw++;
    }

    widget = gnc_build_option_menu(info, num_values);

    for (i = 0; i < num_values * 2; i++)
      if (raw_strings[i])
	free(raw_strings[i]);
  
    g_free(raw_strings);
    g_free(info);
  }
#else
  {
    
    /* New code for GtkComboBox. Is still unused because it is missing
       the feature of per-item tooltips. Not yet implemented in gtk,
       see http://bugzilla.gnome.org/show_bug.cgi?id=303717 */
    char *itemstring;
    /* char *description; */
    widget = gtk_combo_box_new_text();
    for (i = 0; i < num_values; i++) {
      itemstring = gnc_option_permissible_value_name(option, i);
      /* description = gnc_option_permissible_value_description(option, i); */
      gtk_combo_box_append_text(GTK_COMBO_BOX(widget),
				(itemstring && *itemstring) ? _(itemstring) : "");
      /*, (description && *description) ? _(description) : "" */
      /* Maybe the per-item tooltip will simply be added as such an
	 additional argument as shown above, but we'll see. */
      if (itemstring)
	g_free(itemstring);
      /* if (description) g_free(description); */
    }
    g_signal_connect(G_OBJECT(widget), "changed",
		     G_CALLBACK(gnc_option_multichoice_cb), option);
  }
#endif

  return widget;
}

static void
radiobutton_destroy_cb (GtkObject *obj, gpointer data)
{
  GtkTooltips *tips = data;

  g_object_unref (tips);
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
  box = gtk_hbox_new (FALSE, 5);
  gtk_container_add (GTK_CONTAINER (frame), box);

  /* Create the tooltips */
  tooltips = gtk_tooltips_new ();
#ifdef HAVE_GTK_2_10
  g_object_ref_sink(tooltips);
#else
  g_object_ref (tooltips);
  gtk_object_sink (GTK_OBJECT (tooltips));
#endif

  /* Iterate over the options and create a radio button for each one */
  for (i = 0; i < num_values; i++)
  {
    label = gnc_option_permissible_value_name(option, i);
    tip = gnc_option_permissible_value_description(option, i);

    widget =
      gtk_radio_button_new_with_label_from_widget (widget ?
						   GTK_RADIO_BUTTON (widget) :
						   NULL,
						   label && *label ? _(label) : "");
    g_object_set_data (G_OBJECT (widget), "gnc_radiobutton_index",
		       GINT_TO_POINTER (i));
    gtk_tooltips_set_tip(tooltips, widget, tip && *tip ? _(tip) : "", NULL);
    g_signal_connect(G_OBJECT(widget), "toggled",
		     G_CALLBACK(gnc_option_radiobutton_cb), option);
    gtk_box_pack_start (GTK_BOX (box), widget, FALSE, FALSE, 0);

    if (label)
      free (label);
    if (tip)
      free (tip);
  }

  g_signal_connect (G_OBJECT (frame), "destroy",
		    G_CALLBACK (radiobutton_destroy_cb), tooltips);

  return frame;
}


static void
gnc_option_account_cb(GtkTreeSelection *selection, gpointer data)
{
  GNCOption *option = data;
  GtkTreeView *tree_view;

  tree_view = gtk_tree_selection_get_tree_view(selection);

  gnc_option_changed_widget_cb(GTK_WIDGET(tree_view), option);
}

static void
gnc_option_account_select_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GncTreeViewAccount *tree_view;
  GtkTreeSelection *selection;

  tree_view = GNC_TREE_VIEW_ACCOUNT(gnc_option_get_widget (option));
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
  gtk_tree_selection_select_all(selection);
  gnc_option_changed_widget_cb(widget, option);
}

static void
gnc_option_account_clear_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GncTreeViewAccount *tree_view;
  GtkTreeSelection *selection;

  tree_view = GNC_TREE_VIEW_ACCOUNT(gnc_option_get_widget (option));
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
  gtk_tree_selection_unselect_all(selection);
  gnc_option_changed_widget_cb(widget, option);
}


static GtkWidget *
gnc_option_create_account_widget(GNCOption *option, char *name, GtkTooltips *tooltips)
{
  gboolean multiple_selection;
  GtkWidget *scroll_win;
  GtkWidget *button;
  GtkWidget *frame;
  GtkWidget *tree;
  GtkWidget *vbox;
  GtkWidget *bbox;
  GList *acct_type_list;
  GtkTreeSelection *selection;

  multiple_selection = gnc_option_multiple_selection(option);
  acct_type_list = gnc_option_get_account_type_list(option);

  frame = gtk_frame_new(name);

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(frame), vbox);

  tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(tree), FALSE);
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
  if (multiple_selection)
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);
  else
    gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

  if (acct_type_list) {
    GList *node;
    AccountViewInfo avi;
    int i;

    gnc_tree_view_account_get_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);

    for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
      avi.include_type[i] = FALSE;
    avi.show_hidden = FALSE;

    for (node = acct_type_list; node; node = node->next) {
      GNCAccountType type = GPOINTER_TO_INT (node->data);
      avi.include_type[type] = TRUE;
    }

    gnc_tree_view_account_set_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);
    g_list_free (acct_type_list);    
  } else {
    AccountViewInfo avi;
    int i;

    gnc_tree_view_account_get_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);

    for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
      avi.include_type[i] = TRUE;
	avi.show_hidden = FALSE;
    gnc_tree_view_account_set_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);
  }

  scroll_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                 GTK_POLICY_AUTOMATIC, 
                                 GTK_POLICY_AUTOMATIC);

  gtk_box_pack_start(GTK_BOX(vbox), scroll_win, TRUE, TRUE, 0);
  gtk_container_set_border_width(GTK_CONTAINER(scroll_win), 5);
  gtk_container_add(GTK_CONTAINER(scroll_win), tree);

  bbox = gtk_hbutton_box_new();
  gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
  gtk_box_pack_start(GTK_BOX(vbox), bbox, FALSE, FALSE, 10);

  if (multiple_selection)
  {
    button = gtk_button_new_with_label(_("Select All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_tooltips_set_tip(tooltips, button, _("Select all accounts."), NULL);

    g_signal_connect(G_OBJECT(button), "clicked",
		     G_CALLBACK(gnc_option_account_select_all_cb), option);

    button = gtk_button_new_with_label(_("Clear All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_tooltips_set_tip(tooltips, button, _("Clear the selection and unselect all accounts."), NULL);

    g_signal_connect(G_OBJECT(button), "clicked",
		     G_CALLBACK(gnc_option_account_clear_all_cb), option);
  }

  button = gtk_button_new_with_label(_("Select Default"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
  gtk_tooltips_set_tip(tooltips, button, _("Select the default account selection."), NULL);

  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(gnc_option_default_cb), option);

  button = gtk_check_button_new_with_label(_("Show Hidden"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
  gtk_tooltips_set_tip(tooltips, button, _("Show hidden accounts."), NULL);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), FALSE);
  g_signal_connect(G_OBJECT(button), "toggled",
		   G_CALLBACK(gnc_option_show_hidden_toggled_cb), option);

  gnc_option_set_widget (option, tree);

  return frame;
}

static void
gnc_option_list_changed_cb(GtkTreeSelection *selection,
			   GNCOption *option)
{
  GtkTreeView *view;

  view = gtk_tree_selection_get_tree_view(selection);
  gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
gnc_option_list_select_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkTreeView *view;
  GtkTreeSelection *selection;

  view = GTK_TREE_VIEW(gnc_option_get_widget (option));
  selection = gtk_tree_view_get_selection(view);
  gtk_tree_selection_select_all(selection);
  gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
gnc_option_list_clear_all_cb(GtkWidget *widget, gpointer data)
{
  GNCOption *option = data;
  GtkTreeView *view;
  GtkTreeSelection *selection;

  view = GTK_TREE_VIEW(gnc_option_get_widget (option));
  selection = gtk_tree_view_get_selection(view);
  gtk_tree_selection_unselect_all(selection);
  gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static GtkWidget *
gnc_option_create_list_widget(GNCOption *option, char *name, GtkTooltips *tooltips)
{
  GtkListStore *store;
  GtkTreeView *view;
  GtkTreeIter iter;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GtkTreeSelection *selection;

  GtkWidget *button;
  GtkWidget *frame;
  GtkWidget *hbox;
  GtkWidget *bbox;
  gint num_values;
  gint i;

  frame = gtk_frame_new(name);
  hbox = gtk_hbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(frame), hbox);

  store = gtk_list_store_new(1, G_TYPE_STRING);
  view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
  g_object_unref(store);
  renderer = gtk_cell_renderer_text_new();
  column = gtk_tree_view_column_new_with_attributes("", renderer,
						    "text", 0,
						    NULL);
  gtk_tree_view_append_column(view, column);
  gtk_tree_view_set_headers_visible(view, FALSE);

  num_values = gnc_option_num_permissible_values(option);
  for (i = 0; i < num_values; i++)
  {
    gchar *raw_string, *string;

    raw_string = gnc_option_permissible_value_name(option, i);
    string = (raw_string && *raw_string) ? _(raw_string) : "";
    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter,
		       0, string ? string : "",
		       -1);
    g_free(raw_string);
  }

  gtk_box_pack_start(GTK_BOX(hbox), GTK_WIDGET(view), FALSE, FALSE, 0);

  selection = gtk_tree_view_get_selection(view);
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
  g_signal_connect(selection, "changed",
		   G_CALLBACK(gnc_option_list_changed_cb), option);


  bbox = gtk_vbutton_box_new();
  gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
  gtk_box_pack_start(GTK_BOX(hbox), bbox, FALSE, FALSE, 10);

  button = gtk_button_new_with_label(_("Select All"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
  gtk_tooltips_set_tip(tooltips, button, _("Select all entries."), NULL);

  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(gnc_option_list_select_all_cb), option);

  button = gtk_button_new_with_label(_("Clear All"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
  gtk_tooltips_set_tip(tooltips, button, _("Clear the selection and unselect all entries."), NULL);

  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(gnc_option_list_clear_all_cb), option);

  button = gtk_button_new_with_label(_("Select Default"));
  gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
  gtk_tooltips_set_tip(tooltips, button, _("Select the default selection."), NULL);

  g_signal_connect(G_OBJECT(button), "clicked",
		   G_CALLBACK(gnc_option_default_cb), option);

  gnc_option_set_widget (option, GTK_WIDGET(view));

  return frame;
}

static void
gnc_option_color_changed_cb(GtkColorButton *color_button, GNCOption *option)
{
  gnc_option_changed_widget_cb(GTK_WIDGET(color_button), option);
}

static void
gnc_option_font_changed_cb(GtkFontButton *font_button, GNCOption *option)
{
  gnc_option_changed_widget_cb(GTK_WIDGET(font_button), option);
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

  ENTER("option %p(%s), box %p, tips %p",
	option, gnc_option_name(option), page_box, tooltips);
  type = gnc_option_type(option);
  if (type == NULL) {
    LEAVE("bad type");
    return;
  }

  raw_name = gnc_option_name(option);
  if (raw_name && *raw_name)
    name = _(raw_name);
  else
    name = NULL;

  raw_documentation = gnc_option_documentation(option);
  if (raw_documentation && *raw_documentation)
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
    PERR("Unknown option type. Ignoring option \"%s\".\n", name);
  }

  if (!packed && (enclosing != NULL)) {
    /* Pack option widget into an extra eventbox because otherwise the
       "documentation" tooltip is not displayed. */
    GtkWidget *eventbox = gtk_event_box_new();

    gtk_container_add (GTK_CONTAINER (eventbox), enclosing);
    gtk_box_pack_start (page_box, eventbox, FALSE, FALSE, 0);

    gtk_tooltips_set_tip (tooltips, eventbox, documentation, NULL);
  }

  if (value != NULL)
    gtk_tooltips_set_tip(tooltips, value, documentation, NULL);

  if (raw_name != NULL)
    free(raw_name);
  if (raw_documentation != NULL)
    free(raw_documentation);
  free(type);
  LEAVE(" ");
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
                               GNCOptionSection *section,
			       GtkTooltips *tooltips)
{
  GNCOption *option;
  GtkWidget *page_label;
  GtkWidget *options_box;
  GtkWidget *page_content_box;
  GtkWidget* notebook_page;
  GtkWidget *reset_button;
  GtkWidget *listitem = NULL;
  GtkWidget *buttonbox;
  gint num_options;
  const char *name;
  gint i, page_count, name_offset;
  gboolean advanced;

  name = gnc_option_section_name(section);
  if (!name)
    return -1;

  if (strncmp(name, "__", 2) == 0)
    return -1;
  advanced = (strncmp(name, "_+", 2) == 0);
  name_offset = (advanced) ? 2 : 0;
  page_label = gtk_label_new(_(name + name_offset));
  gtk_widget_show(page_label);

  /* Build this options page */
  page_content_box = gtk_vbox_new(FALSE, 2);
  gtk_container_set_border_width(GTK_CONTAINER(page_content_box), 12);

  /* Build space for the content - the options box */
  options_box = gtk_vbox_new(FALSE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(options_box), 0);
  gtk_box_pack_start(GTK_BOX(page_content_box), options_box, TRUE, TRUE, 0);

  /* Create all the options */
  num_options = gnc_option_section_num_options(section);
  for (i = 0; i < num_options; i++)
  {
    option = gnc_get_option_section_option(section, i);
    gnc_options_dialog_add_option(options_box, option,
                                  propertybox->tips);
  }

  /* Add a button box at the bottom of the page */
  buttonbox = gtk_hbutton_box_new();
  gtk_button_box_set_layout (GTK_BUTTON_BOX (buttonbox),
			     GTK_BUTTONBOX_EDGE);
  gtk_container_set_border_width(GTK_CONTAINER (buttonbox), 5);
  gtk_box_pack_end(GTK_BOX(page_content_box), buttonbox, FALSE, FALSE, 0);

  /* The reset button on each option page */
  reset_button = gtk_button_new_with_label (_("Defaults"));
  gtk_tooltips_set_tip(tooltips, reset_button, 
		       _("Reset all values to their defaults."), NULL);

  g_signal_connect(G_OBJECT(reset_button), "clicked",
		   G_CALLBACK(gnc_options_dialog_reset_cb), propertybox);
  g_object_set_data(G_OBJECT(reset_button), "section", section);
  gtk_box_pack_end(GTK_BOX(buttonbox), reset_button, FALSE, FALSE, 0);
  gtk_widget_show_all(page_content_box);
  gtk_notebook_append_page(GTK_NOTEBOOK(propertybox->notebook), 
                           page_content_box, page_label);

  /* Switch to selection from a list if the page count threshhold is reached */
  page_count = gtk_notebook_page_num(GTK_NOTEBOOK(propertybox->notebook),
				     page_content_box);

  if (propertybox->page_list) {
    /* Build the matching list item for selecting from large page sets */
    listitem = gtk_list_item_new_with_label(_(name + name_offset));
    gtk_widget_show(listitem);
    gtk_container_add(GTK_CONTAINER(propertybox->page_list), listitem);

    if (page_count > MAX_TAB_COUNT - 1) { /* Convert 1-based -> 0-based */
      gtk_widget_show(propertybox->page_list);
      gtk_notebook_set_show_tabs(GTK_NOTEBOOK(propertybox->notebook), FALSE);
      gtk_notebook_set_show_border(GTK_NOTEBOOK(propertybox->notebook), FALSE);
    }

    /* Tweak "advanced" pages for later handling. */
    if (advanced) {
      notebook_page =
	gtk_notebook_get_nth_page(GTK_NOTEBOOK(propertybox->notebook),
				  page_count);

      g_object_set_data(G_OBJECT(notebook_page), "listitem", listitem);
      g_object_set_data(G_OBJECT(notebook_page), "advanced",
			GINT_TO_POINTER(advanced));
    }
  }

  return(page_count);
}


/********************************************************************\
 * gnc_options_dialog_build_contents                                *
 *   builds an options dialog given a property box and an options   *
 *   database                                                       *
 *                                                                  *
 * Args: propertybox - gnome property box to use                    *
 *       odb         - option database to use                       *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_options_dialog_build_contents(GNCOptionWin *propertybox,
                                  GNCOptionDB  *odb)
{
  GNCOptionSection *section;
  gchar *default_section_name;
  gint default_page = -1;
  gint num_sections;
  gint page;
  gint i;
  guint j;

  g_return_if_fail (propertybox != NULL);
  g_return_if_fail (odb != NULL);

  gnc_option_db_set_ui_callbacks (odb,
                                  gnc_option_get_ui_value_internal,
                                  gnc_option_set_ui_value_internal,
                                  gnc_option_set_selectable_internal);

  propertybox->tips = gtk_tooltips_new();
  propertybox->option_db = odb;

#ifdef HAVE_GTK_2_10
  g_object_ref_sink(propertybox->tips);
#else
  g_object_ref (propertybox->tips);
  gtk_object_sink (GTK_OBJECT (propertybox->tips));
#endif

  num_sections = gnc_option_db_num_sections(odb);
  default_section_name = gnc_option_db_get_default_section(odb);

  for (i = 0; i < num_sections; i++)
  {
    const char *section_name;

    section = gnc_option_db_get_section(odb, i);
    page = gnc_options_dialog_append_page(propertybox, section, propertybox->tips);

    section_name = gnc_option_section_name(section);
    if (safe_strcmp(section_name, default_section_name) == 0)
      default_page = page;
  }

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

  gtk_notebook_popup_enable(GTK_NOTEBOOK(propertybox->notebook));
  if (default_page >= 0) {
    gtk_notebook_set_current_page(GTK_NOTEBOOK(propertybox->notebook), default_page);
    gtk_list_select_item(GTK_LIST(propertybox->page_list), default_page);
  } else {
    /* GTKList doesn't default to selecting the first item. */
    gtk_list_select_item(GTK_LIST(propertybox->page_list), 0);
  }
  gnc_options_dialog_changed_internal(propertybox->dialog, FALSE);
  gtk_widget_show(propertybox->dialog);
}


GtkWidget *
gnc_options_dialog_widget(GNCOptionWin * win)
{
  return win->dialog;
}

GtkWidget *
gnc_options_dialog_notebook(GNCOptionWin * win)
{
  return win->notebook;
}

void
gnc_options_dialog_response_cb(GtkDialog *dialog, gint response, GNCOptionWin *window)
{
  GNCOptionWinCallback close_cb;

  switch (response) {
   case GTK_RESPONSE_HELP:
    if(window->help_cb)
      (window->help_cb)(window, window->help_cb_data);
    break;

   case GTK_RESPONSE_OK:
   case GTK_RESPONSE_APPLY:
    gnc_options_dialog_changed_internal (window->dialog, FALSE);
    close_cb = window->close_cb;
    window->close_cb = NULL;
    if (window->apply_cb)
      window->apply_cb (window, window->apply_cb_data);
    window->close_cb = close_cb;
    if (response == GTK_RESPONSE_APPLY)
      break;
    /* fall through */

   default:
    if (window->close_cb) {
      (window->close_cb)(window, window->close_cb_data);
    } else {
      gtk_widget_hide(window->dialog);
    }
  }
}

static void
gnc_options_dialog_reset_cb(GtkWidget * w, gpointer data)
{
  GNCOptionWin *win = data;
  GNCOptionSection *section;
  gpointer val;

  val = g_object_get_data(G_OBJECT(w), "section");
  g_return_if_fail (val);
  g_return_if_fail (win);

  section = (GNCOptionSection*)val;
  gnc_option_db_section_reset_widgets (section);
  gnc_options_dialog_changed_internal (win->dialog, TRUE);
}

void
gnc_options_dialog_list_select_cb(GtkWidget * list, GtkWidget * item,
				  gpointer data)
{
  GNCOptionWin * win = data;
  gint index;

  g_return_if_fail (list);
  g_return_if_fail (win);

  index = gtk_list_child_position(GTK_LIST(list), item);
  gtk_notebook_set_current_page(GTK_NOTEBOOK(win->notebook), index);
}

void
gnc_options_register_stocks (void)
{
#if 0
	static gboolean done = FALSE;
	
	GtkStockItem items[] = {
		{ GTK_STOCK_APPLY		,"gnc_option_apply_button",	0, 0, NULL },
		{ GTK_STOCK_HELP		,"gnc_options_dialog_help",	0, 0, NULL },
		{ GTK_STOCK_OK			,"gnc_options_dialog_ok",	0, 0, NULL },
		{ GTK_STOCK_CANCEL		,"gnc_options_dialog_cancel",	0, 0, NULL },
	};

	if (done) 
	{
		return;
	}
	done = TRUE;

	gtk_stock_add (items, G_N_ELEMENTS (items));
#endif
}

static void
component_close_handler (gpointer data)
{
  GNCOptionWin *window = data;
  gtk_dialog_response(GTK_DIALOG(window->dialog), GTK_RESPONSE_CANCEL);
}

/* gnc_options_dialog_new:
 *
 *   - Opens the preferences glade file
 *   - Connects signals specified in the glade file
 *   - Sets the window's title
 *   - Initializes a new GtkNotebook, and adds it to the window
 *
 */
GNCOptionWin *
gnc_options_dialog_new(gchar *title)
{
  GNCOptionWin * retval;
  GladeXML *xml;
  GtkWidget * hbox;
  gint component_id;

  retval = g_new0(GNCOptionWin, 1);
  xml = gnc_glade_xml_new ("preferences.glade", "GnuCash Options");
  retval->dialog = glade_xml_get_widget (xml, "GnuCash Options");
  retval->page_list = glade_xml_get_widget (xml, "page_list");

  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     retval );

  if (title)
    gtk_window_set_title(GTK_WINDOW(retval->dialog), title);

  /* glade doesn't suport a notebook with zero pages */
  hbox = glade_xml_get_widget (xml, "notebook placeholder");
  retval->notebook = gtk_notebook_new();
  gtk_widget_show(retval->notebook);
  gtk_box_pack_start(GTK_BOX(hbox), retval->notebook, TRUE, TRUE, 5);

  component_id = gnc_register_gui_component (DIALOG_OPTIONS_CM_CLASS,
                                             NULL, component_close_handler,
                                             retval);
  gnc_gui_component_set_session (component_id, gnc_get_current_session());

  return retval;
}

/* Creates a new GNCOptionWin structure, but assumes you have your own
   dialog widget you want to plugin */
GNCOptionWin *
gnc_options_dialog_new_w_dialog(gchar *title, GtkWidget *dialog)
{
  GNCOptionWin * retval;

  retval = g_new0(GNCOptionWin, 1);
  retval->dialog = dialog;
  return retval;
}

void 
gnc_options_dialog_set_apply_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                                gpointer data)
{
  win->apply_cb = cb;
  win->apply_cb_data = data;
}

void 
gnc_options_dialog_set_help_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                               gpointer data)
{
  win->help_cb = cb;
  win->help_cb_data = data;
}

void 
gnc_options_dialog_set_close_cb(GNCOptionWin * win, GNCOptionWinCallback cb,
                                gpointer data)
{
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

/* This is for global program preferences. */
void
gnc_options_dialog_destroy(GNCOptionWin * win)
{
  if (!win) return;

  gnc_unregister_gui_component_by_data(DIALOG_OPTIONS_CM_CLASS, win);

  gtk_widget_destroy(win->dialog);

  if(win->tips) {
    g_object_unref (win->tips);
  }

  win->dialog = NULL;
  win->notebook = NULL;
  win->apply_cb = NULL;
  win->help_cb = NULL;
  win->tips = NULL;

  g_free(win);
}


/*****************************************************************/
/* Option Registration                                           */

/*************************
 *       SET WIDGET      *
 *************************
 *
 * gnc_option_set_ui_widget_<type>():
 *
 * You should create the widget representation for the option type,
 * and set the top-level container widget for your control in
 * *enclosing.  If you want to pack the widget into the page yourself,
 * then you may -- just set *packed to TRUE.  Otherwise, the widget
 * you return in *enclosing will be packed for you.  (*packed is
 * initialized to FALSE, so if you're not setting it to TRUE, you
 * don't have to touch it at all.)
 *
 * If you need to initialize the state of your control or to connect
 * any signals to you widgets, then you should do so in this function.
 * If you want to create a label for the widget you should use 'name'
 * for the label text.
 *
 * Somewhere in this function, you should also call
 * gnc_option_set_widget(option, value); where 'value' is the
 * GtkWidget you will actually store the value in.
 *
 * Also call gnc_option_set_ui_value(option, FALSE);
 *
 * You probably want to end with something like:
 *   gtk_widget_show_all(*enclosing);
 *
 * If you can can detect state changes for your widget's value, you should also
 * gnc_option_changed_widget_cb() upon changes.
 *
 * The widget you return from this function should be the widget in
 * which you're storing the option value.
 */
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

  g_signal_connect(G_OBJECT(value), "toggled",
		   G_CALLBACK(gnc_option_changed_widget_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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

  g_signal_connect(G_OBJECT(value), "changed",
		   G_CALLBACK(gnc_option_changed_widget_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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
  GtkTextBuffer* text_buffer;

  frame = gtk_frame_new(name);

  scroll = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
				 GTK_POLICY_NEVER, 
				 GTK_POLICY_AUTOMATIC);
  gtk_container_set_border_width(GTK_CONTAINER(scroll), 2);

  gtk_container_add(GTK_CONTAINER(frame), scroll);

  *enclosing = gtk_hbox_new(FALSE, 10);
  value = gtk_text_view_new();
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(value), GTK_WRAP_WORD);
  gtk_text_view_set_editable(GTK_TEXT_VIEW(value), TRUE);
  gtk_container_add (GTK_CONTAINER (scroll), value);

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(value));
  g_signal_connect(G_OBJECT(text_buffer), "changed",
		   G_CALLBACK(gnc_option_changed_option_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), frame, TRUE, TRUE, 0);
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

  g_signal_connect(G_OBJECT(value), "changed",
		   G_CALLBACK(gnc_option_changed_widget_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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

  g_signal_connect(G_OBJECT(GNC_GENERAL_SELECT(value)->entry), "changed",
		   G_CALLBACK(gnc_option_changed_widget_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);

  value = gnc_option_create_multichoice_widget(option, tooltips);
  gnc_option_set_widget (option, value);

  gnc_option_set_ui_value(option, FALSE);
  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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
  GtkWidget *eventbox;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);

  value = gnc_option_create_date_widget(option);

  gnc_option_set_widget (option, value);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);

  /* Pack option widget into an extra eventbox because otherwise the
     "documentation" tooltip is not displayed. */
  eventbox = gtk_event_box_new();
  gtk_container_add (GTK_CONTAINER (eventbox), *enclosing);
  gtk_box_pack_start(page_box, eventbox, FALSE, FALSE, 5);
  *packed = TRUE;  

  gtk_tooltips_set_tip (tooltips, eventbox, documentation, NULL);

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
  GtkTreeSelection *selection;

  *enclosing = gnc_option_create_account_widget(option, name, tooltips);
  value = gnc_option_get_widget (option);

  gtk_tooltips_set_tip(tooltips, *enclosing, documentation, NULL);

  gtk_box_pack_start(page_box, *enclosing, TRUE, TRUE, 5);
  *packed = TRUE;

  //gtk_widget_realize(value);

  gnc_option_set_ui_value(option, FALSE);

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(value));
  g_signal_connect(G_OBJECT(selection), "changed",
		   G_CALLBACK(gnc_option_account_cb), option);

  //  gtk_clist_set_row_height(GTK_CLIST(value), 0);
  //  gtk_widget_set_size_request(value, -1, GTK_CLIST(value)->row_height * 10);
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_account_sel (GNCOption *option, GtkBox *page_box,
				  GtkTooltips *tooltips,
				  char *name, char *documentation,
				  /* Return values */
				  GtkWidget **enclosing, gboolean *packed)
{
  GtkWidget *value;
  GtkWidget *label;
  GList *acct_type_list;
  gchar *colon_name;

  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  acct_type_list = gnc_option_get_account_type_list(option);
  value = gnc_account_sel_new();
  gnc_account_sel_set_acct_filters(GNC_ACCOUNT_SEL(value), acct_type_list, NULL);

  g_signal_connect(value, "account_sel_changed",
		   G_CALLBACK(gnc_option_changed_widget_cb), option);

  gnc_option_set_widget (option, value);
  /* DOCUMENT ME: Why is the only option type that sets use_default to
     TRUE? */
  gnc_option_set_ui_value(option, TRUE);

  *enclosing = gtk_hbox_new(FALSE, 5);
  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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
  GtkWidget *eventbox;

  *enclosing = gnc_option_create_list_widget(option, name, tooltips);
  value = gnc_option_get_widget (option);

  /* Pack option widget into an extra eventbox because otherwise the
     "documentation" tooltip is not displayed. */
  eventbox = gtk_event_box_new();
  gtk_container_add (GTK_CONTAINER (eventbox), *enclosing);
  gtk_box_pack_start(page_box, eventbox, FALSE, FALSE, 5);
  *packed = TRUE;

  gtk_tooltips_set_tip(tooltips, eventbox, documentation, NULL);

  gnc_option_set_ui_value(option, FALSE);
  gtk_widget_show(*enclosing);
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

    num_digits += num_decimals;

    gtk_entry_set_width_chars(GTK_ENTRY(value), num_digits);
  }

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  g_signal_connect(G_OBJECT(value), "changed",
		   G_CALLBACK(gnc_option_changed_widget_cb), option);
  
  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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

  value = gtk_color_button_new();
  gtk_color_button_set_title(GTK_COLOR_BUTTON(value), name);
  gtk_color_button_set_use_alpha(GTK_COLOR_BUTTON(value), use_alpha);

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  g_signal_connect(G_OBJECT(value), "color-set",
		   G_CALLBACK(gnc_option_color_changed_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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
  value = gtk_font_button_new();
  g_object_set(G_OBJECT(value),
	       "use-font", TRUE,
	       "show-style", TRUE,
	       "show-size", TRUE,
	       (char *)NULL);

  gnc_option_set_widget (option, value);

  gnc_option_set_ui_value(option, FALSE);

  g_signal_connect(G_OBJECT(value), "font-set",
		   G_CALLBACK(gnc_option_font_changed_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
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
  GtkWidget *button;
  gchar *colon_name;

  ENTER("option %p(%s), name %s", option, gnc_option_name(option), name);
  colon_name = g_strconcat(name, ":", NULL);
  label = gtk_label_new(colon_name);
  gtk_misc_set_alignment(GTK_MISC(label), 1.0, 0.5);
  g_free(colon_name);

  *enclosing = gtk_hbox_new(FALSE, 5);

  button = gtk_button_new_with_label(_("Clear"));
  gtk_tooltips_set_tip(tooltips, button, _("Clear any selected image file."), NULL);

  value = gtk_file_chooser_button_new(_("Select image"),
				      GTK_FILE_CHOOSER_ACTION_OPEN);
  gtk_tooltips_set_tip(tooltips, value, _("Select an image file."), NULL);
  g_object_set(G_OBJECT(value),
	       "width-chars", 30,
	       "preview-widget", gtk_image_new(),
	       (char *)NULL);
  g_signal_connect(G_OBJECT (value), "selection-changed",
		   G_CALLBACK(gnc_option_changed_widget_cb), option);
  g_signal_connect(G_OBJECT (value), "selection-changed",
		   G_CALLBACK(gnc_image_option_selection_changed_cb), option);
  g_signal_connect(G_OBJECT (value), "update-preview",
		   G_CALLBACK(gnc_image_option_update_preview_cb), option);
  g_signal_connect_swapped(G_OBJECT (button), "clicked",
		   G_CALLBACK(gtk_file_chooser_unselect_all), value);

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing), button, FALSE, FALSE, 0);
  gtk_box_pack_end(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);

  gtk_widget_show(value);
  gtk_widget_show(label);
  gtk_widget_show(*enclosing);
  LEAVE("new widget = %p", value);
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
  gtk_widget_show_all(*enclosing);
  return value;
}

static GtkWidget *
gnc_option_set_ui_widget_dateformat (GNCOption *option, GtkBox *page_box,
				     GtkTooltips *tooltips,
				     char *name, char *documentation,
				     /* Return values */
				     GtkWidget **enclosing, gboolean *packed)
{
  *enclosing = gnc_date_format_new_with_label(name);
  gnc_option_set_widget (option, *enclosing);

  gnc_option_set_ui_value(option, FALSE);
  g_signal_connect(G_OBJECT(*enclosing), "format_changed",
		   G_CALLBACK(gnc_option_changed_option_cb), option);
  gtk_widget_show_all(*enclosing);
  return *enclosing;
}

static GtkWidget *
gnc_option_set_ui_widget_budget (GNCOption *option, GtkBox *page_box,
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

  value = gnc_option_create_budget_widget(option);

  gnc_option_set_widget (option, value);
  gnc_option_set_ui_value(option, FALSE);

  /* Maybe connect destroy handler for tree model here? */
  g_signal_connect(G_OBJECT(value), "changed",
		   G_CALLBACK(gnc_option_changed_widget_cb), option);

  gtk_box_pack_start(GTK_BOX(*enclosing), label, FALSE, FALSE, 0);
  gtk_box_pack_start(GTK_BOX(*enclosing), value, FALSE, FALSE, 0);
  gtk_widget_show_all(*enclosing);
  return value;
}

/*************************
 *       SET VALUE       *
 *************************
 *
 * gnc_option_set_ui_value_<type>():
 *
 *   In this function you should set the state of the gui widget to
 * correspond to the value provided in 'value'.  You should return
 * TRUE if there was an error, FALSE otherwise.
 *
 *
 */

static gboolean
gnc_option_set_ui_value_boolean (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  if (SCM_BOOLP(value))
  {
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget),
				 SCM_NFALSEP(value));
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_string (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  if (SCM_STRINGP(value))
  {
    const gchar *string = SCM_STRING_CHARS(value);
    gtk_entry_set_text(GTK_ENTRY(widget), string);
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_text (GNCOption *option, gboolean use_default,
			      GObject *object, SCM value)
{
  GtkTextBuffer *buffer;

  if (GTK_IS_TEXT_BUFFER(object))
    buffer = GTK_TEXT_BUFFER(object);
  else
    buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(object));

  if (SCM_STRINGP(value))
  {
    const gchar *string = SCM_STRING_CHARS(value);
    gtk_text_buffer_set_text (buffer, string, strlen (string));
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
#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
    gtk_combo_box_set_active(GTK_COMBO_BOX(widget), index);
#else
    gtk_option_menu_set_history(GTK_OPTION_MENU(widget), index);
    g_object_set_data(G_OBJECT(widget), "gnc_multichoice_index",
                      GINT_TO_POINTER(index));
#endif 	 
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

  if (SCM_CONSP(value))
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
#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
	  gtk_combo_box_set_active(GTK_COMBO_BOX(widget), index);
#else
	  g_object_set_data(G_OBJECT(widget),
			    "gnc_multichoice_index",
			    GINT_TO_POINTER(index));
	  gtk_option_menu_set_history(GTK_OPTION_MENU(widget), index);
#endif
	}
	else if (safe_strcmp(date_option_type, "both") == 0)
	{
	  GList *widget_list;
	  GtkWidget *rel_date_widget;

	  widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
	  rel_date_widget = g_list_nth_data(widget_list,
					    GNC_RD_WID_REL_WIDGET_POS);
	  g_list_free(widget_list);
	  gnc_date_option_set_select_method(option, FALSE, TRUE);
#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
	  gtk_combo_box_set_active(GTK_COMBO_BOX(rel_date_widget), index);
#else
          g_object_set_data(G_OBJECT(rel_date_widget),
			    "gnc_multichoice_index",
			    GINT_TO_POINTER(index));
	  gtk_option_menu_set_history(GTK_OPTION_MENU(rel_date_widget),
				      index);
#endif
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

	  widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
	  ab_widget = g_list_nth_data(widget_list,
				      GNC_RD_WID_AB_WIDGET_POS);
	  g_list_free(widget_list);
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

  gnc_tree_view_account_set_selected_accounts (GNC_TREE_VIEW_ACCOUNT(widget),
					       list, TRUE);

  g_list_free(list);
  return FALSE;
}

static gboolean
gnc_option_set_ui_value_account_sel (GNCOption *option, gboolean use_default,
				     GtkWidget *widget, SCM value)
{
  Account *acc = NULL;

  if (value != SCM_BOOL_F) {
    if (!SWIG_IsPointer(value))
        scm_misc_error("gnc_option_set_ui_value_account_sel",
                       "Option Value not a wcp.", value);
      
    acc = SWIG_MustGetPtr(value, SWIG_TypeQuery("_p_Account"), 4, 0);
  }
	
  //doesn't default because this function is called to set a specific account
  gnc_account_sel_set_account (GNC_ACCOUNT_SEL(widget), acc, FALSE);

  return FALSE;
}

static gboolean
gnc_option_set_ui_value_list (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  GtkTreeSelection *selection;
  GtkTreePath *path;
  gint row;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
  gtk_tree_selection_unselect_all(selection);

  while (SCM_LISTP(value) && !SCM_NULLP(value))
  {
    SCM item;

    item = SCM_CAR(value);
    value = SCM_CDR(value);

    row = gnc_option_permissible_value_index(option, item);
    if (row < 0)
    {
      return TRUE;
    }

    path = gtk_tree_path_new_from_indices(row, -1);
    gtk_tree_selection_select_path(selection, path);
    gtk_tree_path_free(path);
  }

  if (!SCM_LISTP(value) || !SCM_NULLP(value))
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

  if (SCM_NUMBERP(value))
  {
    d_value = scm_num2dbl(value, G_STRFUNC);
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
    GtkColorButton *color_button;
    GdkColor color;

    DEBUG("red %f, green %f, blue %f, alpha %f", red, green, blue, alpha);
    color_button = GTK_COLOR_BUTTON(widget);

    color.red   = color_d_to_i16(red);
    color.green = color_d_to_i16(green);
    color.blue  = color_d_to_i16(blue);
    gtk_color_button_set_color(color_button, &color);
    gtk_color_button_set_alpha(color_button, color_d_to_i16(alpha));
    return FALSE;
  }

  LEAVE("TRUE");
  return TRUE;
}

static gboolean
gnc_option_set_ui_value_font (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  if (SCM_STRINGP(value))
  {
    const gchar *string = SCM_STRING_CHARS(value);
    if ((string != NULL) && (*string != '\0'))
    {
      GtkFontButton *font_button = GTK_FONT_BUTTON(widget);
      gtk_font_button_set_font_name(font_button, string);
    }
    return FALSE;
  }
  else
    return TRUE;
}

static gboolean
gnc_option_set_ui_value_pixmap (GNCOption *option, gboolean use_default,
				 GtkWidget *widget, SCM value)
{
  ENTER("option %p(%s)", option, gnc_option_name(option));
  if (SCM_STRINGP(value))
  {
    const gchar *string = SCM_STRING_CHARS(value);

    if (string && *string)
    {
      gchar *test;
      DEBUG("string = %s", string);
      gtk_file_chooser_select_filename(GTK_FILE_CHOOSER(widget), string);
      test = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(widget));
      g_object_set_data_full(G_OBJECT(widget), LAST_SELECTION,
			     g_strdup(string), g_free);
      DEBUG("Set %s, retrieved %s", string, test ? test : "(null)");
      gnc_image_option_update_preview_cb(GTK_FILE_CHOOSER(widget), option);
    }
    LEAVE("FALSE");
    return FALSE;
  }

  LEAVE("TRUE");
  return TRUE;
}

static gboolean gnc_option_set_ui_value_budget(
    GNCOption *option, gboolean use_default, GtkWidget *widget, SCM value)
{
    GncBudget *bgt;
    GtkComboBox *cb;
    GtkTreeModel *tm;
    GtkTreeIter iter;

//    if (!SCM_NULLP(value)) {
    if (value != SCM_BOOL_F) {
        if (!SWIG_IsPointer(value))
            scm_misc_error("gnc_option_set_ui_value_budget",
                           "Option Value not a wcp.", value);

        bgt = SWIG_MustGetPtr(value, SWIG_TypeQuery("GncBudget *"), 4, 0);
        cb = GTK_COMBO_BOX(widget);
        tm = gtk_combo_box_get_model(cb);
        if (gnc_tree_model_budget_get_iter_for_budget(tm, &iter, bgt))
	  gtk_combo_box_set_active_iter(cb, &iter);
    }


    //FIXME: Unimplemented.
    return FALSE;
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

    list = gtk_container_get_children (GTK_CONTAINER (widget));
    box = list->data;
    g_list_free(list);

    list = gtk_container_get_children (GTK_CONTAINER (box));
    for (i = 0; i < index && list; i++)
      list = list->next;
    g_return_val_if_fail (list, TRUE);

    button = list->data;
    g_list_free(list);
    val = g_object_get_data (G_OBJECT (button), "gnc_radiobutton_index");
    g_return_val_if_fail (GPOINTER_TO_INT (val) == index, TRUE);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
    //    g_object_set_data(G_OBJECT(widget), "gnc_radiobutton_index",
    //			GINT_TO_POINTER(index));
    return FALSE;
  }
}

static gboolean
gnc_option_set_ui_value_dateformat (GNCOption *option, gboolean use_default,
				    GtkWidget *widget, SCM value)
{
  GNCDateFormat * gdf = GNC_DATE_FORMAT(widget);
  QofDateFormat format;
  GNCDateMonthFormat months;
  gboolean years;
  char *custom;

  if (gnc_dateformat_option_value_parse(value, &format, &months, &years, &custom))
    return TRUE;

  gnc_date_format_set_format(gdf, format);
  gnc_date_format_set_months(gdf, months);
  gnc_date_format_set_years(gdf, years);
  gnc_date_format_set_custom(gdf, custom);
  gnc_date_format_refresh(gdf);

  if (custom)
    free(custom);

  return FALSE;
}

/*************************
 *       GET VALUE       *
 *************************
 *
 * gnc_option_get_ui_value_<type>():
 *
 * 'widget' will be the widget returned from the
 * gnc_option_set_ui_widget_<type>() function.
 *
 * You should return a SCM value corresponding to the current state of the
 * gui widget.
 *
 */

static SCM
gnc_option_get_ui_value_boolean (GNCOption *option, GtkWidget *widget)
{
  gboolean active;

  active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
  return SCM_BOOL(active);
}

static SCM
gnc_option_get_ui_value_string (GNCOption *option, GtkWidget *widget)
{
  char * string;
  SCM result;

  string = gtk_editable_get_chars(GTK_EDITABLE(widget), 0, -1);
  result = scm_makfrom0str(string);
  g_free(string);
  return result;
}

static SCM
gnc_option_get_ui_value_text (GNCOption *option, GtkWidget *widget)
{
  char * string;
  SCM result;

  string = xxxgtk_textview_get_text (GTK_TEXT_VIEW(widget));
  result = scm_makfrom0str(string);
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
  int index;

#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
  index = gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
#else
  {
    gpointer _index = g_object_get_data(G_OBJECT(widget), "gnc_multichoice_index");
    index = GPOINTER_TO_INT(_index);
  }
#endif 	 
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
#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
    index = gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
#else
    index = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(widget),
                                              "gnc_multichoice_index"));
#endif
    type = scm_str2symbol("relative");
    val = gnc_option_permissible_value(option, index);
    result = scm_cons(type, val);
  }
  else if (safe_strcmp(subtype, "absolute") == 0)
  { 		      
    Timespec ts;

    ts.tv_sec  = gnc_date_edit_get_date(GNC_DATE_EDIT(widget));
    ts.tv_nsec = 0;

    result = scm_cons(scm_str2symbol("absolute"), gnc_timespec2timepair(ts));
  }
  else if (safe_strcmp(subtype, "both") == 0)
  {
    Timespec ts;
    int index;
    SCM val;
    GList *widget_list;
    GtkWidget *ab_button, *rel_widget, *ab_widget;

    widget_list = gtk_container_get_children(GTK_CONTAINER(widget));
    ab_button = g_list_nth_data(widget_list,  GNC_RD_WID_AB_BUTTON_POS);
    ab_widget = g_list_nth_data(widget_list,  GNC_RD_WID_AB_WIDGET_POS);
    rel_widget = g_list_nth_data(widget_list, GNC_RD_WID_REL_WIDGET_POS);
    g_list_free(widget_list);

    /* if it's an absolute date */
    if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(ab_button)))
    {
      ts.tv_sec = gnc_date_edit_get_date(GNC_DATE_EDIT(ab_widget));
      ts.tv_nsec = 0;
      result = scm_cons(scm_str2symbol("absolute"), gnc_timespec2timepair(ts));
    }
    else 
    {
#ifdef GTKCOMBOBOX_TOOLTIPS_WORK
      index = gtk_combo_box_get_active(GTK_COMBO_BOX(rel_widget));
#else
      index = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(rel_widget),
                                                "gnc_multichoice_index"));
#endif
      val = gnc_option_permissible_value(option, index);
      result = scm_cons(scm_str2symbol("relative"), val);
    }
  }
  g_free(subtype);
  return result;
}

static SCM
gnc_option_get_ui_value_account_list (GNCOption *option, GtkWidget *widget)
{
  GncTreeViewAccount *tree;
  GList *list;
  SCM result;

  tree = GNC_TREE_VIEW_ACCOUNT(widget);
  list = gnc_tree_view_account_get_selected_accounts (tree);

  /* handover list */
  result = gnc_glist_to_scm_list(list, "_p_Account");
  g_list_free(list);
  return result;
}

static SCM
gnc_option_get_ui_value_account_sel (GNCOption *option, GtkWidget *widget)
{
  GNCAccountSel *gas;
  Account* acc;

  gas = GNC_ACCOUNT_SEL(widget);
  acc = gnc_account_sel_get_account (gas);

  if (!acc)
    return SCM_BOOL_F;

  return SWIG_NewPointerObj(acc, SWIG_TypeQuery("_p_Account"), 0);
}

static SCM
gnc_option_get_ui_value_budget(GNCOption *option, GtkWidget *widget)
{
    GncBudget *bgt;
    GtkComboBox *cb;
    GtkTreeModel *tm;
    GtkTreeIter iter;
    gboolean success;

    cb = GTK_COMBO_BOX(widget);
    success = gtk_combo_box_get_active_iter(cb, &iter);
    tm = gtk_combo_box_get_model(cb);
    bgt = gnc_tree_model_budget_get_budget(tm, &iter);

    if (!bgt)
        return SCM_BOOL_F;

    return SWIG_NewPointerObj(bgt, SWIG_TypeQuery("_p_budget_s"), 0);
}

static SCM
gnc_option_get_ui_value_list (GNCOption *option, GtkWidget *widget)
{
  GtkTreeSelection *selection;
  GtkTreePath *path;
  SCM result;
  gboolean selected;
  gint num_rows;
  gint row;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(widget));
  num_rows = gnc_option_num_permissible_values(option);
  result = scm_c_eval_string("'()");

  for (row = 0; row < num_rows; row++)
  {
   path = gtk_tree_path_new_from_indices(row, -1);
   selected = gtk_tree_selection_path_is_selected(selection, path);
   gtk_tree_path_free(path);
   if (selected)
      result = scm_cons(gnc_option_permissible_value(option, row), result);
  }

  return (scm_reverse(result));
}

static SCM
gnc_option_get_ui_value_number_range (GNCOption *option, GtkWidget *widget)
{
  GtkSpinButton *spinner;
  gdouble value;

  spinner = GTK_SPIN_BUTTON(widget);

  value = gtk_spin_button_get_value(spinner);

  return (scm_make_real(value));
}

static SCM
gnc_option_get_ui_value_color (GNCOption *option, GtkWidget *widget)
{
  SCM result;
  GtkColorButton *color_button;
  GdkColor color;
  gdouble red, green, blue, alpha;
  gdouble scale;

  ENTER("option %p(%s), widget %p",
	option, gnc_option_name(option), widget);

  color_button = GTK_COLOR_BUTTON(widget);
  gtk_color_button_get_color(color_button, &color);
  red   = color_i16_to_d(color.red);
  green = color_i16_to_d(color.green);
  blue  = color_i16_to_d(color.blue);
  alpha = color_i16_to_d(gtk_color_button_get_alpha(color_button));

  scale = gnc_option_color_range(option);

  result = SCM_EOL;
  result = scm_cons(scm_make_real(alpha * scale), result);
  result = scm_cons(scm_make_real(blue * scale), result);
  result = scm_cons(scm_make_real(green * scale), result);
  result = scm_cons(scm_make_real(red * scale), result);
  return result;
}

static SCM
gnc_option_get_ui_value_font (GNCOption *option, GtkWidget *widget)
{
  GtkFontButton *font_button = GTK_FONT_BUTTON(widget);
  const gchar * string;

  string = gtk_font_button_get_font_name(font_button);
  return (scm_makfrom0str(string));
}

static SCM
gnc_option_get_ui_value_pixmap (GNCOption *option, GtkWidget *widget)
{
  gchar *string;
  SCM result;

  string = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(widget));
  DEBUG("filename %s", string ? string : "(null)");
  result = scm_makfrom0str(string ? string : "");
  g_free(string);
  return result;
}

static SCM
gnc_option_get_ui_value_radiobutton (GNCOption *option, GtkWidget *widget)
{
  gpointer _index;
  int index;

  _index = g_object_get_data(G_OBJECT(widget), "gnc_radiobutton_index");
  index = GPOINTER_TO_INT(_index);

  return (gnc_option_permissible_value(option, index));
}

static SCM
gnc_option_get_ui_value_dateformat (GNCOption *option, GtkWidget *widget)
{
  GNCDateFormat *gdf = GNC_DATE_FORMAT(widget);
  QofDateFormat format;
  GNCDateMonthFormat months;
  gboolean years;
  const char* custom;

  format = gnc_date_format_get_format(gdf);
  months = gnc_date_format_get_months(gdf);
  years = gnc_date_format_get_years(gdf);
  custom = gnc_date_format_get_custom(gdf);

  return (gnc_dateformat_option_set_value(format, months, years, custom));
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
      (GNCOptionUISetValue)gnc_option_set_ui_value_text,
      gnc_option_get_ui_value_text },
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
    { "account-sel", gnc_option_set_ui_widget_account_sel,
      gnc_option_set_ui_value_account_sel, gnc_option_get_ui_value_account_sel },
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
    { "dateformat", gnc_option_set_ui_widget_dateformat,
      gnc_option_set_ui_value_dateformat, gnc_option_get_ui_value_dateformat },
    { "budget", gnc_option_set_ui_widget_budget,
      gnc_option_set_ui_value_budget, gnc_option_get_ui_value_budget },
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

  /* FIXME: should protect against repeat insertion. */
  g_hash_table_insert (optionTable, (gpointer)(option->option_name), option);
}

GNCOptionDef_t * gnc_options_ui_get_option (const char *option_name)
{
  GNCOptionDef_t *retval;
  g_return_val_if_fail (optionTable, NULL);
  g_return_val_if_fail (option_name, NULL);

  retval = g_hash_table_lookup (optionTable, option_name);
  if (!retval) {
      PERR("Option lookup for type '%s' failed!", option_name);
  }
  return retval;
}

void gnc_options_ui_initialize (void)
{
  SWIG_GetModule(NULL); /* Work-around for SWIG bug. */
  //  gnc_options_register_stocks ();
  g_return_if_fail (optionTable == NULL);
  optionTable = g_hash_table_new (g_str_hash, g_str_equal);

  /* add known types */
  gnc_options_initialize_options ();
}

struct scm_cb
{
  SCM	apply_cb;
  SCM	close_cb;
};

static void
scm_apply_cb (GNCOptionWin *win, gpointer data)
{
  struct scm_cb *cbdata = data;

  if (gnc_option_db_get_changed (win->option_db)) {
    gnc_option_db_commit (win->option_db);
    if (cbdata->apply_cb != SCM_BOOL_F) {
      scm_call_0 (cbdata->apply_cb);
    }
  }
}

static void
scm_close_cb (GNCOptionWin *win, gpointer data)
{
  struct scm_cb *cbdata = data;

  if (cbdata->close_cb != SCM_BOOL_F) {
    scm_call_0 (cbdata->close_cb);
    scm_gc_unprotect_object (cbdata->close_cb);
  }

  if (cbdata->apply_cb != SCM_BOOL_F)
    scm_gc_unprotect_object (cbdata->apply_cb);

  g_free (cbdata);
}

/* Both apply_cb and close_cb should be scheme functions with 0 arguments.
 * References to these functions will be held until the close_cb is called
 */
void
gnc_options_dialog_set_scm_callbacks (GNCOptionWin *win, SCM apply_cb,
				      SCM close_cb)
{
  struct scm_cb *cbdata;

  cbdata = g_new0 (struct scm_cb, 1);
  cbdata->apply_cb = apply_cb;
  cbdata->close_cb = close_cb;

  if (apply_cb != SCM_BOOL_F)
    scm_gc_protect_object (cbdata->apply_cb);

  if (close_cb != SCM_BOOL_F)
    scm_gc_protect_object (cbdata->close_cb);

  gnc_options_dialog_set_apply_cb (win, scm_apply_cb, cbdata);
  gnc_options_dialog_set_close_cb (win, scm_close_cb, cbdata);
}
