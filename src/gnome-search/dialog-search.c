/*
 * dialog-search.c -- Search Dialog
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <gnome.h>
#include <glib.h>

#include "dialog-utils.h"
#include "window-help.h"
#include "gnc-component-manager.h"
#include "gncObject.h"
#include "QueryNew.h"

#include "dialog-search.h"
#include "search-core-type.h"
#include "search-param.h"

#define DIALOG_SEARCH_CM_CLASS "dialog-search"

struct _GNCSearchWindow {
  GtkWidget *	dialog;
  GtkWidget *	criteria_table;

  GNCIdTypeConst search_for;
  GNCSearchType	grouping;	/* Match Any, Match All */
  int		search_type;	/* New, Narrow, Add, Delete */

  QueryNew *	q;

  GNCSearchParam * last_param;
  GList *	params_list;	/* List of GNCSearchParams */
  GList *	crit_list;	/* list of crit_data */
};

struct _crit_data {
  GNCSearchParam *	param;
  GNCSearchCoreType *	element;
  GtkWidget *		elemwidget;
  GtkWidget *		container;
};

static void
match_all (GtkWidget *widget, GNCSearchWindow *sw)
{
  sw->grouping = GNC_SEARCH_MATCH_ALL;
}

static void
match_any (GtkWidget *widget, GNCSearchWindow *sw)
{
  sw->grouping = GNC_SEARCH_MATCH_ANY;
}

static void
search_type_cb (GtkToggleButton *button, GNCSearchWindow *sw)
{
  GSList * buttongroup = gtk_radio_button_group (GTK_RADIO_BUTTON (button));

  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (button))) {
    sw->search_type =
      g_slist_length (buttongroup) - g_slist_index (buttongroup, button) - 1;
  }
}

static void
search_find_cb (GtkButton *button, GNCSearchWindow *sw)
{
  /* XXX */
}

static void
search_cancel_cb (GtkButton *button, GNCSearchWindow *sw)
{
  gnc_search_dialog_destroy (sw);
}

static void
search_help_cb (GtkButton *button, GNCSearchWindow *sw)
{
  helpWindow (NULL, NULL, "");	/* XXX */
}

static void
remove_element (GtkWidget *button, GNCSearchWindow *sw)
{
  GtkWidget *element;
  struct _elem_data *data;

  if (g_list_length (sw->crit_list) < 2)
    return;

  element = gtk_object_get_data (GTK_OBJECT (button), "element");
  data = gtk_object_get_data (GTK_OBJECT (element), "data");

  /* remove the element from the list */
  sw->crit_list = g_list_remove (sw->crit_list, data);

  /* and from the display */
  gtk_container_remove (GTK_CONTAINER (sw->criteria_table), element);
  gtk_container_remove (GTK_CONTAINER (sw->criteria_table), button);
  gtk_object_destroy (GTK_OBJECT (element));
  gtk_object_destroy (GTK_OBJECT (button));
}

static void
attach_element (GtkWidget *element, GNCSearchWindow *sw, int row)
{
  GtkWidget *pixmap, *remove;

  gtk_table_attach (GTK_TABLE (sw->criteria_table), element, 0, 1, row, row+1,
		    GTK_EXPAND | GTK_FILL, 0, 0, 0);

  pixmap = gnome_stock_new_with_icon (GNOME_STOCK_PIXMAP_REMOVE);
  remove = gnome_pixmap_button (pixmap, _("Remove"));
  gtk_object_set_data (GTK_OBJECT (remove), "element", element);
  gtk_signal_connect (GTK_OBJECT (remove), "clicked", remove_element, sw);
  gtk_table_attach (GTK_TABLE (sw->criteria_table), remove, 1, 2, row, row+1,
		    0, 0, 0, 0);
  gtk_widget_show (remove);
}

static void
option_activate (GtkMenuItem *item, struct _crit_data *data)
{
  GNCSearchParam *param = gtk_object_get_data (GTK_OBJECT (item), "param");
  GNCSearchCoreType *newelem;

  if (gnc_search_param_type_match (param, data->param)) {
    /* The param type is the same, just save the new param */
    data->param = param;
    return;
  }
  data->param = param;

  /* OK, let's do a widget shuffle, throw away the old widget/element,
   * and create another one here.  No need to change the crit_list --
   * the pointer to data stays the same.
   */
  if (data->elemwidget)
    gtk_container_remove (GTK_CONTAINER (data->container), data->elemwidget);
  gtk_object_destroy (GTK_OBJECT (data->element));

  newelem = gnc_search_core_type_new_type_name
    (gnc_search_param_get_param_type (param));
  data->element = newelem;
  data->elemwidget = gnc_search_core_type_get_widget (newelem);
  if (data->elemwidget)
    gtk_box_pack_start (GTK_BOX (data->container), data->elemwidget,
			FALSE, FALSE, 0);

  /* Make sure it's visible */
  gtk_widget_show_all (data->container);
}

static GtkWidget *
get_element_widget (GNCSearchWindow *sw, GNCSearchCoreType *element)
{
  GtkWidget *menu, *item, *omenu, *hbox, *p;
  GList *l;
  struct _crit_data *data;
  int index = 0, current = 0;

  data = g_new0 (struct _crit_data, 1);
  data->element = element;

  hbox = gtk_hbox_new (FALSE, 0);
  /* only set to automaticaly clean up the memory */
  gtk_object_set_data_full (GTK_OBJECT (hbox), "data", data, g_free);

  p = gnc_search_core_type_get_widget (element);
  data->elemwidget = p;
  data->container = hbox;
  data->param = sw->last_param;

  menu = gtk_menu_new ();
  for (l = sw->params_list; l; l = l->next) {
    GNCSearchParam *param = l->data;
    item = gtk_menu_item_new_with_label (_(param->title));
    gtk_object_set_data (GTK_OBJECT (item), "param", param);
    gtk_signal_connect (GTK_OBJECT (item), "activate", option_activate, data);
    gtk_menu_append (GTK_MENU (menu), item);
    gtk_widget_show (item);

    if (param == sw->last_param) /* is this the right parameter to start? */
      current = index;

    index++;
  }

  omenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (omenu), menu);
  gtk_option_menu_set_history (GTK_OPTION_MENU (omenu), current);
  gtk_widget_show (omenu);

  gtk_box_pack_start (GTK_BOX (hbox), omenu, FALSE, FALSE, 0);
  if (p)
    gtk_box_pack_start (GTK_BOX (hbox), p, FALSE, FALSE, 0);
  gtk_widget_show_all (hbox);

  return hbox;
}

static void
gnc_search_dialog_add_criterion (GNCSearchWindow *sw)
{
  GNCSearchCoreType *new;

  /* First, make sure that the last criterion is ok */
  if (sw->crit_list) {
    struct _crit_data *data;
    GList *l;

    l = g_list_last (sw->crit_list);
    data = l->data;
    if (!gnc_search_core_type_validate (data->element))
      return;

    sw->last_param = data->param;

  } else
    sw->last_param = sw->params_list->data;

  /* create a new criterion element */

  new = gnc_search_core_type_new_type_name
    (gnc_search_param_get_param_type (sw->last_param));

  if (new) {
    struct _crit_data *data;
    GtkWidget *w;
    int rows;

    w = get_element_widget (sw, new);
    data = gtk_object_get_data (GTK_OBJECT (w), "data");
    sw->crit_list = g_list_append (sw->crit_list, data);

    rows = GTK_TABLE (sw->criteria_table)->nrows;
    gtk_table_resize (GTK_TABLE (sw->criteria_table), rows+1, 2);
    attach_element (w, sw, rows);
  }
}

static void
add_criterion (GtkWidget *button, GNCSearchWindow *sw)
{
  gnc_search_dialog_add_criterion (sw);
}

static int
gnc_search_dialog_close_cb (GnomeDialog *dialog, GNCSearchWindow *sw)
{
  gnc_unregister_gui_component_by_data (DIALOG_SEARCH_CM_CLASS, sw);

  /* XXX: Clear the params_list? */
  g_list_free (sw->crit_list);
  g_free (sw);
  return FALSE;
}

static void
close_handler (GNCSearchWindow *sw)
{
  gnome_dialog_close (GNOME_DIALOG (sw->dialog));
}

static void
gnc_search_dialog_init_widgets (GNCSearchWindow *sw)
{
  GladeXML *xml;
  GtkWidget *label, *pixmap, *add, *box;
  GtkWidget *menu, *item, *omenu;
  GtkWidget *new_rb, *narrow_rb, *add_rb, *del_rb;

  xml = gnc_glade_xml_new ("search.glade", "Search Dialog");

  /* Grab the dialog */
  sw->dialog = glade_xml_get_widget (xml, "Search Dialog");

  /* Grab the search-table widget */
  sw->criteria_table = glade_xml_get_widget (xml, "criteria_table");

  /* Set the type label */
  label = glade_xml_get_widget (xml, "type_label");
  gtk_label_set_text (GTK_LABEL (label),
		      gncObjectGetTypeLabel (sw->search_for));

  /* Set the 'add criterion' button */
  pixmap = gnome_stock_new_with_icon (GNOME_STOCK_PIXMAP_ADD);
  add = gnome_pixmap_button (pixmap, _("Add criterion"));
  gtk_signal_connect (GTK_OBJECT (add), "clicked", add_criterion, sw);
  box = glade_xml_get_widget (xml, "add_button_box");
  gtk_box_pack_start (GTK_BOX (box), add, FALSE, FALSE, 3);
  
  /* Set the match-type menu */
  menu = gtk_menu_new ();

  item = gtk_menu_item_new_with_label (_("all criteria are met"));
  gtk_signal_connect (GTK_OBJECT (item), "activate", match_all, sw);
  gtk_menu_append (GTK_MENU (menu), item);
  gtk_widget_show (item);
	
  item = gtk_menu_item_new_with_label (_("any criteria are met"));
  gtk_signal_connect (GTK_OBJECT (item), "activate", match_any, sw);
  gtk_menu_append (GTK_MENU (menu), item);
  gtk_widget_show (item);
	
  omenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu (GTK_OPTION_MENU (omenu), menu);
  gtk_option_menu_set_history (GTK_OPTION_MENU (omenu), sw->grouping);

  gtk_widget_show (omenu);
  box = glade_xml_get_widget (xml, "type_menu_box");
  gtk_box_pack_start (GTK_BOX (box), omenu, FALSE, FALSE, 3);

  /* add the first criterion */
  gnc_search_dialog_add_criterion (sw);

  /* if there's no original query, make the narrow, add, delete 
   * buttons inaccessible */
  new_rb = glade_xml_get_widget (xml, "new_search_radiobutton");
  narrow_rb = glade_xml_get_widget (xml, "narrow_search_radiobutton");
  add_rb = glade_xml_get_widget (xml, "add_search_radiobutton");
  del_rb = glade_xml_get_widget (xml, "delete_search_radiobutton");

  if(!sw->q) {
    gtk_widget_set_sensitive(GTK_WIDGET(narrow_rb), 0);
    gtk_widget_set_sensitive(GTK_WIDGET(add_rb), 0);
    gtk_widget_set_sensitive(GTK_WIDGET(del_rb), 0);
  }
  else {
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (new_rb), 0);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (narrow_rb), 1);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (add_rb), 0);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON (del_rb), 0);
  }

  /* show it all */
  gtk_widget_show_all (sw->dialog);

  /* Connect XML signals */

  glade_xml_signal_connect_data (xml, "gnc_ui_search_type_cb",
				 GTK_SIGNAL_FUNC (search_type_cb), sw);
  
  glade_xml_signal_connect_data (xml, "gnc_ui_search_find_cb",
				 GTK_SIGNAL_FUNC (search_find_cb), sw);
  
  glade_xml_signal_connect_data (xml, "gnc_ui_search_cancel_cb",
				 GTK_SIGNAL_FUNC (search_cancel_cb), sw);
  
  glade_xml_signal_connect_data (xml, "gnc_ui_search_help_cb",
				 GTK_SIGNAL_FUNC (search_help_cb), sw);
  
  /* Register ourselves */
  gnc_register_gui_component (DIALOG_SEARCH_CM_CLASS, NULL, close_handler, sw);

  /* And setup the close callback */
  gtk_signal_connect (GTK_OBJECT (sw->dialog), "close",
		      GTK_SIGNAL_FUNC (gnc_search_dialog_close_cb), sw);

}

void
gnc_search_dialog_destroy (GNCSearchWindow *sw)
{
  if (!sw) return;
  gnc_close_gui_component_by_data (DIALOG_SEARCH_CM_CLASS, sw);
}

static GNCSearchParam *
make_param (GNCIdTypeConst type, const char *title, const char *path1,
	    const char *path2)
{
  GSList *l = NULL;
  GNCSearchParam *param = gnc_search_param_new ();
  gnc_search_param_set_title (param, title);

  if (path2)
    l = g_slist_prepend (l, path2);
  l = g_slist_prepend (l, path1);

  gnc_search_param_set_param_path (param, type, l);

  return param;
}

static GList *
get_params_list (GNCIdTypeConst type)
{
  GList *list = NULL;
  GNCSearchParam *param;

  list = g_list_prepend (list, make_param (type, "Split->Txn->Void?",
					   SPLIT_TRANS, TRANS_VOID_STATUS));
  list = g_list_prepend (list, make_param (type, "Split Int64",
					   "d-share-int64", NULL));
  list = g_list_prepend (list, make_param (type, "Split Amount (double)",
					   "d-share-amount", NULL));
  list = g_list_prepend (list, make_param (type, "Split Value (debcred)",
					   SPLIT_VALUE, NULL));
  list = g_list_prepend (list, make_param (type, "Split Amount (numeric)",
					   SPLIT_AMOUNT, NULL));
  list = g_list_prepend (list, make_param (type, "Date Reconciled (date)",
					   SPLIT_DATE_RECONCILED, NULL));
  list = g_list_prepend (list, make_param (type, "Split Memo (string)",
					   SPLIT_MEMO, NULL));

  return list;
}

GNCSearchWindow *
gnc_search_dialog_create (GNCIdTypeConst obj_type)
{
  GNCSearchWindow *sw = g_new0 (GNCSearchWindow, 1);

  sw->search_for = obj_type;
  sw->params_list = get_params_list (obj_type);
  gnc_search_dialog_init_widgets (sw);

  return sw;
}

static int
on_close_cb (GnomeDialog *dialog, gpointer *data)
{
  if (data) {
    *data = "FOO";
  }

  gtk_main_quit ();
  return FALSE;
}

void
gnc_search_dialog_test (void)
{
  gpointer result = NULL;
  GNCSearchWindow *sw = gnc_search_dialog_create (GNC_ID_SPLIT);

  gtk_signal_connect (GTK_OBJECT (sw->dialog), "close",
		      GTK_SIGNAL_FUNC (on_close_cb), &result);

  gtk_main ();
}
