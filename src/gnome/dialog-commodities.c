/********************************************************************\
 * dialog-commodities.c -- commodities dialog                       *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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
#include "eggtreemodelfilter.h"

#include "dialog-commodity.h"
#include "dialog-utils.h"
#include "gnc-commodity.h"
#include "gnc-component-manager.h"
#include "gnc-engine-util.h"
#include "gnc-tree-model-commodity.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "messages.h"
#include "global-options.h"


#define DIALOG_COMMODITIES_CM_CLASS "dialog-commodities"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

typedef struct
{
  GtkWidget * dialog;

  GtkWidget * commodity_list;
  GtkWidget * edit_button;
  GtkWidget * remove_button;
  gboolean    show_currencies;

  gboolean new;
} CommoditiesDialog;


static gint last_width = 0;
static gint last_height = 0;

void gnc_commodities_dialog_response (GtkDialog *dialog,
				      gint response,
				      CommoditiesDialog *cd);
void gnc_commodities_window_destroy_cb (GtkObject *object, CommoditiesDialog *cd);
void gnc_commodities_edit_clicked (GtkWidget *widget, CommoditiesDialog *cd);
void gnc_commodities_remove_clicked (GtkWidget *widget, CommoditiesDialog *cd);
void gnc_commodities_add_clicked (GtkWidget *widget, CommoditiesDialog *cd);



static gnc_commodity *
gnc_commodities_dialog_get_selected (CommoditiesDialog *cd)
{
	GtkTreeSelection *selection;
	GtkTreeModel *sort_model, *filter_model, *model;
	GtkTreeIter sort_iter, filter_iter, iter;
	
	g_return_val_if_fail (cd != NULL, NULL);
	g_return_val_if_fail (cd->commodity_list != NULL, NULL);

	selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (cd->commodity_list));

	if (!gtk_tree_selection_get_selected (selection, &sort_model, &sort_iter)) {
		return NULL;
	}

	filter_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (sort_model));
	gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (sort_model), 
							&filter_iter, &sort_iter);

	model = egg_tree_model_filter_get_model (EGG_TREE_MODEL_FILTER (filter_model));
	egg_tree_model_filter_convert_iter_to_child_iter (EGG_TREE_MODEL_FILTER (filter_model),
							  &iter, &filter_iter);

	return gnc_tree_model_commodity_get_commodity (GNC_TREE_MODEL_COMMODITY (model), &iter);
}

void
gnc_commodities_window_destroy_cb (GtkObject *object,   CommoditiesDialog *cd)
{
  gnc_unregister_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);

  g_free (cd);
}

static void
edit_clicked (CommoditiesDialog *cd)
{
	gnc_commodity *commodity;

	commodity = gnc_commodities_dialog_get_selected (cd);

	if (commodity == NULL)
		return;

	if (gnc_ui_edit_commodity_modal (commodity, cd->dialog))
		gnc_gui_refresh_all ();
}

static void
remove_clicked (CommoditiesDialog *cd)
{
  QofBook *book;
  GNCPriceDB *pdb;
  GList *node;
  GList *prices;
  GList *accounts;
  gboolean do_delete;
  gboolean can_delete;
  gnc_commodity *commodity;
  
  commodity = gnc_commodities_dialog_get_selected (cd);

  if (commodity == NULL)
    return;

  accounts = xaccGroupGetSubAccounts (gnc_get_current_group ());
  can_delete = TRUE;
  do_delete = FALSE;

  for (node = accounts; node; node = node->next)
  {
    Account *account = node->data;

    if (commodity == xaccAccountGetCommodity (account))
    {
      can_delete = FALSE;
      break;
    }
  }

  /* FIXME check for transaction references */

  if (!can_delete)
  {
    const char *message = _("That commodity is currently used by\n"
                            "at least one of your accounts. You may\n"
                            "not delete it.");

    gnc_warning_dialog (cd->dialog, message);
    g_list_free (accounts);
    return;
  }

  book = xaccGroupGetBook (xaccAccountGetRoot (accounts->data));
  pdb = gnc_pricedb_get_db (book);
  prices = gnc_pricedb_get_prices(pdb, commodity, NULL);
  if (prices)
  {
    const char *message = _("This commodity has price quotes. Are\n"
			    "you sure you want to delete the selected\n"
                            "commodity and its price quotes?");

    do_delete = gnc_verify_dialog (cd->dialog, TRUE, message);
  }
  else
  {
    const char *message = _("Are you sure you want to delete the\n"
                            "selected commodity?");

    do_delete = gnc_verify_dialog (cd->dialog, TRUE, message);
  }

  if (do_delete)
  {
    GtkTreeModel *sort_model, *filter_model, *model;
    gnc_commodity_table *ct;

    ct = gnc_get_current_commodities ();
    for (node = prices; node; node = node->next)
      gnc_pricedb_remove_price(pdb, node->data);

    sort_model = gtk_tree_view_get_model (GTK_TREE_VIEW (cd->commodity_list));
    filter_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (sort_model));
    model = egg_tree_model_filter_get_model (EGG_TREE_MODEL_FILTER (filter_model));
    gnc_tree_model_commodity_remove_commodity (GNC_TREE_MODEL_COMMODITY(model), commodity);

    gnc_commodity_table_remove (ct, commodity);
    gnc_commodity_destroy (commodity);
    commodity = NULL;
  }

  gnc_price_list_destroy(prices);
  g_list_free (accounts);
  gnc_gui_refresh_all ();
}

static void
add_clicked (CommoditiesDialog *cd)
{
  GtkTreeModel *sort_model, *filter_model, *model;
  gnc_commodity *commodity;
  const char *namespace;

  commodity = gnc_commodities_dialog_get_selected (cd);

  if (commodity)
    namespace = gnc_commodity_get_namespace (commodity);
  else
    namespace = NULL;

  commodity = gnc_ui_new_commodity_modal (namespace, cd->dialog);
  if (commodity != NULL) {
    sort_model = gtk_tree_view_get_model (GTK_TREE_VIEW (cd->commodity_list));
    filter_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (sort_model));
    model = egg_tree_model_filter_get_model (EGG_TREE_MODEL_FILTER (filter_model));
    gnc_tree_model_commodity_add_commodity (GNC_TREE_MODEL_COMMODITY(model), commodity);
    gnc_gui_refresh_all ();
  }
}

void
gnc_commodities_dialog_response (GtkDialog *dialog,
				 gint response,
				 CommoditiesDialog *cd)
{
	switch (response) {
	 case GNC_RESPONSE_ADD:
	  add_clicked (cd);
	  return;

	 case GNC_RESPONSE_REMOVE:
	  remove_clicked (cd);
	  return;

	 case GNC_RESPONSE_EDIT:
	  edit_clicked (cd);
	  return;

	 case GTK_RESPONSE_CLOSE:
	 default:
	  gnc_set_boolean_option ("__gui", "commodity_include_iso", cd->show_currencies);
	  gnc_close_gui_component_by_data (DIALOG_COMMODITIES_CM_CLASS, cd);
	  return;
	}
}

static void
gnc_commodities_dialog_selection_changed (GtkTreeSelection *selection,
					  CommoditiesDialog *cd)
{
	gboolean sensitive = FALSE;
	gnc_commodity *commodity;

	commodity = gnc_commodities_dialog_get_selected (cd);

	if (commodity != NULL &&
	    safe_strcmp (gnc_commodity_get_namespace (commodity), GNC_COMMODITY_NS_ISO) != 0) {
		sensitive = TRUE;
	}

	gtk_widget_set_sensitive (cd->edit_button, sensitive);
	gtk_widget_set_sensitive (cd->remove_button, sensitive);
}

static void
show_currencies_toggled (GtkToggleButton *toggle,
			 CommoditiesDialog *cd)
{
	GtkTreeModel *sort_model, *filter_model;

	cd->show_currencies = gtk_toggle_button_get_active (toggle);

	sort_model = gtk_tree_view_get_model (GTK_TREE_VIEW (cd->commodity_list));
	filter_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (sort_model));
	egg_tree_model_filter_refilter (EGG_TREE_MODEL_FILTER (filter_model));
}

static gboolean
gnc_commodities_dialog_filter_func (GtkTreeModel *model,
				    GtkTreeIter  *iter,
				    gpointer data)
{
	CommoditiesDialog *cd = data;
	gnc_commodity *commodity;

	commodity = gnc_tree_model_commodity_get_commodity (GNC_TREE_MODEL_COMMODITY (model),
							    iter);

	return cd->show_currencies ||
	       safe_strcmp (gnc_commodity_get_namespace (commodity), GNC_COMMODITY_NS_ISO) != 0;
}

static void
gnc_commodities_dialog_create_tree_view (GtkWidget *tree_view)
{
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  int i;

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Type"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE,
						       NULL);
    gtk_tree_view_column_set_sort_column_id (column, GNC_TREE_MODEL_COMMODITY_COL_NAMESPACE);
    gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Symbol"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC,
						       NULL);
    gtk_tree_view_column_set_sort_column_id (column, GNC_TREE_MODEL_COMMODITY_COL_MNEMONIC);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
    gtk_tree_view_column_clicked (column); /* Start sorted by Type -- DRH change to remember last sort order */

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Name"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_COMMODITY_COL_FULLNAME,
						       NULL);
    gtk_tree_view_column_set_sort_column_id (column, GNC_TREE_MODEL_COMMODITY_COL_FULLNAME);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Code"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_COMMODITY_COL_EXCHANGE_CODE,
						       NULL);
    gtk_tree_view_column_set_sort_column_id (column,  GNC_TREE_MODEL_COMMODITY_COL_EXCHANGE_CODE);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Fraction"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_COMMODITY_COL_FRACTION,
						       NULL);
    gtk_tree_view_column_set_sort_column_id (column,  GNC_TREE_MODEL_COMMODITY_COL_FRACTION);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

    renderer = gtk_cell_renderer_toggle_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Q"),
		    				       renderer,
						       "active", GNC_TREE_MODEL_COMMODITY_COL_QUOTE_FLAG,
						       NULL);
    gtk_tree_view_column_set_resizable(column, FALSE);
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Source"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE,
						       NULL);
    gtk_tree_view_column_set_sort_column_id (column,  GNC_TREE_MODEL_COMMODITY_COL_QUOTE_SOURCE);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);

    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Timezone"),
		    				       renderer,
						       "text", GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ,
						       NULL);
    gtk_tree_view_column_set_sort_column_id (column,  GNC_TREE_MODEL_COMMODITY_COL_QUOTE_TZ);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
    gtk_tree_view_append_column (GTK_TREE_VIEW (tree_view), column);
    

}

static void
gnc_commodities_dialog_create (GtkWidget * parent, CommoditiesDialog *cd)
{
  GtkWidget *dialog;
  GtkWidget *button;
  GtkWidget *vbox;
  GtkWidget *list;
  GladeXML *xml;
  gnc_commodity_table *ct;
  GList *namespaces;
  GList *commodities;
  GtkTreeModel *model, *filter_model, *sort_model;
  GtkTreeSelection *selection;
  gboolean active;
 
  xml = gnc_glade_xml_new ("commodities.glade", "Commodities Dialog");
  dialog = glade_xml_get_widget (xml, "Commodities Dialog");
  
  cd->dialog = dialog;

  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, cd);

  /* parent */
  if (parent != NULL)
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

  /* buttons */
  cd->remove_button = glade_xml_get_widget (xml, "remove_button");
  cd->edit_button = glade_xml_get_widget (xml, "edit_button");

  /* commodity tree */
    
    list = glade_xml_get_widget (xml, "commodity_list");
    cd->commodity_list = list;

    ct = gnc_get_current_commodities ();

    namespaces = gnc_commodity_table_get_namespaces (ct);

    commodities = NULL;
    while (namespaces != NULL) {
	    commodities = g_list_concat (commodities, 
			    gnc_commodity_table_get_commodities (ct, (const gchar *) namespaces->data));
	    namespaces = namespaces->next;
    }
    g_list_free (namespaces);

    model = gnc_tree_model_commodity_new (commodities);
    filter_model = egg_tree_model_filter_new (model, NULL);
    egg_tree_model_filter_set_visible_func (EGG_TREE_MODEL_FILTER (filter_model),
					    gnc_commodities_dialog_filter_func,
					    cd, NULL);
    sort_model = gtk_tree_model_sort_new_with_model (filter_model);

    gtk_tree_view_set_model (GTK_TREE_VIEW (list), sort_model);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (list));
    g_signal_connect (G_OBJECT (selection), "changed",
		      G_CALLBACK (gnc_commodities_dialog_selection_changed), cd);

    gnc_commodities_dialog_create_tree_view (list);

    /* Show currency button */
    button = glade_xml_get_widget (xml, "show_currencies_button");
    active = gnc_lookup_boolean_option ("__gui", "commodity_include_iso", FALSE);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button), active);
    g_signal_connect (G_OBJECT (button), "toggled",
		      G_CALLBACK (show_currencies_toggled), cd);

  if (last_width == 0)
    gnc_get_window_size ("commodities_win", &last_width, &last_height);

  if (last_width != 0)
    gtk_window_resize (GTK_WINDOW(cd->dialog), last_width, last_height);
}

static void
close_handler (gpointer user_data)
{
  CommoditiesDialog *cd = user_data;

  gtk_window_get_size(GTK_WINDOW(cd->dialog), &last_width, &last_height);
  gnc_save_window_size("commodities_win", last_width, last_height);

  gtk_widget_destroy(cd->dialog);
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  CommoditiesDialog *cd = user_data;
  GtkTreeModel *sort_model, *filter_model, *model;

  g_return_if_fail(cd != NULL);

  sort_model = gtk_tree_view_get_model (GTK_TREE_VIEW (cd->commodity_list));
  filter_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (sort_model));
  egg_tree_model_filter_refilter (EGG_TREE_MODEL_FILTER (filter_model));
}

static gboolean
show_handler (const char *class, gint component_id,
	      gpointer user_data, gpointer iter_data)
{
  CommoditiesDialog *cd = user_data;

  if (!cd)
    return(FALSE);
  gtk_window_present (GTK_WINDOW(cd->dialog));
  return(TRUE);
}

/********************************************************************\
 * gnc_commodities_dialog                                           *
 *   opens up a window to edit price information                    *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_commodities_dialog (GtkWidget * parent)
{
  CommoditiesDialog *cd;
  gint component_id;

  if (gnc_forall_gui_components (DIALOG_COMMODITIES_CM_CLASS,
				 show_handler, NULL))
      return;

  cd = g_new0 (CommoditiesDialog, 1);

  gnc_commodities_dialog_create (parent, cd);

  component_id = gnc_register_gui_component (DIALOG_COMMODITIES_CM_CLASS,
                                             refresh_handler, close_handler,
                                             cd);

  gtk_widget_grab_focus (cd->commodity_list);

  gtk_widget_show (cd->dialog);
}
