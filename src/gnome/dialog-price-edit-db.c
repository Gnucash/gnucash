/********************************************************************\
 * dialog-price-editor.c -- price selector dialog                   *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 * Copyright (C) 2003,2005 David Hampton                            *
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
#include <libguile.h>
#include <time.h>

#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-general-select.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-engine.h"
#include "gnc-pricedb.h"
#include "gnc-tree-view-price.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "guile-util.h"
#include "engine-helpers.h"
#include "messages.h"


#define DIALOG_PRICE_DB_CM_CLASS "dialog-price-edit-db"
#define GCONF_SECTION "dialogs/edit_prices"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;


void gnc_prices_dialog_window_destroy_cb (GtkObject *object, gpointer data);
void gnc_prices_dialog_response (GtkDialog *dialog, gint response_id, gpointer data);
void gnc_prices_dialog_edit_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_remove_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_remove_old_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_add_clicked (GtkWidget *widget, gpointer data);
void gnc_prices_dialog_get_quotes_clicked (GtkWidget *widget, gpointer data);


typedef struct
{
  GtkWidget * dialog;

  GncTreeViewPrice * price_tree;

  GtkWidget * edit_button;
  GtkWidget * remove_button;
  GtkWidget * remove_old_button;

  GNCPriceDB *price_db;
  GNCPrice  * price;		/* Currently selected price */
} PricesDialog;


void
gnc_prices_dialog_window_destroy_cb (GtkObject *object, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  ENTER(" ");
  gnc_unregister_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);

  if (pdb_dialog->price)
  {
    gnc_price_unref (pdb_dialog->price);
    pdb_dialog->price = NULL;
  }

  g_free (pdb_dialog);
  LEAVE(" ");
}

void
gnc_prices_dialog_response (GtkDialog *dialog, gint response_id, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  ENTER(" ");
  gnc_close_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);
  LEAVE(" ");
}

void
gnc_prices_dialog_edit_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  ENTER(" ");
  if (!pdb_dialog->price) {
    LEAVE("no price selected");
    return;
  }

  gnc_price_edit_dialog (pdb_dialog->dialog, pdb_dialog->price, GNC_PRICE_EDIT);
  LEAVE(" ");
}

void
gnc_prices_dialog_remove_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  const char *message = _("Are you sure you want to delete the\n"
                          "selected price?");

  ENTER(" ");
  if (!pdb_dialog->price) {
    LEAVE("no price selected");
    return;
  }

  if (gnc_verify_dialog (pdb_dialog->dialog, TRUE, message))
  {
    GNCBook *book = gnc_get_current_book ();
    GNCPriceDB *pdb = gnc_book_get_pricedb (book);

    gnc_pricedb_remove_price (pdb, pdb_dialog->price);
  }
  LEAVE(" ");
}

void
gnc_prices_dialog_remove_old_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *date;
  GtkWidget *vbox;
  gint result;

  ENTER(" ");
  dialog = gtk_dialog_new_with_buttons (_("Remove old prices"),
		  			GTK_WINDOW (pdb_dialog->dialog),
					GTK_DIALOG_DESTROY_WITH_PARENT,
					GTK_STOCK_CANCEL,
					GTK_RESPONSE_REJECT,
					GTK_STOCK_OK,
					GTK_RESPONSE_ACCEPT,
					NULL);	    

  vbox = GTK_DIALOG (dialog)->vbox;

  gtk_box_set_spacing (GTK_BOX (vbox), 3);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 3);

  label = gtk_label_new (_("All prices before the date below "
                           "will be deleted."));

  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_widget_show (label);

  date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
  g_object_ref (date);
  gtk_object_sink (GTK_OBJECT (date));

  gtk_box_pack_start (GTK_BOX (vbox), date, FALSE, FALSE, 0);
  gtk_widget_show (date);

  result = gtk_dialog_run (GTK_DIALOG (dialog));
  if (result == GTK_RESPONSE_ACCEPT)
  {
    GNCBook *book = gnc_get_current_book ();
    GNCPriceDB *pdb = gnc_book_get_pricedb (book);
    Timespec ts;

    DEBUG("deleting prices");
    ts.tv_sec = gnc_date_edit_get_date (GNC_DATE_EDIT (date));
    ts.tv_nsec = 0;

    gnc_pricedb_remove_old_prices(pdb, ts);
  }

  g_object_unref (date);
  gtk_widget_destroy(dialog);
  LEAVE(" ");
}

void
gnc_prices_dialog_add_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  ENTER(" ");
  gnc_price_edit_dialog (pdb_dialog->dialog, pdb_dialog->price, GNC_PRICE_NEW);
  LEAVE(" ");
}

void
gnc_prices_dialog_get_quotes_clicked (GtkWidget *widget, gpointer data)
{
  GNCBook *book = gnc_get_current_book ();
  SCM quotes_func;
  SCM book_scm;

  ENTER(" ");
  quotes_func = scm_c_eval_string ("gnc:book-add-quotes");
  if (!SCM_PROCEDUREP (quotes_func)) {
    LEAVE(" no procedure");
    return;
  }

  book_scm = gnc_book_to_scm (book);
  if (SCM_NFALSEP (scm_not (book_scm))) {
    LEAVE("no book");
    return;
  }

  gnc_set_busy_cursor (NULL, TRUE);
  scm_call_1 (quotes_func, book_scm);
  gnc_unset_busy_cursor (NULL);
  LEAVE(" ");
}


static void
gnc_prices_dialog_selection_changed (GtkTreeSelection *treeselection,
				     gpointer data)
{
  PricesDialog *pdb_dialog = data;

  ENTER(" ");
  if (pdb_dialog->price)
    gnc_price_unref (pdb_dialog->price);

  pdb_dialog->price =
    gnc_tree_view_price_get_selected_price (pdb_dialog->price_tree);

  if (pdb_dialog->price)
    gnc_price_ref (pdb_dialog->price);

  gtk_widget_set_sensitive (pdb_dialog->edit_button,
                            pdb_dialog->price != NULL);
  gtk_widget_set_sensitive (pdb_dialog->remove_button,
                            pdb_dialog->price != NULL);
  gtk_widget_set_sensitive (pdb_dialog->remove_old_button,
                            pdb_dialog->price != NULL);
  LEAVE(" ");
}


static gboolean
gnc_price_dialog_filter_ns_func (gnc_commodity_namespace *namespace,
				 gpointer data)
{
  PricesDialog *pdb_dialog = data;
  const gchar *name;
  GList *cm_list, *price_list, *item;
  gboolean result;

  /* Never show the template list */
  name = gnc_commodity_namespace_get_name (namespace);
  if (safe_strcmp (name, "template") == 0)
    return FALSE;

  /* See if this namespace has commodities */
  cm_list = gnc_commodity_namespace_get_commodity_list(namespace);
  for (item = cm_list; item; item = g_list_next(item)) {

    /* For each commodity, see if there are prices */
    price_list = gnc_pricedb_get_prices(pdb_dialog->price_db, item->data, NULL);
    result = (price_list != NULL);
    gnc_price_list_destroy(price_list);
    if (result) {
//      printf("Namespace %s visible because %s has prices\n",
//	     name, gnc_commodity_get_mnemonic(item->data));
      return TRUE;
    }
  }

  //  printf("Namespace %s not visible\n", name);
  return FALSE;
}

static gboolean
gnc_price_dialog_filter_cm_func (gnc_commodity *commodity,
				 gpointer data)
{
  PricesDialog *pdb_dialog = data;
  GList *list;
  gboolean result;

  /* Show any commodity that has prices */
  list = gnc_pricedb_get_prices(pdb_dialog->price_db, commodity, NULL);
  result = (list != NULL);
  gnc_price_list_destroy(list);
//  printf("Commodity %s%s visible\n",
//	 gnc_commodity_get_mnemonic(commodity),
//	 result ? "" : " not");
  return result;
}

static void
gnc_prices_dialog_create (GtkWidget * parent, PricesDialog *pdb_dialog)
{
  GtkWidget *dialog, *scrolled_window;
  GladeXML *xml;
  GtkTreeView *view;
  GtkTreeSelection *selection;

  ENTER(" ");
  xml = gnc_glade_xml_new ("price.glade", "Prices Dialog");

  dialog = glade_xml_get_widget (xml, "Prices Dialog");
  pdb_dialog->dialog = dialog;
  pdb_dialog->price_db = gnc_pricedb_get_db(gnc_get_current_book());

  glade_xml_signal_autoconnect_full(xml, gnc_glade_autoconnect_full_func, pdb_dialog);

  /* parent */
  if (parent != NULL)
    gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

  /* default to 'close' button */
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_CLOSE);

  /* price tree */
  scrolled_window = glade_xml_get_widget (xml, "price_list_window");
  view = gnc_tree_view_price_new(gnc_get_current_book(),
				 "gconf-section", GCONF_SECTION,
				 "show-column-menu", TRUE,
				 NULL);
  pdb_dialog->price_tree = GNC_TREE_VIEW_PRICE(view);
  gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(view));
  gnc_tree_view_price_set_filter (pdb_dialog->price_tree,
				  gnc_price_dialog_filter_ns_func,
				  gnc_price_dialog_filter_cm_func,
				  NULL,
				  pdb_dialog, NULL);

  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (view));
  g_signal_connect (G_OBJECT (selection), "changed",
		    G_CALLBACK (gnc_prices_dialog_selection_changed), pdb_dialog);

  /* buttons */
  {
    GtkWidget *button;

    button = glade_xml_get_widget (xml, "edit_button");
    pdb_dialog->edit_button = button;

    button = glade_xml_get_widget (xml, "remove_button");
    pdb_dialog->remove_button = button;

    button = glade_xml_get_widget (xml, "remove_old_button");
    pdb_dialog->remove_old_button = button;
  }

  gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(pdb_dialog->dialog));
  LEAVE(" ");
}

static void
close_handler (gpointer user_data)
{
  PricesDialog *pdb_dialog = user_data;

  ENTER(" ");
  gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(pdb_dialog->dialog));

  gtk_widget_destroy (GTK_WIDGET (pdb_dialog->dialog));
  LEAVE(" ");
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  ENTER(" ");
}

static gboolean
show_handler (const char *class, gint component_id,
	      gpointer user_data, gpointer iter_data)
{
  PricesDialog *pdb_dialog = user_data;

  ENTER(" ");
  if (!pdb_dialog) {
    LEAVE("no data strucure");
    return(FALSE);
  }

  gtk_window_present (GTK_WINDOW(pdb_dialog->dialog));
  LEAVE(" ");
  return(TRUE);
}

/********************************************************************\
 * gnc_prices_dialog                                                *
 *   opens up a window showing all price information                *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_prices_dialog (GtkWidget * parent)
{
  PricesDialog *pdb_dialog;
  gint component_id;

  ENTER(" ");
  if (gnc_forall_gui_components (DIALOG_PRICE_DB_CM_CLASS, show_handler, NULL)) {
      LEAVE("existing dialog raised");
      return;
  }

  pdb_dialog = g_new0 (PricesDialog, 1);

  gnc_prices_dialog_create (parent, pdb_dialog);

  component_id = gnc_register_gui_component (DIALOG_PRICE_DB_CM_CLASS,
                                             refresh_handler, close_handler,
                                             pdb_dialog);

  gtk_widget_grab_focus (GTK_WIDGET(pdb_dialog->price_tree));

  gtk_widget_show (pdb_dialog->dialog);
  LEAVE(" ");
}
