/********************************************************************\
 * dialog-price-editor.c -- price selector dialog                   *
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
#include <guile/gh.h>
#include <time.h>

#include "dialog-utils.h"
#include "global-options.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-general-select.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "guile-util.h"
#include "engine-helpers.h"
#include "messages.h"


#define DIALOG_PRICE_DB_CM_CLASS "dialog-price-edit-db"

#define COMMODITY_COLUMN 0
#define DATE_COLUMN      2

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
  GtkWidget * dialog;

  gint        sort_column;
  gboolean    ascending;
  GtkWidget * commodity_arrow;
  GtkWidget * date_arrow;

  GtkWidget * price_list;
  GtkWidget * edit_button;
  GtkWidget * remove_button;
  GtkWidget * remove_old_button;

  GNCPrice  * price;		/* Currently selected price */
  GList     * prices;		/* All prices */
} PricesDialog;


static gint last_width = 0;
static gint last_height = 0;

static gboolean
load_price_helper (GNCPrice *price, gpointer data)
{
  GList **prices_p = data;

  gnc_price_ref (price);
  *prices_p = g_list_prepend (*prices_p, price);

  return TRUE;
}

static int
price_compare (gconstpointer a, gconstpointer b)
{
  GNCPrice *price_a = (GNCPrice *) a;
  GNCPrice *price_b = (GNCPrice *) b;
  gnc_commodity *comm_a;
  gnc_commodity *comm_b;
  Timespec ts_a;
  Timespec ts_b;
  gint result;

  comm_a = gnc_price_get_commodity (price_a);
  comm_b = gnc_price_get_commodity (price_b);

  SAFE_STRCMP (gnc_commodity_get_namespace (comm_a),
               gnc_commodity_get_namespace (comm_b));

  SAFE_STRCMP (gnc_commodity_get_mnemonic (comm_a),
               gnc_commodity_get_mnemonic (comm_b));

  comm_a = gnc_price_get_currency (price_a);
  comm_b = gnc_price_get_currency (price_b);

  SAFE_STRCMP (gnc_commodity_get_namespace (comm_a),
               gnc_commodity_get_namespace (comm_b));

  SAFE_STRCMP (gnc_commodity_get_mnemonic (comm_a),
               gnc_commodity_get_mnemonic (comm_b));

  ts_a = gnc_price_get_time (price_a);
  ts_b = gnc_price_get_time (price_b);

  result = timespec_cmp (&ts_a, &ts_b);
  if (result)
    return -result;

  SAFE_STRCMP (gnc_price_get_type (price_a),
               gnc_price_get_type (price_b));

  SAFE_STRCMP (gnc_price_get_source (price_a),
               gnc_price_get_source (price_b));

  return gnc_numeric_compare (gnc_price_get_value (price_a),
			      gnc_price_get_value (price_b));
}

static int
price_date_compare (gconstpointer a, gconstpointer b)
{
  GNCPrice *price_a = (GNCPrice *) a;
  GNCPrice *price_b = (GNCPrice *) b;
  Timespec ts_a;
  Timespec ts_b;
  gint result;

  ts_a = gnc_price_get_time (price_a);
  ts_b = gnc_price_get_time (price_b);

  result = timespec_cmp (&ts_a, &ts_b);
  if (result)
    return -result;

  return price_compare (a, b);
}

static int
gnc_prices_load_prices (PricesDialog *pdb_dialog)
{
  gnc_commodity *current_commodity;
  GNCPrintAmountInfo print_info;
  gboolean sort_commodity;
  GtkArrowType arrow_dir;
  GtkWidget *show, *hide;
  GCompareFunc sort_fn;
  gboolean sort_ascending;
  GNCPrice *old_price;
  GNCBook *book;
  GList *prices;
  GList *node;
  int new_row;

  book = gnc_get_current_book ();
  old_price = pdb_dialog->price;
  prices = NULL;
  new_row = 0;

  gnc_pricedb_foreach_price (gnc_book_get_pricedb (book),
                             load_price_helper, &prices, FALSE);

  sort_commodity = (pdb_dialog->sort_column == COMMODITY_COLUMN);

  if (sort_commodity) {
    show = pdb_dialog->commodity_arrow;
    hide = pdb_dialog->date_arrow;
    sort_fn = price_compare;
    sort_ascending = pdb_dialog->ascending;
  } else {
    show = pdb_dialog->date_arrow;
    hide = pdb_dialog->commodity_arrow;
    sort_fn = price_date_compare;
    sort_ascending = !pdb_dialog->ascending; /* Aren't date sorts fun */
  }

  prices = g_list_sort (prices, sort_fn);
  if (!sort_ascending)
    prices = g_list_reverse (prices);

  arrow_dir = pdb_dialog->ascending ? GTK_ARROW_DOWN: GTK_ARROW_UP;
  gtk_arrow_set(GTK_ARROW(show), arrow_dir, GTK_SHADOW_ETCHED_IN);
  gtk_widget_show(show);
  gtk_widget_hide(hide);

  gtk_clist_freeze (GTK_CLIST (pdb_dialog->price_list));

  gtk_clist_clear (GTK_CLIST (pdb_dialog->price_list));

  current_commodity = NULL;
  print_info = gnc_default_price_print_info ();

  for (node = prices; node; node = node->next)
  {
    GNCPrice *price = node->data;
    const char *text[6];
    gint row;

    text[0] = gnc_commodity_get_printname (gnc_price_get_commodity (price));
    text[1] = gnc_commodity_get_printname (gnc_price_get_currency (price));
    text[2] = gnc_print_date (gnc_price_get_time (price));
    text[3] = gnc_price_get_source (price);
    text[4] = gnc_price_get_type (price);
    text[5] = xaccPrintAmount (gnc_price_get_value (price), print_info);

    row = gtk_clist_append (GTK_CLIST (pdb_dialog->price_list), (char **)text);

    gtk_clist_set_row_data (GTK_CLIST (pdb_dialog->price_list), row, price);

    if (price == old_price)
      new_row = row;
  }

  gtk_clist_thaw (GTK_CLIST (pdb_dialog->price_list));

  gtk_clist_columns_autosize (GTK_CLIST (pdb_dialog->price_list));

  for (node = pdb_dialog->prices; node; node = node->next)
  {
    GNCPrice *price = node->data;

    gnc_price_unref (price);
  }

  g_list_free (pdb_dialog->prices);
  pdb_dialog->prices = prices;

  gtk_clist_select_row (GTK_CLIST (pdb_dialog->price_list), new_row, 0);
  if (gtk_clist_row_is_visible (GTK_CLIST (pdb_dialog->price_list), new_row)
      != GTK_VISIBILITY_FULL)
    gtk_clist_moveto (GTK_CLIST (pdb_dialog->price_list),
                      new_row, 0, 0.5, 0.0);

  gtk_widget_set_sensitive (pdb_dialog->edit_button, prices != NULL);
  gtk_widget_set_sensitive (pdb_dialog->remove_button, prices != NULL);
  gtk_widget_set_sensitive (pdb_dialog->remove_old_button, prices != NULL);

  return g_list_length (prices);
}

static void
window_destroy_cb (GtkObject *object, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  GList *node;

  gnc_unregister_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);

  if (pdb_dialog->price)
  {
    gnc_price_unref (pdb_dialog->price);
    pdb_dialog->price = NULL;
  }

  for (node = pdb_dialog->prices; node; node = node->next)
  {
    GNCPrice *price = node->data;

    gnc_price_unref (price);
  }

  g_list_free (pdb_dialog->prices);
  pdb_dialog->prices = NULL;

  g_free (pdb_dialog);
}

static void
prices_close_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gnc_close_gui_component_by_data (DIALOG_PRICE_DB_CM_CLASS, pdb_dialog);
}

static void
edit_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  if (!pdb_dialog->price)
    return;

  gnc_price_edit_dialog (pdb_dialog->dialog, pdb_dialog->price, GNC_PRICE_EDIT);
}

static void
remove_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  const char *message = _("Are you sure you want to delete the\n"
                          "current price?");

  if (!pdb_dialog->price)
    return;

  if (gnc_verify_dialog_parented (pdb_dialog->dialog, TRUE, message))
  {
    GNCBook *book = gnc_get_current_book ();
    GNCPriceDB *pdb = gnc_book_get_pricedb (book);

    gnc_pricedb_remove_price (pdb, pdb_dialog->price);

    gnc_gui_refresh_all ();
  }
}

static void
remove_old_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *date;
  GtkWidget *vbox;
  gint result;

  dialog = gnome_dialog_new (_("Remove old prices"),
                             GNOME_STOCK_BUTTON_OK,
                             GNOME_STOCK_BUTTON_CANCEL,
                             NULL);

  gnome_dialog_set_parent (GNOME_DIALOG (dialog),
                           GTK_WINDOW (pdb_dialog->dialog));
  gnome_dialog_close_hides (GNOME_DIALOG (dialog), FALSE);

  vbox = GNOME_DIALOG (dialog)->vbox;

  gtk_box_set_spacing (GTK_BOX (vbox), 3);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 3);

  label = gtk_label_new (_("All prices before the date below "
                           "will be deleted."));

  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_widget_show (label);

  date = gnc_date_edit_new (time (NULL), FALSE, FALSE);
  gtk_object_ref (GTK_OBJECT (date));
  gtk_object_sink (GTK_OBJECT (date));

  gtk_box_pack_start (GTK_BOX (vbox), date, FALSE, FALSE, 0);
  gtk_widget_show (date);

  result = gnome_dialog_run_and_close (GNOME_DIALOG (dialog));
  if (result == 0)
  {
    GNCBook *book = gnc_get_current_book ();
    GNCPriceDB *pdb = gnc_book_get_pricedb (book);
    GList *node;
    Timespec ts;

    ts.tv_sec = gnc_date_edit_get_date (GNC_DATE_EDIT (date));
    ts.tv_nsec = 0;

    for (node = pdb_dialog->prices; node; node = node->next)
    {
      GNCPrice *price = node->data;
      Timespec pt = gnc_price_get_time (price);;

      if (timespec_cmp (&pt, &ts) < 0)
        gnc_pricedb_remove_price (pdb, price);
    }

    gnc_gui_refresh_all ();
  }

  gtk_object_unref (GTK_OBJECT (date));
}

static void
add_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gnc_price_edit_dialog (pdb_dialog->dialog, pdb_dialog->price, GNC_PRICE_NEW);
}

static void
get_quotes_clicked (GtkWidget *widget, gpointer data)
{
  GNCBook *book = gnc_get_current_book ();
  SCM quotes_func;
  SCM book_scm;

  quotes_func = gh_eval_str ("gnc:book-add-quotes");
  if (!gh_procedure_p (quotes_func))
    return;

  book_scm = gnc_book_to_scm (book);
  if (gh_scm2bool (gh_not (book_scm)))
    return;

  gnc_set_busy_cursor (NULL, TRUE);
  gh_call1 (quotes_func, book_scm);
  gnc_unset_busy_cursor (NULL);

  gnc_gui_refresh_all ();
}

/**
 * gnc_prices_click_column_cb
 *
 * @par1: A pointer to the clist.
 * @par2: The column number clicked (0 based).
 * @par3: A pointer to the data structure describing this window.
 *
 * This function checks for a valid column number, and determines
 * whether or not to invert the current sort or select a new column
 * for sorting.  It calls the gnc_prices_load_prices() function to
 * sort and display the data.
 */
static void
gnc_prices_click_column_cb(GtkCList *clist, gint column, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  if ((column != COMMODITY_COLUMN) && (column != DATE_COLUMN))
    return;

  if (pdb_dialog->sort_column == column) {
    pdb_dialog->ascending = !pdb_dialog->ascending;
  } else {
    pdb_dialog->sort_column = column;
    pdb_dialog->ascending = TRUE;
  }
  gnc_prices_load_prices (pdb_dialog);
}

static void
gnc_prices_select_price_cb (GtkCList *clist, gint row, gint col,
                            GdkEventButton *event, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  if (pdb_dialog->price)
    gnc_price_unref (pdb_dialog->price);

  pdb_dialog->price = gtk_clist_get_row_data (clist, row);

  if (pdb_dialog->price)
    gnc_price_ref (pdb_dialog->price);

  gtk_widget_set_sensitive (pdb_dialog->edit_button,
                            pdb_dialog->price != NULL);
  gtk_widget_set_sensitive (pdb_dialog->remove_button,
                            pdb_dialog->price != NULL);
  gtk_widget_set_sensitive (pdb_dialog->remove_old_button,
                            pdb_dialog->price != NULL);

  if (event && (event->type == GDK_2BUTTON_PRESS)) {
    edit_clicked(NULL, data);
  }
}

static void
gnc_prices_unselect_price_cb (GtkCTree *ctre, gint row, gint col,
                              GdkEventButton *event, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  if (pdb_dialog->price)
    gnc_price_unref (pdb_dialog->price);

  pdb_dialog->price = NULL;

  gtk_widget_set_sensitive (pdb_dialog->edit_button, FALSE);
  gtk_widget_set_sensitive (pdb_dialog->remove_button, FALSE);
  gtk_widget_set_sensitive (pdb_dialog->remove_old_button, FALSE);
}

static void
prices_set_min_widths (PricesDialog *pdb_dialog)
{
  const char *titles[] = { _("Commodity"),
                           _("Currency"),
                           _("Date"),
                           _("Source"),
                           _("Type"),
                           _("Price") };

  GtkStyle *style = gtk_widget_get_style (pdb_dialog->price_list);
  GdkFont *font = NULL;
  gint width;
  gint i;

  if (style != NULL)
    font = style->font;

  if (font != NULL)
    for (i = 0; i < 6; i++)
    {
      width = gdk_string_width (font, titles[i]);
      gtk_clist_set_column_min_width (GTK_CLIST (pdb_dialog->price_list),
                                      i, width + 5);
    }
}

static void
gnc_prices_dialog_create (GtkWidget * parent, PricesDialog *pdb_dialog)
{
  GtkWidget *dialog;
  GladeXML *xml;

  xml = gnc_glade_xml_new ("price.glade", "Prices Dialog");

  dialog = glade_xml_get_widget (xml, "Prices Dialog");
  pdb_dialog->dialog = dialog;

  gnome_dialog_button_connect (GNOME_DIALOG (dialog), 0,
                               GTK_SIGNAL_FUNC (prices_close_clicked),
                               pdb_dialog);

  gtk_signal_connect (GTK_OBJECT (dialog), "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), pdb_dialog);

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  /* default to 'close' button */
  gnome_dialog_set_default (GNOME_DIALOG(dialog), 0);

  /* price tree */
  {
    GtkWidget *list;

    list = glade_xml_get_widget (xml, "price_list");
    pdb_dialog->price_list = list;
    pdb_dialog->sort_column = COMMODITY_COLUMN;
    pdb_dialog->ascending = TRUE;

    gtk_clist_column_titles_passive(GTK_CLIST(list));
    gtk_clist_column_title_active(GTK_CLIST(list), COMMODITY_COLUMN);
    gtk_clist_column_title_active(GTK_CLIST(list), DATE_COLUMN);

    gtk_signal_connect (GTK_OBJECT(list), "select_row",
                        GTK_SIGNAL_FUNC(gnc_prices_select_price_cb),
                        pdb_dialog);

    gtk_signal_connect (GTK_OBJECT(list), "unselect_row",
                        GTK_SIGNAL_FUNC(gnc_prices_unselect_price_cb),
                        pdb_dialog);

    gtk_signal_connect (GTK_OBJECT(list), "click_column",
			GTK_SIGNAL_FUNC(gnc_prices_click_column_cb),
			pdb_dialog);
  }

  /* buttons */
  {
    GtkWidget *button;

    button = glade_xml_get_widget (xml, "edit_button");
    pdb_dialog->edit_button = button;

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (edit_clicked), pdb_dialog);

    button = glade_xml_get_widget (xml, "remove_button");
    pdb_dialog->remove_button = button;

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (remove_clicked), pdb_dialog);

    button = glade_xml_get_widget (xml, "remove_old_button");
    pdb_dialog->remove_old_button = button;

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (remove_old_clicked), pdb_dialog);

    button = glade_xml_get_widget (xml, "add_button");

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (add_clicked), pdb_dialog);

    button = glade_xml_get_widget (xml, "get_quotes_button");

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (get_quotes_clicked), pdb_dialog);
  }

  /* arrows */
  {
    GtkWidget *arrow;

    arrow = glade_xml_get_widget (xml, "commodity_arrow");
    pdb_dialog->commodity_arrow = arrow;

    arrow = glade_xml_get_widget (xml, "date_arrow");
    pdb_dialog->date_arrow = arrow;
  }

  gnc_prices_load_prices (pdb_dialog);
  prices_set_min_widths (pdb_dialog);

  if (last_width == 0)
    gnc_get_window_size ("prices_win", &last_width, &last_height);

  if (last_height == 0)
    last_height = 400;

  gtk_window_set_default_size (GTK_WINDOW(pdb_dialog->dialog),
                               last_width, last_height);
}

static void
close_handler (gpointer user_data)
{
  PricesDialog *pdb_dialog = user_data;

  gdk_window_get_geometry (GTK_WIDGET(pdb_dialog->dialog)->window,
                           NULL, NULL, &last_width, &last_height, NULL);

  gnc_save_window_size ("prices_win", last_width, last_height);

  gnome_dialog_close (GNOME_DIALOG (pdb_dialog->dialog));
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  PricesDialog *pdb_dialog = user_data;

  gnc_prices_load_prices (pdb_dialog);
}

static gboolean
show_handler (const char *class, gint component_id,
	      gpointer user_data, gpointer iter_data)
{
  PricesDialog *pdb_dialog = user_data;

  if (!pdb_dialog)
    return(FALSE);

  gtk_window_present (GTK_WINDOW(pdb_dialog->dialog));
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

  if (gnc_forall_gui_components (DIALOG_PRICE_DB_CM_CLASS, show_handler, NULL))
      return;

  pdb_dialog = g_new0 (PricesDialog, 1);

  gnc_prices_dialog_create (parent, pdb_dialog);

  component_id = gnc_register_gui_component (DIALOG_PRICE_DB_CM_CLASS,
                                             refresh_handler, close_handler,
                                             pdb_dialog);

  gtk_widget_grab_focus (pdb_dialog->price_list);

  gtk_widget_show (pdb_dialog->dialog);
}
