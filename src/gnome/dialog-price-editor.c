/********************************************************************\
 * dialog-price-editor.c -- price editor dialog                     *
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
#include <time.h>

#include "FileDialog.h"
#include "dialog-utils.h"
#include "glade-gnc-dialogs.h"
#include "glade-support.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-dateedit.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb.h"
#include "gnc-ui.h"
#include "messages.h"


#define DIALOG_PRICES_CM_CLASS "dialog-prices"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
  GtkWidget * dialog;
  GtkWidget * price_dialog;

  GtkWidget * price_list;
  GtkWidget * edit_button;
  GtkWidget * remove_button;

  GtkWidget * commodity_edit;
  GtkWidget * currency_edit;
  GtkWidget * date_edit;
  GtkWidget * source_entry;
  GtkWidget * type_option;
  GtkWidget * price_edit;

  GNCPrice *price;
  gboolean changed;
  gboolean new;

  GList *prices;
} PricesDialog;


static gint last_width = 0;
static gint last_height = 0;


static void gnc_price_dialog_create (PricesDialog *pdb_dialog);


static void
gnc_prices_set_changed (PricesDialog *pdb_dialog, gboolean changed)
{
  GtkWidget *button;

  pdb_dialog->changed = changed;
}


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
    return result;

  SAFE_STRCMP (gnc_price_get_type (price_a),
               gnc_price_get_type (price_b));

  SAFE_STRCMP (gnc_price_get_source (price_a),
               gnc_price_get_source (price_b));

  return gnc_numeric_compare (gnc_price_get_value (price_a),
                              gnc_price_get_value (price_b));
}

static int
gnc_prices_load_prices (PricesDialog *pdb_dialog)
{
  gnc_commodity *current_commodity;
  GNCPrintAmountInfo print_info;
  GNCPrice *old_price;
  GNCBook *book;
  GList *prices;
  GList *node;
  int new_row;

  book = gncGetCurrentBook ();
  old_price = pdb_dialog->price;
  prices = NULL;
  new_row = 0;

  gnc_pricedb_foreach_price (gnc_book_get_pricedb (book),
                             load_price_helper, &prices, FALSE);

  prices = g_list_sort (prices, price_compare);

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

  return g_list_length (prices);
}

static int
type_string_to_index (const char *type)
{
  if (safe_strcmp (type, "bid") == 0)
    return 0;

  if (safe_strcmp (type, "ask") == 0)
    return 1;

  if (safe_strcmp (type, "last") == 0)
    return 2;

  return 3;
}

static const char *
type_index_to_string (int index)
{
  switch (index)
  {
    case 0: return "bid";
    case 1: return "ask";
    case 2: return "last";
    default: return "unknown";
  }
}

static void
price_to_gui (PricesDialog *pdb_dialog)
{
  gnc_commodity *commodity;
  gnc_commodity *currency;
  const char *source;
  const char *type;
  gnc_numeric value;
  Timespec date;

  if (pdb_dialog->price)
  {
    commodity = gnc_price_get_commodity (pdb_dialog->price);
    currency = gnc_price_get_currency (pdb_dialog->price);
    date = gnc_price_get_time (pdb_dialog->price);
    source = gnc_price_get_source (pdb_dialog->price);
    type = gnc_price_get_type (pdb_dialog->price);
    value = gnc_price_get_value (pdb_dialog->price);
  }
  else
  {
    commodity = NULL;
    currency = gnc_locale_default_currency ();
    date.tv_sec = time (NULL);
    date.tv_nsec = 0;
    source = "";
    type = "";
    value = gnc_numeric_zero ();
  }

  if (commodity)
    gnc_commodity_edit_set_commodity
      (GNC_COMMODITY_EDIT (pdb_dialog->commodity_edit), commodity);

  if (currency)
    gnc_currency_edit_set_currency
      (GNC_CURRENCY_EDIT (pdb_dialog->currency_edit), currency);

  gnc_date_edit_set_time (GNC_DATE_EDIT (pdb_dialog->date_edit), date.tv_sec);

  gtk_entry_set_text (GTK_ENTRY (pdb_dialog->source_entry), source);

  gtk_option_menu_set_history (GTK_OPTION_MENU (pdb_dialog->type_option),
                               type_string_to_index (type));

  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (pdb_dialog->price_edit), value);
}

static const char *
gui_to_price (PricesDialog *pdb_dialog)
{
  gnc_commodity *commodity;
  gnc_commodity *currency;
  const char *source;
  const char *type;
  gnc_numeric value;
  Timespec date;

  if (!pdb_dialog->price)
    return NULL;

  commodity = gnc_commodity_edit_get_commodity
    (GNC_COMMODITY_EDIT (pdb_dialog->commodity_edit));
  if (!commodity)
    return _("You must select a commodity.");

  currency = gnc_currency_edit_get_currency
    (GNC_CURRENCY_EDIT (pdb_dialog->currency_edit));
  if (!currency)
    return _("You must select a currency.");

  date.tv_sec = gnc_date_edit_get_date (GNC_DATE_EDIT (pdb_dialog->date_edit));
  date.tv_nsec = 0;

  type = type_index_to_string
    (gnc_option_menu_get_active (pdb_dialog->type_option));

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (pdb_dialog->price_edit)))
    return _("You must enter a valid amount.");

  value = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT (pdb_dialog->price_edit));

  gnc_price_set_commodity (pdb_dialog->price, commodity);
  gnc_price_set_currency (pdb_dialog->price, currency);
  gnc_price_set_time (pdb_dialog->price, date);
  gnc_price_set_type (pdb_dialog->price, type);
  gnc_price_set_value (pdb_dialog->price, value);

  return NULL;
}

static void
window_destroy_cb (GtkObject *object, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  GList *node;

  gnc_unregister_gui_component_by_data (DIALOG_PRICES_CM_CLASS, pdb_dialog);

  if (pdb_dialog->price)
  {
    gnc_price_unref (pdb_dialog->price);
    pdb_dialog->price = NULL;
  }

  gtk_widget_destroy (pdb_dialog->price_dialog);
  pdb_dialog->price_dialog = NULL;

  for (node = pdb_dialog->prices; node; node = node->next)
  {
    GNCPrice *price = node->data;

    gnc_price_unref (price);
  }

  g_list_free (pdb_dialog->prices);
  pdb_dialog->prices = NULL;

  g_free (pdb_dialog);
}

static gboolean
price_window_delete_cb (GtkWidget *widget, GdkEvent *event, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gtk_widget_hide (pdb_dialog->price_dialog);

  if (pdb_dialog->price && pdb_dialog->new)
  {
    gnc_price_unref (pdb_dialog->price);
    pdb_dialog->price = NULL;
    pdb_dialog->new = FALSE;
  }

  pdb_dialog->price_dialog = NULL;
  gnc_price_dialog_create (pdb_dialog);

  gnc_prices_load_prices (pdb_dialog);

  /* delete the window */
  return FALSE;
}

static void
price_ok_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  GNCBook *book = gncGetCurrentBook ();
  GNCPriceDB *pdb = gnc_book_get_pricedb (book);
  const char *error_str;

  if (!pdb_dialog->changed)
  {
    gtk_widget_hide (pdb_dialog->price_dialog);
    return;
  }

  if (!pdb_dialog->new)
    gnc_pricedb_remove_price (pdb, pdb_dialog->price);

  error_str = gui_to_price (pdb_dialog);

  if (error_str)
  {
    gnc_warning_dialog_parented (pdb_dialog->price_dialog, error_str);

    if (!pdb_dialog->new)
      gnc_pricedb_add_price (pdb, pdb_dialog->price);

    return;
  }

  gtk_widget_hide (pdb_dialog->price_dialog);

  if (!gnc_pricedb_add_price (pdb, pdb_dialog->price))
  {
    gnc_price_unref (pdb_dialog->price);
    pdb_dialog->price = NULL;
  }

  pdb_dialog->new = FALSE;

  gnc_gui_refresh_all ();

  pdb_dialog->changed = FALSE;
}

static void
price_cancel_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  if (pdb_dialog->price && pdb_dialog->new)
  {
    gnc_price_unref (pdb_dialog->price);
    pdb_dialog->price = NULL;
    pdb_dialog->new = FALSE;
  }

  gnc_prices_load_prices (pdb_dialog);

  gtk_widget_hide (pdb_dialog->price_dialog);
}

static void
prices_close_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gnc_close_gui_component_by_data (DIALOG_PRICES_CM_CLASS, pdb_dialog);
}

static void
edit_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  if (!pdb_dialog->price)
    return;

  gtk_widget_show (pdb_dialog->price_dialog);
}

static void
remove_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  const char *message = _("Are you sure you want to delete the\n"
                          "current price?");

  if (!pdb_dialog->price)
    return;

  if (gnc_verify_dialog_parented (pdb_dialog->dialog, message, TRUE))
  {
    GNCBook *book = gncGetCurrentBook ();
    GNCPriceDB *pdb = gnc_book_get_pricedb (book);

    gnc_pricedb_remove_price (pdb, pdb_dialog->price);

    gnc_gui_refresh_all ();
  }
}

static void
add_clicked (GtkWidget *widget, gpointer data)
{
  PricesDialog *pdb_dialog = data;
  Timespec date;

  if (pdb_dialog->price)
    gnc_price_unref (pdb_dialog->price);

  pdb_dialog->price = gnc_price_create ();
  pdb_dialog->new = TRUE;
  pdb_dialog->changed = TRUE;

  gnc_price_set_source (pdb_dialog->price, "user:price-editor");

  date.tv_sec = time (NULL);
  date.tv_nsec = 0;
  gnc_price_set_time (pdb_dialog->price, date);

  gtk_widget_show (pdb_dialog->price_dialog);
}

static void
gnc_prices_select_price_cb (GtkCList *clist, gint row, gint col,
                            GdkEventButton *event, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  if (pdb_dialog->price)
    gnc_price_unref (pdb_dialog->price);

  pdb_dialog->price = gtk_clist_get_row_data (clist, row);
  pdb_dialog->new = FALSE;

  if (pdb_dialog->price)
    gnc_price_ref (pdb_dialog->price);

  price_to_gui (pdb_dialog);
  gtk_widget_set_sensitive (pdb_dialog->edit_button,
                            pdb_dialog->price != NULL);
  gtk_widget_set_sensitive (pdb_dialog->remove_button,
                            pdb_dialog->price != NULL);
  gnc_prices_set_changed (pdb_dialog, FALSE);
}

static void
gnc_prices_unselect_price_cb (GtkCTree *ctre, gint row, gint col,
                              GdkEventButton *event, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  if (pdb_dialog->price)
    gnc_price_unref (pdb_dialog->price);

  pdb_dialog->price = NULL;
  pdb_dialog->new = FALSE;

  gtk_widget_set_sensitive (pdb_dialog->edit_button, FALSE);
  gtk_widget_set_sensitive (pdb_dialog->remove_button, FALSE);
  gnc_prices_set_changed (pdb_dialog, FALSE);
}

static void
commodity_changed_cb (GNCCommodityEdit *gce, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gnc_prices_set_changed (pdb_dialog, TRUE);
}

static void
currency_changed_cb (GtkWidget *w, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gnc_prices_set_changed (pdb_dialog, TRUE);
}

static void
date_changed_cb (GNCDateEdit *gde, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gnc_prices_set_changed (pdb_dialog, TRUE);
}

static void
type_menu_changed (GtkButton *button, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gnc_prices_set_changed (pdb_dialog, TRUE);
}

static void
connect_type_menu_item (GtkWidget *item, gpointer data)
{
  gtk_signal_connect (GTK_OBJECT(item), "activate",
                      GTK_SIGNAL_FUNC (type_menu_changed), data);
}

static void
amount_changed_cb (GtkWidget *w, gpointer data)
{
  PricesDialog *pdb_dialog = data;

  gnc_prices_set_changed (pdb_dialog, TRUE);
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
gnc_price_dialog_create (PricesDialog *pdb_dialog)
{
  GNCPrintAmountInfo print_info;
  GtkWidget *price_dialog;
  GtkWidget *button;
  GtkWidget *entry;
  GtkWidget *menu;
  GtkWidget *box;
  GtkWidget *w;

  price_dialog = create_Price_Dialog ();
  pdb_dialog->price_dialog = price_dialog;

  gnome_dialog_button_connect (GNOME_DIALOG (price_dialog), 0,
                               GTK_SIGNAL_FUNC (price_ok_clicked),
                               pdb_dialog);

  gnome_dialog_button_connect (GNOME_DIALOG (price_dialog), 1,
                               GTK_SIGNAL_FUNC (price_cancel_clicked),
                               pdb_dialog);

  gtk_signal_connect (GTK_OBJECT (price_dialog), "delete_event",
                      GTK_SIGNAL_FUNC (price_window_delete_cb),
                      pdb_dialog);

  box = lookup_widget (price_dialog, "commodity_box");

  w = gnc_commodity_edit_new ();
  pdb_dialog->commodity_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);

  gtk_signal_connect (GTK_OBJECT (w), "changed",
                      GTK_SIGNAL_FUNC (commodity_changed_cb), pdb_dialog);

  box = lookup_widget (price_dialog, "currency_box");

  w = gnc_currency_edit_new ();
  pdb_dialog->currency_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);

  gtk_signal_connect (GTK_OBJECT (GTK_COMBO(w)->entry), "changed",
                      GTK_SIGNAL_FUNC (currency_changed_cb), pdb_dialog);

  box = lookup_widget (price_dialog, "date_box");

  w = gnc_date_edit_new (time (NULL), FALSE, FALSE);
  pdb_dialog->date_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);

  gtk_signal_connect (GTK_OBJECT (w), "date_changed",
                      GTK_SIGNAL_FUNC (date_changed_cb), pdb_dialog);

  w = lookup_widget (price_dialog, "source_entry");
  pdb_dialog->source_entry = w;

  w = lookup_widget (price_dialog, "type_option");
  pdb_dialog->type_option = w;

  gnc_option_menu_init (w);
  menu = gtk_option_menu_get_menu (GTK_OPTION_MENU (w));
  gtk_container_forall (GTK_CONTAINER (menu),
                        connect_type_menu_item, pdb_dialog);

  box = lookup_widget (price_dialog, "price_box");

  w = gnc_amount_edit_new ();
  pdb_dialog->price_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (w), TRUE);
  print_info = gnc_default_price_print_info ();
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (w), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (w), 1000000);
  gtk_widget_show (w);

  entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (w));
  gtk_signal_connect (GTK_OBJECT (entry), "changed",
                      GTK_SIGNAL_FUNC (amount_changed_cb), pdb_dialog);
}

static void
gnc_prices_dialog_create (GtkWidget * parent, PricesDialog *pdb_dialog)
{
  GtkWidget *dialog;

  dialog = create_Prices_Dialog ();
  pdb_dialog->dialog = dialog;

  gnc_price_dialog_create (pdb_dialog);

  gnome_dialog_set_parent (GNOME_DIALOG (pdb_dialog->price_dialog),
                           GTK_WINDOW (dialog));

  gnome_dialog_button_connect (GNOME_DIALOG (dialog), 0,
                               GTK_SIGNAL_FUNC (prices_close_clicked),
                               pdb_dialog);

  gtk_signal_connect (GTK_OBJECT (dialog), "destroy",
                      GTK_SIGNAL_FUNC (window_destroy_cb), pdb_dialog);

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  /* default to ok */
  gnome_dialog_set_default (GNOME_DIALOG(dialog), 0);

  /* price tree */
  {
    GtkWidget *income_radio;
    GtkWidget *list;

    list = lookup_widget (dialog, "price_list");
    pdb_dialog->price_list = list;

    gtk_signal_connect (GTK_OBJECT(list), "select_row",
                        GTK_SIGNAL_FUNC(gnc_prices_select_price_cb),
                        pdb_dialog);

    gtk_signal_connect (GTK_OBJECT(list), "unselect_row",
                        GTK_SIGNAL_FUNC(gnc_prices_unselect_price_cb),
                        pdb_dialog);
  }

  /* buttons */
  {
    GtkWidget *button;

    button = lookup_widget (dialog, "edit_button");
    pdb_dialog->edit_button = button;

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (edit_clicked), pdb_dialog);

    button = lookup_widget (dialog, "remove_button");
    pdb_dialog->remove_button = button;

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (remove_clicked), pdb_dialog);

    button = lookup_widget (dialog, "add_button");

    gtk_signal_connect (GTK_OBJECT (button), "clicked",
                        GTK_SIGNAL_FUNC (add_clicked), pdb_dialog);
  }

  gnc_prices_load_prices (pdb_dialog);
  gnc_prices_set_changed (pdb_dialog, FALSE);
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

/********************************************************************\
 * gnc_prices_dialog                                                *
 *   opens up a window to edit price information                    *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
void
gnc_prices_dialog (GtkWidget * parent)
{
  PricesDialog *pdb_dialog;
  gint component_id;

  pdb_dialog = g_new0 (PricesDialog, 1);

  gnc_prices_dialog_create (parent, pdb_dialog);

  component_id = gnc_register_gui_component (DIALOG_PRICES_CM_CLASS,
                                             refresh_handler, close_handler,
                                             pdb_dialog);

  gtk_widget_grab_focus (pdb_dialog->price_list);

  gtk_widget_show (pdb_dialog->dialog);
}
