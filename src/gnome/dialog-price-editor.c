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


#define DIALOG_PRICE_EDIT_CM_CLASS "dialog-price-edit"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
  GtkWidget * dialog;

  GtkWidget * commodity_edit;
  GtkWidget * currency_edit;
  GtkWidget * date_edit;
  GtkWidget * source_entry;
  GtkWidget * type_option;
  GtkWidget * price_edit;

  GNCPrice *price;
  gboolean changed;
  gboolean new;

} PriceEditDialog;



static void
gnc_prices_set_changed (PriceEditDialog *pedit_dialog, gboolean changed)
{
  pedit_dialog->changed = changed;
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

  if (safe_strcmp (type, "nav") == 0)
    return 3;

  return 4;
}

static const char *
type_index_to_string (int index)
{
  switch (index)
  {
    case 0: return "bid";
    case 1: return "ask";
    case 2: return "last";
    case 3: return "nav";
    default: return "unknown";
  }
}

static void
price_to_gui (PriceEditDialog *pedit_dialog)
{
  gnc_commodity *commodity;
  gnc_commodity *currency;
  const char *source;
  const char *type;
  gnc_numeric value;
  Timespec date;

  if (pedit_dialog->price)
  {
    commodity = gnc_price_get_commodity (pedit_dialog->price);
    currency = gnc_price_get_currency (pedit_dialog->price);
    date = gnc_price_get_time (pedit_dialog->price);
    source = gnc_price_get_source (pedit_dialog->price);
    type = gnc_price_get_type (pedit_dialog->price);
    value = gnc_price_get_value (pedit_dialog->price);
  }
  else
  {
    commodity = NULL;
    currency = gnc_default_currency ();
    date.tv_sec = time (NULL);
    date.tv_nsec = 0;
    source = "";
    type = "";
    value = gnc_numeric_zero ();
  }

  if (commodity)
    gnc_general_select_set_selected
      (GNC_GENERAL_SELECT (pedit_dialog->commodity_edit), commodity);

  if (currency)
    gnc_currency_edit_set_currency
      (GNC_CURRENCY_EDIT (pedit_dialog->currency_edit), currency);

  gnc_date_edit_set_time (GNC_DATE_EDIT (pedit_dialog->date_edit), date.tv_sec);

  gtk_entry_set_text (GTK_ENTRY (pedit_dialog->source_entry), source);

  gtk_option_menu_set_history (GTK_OPTION_MENU (pedit_dialog->type_option),
                               type_string_to_index (type));

  gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (pedit_dialog->price_edit), value);
}

static const char *
gui_to_price (PriceEditDialog *pedit_dialog)
{
  gnc_commodity *commodity;
  gnc_commodity *currency;
  const char *type;
  gnc_numeric value;
  Timespec date;

  if (!pedit_dialog->price)
    return NULL;

  commodity = gnc_general_select_get_selected
    (GNC_GENERAL_SELECT (pedit_dialog->commodity_edit));
  if (!commodity)
    return _("You must select a commodity.");

  currency = gnc_currency_edit_get_currency
    (GNC_CURRENCY_EDIT (pedit_dialog->currency_edit));
  if (!currency)
    return _("You must select a currency.");

  date.tv_sec = gnc_date_edit_get_date (GNC_DATE_EDIT (pedit_dialog->date_edit));
  date.tv_nsec = 0;

  type = type_index_to_string
    (gnc_option_menu_get_active (pedit_dialog->type_option));

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (pedit_dialog->price_edit)))
    return _("You must enter a valid amount.");

  value = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT (pedit_dialog->price_edit));

  gnc_price_begin_edit (pedit_dialog->price);
  gnc_price_set_commodity (pedit_dialog->price, commodity);
  gnc_price_set_currency (pedit_dialog->price, currency);
  gnc_price_set_time (pedit_dialog->price, date);
  gnc_price_set_type (pedit_dialog->price, type);
  gnc_price_set_value (pedit_dialog->price, value);
  gnc_price_commit_edit (pedit_dialog->price);

  return NULL;
}

static void
price_window_destroy_cb (GtkWidget *widget, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;

  gnc_unregister_gui_component_by_data (DIALOG_PRICE_EDIT_CM_CLASS,
					pedit_dialog);

  if (pedit_dialog->price)
  {
    gnc_price_unref (pedit_dialog->price);
    pedit_dialog->price = NULL;
    pedit_dialog->new = FALSE;
  }

  g_free (pedit_dialog);
}

static void
price_ok_clicked (GtkWidget *widget, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;
  GNCBook *book = gnc_get_current_book ();
  GNCPriceDB *pdb = gnc_book_get_pricedb (book);
  const char *error_str;

  error_str = gui_to_price (pedit_dialog);
  if (error_str) {
    gnc_warning_dialog_parented (pedit_dialog->dialog, error_str);
    return;
  }

  pedit_dialog->changed = FALSE;
  if (TRUE == pedit_dialog->new)
    gnc_pricedb_add_price (pdb, pedit_dialog->price);
  
  gnc_gui_refresh_all ();

  gnome_dialog_close (GNOME_DIALOG (pedit_dialog->dialog));
}

static void
price_cancel_clicked (GtkWidget *widget, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;

  /* price_window_destroy_cb will unref the price */
  gnome_dialog_close (GNOME_DIALOG (pedit_dialog->dialog));
}

static void
commodity_changed_cb (GNCGeneralSelect *gsl, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;

  gnc_prices_set_changed (pedit_dialog, TRUE);
}

static void
currency_changed_cb (GtkWidget *w, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;

  gnc_prices_set_changed (pedit_dialog, TRUE);
}

static void
date_changed_cb (GNCDateEdit *gde, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;

  gnc_prices_set_changed (pedit_dialog, TRUE);
}

static void
date_entry_changed_cb (GtkWidget *w, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;

  gnc_prices_set_changed (pedit_dialog, TRUE);
}

static void
type_menu_changed (GtkButton *button, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;

  gnc_prices_set_changed (pedit_dialog, TRUE);
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
  PriceEditDialog *pedit_dialog = data;

  gnc_prices_set_changed (pedit_dialog, TRUE);
}

static void
gnc_price_pedit_dialog_create (GtkWidget * parent, PriceEditDialog *pedit_dialog)
{
  GladeXML *xml;
  GNCPrintAmountInfo print_info;
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *menu;
  GtkWidget *box;
  GtkWidget *w;

  xml = gnc_glade_xml_new ("price.glade", "Price Dialog");

  dialog = glade_xml_get_widget (xml, "Price Dialog");
  pedit_dialog->dialog = dialog;

  /* parent */
  if (parent != NULL)
    gnome_dialog_set_parent (GNOME_DIALOG (dialog), GTK_WINDOW (parent));

  gnome_dialog_button_connect (GNOME_DIALOG (dialog), 0,
                               GTK_SIGNAL_FUNC (price_ok_clicked),
                               pedit_dialog);

  gnome_dialog_button_connect (GNOME_DIALOG (dialog), 1,
                               GTK_SIGNAL_FUNC (price_cancel_clicked),
                               pedit_dialog);

  gtk_signal_connect (GTK_OBJECT (dialog), "destroy",
		      GTK_SIGNAL_FUNC (price_window_destroy_cb),
		      pedit_dialog);

  box = glade_xml_get_widget (xml, "commodity_box");
  w = gnc_general_select_new (GNC_GENERAL_SELECT_TYPE_SELECT,
			      gnc_commodity_edit_get_string,
			      gnc_commodity_edit_new_select,
			      NULL);
  pedit_dialog->commodity_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);
  gtk_signal_connect (GTK_OBJECT (w), "changed",
                      GTK_SIGNAL_FUNC (commodity_changed_cb), pedit_dialog);


  box = glade_xml_get_widget (xml, "currency_box");
  w = gnc_currency_edit_new ();
  gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT (w),
                                  gnc_default_currency ());
  pedit_dialog->currency_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);
  gtk_signal_connect (GTK_OBJECT (GTK_COMBO(w)->entry), "changed",
                      GTK_SIGNAL_FUNC (currency_changed_cb), pedit_dialog);


  box = glade_xml_get_widget (xml, "date_box");
  w = gnc_date_edit_new (time (NULL), FALSE, FALSE);
  pedit_dialog->date_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);
  gtk_signal_connect (GTK_OBJECT (w), "date_changed",
                      GTK_SIGNAL_FUNC (date_changed_cb), pedit_dialog);
  gtk_signal_connect (GTK_OBJECT (GNC_DATE_EDIT (w)->date_entry), "changed",
                      GTK_SIGNAL_FUNC (date_entry_changed_cb), pedit_dialog);


  w = glade_xml_get_widget (xml, "source_entry");
  pedit_dialog->source_entry = w;

  w = glade_xml_get_widget (xml, "type_option");
  pedit_dialog->type_option = w;


  gnc_option_menu_init (w);
  menu = gtk_option_menu_get_menu (GTK_OPTION_MENU (w));
  gtk_container_forall (GTK_CONTAINER (menu),
                        connect_type_menu_item, pedit_dialog);

  box = glade_xml_get_widget (xml, "price_box");
  w = gnc_amount_edit_new ();
  pedit_dialog->price_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (w), TRUE);
  print_info = gnc_default_price_print_info ();
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (w), print_info);
  gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (w), 1000000);
  gtk_widget_show (w);

  entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (w));
  gtk_signal_connect (GTK_OBJECT (entry), "changed",
                      GTK_SIGNAL_FUNC (amount_changed_cb), pedit_dialog);
}

static void
gnc_price_new_price_init (GNCPrice *price)
{
  Timespec date;

  gnc_price_begin_edit (price);

  gnc_price_set_source (price, "user:price-editor");
  date.tv_sec = time (NULL);
  date.tv_nsec = 0;
  gnc_price_set_time (price, date);

  gnc_price_commit_edit (price);
  
}

static void
close_handler (gpointer user_data)
{
  PriceEditDialog *pedit_dialog = user_data;

  //  gdk_window_get_geometry (GTK_WIDGET(pedit_dialog->dialog)->window,
  //                           NULL, NULL, &last_width, &last_height, NULL);

  //  gnc_save_window_size ("prices_win", last_width, last_height);

  gnome_dialog_close (GNOME_DIALOG (pedit_dialog->dialog));
}

static void
refresh_handler (GHashTable *changes, gpointer user_data)
{
  //  PriceEditDialog *pedit_dialog = user_data;

  //  gnc_prices_load_prices (pedit_dialog);
}

static gboolean
show_handler (const char *class, gint component_id,
	      gpointer user_data, gpointer iter_data)
{
  PriceEditDialog *pedit_dialog = user_data;
  GNCPrice * price = iter_data;

  if (!pedit_dialog || (pedit_dialog->price != price))
    return(FALSE);

  gtk_window_present (GTK_WINDOW(pedit_dialog->dialog));
  return(TRUE);
}

/********************************************************************\
 * gnc_price_edit_dialog                                            *
 *   opens up a window to edit price information                    *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
GNCPrice *
gnc_price_edit_dialog (GtkWidget * parent, GNCPrice * price, GNCPriceEditType type)
{
  PriceEditDialog *pedit_dialog;
  gint component_id;

  if ((type == GNC_PRICE_EDIT) &&
      (gnc_forall_gui_components (DIALOG_PRICE_EDIT_CM_CLASS,
				  show_handler, price)))
      return(price);

  pedit_dialog = g_new0 (PriceEditDialog, 1);
  gnc_price_pedit_dialog_create (parent, pedit_dialog);

  switch (type) {
   case GNC_PRICE_NEW:
    if (price) {
      price = gnc_price_clone(price, gnc_get_current_book ());
    } else {
      price = gnc_price_create (gnc_get_current_book ());
    }
    gnc_price_new_price_init(price);
    pedit_dialog->new = TRUE;
    /* New price will only have one ref, this dialog. */
    break;
   case GNC_PRICE_EDIT:
    gnc_price_ref(price); /* Add ref from this dialog */
    pedit_dialog->new = FALSE;
    break;
  }

  pedit_dialog->price = price;
  price_to_gui(pedit_dialog);

  component_id = gnc_register_gui_component (DIALOG_PRICE_EDIT_CM_CLASS,
                                             refresh_handler, close_handler,
                                             pedit_dialog);

  gtk_widget_grab_focus (pedit_dialog->commodity_edit);

  gtk_widget_show (pedit_dialog->dialog);
  return(price);
}

/********************************************************************\
 * gnc_price_edit_by_guid                                           *
 *   opens up a window to edit price information                    *
 *                                                                  * 
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/
GNCPrice *
gnc_price_edit_by_guid (GtkWidget * parent, const GUID * guid)
{
  PriceEditDialog *pedit_dialog;
  GNCPrice *price;
  gint component_id;

  price = gnc_price_lookup (guid, gnc_get_current_book ());
  if (price == NULL)
    return(NULL);

  return(gnc_price_edit_dialog(parent, price, GNC_PRICE_EDIT));
}
