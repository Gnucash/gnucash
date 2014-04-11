/********************************************************************\
 * dialog-price-editor.c -- price editor dialog                     *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
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
#include <glib/gi18n.h>
#include <time.h>

#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "gnc-general-select.h"
#include "gnc-component-manager.h"
#include "gnc-currency-edit.h"
#include "gnc-date-edit.h"
#include "qof.h"
#include "gnc-pricedb.h"
#include "gnc-session.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "guile-util.h"
#include "engine-helpers.h"


#define DIALOG_PRICE_EDIT_CM_CLASS "dialog-price-edit"
#define GCONF_SECTION "dialogs/price_editor"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_GUI; */

typedef struct
{
  GtkWidget * dialog;
  QofSession *session;
  QofBook *book;
  GNCPriceDB *price_db;

  GtkWidget * commodity_edit;
  GtkWidget * currency_edit;
  GtkWidget * date_edit;
  GtkWidget * source_entry;
  GtkWidget * type_combobox;
  GtkWidget * price_edit;

  GNCPrice *price;
  gboolean changed;
  gboolean new;

} PriceEditDialog;

void pedit_dialog_response_cb (GtkDialog *dialog, gint response, gpointer data);
void pedit_data_changed_cb (GtkWidget *w, gpointer data);


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

  commodity = gnc_price_get_commodity (pedit_dialog->price);
  if (commodity)
  {
    currency = gnc_price_get_currency (pedit_dialog->price);
    date = gnc_price_get_time (pedit_dialog->price);
    source = gnc_price_get_source (pedit_dialog->price);
    type = gnc_price_get_typestr (pedit_dialog->price);
    value = gnc_price_get_value (pedit_dialog->price);
  }
  else
  {
    currency = gnc_default_currency ();
    date.tv_sec = time (NULL);
    date.tv_nsec = 0;
    source = "user:price-editor";
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

  gtk_combo_box_set_active (GTK_COMBO_BOX(pedit_dialog->type_combobox),
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
    (gtk_combo_box_get_active (GTK_COMBO_BOX (pedit_dialog->type_combobox)));

  if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (pedit_dialog->price_edit)))
    return _("You must enter a valid amount.");

  value = gnc_amount_edit_get_amount
    (GNC_AMOUNT_EDIT (pedit_dialog->price_edit));

  gnc_price_begin_edit (pedit_dialog->price);
  gnc_price_set_commodity (pedit_dialog->price, commodity);
  gnc_price_set_currency (pedit_dialog->price, currency);
  gnc_price_set_time (pedit_dialog->price, date);
  gnc_price_set_typestr (pedit_dialog->price, type);
  gnc_price_set_value (pedit_dialog->price, value);
  gnc_price_commit_edit (pedit_dialog->price);

  return NULL;
}

static void
pedit_dialog_destroy_cb (GtkWidget *widget, gpointer data)
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

void
pedit_dialog_response_cb (GtkDialog *dialog, gint response, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;
  const char *error_str;

  if (response == GTK_RESPONSE_OK) {
    error_str = gui_to_price (pedit_dialog);
    if (error_str) {
      gnc_warning_dialog (pedit_dialog->dialog, "%s", error_str);
      return;
    }

    pedit_dialog->changed = FALSE;
    if (TRUE == pedit_dialog->new)
      gnc_pricedb_add_price (pedit_dialog->price_db, pedit_dialog->price);
  
    gnc_gui_refresh_all ();
  }

  gnc_save_window_size(GCONF_SECTION, GTK_WINDOW(pedit_dialog->dialog));
  gtk_widget_destroy (GTK_WIDGET (pedit_dialog->dialog));
  pedit_dialog_destroy_cb (NULL, pedit_dialog);
}

static void
commodity_changed_cb (GNCGeneralSelect *gsl, gpointer data)
{
  gnc_commodity *commodity = NULL;
  gnc_commodity *currency = NULL;
  GList *price_list;
  PriceEditDialog *pedit_dialog = data;

  gnc_prices_set_changed (pedit_dialog, TRUE);

  commodity = gnc_general_select_get_selected
    (GNC_GENERAL_SELECT (pedit_dialog->commodity_edit));

  if(commodity)
  {
    price_list = gnc_pricedb_lookup_latest_any_currency
      (pedit_dialog->price_db, commodity);
    if(price_list)
    {
      currency = gnc_price_get_currency((GNCPrice *)price_list->data);

      if (currency)
	gnc_currency_edit_set_currency
	  (GNC_CURRENCY_EDIT (pedit_dialog->currency_edit), currency);

      gnc_price_list_destroy(price_list);
    }
    else
    {
      gnc_currency_edit_set_currency
	(GNC_CURRENCY_EDIT (pedit_dialog->currency_edit), gnc_default_currency());
    }
  }
}

void
pedit_data_changed_cb (GtkWidget *w, gpointer data)
{
  PriceEditDialog *pedit_dialog = data;

  gnc_prices_set_changed (pedit_dialog, TRUE);
}

static void
gnc_price_pedit_dialog_create (GtkWidget *parent,
			       PriceEditDialog *pedit_dialog,
			       QofSession *session)
{
  GladeXML *xml;
  GNCPrintAmountInfo print_info;
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *box;
  GtkWidget *w;
  GtkWidget *label;

  xml = gnc_glade_xml_new ("price.glade", "Price Dialog");

  pedit_dialog->session = session;
  pedit_dialog->book = qof_session_get_book(pedit_dialog->session);
  pedit_dialog->price_db = gnc_pricedb_get_db(pedit_dialog->book);

  dialog = glade_xml_get_widget (xml, "Price Dialog");
  pedit_dialog->dialog = dialog;

  /* parent */
  if (parent != NULL)
	  gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

  box = glade_xml_get_widget (xml, "commodity_box");
  w = gnc_general_select_new (GNC_GENERAL_SELECT_TYPE_SELECT,
			      gnc_commodity_edit_get_string,
			      gnc_commodity_edit_new_select,
			      NULL);
  pedit_dialog->commodity_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);
  g_signal_connect (G_OBJECT (w), "changed",
                    G_CALLBACK (commodity_changed_cb), pedit_dialog);
  label = glade_xml_get_widget (xml, "commodity_label");
  gnc_general_select_make_mnemonic_target (GNC_GENERAL_SELECT(w), label);


  box = glade_xml_get_widget (xml, "currency_box");
  w = gnc_currency_edit_new ();
  gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT (w),
                                  gnc_default_currency ());
  pedit_dialog->currency_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);
  g_signal_connect (G_OBJECT (GTK_COMBO_BOX(w)), "changed",
                    G_CALLBACK (pedit_data_changed_cb), pedit_dialog);
  label = glade_xml_get_widget (xml, "currency_label");
  gtk_label_set_mnemonic_widget (GTK_LABEL(label), w);

  box = glade_xml_get_widget (xml, "date_box");
  w = gnc_date_edit_new (time (NULL), FALSE, FALSE);
  pedit_dialog->date_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gtk_widget_show (w);
  g_signal_connect (G_OBJECT (w), "date_changed",
                    G_CALLBACK (pedit_data_changed_cb), pedit_dialog);
  g_signal_connect (G_OBJECT (GNC_DATE_EDIT (w)->date_entry), "changed",
                    G_CALLBACK (pedit_data_changed_cb), pedit_dialog);
  gtk_entry_set_activates_default(GTK_ENTRY(GNC_DATE_EDIT(w)->date_entry), TRUE);
  label = glade_xml_get_widget (xml, "date_label");
  gnc_date_make_mnemonic_target (GNC_DATE_EDIT(w), label);


  w = glade_xml_get_widget (xml, "source_entry");
  pedit_dialog->source_entry = w;

  w = glade_xml_get_widget (xml, "type_combobox");
  pedit_dialog->type_combobox = w;

  box = glade_xml_get_widget (xml, "price_box");
  w = gnc_amount_edit_new ();
  pedit_dialog->price_edit = w;
  gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
  gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (w), TRUE);
  print_info = gnc_default_price_print_info ();
  gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (w), print_info);
  gtk_entry_set_activates_default(GTK_ENTRY(w), TRUE);
  gtk_widget_show (w);
  label = glade_xml_get_widget (xml, "price_label");
  gtk_label_set_mnemonic_widget (GTK_LABEL(label), w);

  entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (w));
  g_signal_connect (G_OBJECT (entry), "changed",
                      G_CALLBACK (pedit_data_changed_cb), pedit_dialog);

  glade_xml_signal_autoconnect_full( xml,
                                     gnc_glade_autoconnect_full_func,
                                     pedit_dialog );

}

static void
close_handler (gpointer user_data)
{
  PriceEditDialog *pedit_dialog = user_data;

  gtk_dialog_response(GTK_DIALOG(pedit_dialog->dialog), GTK_RESPONSE_CANCEL);
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
void
gnc_price_edit_dialog (GtkWidget * parent,
		       QofSession *session,
		       GNCPrice * price,
		       GNCPriceEditType type)
{
  PriceEditDialog *pedit_dialog;
  gint component_id;

  if ((type == GNC_PRICE_EDIT) &&
      (gnc_forall_gui_components (DIALOG_PRICE_EDIT_CM_CLASS,
				  show_handler, price)))
      return;

  pedit_dialog = g_new0 (PriceEditDialog, 1);
  gnc_price_pedit_dialog_create (parent, pedit_dialog, session);
  gnc_restore_window_size(GCONF_SECTION, GTK_WINDOW(pedit_dialog->dialog));

  switch (type) {
   case GNC_PRICE_NEW:
    if (price) {
      price = gnc_price_clone(price, pedit_dialog->book);
    } else {
      price = gnc_price_create (pedit_dialog->book);
    }

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
  gnc_gui_component_set_session (component_id, pedit_dialog->session);
  gtk_widget_grab_focus (pedit_dialog->commodity_edit);
  gtk_widget_show (pedit_dialog->dialog);
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
  GNCPrice *price;
  QofSession *session;

  session = gnc_get_current_session ();
  price = gnc_price_lookup (guid, qof_session_get_book(session));
  if (price == NULL)
    return(NULL);

  gnc_price_edit_dialog(parent, session, price, GNC_PRICE_EDIT);
  return price;
}
