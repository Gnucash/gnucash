/********************************************************************\
 * dialog-price-editor.c -- price editor dialog                     *
 * Copyright (C) 2001 Gnumatic, Inc.                                *
 * Author: Dave Peticolas <dave@krondo.com>                         *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (c) 2009 Herbert Thoma <herbie@hthoma.de>              *
 * Copyright (c) 2011 Robert Fewell                                 *
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
#include "gnc-gtk-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-commodity-edit.h"
#include "dialog-commodity.h"
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
#define GNC_PREFS_GROUP "dialogs.price-editor"
#define DIALOG_PRICE_EDIT_SOURCE "user:price-editor"


/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;


typedef struct
{
    GtkWidget * dialog;
    QofSession *session;
    QofBook *book;
    GNCPriceDB *price_db;
    GNCPriceEditType type;

    GtkWidget * namespace_cbwe;
    GtkWidget * commodity_cbwe;
    GtkWidget * currency_edit;
    GtkWidget * date_edit;
    GtkWidget * source_entry;
    GtkWidget * type_combobox;
    GtkWidget * price_edit;

    GtkWidget * cancel_button;
    GtkWidget * apply_button;
    GtkWidget * ok_button;

    GNCPrice *price;
    gboolean changed;
    gboolean is_new;

} PriceEditDialog;

void pedit_dialog_response_cb (GtkDialog *dialog, gint response, gpointer data);
void pedit_data_changed_cb (GtkWidget *w, gpointer data);
void pedit_commodity_ns_changed_cb (GtkComboBox *cbwe, gpointer data);
void pedit_commodity_changed_cb (GtkComboBox *cbwe, gpointer data);


static void
gnc_prices_set_changed (PriceEditDialog *pedit_dialog, gboolean changed)
{
    pedit_dialog->changed = changed;

    gtk_widget_set_sensitive (pedit_dialog->apply_button, changed);
}


static int
type_string_to_index (const char *type)
{
    if (g_strcmp0 (type, "bid") == 0)
        return 0;

    if (g_strcmp0 (type, "ask") == 0)
        return 1;

    if (g_strcmp0 (type, "last") == 0)
        return 2;

    if (g_strcmp0 (type, "nav") == 0)
        return 3;

    return 4;
}


static const char *
type_index_to_string (int index)
{
    switch (index)
    {
    case 0:
        return "bid";
    case 1:
        return "ask";
    case 2:
        return "last";
    case 3:
        return "nav";
    default:
        return "unknown";
    }
}


static void
price_to_gui (PriceEditDialog *pedit_dialog)
{
    gnc_commodity *commodity = NULL;
    gnc_commodity *currency = NULL;
    const gchar *name_space, *fullname;
    const char *source;
    const char *type;
    gnc_numeric value;
    Timespec date;

    if (pedit_dialog->price)
    {
        commodity = gnc_price_get_commodity (pedit_dialog->price);
    }

    if (commodity)
    {
        name_space = gnc_commodity_get_namespace(commodity);
        fullname = gnc_commodity_get_printname(commodity);
        gnc_ui_update_namespace_picker(pedit_dialog->namespace_cbwe,
                                       name_space, DIAG_COMM_ALL);
        gnc_ui_update_commodity_picker(pedit_dialog->commodity_cbwe,
                                       name_space, fullname);

        currency = gnc_price_get_currency (pedit_dialog->price);
        date = gnc_price_get_time (pedit_dialog->price);
        source = gnc_price_get_source (pedit_dialog->price);
        type = gnc_price_get_typestr (pedit_dialog->price);
        value = gnc_price_get_value (pedit_dialog->price);
    }
    else
    {
        currency = gnc_default_currency ();
        date.tv_sec = gnc_time (NULL);
        date.tv_nsec = 0;
        source = DIALOG_PRICE_EDIT_SOURCE;
        type = "";
        value = gnc_numeric_zero ();
    }


    if (currency)
    {
        gnc_currency_edit_set_currency
        (GNC_CURRENCY_EDIT (pedit_dialog->currency_edit), currency);
    }

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
    gchar         *name_space;
    const gchar   *fullname;
    const char *source;
    const char *type;
    gnc_numeric value;
    Timespec date;

    name_space = gnc_ui_namespace_picker_ns (pedit_dialog->namespace_cbwe);
    fullname = gtk_entry_get_text( GTK_ENTRY( gtk_bin_get_child( GTK_BIN( GTK_COMBO_BOX(pedit_dialog->commodity_cbwe)))));

    commodity = gnc_commodity_table_find_full(gnc_get_current_commodities(), name_space, fullname);
    if (!commodity)
        return _("You must select a Security.");

    currency = gnc_currency_edit_get_currency
               (GNC_CURRENCY_EDIT (pedit_dialog->currency_edit));
    if (!currency)
        return _("You must select a Currency.");

    date.tv_sec = gnc_date_edit_get_date (GNC_DATE_EDIT (pedit_dialog->date_edit));
    date.tv_nsec = 0;

    source = gtk_entry_get_text (GTK_ENTRY (pedit_dialog->source_entry));

    type = type_index_to_string
           (gtk_combo_box_get_active (GTK_COMBO_BOX (pedit_dialog->type_combobox)));

    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (pedit_dialog->price_edit)))
        return _("You must enter a valid amount.");

    value = gnc_amount_edit_get_amount
            (GNC_AMOUNT_EDIT (pedit_dialog->price_edit));

    if (!pedit_dialog->price)
        pedit_dialog->price = gnc_price_create (pedit_dialog->book);
    gnc_price_begin_edit (pedit_dialog->price);
    gnc_price_set_commodity (pedit_dialog->price, commodity);
    gnc_price_set_currency (pedit_dialog->price, currency);
    gnc_price_set_time (pedit_dialog->price, date);
    gnc_price_set_source (pedit_dialog->price, source);
    gnc_price_set_typestr (pedit_dialog->price, type);
    gnc_price_set_value (pedit_dialog->price, value);
    gnc_price_commit_edit (pedit_dialog->price);

    g_free(name_space);

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
        pedit_dialog->is_new = FALSE;
    }

    g_free (pedit_dialog);
}


void
pedit_dialog_response_cb (GtkDialog *dialog, gint response, gpointer data)
{
    PriceEditDialog *pedit_dialog = data;
    GNCPrice *new_price = NULL;
    const char *error_str;

    if ((response == GTK_RESPONSE_OK) || (response == GTK_RESPONSE_APPLY))
    {
        error_str = gui_to_price (pedit_dialog);
        if (error_str)
        {
            gnc_warning_dialog (pedit_dialog->dialog, "%s", error_str);
            return;
        }

        gnc_prices_set_changed (pedit_dialog, FALSE);
        if (TRUE == pedit_dialog->is_new)
        {
            gnc_pricedb_add_price (pedit_dialog->price_db, pedit_dialog->price);
        }

        gnc_gui_refresh_all ();
    }

    if (response == GTK_RESPONSE_APPLY)
    {
        new_price = gnc_price_clone (pedit_dialog->price, pedit_dialog->book);
        pedit_dialog->is_new = TRUE;

        gnc_price_unref (pedit_dialog->price);
        pedit_dialog->price = new_price;
    }
    else
    {
        gnc_save_window_size(GNC_PREFS_GROUP, GTK_WINDOW(pedit_dialog->dialog));
        gtk_widget_destroy (GTK_WIDGET (pedit_dialog->dialog));
        pedit_dialog_destroy_cb (NULL, pedit_dialog);
    }
}


void
pedit_commodity_ns_changed_cb (GtkComboBox *cbwe, gpointer data)
{
    PriceEditDialog *pedit_dialog = data;
    gchar *name_space;

    gnc_prices_set_changed (pedit_dialog, TRUE);

    name_space = gnc_ui_namespace_picker_ns (pedit_dialog->namespace_cbwe);
    gnc_ui_update_commodity_picker (pedit_dialog->commodity_cbwe, name_space, NULL);

    g_free(name_space);
}


void
pedit_commodity_changed_cb (GtkComboBox *cbwe, gpointer data)
{
    gnc_commodity   *commodity = NULL;
    gnc_commodity   *currency = NULL;
    gchar           *name_space;
    const gchar     *fullname;
    GList           *price_list;
    PriceEditDialog *pedit_dialog = data;

    gnc_prices_set_changed (pedit_dialog, TRUE);

    name_space = gnc_ui_namespace_picker_ns (pedit_dialog->namespace_cbwe);
    fullname = gtk_entry_get_text( GTK_ENTRY( gtk_bin_get_child( GTK_BIN( GTK_COMBO_BOX(pedit_dialog->commodity_cbwe)))));

    commodity = gnc_commodity_table_find_full(gnc_get_current_commodities(), name_space, fullname);

    if (commodity)
    {
        price_list = gnc_pricedb_lookup_latest_any_currency
                     (pedit_dialog->price_db, commodity);
        if (price_list)
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

    g_free(name_space);
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
    GtkBuilder *builder;
    GNCPrintAmountInfo print_info;
    GtkWidget *dialog;
    GtkWidget *entry;
    GtkWidget *box;
    GtkWidget *w;
    GtkWidget *label;
    gchar     *name_space;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-price.glade", "liststore1");
    gnc_builder_add_from_file (builder, "dialog-price.glade", "liststore2");
    gnc_builder_add_from_file (builder, "dialog-price.glade", "liststore3");
    gnc_builder_add_from_file (builder, "dialog-price.glade", "Price Dialog");

    pedit_dialog->session = session;
    pedit_dialog->book = qof_session_get_book(pedit_dialog->session);
    pedit_dialog->price_db = gnc_pricedb_get_db(pedit_dialog->book);

    dialog =  GTK_WIDGET(gtk_builder_get_object (builder, "Price Dialog"));
    pedit_dialog->dialog = dialog;

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (dialog), GTK_WINDOW (parent));

    w = GTK_WIDGET(gtk_builder_get_object (builder, "namespace_cbwe"));
    pedit_dialog->namespace_cbwe = w;

    gnc_ui_update_namespace_picker(w, NULL, DIAG_COMM_ALL);
    gnc_cbwe_require_list_item(GTK_COMBO_BOX(pedit_dialog->namespace_cbwe));
    gtk_combo_box_set_active(GTK_COMBO_BOX(pedit_dialog->namespace_cbwe), 1);

    w = GTK_WIDGET(gtk_builder_get_object (builder, "commodity_cbwe"));
    pedit_dialog->commodity_cbwe = w;

    gnc_cbwe_require_list_item(GTK_COMBO_BOX(pedit_dialog->commodity_cbwe));
    name_space = gnc_ui_namespace_picker_ns(pedit_dialog->namespace_cbwe);
    gnc_ui_update_commodity_picker(pedit_dialog->commodity_cbwe, name_space, NULL);
    g_free(name_space);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "currency_box"));
    w = gnc_currency_edit_new ();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT (w),
                                    gnc_default_currency ());
    pedit_dialog->currency_edit = w;
    gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
    gtk_widget_show (w);
    g_signal_connect (G_OBJECT (GTK_COMBO_BOX(w)), "changed",
                      G_CALLBACK (pedit_data_changed_cb), pedit_dialog);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "currency_label"));
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), w);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "date_box"));
    w = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    pedit_dialog->date_edit = w;
    gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
    gtk_widget_show (w);
    g_signal_connect (G_OBJECT (w), "date_changed",
                      G_CALLBACK (pedit_data_changed_cb), pedit_dialog);
    g_signal_connect (G_OBJECT (GNC_DATE_EDIT (w)->date_entry), "changed",
                      G_CALLBACK (pedit_data_changed_cb), pedit_dialog);
    gtk_entry_set_activates_default(GTK_ENTRY(GNC_DATE_EDIT(w)->date_entry), TRUE);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "date__label"));
    gnc_date_make_mnemonic_target (GNC_DATE_EDIT(w), label);

    w = GTK_WIDGET(gtk_builder_get_object (builder, "source_entry"));
    pedit_dialog->source_entry = w;

    w = GTK_WIDGET(gtk_builder_get_object (builder, "type_combobox"));
    pedit_dialog->type_combobox = w;

    box = GTK_WIDGET(gtk_builder_get_object (builder, "price_box"));
    w = gnc_amount_edit_new ();
    pedit_dialog->price_edit = w;
    gtk_box_pack_start (GTK_BOX (box), w, TRUE, TRUE, 0);
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (w), TRUE);
    print_info = gnc_default_price_print_info ();
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (w), print_info);
    gtk_entry_set_activates_default(GTK_ENTRY(w), TRUE);
    gtk_widget_show (w);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "price_label"));
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), w);

    entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (w));
    g_signal_connect (G_OBJECT (entry), "changed",
                      G_CALLBACK (pedit_data_changed_cb), pedit_dialog);

    w = GTK_WIDGET(gtk_builder_get_object (builder, "pd_cancel_button"));
    pedit_dialog->cancel_button = w;

    w = GTK_WIDGET(gtk_builder_get_object (builder, "pd_apply_button"));
    pedit_dialog->apply_button = w;
    gnc_prices_set_changed (pedit_dialog, FALSE);

    w = GTK_WIDGET(gtk_builder_get_object (builder, "pd_ok_button"));
    pedit_dialog->ok_button = w;

    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, pedit_dialog);

    g_object_unref(G_OBJECT(builder));
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
show_handler (const char *klass, gint component_id,
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
    gnc_restore_window_size(GNC_PREFS_GROUP, GTK_WINDOW(pedit_dialog->dialog));
    pedit_dialog->type = type;

    switch (type)
    {
    case GNC_PRICE_NEW:
        if (price)
        {
            price = gnc_price_clone(price, pedit_dialog->book);
//  } else {
//      price = gnc_price_create (pedit_dialog->book);
            gnc_price_set_source (price, DIALOG_PRICE_EDIT_SOURCE);
        }

        pedit_dialog->is_new = TRUE;
        /* New price will only have one ref, this dialog. */
        break;
    case GNC_PRICE_EDIT:
        gnc_price_ref(price); /* Add ref from this dialog */
        pedit_dialog->is_new = FALSE;
        break;
    }

    pedit_dialog->price = price;
    price_to_gui(pedit_dialog);
    gnc_prices_set_changed (pedit_dialog, FALSE);
    component_id = gnc_register_gui_component (DIALOG_PRICE_EDIT_CM_CLASS,
                   refresh_handler, close_handler,
                   pedit_dialog);
    gnc_gui_component_set_session (component_id, pedit_dialog->session);
    gtk_widget_grab_focus (pedit_dialog->commodity_cbwe);
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
gnc_price_edit_by_guid (GtkWidget * parent, const GncGUID * guid)
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
