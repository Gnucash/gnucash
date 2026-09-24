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

#include <config.h>

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
#include "gnc-warnings.h"
#include "engine-helpers.h"


#define DIALOG_PRICE_EDIT_CM_CLASS "dialog-price-edit"
#define GNC_PREFS_GROUP "dialogs.price-editor"
#define PRICE_EDIT_DIALOG_DATA "gnc-price-edit-dialog-data"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = GNC_MOD_GUI;


typedef struct
{
    GtkWindow *dialog;
    QofSession *session;
    QofBook *book;
    GNCPriceDB *price_db;
    GncGUID book_guid;
    gint component_id;
    GNCPriceEditType type;

    GtkWidget *namespace_cbwe;
    GtkWidget *commodity_cbwe;
    GtkWidget *currency_edit;
    GtkWidget *date_edit;
    GtkWidget *source_entry;
    GtkWidget *type_combobox;
    GtkWidget *price_edit;

    GtkWidget *help_button;
    GtkWidget *cancel_button;
    GtkWidget *apply_button;
    GtkWidget *ok_button;

    GNCPrice *price;
    gboolean changed;
    gboolean is_new;
    gboolean closing;
    gboolean save_pending;
} PriceEditDialog;

typedef struct
{
    GWeakRef dialog;
    GncGUID book_guid;
    GncGUID price_guid;
    GncGUID commodity_guid;
    GncGUID currency_guid;
    gchar *source;
    gchar *type;
    gnc_numeric value;
    time64 date;
    gboolean has_price_guid;
    gboolean replace_existing;
    gboolean apply;
} PriceEditSaveRequest;

static void pedit_data_changed_cb (GtkWidget *widget, gpointer user_data);
static void pedit_commodity_ns_changed_cb (GtkEditable *entry, gpointer user_data);
static void pedit_commodity_changed_cb (GtkEditable *entry, gpointer user_data);
static void pedit_dialog_close (PriceEditDialog *pedit_dialog);


static void
gnc_prices_set_changed (PriceEditDialog *pedit_dialog, gboolean changed)
{
    gboolean save_sensitive;

    pedit_dialog->changed = changed;
    save_sensitive = changed && !pedit_dialog->save_pending;
    gtk_widget_set_sensitive (pedit_dialog->apply_button, save_sensitive);
    gtk_widget_set_sensitive (pedit_dialog->ok_button, save_sensitive);
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
    GNCPrintAmountInfo print_info;
    gnc_commodity *commodity = NULL;
    gnc_commodity *currency = NULL;
    const gchar *name_space, *fullname;
    const char *source;
    const char *type;
    gnc_numeric value;
    time64 date;

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
        date = gnc_price_get_time64 (pedit_dialog->price);
        source = gnc_price_get_source_string (pedit_dialog->price);
        type = gnc_price_get_typestr (pedit_dialog->price);
        value = gnc_price_get_value (pedit_dialog->price);
    }
    else
    {
        currency = gnc_default_currency ();
        date = gnc_time (NULL);
        source = "user:price-editor"; //Sync with source_names in gnc-pricedb.c
        type = "";
        value = gnc_numeric_zero ();
    }


    if (currency)
    {
        gnc_currency_edit_set_currency
        (GNC_CURRENCY_EDIT (pedit_dialog->currency_edit), currency);
    }

    gnc_date_edit_set_time (GNC_DATE_EDIT (pedit_dialog->date_edit), date);

    gnc_entry_set_text (GTK_ENTRY (pedit_dialog->source_entry), source);

    gtk_drop_down_set_selected (GTK_DROP_DOWN (pedit_dialog->type_combobox),
                                type_string_to_index (type));

    print_info = gnc_commodity_print_info (currency, FALSE);
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (pedit_dialog->price_edit), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (pedit_dialog->price_edit), 0);

    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (pedit_dialog->price_edit), value);
}


static gboolean
pedit_dialog_matches_book (const PriceEditDialog *pedit_dialog)
{
    QofBook *book;

    if (!pedit_dialog || pedit_dialog->closing || !pedit_dialog->session ||
        !pedit_dialog->book)
        return FALSE;

    book = qof_session_get_book (pedit_dialog->session);
    return book == pedit_dialog->book && book == gnc_get_current_book () &&
           !qof_instance_get_destroying (QOF_INSTANCE (book)) &&
           guid_equal (qof_instance_get_guid (QOF_INSTANCE (book)),
                       &pedit_dialog->book_guid);
}

static void
pedit_save_request_free (PriceEditSaveRequest *request)
{
    if (!request)
        return;

    g_weak_ref_clear (&request->dialog);
    g_free (request->source);
    g_free (request->type);
    g_free (request);
}

static PriceEditSaveRequest *
pedit_save_request_new (PriceEditDialog *pedit_dialog, gboolean apply)
{
    PriceEditSaveRequest *request = g_new0 (PriceEditSaveRequest, 1);

    g_weak_ref_init (&request->dialog, pedit_dialog->dialog);
    request->book_guid = pedit_dialog->book_guid;
    request->apply = apply;
    return request;
}

static const char *
pedit_save_request_collect (PriceEditDialog *pedit_dialog,
                            PriceEditSaveRequest *request)
{
    GNCPrice *test_price;
    gnc_commodity *commodity;
    gnc_commodity *currency;
    gchar *name_space;
    const gchar *fullname;

    if (!pedit_dialog_matches_book (pedit_dialog))
        return _("The active book has changed.");

    name_space = gnc_ui_namespace_picker_ns (pedit_dialog->namespace_cbwe);
    fullname = gtk_editable_get_text (GTK_EDITABLE (
        gnc_ui_commodity_picker_get_entry (pedit_dialog->commodity_cbwe)));
    commodity = gnc_commodity_table_find_full (gnc_get_current_commodities (),
                                                name_space, fullname);
    g_free (name_space);
    if (!commodity || qof_instance_get_destroying (QOF_INSTANCE (commodity)))
        return _("You must select a Security.");

    currency = gnc_currency_edit_get_currency (
        GNC_CURRENCY_EDIT (pedit_dialog->currency_edit));
    if (!currency || qof_instance_get_destroying (QOF_INSTANCE (currency)))
        return _("You must select a Currency.");

    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT (pedit_dialog->price_edit), NULL))
        return _("You must enter a valid amount.");

    request->commodity_guid = *qof_instance_get_guid (QOF_INSTANCE (commodity));
    request->currency_guid = *qof_instance_get_guid (QOF_INSTANCE (currency));
    request->date = gnc_date_edit_get_date (GNC_DATE_EDIT (pedit_dialog->date_edit));
    request->source = g_strdup (gnc_entry_get_text (
        GTK_ENTRY (pedit_dialog->source_entry)));
    request->type = g_strdup (type_index_to_string (gtk_drop_down_get_selected (
        GTK_DROP_DOWN (pedit_dialog->type_combobox))));
    request->value = gnc_amount_edit_get_amount (
        GNC_AMOUNT_EDIT (pedit_dialog->price_edit));

    if (pedit_dialog->price)
    {
        request->price_guid = *gnc_price_get_guid (pedit_dialog->price);
        request->has_price_guid = TRUE;
    }

    test_price = gnc_pricedb_lookup_day_t64 (pedit_dialog->price_db, commodity,
                                              currency, request->date);
    if (test_price)
    {
        request->replace_existing = pedit_dialog->is_new ||
            !pedit_dialog->price || !gnc_price_equal (test_price,
                                                       pedit_dialog->price);
        gnc_price_unref (test_price);
    }
    return NULL;
}

static const char *
pedit_save_request_commit (PriceEditDialog *pedit_dialog,
                           const PriceEditSaveRequest *request)
{
    gnc_commodity *commodity;
    gnc_commodity *currency;
    GNCPrice *current_price;

    if (!pedit_dialog_matches_book (pedit_dialog) ||
        !guid_equal (&pedit_dialog->book_guid, &request->book_guid))
        return _("The active book has changed.");

    commodity = gnc_commodity_find_commodity_by_guid (&request->commodity_guid,
                                                        pedit_dialog->book);
    currency = gnc_commodity_find_commodity_by_guid (&request->currency_guid,
                                                       pedit_dialog->book);
    if (!commodity || !currency ||
        qof_instance_get_destroying (QOF_INSTANCE (commodity)) ||
        qof_instance_get_destroying (QOF_INSTANCE (currency)))
        return _("The selected commodity is no longer available.");

    if (!pedit_dialog->is_new)
    {
        if (!request->has_price_guid || !pedit_dialog->price ||
            !guid_equal (gnc_price_get_guid (pedit_dialog->price),
                         &request->price_guid) ||
            qof_instance_get_destroying (QOF_INSTANCE (pedit_dialog->price)))
            return _("The selected price is no longer available.");

        current_price = gnc_price_lookup (&request->price_guid,
                                          pedit_dialog->book);
        if (current_price != pedit_dialog->price)
            return _("The selected price is no longer available.");
    }

    if (!pedit_dialog->price)
        pedit_dialog->price = gnc_price_create (pedit_dialog->book);
    if (!pedit_dialog->price)
        return _("Unable to create a price in the active book.");

    gnc_price_begin_edit (pedit_dialog->price);
    gnc_price_set_commodity (pedit_dialog->price, commodity);
    gnc_price_set_currency (pedit_dialog->price, currency);
    gnc_price_set_time64 (pedit_dialog->price, request->date);
    gnc_price_set_source_string (pedit_dialog->price, request->source);
    gnc_price_set_typestr (pedit_dialog->price, request->type);
    gnc_price_set_value (pedit_dialog->price, request->value);
    gnc_price_commit_edit (pedit_dialog->price);
    return NULL;
}

static void
pedit_dialog_show_error (PriceEditDialog *pedit_dialog, const char *message)
{
    gnc_warning_dialog_async (pedit_dialog->dialog, NULL, _("Invalid Price"),
                              message, _("_Close"), GTK_RESPONSE_CLOSE, TRUE,
                              NULL, NULL);
}

static void
pedit_dialog_commit_succeeded (PriceEditDialog *pedit_dialog, gboolean apply)
{
    GNCPrice *new_price;

    gnc_prices_set_changed (pedit_dialog, FALSE);
    if (pedit_dialog->is_new)
        gnc_pricedb_add_price (pedit_dialog->price_db, pedit_dialog->price);
    gnc_gui_refresh_all ();

    if (!apply)
    {
        pedit_dialog_close (pedit_dialog);
        return;
    }

    new_price = gnc_price_clone (pedit_dialog->price, pedit_dialog->book);
    if (!new_price)
    {
        pedit_dialog_close (pedit_dialog);
        return;
    }

    pedit_dialog->is_new = TRUE;
    gnc_price_unref (pedit_dialog->price);
    pedit_dialog->price = new_price;
}

static gboolean
pedit_save_request_matches (PriceEditDialog *pedit_dialog,
                            const PriceEditSaveRequest *request)
{
    return pedit_dialog_matches_book (pedit_dialog) &&
           guid_equal (&pedit_dialog->book_guid, &request->book_guid);
}

static void
pedit_dialog_replace_finished (gint response, gpointer user_data)
{
    PriceEditSaveRequest *request = user_data;
    GtkWindow *window = GTK_WINDOW (g_weak_ref_get (&request->dialog));
    PriceEditDialog *pedit_dialog = NULL;
    const char *error = NULL;

    if (window)
        pedit_dialog = g_object_get_data (G_OBJECT (window),
                                          PRICE_EDIT_DIALOG_DATA);
    if (!pedit_dialog || pedit_dialog->dialog != window ||
        !pedit_save_request_matches (pedit_dialog, request))
        goto done;

    pedit_dialog->save_pending = FALSE;
    if (response != GTK_RESPONSE_YES)
    {
        gnc_prices_set_changed (pedit_dialog, FALSE);
        goto done;
    }

    error = pedit_save_request_commit (pedit_dialog, request);
    if (error)
    {
        pedit_dialog_show_error (pedit_dialog, error);
        gnc_prices_set_changed (pedit_dialog, pedit_dialog->changed);
    }
    else
        pedit_dialog_commit_succeeded (pedit_dialog, request->apply);

done:
    g_clear_object (&window);
    pedit_save_request_free (request);
}

static void
pedit_dialog_save (PriceEditDialog *pedit_dialog, gboolean apply)
{
    PriceEditSaveRequest *request;
    const char *error;

    if (!pedit_dialog || pedit_dialog->closing || pedit_dialog->save_pending)
        return;
    if (!pedit_dialog_matches_book (pedit_dialog))
    {
        pedit_dialog_close (pedit_dialog);
        return;
    }

    request = pedit_save_request_new (pedit_dialog, apply);
    error = pedit_save_request_collect (pedit_dialog, request);
    if (error)
    {
        pedit_dialog_show_error (pedit_dialog, error);
        pedit_save_request_free (request);
        return;
    }

    if (request->replace_existing)
    {
        pedit_dialog->save_pending = TRUE;
        gnc_prices_set_changed (pedit_dialog, pedit_dialog->changed);
        gnc_warning_dialog_async (pedit_dialog->dialog,
                                  GNC_PREF_WARN_PRICE_QUOTES_REPLACE,
                                  _("Replace price?"),
                                  _("Are you sure you want to replace the existing price?"),
                                  _("_Replace"), GTK_RESPONSE_YES, TRUE,
                                  pedit_dialog_replace_finished, request);
        return;
    }

    error = pedit_save_request_commit (pedit_dialog, request);
    if (error)
        pedit_dialog_show_error (pedit_dialog, error);
    else
        pedit_dialog_commit_succeeded (pedit_dialog, apply);
    pedit_save_request_free (request);
}

static void
pedit_dialog_destroy_cb (GtkWidget *widget, gpointer user_data)
{
    PriceEditDialog *pedit_dialog = user_data;

    if (!pedit_dialog)
        return;

    pedit_dialog->closing = TRUE;
    if (pedit_dialog->dialog == GTK_WINDOW (widget))
    {
        g_object_set_data (G_OBJECT (widget), PRICE_EDIT_DIALOG_DATA, NULL);
        pedit_dialog->dialog = NULL;
    }
    if (pedit_dialog->component_id)
    {
        gnc_unregister_gui_component (pedit_dialog->component_id);
        pedit_dialog->component_id = 0;
    }
    if (pedit_dialog->price)
    {
        gnc_price_unref (pedit_dialog->price);
        pedit_dialog->price = NULL;
    }
    g_free (pedit_dialog);
}

static void
pedit_type_changed_cb (GtkDropDown *drop_down, GParamSpec *property,
                       gpointer user_data)
{
    (void)drop_down;
    (void)property;
    gnc_prices_set_changed (user_data, TRUE);
}

static void
pedit_dialog_close (PriceEditDialog *pedit_dialog)
{
    if (!pedit_dialog || pedit_dialog->closing || !pedit_dialog->dialog)
        return;

    pedit_dialog->closing = TRUE;
    gnc_save_window_size (GNC_PREFS_GROUP, pedit_dialog->dialog);
    gtk_window_destroy (pedit_dialog->dialog);
}

static void
pedit_dialog_help_clicked_cb (GtkButton *button, gpointer user_data)
{
    PriceEditDialog *pedit_dialog = user_data;

    (void)button;
    if (pedit_dialog && !pedit_dialog->closing && pedit_dialog->dialog)
        gnc_gnome_help (pedit_dialog->dialog, DF_MANUAL, DL_PRICE_EDIT);
}

static void
pedit_dialog_cancel_clicked_cb (GtkButton *button, gpointer user_data)
{
    (void)button;
    pedit_dialog_close (user_data);
}

static void
pedit_dialog_apply_clicked_cb (GtkButton *button, gpointer user_data)
{
    (void)button;
    pedit_dialog_save (user_data, TRUE);
}

static void
pedit_dialog_ok_clicked_cb (GtkButton *button, gpointer user_data)
{
    (void)button;
    pedit_dialog_save (user_data, FALSE);
}

static gboolean
pedit_dialog_close_request_cb (GtkWindow *window, gpointer user_data)
{
    (void)window;
    pedit_dialog_close (user_data);
    return TRUE;
}

static gboolean
pedit_dialog_key_pressed_cb (GtkEventControllerKey *controller, guint keyval,
                             guint keycode, GdkModifierType state,
                             gpointer user_data)
{
    (void)controller;
    (void)keycode;
    (void)state;

    if (keyval != GDK_KEY_Escape)
        return FALSE;

    pedit_dialog_close (user_data);
    return TRUE;
}

static void
pedit_commodity_ns_changed_cb (GtkEditable *, gpointer data)
{
    PriceEditDialog *pedit_dialog = data;
    gchar *name_space;

    gnc_prices_set_changed (pedit_dialog, TRUE);

    name_space = gnc_ui_namespace_picker_ns (pedit_dialog->namespace_cbwe);
    gnc_ui_update_commodity_picker (pedit_dialog->commodity_cbwe, name_space, NULL);

    g_free(name_space);
}


static void
pedit_commodity_changed_cb (GtkEditable *entry, gpointer data)
{
    gnc_commodity   *commodity = NULL;
    gnc_commodity   *currency = NULL;
    gchar           *name_space;
    const gchar     *fullname;
    GList           *price_list;
    PriceEditDialog *pedit_dialog = data;

    gnc_prices_set_changed (pedit_dialog, TRUE);

    name_space = gnc_ui_namespace_picker_ns (pedit_dialog->namespace_cbwe);
    fullname = gtk_editable_get_text (entry);

    commodity = gnc_commodity_table_find_full(gnc_get_current_commodities(), name_space, fullname);

    if (commodity)
    {
        price_list = gnc_pricedb_lookup_latest_any_currency
                     (pedit_dialog->price_db, commodity);
        if (price_list)
        {
            GNCPrice * price = (GNCPrice*)price_list->data;
            if (gnc_commodity_equiv(commodity, gnc_price_get_currency(price)))
                currency = gnc_price_get_commodity((GNCPrice *)price);
            else
                currency = gnc_price_get_currency((GNCPrice *)price);

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


static void
pedit_data_changed_cb (GtkWidget *widget, gpointer data)
{
    PriceEditDialog *pedit_dialog = data;

    (void)widget;
    gnc_prices_set_changed (pedit_dialog, TRUE);
}


static void
gnc_price_pedit_dialog_create (GtkWidget *parent,
                               PriceEditDialog *pedit_dialog,
                               QofSession *session)
{
    GtkBuilder *builder;
    GNCPrintAmountInfo print_info;
    GtkWindow *dialog;
    GtkWidget *entry;
    GtkWidget *box;
    GtkWidget *w;
    GtkWidget *label;
    gchar     *name_space;

    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-price.ui", "price_dialog");

    pedit_dialog->session = session;
    pedit_dialog->book = qof_session_get_book (pedit_dialog->session);
    pedit_dialog->book_guid = *qof_instance_get_guid (QOF_INSTANCE (pedit_dialog->book));
    pedit_dialog->price_db = gnc_pricedb_get_db (pedit_dialog->book);

    dialog = GTK_WINDOW (gtk_builder_get_object (builder, "price_dialog"));
    pedit_dialog->dialog = dialog;
    g_object_set_data (G_OBJECT (dialog), PRICE_EDIT_DIALOG_DATA, pedit_dialog);

    /* parent */
    if (GTK_IS_WINDOW (parent))
        gtk_window_set_transient_for (dialog, GTK_WINDOW (parent));

    w = GTK_WIDGET(gtk_builder_get_object (builder, "namespace_cbwe"));
    pedit_dialog->namespace_cbwe = w;
    gnc_ui_commodity_picker_setup (w);
    w = GTK_WIDGET(gtk_builder_get_object (builder, "commodity_cbwe"));
    pedit_dialog->commodity_cbwe = w;
    gnc_ui_commodity_picker_setup (w);

    label = GTK_WIDGET(gtk_builder_get_object (builder, "namespace_label"));
    gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (
        gnc_ui_commodity_picker_get_entry (pedit_dialog->namespace_cbwe)));
    label = GTK_WIDGET(gtk_builder_get_object (builder, "commodity_label"));
    gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (
        gnc_ui_commodity_picker_get_entry (pedit_dialog->commodity_cbwe)));
    gnc_ui_update_namespace_picker(pedit_dialog->namespace_cbwe,
                                   GNC_COMMODITY_NS_NONISO_GUI, DIAG_COMM_ALL);
    name_space = gnc_ui_namespace_picker_ns(pedit_dialog->namespace_cbwe);
    gnc_ui_update_commodity_picker(pedit_dialog->commodity_cbwe, name_space, NULL);
    g_free(name_space);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "currency_box"));
    w = gnc_currency_edit_new ();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT (w),
                                    gnc_default_currency ());
    pedit_dialog->currency_edit = w;
    gnc_box_append_full (GTK_BOX (box), w, TRUE, TRUE, 0);
    gtk_widget_set_visible (w, TRUE);
    g_signal_connect (G_OBJECT (w), "changed",
                      G_CALLBACK (pedit_data_changed_cb), pedit_dialog);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "currency_label"));
    gtk_label_set_mnemonic_widget (GTK_LABEL(label), w);

    box = GTK_WIDGET(gtk_builder_get_object (builder, "date_box"));
    w = gnc_date_edit_new (time (NULL), FALSE, FALSE);
    pedit_dialog->date_edit = w;
    gnc_box_append_full (GTK_BOX (box), w, TRUE, TRUE, 0);
    gtk_widget_set_visible (w, TRUE);
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
    gnc_box_append_full (GTK_BOX (box), w, TRUE, TRUE, 0);
    entry = gnc_amount_edit_gtk_entry (GNC_AMOUNT_EDIT (w));
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (w), TRUE);
    print_info = gnc_default_price_print_info (gnc_currency_edit_get_currency
                                              (GNC_CURRENCY_EDIT (pedit_dialog->currency_edit)));
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (w), print_info);
    gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
    gtk_widget_set_visible (w, TRUE);
    label = GTK_WIDGET(gtk_builder_get_object (builder, "price_label"));
    gnc_amount_edit_make_mnemonic_target (GNC_AMOUNT_EDIT(w), label);

    g_signal_connect (G_OBJECT (w), "changed",
                      G_CALLBACK (pedit_data_changed_cb), pedit_dialog);

    w = GTK_WIDGET(gtk_builder_get_object (builder, "pd_help_button"));
    pedit_dialog->help_button = w;

    w = GTK_WIDGET(gtk_builder_get_object (builder, "pd_cancel_button"));
    pedit_dialog->cancel_button = w;

    w = GTK_WIDGET(gtk_builder_get_object (builder, "pd_apply_button"));
    pedit_dialog->apply_button = w;

    w = GTK_WIDGET(gtk_builder_get_object (builder, "pd_ok_button"));
    pedit_dialog->ok_button = w;

    gnc_prices_set_changed (pedit_dialog, FALSE);
    gtk_window_set_default_widget (dialog, pedit_dialog->cancel_button);

    g_signal_connect (pedit_dialog->help_button, "clicked",
                      G_CALLBACK (pedit_dialog_help_clicked_cb), pedit_dialog);
    g_signal_connect (pedit_dialog->cancel_button, "clicked",
                      G_CALLBACK (pedit_dialog_cancel_clicked_cb), pedit_dialog);
    g_signal_connect (pedit_dialog->apply_button, "clicked",
                      G_CALLBACK (pedit_dialog_apply_clicked_cb), pedit_dialog);
    g_signal_connect (pedit_dialog->ok_button, "clicked",
                      G_CALLBACK (pedit_dialog_ok_clicked_cb), pedit_dialog);
    g_signal_connect (pedit_dialog->type_combobox, "notify::selected",
                      G_CALLBACK (pedit_type_changed_cb), pedit_dialog);
    g_signal_connect (dialog, "close-request",
                      G_CALLBACK (pedit_dialog_close_request_cb), pedit_dialog);
    g_signal_connect (dialog, "destroy", G_CALLBACK (pedit_dialog_destroy_cb),
                      pedit_dialog);
    GtkEventController *key_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET (dialog), key_controller);
    g_signal_connect (key_controller, "key-pressed",
                      G_CALLBACK (pedit_dialog_key_pressed_cb), pedit_dialog);
    g_signal_connect (gnc_ui_commodity_picker_get_entry (pedit_dialog->namespace_cbwe),
                      "changed", G_CALLBACK (pedit_commodity_ns_changed_cb),
                      pedit_dialog);
    g_signal_connect (gnc_ui_commodity_picker_get_entry (pedit_dialog->commodity_cbwe),
                      "changed", G_CALLBACK (pedit_commodity_changed_cb),
                      pedit_dialog);

    g_object_unref (builder);
}


static void
close_handler (gpointer user_data)
{
    pedit_dialog_close (user_data);
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

    if (!pedit_dialog || pedit_dialog->closing ||
        !pedit_dialog_matches_book (pedit_dialog) ||
        pedit_dialog->price != price)
        return FALSE;

    gtk_window_present (pedit_dialog->dialog);
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

    if (!session || !qof_session_get_book (session))
        return;

    pedit_dialog = g_new0 (PriceEditDialog, 1);
    gnc_price_pedit_dialog_create (parent, pedit_dialog, session);
    gnc_restore_window_size (GNC_PREFS_GROUP, pedit_dialog->dialog,
                             GTK_IS_WINDOW (parent) ? GTK_WINDOW (parent) : NULL);
    pedit_dialog->type = type;

    switch (type)
    {
    case GNC_PRICE_NEW:
        if (price)
        {
            price = gnc_price_clone(price, pedit_dialog->book);

            gnc_price_set_source (price, PRICE_SOURCE_EDIT_DLG);
            gnc_price_set_time64 (price, gnc_time (NULL));
            gnc_price_set_value (price, gnc_numeric_zero ());
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
    pedit_dialog->component_id = component_id;
    gnc_gui_component_set_session (component_id, pedit_dialog->session);
    gtk_widget_grab_focus (pedit_dialog->commodity_cbwe);
    gtk_window_present (pedit_dialog->dialog);
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
    QofSession *session = gnc_get_current_session();
    QofBook* book = qof_session_get_book (session);

    if (!book)
        return (NULL);
    price = gnc_price_lookup (guid, book);
    if (price == NULL)
        return(NULL);

    gnc_price_edit_dialog(parent, session, price, GNC_PRICE_EDIT);
    return price;
}
