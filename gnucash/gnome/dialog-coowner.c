/*
 * dialog-coowner.c -- Dialog for Co-Owner entry
 * Copyright (C) 2022 Ralf Zerres
 * Author: Ralf Zerres <ralf.zerres@mail.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-amount-edit.h"
#include "gnc-currency-edit.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"

#include "dialog-search.h"
#include "search-param.h"
#include "QuickFill.h"
#include "gnc-addr-quickfill.h"
#include "gnc-account-sel.h"

#include "gncAddress.h"
#include "gncCoOwner.h"
#include "gncCoOwnerP.h"
#include "gncOwner.h"

#include "business-gnome-utils.h"
#include "dialog-coowner.h"
#include "dialog-job.h"
#include "dialog-invoice.h"
#include "dialog-order.h"
#include "dialog-payment.h"

#define DIALOG_NEW_COOWNER_CM_CLASS "dialog-new-coowner"
#define DIALOG_EDIT_COOWNER_CM_CLASS "dialog-edit-coowner"

#define GNC_PREFS_GROUP_SEARCH "dialogs.business.coowner-search"

void gnc_coowner_window_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_coowner_window_destroy_cb (GtkWidget *widget, gpointer data);
void gnc_coowner_window_help_cb (GtkWidget *widget, gpointer data);
void gnc_coowner_window_ok_cb (GtkWidget *widget, gpointer data);
void gnc_coowner_ccard_acct_toggled_cb (GtkToggleButton *button, gpointer data);
void gnc_coowner_name_changed_cb (GtkWidget *widget, gpointer data);
void gnc_coowner_terms_changed_cb (GtkWidget *widget, gpointer data);
void gnc_coowner_taxincluded_changed_cb (GtkWidget *widget, gpointer data);
void gnc_coowner_taxtable_changed_cb (GtkWidget *widget, gpointer data);
void gnc_coowner_taxtable_check_cb (GtkToggleButton *togglebutton,
                                    gpointer user_data);

void gnc_coowner_addr2_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data);
void gnc_coowner_addr3_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data);
void gnc_coowner_addr4_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data);
gboolean
gnc_coowner_addr2_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                 gpointer user_data );
gboolean
gnc_coowner_addr3_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                 gpointer user_data );
gboolean
gnc_coowner_addr4_key_press_cb( GtkEntry *entry, GdkEventKey *event,
                                 gpointer user_data );

#define ADDR_QUICKFILL "GncAddress-Quickfill"

typedef enum
{
    NEW_COOWNER,
    EDIT_COOWNER
} CoOwnerDialogType;

struct _coowner_select_window
{
    QofBook  *book;
    QofQuery *q;
};

struct _coowner_window
{
    GtkWidget * dialog;

    GtkWidget * id_entry;

    GtkWidget * active_check;

    GtkWidget * addr1_entry;
    GtkWidget * addr2_entry;
    GtkWidget * addr3_entry;
    GtkWidget * addr4_entry;
    GtkWidget * phone_entry;
    GtkWidget * fax_entry;
    GtkWidget * email_entry;

    GtkWidget * apt_share;
    GtkWidget * apt_unit;
    GtkWidget * ccard_acct_check;
    GtkWidget * ccard_acct_sel;
    GtkWidget * currency_edit;
    GtkWidget * credit_amount;
    GtkWidget * discount_amount;
    GtkWidget * distribution_key;
    GtkWidget * language_entry;
    GtkWidget * notes_text;

    GncBillTerm *  terms;
    GtkWidget *    terms_menu;
    GncTaxIncluded taxincluded;
    GtkWidget *    taxincluded_menu;
    GncTaxTable *  taxtable;
    GtkWidget *    taxtable_check;
    GtkWidget *    taxtable_menu;

    GtkWidget * name_entry;
    GtkWidget * username_entry;


    /* ACL? */

    CoOwnerDialogType   dialog_type;
    GncGUID             coowner_guid;
    gint                component_id;
    QofBook *           book;
    GncCoOwner *        created_coowner;

    /* stored data for the description quickfill selection function */
    QuickFill *addr2_quickfill;
    QuickFill *addr3_quickfill;
    QuickFill *addr4_quickfill;
    gint addrX_start_selection;
    gint addrX_end_selection;
    guint addrX_selection_source_id;
};

void
gnc_coowner_taxtable_check_cb (GtkToggleButton *togglebutton,
                                gpointer user_data)
{
    CoOwnerWindow *ow = user_data;

    if (gtk_toggle_button_get_active (togglebutton))
        gtk_widget_set_sensitive (ow->taxtable_menu, TRUE);
    else
        gtk_widget_set_sensitive (ow->taxtable_menu, FALSE);
}

static GncCoOwner *
ow_get_coowner (CoOwnerWindow *ow)
{
    if (!ow)
        return NULL;

    return gncCoOwnerLookup (ow->book, &ow->coowner_guid);
}

static void gnc_ui_to_coowner (CoOwnerWindow *ow, GncCoOwner *coowner)
{
    GtkTextBuffer* text_buffer;
    GtkTextIter start, end;
    gchar *text;
    GncAddress *addr;

    addr = gncCoOwnerGetAddr (coowner);

    gnc_suspend_gui_refresh ();

    gncCoOwnerBeginEdit (coowner);

    if (ow->dialog_type == NEW_COOWNER)
        qof_event_gen(QOF_INSTANCE(coowner), QOF_EVENT_ADD, NULL);

    gncCoOwnerSetID (coowner, gtk_editable_get_chars
                      (GTK_EDITABLE (ow->id_entry), 0, -1));
    gncCoOwnerSetActive (coowner, gtk_toggle_button_get_active
                          (GTK_TOGGLE_BUTTON (ow->active_check)));

    gncAddressSetName (addr, gtk_editable_get_chars
                       (GTK_EDITABLE (ow->name_entry), 0, -1));
    gncAddressSetAddr1 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ow->addr1_entry), 0, -1));
    gncAddressSetAddr2 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ow->addr2_entry), 0, -1));
    gncAddressSetAddr3 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ow->addr3_entry), 0, -1));
    gncAddressSetAddr4 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ow->addr4_entry), 0, -1));
    gncAddressSetPhone (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ow->phone_entry), 0, -1));
    gncAddressSetFax (addr, gtk_editable_get_chars
                      (GTK_EDITABLE (ow->fax_entry), 0, -1));
    gncAddressSetEmail (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (ow->email_entry), 0, -1));

    gncCoOwnerSetLanguage (coowner, gtk_editable_get_chars
                            (GTK_EDITABLE (ow->language_entry), 0, -1));

    gncCoOwnerSetAptUnit (coowner, gnc_amount_edit_get_amount
                           (GNC_AMOUNT_EDIT (ow->apt_unit)));
    gncCoOwnerSetAptShare (coowner, gnc_amount_edit_get_amount
                           (GNC_AMOUNT_EDIT (ow->apt_share)));
    gncCoOwnerSetCredit (coowner, gnc_amount_edit_get_amount
                          (GNC_AMOUNT_EDIT (ow->credit_amount)));
    gncCoOwnerSetCurrency (coowner, gnc_currency_edit_get_currency
                            (GNC_CURRENCY_EDIT (ow->currency_edit)));
    gncCoOwnerSetDiscount (coowner, gnc_amount_edit_get_amount
                            (GNC_AMOUNT_EDIT (ow->discount_amount)));
    gncCoOwnerSetDistributionKey (coowner, gtk_editable_get_chars
                                  (GTK_EDITABLE (ow->distribution_key), 0, -1));

    /* Fill in the CCard Acct */
    gncCoOwnerSetCCard (coowner,
                         (gtk_toggle_button_get_active
                          (GTK_TOGGLE_BUTTON (ow->ccard_acct_check)) ?
                          gnc_account_sel_get_account
                          (GNC_ACCOUNT_SEL (ow->ccard_acct_sel)) : NULL));

    /* Fill in extra notes */
    text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(ow->notes_text));
    gtk_text_buffer_get_bounds (text_buffer, &start, &end);
    text = gtk_text_buffer_get_text (text_buffer, &start, &end, FALSE);
    gncCoOwnerSetNotes (coowner, text);

    gncCoOwnerSetTaxIncluded (coowner, ow->taxincluded);
    gncCoOwnerSetTaxTableOverride
    (coowner, gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (ow->taxtable_check)));
    gncCoOwnerSetTaxTable (coowner, ow->taxtable);

    gncCoOwnerSetTerms (coowner, ow->terms);

    gncCoOwnerSetUsername (coowner, gtk_editable_get_chars
                            (GTK_EDITABLE (ow->username_entry), 0, -1));
    gncCoOwnerCommitEdit (coowner);
    gnc_resume_gui_refresh ();
}

static gboolean check_entry_nonempty (GtkWidget *entry,
                                      const char * error_message)
{
    const char *res = gtk_entry_get_text (GTK_ENTRY (entry));
    if (g_strcmp0 (res, "") == 0)
    {
        if (error_message)
            gnc_error_dialog (gnc_ui_get_gtk_window(entry), "%s", error_message);
        return TRUE;
    }
    return FALSE;
}

static gboolean check_edit_amount (GtkWidget *amount,
                                   gnc_numeric *min, gnc_numeric *max,
                                   const char * error_message)

{
    GError *error = NULL;
    if (!gnc_amount_edit_evaluate (GNC_AMOUNT_EDIT(amount), &error))
    {
        gnc_error_dialog (gnc_ui_get_gtk_window (amount), "%s", error->message);
        g_error_free (error);
        return TRUE;
    }
    return FALSE;
}

void
gnc_coowner_window_ok_cb (GtkWidget *widget, gpointer data)
{
    CoOwnerWindow *ow = data;
    gnc_numeric min,max,share_max;
    gchar *string;
    gnc_commodity *currency;
    GNCPrintAmountInfo print_info;

    /* Check for valid coowner name */
    if (check_entry_nonempty (ow->name_entry,
                              _("The Co-Owner Name filed cannot be left blank, please"
                                "enter a coowner name/person's name.")))
        return;

    /* validate discount selection */
    min = gnc_numeric_zero ();
    max = gnc_numeric_create (100, 1);

    if (check_edit_amount (ow->discount_amount, &min, &max,
                           _("Discount percentage must be between 0-100 "
                             "or just leave it blank.")))
      return;

    /* Set the coowner id if one has not been chosen */
    if (g_strcmp0 (gtk_entry_get_text (GTK_ENTRY (ow->id_entry)), "") == 0)
    {
        string = gncCoOwnerNextID (ow->book);
        gtk_entry_set_text (GTK_ENTRY (ow->id_entry), string);
        g_free(string);
    }

    /* Check for valid apartment share */
    share_max = gnc_numeric_create (1000, 0);
    if (check_edit_amount (ow->apt_share, &min, &share_max,
                           _("Apartment share value must set "
                             "inside the valid range [0-1000].")))
      return;

    /* Check for valid apartment unit */
    if (check_entry_nonempty (ow->apt_unit,
                              _("Please assign the apartment unit.")))
        return;

    /* Check for currency  */
    currency = gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT(ow->currency_edit));
    print_info = gnc_commodity_print_info (currency, FALSE);

    /* Check credit value */
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (ow->credit_amount), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (ow->credit_amount),
                                  gnc_commodity_get_fraction (currency));
    if (check_edit_amount (ow->credit_amount, &min, NULL,
                           _("Credit must be a positive amount or "
                             "you just leave it blank.")))
      return;

    /* Check for valid distribution key */
    if (check_entry_nonempty (ow->distribution_key,
                              _("The Distribution Key cannot be left blank, please"
                                "enter a valid identification string.")))
        return;

    /* Now save it off */
    {
        GncCoOwner *coowner = ow_get_coowner (ow);
        if (coowner)
        {
            gnc_ui_to_coowner (ow, coowner);
        }
        ow->created_coowner = coowner;
        ow->coowner_guid = *guid_null ();
    }

    gnc_close_gui_component (ow->component_id);
}

void
gnc_coowner_window_cancel_cb (GtkWidget *widget, gpointer data)
{
    CoOwnerWindow *ow = data;

    gnc_close_gui_component (ow->component_id);
}

void
gnc_coowner_window_help_cb (GtkWidget *widget, gpointer data)
{
    CoOwnerWindow *ow = data;
    gnc_gnome_help (GTK_WINDOW(ow->dialog), HF_HELP, HL_USAGE_COOWNER);
}

void
gnc_coowner_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    CoOwnerWindow *ow = data;
    GncCoOwner *coowner = ow_get_coowner (ow);

    gnc_suspend_gui_refresh ();

    if (ow->dialog_type == NEW_COOWNER && coowner != NULL)
    {
        gncCoOwnerBeginEdit (coowner);
        gncCoOwnerDestroy (coowner);
        ow->coowner_guid = *guid_null ();
    }

    gnc_unregister_gui_component (ow->component_id);
    gnc_resume_gui_refresh ();

    g_free (ow);
}

void
gnc_coowner_name_changed_cb (GtkWidget *widget, gpointer data)
{
    CoOwnerWindow *ow = data;
    char *fullname, *title;
    const char *name, *id;

    if (!ow)
        return;

    name = gtk_entry_get_text (GTK_ENTRY (ow->name_entry));
    if (!name || *name == '\0')
        name = _("<No name>");

    id = gtk_entry_get_text (GTK_ENTRY (ow->id_entry));

    fullname = g_strconcat (name, " (", id, ")", (char *)NULL);

    if (ow->dialog_type == EDIT_COOWNER)
        title = g_strconcat (_("Edit Co-Owner"), " - ", fullname, (char *)NULL);
    else
        title = g_strconcat (_("Now Co-Owner"), " - ", fullname, (char *)NULL);

    gtk_window_set_title (GTK_WINDOW (ow->dialog), title);

    g_free (fullname);
    g_free (title);
}

void
gnc_coowner_ccard_acct_toggled_cb (GtkToggleButton *button, gpointer data)
{
    CoOwnerWindow *ow = data;

    if (!ow)
        return;

    if (gtk_toggle_button_get_active (button))
    {
        gtk_widget_set_sensitive (ow->ccard_acct_sel, TRUE);
        gtk_widget_show (ow->ccard_acct_sel);
    }
    else
    {
        gtk_widget_set_sensitive (ow->ccard_acct_sel, TRUE);
        gtk_widget_hide (ow->ccard_acct_sel);
    }
}

void
gnc_coowner_terms_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    CoOwnerWindow *ow = data;

    if (!ow) return;
    if (!cbox) return;

    ow->terms = gnc_simple_combo_get_value (cbox);
}

void
gnc_coowner_taxincluded_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    CoOwnerWindow *ow = data;

    if (!ow) return;
    if (!cbox) return;

    ow->taxincluded = GPOINTER_TO_INT (gnc_simple_combo_get_value (cbox));
}

void
gnc_coowner_taxtable_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    CoOwnerWindow *ow = data;

    if (!ow) return;
    if (!cbox) return;

    ow->taxtable = gnc_simple_combo_get_value (cbox);
}

static void
gnc_coowner_window_close_handler (gpointer user_data)
{
    CoOwnerWindow *ow = user_data;

    gtk_widget_destroy (ow->dialog);
}

static void
gnc_coowner_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
    CoOwnerWindow *ow = user_data;
    const EventInfo *info;
    GncCoOwner *coowner = ow_get_coowner (ow);

    /* If there isn't a coowner behind us, close down */
    if (!coowner)
    {
        gnc_close_gui_component (ow->component_id);
        return;
    }

    /* Next, close if this is a destroy event */
    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &ow->coowner_guid);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (ow->component_id);
            return;
        }
    }
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
    const GncGUID *coowner_guid = find_data;
    CoOwnerWindow *ow = user_data;

    return(ow && guid_equal(&ow->coowner_guid, coowner_guid));
}

static CoOwnerWindow *
gnc_coowner_new_window (GtkWindow *parent,
                         QofBook *bookp,
                         GncCoOwner *coowner)
{
    CoOwnerWindow *ow;
    GtkBuilder *builder;
    GtkWidget *hbox, *edit;
    gnc_commodity *currency;
    GNCPrintAmountInfo print_info;
    GList *acct_types;
    Account *ccard_acct;

    /*
     * Find an existing window for this coowner.  If found, bring it to
     * the front.
     */
    if (coowner)
    {
        GncGUID coowner_guid;

        coowner_guid = *gncCoOwnerGetGUID (coowner);
        ow = gnc_find_first_gui_component (DIALOG_EDIT_COOWNER_CM_CLASS,
                                           find_handler, &coowner_guid);
        if (ow)
        {
            gtk_window_set_transient_for (GTK_WINDOW(ow->dialog), parent);
            gtk_window_present (GTK_WINDOW(ow->dialog));
            return(ow);
        }
    }

    /* Find the default currency */
    if (coowner)
        currency = gncCoOwnerGetCurrency (coowner);
    else
        currency = gnc_default_currency ();

    /*
     * No existing coowner window found.  Build a new one.
     */
    ow = g_new0 (CoOwnerWindow, 1);

    ow->book = bookp;

    /* Find the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-coowner.glade", "taxincluded_store");
    gnc_builder_add_from_file (builder, "dialog-coowner.glade", "taxtable_store");
    gnc_builder_add_from_file (builder, "dialog-coowner.glade", "coowner_dialog");
    ow->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "coowner_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(ow->dialog), parent);

    // Set the name for this dialog so it can be easily manipulated with css
    gtk_widget_set_name (GTK_WIDGET(ow->dialog), "gnc-id-coowner");
    gnc_widget_style_context_add_class (GTK_WIDGET(ow->dialog), "gnc-class-coowners");

    g_object_set_data (G_OBJECT (ow->dialog), "dialog_info", ow);

    /* Get entry points */
    ow->id_entry = GTK_WIDGET(gtk_builder_get_object (builder, "id_entry"));

    ow->active_check = GTK_WIDGET(gtk_builder_get_object (builder, "active_check"));

    /* Address fields */
    ow->name_entry = GTK_WIDGET(gtk_builder_get_object (builder, "name_entry"));
    ow->addr1_entry = GTK_WIDGET(gtk_builder_get_object (builder, "addr1_entry"));
    ow->addr2_entry = GTK_WIDGET(gtk_builder_get_object (builder, "addr2_entry"));
    ow->addr3_entry = GTK_WIDGET(gtk_builder_get_object (builder, "addr3_entry"));
    ow->addr4_entry = GTK_WIDGET(gtk_builder_get_object (builder, "addr4_entry"));
    ow->phone_entry = GTK_WIDGET(gtk_builder_get_object (builder, "phone_entry"));
    ow->fax_entry = GTK_WIDGET(gtk_builder_get_object (builder, "fax_entry"));
    ow->email_entry = GTK_WIDGET(gtk_builder_get_object (builder, "email_entry"));

    /* Apartment share value */
    edit = gnc_amount_edit_new();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
    print_info = gnc_integral_print_info ();
    print_info.max_decimal_places = 3;
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit), 1000);
    ow->apt_share = edit;
    gtk_widget_show (edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "apt_share_box"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* Apartment unit value */
    ow->apt_unit = GTK_WIDGET(gtk_builder_get_object (builder, "apt_unit"));

    ow->distribution_key = GTK_WIDGET(gtk_builder_get_object (builder, "distribution_key"));
    ow->language_entry = GTK_WIDGET(gtk_builder_get_object (builder, "language_entry"));

    /* Credit: Monetary Value */
    edit = gnc_amount_edit_new();
    print_info = gnc_commodity_print_info (currency, FALSE);
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit),
                                  gnc_commodity_get_fraction (currency));
    ow->credit_amount = edit;
    gtk_widget_show (edit);

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "credit_box"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* CCard Account Selection */
    ow->ccard_acct_check = GTK_WIDGET(gtk_builder_get_object (builder, "ccard_check"));

    edit = gnc_account_sel_new();
    acct_types = g_list_prepend(NULL, (gpointer)ACCT_TYPE_CREDIT);
    gnc_account_sel_set_acct_filters (GNC_ACCOUNT_SEL(edit), acct_types, NULL);
    gnc_account_sel_set_hexpand (GNC_ACCOUNT_SEL(edit), TRUE);
    g_list_free (acct_types);

    ow->ccard_acct_sel = edit;
    gtk_widget_show (edit);

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "ccard_acct_hbox"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* Currency */
    edit = gnc_currency_edit_new();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(edit), currency);
    ow->currency_edit = edit;

    hbox = GTK_WIDGET(gtk_builder_get_object (builder, "currency_box"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* Discount: Percentage Value */
    edit = gnc_amount_edit_new();
    gnc_amount_edit_set_evaluate_on_enter (GNC_AMOUNT_EDIT (edit), TRUE);
    print_info = gnc_integral_print_info ();
    print_info.max_decimal_places = 5;
    gnc_amount_edit_set_print_info (GNC_AMOUNT_EDIT (edit), print_info);
    gnc_amount_edit_set_fraction (GNC_AMOUNT_EDIT (edit), 100000);
    ow->discount_amount = edit;
    gtk_widget_show (edit);

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "discount_box"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* Notes */
    ow->notes_text = GTK_WIDGET (gtk_builder_get_object (builder, "notes_text"));

    /* Tax table */
    ow->taxincluded_menu = GTK_WIDGET (gtk_builder_get_object (builder, "taxincluded_menu"));
    ow->taxtable_check = GTK_WIDGET (gtk_builder_get_object (builder, "taxtable_button"));
    ow->taxtable_menu = GTK_WIDGET (gtk_builder_get_object (builder, "taxtable_menu"));

    /* Terms  */
    ow->terms_menu = GTK_WIDGET (gtk_builder_get_object (builder, "terms_menu"));

    /* Username  */
    ow->username_entry = GTK_WIDGET(gtk_builder_get_object (builder, "username_entry"));

    /* Setup signals */
    gtk_builder_connect_signals_full (builder, gnc_builder_connect_full_func, ow);

    /* Setup initial values */
    if (coowner != NULL)
    {
        GtkTextBuffer* text_buffer;
        GncAddress *addr;
        const char *string;

        ow->dialog_type = EDIT_COOWNER;
        ow->coowner_guid = *gncCoOwnerGetGUID (coowner);

        addr = gncCoOwnerGetAddr (coowner);

        gtk_entry_set_text (GTK_ENTRY (ow->id_entry), gncCoOwnerGetID (coowner));

        /* Active check toggle button */
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ow->active_check),
                                      gncCoOwnerGetActive (coowner));

        /* Setup Address */
        gtk_entry_set_text (GTK_ENTRY (ow->name_entry), gncAddressGetName (addr));
        gtk_entry_set_text (GTK_ENTRY (ow->addr1_entry), gncAddressGetAddr1 (addr));
        gtk_entry_set_text (GTK_ENTRY (ow->addr2_entry), gncAddressGetAddr2 (addr));
        gtk_entry_set_text (GTK_ENTRY (ow->addr3_entry), gncAddressGetAddr3 (addr));
        gtk_entry_set_text (GTK_ENTRY (ow->addr4_entry), gncAddressGetAddr4 (addr));
        gtk_entry_set_text (GTK_ENTRY (ow->phone_entry), gncAddressGetPhone (addr));
        gtk_entry_set_text (GTK_ENTRY (ow->fax_entry), gncAddressGetFax (addr));
        gtk_entry_set_text (GTK_ENTRY (ow->email_entry), gncAddressGetEmail (addr));

        /* Set Language */
        gtk_entry_set_text (GTK_ENTRY (ow->language_entry),
                            gncCoOwnerGetLanguage (coowner));

        /* Distribution Key */
        gtk_entry_set_text (GTK_ENTRY (ow->distribution_key),
                            gncCoOwnerGetDistributionKey (coowner));

        /* Notes */
        string = gncCoOwnerGetNotes (coowner);
        text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(ow->notes_text));
        gtk_text_buffer_set_text (text_buffer, string, -1);

        /* Assigne name values */
        gtk_entry_set_text (GTK_ENTRY (ow->name_entry),
                            gncCoOwnerGetName (coowner));
        gtk_entry_set_text (GTK_ENTRY (ow->username_entry),
                            gncCoOwnerGetUsername (coowner));

        ow->terms = gncCoOwnerGetTerms (coowner);

        /* Registrer the handler */
        ow->component_id =
            gnc_register_gui_component (DIALOG_EDIT_COOWNER_CM_CLASS,
                                        gnc_coowner_window_refresh_handler,
                                        gnc_coowner_window_close_handler,
                                        ow);
        /* Set toggle buttons */
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ow->taxtable_check),
                                      gncCoOwnerGetTaxTableOverride (coowner));

        string = gncCoOwnerGetNotes (coowner);
        text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(ow->notes_text));
        gtk_text_buffer_set_text (text_buffer, string, -1);

        ow->component_id =
          gnc_register_gui_component (DIALOG_EDIT_COOWNER_CM_CLASS,
                                      gnc_coowner_window_refresh_handler,
                                      gnc_coowner_window_close_handler,
                                      ow);
        ow->terms = gncCoOwnerGetTerms (coowner);
    }
    else
    {
        coowner = gncCoOwnerCreate (bookp);

        ow->coowner_guid = *gncCoOwnerGetGUID (coowner);

        ow->dialog_type = NEW_COOWNER;
        ow->component_id =
            gnc_register_gui_component (DIALOG_NEW_COOWNER_CM_CLASS,
                                        gnc_coowner_window_refresh_handler,
                                        gnc_coowner_window_close_handler,
                                        ow);

        /* TODO: assign global-default terms */
        ow->terms = NULL;
    }

    /* I know that coowner exists here -- either passed in or just created */

    /* Set the apartment share and unit values */
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ow->apt_share),
                                gncCoOwnerGetAptShare (coowner));
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ow->apt_unit),
                                gncCoOwnerGetAptUnit (coowner));


    /* Set Credit-Card entities */
    ccard_acct = gncCoOwnerGetCCard (coowner);
    if (ccard_acct == NULL)
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ow->ccard_acct_check), FALSE);
        gtk_widget_set_sensitive (ow->ccard_acct_sel, FALSE);
    }
    else
    {
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (ow->ccard_acct_check), TRUE);
        gnc_account_sel_set_account (GNC_ACCOUNT_SEL (ow->ccard_acct_sel), ccard_acct, FALSE);
    }

    /* Set billing terms */
    gnc_billterms_combo (GTK_COMBO_BOX(ow->terms_menu), bookp, TRUE, ow->terms);

    /* Setup tax related entities */
    ow->taxincluded = gncCoOwnerGetTaxIncluded (coowner);
    gnc_taxincluded_combo (GTK_COMBO_BOX(ow->taxincluded_menu), ow->taxincluded);

    ow->taxtable = gncCoOwnerGetTaxTable (coowner);
    gnc_taxtables_combo (GTK_COMBO_BOX(ow->taxtable_menu), bookp, TRUE, ow->taxtable);

    gnc_coowner_taxtable_check_cb (GTK_TOGGLE_BUTTON (ow->taxtable_check), ow);

    /* Set up the addr line quickfill */
    ow->addr2_quickfill = gnc_get_shared_address_addr2_quickfill(ow->book, ADDR_QUICKFILL);
    ow->addr3_quickfill = gnc_get_shared_address_addr3_quickfill(ow->book, ADDR_QUICKFILL);
    ow->addr4_quickfill = gnc_get_shared_address_addr4_quickfill(ow->book, ADDR_QUICKFILL);

    /* Set Credit and Discount amounts */
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ow->discount_amount),
                                gncCoOwnerGetDiscount (coowner));
    gnc_amount_edit_set_amount (GNC_AMOUNT_EDIT (ow->credit_amount),
                                gncCoOwnerGetCredit (coowner));

    gnc_gui_component_watch_entity_type (ow->component_id,
                                         GNC_COOWNER_MODULE_NAME,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    /* Handle the coowner widget */
    gtk_widget_show_all (ow->dialog);

    if (ccard_acct == NULL)
        gtk_widget_hide (ow->ccard_acct_sel);

    g_object_unref(G_OBJECT(builder));

    return ow;
}

CoOwnerWindow *
gnc_ui_coowner_new (GtkWindow *parent, QofBook *bookp)
{
    CoOwnerWindow *ow;

    /* Make sure required options exist */
    if (!bookp) return NULL;

    ow = gnc_coowner_new_window (parent, bookp, NULL);

    return ow;
}

CoOwnerWindow *
gnc_ui_coowner_edit (GtkWindow *parent, GncCoOwner *coowner)
{
    CoOwnerWindow *ow;

    if (!coowner) return NULL;

    ow = gnc_coowner_new_window (parent, gncCoOwnerGetBook(coowner), coowner);

    return ow;
}

/* Functions for coowner selection widgets */

static void
edit_coowner_cb (GtkWindow *dialog, gpointer *coowner_p, gpointer user_data)
{
    GncCoOwner *coowner;

    g_return_if_fail (coowner_p && user_data);

    coowner = *coowner_p;

    if (!coowner)
        return;

    gnc_ui_coowner_edit (dialog, coowner);
    return;
}

static void
free_coowner_cb (gpointer user_data)
{
    struct _coowner_select_window *sw = user_data;

    g_return_if_fail (sw);

    qof_query_destroy (sw->q);
    g_free (sw);
}

static void
invoice_coowner_cb (GtkWindow *dialog, gpointer *coowner_p, gpointer user_data)
{
    struct _coowner_select_window *sw = user_data;
    GncOwner owner;
    GncCoOwner *coowner;

    g_return_if_fail (coowner_p && user_data);

    coowner = *coowner_p;

    if (!coowner)
        return;

    gncOwnerInitCoOwner (&owner, coowner);
    gnc_invoice_search (dialog, NULL, &owner, sw->book);
    return;
}

static gpointer
new_coowner_cb (GtkWindow *dialog, gpointer user_data)
{
    struct _coowner_select_window *sw = user_data;
    CoOwnerWindow *ow;

    g_return_val_if_fail (user_data, NULL);

    ow = gnc_ui_coowner_new (dialog, sw->book);
    return ow_get_coowner (ow);
}

static void
order_coowner_cb (GtkWindow *dialog, gpointer *coowner_p, gpointer user_data)
{
    struct _coowner_select_window *sw = user_data;
    GncOwner owner;
    GncCoOwner *coowner;

    g_return_if_fail (coowner_p && user_data);

    coowner = *coowner_p;

    if (!coowner)
        return;

    gncOwnerInitCoOwner (&owner, coowner);
    gnc_order_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
jobs_coowner_cb (GtkWindow *dialog, gpointer *coowner_p, gpointer user_data)
{
    struct _coowner_select_window *sw = user_data;
    GncOwner owner;
    GncCoOwner *coowner;

    g_return_if_fail (coowner_p && user_data);

    coowner = *coowner_p;

    if (!coowner)
        return;

    gncOwnerInitCoOwner (&owner, coowner);
    gnc_job_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
payment_coowner_cb (GtkWindow *dialog, gpointer *coowner_p, gpointer user_data)
{
    struct _coowner_select_window *sw = user_data;
    GncOwner owner;
    GncCoOwner *coowner;

    g_return_if_fail (coowner_p && user_data);

    coowner = *coowner_p;

    if (!coowner)
        return;

    gncOwnerInitCoOwner (&owner, coowner);
    gnc_ui_payment_new (dialog, &owner, sw->book);
    return;
}

GNCSearchWindow *
gnc_coowner_search (GtkWindow *parent, GncCoOwner *start, QofBook *book)
{
    QofIdType type = GNC_COOWNER_MODULE_NAME;
    struct _coowner_select_window *sw;
    QofQuery *q, *q2 = NULL;
    static GList *params = NULL;
    static GList *columns = NULL;
    static GNCSearchCallbackButton buttons[] =
    {
        { N_("View/Edit Co-Owner"), edit_coowner_cb, NULL, TRUE},
        { N_("Co-Owner's Jobs"), jobs_coowner_cb, NULL, TRUE},
        { N_("Invoices"), invoice_coowner_cb, NULL, TRUE},
        { N_("Process Payment"), payment_coowner_cb, NULL, FALSE},
        { NULL },
    };

    g_return_val_if_fail (book, NULL);

    /* Build parameter list in reverse order */
    if (params == NULL)
    {
        params = gnc_search_param_prepend (params, _("Co-Owner ID"), NULL, type,
                                           COOWNER_ID, NULL);
        params = gnc_search_param_prepend (params, _("Co-Owner Username"), NULL,
                                           type, COOWNER_USERNAME, NULL);
        params = gnc_search_param_prepend (params, _("Co-Owner Name"), NULL,
                                           type, COOWNER_ADDR, ADDRESS_NAME, NULL);
    }

    /* Build the column list in reverse order */
    if (columns == NULL)
    {
        columns = gnc_search_param_prepend (columns, _("Username"), NULL, type,
                                            COOWNER_USERNAME, NULL);
        columns = gnc_search_param_prepend (columns, _("ID #"), NULL, type,
                                            COOWNER_ID, NULL);
        columns = gnc_search_param_prepend (columns, _("Name"), NULL, type,
                                            COOWNER_ADDR, ADDRESS_NAME, NULL);
    }

    /* Build the queries */
    q = qof_query_create_for (type);
    qof_query_set_book (q, book);

#if 0
    if (start)
    {
        q2 = qof_query_copy (q);
        qof_query_add_guid_match (q2, g_slist_prepend (NULL, QOF_PARAM_GUID),
                                  gncCoOwnerGetGUID (start), QOF_QUERY_AND);
    }
#endif

    /* launch select dialog and return the result */
    sw = g_new0 (struct _coowner_select_window, 1);
    sw->book = book;
    sw->q = q;

    return gnc_search_dialog_create (parent, type, _("Find Co-Owner"),
                                     params, columns, q, q2,
                                     buttons, NULL, new_coowner_cb,
                                     sw, free_coowner_cb,
                                     GNC_PREFS_GROUP_SEARCH, NULL,
                                     "gnc-class-coowners");
}

GNCSearchWindow *
gnc_coowner_search_select (GtkWindow *parent, gpointer start, gpointer book)
{
    if (!book) return NULL;

    return gnc_coowner_search (parent, start, book);
}

GNCSearchWindow *
gnc_coowner_search_edit (GtkWindow *parent, gpointer start, gpointer book)
{
    if (start)
        gnc_ui_coowner_edit (parent, start);

    return NULL;
}

static gboolean
idle_select_region_addr2(gpointer user_data)
{
    CoOwnerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    /* g_warning("Selection start=%d end=%d",wdata->addrX_start_selection, wdata->addrX_end_selection); */

    gtk_editable_select_region(GTK_EDITABLE(wdata->addr2_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);

    wdata->addrX_selection_source_id = 0;
    return FALSE;
}

static gboolean
idle_select_region_addr3(gpointer user_data)
{
    CoOwnerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    gtk_editable_select_region(GTK_EDITABLE(wdata->addr3_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);

    wdata->addrX_selection_source_id = 0;
    return FALSE;
}
static gboolean
idle_select_region_addr4(gpointer user_data)
{
    CoOwnerWindow *wdata = user_data;
    g_return_val_if_fail(user_data, FALSE);

    gtk_editable_select_region(GTK_EDITABLE(wdata->addr4_entry),
                               wdata->addrX_start_selection,
                               wdata->addrX_end_selection);
    wdata->addrX_selection_source_id = 0;
    return FALSE;
}

/* Implementation of the steps common to all address lines. Returns
 * TRUE if anything was inserted by quickfill, otherwise FALSE. */
static gboolean
gnc_coowner_addr_common_insert_cb(GtkEditable *editable,
                                   gchar *new_text, gint new_text_length,
                                   gint *position, gpointer user_data, QuickFill *qf)
{
    CoOwnerWindow *wdata = user_data;
    gchar *concatenated_text;
    QuickFill *match;
    gint prefix_len, concatenated_text_len;

    if (new_text_length <= 0)
        return FALSE;

    /*g_warning("In gnc_coowner_addr_common_insert_cb");*/

    {
        gchar *suffix = gtk_editable_get_chars(editable, *position, -1);
        /* If we are inserting in the middle, do nothing */
        if (*suffix)
        {
            g_free(suffix);
            return FALSE;
        }
        g_free(suffix);
    }

    {
        gchar *prefix = gtk_editable_get_chars(editable, 0, *position);
        prefix_len = strlen(prefix);
        concatenated_text = g_strconcat(prefix, new_text, (gchar*) NULL);
        concatenated_text_len = prefix_len + new_text_length;
        g_free(prefix);
    }

    match = gnc_quickfill_get_string_match(qf, concatenated_text);
    g_free(concatenated_text);
    if (match)
    {
        const char* match_str = gnc_quickfill_string(match);
        if (match_str)
        {
            gint match_str_len = strlen(match_str);
            if (match_str_len > concatenated_text_len)
            {
                /* g_warning("Got match \"%s\" match_str_len=%d new_text=%s position=%d prefix_len=%d", match_str, match_str_len, new_text, *position, prefix_len); */

                g_signal_handlers_block_matched (G_OBJECT (editable),
                                                 G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, user_data);

                gtk_editable_insert_text(editable,
                                         match_str + prefix_len,
                                         match_str_len - prefix_len,
                                         position);

                g_signal_handlers_unblock_matched (G_OBJECT (editable),
                                                   G_SIGNAL_MATCH_DATA, 0, 0, NULL, NULL, user_data);

                /* stop the current insert */
                g_signal_stop_emission_by_name (G_OBJECT (editable), "insert_text");

                /* set the position */
                *position = concatenated_text_len;

                /* select region on idle, because it would be reset once this function
                   finishes */
                wdata->addrX_start_selection = *position;
                wdata->addrX_end_selection = -1;

                return TRUE;
            }
        }
    }
    return FALSE;
}

void gnc_coowner_addr2_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data)
{
    CoOwnerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_coowner_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr2_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        /* select region on idle, because it would be reset once this function
           finishes */
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_addr2,
                                           user_data);
    }
}

void gnc_coowner_addr3_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data)
{
    CoOwnerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_coowner_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr3_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_addr3,
                                           user_data);
    }
}

void gnc_coowner_addr4_insert_cb(GtkEditable *editable,
                                  gchar *new_text, gint new_text_length,
                                  gint *position, gpointer user_data)
{
    CoOwnerWindow *wdata = user_data;
    gboolean r;

    /* The handling common to all address lines is done in this other
     * function. */
    r = gnc_coowner_addr_common_insert_cb(editable, new_text, new_text_length,
                                           position, user_data, wdata->addr4_quickfill);

    /* Did we insert something? Then set up the correct idle handler */
    if (r)
    {
        wdata->addrX_selection_source_id = g_idle_add(idle_select_region_addr4,
                                           user_data);
    }
}
