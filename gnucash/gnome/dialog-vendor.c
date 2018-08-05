/*
 * dialog-vendor.c -- Dialog for Vendor entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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
#include "gnc-currency-edit.h"
#include "gnc-component-manager.h"
#include "gnc-ui.h"
#include "gnc-gui-query.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "dialog-search.h"
#include "search-param.h"

#include "gncAddress.h"
#include "gncVendor.h"
#include "gncVendorP.h"

#include "business-gnome-utils.h"
#include "dialog-vendor.h"
#include "dialog-job.h"
#include "dialog-order.h"
#include "dialog-invoice.h"
#include "dialog-payment.h"

#define DIALOG_NEW_VENDOR_CM_CLASS "dialog-new-vendor"
#define DIALOG_EDIT_VENDOR_CM_CLASS "dialog-edit-vendor"

#define GNC_PREFS_GROUP_SEARCH "dialogs.business.vendor-search"

void gnc_vendor_taxtable_check_cb (GtkToggleButton *togglebutton, gpointer user_data);
void gnc_vendor_window_ok_cb (GtkWidget *widget, gpointer data);
void gnc_vendor_window_cancel_cb (GtkWidget *widget, gpointer data);
void gnc_vendor_window_help_cb (GtkWidget *widget, gpointer data);
void gnc_vendor_window_destroy_cb (GtkWidget *widget, gpointer data);
void gnc_vendor_name_changed_cb (GtkWidget *widget, gpointer data);
void gnc_vendor_terms_changed_cb (GtkWidget *widget, gpointer data);
void gnc_vendor_taxincluded_changed_cb (GtkWidget *widget, gpointer data);
void gnc_vendor_taxtable_changed_cb (GtkWidget *widget, gpointer data);

typedef enum
{
    NEW_VENDOR,
    EDIT_VENDOR
} VendorDialogType;

struct _vendor_select_window
{
    QofBook  *book;
    QofQuery *q;
};

struct _vendor_window
{
    GtkWidget *	dialog;

    GtkWidget *	id_entry;
    GtkWidget *	company_entry;

    GtkWidget *	name_entry;
    GtkWidget *	addr1_entry;
    GtkWidget *	addr2_entry;
    GtkWidget *	addr3_entry;
    GtkWidget *	addr4_entry;
    GtkWidget *	phone_entry;
    GtkWidget *	fax_entry;
    GtkWidget *	email_entry;
    GtkWidget *	terms_menu;
    GtkWidget *	currency_edit;

    GtkWidget *	active_check;
    GtkWidget *	taxincluded_menu;
    GtkWidget *	notes_text;

    GtkWidget *	taxtable_check;
    GtkWidget *	taxtable_menu;

    GncTaxIncluded taxincluded;
    GncBillTerm *	terms;
    VendorDialogType	dialog_type;
    GncGUID		vendor_guid;
    gint		component_id;
    QofBook *	book;
    GncVendor *	created_vendor;

    GncTaxTable *	taxtable;
};

void
gnc_vendor_taxtable_check_cb (GtkToggleButton *togglebutton,
                              gpointer user_data)
{
    VendorWindow *vw = user_data;

    if (gtk_toggle_button_get_active (togglebutton))
        gtk_widget_set_sensitive (vw->taxtable_menu, TRUE);
    else
        gtk_widget_set_sensitive (vw->taxtable_menu, FALSE);
}

static GncVendor *
vw_get_vendor (VendorWindow *vw)
{
    if (!vw)
        return NULL;

    return gncVendorLookup (vw->book, &vw->vendor_guid);
}

static void gnc_ui_to_vendor (VendorWindow *vw, GncVendor *vendor)
{
    GtkTextBuffer* text_buffer;
    GtkTextIter start, end;
    gchar *text;
    GncAddress *addr;

    addr = gncVendorGetAddr (vendor);

    gnc_suspend_gui_refresh ();
    gncVendorBeginEdit (vendor);

    if (vw->dialog_type == NEW_VENDOR)
        qof_event_gen(QOF_INSTANCE(vendor), QOF_EVENT_ADD, NULL);

    gncVendorSetID (vendor, gtk_editable_get_chars
                    (GTK_EDITABLE (vw->id_entry), 0, -1));
    gncVendorSetName (vendor, gtk_editable_get_chars
                      (GTK_EDITABLE (vw->company_entry), 0, -1));

    gncAddressSetName (addr, gtk_editable_get_chars
                       (GTK_EDITABLE (vw->name_entry), 0, -1));
    gncAddressSetAddr1 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (vw->addr1_entry), 0, -1));
    gncAddressSetAddr2 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (vw->addr2_entry), 0, -1));
    gncAddressSetAddr3 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (vw->addr3_entry), 0, -1));
    gncAddressSetAddr4 (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (vw->addr4_entry), 0, -1));
    gncAddressSetPhone (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (vw->phone_entry), 0, -1));
    gncAddressSetFax (addr, gtk_editable_get_chars
                      (GTK_EDITABLE (vw->fax_entry), 0, -1));
    gncAddressSetEmail (addr, gtk_editable_get_chars
                        (GTK_EDITABLE (vw->email_entry), 0, -1));

    gncVendorSetActive (vendor, gtk_toggle_button_get_active
                        (GTK_TOGGLE_BUTTON (vw->active_check)));
    gncVendorSetTaxIncluded (vendor, vw->taxincluded);

    text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(vw->notes_text));
    gtk_text_buffer_get_bounds (text_buffer, &start, &end);
    text = gtk_text_buffer_get_text (text_buffer, &start, &end, FALSE);
    gncVendorSetNotes (vendor, text);

    gncVendorSetTerms (vendor, vw->terms);
    gncVendorSetCurrency (vendor,
                          gnc_currency_edit_get_currency (GNC_CURRENCY_EDIT
                                  (vw->currency_edit)));

    gncVendorSetTaxTableOverride
    (vendor, gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (vw->taxtable_check)));
    gncVendorSetTaxTable (vendor, vw->taxtable);

    gncVendorCommitEdit (vendor);
    gnc_resume_gui_refresh ();
}

static gboolean check_entry_nonempty (GtkWidget *entry,
                                      const char * error_message)
{
    const char *res = gtk_entry_get_text (GTK_ENTRY (entry));
    if (g_strcmp0 (res, "") == 0)
    {
        if (error_message)
            gnc_error_dialog (gnc_ui_get_gtk_window (entry), "%s", error_message);
        return TRUE;
    }
    return FALSE;
}

void
gnc_vendor_window_ok_cb (GtkWidget *widget, gpointer data)
{
    VendorWindow *vw = data;
    gchar *string;

    /* Check for valid company name */
    if (check_entry_nonempty (vw->company_entry,
                              _("You must enter a company name. "
                                "If this vendor is an individual (and not a company) "
                                "you should enter the same value for:\nIdentification "
                                "- Company Name, and\nPayment Address - Name.")))
        return;

    /* Make sure we have an address */
    if (check_entry_nonempty (vw->addr1_entry, NULL) &&
            check_entry_nonempty (vw->addr2_entry, NULL) &&
            check_entry_nonempty (vw->addr3_entry, NULL) &&
            check_entry_nonempty (vw->addr4_entry, NULL))
    {
        const char *msg = _("You must enter a payment address.");
        gnc_error_dialog (gnc_ui_get_gtk_window (widget), "%s", msg);
        return;
    }

    /* Check for valid id and set one if necessary */
    if (g_strcmp0 (gtk_entry_get_text (GTK_ENTRY (vw->id_entry)), "") == 0)
    {
        string = gncVendorNextID(vw->book);
        gtk_entry_set_text (GTK_ENTRY (vw->id_entry), string);
        g_free(string);
    }

    /* Now save it off */
    {
        GncVendor *vendor = vw_get_vendor (vw);
        if (vendor)
        {
            gnc_ui_to_vendor (vw, vendor);
        }
        vw->created_vendor = vendor;
        vw->vendor_guid = *guid_null ();
    }

    gnc_close_gui_component (vw->component_id);
}

void
gnc_vendor_window_cancel_cb (GtkWidget *widget, gpointer data)
{
    VendorWindow *vw = data;

    gnc_close_gui_component (vw->component_id);
}

void
gnc_vendor_window_help_cb (GtkWidget *widget, gpointer data)
{
    gnc_gnome_help(HF_HELP, HL_USAGE_VENDOR);
}

void
gnc_vendor_window_destroy_cb (GtkWidget *widget, gpointer data)
{
    VendorWindow *vw = data;
    GncVendor *vendor = vw_get_vendor (vw);

    gnc_suspend_gui_refresh ();

    if (vw->dialog_type == NEW_VENDOR && vendor != NULL)
    {
        gncVendorBeginEdit (vendor);
        gncVendorDestroy (vendor);
        vw->vendor_guid = *guid_null ();
    }

    gnc_unregister_gui_component (vw->component_id);
    gnc_resume_gui_refresh ();

    g_free (vw);
}

void
gnc_vendor_name_changed_cb (GtkWidget *widget, gpointer data)
{
    VendorWindow *vw = data;
    char *name, *id, *fullname, *title;

    if (!vw)
        return;

    name = gtk_editable_get_chars (GTK_EDITABLE (vw->company_entry), 0, -1);
    if (!name || *name == '\0')
        name = g_strdup (_("<No name>"));

    id = gtk_editable_get_chars (GTK_EDITABLE (vw->id_entry), 0, -1);

    fullname = g_strconcat (name, " (", id, ")", (char *)NULL);

    if (vw->dialog_type == EDIT_VENDOR)
        title = g_strconcat (_("Edit Vendor"), " - ", fullname, (char *)NULL);
    else
        title = g_strconcat (_("New Vendor"), " - ", fullname, (char *)NULL);

    gtk_window_set_title (GTK_WINDOW (vw->dialog), title);

    g_free (name);
    g_free (id);
    g_free (fullname);
    g_free (title);
}

void
gnc_vendor_terms_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    VendorWindow *vw = data;

    if (!vw) return;
    if (!cbox) return;

    vw->terms = gnc_simple_combo_get_value (cbox);
}

void
gnc_vendor_taxincluded_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    VendorWindow *vw = data;

    if (!vw) return;
    if (!cbox) return;

    vw->taxincluded = GPOINTER_TO_INT (gnc_simple_combo_get_value (cbox));
}

void
gnc_vendor_taxtable_changed_cb (GtkWidget *widget, gpointer data)
{
    GtkComboBox *cbox = GTK_COMBO_BOX (widget);
    VendorWindow *vw = data;

    if (!vw) return;
    if (!cbox) return;

    vw->taxtable = gnc_simple_combo_get_value (cbox);
}

static void
gnc_vendor_window_close_handler (gpointer user_data)
{
    VendorWindow *vw = user_data;

    gtk_widget_destroy (vw->dialog);
}

static void
gnc_vendor_window_refresh_handler (GHashTable *changes, gpointer user_data)
{
    VendorWindow *vw = user_data;
    const EventInfo *info;
    GncVendor *vendor = vw_get_vendor (vw);

    /* If there isn't a vendor behind us, close down */
    if (!vendor)
    {
        gnc_close_gui_component (vw->component_id);
        return;
    }

    /* Next, close if this is a destroy event */
    if (changes)
    {
        info = gnc_gui_get_entity_events (changes, &vw->vendor_guid);
        if (info && (info->event_mask & QOF_EVENT_DESTROY))
        {
            gnc_close_gui_component (vw->component_id);
            return;
        }
    }
}

static gboolean
find_handler (gpointer find_data, gpointer user_data)
{
    const GncGUID *vendor_guid = find_data;
    VendorWindow *vw = user_data;

    return(vw && guid_equal(&vw->vendor_guid, vendor_guid));
}

static VendorWindow *
gnc_vendor_new_window (GtkWindow *parent, QofBook *bookp, GncVendor *vendor)
{
    VendorWindow *vw;
    GtkBuilder *builder;
    GtkWidget *edit, *hbox;
    gnc_commodity *currency;

    /*
     * Find an existing window for this vendor.  If found, bring it to
     * the front.
     */
    if (vendor)
    {
        GncGUID vendor_guid;

        vendor_guid = *gncVendorGetGUID (vendor);
        vw = gnc_find_first_gui_component (DIALOG_EDIT_VENDOR_CM_CLASS,
                                           find_handler, &vendor_guid);
        if (vw)
        {
            gtk_window_set_transient_for (GTK_WINDOW(vw->dialog), parent);
            gtk_window_present (GTK_WINDOW(vw->dialog));
            return(vw);
        }
    }

    /* Find the default currency */
    if (vendor)
        currency = gncVendorGetCurrency (vendor);
    else
        currency = gnc_default_currency ();

    /*
     * No existing vendor window found.  Build a new one.
     */
    vw = g_new0 (VendorWindow, 1);

    vw->book = bookp;

    /* Find the dialog */
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-vendor.glade", "terms_store");
    gnc_builder_add_from_file (builder, "dialog-vendor.glade", "tax_included_store");
    gnc_builder_add_from_file (builder, "dialog-vendor.glade", "taxtable_store");
    gnc_builder_add_from_file (builder, "dialog-vendor.glade", "vendor_dialog");
    vw->dialog = GTK_WIDGET (gtk_builder_get_object (builder, "vendor_dialog"));
    gtk_window_set_transient_for (GTK_WINDOW(vw->dialog), parent);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(vw->dialog), "GncVendorDialog");

    /* Get entry points */
    vw->id_entry = GTK_WIDGET (gtk_builder_get_object (builder, "id_entry"));
    vw->company_entry = GTK_WIDGET (gtk_builder_get_object (builder, "company_entry"));

    vw->name_entry = GTK_WIDGET (gtk_builder_get_object (builder, "name_entry"));
    vw->addr1_entry = GTK_WIDGET (gtk_builder_get_object (builder, "addr1_entry"));
    vw->addr2_entry = GTK_WIDGET (gtk_builder_get_object (builder, "addr2_entry"));
    vw->addr3_entry = GTK_WIDGET (gtk_builder_get_object (builder, "addr3_entry"));
    vw->addr4_entry = GTK_WIDGET (gtk_builder_get_object (builder, "addr4_entry"));
    vw->phone_entry = GTK_WIDGET (gtk_builder_get_object (builder, "phone_entry"));
    vw->fax_entry = GTK_WIDGET (gtk_builder_get_object (builder, "fax_entry"));
    vw->email_entry = GTK_WIDGET (gtk_builder_get_object (builder, "email_entry"));

    vw->active_check = GTK_WIDGET (gtk_builder_get_object (builder, "active_check"));
    vw->taxincluded_menu = GTK_WIDGET (gtk_builder_get_object (builder, "tax_included_menu"));
    vw->notes_text = GTK_WIDGET (gtk_builder_get_object (builder, "notes_text"));
    vw->terms_menu = GTK_WIDGET (gtk_builder_get_object (builder, "terms_menu"));

    vw->taxtable_check = GTK_WIDGET (gtk_builder_get_object (builder, "taxtable_button"));
    vw->taxtable_menu = GTK_WIDGET (gtk_builder_get_object (builder, "taxtable_menu"));

    /* Currency */
    edit = gnc_currency_edit_new();
    gnc_currency_edit_set_currency (GNC_CURRENCY_EDIT(edit), currency);
    vw->currency_edit = edit;

    hbox = GTK_WIDGET (gtk_builder_get_object (builder, "currency_box"));
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);

    /* Setup signals */
    gtk_builder_connect_signals_full( builder,
                                      gnc_builder_connect_full_func,
                                      vw);

    /* Setup initial values */
    if (vendor != NULL)
    {
        GtkTextBuffer* text_buffer;
        GncAddress *addr;
        const char *string;

        vw->dialog_type = EDIT_VENDOR;
        vw->vendor_guid = *gncVendorGetGUID (vendor);

        addr = gncVendorGetAddr (vendor);

        gtk_entry_set_text (GTK_ENTRY (vw->id_entry), gncVendorGetID (vendor));
        gtk_entry_set_text (GTK_ENTRY (vw->company_entry), gncVendorGetName (vendor));

        /* Setup Address */
        gtk_entry_set_text (GTK_ENTRY (vw->name_entry), gncAddressGetName (addr));
        gtk_entry_set_text (GTK_ENTRY (vw->addr1_entry), gncAddressGetAddr1 (addr));
        gtk_entry_set_text (GTK_ENTRY (vw->addr2_entry), gncAddressGetAddr2 (addr));
        gtk_entry_set_text (GTK_ENTRY (vw->addr3_entry), gncAddressGetAddr3 (addr));
        gtk_entry_set_text (GTK_ENTRY (vw->addr4_entry), gncAddressGetAddr4 (addr));
        gtk_entry_set_text (GTK_ENTRY (vw->phone_entry), gncAddressGetPhone (addr));
        gtk_entry_set_text (GTK_ENTRY (vw->fax_entry), gncAddressGetFax (addr));
        gtk_entry_set_text (GTK_ENTRY (vw->email_entry), gncAddressGetEmail (addr));

        /* Set toggle buttons */
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (vw->active_check),
                                      gncVendorGetActive (vendor));

        string = gncVendorGetNotes (vendor);
        text_buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(vw->notes_text));
        gtk_text_buffer_set_text (text_buffer, string, -1);

        vw->component_id =
            gnc_register_gui_component (DIALOG_EDIT_VENDOR_CM_CLASS,
                                        gnc_vendor_window_refresh_handler,
                                        gnc_vendor_window_close_handler,
                                        vw);

        vw->terms = gncVendorGetTerms (vendor);

    }
    else
    {
        vendor = gncVendorCreate (bookp);
        vw->vendor_guid = *gncVendorGetGUID (vendor);

        vw->dialog_type = NEW_VENDOR;
        vw->component_id =
            gnc_register_gui_component (DIALOG_NEW_VENDOR_CM_CLASS,
                                        gnc_vendor_window_refresh_handler,
                                        gnc_vendor_window_close_handler,
                                        vw);

        /* XXX: Get the default Billing Terms */
        vw->terms = NULL;
    }

    /* I know that vendor exists here -- either passed in or just created */

    vw->taxincluded = gncVendorGetTaxIncluded (vendor);
    gnc_taxincluded_combo (GTK_COMBO_BOX(vw->taxincluded_menu), vw->taxincluded);
    gnc_billterms_combo (GTK_COMBO_BOX(vw->terms_menu), bookp, TRUE, vw->terms);

    vw->taxtable = gncVendorGetTaxTable (vendor);
    gnc_taxtables_combo (GTK_COMBO_BOX(vw->taxtable_menu), bookp, TRUE, vw->taxtable);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (vw->taxtable_check),
                                  gncVendorGetTaxTableOverride (vendor));
    gnc_vendor_taxtable_check_cb (GTK_TOGGLE_BUTTON (vw->taxtable_check), vw);

    gnc_gui_component_watch_entity_type (vw->component_id,
                                         GNC_VENDOR_MODULE_NAME,
                                         QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

    gtk_widget_show_all (vw->dialog);
    g_object_unref(G_OBJECT(builder));

    return vw;
}

VendorWindow *
gnc_ui_vendor_new (GtkWindow *parent, QofBook *bookp)
{
    VendorWindow *vw;

    /* Make sure required options exist */
    if (!bookp) return NULL;

    vw = gnc_vendor_new_window (parent, bookp, NULL);
    return vw;
}

VendorWindow *
gnc_ui_vendor_edit (GtkWindow *parent, GncVendor *vendor)
{
    VendorWindow *vw;

    if (!vendor) return NULL;

    vw = gnc_vendor_new_window (parent, gncVendorGetBook(vendor), vendor);

    return vw;
}

/* Functions for vendor selection widgets */

static void
invoice_vendor_cb (GtkWindow *dialog, gpointer *vendor_p, gpointer user_data)
{
    struct _vendor_select_window *sw = user_data;
    GncOwner owner;
    GncVendor *vendor;

    g_return_if_fail (vendor_p && user_data);

    vendor = *vendor_p;

    if (!vendor)
        return;

    gncOwnerInitVendor (&owner, vendor);
    gnc_invoice_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
order_vendor_cb (GtkWindow *dialog, gpointer *vendor_p, gpointer user_data)
{
    struct _vendor_select_window *sw = user_data;
    GncOwner owner;
    GncVendor *vendor;

    g_return_if_fail (vendor_p && user_data);

    vendor = *vendor_p;

    if (!vendor)
        return;

    gncOwnerInitVendor (&owner, vendor);
    gnc_order_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
jobs_vendor_cb (GtkWindow *dialog, gpointer *vendor_p, gpointer user_data)
{
    struct _vendor_select_window *sw = user_data;
    GncOwner owner;
    GncVendor *vendor;

    g_return_if_fail (vendor_p && user_data);

    vendor = *vendor_p;

    if (!vendor)
        return;

    gncOwnerInitVendor (&owner, vendor);
    gnc_job_search (dialog, NULL, &owner, sw->book);
    return;
}

static void
payment_vendor_cb (GtkWindow *dialog, gpointer *vendor_p, gpointer user_data)
{
    struct _vendor_select_window *sw = user_data;
    GncOwner owner;
    GncVendor *vendor;

    g_return_if_fail (vendor_p && user_data);

    vendor = *vendor_p;

    if (!vendor)
        return;

    gncOwnerInitVendor (&owner, vendor);
    gnc_ui_payment_new (dialog, &owner, sw->book);
    return;
}

static void
edit_vendor_cb (GtkWindow *dialog, gpointer *vendor_p, gpointer user_data)
{
    GncVendor *vendor;

    g_return_if_fail (vendor_p && user_data);

    vendor = *vendor_p;

    if (!vendor)
        return;

    gnc_ui_vendor_edit (dialog, vendor);
    return;
}

static gpointer
new_vendor_cb (GtkWindow *dialog, gpointer user_data)
{
    struct _vendor_select_window *sw = user_data;
    VendorWindow *vw;

    g_return_val_if_fail (user_data, NULL);

    vw = gnc_ui_vendor_new (dialog, sw->book);
    return vw_get_vendor (vw);
}

static void
free_vendor_cb (gpointer user_data)
{
    struct _vendor_select_window *sw = user_data;
    g_return_if_fail (sw);

    qof_query_destroy (sw->q);
    g_free (sw);
}

GNCSearchWindow *
gnc_vendor_search (GtkWindow *parent, GncVendor *start, QofBook *book)
{
    QofIdType type = GNC_VENDOR_MODULE_NAME;
    struct _vendor_select_window *sw;
    QofQuery *q, *q2 = NULL;
    static GList *params = NULL;
    static GList *columns = NULL;
    static GNCSearchCallbackButton buttons[] =
    {
        { N_("View/Edit Vendor"), edit_vendor_cb, NULL, TRUE},
        { N_("Vendor's Jobs"), jobs_vendor_cb, NULL, TRUE},
        //    { N_("Vendor Orders"), order_vendor_cb, NULL, TRUE},
        { N_("Vendor's Bills"), invoice_vendor_cb, NULL, TRUE},
        { N_("Pay Bill"), payment_vendor_cb, NULL, FALSE},
        { NULL },
    };
    (void)order_vendor_cb;

    g_return_val_if_fail (book, NULL);

    /* Build parameter list in reverse order */
    if (params == NULL)
    {
        params = gnc_search_param_prepend (params, _("Billing Contact"), NULL, type,
                                           VENDOR_ADDR, ADDRESS_NAME, NULL);
        params = gnc_search_param_prepend (params, _("Vendor ID"), NULL, type,
                                           VENDOR_ID, NULL);
        params = gnc_search_param_prepend (params, _("Company Name"), NULL, type,
                                           VENDOR_NAME, NULL);
    }

    /* Build the column list in reverse order */
    if (columns == NULL)
    {
        columns = gnc_search_param_prepend (columns, _("Contact"), NULL, type,
                                            VENDOR_ADDR, ADDRESS_NAME, NULL);
        columns = gnc_search_param_prepend (columns, _("Company"), NULL, type,
                                            VENDOR_NAME, NULL);
        columns = gnc_search_param_prepend (columns, _("ID #"), NULL, type,
                                            VENDOR_ID, NULL);
    }

    /* Build the queries */
    q = qof_query_create_for (type);
    qof_query_set_book (q, book);

#if 0
    if (start)
    {
        q2 = qof_query_copy (q);
        qof_query_add_guid_match (q2, g_slist_prepend (NULL, QOF_PARAM_GUID),
                                  gncVendorGetGUID (start), QOF_QUERY_AND);
    }
#endif

    /* launch select dialog and return the result */
    sw = g_new0 (struct _vendor_select_window, 1);
    sw->book = book;
    sw->q = q;

    return gnc_search_dialog_create (parent, type, _("Find Vendor"),
                                     params, columns, q, q2, buttons, NULL,
                                     new_vendor_cb, sw, free_vendor_cb,
                                     GNC_PREFS_GROUP_SEARCH, NULL,
                                     "GncFindVendorDialog");
}

GNCSearchWindow *
gnc_vendor_search_select (GtkWindow *parent, gpointer start, gpointer book)
{
    if (!book) return NULL;

    return gnc_vendor_search (parent, start, book);
}

GNCSearchWindow *
gnc_vendor_search_edit (GtkWindow *parent, gpointer start, gpointer book)
{
    if (start)
        gnc_ui_vendor_edit (parent, start);

    return NULL;
}
