/*
 * business-gnome-utils.c -- General GUI Utilities for GNC Business Objects
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2001,2002,2006 Derek Atkins
 * Copyright (c) 2006 David Hampton <hampton@employees.org>
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

#include "Account.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "gnc-component-manager.h"
#include "gnc-gtk-utils.h"

#include "gncCustomer.h"
#include "gncJob.h"
#include "gncVendor.h"
#include "gncOwner.h"
#include "gncInvoice.h"

#include "gnc-general-search.h"
#include "qof.h"
#include "business-gnome-utils.h"
#include "dialog-customer.h"
#include "dialog-job.h"
#include "dialog-vendor.h"
#include "dialog-employee.h"
#include "dialog-invoice.h"

#include "gnc-commodity.h"

typedef enum
{
    GNCSEARCH_TYPE_SELECT,
    GNCSEARCH_TYPE_EDIT
} GNCSearchType;

static GtkWidget * gnc_owner_new (GtkWidget *label, GtkWidget *hbox,
                                  QofBook *book, GncOwner *owner,
                                  GNCSearchType type)
{
    GtkWidget *edit;
    GNCSearchCB search_cb = NULL;
    const char *type_name = NULL;
    const char *text = NULL;
    gboolean text_editable = FALSE;

    switch (type)
    {
    case GNCSEARCH_TYPE_SELECT:
        text = _("Select...");
        text_editable = TRUE;
        break;
    case GNCSEARCH_TYPE_EDIT:
        text = _("Edit...");
        text_editable = FALSE;
        break;
    };

    switch (owner->type)
    {
    case GNC_OWNER_NONE:
    case GNC_OWNER_UNDEFINED:
        return NULL;

    case GNC_OWNER_CUSTOMER:
        if (type == GNCSEARCH_TYPE_SELECT)
            search_cb = gnc_customer_search_select;
        else
            search_cb = gnc_customer_search_edit;
        type_name = GNC_CUSTOMER_MODULE_NAME;
        break;

    case GNC_OWNER_JOB:
        if (type == GNCSEARCH_TYPE_SELECT)
            search_cb = gnc_job_search_select;
        else
            search_cb = gnc_job_search_edit;
        type_name = GNC_JOB_MODULE_NAME;
        break;

    case GNC_OWNER_VENDOR:
        if (type == GNCSEARCH_TYPE_SELECT)
            search_cb = gnc_vendor_search_select;
        else
            search_cb = gnc_vendor_search_edit;
        type_name = GNC_VENDOR_MODULE_NAME;
        break;

    case GNC_OWNER_EMPLOYEE:
        if (type == GNCSEARCH_TYPE_SELECT)
            search_cb = gnc_employee_search_select;
        else
            search_cb = gnc_employee_search_edit;
        type_name = GNC_EMPLOYEE_MODULE_NAME;
        break;

    default:
        g_warning ("Unknown type");
        return NULL;
    }

    edit = gnc_general_search_new (type_name, text, text_editable, search_cb, book, book);
    if (!edit)
        return NULL;

    gnc_general_search_set_selected (GNC_GENERAL_SEARCH (edit),
                                     owner->owner.undefined);
    gtk_box_pack_start (GTK_BOX (hbox), edit, TRUE, TRUE, 0);
    if (label)
        gtk_label_set_text (GTK_LABEL (label), _(qof_object_get_type_label (type_name)));

    return edit;
}

GtkWidget * gnc_owner_select_create (GtkWidget *label, GtkWidget *hbox,
                                     QofBook *book, GncOwner *owner)
{
    g_return_val_if_fail (hbox != NULL, NULL);
    g_return_val_if_fail (book != NULL, NULL);
    g_return_val_if_fail (owner != NULL, NULL);

    return gnc_owner_new (label, hbox, book, owner, GNCSEARCH_TYPE_SELECT);
}

GtkWidget * gnc_owner_edit_create (GtkWidget *label, GtkWidget *hbox,
                                   QofBook *book, GncOwner *owner)
{
    g_return_val_if_fail (hbox != NULL, NULL);
    g_return_val_if_fail (book != NULL, NULL);
    g_return_val_if_fail (owner != NULL, NULL);

    return gnc_owner_new (label, hbox, book, owner, GNCSEARCH_TYPE_EDIT);
}

void gnc_owner_get_owner (GtkWidget *widget, GncOwner *owner)
{
    g_return_if_fail (widget != NULL);
    g_return_if_fail (owner != NULL);

    /* We'll assume that the owner has the proper 'type' because we
     * can't change it here.  Hopefully the caller has it set properly
     */
    owner->owner.undefined =
        gnc_general_search_get_selected (GNC_GENERAL_SEARCH (widget));
}

void gnc_owner_set_owner (GtkWidget *widget, GncOwner *owner)
{
    g_return_if_fail (widget != NULL);
    g_return_if_fail (owner != NULL);

    /* We'll assume that the owner has the proper 'type' because we
     * can't change it here.  Hopefully the caller has it set properly
     */

    gnc_general_search_set_selected (GNC_GENERAL_SEARCH (widget),
                                     owner->owner.undefined);
}

typedef struct _invoice_select_info
{
    GtkWidget *label;
    QofBook *book;
    GncOwner owner;
    gboolean have_owner;
} GncISI;

static GNCSearchWindow *
gnc_invoice_select_search_cb (GtkWindow *parent, gpointer start, gpointer isip)
{
    GncISI *isi = isip;

    if (!isi) return NULL;
    g_assert(isi->book);

    return gnc_invoice_search (parent, start,
                               isi->have_owner ? &isi->owner : NULL,
                               isi->book);
}

static void
gnc_invoice_select_search_set_label(GncISI* isi)
{
    GncOwnerType owner_type;
    char *label;

    g_assert(isi);
    if (!isi->label) return;

    owner_type = gncOwnerGetType(gncOwnerGetEndOwner(&isi->owner));

    /* Translators:  See comments in dialog-invoice.c:gnc_invoice_search() */
    switch (owner_type)
    {
    case GNC_OWNER_VENDOR:
        label = _("Bill");
        break;
    case GNC_OWNER_EMPLOYEE:
        label = _("Voucher");
        break;
    default:
        label = _("Invoice");
        break;
    }

    gtk_label_set_text(GTK_LABEL(isi->label), label);
}

GtkWidget * gnc_invoice_select_create (GtkWidget *hbox, QofBook *book,
                                       const GncOwner *owner,
                                       GncInvoice *invoice,
                                       GtkWidget *label)
{
    GtkWidget *edit;
    GncISI *isi;

    g_return_val_if_fail (hbox != NULL, NULL);
    g_return_val_if_fail (book != NULL, NULL);
    /* Note: it is legal to have no owner or invoice */

    isi = g_new0(GncISI, 1);
    if (!isi)
        return NULL;

    if (owner)
    {
        gncOwnerCopy(owner, &isi->owner);
        isi->have_owner = TRUE;
    }
    else
    {
        gncOwnerInitCustomer(&isi->owner, NULL);
    }
    isi->book = book;
    isi->label = label;

    edit = gnc_general_search_new (GNC_INVOICE_MODULE_NAME, _("Select..."),
                                   TRUE, gnc_invoice_select_search_cb, isi, isi->book);
    if (!edit)
    {
        g_free(isi);
        return NULL;
    }

    gnc_general_search_set_selected (GNC_GENERAL_SEARCH (edit), invoice);
    gtk_box_pack_start (GTK_BOX (hbox), edit, FALSE, FALSE, 0);
    g_object_set_data_full(G_OBJECT(edit), "isi-state", isi, g_free);

    /* Set the label */
    gnc_invoice_select_search_set_label(isi);

    return edit;
}

GncInvoice * gnc_invoice_get_invoice (GtkWidget *widget)
{
    g_return_val_if_fail (widget != NULL, NULL);

    return gnc_general_search_get_selected (GNC_GENERAL_SEARCH (widget));
}

void gnc_invoice_set_invoice (GtkWidget *widget, GncInvoice *invoice)
{
    g_return_if_fail (widget != NULL);
    g_return_if_fail (invoice != NULL);

    gnc_general_search_set_selected (GNC_GENERAL_SEARCH (widget), invoice);
}

void gnc_invoice_set_owner (GtkWidget *widget, GncOwner *owner)
{
    GncISI *isi;

    g_return_if_fail (widget != NULL);
    g_return_if_fail (owner != NULL);

    isi = g_object_get_data(G_OBJECT(widget), "isi-state");
    g_assert(isi);

    if (isi->owner.owner.undefined == owner->owner.undefined)
        return;

    gncOwnerCopy(owner, &isi->owner);
    isi->have_owner = TRUE;
    gnc_general_search_set_selected(GNC_GENERAL_SEARCH(widget), NULL);

    /* Reset the label */
    gnc_invoice_select_search_set_label(isi);
}

Account *
gnc_account_select_combo_fill (GtkWidget *combo, QofBook *book,
                               GList *acct_types, GList *acct_commodities)
{
    GtkListStore *store;
    GtkTreeIter iter;
    GList *list, *node;
    const gchar *text;

    g_return_val_if_fail (combo && GTK_IS_COMBO_BOX(combo), NULL);
    g_return_val_if_fail (book, NULL);
    g_return_val_if_fail (acct_types, NULL);

    /* Figure out if anything is set in the combo */
    text = gtk_entry_get_text(GTK_ENTRY (gtk_bin_get_child(GTK_BIN (GTK_COMBO_BOX(combo)))));

    g_object_set_data (G_OBJECT(combo), "book", book);
    list = gnc_account_get_descendants (gnc_book_get_root_account (book));

    /* Clear the existing list */
    store = GTK_LIST_STORE(gtk_combo_box_get_model(GTK_COMBO_BOX(combo)));
    gtk_list_store_clear(store);

    /* Add the account names to the combo box */
    for (node = list; node; node = node->next)
    {
        Account *account = node->data;
        char *name;

        /* Only present accounts of the appropriate type */
        if (g_list_index (acct_types, (gpointer)xaccAccountGetType (account))
                == -1)
            continue;

        /* Only present accounts with the right commodity, if that's a
           restriction */
        if (acct_commodities)
        {
            if ( g_list_find_custom( acct_commodities,
                                     GINT_TO_POINTER(xaccAccountGetCommodity(account)),
                                     gnc_commodity_compare_void) == NULL )
            {
                continue;
            }
        }

        name = gnc_account_get_full_name (account);
        gtk_list_store_append(store, &iter);
        gtk_list_store_set (store, &iter, 0, name, -1);

        /* Save the first account name in case no account name was set */
        if (!text || g_strcmp0 (text, "") == 0)
        {
            text = g_strdup (name);
        }
        g_free(name);
    }
    gtk_combo_box_set_active(GTK_COMBO_BOX(combo), 0);

    g_list_free (list);

    gnc_cbwe_set_by_string(GTK_COMBO_BOX(combo), text);

    return gnc_account_select_combo_get_active (combo);
}

Account *
gnc_account_select_combo_get_active (GtkWidget *combo)
{
    const gchar *text;
    QofBook *book;

    if (!combo || !GTK_IS_COMBO_BOX(combo))
        return NULL;

    book = g_object_get_data (G_OBJECT(combo), "book");
    if (!book)
        return NULL;

    text = gtk_entry_get_text( GTK_ENTRY( gtk_bin_get_child( GTK_BIN( GTK_COMBO_BOX(combo)))));

    if (!text || g_strcmp0 (text, "") == 0)
        return NULL;

    return gnc_account_lookup_by_full_name (gnc_book_get_root_account (book), text);
}

/***********************************************************************
 * gnc_simple_combo implementation functions
 */

typedef const char * (*GenericLookup_t)(gpointer);
typedef gboolean (*GenericEqual_t)(gpointer, gpointer);

typedef struct
{
    gint         component_id;
    GtkComboBox  *cbox;
    QofBook      *book;
    gboolean     none_ok;
    const char * (*get_name)(gpointer);
    GList *      (*get_list)(QofBook*);
    gboolean     (*is_equal)(gpointer, gpointer);

} ListStoreData;

static void
gnc_simple_combo_add_item (GtkListStore *liststore, const char *label, gpointer this_item)
{
    GtkTreeIter iter;

    gtk_list_store_append (liststore, &iter);
    gtk_list_store_set (liststore, &iter, 0, label, 1, this_item, -1);
}

static void
gnc_simple_combo_generate_liststore (ListStoreData *lsd)
{
    GList *items;
    GtkListStore *liststore;

    if (!(lsd->get_list))
        return;
    if (!(lsd->get_name))
        return;

    /* Get the list of items */
    items = (lsd->get_list)(lsd->book);

    /* Reset the combobox' liststore */
    liststore = GTK_LIST_STORE (gtk_combo_box_get_model (lsd->cbox));
    gtk_list_store_clear (liststore);

    if (lsd->none_ok || !items)
        gnc_simple_combo_add_item (liststore, _("None"), NULL);

    for ( ; items; items = items->next)
        gnc_simple_combo_add_item (liststore, (lsd->get_name)(items->data), items->data);
}

static void
gnc_simple_combo_refresh_handler (GHashTable *changes, gpointer user_data)
{
    ListStoreData *lsd = user_data;
    gnc_simple_combo_generate_liststore (lsd);
}

static void
gnc_simple_combo_destroy_cb (GtkWidget *widget, gpointer data)
{
    ListStoreData *lsd = data;

    gnc_unregister_gui_component (lsd->component_id);
    g_free (lsd);
}

static void
gnc_simple_combo_make (GtkComboBox *cbox, QofBook *book,
                       gboolean none_ok, QofIdType type_name,
                       GList * (*get_list)(QofBook*),
                       GenericLookup_t get_name,
                       GenericEqual_t is_equal,
                       gpointer initial_choice)
{
    ListStoreData *lsd;

    lsd = g_object_get_data (G_OBJECT (cbox), "liststore-data");

    /* If this is the first time we've been called, then build the
     * Option Menu Data object, register with the component manager, and
     * watch for changed items.  Then register for deletion, so we can
     * unregister and free the data when this menu is destroyed.
     */
    if (!lsd)
    {

        lsd = g_new0 (ListStoreData, 1);
        lsd->cbox = cbox;
        lsd->book = book;
        lsd->none_ok = none_ok;
        lsd->get_name = get_name;
        lsd->get_list = get_list;
        lsd->is_equal = is_equal;
        g_object_set_data (G_OBJECT (cbox), "liststore-data", lsd);

        lsd->component_id =
            gnc_register_gui_component ("gnc-simple-combo-refresh-hook",
                                        gnc_simple_combo_refresh_handler,
                                        NULL, lsd);

        if (type_name)
            gnc_gui_component_watch_entity_type (lsd->component_id,
                                                 type_name,
                                                 QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);

        g_signal_connect (G_OBJECT (cbox), "destroy",
                          G_CALLBACK (gnc_simple_combo_destroy_cb), lsd);
    }

    gnc_simple_combo_generate_liststore (lsd);
    gnc_simple_combo_set_value (cbox, initial_choice);
}

/***********************************************************
 * Specific invocations of the gnc_simple_combo widget
 */

/* Use a list available billing terms to fill the model of
 * the combobox passed in.  If none_ok is true, then add "none" as a
 * choice (with data set to NULL)..  If initial_choice is non-NULL,
 * then that will be the default option setting when the menu is
 * created.
 */
void
gnc_billterms_combo (GtkComboBox *cbox, QofBook *book,
                     gboolean none_ok, GncBillTerm *initial_choice)
{
    if (!cbox || !book) return;

    gnc_simple_combo_make (cbox, book, none_ok, GNC_BILLTERM_MODULE_NAME,
                           gncBillTermGetTerms,
                           (GenericLookup_t)gncBillTermGetName,
                           (GenericEqual_t)gncBillTermIsFamily,
                           (gpointer)initial_choice);
}

void
gnc_taxtables_combo (GtkComboBox *cbox, QofBook *book,
                     gboolean none_ok, GncTaxTable *initial_choice)
{
    if (!cbox || !book) return;

    gnc_simple_combo_make (cbox, book, none_ok, GNC_TAXTABLE_MODULE_NAME,
                           gncTaxTableGetTables,
                           (GenericLookup_t)gncTaxTableGetName,
                           NULL,
                           (gpointer)initial_choice);
}

void
gnc_taxincluded_combo (GtkComboBox *cbox, GncTaxIncluded initial_choice)
{
    GtkListStore *liststore;

    if (!cbox) return;

    gnc_simple_combo_make (cbox, NULL, FALSE, NULL, NULL, NULL, NULL,
                           GINT_TO_POINTER(initial_choice));
    liststore = GTK_LIST_STORE (gtk_combo_box_get_model (cbox));

    gnc_simple_combo_add_item (liststore, _("Yes"),
                               GINT_TO_POINTER (GNC_TAXINCLUDED_YES));
    gnc_simple_combo_add_item (liststore, _("No"),
                               GINT_TO_POINTER (GNC_TAXINCLUDED_NO));
    gnc_simple_combo_add_item (liststore, _("Use Global"),
                               GINT_TO_POINTER (GNC_TAXINCLUDED_USEGLOBAL));

    gnc_simple_combo_set_value (cbox, GINT_TO_POINTER(initial_choice));
}

/* Convenience functions for the above simple combo box types.  */

/** Get the value of the item that is currently selected in the combo box */
gpointer
gnc_simple_combo_get_value (GtkComboBox *cbox)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    GValue value = { 0 };

    if (!cbox) return NULL;

    model = gtk_combo_box_get_model (cbox);
    if (!gtk_combo_box_get_active_iter (cbox, &iter))
        return NULL;
    gtk_tree_model_get_value (model, &iter, 1, &value);
    return g_value_get_pointer (&value);
}

/** Find the item in the combo box whose value is "data"
 *  and make it the active item. */
void
gnc_simple_combo_set_value (GtkComboBox *cbox, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    gboolean valid_iter;
    ListStoreData *lsd = g_object_get_data (G_OBJECT (cbox), "liststore-data");

    if (!cbox) return;

    model = gtk_combo_box_get_model (cbox);
    valid_iter = gtk_tree_model_get_iter_first (model, &iter);

    while (valid_iter)
    {
        GValue value = { 0 };

        gtk_tree_model_get_value (model, &iter, 1, &value);
        if (lsd && lsd->is_equal)    // A specific comparator function was set
        {
            if ((lsd->is_equal)(g_value_get_pointer(&value), data))
            {
                gtk_combo_box_set_active_iter (cbox, &iter);
                return;
            }
        }
        else    // No specific comparator function set, use generic pointer comparison instead
        {
            if (g_value_get_pointer(&value) == data)
            {
                gtk_combo_box_set_active_iter (cbox, &iter);
                return;
            }
        }
        valid_iter = gtk_tree_model_iter_next (model, &iter);
    }
}
