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

#include <assert.h>
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
#include "qofbook.h"
#include "business-gnome-utils.h"
#include "dialog-customer.h"
#include "dialog-job.h"
#include "dialog-vendor.h"
#include "dialog-employee.h"
#include "dialog-invoice.h"
#include "dialog-utils.h"

#include "guile-mappings.h"
#include "gnc-guile-utils.h"
#include "gnc-prefs.h"
#include "gnc-commodity.h"
#include "gnc-report-combo.h"
#include "qofinstance.h"
#include "qoflog.h"

static const QofLogModule log_module = G_LOG_DOMAIN;

typedef enum
{
    GNCSEARCH_TYPE_SELECT,
    GNCSEARCH_TYPE_EDIT
} GNCSearchType;

enum
{
    COL_INV_NAME = 0,
    COL_INV_GUID,
    COL_INV_MISSING,
    NUM_INV_COLS
};

#define PRINTABLE_INVOICE_GUID "5123a759ceb9483abf2182d01c140e8d"
#define TAX_INVOICE_GUID       "0769e242be474010b4acf264a5512e6e"
#define EASY_INVOICE_GUID      "67112f318bef4fc496bdc27d106bbda4"
#define FANCY_INVOICE_GUID     "3ce293441e894423a2425d7a22dd1ac6"

enum
{
    PRINTABLE_INVOICE_PREF_NUM = 0,
    TAX_INVOICE_PREF_NUM,
    EASY_INVOICE_PREF_NUM,
    FANCY_INVOICE_PREF_NUM,
};

static const char* invoice_printreport_values[] =
{
    /* The list below are the guids of reports that can
     * be used to print an invoice.
     *
     * Important: This list matches the order of existing saved
     * preference entries.
     */
    PRINTABLE_INVOICE_GUID,
    TAX_INVOICE_GUID,
    EASY_INVOICE_GUID,
    FANCY_INVOICE_GUID,
    NULL
};

#define GNC_PREFS_GROUP_INVOICE    "dialogs.business.invoice"
#define GNC_PREF_INV_PRINT_RPT     "invoice-printreport"

const char *
gnc_get_builtin_default_invoice_print_report (void)
{
    return PRINTABLE_INVOICE_GUID;
}

const char *
gnc_migrate_default_invoice_print_report (void)
{
    QofBook *book = gnc_get_current_book ();
    int old_style_value = gnc_prefs_get_int (GNC_PREFS_GROUP_INVOICE,
                                             GNC_PREF_INV_PRINT_RPT);

    if (old_style_value >= TAX_INVOICE_PREF_NUM &&
        old_style_value <= FANCY_INVOICE_PREF_NUM)
    {
        const gchar *ret = invoice_printreport_values[old_style_value];
        qof_book_set_default_invoice_report (book, ret, " ");
        return ret;
    }
    else
        return gnc_get_builtin_default_invoice_print_report ();
}

char *
gnc_get_default_invoice_print_report (void)
{
    QofBook *book = gnc_get_current_book ();
    gchar *default_guid = qof_book_get_default_invoice_report_guid (book);

    if (!default_guid)
        return g_strdup (gnc_migrate_default_invoice_print_report ());

    return default_guid;
}

GtkWidget *
gnc_default_invoice_report_combo (const char* guid_scm_function)
{
    GSList *invoice_list = NULL;
    SCM template_menu_name = scm_c_eval_string ("gnc:report-template-menu-name/report-guid");
    SCM get_rpt_guids = scm_c_eval_string (guid_scm_function);
    SCM reportlist;
    SCM rpt_guids;

    if (!scm_is_procedure (get_rpt_guids))
        return NULL;

    reportlist = scm_call_0 (get_rpt_guids);
    rpt_guids = reportlist;

    if (scm_is_list (rpt_guids))
    {
        while (!scm_is_null (rpt_guids))
        {
            gchar *guid_str = scm_to_utf8_string (SCM_CAR(rpt_guids));
            gchar *name = gnc_scm_to_utf8_string (scm_call_2(template_menu_name,
                                                  SCM_CAR(rpt_guids), SCM_BOOL_F));

            // Note: invoice_list and entries freed in report combo
            ReportListEntry *rle = g_new0 (ReportListEntry, 1);

            rle->report_guid = guid_str;
            rle->report_name = name;

            invoice_list = g_slist_append (invoice_list, rle);

            rpt_guids = SCM_CDR(rpt_guids);
        }
    }
    return gnc_report_combo_new (invoice_list);
}

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
        text = _("Select…");
        text_editable = TRUE;
        break;
    case GNCSEARCH_TYPE_EDIT:
        text = _("Edit…");
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
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(edit));
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

    QofInstance *instance =
        gnc_general_search_get_selected (GNC_GENERAL_SEARCH (widget));

    if (!instance)
        return;

    if (owner->type == GNC_OWNER_NONE ||
        g_strcmp0(instance->e_type, qofOwnerGetType(owner)) == 0)
        qofOwnerSetEntity(owner, instance);
    else
    {
        PWARN("Owner type mismatch: Instance %s, Owner %s",
              instance->e_type, qofOwnerGetType(owner));
        owner->owner.undefined = instance;
    }
}

void gnc_owner_set_owner (GtkWidget *widget, const GncOwner *owner)
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

    edit = gnc_general_search_new (GNC_INVOICE_MODULE_NAME, _("Select…"),
                                   TRUE, gnc_invoice_select_search_cb, isi, isi->book);
    if (!edit)
    {
        g_free(isi);
        return NULL;
    }

    gnc_general_search_set_selected (GNC_GENERAL_SEARCH (edit), invoice);
    gtk_box_append (GTK_BOX(hbox), GTK_WIDGET(edit));
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
gnc_account_select_dropdown_fill (GtkWidget *widget, QofBook *book,
                                  GList *acct_types, GList *acct_commodities)
{
    GtkDropDown *dropdown;
    GtkStringList *model;
    GList *list, *node;
    const gchar *current_name = NULL;
    gchar *saved_name = NULL;
    guint selected = GTK_INVALID_LIST_POSITION;
    guint position = 0;

    g_return_val_if_fail (GTK_IS_DROP_DOWN (widget), NULL);
    g_return_val_if_fail (book, NULL);
    g_return_val_if_fail (acct_types, NULL);

    dropdown = GTK_DROP_DOWN (widget);
    if (GTK_IS_STRING_OBJECT (gtk_drop_down_get_selected_item (dropdown)))
        current_name = gtk_string_object_get_string (GTK_STRING_OBJECT (
            gtk_drop_down_get_selected_item (dropdown)));
    saved_name = g_strdup (current_name);

    model = gtk_string_list_new (NULL);
    list = gnc_account_get_descendants (gnc_book_get_root_account (book));
    for (node = list; node; node = node->next)
    {
        Account *account = node->data;
        gchar *name;

        if (g_list_index (acct_types,
                          GINT_TO_POINTER (xaccAccountGetType (account))) == -1)
            continue;
        if (acct_commodities &&
            g_list_find_custom (acct_commodities,
                                GINT_TO_POINTER (xaccAccountGetCommodity (account)),
                                gnc_commodity_compare_void) == NULL)
            continue;

        name = gnc_account_get_full_name (account);
        gtk_string_list_append (model, name);
        if (saved_name && g_strcmp0 (saved_name, name) == 0)
            selected = position;
        position++;
        g_free (name);
    }
    g_list_free (list);

    g_object_set_data (G_OBJECT (dropdown), "book", book);
    gtk_drop_down_set_model (dropdown, G_LIST_MODEL (model));
    if (selected == GTK_INVALID_LIST_POSITION && position)
        selected = 0;
    if (selected != GTK_INVALID_LIST_POSITION)
        gtk_drop_down_set_selected (dropdown, selected);
    g_object_unref (model);
    g_free (saved_name);

    return gnc_account_select_dropdown_get_active (widget);
}

Account *
gnc_account_select_dropdown_get_active (GtkWidget *widget)
{
    GtkDropDown *dropdown;
    GtkStringObject *item;
    QofBook *book;
    const gchar *name;

    if (!GTK_IS_DROP_DOWN (widget))
        return NULL;
    dropdown = GTK_DROP_DOWN (widget);
    book = g_object_get_data (G_OBJECT (dropdown), "book");
    if (!book)
        return NULL;

    item = GTK_STRING_OBJECT (gtk_drop_down_get_selected_item (dropdown));
    if (!item)
        return NULL;
    name = gtk_string_object_get_string (item);
    if (!name || !*name)
        return NULL;
    return gnc_account_lookup_by_full_name (gnc_book_get_root_account (book), name);
}

/***********************************************************************
 * GncDropDown implementation functions
 */

typedef const char * (*GenericLookup_t)(gpointer);
typedef gboolean (*GenericEqual_t)(gpointer, gpointer);

typedef struct
{
    gint component_id;
    GtkDropDown *dropdown;
    QofBook *book;
    gboolean none_ok;
    const char *(*get_name)(gpointer);
    GList *(*get_list)(QofBook*);
    gboolean (*is_equal)(gpointer, gpointer);
} ListStoreData;

#define SIMPLE_DROPDOWN_DATA "simple-dropdown-data"
#define SIMPLE_DROPDOWN_VALUE "simple-dropdown-value"

static void
gnc_simple_dropdown_add_item (GtkStringList *model, const char *label,
                              gpointer value)
{
    GtkStringObject *item;
    guint position = g_list_model_get_n_items (G_LIST_MODEL (model));

    gtk_string_list_append (model, label ? label : "");
    item = GTK_STRING_OBJECT (g_list_model_get_item (G_LIST_MODEL (model), position));
    g_object_set_data (G_OBJECT (item), SIMPLE_DROPDOWN_VALUE, value);
    g_object_unref (item);
}

static gpointer
gnc_simple_dropdown_value_at (GtkDropDown *dropdown, guint position)
{
    GListModel *model;
    GObject *item;
    gpointer value;

    model = gtk_drop_down_get_model (dropdown);
    if (!model || position >= g_list_model_get_n_items (model))
        return NULL;
    item = g_list_model_get_item (model, position);
    value = g_object_get_data (item, SIMPLE_DROPDOWN_VALUE);
    g_object_unref (item);
    return value;
}

static void
gnc_simple_dropdown_generate_model (ListStoreData *lsd)
{
    GList *items;
    GtkStringList *model;

    if (!lsd->get_list || !lsd->get_name)
        return;

    model = gtk_string_list_new (NULL);
    items = lsd->get_list (lsd->book);
    if (lsd->none_ok || !items)
        gnc_simple_dropdown_add_item (model, _("None"), NULL);
    for (; items; items = items->next)
        gnc_simple_dropdown_add_item (model, lsd->get_name (items->data),
                                      items->data);
    gtk_drop_down_set_model (lsd->dropdown, G_LIST_MODEL (model));
    g_object_unref (model);
}

static void
gnc_simple_dropdown_refresh_handler (GHashTable *changes, gpointer user_data)
{
    ListStoreData *lsd = user_data;
    gpointer selected = gnc_simple_dropdown_get_value (lsd->dropdown);
    gnc_simple_dropdown_generate_model (lsd);
    gnc_simple_dropdown_set_value (lsd->dropdown, selected);
}

static void
gnc_simple_dropdown_data_free (gpointer data)
{
    ListStoreData *lsd = data;
    if (lsd->component_id)
        gnc_unregister_gui_component (lsd->component_id);
    g_free (lsd);
}

static void
gnc_simple_dropdown_make (GtkDropDown *dropdown, QofBook *book,
                          gboolean none_ok, QofIdType type_name,
                          GList *(*get_list)(QofBook*),
                          GenericLookup_t get_name,
                          GenericEqual_t is_equal,
                          gpointer initial_choice)
{
    ListStoreData *lsd;

    lsd = g_object_get_data (G_OBJECT (dropdown), SIMPLE_DROPDOWN_DATA);
    if (!lsd)
    {
        lsd = g_new0 (ListStoreData, 1);
        lsd->dropdown = dropdown;
        lsd->book = book;
        lsd->none_ok = none_ok;
        lsd->get_name = get_name;
        lsd->get_list = get_list;
        lsd->is_equal = is_equal;
        lsd->component_id = gnc_register_gui_component (
            "gnc-simple-dropdown-refresh-hook",
            gnc_simple_dropdown_refresh_handler, NULL, lsd);
        if (type_name)
            gnc_gui_component_watch_entity_type (lsd->component_id, type_name,
                                                  QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
        g_object_set_data_full (G_OBJECT (dropdown), SIMPLE_DROPDOWN_DATA,
                                lsd, gnc_simple_dropdown_data_free);
    }

    if (get_list)
        gnc_simple_dropdown_generate_model (lsd);
    else
    {
        GtkStringList *model = gtk_string_list_new (NULL);
        gtk_drop_down_set_model (dropdown, G_LIST_MODEL (model));
        g_object_unref (model);
    }
    gnc_simple_dropdown_set_value (dropdown, initial_choice);
}

void
gnc_billterms_dropdown (GtkDropDown *dropdown, QofBook *book,
                        gboolean none_ok, GncBillTerm *initial_choice)
{
    if (!dropdown || !book)
        return;
    gnc_simple_dropdown_make (dropdown, book, none_ok, GNC_BILLTERM_MODULE_NAME,
                              gncBillTermGetTerms,
                              (GenericLookup_t)gncBillTermGetName,
                              (GenericEqual_t)gncBillTermIsFamily,
                              initial_choice);
}

void
gnc_taxtables_dropdown (GtkDropDown *dropdown, QofBook *book,
                        gboolean none_ok, GncTaxTable *initial_choice)
{
    if (!dropdown || !book)
        return;
    gnc_simple_dropdown_make (dropdown, book, none_ok, GNC_TAXTABLE_MODULE_NAME,
                              gncTaxTableGetTables,
                              (GenericLookup_t)gncTaxTableGetName, NULL,
                              initial_choice);
}

void
gnc_taxincluded_dropdown (GtkDropDown *dropdown, GncTaxIncluded initial_choice)
{
    GtkStringList *model;

    if (!dropdown)
        return;
    gnc_simple_dropdown_make (dropdown, NULL, FALSE, NULL, NULL, NULL, NULL,
                              GINT_TO_POINTER (initial_choice));
    model = GTK_STRING_LIST (gtk_drop_down_get_model (dropdown));
    gnc_simple_dropdown_add_item (model, _("Yes"),
                                  GINT_TO_POINTER (GNC_TAXINCLUDED_YES));
    gnc_simple_dropdown_add_item (model, _("No"),
                                  GINT_TO_POINTER (GNC_TAXINCLUDED_NO));
    gnc_simple_dropdown_add_item (model, _("Use Global"),
                                  GINT_TO_POINTER (GNC_TAXINCLUDED_USEGLOBAL));
    gnc_simple_dropdown_set_value (dropdown, GINT_TO_POINTER (initial_choice));
}

gpointer
gnc_simple_dropdown_get_value (GtkDropDown *dropdown)
{
    if (!dropdown)
        return NULL;
    return gnc_simple_dropdown_value_at (dropdown,
                                         gtk_drop_down_get_selected (dropdown));
}

void
gnc_simple_dropdown_set_value (GtkDropDown *dropdown, gpointer data)
{
    ListStoreData *lsd;
    GListModel *model;
    guint n_items;

    if (!dropdown)
        return;
    model = gtk_drop_down_get_model (dropdown);
    if (!model)
        return;
    lsd = g_object_get_data (G_OBJECT (dropdown), SIMPLE_DROPDOWN_DATA);
    n_items = g_list_model_get_n_items (model);
    for (guint i = 0; i < n_items; i++)
    {
        gpointer value = gnc_simple_dropdown_value_at (dropdown, i);
        if ((lsd && lsd->is_equal && lsd->is_equal (value, data)) ||
            ((!lsd || !lsd->is_equal) && value == data))
        {
            gtk_drop_down_set_selected (dropdown, i);
            return;
        }
    }
    gtk_drop_down_set_selected (dropdown, GTK_INVALID_LIST_POSITION);
}