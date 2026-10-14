/*
 * plugin-page-invoices-owner.cpp -- Page for Invoices Overview 
 * Copyright (C) 2026 Roy Hansen 
 * Author: Roy Hansen <roy@royhansen.no>
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
 * along with this program; if not, see
 * <https://www.gnu.org/licenses/>.
 */

#include <memory>
#include <string>
#include <iterator>
#include <config.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "gnc-ui-util.h"
#include "gnc-query-view.h"
#include "dialog-utils.h"
#include <gnc-date-edit.h>
#include "dialog-invoice.h"
#include "search-param.h"
#include "gnc-icons.h"
#include "gnc-plugin.h"
#include "gnc-plugin-page-invoices-owner.h"
#include "gnc-gobject-utils.h"
#include "dialog-filter-invoices.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static const char * const PLUGIN_NAME = "GncPluginPageInvoicesOwner";
static const char * const PLUGIN_ACTIONS_NAME = "GncPluginInvoicesOwnerActions";
static const char * const UI_FILE = "gnc-plugin-page-invoices-owner.ui";

namespace { // Isolate class-scope to this file. 
class Page;
}

struct GncPluginPageInvoicesOwnerPrivate
{
    Page *m_page = nullptr;
    GncOwnerType m_owner_type;
};

G_DEFINE_TYPE_WITH_PRIVATE (GncPluginPageInvoicesOwner, gnc_plugin_page_invoices_owner, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_INVOICES_OWNER_GET_PRIVATE(o)  \
   ((GncPluginPageInvoicesOwnerPrivate*)gnc_plugin_page_invoices_owner_get_instance_private ((GncPluginPageInvoicesOwner*)o))


/************************************************************/
/*                          Helpers                         */
/************************************************************/

static GncPluginPageInvoicesOwnerPrivate * 
get_private (GncPluginPage *plugin_page)
{
    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_INVOICES_OWNER (plugin_page), nullptr);

    return GNC_PLUGIN_PAGE_INVOICES_OWNER_GET_PRIVATE (
        GNC_PLUGIN_PAGE_INVOICES_OWNER (plugin_page)
    );
}

static GncPluginPageInvoicesOwnerPrivate * 
get_private (GncPluginPageInvoicesOwner *plugin_page)
{
    g_return_val_if_fail (GNC_IS_PLUGIN_PAGE_INVOICES_OWNER (plugin_page), nullptr);

    return GNC_PLUGIN_PAGE_INVOICES_OWNER_GET_PRIVATE (plugin_page);
}

static Page *
get_page (GncPluginPage *plugin_page)
{
    auto *priv = get_private (plugin_page);

    g_return_val_if_fail (priv, nullptr);

    return priv->m_page;
}

static Page *
get_page (gpointer user_data)
{
    auto *plugin_page = GNC_PLUGIN_PAGE (user_data);

    g_return_val_if_fail (plugin_page, nullptr);

    return get_page (plugin_page);
}

namespace { // Isolate class-scope to this file.


/************************************************************/
/*                          Page Class                      */
/************************************************************/

class Page {
public:
    GncPluginPageInvoicesOwner *m_plugin_page = nullptr;

private:
    GncOwnerType m_owner_type;
    std::unique_ptr<GncFilterInvoicesDialog> m_filter;
    GtkWidget *m_widget = nullptr; // Managed/freed by gtk
    GtkWidget *m_query_view = nullptr; // Managed/freed by gtk
    GList *m_columns = nullptr;

    GncInvoice *get_selected ();

    void create_table_columns ();

public:
    Page (GncPluginPageInvoicesOwner &p, GncOwnerType owner);

    ~Page ()
    {
        gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE (m_plugin_page));
        g_list_free (m_columns);
    }

    GtkWindow *get_window ();

    GtkWidget *create_widget ();

    void double_click_entry (gpointer item);

    void
    button_press (GdkEventButton *event)
    {
        gnc_main_window_button_press_cb (m_query_view, event, GNC_PLUGIN_PAGE (m_plugin_page));
    }

    void selection_changed ();

    void focus_page ();

    void new_invoice ();

    void edit_invoice ();

    void duplicate_invoice ();

    void print_invoice ();

    void apply_filter ();

    void open_filter_dialog ();
};


/************************************************************/
/*                     Command callbacks                    */
/************************************************************/

static void
action_new_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    auto *page = get_page (user_data);
    g_return_if_fail (page);

    page->new_invoice ();
}

static void
action_edit_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    auto *page = get_page (user_data);
    g_return_if_fail (page);
    
    page->edit_invoice ();
}

static void
action_duplicate_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    auto *page = get_page (user_data);
    g_return_if_fail (page);

    page->duplicate_invoice ();
}

static void
action_print_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    auto *page = get_page (user_data);
    g_return_if_fail (page);

    page->print_invoice ();
}

static void
action_open_filter_dialog (GSimpleAction *simple, GVariant *parameter, gpointer user_data)
{
    auto *page = get_page (user_data);
    g_return_if_fail (page);

    page->open_filter_dialog ();
}

static void
action_double_click_entry (GNCQueryView *qview, gpointer item, gpointer user_data)
{
    auto *page = get_page (user_data);
    g_return_if_fail (page);

    page->double_click_entry (item);
}

static bool 
action_button_press_event (GNCQueryView *qview, GdkEventButton *event,
                           gpointer user_data)
{
    auto *page = get_page (user_data);
    g_return_val_if_fail (page, false);

    page->button_press (event);

    return false;
}

static void
action_selection_changed (GtkTreeSelection *selection, gpointer user_data)
{
    auto *page = get_page (user_data);
    g_return_if_fail (page);

    page->selection_changed ();
}


/************************************************************/
/*                     Page callbacks                       */
/************************************************************/

static GtkWidget * 
page_create_widget (GncPluginPage *plugin_page)
{
    auto *page = get_page (plugin_page);
    g_return_val_if_fail (page, nullptr);

    return page->create_widget ();
}

static void
page_destroy_widget (GncPluginPage *plugin_page)
{
    auto *priv = get_private (plugin_page);
    g_return_if_fail (priv);

    delete priv->m_page;
    priv->m_page = nullptr;
}

static gboolean
page_focus_function (GncPluginPage *plugin_page)
{
    auto *page = get_page (plugin_page);
    g_return_val_if_fail (page, false);

    page->focus_page ();

    return false;
}


/************************************************************/
/*                     Implementation                       */
/************************************************************/

GncInvoice *
Page::get_selected ()
{
    auto *qv = GNC_QUERY_VIEW (m_query_view); 
    g_return_val_if_fail(qv, nullptr);

    gpointer selected_entry = gnc_query_view_get_selected_entry (qv); 

    if (!selected_entry)
        return nullptr;

    auto *invoice = GNC_INVOICE (selected_entry);
    g_return_val_if_fail(invoice, nullptr);

    return invoice;
}

void
Page::create_table_columns ()
{
    QofIdType type = GNC_INVOICE_MODULE_NAME;
    const char *id_name;

    switch (m_owner_type)
    {
    case GNC_OWNER_CUSTOMER:
        id_name = _("Invoice ID");
        break;
    case GNC_OWNER_VENDOR:
        id_name = _("Bill ID");
        break;
    case GNC_OWNER_EMPLOYEE:
        id_name = _("Voucher ID");
        break;
    default:
        id_name = "ERROR"; // Should never happen, owner_type is
                           // checked further up the stack in
                           // gnc_plugin_page_invoices_owner_new ()
        break;
    }

    const char *owner_name = m_owner_type == GNC_OWNER_EMPLOYEE
        ? _("Employee") : _("Company"); 

    m_columns = gnc_search_param_prepend (m_columns, _("Job"), 
                                        nullptr, 
                                        type,
                                        INVOICE_OWNER,
                                        OWNER_JOB,
                                        JOB_NAME,
                                        nullptr); 

    m_columns = gnc_search_param_prepend (m_columns, _("Billing ID"), 
                                        nullptr, 
                                        type,
                                        INVOICE_BILLINGID,
                                        nullptr); 
    
    m_columns = gnc_search_param_prepend (m_columns, _("Due"), 
                                        nullptr, 
                                        type,
                                        INVOICE_DUE,
                                        nullptr); 

    m_columns = gnc_search_param_prepend (m_columns, _("Paid"), 
                                        nullptr, 
                                        type,
                                        INVOICE_IS_PAID,
                                        nullptr); 

    m_columns = gnc_search_param_prepend (m_columns, _("Posted"), 
                                        nullptr, 
                                        type,
                                        INVOICE_POSTED,
                                        nullptr); 

    m_columns = gnc_search_param_prepend (m_columns, owner_name, 
                                        nullptr, 
                                        type,
                                        INVOICE_OWNER,
                                        OWNER_PARENT,
                                        OWNER_NAME,
                                        nullptr); 

    m_columns = gnc_search_param_prepend (m_columns, _("Total"), 
                                          nullptr, 
                                          type,
                                          INVOICE_TOTAL,
                                          nullptr); 

    m_columns = gnc_search_param_prepend (m_columns, _("Type"), 
                                        nullptr, 
                                        type,
                                        INVOICE_TYPE_STRING,
                                        nullptr); 

    m_columns = gnc_search_param_prepend (m_columns, id_name,
                                        nullptr, type,
                                        INVOICE_ID,
                                        nullptr);
}

Page::Page (GncPluginPageInvoicesOwner &p, GncOwnerType owner) : m_plugin_page (&p),
                                                                 m_owner_type (owner)
{
    // Dynamically check m_plugin_page once on class initialization.
    // We only need to do this once here, for the rest of the class
    // we can skip nullptr checks when doing PAGE casts.
    //
    // The entrypoint at gnc_plugin_page_invoices_owner_new () is
    // gracefully checking this exact same value with g_return_val_if_fail()
    // further up the stack.
    // This assert should never trigger unless someone is modifying the
    // internal/non-publicly-exposed code in this file, and messing it up.
    g_assert (GNC_IS_PLUGIN_PAGE_INVOICES_OWNER (m_plugin_page));

    m_filter = std::make_unique<GncFilterInvoicesDialog> (
        *GNC_PLUGIN_PAGE (m_plugin_page), m_owner_type,
        [this]() {
            this->apply_filter(); 
        }
    );

    switch (m_owner_type)
    {
    case GNC_OWNER_CUSTOMER:
        gnc_plugin_page_set_menu_qualifier(
            GNC_PLUGIN_PAGE (m_plugin_page), "c");
        break;
    case GNC_OWNER_VENDOR:
        gnc_plugin_page_set_menu_qualifier(
            GNC_PLUGIN_PAGE (m_plugin_page), "v");
        break;
    case GNC_OWNER_EMPLOYEE:
        gnc_plugin_page_set_menu_qualifier(
            GNC_PLUGIN_PAGE (m_plugin_page), "e");
        break;
    default:
        break;
    }

    static GActionEntry gnc_plugin_actions [] =
    {
        { "NewInvoice", action_new_invoice, nullptr, nullptr, nullptr },
        { "EditInvoice", action_edit_invoice, nullptr, nullptr, nullptr },
        { "DuplicateInvoice", action_duplicate_invoice, nullptr, nullptr, nullptr },
        { "PrintInvoice", action_print_invoice, nullptr, nullptr, nullptr },
        { "OpenFilterBy", action_open_filter_dialog, nullptr, nullptr, nullptr },
    };

    static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

    GSimpleActionGroup *simple_action_group;

    {
        const char *title;

        switch (m_owner_type)
        {
        case GNC_OWNER_CUSTOMER:
            title = _("Invoices");
            break;
        case GNC_OWNER_VENDOR:
            title = _("Bills");
            break;
        case GNC_OWNER_EMPLOYEE:
            title = _("Expense Vouchers");
            break;
        default:
            title = "ERROR"; // Should never happen, owner_type is
                             // checked further up the stack in
                             // gnc_plugin_page_invoices_owner_new ()
            break;
        }

        g_object_set (G_OBJECT (m_plugin_page), "page-name", title,
                      "ui-description", UI_FILE,
                      nullptr);
    }

    gnc_plugin_page_add_book (
        GNC_PLUGIN_PAGE (m_plugin_page), gnc_get_current_book()
    );

    simple_action_group = gnc_plugin_page_create_action_group (
        GNC_PLUGIN_PAGE (m_plugin_page), PLUGIN_ACTIONS_NAME);

    g_action_map_add_action_entries (
        G_ACTION_MAP (simple_action_group),
        gnc_plugin_actions,
        gnc_plugin_n_actions,
        m_plugin_page
    );
}

GtkWindow *
Page::get_window ()
{
    auto *p = GNC_PLUGIN_PAGE (m_plugin_page);
    g_return_val_if_fail (p, nullptr);

    auto *window = GTK_WINDOW (gnc_plugin_page_get_window (p));
    g_return_val_if_fail (window, nullptr);

    return window;
}

GtkWidget *
Page::create_widget ()
{
    ENTER ("page %p", m_plugin_page);

    create_table_columns ();

    auto *query = m_filter->create_qof_query ();

    g_return_val_if_fail (query, nullptr);

    m_query_view = gnc_query_view_new (m_columns, query);

    qof_query_destroy (query);
    
    {
        auto *qview = GNC_QUERY_VIEW (m_query_view);
        g_warn_if_fail (qview);

        if (qview)
        {
            g_signal_connect (qview, "double_click_entry",
                              G_CALLBACK(action_double_click_entry),
                              m_plugin_page);

            g_signal_connect (qview, "button-press-event",
                              G_CALLBACK (action_button_press_event),
                              m_plugin_page);

            auto *selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(qview));

            g_signal_connect (selection, "changed",
                              G_CALLBACK(action_selection_changed),
                              m_plugin_page);
        }
    }

    m_widget = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);

    {
        auto *gtk_widget = GTK_WIDGET (m_widget);
        g_warn_if_fail (gtk_widget);

        if (gtk_widget)
            gtk_widget_set_name (gtk_widget, "gnc-id-invoices-owner-page");
    }

    auto *scrolled = gtk_scrolled_window_new (nullptr, nullptr);

    {
        auto *scrolled = GTK_SCROLLED_WINDOW (m_widget);
        g_warn_if_fail (scrolled);

        if (scrolled)
            gtk_scrolled_window_set_policy (scrolled, GTK_POLICY_AUTOMATIC,
                                            GTK_POLICY_AUTOMATIC);
    }

    {
        auto *gtk_container = GTK_CONTAINER (scrolled);
        g_warn_if_fail (gtk_container);

        if (gtk_container)
            gtk_container_add (gtk_container, m_query_view);
    }

    {
        auto *box = GTK_BOX (m_widget);
        g_warn_if_fail (box);

        if (box)
            gtk_box_pack_start(box, scrolled, true, true, 0);
    }

    gtk_widget_show_all (m_widget);

    g_signal_connect (G_OBJECT (m_plugin_page), "inserted",
                      G_CALLBACK (gnc_plugin_page_inserted_cb),
                      nullptr);

    selection_changed ();

    LEAVE ("");

    return m_widget;
}

void
Page::double_click_entry (gpointer item)
{
    auto *invoice = GNC_INVOICE (item);

    if (!invoice)
        return;

    auto *window = get_window ();
    g_return_if_fail (window);

    gnc_ui_invoice_edit (window, invoice);
}

void
Page::selection_changed ()
{
    static const gchar *actions_requiring_owner_always[] =
    {
        "EditInvoice",
        "DuplicateInvoice",
        "PrintInvoice",
        NULL
    };
    bool has_selection = get_selected () != nullptr; 

    GSimpleActionGroup *simple_action_group =
        gnc_plugin_page_get_action_group (
            GNC_PLUGIN_PAGE (m_plugin_page)
        );

    gnc_plugin_set_actions_enabled (
        G_ACTION_MAP(simple_action_group),
        actions_requiring_owner_always,
        has_selection 
    );
}

void
Page::focus_page ()
{
    static const gchar *load_ui [] =
    {
        "ViewPlaceholder1",
        "EditPlaceholder2",
        "EditPlaceholder3",
        nullptr,
    };

    auto *window = get_window ();
    g_return_if_fail (window);

    gnc_main_window_update_menu_and_toolbar (
        GNC_MAIN_WINDOW (window),
        GNC_PLUGIN_PAGE (m_plugin_page),
        load_ui 
    );
}

void
Page::new_invoice ()
{
    GncOwner current_owner;

    switch (m_owner_type)
    {
    case GNC_OWNER_VENDOR:
        gncOwnerInitVendor(&current_owner, gncOwnerGetVendor (nullptr));
        break;
    case GNC_OWNER_EMPLOYEE:
        gncOwnerInitEmployee(&current_owner, gncOwnerGetEmployee(nullptr));
        break;
    default:
        gncOwnerInitCustomer (&current_owner, gncOwnerGetCustomer (nullptr));
    }

    auto *window = get_window ();
    g_return_if_fail (window);

    gnc_ui_invoice_new (window, &current_owner, gnc_get_current_book ());
}

void
Page::edit_invoice ()
{
    auto *invoice = get_selected (); 

    if (!invoice)
        return;

    auto *window = get_window ();
    g_return_if_fail (window);

    gnc_ui_invoice_edit (window, invoice);
}

void
Page::duplicate_invoice ()
{
    auto *invoice = get_selected (); 

    if (!invoice)
        return;

    auto *window = get_window ();
    g_return_if_fail (window);

    gnc_ui_invoice_duplicate (window, invoice, true, nullptr);
}

void
Page::print_invoice ()
{
    auto *window = get_window ();
    g_return_if_fail (window);

    auto *invoice = get_selected (); 

    if (!invoice)
        return;

    gchar *report_guid = gnc_invoice_window_use_default_report_template_or_change (window);

    g_return_if_fail (report_guid);

    auto *report_page = gnc_invoice_window_print_invoice (window, invoice, report_guid);

    g_free (report_guid);

    g_return_if_fail (report_page);

    gnc_main_window_open_page (GNC_MAIN_WINDOW (window), report_page);
}

void
Page::apply_filter ()
{
    auto *query = m_filter->create_qof_query ();

    g_return_if_fail (query);

    gnc_query_view_reset_query (GNC_QUERY_VIEW (m_query_view), query);

    qof_query_destroy (query);
}

void
Page::open_filter_dialog ()
{
    m_filter->create_dialog();
}

} // End namespace


/************************************************************/
/*                     Plugin page                          */
/************************************************************/

GncPluginPage *
gnc_plugin_page_invoices_owner_new (GncOwnerType owner_type)
{
    ENTER ("");

    g_return_val_if_fail(owner_type == GNC_OWNER_CUSTOMER ||
                         owner_type == GNC_OWNER_VENDOR ||
                         owner_type == GNC_OWNER_EMPLOYEE, nullptr);

    /* Is there an existing page? */
    for (auto *item = gnc_gobject_tracking_get_list (PLUGIN_NAME); item;
         item = g_list_next (item))
    {
        auto *plugin_page =
            GNC_PLUGIN_PAGE_INVOICES_OWNER (item->data);
        g_return_val_if_fail (plugin_page, nullptr);

        auto *priv = get_private (plugin_page);
        g_return_val_if_fail (priv, nullptr);

        if (priv->m_owner_type != owner_type)
            continue;

        LEAVE ("existing page %p", plugin_page);
        return GNC_PLUGIN_PAGE (plugin_page);
    }

    auto *plugin_page = GNC_PLUGIN_PAGE_INVOICES_OWNER (g_object_new (gnc_plugin_page_invoices_owner_get_type (), nullptr));
    g_return_val_if_fail (plugin_page, nullptr);

    auto *priv = get_private (plugin_page);
    g_return_val_if_fail (priv, nullptr);

    priv->m_owner_type = owner_type;
    priv->m_page = new Page(*plugin_page, owner_type);

    LEAVE ("new page %p", plugin_page);
    return GNC_PLUGIN_PAGE (plugin_page);
}

static void
gnc_plugin_page_invoices_owner_class_init (GncPluginPageInvoicesOwnerClass *klass)
{
    auto *page_class = GNC_PLUGIN_PAGE_CLASS (klass);

    page_class->plugin_name         = PLUGIN_NAME;

    page_class->tab_icon            = GNC_ICON_INVOICE;

    page_class->create_widget       = page_create_widget;

    page_class->destroy_widget      = page_destroy_widget;

    page_class->focus_page_function = page_focus_function;
}

static void 
gnc_plugin_page_invoices_owner_init (GncPluginPageInvoicesOwner *plugin_page)
{
}
