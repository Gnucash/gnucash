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
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
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
extern "C" {
#include "search-param.h"
#include "dialog-invoice.h"
}
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
    Page *page = nullptr;
    GncOwnerType owner_type;
};

G_DEFINE_TYPE_WITH_PRIVATE (GncPluginPageInvoicesOwner, gnc_plugin_page_invoices_owner, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_INVOICES_OWNER_GET_PRIVATE(o)  \
   ((GncPluginPageInvoicesOwnerPrivate*)gnc_plugin_page_invoices_owner_get_instance_private ((GncPluginPageInvoicesOwner*)o))

static GncPluginPageInvoicesOwnerPrivate * 
get_private (GncPluginPage *plugin_page)
{
    g_assert (GNC_IS_PLUGIN_PAGE_INVOICES_OWNER (plugin_page));

    return GNC_PLUGIN_PAGE_INVOICES_OWNER_GET_PRIVATE (
        GNC_PLUGIN_PAGE_INVOICES_OWNER (plugin_page)
    );
}

static GncPluginPageInvoicesOwnerPrivate * 
get_private (GncPluginPageInvoicesOwner *plugin_page)
{
    g_assert (GNC_IS_PLUGIN_PAGE_INVOICES_OWNER (plugin_page));

    return GNC_PLUGIN_PAGE_INVOICES_OWNER_GET_PRIVATE (plugin_page);
}

static Page *
get_page (GncPluginPage *plugin_page)
{
    auto *priv = get_private (plugin_page);
    g_assert (priv);

    return priv->page;
}

static Page *
get_page (gpointer user_data)
{
    auto *plugin_page = GNC_PLUGIN_PAGE (user_data);
    g_assert (plugin_page);

    return get_page (plugin_page);
}

namespace { // Isolate class-scope to this file.

class Page {
public:
    GncPluginPageInvoicesOwner *plugin_page = nullptr;

private:
    std::unique_ptr<GncFilterInvoicesDialog> filter;
    GtkWidget     *widget = nullptr; // Managed/freed by gtk
    GtkWidget     *query_view = nullptr; // Managed/freed by gtk
    GList         *columns = nullptr;

    GncInvoice *
    get_selected ()
    {
        auto *qv = GNC_QUERY_VIEW (query_view); 
        g_assert (qv);

        gpointer selected_entry = gnc_query_view_get_selected_entry (qv); 

        if (!selected_entry) return nullptr;

        auto *invoice = GNC_INVOICE (selected_entry);
        g_assert (invoice);

        return invoice;
    }

public:
    Page (GncPluginPageInvoicesOwner &p) : plugin_page (&p)
    {
        // Dynamically check plugin_page once on class initialization.
        // We only need to do this once here, for the rest of the class
        // we can skip nullptr checks when doing PAGE casts.
        g_assert (GNC_IS_PLUGIN_PAGE_INVOICES_OWNER (plugin_page));

        auto owner_type = get_owner();

        filter = std::make_unique<GncFilterInvoicesDialog> (
            *GNC_PLUGIN_PAGE (plugin_page), owner_type,
            [this]() {
                this->apply_filter(); 
            }
        );

        switch (owner_type)
        {
        case GNC_OWNER_CUSTOMER:
            gnc_plugin_page_set_menu_qualifier(
                GNC_PLUGIN_PAGE (plugin_page), "c");
            break;
        case GNC_OWNER_VENDOR:
            gnc_plugin_page_set_menu_qualifier(
                GNC_PLUGIN_PAGE (plugin_page), "v");
            break;
        case GNC_OWNER_EMPLOYEE:
            gnc_plugin_page_set_menu_qualifier(
                GNC_PLUGIN_PAGE (plugin_page), "e");
            break;
        default: g_assert_not_reached (); // Fail if invalid type 
        }

        static GActionEntry gnc_plugin_actions [] =
        {
            {
              "NewInvoice",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                  get_page (user_data)->new_invoice ();
              },
              nullptr, nullptr, nullptr
            },

            {
              "EditInvoice",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                  get_page (user_data)->edit_invoice ();
              },
              nullptr, nullptr, nullptr
            },

            {
              "DuplicateInvoice",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                  get_page (user_data)->duplicate_invoice ();
              },
              nullptr, nullptr, nullptr
            },

            {
              "PrintInvoice",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                  get_page (user_data)->print_invoice ();
              },
              nullptr, nullptr, nullptr
            },

            {
              "OpenFilterBy",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                  get_page (user_data)->filter->create_dialog ();
              },
              nullptr, nullptr, nullptr
            },
        };

        static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

        GSimpleActionGroup *simple_action_group;

        {
            const char *title;

            switch (owner_type)
            {
            case GNC_OWNER_CUSTOMER: title = _("Invoices"); break;
            case GNC_OWNER_VENDOR: title = _("Bills"); break;
            case GNC_OWNER_EMPLOYEE: title = _("Expense Vouchers"); break;
            default: g_assert_not_reached ();
            }

            g_object_set (G_OBJECT (plugin_page), "page-name", title,
                          "ui-description", UI_FILE,
                          nullptr);
        }

        gnc_plugin_page_add_book (
            GNC_PLUGIN_PAGE (plugin_page), gnc_get_current_book()
        );

        simple_action_group = gnc_plugin_page_create_action_group (
            GNC_PLUGIN_PAGE (plugin_page), PLUGIN_ACTIONS_NAME);

        g_action_map_add_action_entries (
            G_ACTION_MAP (simple_action_group),
            gnc_plugin_actions,
            gnc_plugin_n_actions,
            plugin_page
        );
    }

    ~Page ()
    {
        gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE (plugin_page));

        g_list_free (columns);
    }

    GncOwnerType
    get_owner ()
    {
        return get_private (plugin_page)->owner_type;
    }

    GtkWindow *
    get_window ()
    {
        auto *p = GNC_PLUGIN_PAGE (plugin_page);
        g_assert (p);

        auto *window = GTK_WINDOW (gnc_plugin_page_get_window (p));
        g_assert (window);

        return window;
    }

    GtkWidget *
    create_widget ()
    {
        ENTER ("page %p", plugin_page);

        QofIdType type = GNC_INVOICE_MODULE_NAME;

        auto owner_type = get_owner ();
     
        { // Set up table columns.
            const char *id_name;

            switch (owner_type)
            {
            case GNC_OWNER_CUSTOMER: id_name = _("Invoice ID"); break;
            case GNC_OWNER_VENDOR: id_name = _("Bill ID"); break;
            case GNC_OWNER_EMPLOYEE: id_name = _("Voucher ID"); break;
            default: g_assert_not_reached ();
            }

            const char *owner_name = owner_type == GNC_OWNER_EMPLOYEE
                ? _("Employee") : _("Company"); 

            columns = gnc_search_param_prepend (columns, _("Job"), 
                                                nullptr, 
                                                type,
                                                INVOICE_OWNER,
                                                OWNER_JOB,
                                                JOB_NAME,
                                                nullptr); 

            columns = gnc_search_param_prepend (columns, _("Billing ID"), 
                                                nullptr, 
                                                type,
                                                INVOICE_BILLINGID,
                                                nullptr); 
            
            columns = gnc_search_param_prepend (columns, _("Due"), 
                                                nullptr, 
                                                type,
                                                INVOICE_DUE,
                                                nullptr); 

            columns = gnc_search_param_prepend (columns, _("Paid"), 
                                                nullptr, 
                                                type,
                                                INVOICE_IS_PAID,
                                                nullptr); 

            columns = gnc_search_param_prepend (columns, _("Posted"), 
                                                nullptr, 
                                                type,
                                                INVOICE_POSTED,
                                                nullptr); 

            columns = gnc_search_param_prepend (columns, owner_name, 
                                                nullptr, 
                                                type,
                                                INVOICE_OWNER,
                                                OWNER_PARENT,
                                                OWNER_NAME,
                                                nullptr); 

            columns = gnc_search_param_prepend (columns, _("Total"), 
                                                  nullptr, 
                                                  type,
                                                  INVOICE_TOTAL,
                                                  nullptr); 

            columns = gnc_search_param_prepend (columns, _("Type"), 
                                                nullptr, 
                                                type,
                                                INVOICE_TYPE_STRING,
                                                nullptr); 

            columns = gnc_search_param_prepend (columns, id_name,
                                                nullptr, type,
                                                INVOICE_ID,
                                                nullptr);
        }

        auto *query = filter->make_filter ();

        g_return_val_if_fail (query, nullptr);

        query_view = gnc_query_view_new (columns, query);

        filter->free_filter (query);
        
        {
            auto *qview = GNC_QUERY_VIEW (query_view);
            g_assert(qview);

            g_signal_connect (
                qview,
                "double_click_entry",
                G_CALLBACK (
                    +[] (GNCQueryView *qview, gpointer item, gpointer user_data)
                    {
                      get_page (user_data)->double_click_entry (item);
                    }
                ),
                plugin_page
            );

            g_signal_connect (
                qview,
                "button-press-event",
                G_CALLBACK (
                    +[] (GNCQueryView *qview, GdkEventButton *event,
                         gpointer user_data) -> bool
                    {
                        get_page (user_data)->button_press (event);

                        return false;
                    }
                ),
                plugin_page
            );

            auto *selection = gtk_tree_view_get_selection(
                GTK_TREE_VIEW(qview)
            );

            g_signal_connect(
                selection,
                "changed",
                G_CALLBACK(
                    +[] (GtkTreeSelection *selection, gpointer user_data)
                    {
                        get_page (user_data)->selection_changed ();
                    }
                ),
                plugin_page
            );
        }

        widget = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);

        {
            auto *gtk_widget = GTK_WIDGET (widget);
            g_assert (gtk_widget);

            gtk_widget_set_name (gtk_widget, "gnc-id-invoices-owner-page");
        }

        auto *scrolled = gtk_scrolled_window_new (nullptr, nullptr);

        {
            auto *scrolled = GTK_SCROLLED_WINDOW (widget);
            g_assert (scrolled);

            gtk_scrolled_window_set_policy (scrolled, GTK_POLICY_AUTOMATIC,
                                            GTK_POLICY_AUTOMATIC);
        }

        {
            auto *gtk_container = GTK_CONTAINER (scrolled);
            g_assert (gtk_container);

            gtk_container_add (gtk_container, query_view);
        }

        {
            auto *box = GTK_BOX (widget);
            g_assert (box);

            gtk_box_pack_start(box, scrolled, true, true, 0);
        }

        gtk_widget_show_all (widget);

        g_signal_connect (G_OBJECT (plugin_page), "inserted",
                          G_CALLBACK (gnc_plugin_page_inserted_cb),
                          nullptr);

        selection_changed ();

        LEAVE ("");

        return widget;
    }

    void
    double_click_entry (gpointer item)
    {
        auto *invoice = GNC_INVOICE (item);

        if (!invoice) return;

        gnc_ui_invoice_edit (get_window (), invoice);
    }

    void
    button_press (GdkEventButton *event)
    {
        gnc_main_window_button_press_cb (query_view, event, GNC_PLUGIN_PAGE (plugin_page));
    }

    void
    selection_changed ()
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
                GNC_PLUGIN_PAGE (plugin_page)
            );

        gnc_plugin_set_actions_enabled (
            G_ACTION_MAP(simple_action_group),
            actions_requiring_owner_always,
            has_selection 
        );
    }

    void
    focus_page ()
    {
        static const gchar *load_ui [] =
        {
            "ViewPlaceholder1",
            "EditPlaceholder2",
            "EditPlaceholder3",
            nullptr,
        };

        gnc_main_window_update_menu_and_toolbar (
            GNC_MAIN_WINDOW (get_window ()),
            GNC_PLUGIN_PAGE (plugin_page),
            load_ui 
        );
    }

    void
    new_invoice ()
    {
        GncOwner current_owner;

        switch (get_owner ())
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

        gnc_ui_invoice_new (get_window (), &current_owner, gnc_get_current_book ());
    }

    void
    edit_invoice ()
    {
        auto *invoice = get_selected (); 

        if (!invoice) return;

        gnc_ui_invoice_edit (get_window (), invoice);
    }

    void
    duplicate_invoice ()
    {
        auto *invoice = get_selected (); 

        if (!invoice) return;

        gnc_ui_invoice_duplicate (get_window (), invoice, true, nullptr);
    }

    void
    print_invoice ()
    {
        auto *window = get_window ();
        auto *invoice = get_selected (); 

        if (!invoice) return;

        gchar *report_guid = gnc_invoice_window_use_default_report_template_or_change (window);

        g_return_if_fail (report_guid);

        auto *report_page = gnc_invoice_window_print_invoice (window, invoice, report_guid);

        g_free (report_guid);

        g_return_if_fail (report_page);

        gnc_main_window_open_page (GNC_MAIN_WINDOW (window), report_page);
    }

    void
    apply_filter ()
    {
        auto *query = filter->make_filter ();

        g_return_if_fail (query);

        gnc_query_view_reset_query (GNC_QUERY_VIEW (query_view), query);

        filter->free_filter (query);
    }
};

} // End namespace

GncPluginPage *
gnc_plugin_page_invoices_owner_new (GncOwnerType owner_type)
{
    ENTER ("");

    GncPluginPageInvoicesOwner *plugin_page;
    const GList *item;

    switch (owner_type)
    {
    case GNC_OWNER_CUSTOMER: break;
    case GNC_OWNER_VENDOR: break;
    case GNC_OWNER_EMPLOYEE: break;
    default: g_return_val_if_fail (false, nullptr); // Fail if invalid value. 
    }

    /* Is there an existing page? */
    item = gnc_gobject_tracking_get_list (PLUGIN_NAME);
    for ( ; item; item = g_list_next (item))
    {
        plugin_page = GNC_PLUGIN_PAGE_INVOICES_OWNER (item->data);
        g_return_val_if_fail (plugin_page, nullptr);

        if (get_private (plugin_page)->owner_type != owner_type) continue;

        LEAVE ("existing page %p", plugin_page);
        return GNC_PLUGIN_PAGE (plugin_page);
    }

    plugin_page = GNC_PLUGIN_PAGE_INVOICES_OWNER (g_object_new (gnc_plugin_page_invoices_owner_get_type (), nullptr));
    g_return_val_if_fail (plugin_page, nullptr);

    auto *priv = get_private (plugin_page);
    priv->owner_type = owner_type;
    priv->page = new Page(*plugin_page);

    LEAVE ("new page %p", plugin_page);
    return GNC_PLUGIN_PAGE (plugin_page);
}

static void
gnc_plugin_page_invoices_owner_class_init (GncPluginPageInvoicesOwnerClass *klass)
{
    auto *page_class = GNC_PLUGIN_PAGE_CLASS (klass);

    page_class->plugin_name         = PLUGIN_NAME;

    page_class->tab_icon            = GNC_ICON_INVOICE;

    page_class->create_widget       = +[] (GncPluginPage *plugin_page) -> GtkWidget *
                                      {
                                          return get_page (plugin_page)->create_widget ();
                                      };

    page_class->destroy_widget      = +[] (GncPluginPage *plugin_page) 
                                      {
                                          auto *priv = get_private (plugin_page);

                                          delete priv->page;
                                          priv->page = nullptr;
                                      };

    page_class->focus_page_function = +[] (GncPluginPage *plugin_page) -> gboolean 
                                      {
                                          get_page (plugin_page)->focus_page ();

                                          return false;
                                      };
}

static void 
gnc_plugin_page_invoices_owner_init (GncPluginPageInvoicesOwner *plugin_page)
{
}
