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
extern "C" {
#include "search-param.h"
#include "dialog-invoice.h"
}
#include "gnc-icons.h"
#include "gnc-plugin.h"
#include "gnc-plugin-page-invoices-owner.h"
#include "gnc-gobject-utils.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static const char * const PLUGIN_NAME = "GncPluginPageInvoicesOwner";
static const char * const PLUGIN_ACTIONS_NAME = "GncPluginInvoicesOwnerActions";
static const char * const UI_FILE = "gnc-plugin-page-invoices-owner.ui";

namespace { // Isolate class-scope to this file. 
class Controller;
}

struct GncPluginPageInvoicesOwnerPrivate
{
    Controller *controller = nullptr;
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

static Controller *
get_controller (GncPluginPage *plugin_page)
{
    auto *priv = get_private (plugin_page);
    g_assert (priv);

    return priv->controller;
}

static Controller *
get_controller (gpointer user_data)
{
    auto *plugin_page = GNC_PLUGIN_PAGE (user_data);
    g_assert (plugin_page);

    return get_controller (plugin_page);
}

namespace { // Isolate class-scope to this file.

class Controller {
    enum class DisplayType {
      BOTH,
      INVOICES,
      CREDITNOTES,
    };

    GncPluginPageInvoicesOwner *plugin_page = nullptr;
    GtkWidget     *widget = nullptr; // Managed/freed by gtk
    GtkWidget     *query_view = nullptr; // Managed/freed by gtk
    GList         *columns = nullptr;

    // Filters start
    bool          show_paid = true;
    bool          show_unpaid = true;
    bool          show_posted = true;
    bool          show_unposted = true;
    DisplayType   display_type = DisplayType::BOTH;
    std::string   search_term;
    // Filters end

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

    QofQuery * 
    create_filter ()
    {
        auto owner_type = get_owner ();
        auto *book = gnc_get_current_book ();

        auto *query = qof_query_create_for (GNC_ID_INVOICE);
        qof_query_set_book (query, book);
        
        if (display_type == DisplayType::BOTH || display_type == DisplayType::INVOICES)
        {
            GncInvoiceType inv_type;

            switch (owner_type)
            {
              case GNC_OWNER_CUSTOMER: inv_type = GNC_INVOICE_CUST_INVOICE; break;
              case GNC_OWNER_VENDOR: inv_type = GNC_INVOICE_VEND_INVOICE; break;
              case GNC_OWNER_EMPLOYEE: inv_type = GNC_INVOICE_EMPL_INVOICE; break;
              default: g_assert_not_reached ();
            }

            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_TYPE, nullptr);
            QofQueryPredData *type_pred = qof_query_int32_predicate (QOF_COMPARE_EQUAL, inv_type);
            qof_query_add_term (query, type_path, type_pred, QOF_QUERY_AND);
        }

        if (display_type == DisplayType::BOTH || display_type == DisplayType::CREDITNOTES)
        {
            GncInvoiceType credit_type;

            switch (owner_type)
            {
              case GNC_OWNER_CUSTOMER: credit_type = GNC_INVOICE_CUST_CREDIT_NOTE; break;
              case GNC_OWNER_VENDOR: credit_type = GNC_INVOICE_VEND_CREDIT_NOTE; break;
              case GNC_OWNER_EMPLOYEE: credit_type = GNC_INVOICE_EMPL_CREDIT_NOTE; break;
              default: g_assert_not_reached ();
            }

            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_TYPE, nullptr);
            QofQueryPredData *type_pred = qof_query_int32_predicate (QOF_COMPARE_EQUAL, credit_type);
            qof_query_add_term (query, type_path, type_pred,
                display_type == DisplayType::BOTH ? QOF_QUERY_OR : QOF_QUERY_AND);
        }

        { // Filter only active invoices.
            QofQuery *query_active = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_active, book);

            qof_query_add_boolean_match(
                query_active,
                qof_query_build_param_list(QOF_PARAM_ACTIVE, nullptr),
                true,
                QOF_QUERY_AND);

            qof_query_merge_in_place (query, query_active, QOF_QUERY_AND); 
            qof_query_destroy (query_active);
        }

        if (show_paid != show_unpaid || !show_paid)
        {
            QofQuery *query_paid = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_paid, book);

            {
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_PAID, nullptr);

                qof_query_add_boolean_match(query_paid,
                                            type_path,
                                            show_paid,
                                            QOF_QUERY_AND);
            }

            if (!show_paid && !show_unpaid) { // If both options are false, add a TRUE match so no results are shown.
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_PAID, nullptr);

                qof_query_add_boolean_match(query_paid,
                                            type_path,
                                            true,
                                            QOF_QUERY_AND);
            }

            qof_query_merge_in_place (query, query_paid, QOF_QUERY_AND); 
            qof_query_destroy (query_paid);
        }

        if (show_posted != show_unposted || !show_posted)
        {
            QofQuery *query_posted = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_posted, book);

            {
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                qof_query_add_boolean_match(query_posted,
                                            type_path,
                                            show_posted,
                                            QOF_QUERY_AND);
            }

            if (!show_posted && !show_unposted) { // If both options are false, add a TRUE match so no results are shown.
                QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                qof_query_add_boolean_match(query_posted,
                                            type_path,
                                            true,
                                            QOF_QUERY_AND);
            }

            qof_query_merge_in_place (query, query_posted, QOF_QUERY_AND); 
            qof_query_destroy (query_posted);
        }

        if (search_term.length ())
        {
            QofQuery *query_term = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_term, book);

            QofQueryParamList *params[4] = {
                nullptr,
                nullptr,
                nullptr,
                nullptr,
            };

            // Customer name
            params[0] = qof_query_build_param_list (INVOICE_OWNER, OWNER_PARENT,
                                                    OWNER_NAME, nullptr);

            // Job name
            params[1] = qof_query_build_param_list (INVOICE_OWNER, OWNER_NAME,
                                                    nullptr);

            // Invoice id
            params[2] = qof_query_build_param_list (INVOICE_ID, nullptr);

            // Invoice billing id 
            params[3] = qof_query_build_param_list (INVOICE_BILLINGID, nullptr);

            for (size_t i = 0; i < std::size(params); i++) {
                auto *path = params[i];

                QofQueryPredData *owner_name_pred =
                    qof_query_string_predicate(
                        QOF_COMPARE_CONTAINS,
                        search_term.c_str(),
                        QOF_STRING_MATCH_CASEINSENSITIVE,
                        false);

                qof_query_add_term(
                    query_term,
                    path,
                    owner_name_pred,
                    i == 0 ? QOF_QUERY_AND : QOF_QUERY_OR);
            }

            qof_query_merge_in_place (query, query_term, QOF_QUERY_AND); 
            qof_query_destroy (query_term);
        }

        return query;
    }

    void
    free_filter (QofQuery *query)
    {
        qof_query_destroy (query);
    }

public:
    Controller (GncPluginPageInvoicesOwner *p) : plugin_page (p)
    {
        // Dynamically check plugin_page once on class initialization.
        // We only need to do this once here, for the rest of the class
        // we can skip nullptr checks when doing PAGE casts.
        g_assert (GNC_IS_PLUGIN_PAGE_INVOICES_OWNER (plugin_page));

        auto owner_type = get_owner();

        switch (owner_type)
        {
        case GNC_OWNER_CUSTOMER: break;
        case GNC_OWNER_VENDOR: break;
        case GNC_OWNER_EMPLOYEE: break;
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
                get_controller (user_data)->new_invoice ();
              },
              nullptr, nullptr, nullptr
            },

            {
              "EditInvoice",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                get_controller (user_data)->edit_invoice ();
              },
              nullptr, nullptr, nullptr
            },

            {
              "DuplicateInvoice",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                get_controller (user_data)->duplicate_invoice ();
              },
              nullptr, nullptr, nullptr
            },

            {
              "PrintInvoice",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                get_controller (user_data)->print_invoice ();
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
            case GNC_OWNER_EMPLOYEE: title = _("Expense vouchers"); break;
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

    ~Controller ()
    {
        ENTER ("page %p", plugin_page);

        gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE (plugin_page));

        g_list_free (columns);

        LEAVE("");
    }

    GtkWidget *create_widget ()
    {
        ENTER ("page %p", plugin_page);

        QofIdType type = GNC_INVOICE_MODULE_NAME;

        auto *query = create_filter ();
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

            /*columns = gnc_search_param_prepend (columns, _("Total"), 
                                                  nullptr, 
                                                  type,
                                                  "total-sub",
                                                  nullptr);*/ 

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

        query_view = gnc_query_view_new (columns, query);

        free_filter (query);
        
        {
            auto *qview = GNC_QUERY_VIEW (query_view);
            g_assert(qview);

            g_signal_connect (
                qview,
                "double_click_entry",
                G_CALLBACK (
                    +[] (GNCQueryView *qview, gpointer item, gpointer user_data)
                    {
                      get_controller (user_data)->double_click_entry (item);
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

        { // Add filter UI and connect signals.
            auto resource = std::string(GNUCASH_RESOURCE_PREFIX) +  "/" + UI_FILE;
            auto builder = gtk_builder_new ();
            GError *error = NULL;

            gtk_builder_set_translation_domain (builder, PROJECT_NAME);

            gtk_builder_add_from_resource (builder, resource.c_str(), &error);

            g_assert (!error);

            auto *filter = GTK_WIDGET (gtk_builder_get_object(builder, "invoices-filters"));
            g_assert (filter);

            gtk_widget_show(filter);

            {
                auto *paid_widget = GTK_WIDGET (gtk_builder_get_object(builder, "show-paid"));
                g_assert (paid_widget);

                g_signal_connect(
                    paid_widget,
                    "state-set",
                    G_CALLBACK(
                        +[] (GtkSwitch *gtk_switch, gboolean state, gpointer user_data) -> gboolean
                        {
                            get_controller (user_data)->toggle_paid (state);

                            return false;
                        }
                    ),
                    plugin_page 
                );
            }

            {
                auto *unpaid_widget = GTK_WIDGET (gtk_builder_get_object(builder, "show-unpaid"));
                g_assert (unpaid_widget);

                g_signal_connect(
                    unpaid_widget,
                    "state-set",
                    G_CALLBACK(
                        +[] (GtkSwitch *gtk_switch, gboolean state, gpointer user_data) -> gboolean
                        {
                            get_controller (user_data)->toggle_unpaid (state);

                            return false;
                        }
                    ),
                    plugin_page 
                );
            }

            {
                auto *posted_widget = GTK_WIDGET (gtk_builder_get_object(builder, "show-posted"));
                g_assert (posted_widget);

                g_signal_connect(
                    posted_widget,
                    "state-set",
                    G_CALLBACK(
                        +[] (GtkSwitch *gtk_switch, gboolean state, gpointer user_data) -> gboolean
                        {
                            get_controller (user_data)->toggle_posted (state);

                            return false;
                        }
                    ),
                    plugin_page 
                );
            }

            {
                auto *unposted_widget = GTK_WIDGET (gtk_builder_get_object(builder, "show-unposted"));
                g_assert (unposted_widget);

                g_signal_connect(
                    unposted_widget,
                    "state-set",
                    G_CALLBACK(
                        +[] (GtkSwitch *gtk_switch, gboolean state, gpointer user_data) -> gboolean
                        {
                            get_controller (user_data)->toggle_unposted (state);

                            return false;
                        }
                    ),
                    plugin_page 
                );
            }

            {
                auto *display_widget = GTK_WIDGET (gtk_builder_get_object(builder, "display-type"));
                g_assert (display_widget);

                g_signal_connect(
                    display_widget,
                    "changed",
                    G_CALLBACK(
                        +[] (GtkComboBox *combo,
                              gpointer    user_data)
                        {
                            const gchar *text = gtk_combo_box_get_active_id (GTK_COMBO_BOX (combo));

                            DisplayType type = DisplayType::BOTH;

                            if (g_strcmp0(text, "invoices") == 0)
                                type = DisplayType::INVOICES;
                            else if (g_strcmp0(text, "credit-notes") == 0)
                                type = DisplayType::CREDITNOTES;

                           get_controller (user_data)->toggle_display_type (type);
                        }
                    ),
                    plugin_page 
                );
            }

            {
                auto *search_widget = GTK_WIDGET (gtk_builder_get_object(builder, "search"));
                g_assert (search_widget);

                g_signal_connect(
                    search_widget,
                    "changed",
                    G_CALLBACK(
                        +[] (GtkEditable *editable,
                              gpointer    user_data)
                        {
                            const gchar *text = gtk_entry_get_text(GTK_ENTRY(editable));

                            get_controller (user_data)->toggle_search_term (text);
                        }
                    ),
                    plugin_page 
                );
            }

            auto *container = GTK_CONTAINER (widget);

            gtk_container_add(container, filter);

            g_object_unref(builder);
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

        LEAVE ("");

        return widget;
    }

    void double_click_entry (gpointer item)
    {
        auto *invoice = GNC_INVOICE (item);

        if (!invoice) return;

        gnc_ui_invoice_edit (get_window (), invoice);
    }

    void focus_page ()
    {
        const gchar *gnc_plugin_load_ui_items [] =
        {
            nullptr,
        };

        gnc_main_window_update_menu_and_toolbar (
            GNC_MAIN_WINDOW (get_window ()),
            GNC_PLUGIN_PAGE (plugin_page),
            gnc_plugin_load_ui_items
        );
    }

    void new_invoice ()
    {
        gnc_ui_invoice_new (get_window (), nullptr, gnc_get_current_book ());
    }

    void edit_invoice ()
    {
        auto *invoice = get_selected (); 

        if (!invoice) return;

        gnc_ui_invoice_edit (get_window (), invoice);
    }

    void duplicate_invoice ()
    {
        auto *invoice = get_selected (); 

        if (!invoice) return;

        gnc_ui_invoice_duplicate (get_window (), invoice, true, nullptr);
    }

    void print_invoice ()
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

    void toggle_paid (bool status)
    {
        show_paid = status;
        auto *query = create_filter ();

        gnc_query_view_reset_query (GNC_QUERY_VIEW (query_view), query);

        free_filter (query);
    }

    void toggle_unpaid (bool status)
    {
        show_unpaid = status;
        auto *query = create_filter ();

        gnc_query_view_reset_query (GNC_QUERY_VIEW (query_view), query);

        free_filter (query);
    }

    void toggle_posted (bool status)
    {
        show_posted = status;
        auto *query = create_filter ();

        gnc_query_view_reset_query (GNC_QUERY_VIEW (query_view), query);

        free_filter (query);
    }

    void toggle_unposted (bool status)
    {
        show_unposted = status;
        auto *query = create_filter ();

        gnc_query_view_reset_query (GNC_QUERY_VIEW (query_view), query);

        free_filter (query);
    }


    void toggle_display_type (DisplayType type)
    {
        display_type = type;
        auto *query = create_filter ();

        gnc_query_view_reset_query (GNC_QUERY_VIEW (query_view), query);

        free_filter (query);
    }

    void toggle_search_term (const char *term)
    {
        search_term = term;

        auto *query = create_filter ();

        gnc_query_view_reset_query (GNC_QUERY_VIEW (query_view), query);

        free_filter (query);
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

        LEAVE ("exiting page %p", plugin_page);
        return GNC_PLUGIN_PAGE (plugin_page);
    }

    plugin_page = GNC_PLUGIN_PAGE_INVOICES_OWNER (g_object_new (gnc_plugin_page_invoices_owner_get_type (), nullptr));
    g_return_val_if_fail (plugin_page, nullptr);

    auto *priv = get_private (plugin_page);
    priv->owner_type = owner_type;
    priv->controller = new Controller(plugin_page);

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
                                          return get_controller (plugin_page)->create_widget ();
                                      };

    page_class->destroy_widget      = +[] (GncPluginPage *plugin_page) 
                                      {
                                          auto *priv = get_private (plugin_page);

                                          delete priv->controller;
                                          priv->controller = nullptr;
                                      };

    page_class->focus_page_function = +[] (GncPluginPage *plugin_page) -> gboolean 
                                      {
                                          get_controller (plugin_page)->focus_page ();

                                          return false;
                                      };
}

static void 
gnc_plugin_page_invoices_owner_init (GncPluginPageInvoicesOwner *plugin_page)
{
}
