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

class InvoicesFilter {
    enum Preset {
        CUSTOM,
        OVERDUE
    };

    // State
    Preset        preset = Preset::CUSTOM;
    int           overdue_days = 0;
    bool          show_paid = true;
    bool          show_unpaid = true;
    bool          show_posted = true;
    bool          show_unposted = true;
    bool          show_invoices = true;
    bool          show_creditnotes = true;
    bool          use_start_date = false;
    bool          use_end_date = false;
    time64        start_date = 0;
    time64        end_date = 0;
    std::string   search_term;
    // State end

public:
    Controller      *controller = nullptr;
    GtkWidget       *dialog = nullptr;

    GtkToggleButton *custom_toggle = nullptr;
    GtkToggleButton *overdue_toggle = nullptr;
    GtkWidget       *custom_controls = nullptr;
    GtkWidget       *overdue_controls = nullptr;

    GtkToggleButton *overdue_days_zero = nullptr;
    GtkToggleButton *overdue_days_seven = nullptr;
    GtkToggleButton *overdue_days_fourteen = nullptr;
    GtkToggleButton *overdue_days_thirty = nullptr;
    GtkToggleButton *overdue_days_sixty = nullptr;
    GtkToggleButton *overdue_days_ninety = nullptr;

    GtkToggleButton *paid_toggle = nullptr;
    GtkToggleButton *unpaid_toggle = nullptr;
    GtkToggleButton *posted_toggle = nullptr;
    GtkToggleButton *unposted_toggle = nullptr;
    GtkToggleButton *invoices_toggle = nullptr;
    GtkToggleButton *creditnotes_toggle = nullptr;
    GtkToggleButton *start_toggle = nullptr;
    GtkToggleButton *end_toggle = nullptr;
    GNCDateEdit     *start_picker = nullptr;
    GNCDateEdit     *end_picker = nullptr;
    GtkEntry        *search_entry = nullptr;

    void dialog_open();

    QofQuery *make_filter();

    void
    free_filter (QofQuery *query)
    {
        qof_query_destroy (query);
    }

private:
    void
    dialog_close ()
    {
        dialog = nullptr;

        custom_toggle = nullptr;
        overdue_toggle = nullptr;
        custom_controls = nullptr;
        overdue_controls = nullptr;

        overdue_days_zero = nullptr;
        overdue_days_seven = nullptr;
        overdue_days_fourteen = nullptr;
        overdue_days_thirty = nullptr;
        overdue_days_sixty = nullptr;
        overdue_days_ninety = nullptr;

        paid_toggle = nullptr;
        unpaid_toggle = nullptr;
        posted_toggle = nullptr;
        unposted_toggle = nullptr;
        invoices_toggle = nullptr;
        creditnotes_toggle = nullptr;
        start_toggle = nullptr;
        end_toggle = nullptr;
        start_picker = nullptr;
        end_picker = nullptr;
        search_entry = nullptr;
    }

    bool 
    reset ()
    {
        if (preset == Preset::CUSTOM
            && show_paid && show_unpaid
            && show_posted && show_unposted
            && show_invoices && show_creditnotes
            && search_term == ""
            && !use_start_date && !use_end_date)
        {
            return false;
        }
        else if (preset == Preset::OVERDUE
                 && overdue_days == 0
                 && search_term == "")
        {
            return false;
        }

        if (preset == Preset::CUSTOM)
        {
            set_paid (true);
            set_unpaid (true);
            set_posted (true);
            set_unposted (true);
            set_invoices (true);
            set_creditnotes (true);
            set_use_start_date (false);
            set_use_end_date (false);
            reset_dates ();
        }
        else if (preset == Preset::OVERDUE)
        {
            set_overdue_days (overdue_days_zero);
        }

        set_search_term ("");

        g_warn_if_fail (dialog);

        return true;
    }

    void
    resize_window ()
    {
        g_return_if_fail (dialog);

        GtkRequisition requisition;
        gtk_widget_get_preferred_size (GTK_WIDGET(dialog),
                                       nullptr,
                                       &requisition);

        gtk_window_resize (GTK_WINDOW (dialog),
                           requisition.width,
                           requisition.height);
    }

    void
    set_preset (Preset p)
    {
        preset = p;
        GtkToggleButton *toggle;

        switch (p)
        {
        case Preset::CUSTOM:
          toggle = custom_toggle;
          gtk_widget_hide (overdue_controls);
          gtk_widget_show (custom_controls);
          break;
        case Preset::OVERDUE:
            toggle = overdue_toggle;
            gtk_widget_hide (custom_controls);
            gtk_widget_show (overdue_controls);
            break;
        }

        if (toggle) gtk_toggle_button_set_active (toggle, true);
        else g_warn_if_fail (true);

        resize_window ();
    }

    void
    set_overdue_days (GtkToggleButton *button)
    {
        int days = 0;

        g_return_if_fail (button);

        gtk_toggle_button_set_active (button, true);

        if (button == overdue_days_seven) days = 7;
        else if (button == overdue_days_fourteen) days = 14;
        else if (button == overdue_days_thirty) days = 30;
        else if (button == overdue_days_sixty) days = 60;
        else if (button == overdue_days_ninety) days = 90;

        overdue_days = days;
    }

    void
    set_paid (bool status)
    {
        show_paid = status;

        if (paid_toggle) gtk_toggle_button_set_active(paid_toggle, show_paid);
        else g_warn_if_fail(true);
    }

    void
    set_unpaid (bool status)
    {
        show_unpaid = status;

        if (unpaid_toggle) gtk_toggle_button_set_active(unpaid_toggle, show_unpaid);
        else g_warn_if_fail(true);
    }

    void
    set_posted (bool status)
    {
        show_posted = status;

        if (posted_toggle) gtk_toggle_button_set_active(posted_toggle, show_posted);
        else g_warn_if_fail(true);
    }

    void
    set_unposted (bool status)
    {
        show_unposted = status;

        if (unposted_toggle) gtk_toggle_button_set_active(unposted_toggle, show_unposted);
        else g_warn_if_fail(true);
    }

    void
    set_invoices (bool status)
    {
        show_invoices = status;

        if (invoices_toggle) gtk_toggle_button_set_active(invoices_toggle, show_invoices);
        else g_warn_if_fail(true);
    }

    void
    set_creditnotes (bool status)
    {
        show_creditnotes = status;

        if (creditnotes_toggle) gtk_toggle_button_set_active(creditnotes_toggle, show_creditnotes);
        else g_warn_if_fail(true);
    }

    void
    set_search_term (const char *term)
    {
        search_term = term;

        if (search_entry) gtk_entry_set_text(search_entry, search_term.c_str());
        else g_warn_if_fail(true);
    }

    void
    set_search_term ()
    {
        const gchar *text = gtk_entry_get_text(search_entry);

        set_search_term (text);
    }

    void
    set_use_start_date (bool status)
    {
        use_start_date = status;

        if (start_toggle)
            gtk_toggle_button_set_active(start_toggle, use_start_date);
        else g_warn_if_fail(true);

        if (start_picker)
            gtk_widget_set_sensitive (GTK_WIDGET (start_picker), use_start_date);
        else g_warn_if_fail(true);
    }

    void
    set_use_end_date (bool status)
    {
        use_end_date = status;

        if (end_toggle)
            gtk_toggle_button_set_active(end_toggle, use_end_date);
        else g_warn_if_fail(true);

        if (end_picker)
            gtk_widget_set_sensitive (GTK_WIDGET (end_picker), use_end_date);
        else g_warn_if_fail(true);
    }

    void
    set_dates (bool init)
    {
        if (start_picker)
        {
            if (init && start_date != 0)
            {
                gnc_date_edit_set_time(start_picker, start_date);
            }
            else start_date = gnc_date_edit_get_date(start_picker);
        }
        else g_warn_if_fail(true);

        if (end_picker)
        {
            if (init && end_date != 0)
            {
                gnc_date_edit_set_time(end_picker, end_date);
            }
            else end_date = gnc_date_edit_get_date_end(end_picker);
        }
        else g_warn_if_fail(true);
    }

    void
    reset_dates ()
    {
        if (start_picker)
            gnc_date_edit_set_time(start_picker, time(NULL));
        else g_warn_if_fail(true);

        if (end_picker)
            gnc_date_edit_set_time(end_picker, time(NULL));
        else g_warn_if_fail(true);

        set_dates(false);
    }
};

class Controller {
public:
    GncPluginPageInvoicesOwner *plugin_page = nullptr;

private:
    InvoicesFilter filter;
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
    Controller (GncPluginPageInvoicesOwner *p) : plugin_page (p)
    {
        // Dynamically check plugin_page once on class initialization.
        // We only need to do this once here, for the rest of the class
        // we can skip nullptr checks when doing PAGE casts.
        g_assert (GNC_IS_PLUGIN_PAGE_INVOICES_OWNER (plugin_page));

        filter.controller = this;

        auto owner_type = get_owner();

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

            {
              "OpenFilterBy",
              +[] (GSimpleAction *simple,
                   GVariant      *parameter,
                   gpointer       user_data)
              {
                  get_controller (user_data)->filter.dialog_open ();
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

    ~Controller ()
    {
        gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE (plugin_page));

        g_list_free (columns);

        if (filter.dialog) gtk_widget_destroy (GTK_WIDGET (filter.dialog));
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

    GtkWidget
    *create_widget ()
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

        auto *query = filter.make_filter ();

        query_view = gnc_query_view_new (columns, query);

        filter.free_filter (query);
        
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

            g_signal_connect (
                qview,
                "button-press-event",
                G_CALLBACK (
                    +[] (GNCQueryView *qview, GdkEventButton *event,
                         gpointer user_data) -> bool
                    {
                        get_controller (user_data)->button_press (event);

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
                        get_controller(user_data)->selection_changed ();
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
        auto *query = filter.make_filter ();

        gnc_query_view_reset_query (GNC_QUERY_VIEW (query_view), query);

        filter.free_filter (query);
    }
};

void
InvoicesFilter::dialog_open()
{
    g_assert (controller);

    ENTER ("(fd %p, page %p)", dialog, controller->plugin_page);

    if (dialog)
    {
        gtk_window_present (GTK_WINDOW (dialog));
        LEAVE ("existing dialog");
        return;
    }

    auto resource = std::string (GNUCASH_RESOURCE_PREFIX) +  "/" + UI_FILE;
    auto *builder = gtk_builder_new ();
    GError *error = NULL;

    gtk_builder_set_translation_domain (builder, PROJECT_NAME);

    gtk_builder_add_from_resource (builder, resource.c_str(), &error);

    if (error)
    {
        g_object_unref (G_OBJECT(builder));
        g_return_if_fail (false);
    }

    dialog = GTK_WIDGET (gtk_builder_get_object (builder, "invoices-filters"));

    if (!dialog) {
        g_object_unref (G_OBJECT (builder));
        g_return_if_fail (false);
    }

    gtk_window_set_transient_for(GTK_WINDOW (dialog), controller->get_window ());

    /* Translators: The %s is the name of the plugin page */
    gchar *title = g_strdup_printf(
        _("Filter %s by…"),
        gnc_plugin_page_get_page_name (GNC_PLUGIN_PAGE (controller->plugin_page))
    );

    gtk_window_set_title(GTK_WINDOW(dialog), title);

    g_free(title);

    gtk_widget_show_all (dialog);

    g_signal_connect(
        dialog, "destroy",
        G_CALLBACK (
            +[] (GtkWidget *w, gpointer user_data)
            {
                static_cast<InvoicesFilter *> (user_data)->dialog_close();
            }
        ),
        this 
    );

    {
        custom_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "preset-custom"));
        overdue_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "preset-overdue"));
        custom_controls = GTK_WIDGET (gtk_builder_get_object(builder, "custom-controls"));
        overdue_controls = GTK_WIDGET (gtk_builder_get_object(builder, "overdue-controls"));

        if (!custom_toggle || !overdue_toggle || !custom_controls || !overdue_controls)
            g_warn_if_fail(true);
        else
        {
            {
                auto owner_type = controller->get_owner();

                if (owner_type == GNC_OWNER_VENDOR)
                {
                    gtk_button_set_label(
                        GTK_BUTTON (overdue_toggle),
                        _("Overdue Bills")
                    );
                }
                else if (owner_type == GNC_OWNER_EMPLOYEE)
                {
                    gtk_button_set_label(
                        GTK_BUTTON (overdue_toggle),
                        _("Overdue Expense Vouchers")
                    );
                }
            }

            g_signal_connect(
                custom_toggle,
                "toggled",
                G_CALLBACK(
                    +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                    {
                        if (!gtk_toggle_button_get_active(button)) return false;

                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_preset (Preset::CUSTOM);
                        filter->controller->apply_filter ();

                        return false;
                    }
                ),
                this
            );

            g_signal_connect(
                overdue_toggle,
                "toggled",
                G_CALLBACK(
                    +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                    {
                        if (!gtk_toggle_button_get_active(button)) return false;

                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_preset (Preset::OVERDUE);
                        filter->controller->apply_filter ();

                        return false;
                    }
                ),
                this
            );
        }

        set_preset (preset);
    }

    {
        overdue_days_zero = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-0"));
        overdue_days_seven = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-7"));
        overdue_days_fourteen = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-14"));
        overdue_days_thirty = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-30"));
        overdue_days_sixty = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-60"));
        overdue_days_ninety = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "overdue-days-90"));

        if (!overdue_days_zero || !overdue_days_seven || !overdue_days_fourteen || !overdue_days_thirty || !overdue_days_sixty || !overdue_days_ninety)
            g_warn_if_fail(true);
        else
        {
            GtkToggleButton *toggles[] = {
                overdue_days_zero, overdue_days_seven, overdue_days_fourteen,
                overdue_days_thirty, overdue_days_sixty, overdue_days_ninety
            };

            switch (overdue_days)
            {
            case 0: set_overdue_days (overdue_days_zero); break;
            case 7: set_overdue_days (overdue_days_seven); break;
            case 14: set_overdue_days (overdue_days_fourteen); break;
            case 30: set_overdue_days (overdue_days_thirty); break;
            case 60: set_overdue_days (overdue_days_sixty); break;
            case 90: set_overdue_days (overdue_days_ninety); break;
            default: break;
            }

            for (size_t i = 0; i < std::size (toggles); i++)
            {
                g_signal_connect(
                    toggles[i],
                    "toggled",
                    G_CALLBACK(
                        +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                        {
                            if (!gtk_toggle_button_get_active(button)) return false;

                            auto *filter = static_cast<InvoicesFilter *> (user_data);

                            filter->set_overdue_days (button);
                            filter->controller->apply_filter ();

                            return false;
                        }
                    ),
                    this
                );
            }
        }
    }

    {
        paid_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-paid"));

        if (!paid_toggle)
        {
            g_warn_if_fail(true);
        }
        else
        {
            set_paid (show_paid);

            g_signal_connect(
                paid_toggle,
                "toggled",
                G_CALLBACK(
                    +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                    {
                        auto state = gtk_toggle_button_get_active(button);
                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_paid (state);
                        filter->controller->apply_filter ();

                        return false;
                    }
                ),
                this 
            );
        }
    }

    {
        unpaid_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-unpaid"));


        if (!unpaid_toggle)
        {
            g_warn_if_fail(true);
        }
        else
        {
            set_unpaid (show_unpaid);

            g_signal_connect(
                unpaid_toggle,
                "toggled",
                G_CALLBACK(
                    +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                    {
                        auto state = gtk_toggle_button_get_active(button);
                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_unpaid (state);
                        filter->controller->apply_filter ();

                        return false;
                    }
                ),
                this 
            );
        }
    }

    {
        posted_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-posted"));

        if (!posted_toggle)
        {
            g_warn_if_fail(true);
        }
        else
        {
            set_posted (show_posted);

            g_signal_connect(
                posted_toggle,
                "toggled",
                G_CALLBACK(
                    +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                    {
                        auto state = gtk_toggle_button_get_active(button);
                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_posted (state);
                        filter->controller->apply_filter ();

                        return false;
                    }
                ),
                this 
            );
        }
    }

    {
        unposted_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-unposted"));

        if (!unposted_toggle)
        {
            g_warn_if_fail(true);
        }
        else
        {
            set_unposted (show_unposted);

            g_signal_connect(
                unposted_toggle,
                "toggled",
                G_CALLBACK(
                    +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                    {
                        auto state = gtk_toggle_button_get_active(button);
                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_unposted (state);
                        filter->controller->apply_filter ();

                        return false;
                    }
                ),
                this
            );
        }
    }

    {
        invoices_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-invoices"));
        
        if (!invoices_toggle)
        {
            g_warn_if_fail(true);
        }
        else
        {
            auto owner_type = controller->get_owner();

            if (owner_type == GNC_OWNER_VENDOR)
            {
                gtk_button_set_label(
                    GTK_BUTTON (invoices_toggle),
                    _("Show Bills")
                );
            }
            else if (owner_type == GNC_OWNER_EMPLOYEE)
            {
                gtk_button_set_label(
                    GTK_BUTTON (invoices_toggle),
                    _("Show Expense Vouchers")
                );
            }

            set_invoices (show_invoices);

            g_signal_connect(
                invoices_toggle,
                "toggled",
                G_CALLBACK(
                    +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                    {
                        auto state = gtk_toggle_button_get_active(button);
                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_invoices (state);
                        filter->controller->apply_filter ();

                        return false;
                    }
                ),
                this
            );
        }
    }

    {
        creditnotes_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "show-creditnotes"));

        if (!creditnotes_toggle)
        {
            g_warn_if_fail(true);
        }
        else
        {
            gtk_toggle_button_set_active(creditnotes_toggle, show_creditnotes);
            set_creditnotes (show_creditnotes);

            g_signal_connect(
                creditnotes_toggle,
                "toggled",
                G_CALLBACK(
                    +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                    {
                        auto state = gtk_toggle_button_get_active(button);
                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_creditnotes (state);
                        filter->controller->apply_filter ();

                        return false;
                    }
                ),
                this
            );
        }
    }

    {
        GtkWidget* start_box = GTK_WIDGET (gtk_builder_get_object (builder, "start_date_box"));

        if (!start_box) {
            g_warn_if_fail (true);
        }
        else {
            start_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "filter-by-start"));
            start_picker = GNC_DATE_EDIT (gnc_date_edit_new (time (NULL), FALSE, TRUE));

            if (start_picker)
            {
                gtk_box_pack_start (GTK_BOX (start_box), GTK_WIDGET (start_picker), TRUE, TRUE, 0);

                g_signal_connect(
                    start_picker->date_entry,
                    "activate",
                    G_CALLBACK(
                        +[](GtkWidget *date,
                            gpointer user_data)
                        {
                            auto *filter = static_cast<InvoicesFilter *> (user_data);

                            filter->set_dates (false);
                            filter->controller->apply_filter ();
                        }
                    ),
                    this 
                );
            }
            else g_warn_if_fail (true);

            if (start_toggle)
            {
                g_signal_connect(
                    start_toggle,
                    "toggled",
                    G_CALLBACK(
                        +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                        {
                            auto state = gtk_toggle_button_get_active(button);
                            auto *filter = static_cast<InvoicesFilter *> (user_data);

                            filter->set_use_start_date (state);
                            filter->controller->apply_filter ();

                            return false;
                        }
                    ),
                    this
                );
            }
            else g_warn_if_fail (true);

            set_use_start_date (use_start_date);
            set_dates(true);

            gtk_widget_show_all (start_box);
        }
    }

    {
        GtkWidget* end_box = GTK_WIDGET (gtk_builder_get_object (builder, "end_date_box"));

        if (!end_box) {
            g_warn_if_fail (true);
        }
        else {
            end_toggle = GTK_TOGGLE_BUTTON (gtk_builder_get_object(builder, "filter-by-end"));
            end_picker = GNC_DATE_EDIT (gnc_date_edit_new (time (NULL), FALSE, TRUE));

            if (end_picker)
            {
                gtk_box_pack_start (GTK_BOX (end_box), GTK_WIDGET (end_picker), TRUE, TRUE, 0);

                g_signal_connect(
                    end_picker->date_entry,
                    "activate",
                    G_CALLBACK(
                        +[](GtkWidget *date,
                            gpointer user_data)
                        {
                            auto *filter = static_cast<InvoicesFilter *> (user_data);

                            filter->set_dates (false);
                            filter->controller->apply_filter ();
                        }
                    ),
                    this 
                );
            }
            else g_warn_if_fail (true);

            if (end_toggle)
            {
                g_signal_connect(
                    end_toggle,
                    "toggled",
                    G_CALLBACK(
                        +[] (GtkToggleButton *button, gpointer user_data) -> gboolean
                        {
                            auto state = gtk_toggle_button_get_active(button);
                            auto *filter = static_cast<InvoicesFilter *> (user_data);

                            filter->set_use_end_date (state);
                            filter->controller->apply_filter ();

                            return false;
                        }
                    ),
                    this
                );
            }
            else g_warn_if_fail (true);

            set_use_end_date (use_end_date);
            set_dates(true);

            gtk_widget_show_all(end_box);
        }
    }

    {
        auto *apply_button = GTK_BUTTON (gtk_builder_get_object(builder, "apply-dates"));

        if (!apply_button)
        {
            g_warn_if_fail(true);
        }
        else
        {
            g_signal_connect(
                apply_button,
                "clicked",
                G_CALLBACK(+[] (GtkButton *button, gpointer user_data)
                {
                    auto *filter = static_cast<InvoicesFilter *> (user_data);

                    filter->set_dates (false);

                    if (filter->use_start_date || filter->use_end_date)
                        filter->controller->apply_filter ();
                }),
                this
            );
        }
    }

    {
        search_entry = GTK_ENTRY (gtk_builder_get_object(builder, "search"));

        if (!search_entry)
        {
            g_warn_if_fail(true);
        }
        else
        {
            auto owner_type = controller->get_owner();

            if (owner_type == GNC_OWNER_VENDOR)
            {
                gtk_entry_set_placeholder_text(
                    search_entry,
                    "Type to search for Bills..."
                );
            }
            else if (owner_type == GNC_OWNER_EMPLOYEE)
            {
                gtk_entry_set_placeholder_text(
                    search_entry,
                    "Type to search for Expense Vouchers..."
                );
            }

            if (search_term.length())
                set_search_term (search_term.c_str());

            g_signal_connect(
                search_entry,
                "activate",
                G_CALLBACK(
                    +[] (GtkEditable *editable,
                          gpointer    user_data)
                    {
                        const gchar *text = gtk_entry_get_text(GTK_ENTRY(editable));
                        auto *filter = static_cast<InvoicesFilter *> (user_data);

                        filter->set_search_term (text);
                        filter->controller->apply_filter ();
                    }
                ),
                this
            );
        }

        auto *apply_button = GTK_BUTTON (gtk_builder_get_object(builder, "apply-search"));

        if (!apply_button)
        {
            g_warn_if_fail(true);
        }
        else {
            g_signal_connect(
                apply_button,
                "clicked",
                G_CALLBACK(+[] (GtkButton *button, gpointer user_data)
                {
                    auto *filter = static_cast<InvoicesFilter *> (user_data);

                    filter->set_search_term ();
                    filter->controller->apply_filter ();
                }),
                this
            );
        }
    }

    {
        auto *reset_button = GTK_BUTTON (gtk_builder_get_object(builder, "reset"));

        if (!reset_button)
        {
            g_warn_if_fail(true);
        }
        else
        {
            g_signal_connect (
                reset_button,
                "clicked",
                G_CALLBACK (
                    +[] (GtkButton *button, gpointer user_data)
                    {
                        auto *filter = static_cast<InvoicesFilter *> (user_data);
                        if (filter->reset ())
                            filter->controller->apply_filter ();
                    }
                ),
                this
            );
        }
    }

    {
        auto *close_button = GTK_BUTTON (gtk_builder_get_object(builder, "close"));

        if (!close_button)
        {
            g_warn_if_fail(true);
        }
        else
        {
            g_signal_connect(
                close_button,
                "clicked",
                G_CALLBACK(+[] (GtkButton *button, gpointer user_data)
                {
                    gtk_widget_destroy (GTK_WIDGET (user_data));
                }),
                dialog
            );
        }
    }

    g_object_unref(G_OBJECT(builder));

    LEAVE("");
}

QofQuery *
InvoicesFilter::make_filter()
{
    auto owner_type = controller->get_owner ();
    auto *book = gnc_get_current_book ();

    auto *query = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (query, book);

    {
        bool none = preset == Preset::CUSTOM && (!show_invoices && !show_creditnotes);
        bool show_inv = show_invoices || preset != Preset::CUSTOM;
        bool show_cred = show_creditnotes || preset != Preset::CUSTOM;
    
        if (show_inv || none)
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

        if (show_cred || none)
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
                none || !show_inv ? QOF_QUERY_AND : QOF_QUERY_OR);
        }
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

    if (preset == Preset::OVERDUE)
    {
        QofQuery *query_overdue = qof_query_create_for (GNC_ID_INVOICE);
        qof_query_set_book (query_overdue, book);

        {
            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_PAID, nullptr);

            qof_query_add_boolean_match(query_overdue,
                                        type_path,
                                        false,
                                        QOF_QUERY_AND);
        }

        {
            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

            qof_query_add_boolean_match(query_overdue,
                                        type_path,
                                        true,
                                        QOF_QUERY_AND);
        }

        {
            GDate date;
            gnc_gdate_set_today(&date);

            if (overdue_days) g_date_subtract_days(&date, overdue_days);

            QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_DUE, nullptr);
            QofQueryPredData *pred_end = qof_query_date_predicate(
                QOF_COMPARE_LT, QOF_DATE_MATCH_NORMAL,
                gnc_time64_get_day_end_gdate(&date)
            );

            qof_query_add_term(query_overdue, type_path, pred_end, QOF_QUERY_AND);
        }

        qof_query_merge_in_place (query, query_overdue, QOF_QUERY_AND); 
        qof_query_destroy (query_overdue);
    }
    else if (preset == Preset::CUSTOM) {
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

        if (use_start_date && start_date != 0)
        {
            QofQuery *query_start = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_start, book);

            {
                QofQuery *query_posted = qof_query_create_for (GNC_ID_INVOICE);
                qof_query_set_book (query_posted, book);

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                    qof_query_add_boolean_match(query_posted,
                                                type_path,
                                                true,
                                                QOF_QUERY_AND);
                }

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_POSTED, nullptr);
                    QofQueryPredData *pred_start = qof_query_date_predicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, start_date);
                    qof_query_add_term(query_posted, type_path, pred_start, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_start, query_posted, QOF_QUERY_AND); 
                qof_query_destroy (query_posted);
            }

            {
                QofQuery *query_unposted = qof_query_create_for (GNC_ID_INVOICE);
                qof_query_set_book (query_unposted, book);

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                    qof_query_add_boolean_match(query_unposted,
                                                type_path,
                                                false,
                                                QOF_QUERY_AND);
                }

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_OPENED, nullptr);
                    QofQueryPredData *pred_start = qof_query_date_predicate(QOF_COMPARE_GTE, QOF_DATE_MATCH_NORMAL, start_date);
                    qof_query_add_term(query_unposted, type_path, pred_start, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_start, query_unposted, QOF_QUERY_OR); 
                qof_query_destroy (query_unposted);
            }

            qof_query_merge_in_place (query, query_start, QOF_QUERY_AND); 
            qof_query_destroy (query_start);
        }

        if (use_end_date && end_date != 0)
        {
            QofQuery *query_end = qof_query_create_for (GNC_ID_INVOICE);
            qof_query_set_book (query_end, book);

            {
                QofQuery *query_posted = qof_query_create_for (GNC_ID_INVOICE);
                qof_query_set_book (query_posted, book);

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                    qof_query_add_boolean_match(query_posted,
                                                type_path,
                                                true,
                                                QOF_QUERY_AND);
                }

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_POSTED, nullptr);
                    QofQueryPredData *pred_end = qof_query_date_predicate(QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, end_date);
                    qof_query_add_term(query_posted, type_path, pred_end, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_end, query_posted, QOF_QUERY_AND); 
                qof_query_destroy (query_posted);
            }

            {
                QofQuery *query_unposted = qof_query_create_for (GNC_ID_INVOICE);
                qof_query_set_book (query_unposted, book);

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_IS_POSTED, nullptr);

                    qof_query_add_boolean_match(query_unposted,
                                                type_path,
                                                false,
                                                QOF_QUERY_AND);
                }

                {
                    QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_OPENED, nullptr);
                    QofQueryPredData *pred_end = qof_query_date_predicate(QOF_COMPARE_LTE, QOF_DATE_MATCH_NORMAL, end_date);
                    qof_query_add_term(query_unposted, type_path, pred_end, QOF_QUERY_AND);
                }

                qof_query_merge_in_place (query_end, query_unposted, QOF_QUERY_OR); 
                qof_query_destroy (query_unposted);
            }

            qof_query_merge_in_place (query, query_end, QOF_QUERY_AND); 
            qof_query_destroy (query_end);
        }
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
