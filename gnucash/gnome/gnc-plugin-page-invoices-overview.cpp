/*
 * plugin-page-invoices-overview.cpp -- Page for Invoices Overview 
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
#include "gnc-plugin-page-invoices-overview.h"
#include "gnc-gobject-utils.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

typedef struct GncPluginPageInvoicesOverviewPrivate
{
    GtkWidget   *widget;
    GtkWidget   *query_view;
    QofQuery    *q;
    GList *columns;
} GncPluginPageInvoicesOverviewPrivate;

G_DEFINE_TYPE_WITH_PRIVATE (GncPluginPageInvoicesOverview, gnc_plugin_page_invoices_overview, GNC_TYPE_PLUGIN_PAGE)

#define GNC_PLUGIN_PAGE_INVOICES_OVERVIEW_GET_PRIVATE(o)  \
   ((GncPluginPageInvoicesOverviewPrivate*)gnc_plugin_page_invoices_overview_get_instance_private ((GncPluginPageInvoicesOverview*)o))

#define PLUGIN_ACTIONS_NAME "GncPluginInvoicesOverviewActions"

static GtkWidget *gnc_plugin_page_invoices_overview_create_widget (GncPluginPage *plugin_page);

static GtkWidget * gnc_plugin_page_invoices_overview_create_widget (GncPluginPage *plugin_page);

static void gnc_plugin_page_invoices_overview_destroy_widget (GncPluginPage *plugin_page);

static gboolean gnc_plugin_page_invoices_overview_focus_widget (GncPluginPage *plugin_page);

static void gnc_plugin_invoices_overview_cmd_new_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_invoices_overview_cmd_edit_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_invoices_overview_cmd_duplicate_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static void gnc_plugin_invoices_overview_cmd_print_invoice (GSimpleAction *simple, GVariant *parameter, gpointer user_data);

static GActionEntry gnc_plugin_actions [] =
{
    { "NewInvoice", gnc_plugin_invoices_overview_cmd_new_invoice, NULL, NULL, NULL },
    { "EditInvoice", gnc_plugin_invoices_overview_cmd_edit_invoice, NULL, NULL, NULL },
    { "DuplicateInvoice", gnc_plugin_invoices_overview_cmd_duplicate_invoice, NULL, NULL, NULL },
    { "PrintInvoice", gnc_plugin_invoices_overview_cmd_print_invoice, NULL, NULL, NULL },
};
static guint gnc_plugin_n_actions = G_N_ELEMENTS (gnc_plugin_actions);

static const gchar *gnc_plugin_load_ui_items [] =
{
    NULL,
};

GncPluginPage *
gnc_plugin_page_invoices_overview_new (void)
{
    GncPluginPageInvoicesOverview *plugin_page;
    const GList *item;

    ENTER (" ");

    /* Is there an existing page? */
    item = gnc_gobject_tracking_get_list (GNC_PLUGIN_PAGE_INVOICES_OVERVIEW_NAME);
    for ( ; item; item = g_list_next (item))
    {
        plugin_page = (GncPluginPageInvoicesOverview *)item->data;
        LEAVE ("existing page %p", plugin_page);
        return GNC_PLUGIN_PAGE (plugin_page);
    }

    plugin_page = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW (g_object_new (GNC_TYPE_PLUGIN_PAGE_INVOICES_OVERVIEW, NULL));

    LEAVE ("new page %p", plugin_page);
    return GNC_PLUGIN_PAGE (plugin_page);
}

static void
gnc_plugin_page_invoices_overview_class_init (GncPluginPageInvoicesOverviewClass *klass)
{
    GncPluginPageClass *page_class = GNC_PLUGIN_PAGE_CLASS (klass);

    page_class->plugin_name         = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW_NAME;
    page_class->tab_icon            = GNC_ICON_INVOICE;
    page_class->create_widget       = gnc_plugin_page_invoices_overview_create_widget;
    page_class->destroy_widget      = gnc_plugin_page_invoices_overview_destroy_widget;
    page_class->focus_page_function = gnc_plugin_page_invoices_overview_focus_widget;
}

static void 
gnc_plugin_page_invoices_overview_init (GncPluginPageInvoicesOverview *plugin_page)
{
    GSimpleActionGroup *simple_action_group;

    g_object_set (G_OBJECT (plugin_page),
                 "page-name",      _("Invoices"),
                 "ui-description", "gnc-plugin-page-invoices-overview.ui",
                 NULL);

    gnc_plugin_page_add_book (GNC_PLUGIN_PAGE (plugin_page), gnc_get_current_book());

    simple_action_group = gnc_plugin_page_create_action_group (GNC_PLUGIN_PAGE (plugin_page), PLUGIN_ACTIONS_NAME);
    g_action_map_add_action_entries (G_ACTION_MAP (simple_action_group),
                                     gnc_plugin_actions,
                                     gnc_plugin_n_actions,
                                     plugin_page);
}

static void
gnc_plugin_page_invoices_overview_double_click_cb (GNCQueryView *qview,
                                   gpointer item,
                                   gpointer user_data)
{
    GncPluginPageInvoicesOverview *plugin_page = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW (user_data);
    GncInvoice *invoice = GNC_INVOICE (item);

    if (!invoice) return;

    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (
        GNC_PLUGIN_PAGE (plugin_page)
    ));

    gnc_ui_invoice_edit (parent, invoice);
}

static GtkWidget *
gnc_plugin_page_invoices_overview_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageInvoicesOverview *page;
    GncPluginPageInvoicesOverviewPrivate *priv;
    QofBook *book;
    QofIdType type = GNC_INVOICE_MODULE_NAME;

    ENTER ("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW (plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW_GET_PRIVATE (page);

    book = gnc_get_current_book ();
    priv->q = qof_query_create_for (GNC_ID_INVOICE);
    qof_query_set_book (priv->q, book);

    {
      QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_TYPE, NULL);
      QofQueryPredData *type_pred = qof_query_int32_predicate (QOF_COMPARE_EQUAL, GNC_INVOICE_CUST_INVOICE);
      qof_query_add_term (priv->q, type_path, type_pred, QOF_QUERY_AND);
    }

    {
      QofQueryParamList *type_path = qof_query_build_param_list (INVOICE_TYPE, NULL);
      QofQueryPredData *type_pred = qof_query_int32_predicate (QOF_COMPARE_EQUAL, GNC_INVOICE_CUST_CREDIT_NOTE);
      qof_query_add_term (priv->q, type_path, type_pred, QOF_QUERY_OR);
    }
 
    {
      priv->columns = gnc_search_param_prepend (priv->columns, _("Billing ID"), 
                                                NULL, 
                                                type,
                                                INVOICE_BILLINGID,
                                                NULL); 
      
      priv->columns = gnc_search_param_prepend (priv->columns, _("Due"), 
                                                NULL, 
                                                type,
                                                INVOICE_DUE,
                                                NULL); 

      priv->columns = gnc_search_param_prepend (priv->columns, _("Paid"), 
                                                NULL, 
                                                type,
                                                INVOICE_IS_PAID,
                                                NULL); 

      priv->columns = gnc_search_param_prepend (priv->columns, _("Posted"), 
                                                NULL, 
                                                type,
                                                INVOICE_POSTED,
                                                NULL); 

      priv->columns = gnc_search_param_prepend (priv->columns, _("Company"), 
                                                NULL, 
                                                type,
                                                INVOICE_OWNER,
                                                OWNER_NAME,
                                                NULL); 

      /*priv->columns = gnc_search_param_prepend (priv->columns, _("Total"), 
                                                NULL, 
                                                type,
                                                "total-sub",
                                                NULL);*/ 

      priv->columns = gnc_search_param_prepend (priv->columns, _("Type"), 
                                                NULL, 
                                                type,
                                                INVOICE_TYPE_STRING,
                                                NULL); 

      priv->columns = gnc_search_param_prepend (priv->columns, _("Invoice ID"),
                                                NULL, type,
                                                INVOICE_ID,
                                                NULL);

    }

    priv->query_view = gnc_query_view_new (priv->columns, priv->q);

    g_signal_connect (GNC_QUERY_VIEW (priv->query_view), "double_click_entry",
                      G_CALLBACK (gnc_plugin_page_invoices_overview_double_click_cb),
                      plugin_page);

    priv->widget = gtk_scrolled_window_new (NULL, NULL);
    gtk_widget_set_name (GTK_WIDGET (priv->widget), "gnc-id-invoices-overview-page");

    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (priv->widget),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);

    gtk_container_add (GTK_CONTAINER (priv->widget), priv->query_view);
    gtk_widget_show_all (priv->widget);

    g_signal_connect (G_OBJECT (plugin_page), "inserted",
                      G_CALLBACK (gnc_plugin_page_inserted_cb),
                      NULL);

    LEAVE ("");
    return priv->widget;
}

static void
gnc_plugin_page_invoices_overview_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageInvoicesOverview *page;
    GncPluginPageInvoicesOverviewPrivate *priv;

    ENTER ("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW (plugin_page);
    priv = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW_GET_PRIVATE (page);

    gnc_plugin_page_disconnect_page_changed (GNC_PLUGIN_PAGE (plugin_page));

    qof_query_destroy (priv->q);

    g_list_free (priv->columns);

    LEAVE ("");
}

static gboolean
gnc_plugin_page_invoices_overview_focus_widget (GncPluginPage *plugin_page)
{
    if (GNC_IS_PLUGIN_PAGE_INVOICES_OVERVIEW (plugin_page))
    {
        gnc_main_window_update_menu_and_toolbar (GNC_MAIN_WINDOW (plugin_page->window),
                                                 plugin_page,
                                                 gnc_plugin_load_ui_items);
    }

    return FALSE;
}

static void
gnc_plugin_invoices_overview_cmd_new_invoice (GSimpleAction *simple,
                                                       GVariant      *parameter,
                                                       gpointer       user_data)
{
    GncPluginPageInvoicesOverview *plugin_page = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW (user_data);
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (
        GNC_PLUGIN_PAGE (plugin_page)
    ));

    gnc_ui_invoice_new (parent, NULL, gnc_get_current_book ());
}

static void
gnc_plugin_invoices_overview_cmd_edit_invoice (GSimpleAction *simple,
                                                        GVariant      *parameter,
                                                        gpointer       user_data)
{
    GncPluginPageInvoicesOverview *plugin_page = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW (user_data);
    GncPluginPageInvoicesOverviewPrivate *priv = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW_GET_PRIVATE (plugin_page);

    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (
        GNC_PLUGIN_PAGE (plugin_page)
    ));

    gpointer selected_entry = gnc_query_view_get_selected_entry (GNC_QUERY_VIEW (priv->query_view)); 

    if (!selected_entry) return;

    gnc_ui_invoice_edit (parent, GNC_INVOICE (selected_entry));
}

static void
gnc_plugin_invoices_overview_cmd_duplicate_invoice (GSimpleAction *simple,
                                                             GVariant      *parameter,
                                                             gpointer       user_data)
{
    GncPluginPageInvoicesOverview *plugin_page = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW (user_data);
    GncPluginPageInvoicesOverviewPrivate *priv = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW_GET_PRIVATE (plugin_page);

    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (
        GNC_PLUGIN_PAGE (plugin_page)
    ));

    gpointer selected_entry = gnc_query_view_get_selected_entry (GNC_QUERY_VIEW (priv->query_view)); 

    if (!selected_entry) return;

    gnc_ui_invoice_duplicate (parent, GNC_INVOICE (selected_entry), TRUE, NULL);
}

static void
gnc_plugin_invoices_overview_cmd_print_invoice (GSimpleAction *simple,
                                                GVariant      *parameter,
                                                gpointer       user_data)
{
    GncPluginPageInvoicesOverview *plugin_page = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW (user_data);
    GtkWindow *parent = GTK_WINDOW (gnc_plugin_page_get_window (
        GNC_PLUGIN_PAGE (plugin_page)
    ));
    GncPluginPageInvoicesOverviewPrivate *priv = GNC_PLUGIN_PAGE_INVOICES_OVERVIEW_GET_PRIVATE (plugin_page);
    gpointer selected_entry = gnc_query_view_get_selected_entry (GNC_QUERY_VIEW (priv->query_view)); 
    gchar *report_guid;
    GncPluginPage *report_page;

    if (!selected_entry) return;

    report_guid = use_default_report_template_or_change (parent);

    if (!report_guid)
        return;

    report_page = gnc_invoice_window_print_invoice (parent, GNC_INVOICE (selected_entry), report_guid);

    g_free (report_guid);

    gnc_main_window_open_page (GNC_MAIN_WINDOW (parent), report_page);

}
