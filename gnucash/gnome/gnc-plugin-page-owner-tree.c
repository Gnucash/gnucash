/*
 * gnc-plugin-page-owner-tree.c --
 *
 * Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>
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

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageOwnerTree An Owner Tree Plugin
    @{ */
/** @file gnc-plugin-page-owner-tree.c
    @brief Functions providing a page which lists owners of one type. This type
           can be vendors, customers or employees.
    @author Copyright (C) 2011 Geert Janssens <geert@kobaltwit.be>
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "swig-runtime.h"

#include "gnc-plugin.h"
#include "gnc-plugin-page-owner-tree.h"
#include "gnc-plugin-page-report.h"

#include "dialog-vendor.h"
#include "dialog-customer.h"
#include "dialog-employee.h"
#include "dialog-invoice.h"
#include "dialog-job.h"

#include "gncOwner.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-gnome-utils.h"
#include "gnc-gobject-utils.h"
#include "gnc-icons.h"
#include "gnc-session.h"
#include "gnc-tree-view-owner.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "guile-mappings.h"
#include "dialog-lot-viewer.h"
#include "dialog-object-references.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define PLUGIN_PAGE_ACCT_TREE_CM_CLASS "plugin-page-owner-tree"

#define DELETE_DIALOG_FILTER  "filter"
#define DELETE_DIALOG_OWNER "owner"

enum
{
    OWNER_SELECTED,
    LAST_SIGNAL
};

typedef struct GncPluginPageOwnerTreePrivate
{
    GtkWidget   *widget;
    GtkTreeView *tree_view;
    gint         component_id;
    GncOwnerType owner_type;
    OwnerFilterDialog fd;
} GncPluginPageOwnerTreePrivate;

#define GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_OWNER_TREE, GncPluginPageOwnerTreePrivate))

static GObjectClass *parent_class = NULL;

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_owner_tree_class_init (GncPluginPageOwnerTreeClass *klass);
static void gnc_plugin_page_owner_tree_init (GncPluginPageOwnerTree *plugin_page);
static void gnc_plugin_page_owner_tree_finalize (GObject *object);
static void gnc_plugin_page_owner_tree_selected (GObject *object, gpointer user_data);

static GtkWidget *gnc_plugin_page_owner_tree_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_owner_tree_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_owner_tree_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_owner_tree_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);

/* Callbacks */
static gboolean gnc_plugin_page_owner_tree_button_press_cb (GtkWidget *widget,
        GdkEventButton *event,
        GncPluginPage *page);
static void gnc_plugin_page_owner_tree_double_click_cb (GtkTreeView        *treeview,
        GtkTreePath        *path,
        GtkTreeViewColumn  *col,
        GncPluginPageOwnerTree *page);

static void gnc_plugin_page_owner_tree_selection_changed_cb (GtkTreeSelection *selection,
        GncPluginPageOwnerTree *page);

/* Command callbacks */
static void gnc_plugin_page_owner_tree_cmd_new_owner (GtkAction *action, GncPluginPageOwnerTree *page);
static void gnc_plugin_page_owner_tree_cmd_edit_owner (GtkAction *action, GncPluginPageOwnerTree *page);
#if 0 /* Disabled due to crash */
static void gnc_plugin_page_owner_tree_cmd_delete_owner (GtkAction *action, GncPluginPageOwnerTree *page);
#endif
static void gnc_plugin_page_owner_tree_cmd_view_filter_by (GtkAction *action, GncPluginPageOwnerTree *page);
static void gnc_plugin_page_owner_tree_cmd_new_invoice (GtkAction *action, GncPluginPageOwnerTree *page);
static void gnc_plugin_page_owner_tree_cmd_owners_report (GtkAction *action, GncPluginPageOwnerTree *plugin_page);
static void gnc_plugin_page_owner_tree_cmd_owner_report (GtkAction *action, GncPluginPageOwnerTree *plugin_page);


static guint plugin_page_signals[LAST_SIGNAL] = { 0 };


static GtkActionEntry gnc_plugin_page_owner_tree_actions [] =
{
    /* Toplevel */
    { "FakeToplevel", NULL, "", NULL, NULL, NULL },

    /* Edit menu */
    {
        "OTEditVendorAction", GNC_ICON_EDIT_ACCOUNT, N_("E_dit Vendor"), "<primary>e",
        N_("Edit the selected vendor"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_edit_owner)
    },
    {
        "OTEditCustomerAction", GNC_ICON_EDIT_ACCOUNT, N_("E_dit Customer"), "<primary>e",
        N_("Edit the selected customer"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_edit_owner)
    },
    {
        "OTEditEmployeeAction", GNC_ICON_EDIT_ACCOUNT, N_("E_dit Employee"), "<primary>e",
        N_("Edit the selected employee"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_edit_owner)
    },
    {
        "OTNewVendorAction", GNC_ICON_NEW_ACCOUNT, N_("_New Vendor..."), NULL,
        N_("Create a new vendor"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_new_owner)
    },
    {
        "OTNewCustomerAction", GNC_ICON_NEW_ACCOUNT, N_("_New Customer..."), NULL,
        N_("Create a new customer"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_new_owner)
    },
    {
        "OTNewEmployeeAction", GNC_ICON_NEW_ACCOUNT, N_("_New Employee..."), NULL,
        N_("Create a new employee"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_new_owner)
    },

#if 0 /* Disabled due to crash */
    {
        "EditDeleteOwnerAction", GNC_ICON_DELETE_ACCOUNT, N_("_Delete Owner..."), "Delete",
        N_("Delete selected owner"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_delete_owner)
    },
#endif /* Disabled due to crash */

    /* View menu */
    {
        "ViewFilterByAction", NULL, N_("_Filter By..."), NULL, NULL,
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_view_filter_by)
    },

    /* Business menu */
    {
        "OTNewBillAction", GNC_ICON_INVOICE_NEW, N_("New _Bill..."), NULL,
        N_("Create a new bill"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_new_invoice)
    },
    {
        "OTNewInvoiceAction", GNC_ICON_INVOICE_NEW, N_("New _Invoice..."), NULL,
        N_("Create a new invoice"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_new_invoice)
    },
    {
        "OTNewVoucherAction", GNC_ICON_INVOICE_NEW, N_("New _Voucher..."), NULL,
        N_("Create a new voucher"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_new_invoice)
    },
    {
        "OTVendorListingReportAction", "document-print-preview", N_("Vendor Listing"), NULL,
        N_("Show vendor aging overview for all vendors"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_owners_report)
    },
    {
        "OTCustomerListingReportAction", "document-print-preview", N_("Customer Listing"), NULL,
        N_("Show customer aging overview for all customers"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_owners_report)
    },
    {
        "OTVendorReportAction", NULL, N_("Vendor Report"), NULL,
        N_("Show vendor report"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_owner_report)
    },
    {
        "OTCustomerReportAction", NULL, N_("Customer Report"), NULL,
        N_("Show customer report"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_owner_report)
    },
    {
        "OTEmployeeReportAction", NULL, N_("Employee Report"), NULL,
        N_("Show employee report"),
        G_CALLBACK (gnc_plugin_page_owner_tree_cmd_owner_report)
    },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_page_owner_tree_n_actions = G_N_ELEMENTS (gnc_plugin_page_owner_tree_actions);


/** Actions that require an owner to be selected before they are
 *  enabled. These ones are only sensitive in a read-write book. */
static const gchar *actions_requiring_owner_rw[] =
{
    "OTEditVendorAction",
    "OTEditCustomerAction",
    "OTEditEmployeeAction",
/* FIXME disabled due to crash    "EditDeleteOwnerAction", */
    NULL
};

/** Actions that require an owner to be selected before they are
 *  enabled. These are sensitive always. */
static const gchar *actions_requiring_owner_always[] =
{
    "OTVendorReportAction",
    "OTCustomerReportAction",
    "OTEmployeeReportAction",
    NULL
};

/* This is the list of actions which are switched inactive in a read-only book. */
static const gchar* readonly_inactive_actions[] =
{
    "OTNewVendorAction",
    "OTNewCustomerAction",
    "OTNewEmployeeAction",
    "OTNewBillAction",
    "OTNewInvoiceAction",
    "OTNewVoucherAction",
    NULL
};


/** Short labels for use on the toolbar buttons. */
static action_toolbar_labels toolbar_labels[] =
{
    { "OTEditVendorAction",             N_("Edit") },
    { "OTEditCustomerAction",           N_("Edit") },
    { "OTEditEmployeeAction",           N_("Edit") },
    { "OTNewVendorAction",              N_("New") },
    { "OTNewCustomerAction",            N_("New") },
    { "OTNewEmployeeAction",            N_("New") },
    { "OTNewBillAction",                N_("New Bill") },
    { "OTNewInvoiceAction",             N_("New Invoice") },
    { "OTNewVoucherAction",             N_("New Voucher") },
    { "OTVendorListingReportAction",    N_("Vendor Listing") },
    { "OTCustomerListingReportAction",  N_("Customer Listing") },
/* FIXME disable due to crash   { "EditDeleteOwnerAction",   N_("Delete") },*/
    { NULL, NULL },
};

/** Map actions to owners. Will be used to hide actions that are
 *  not relevant on the active owner overview page
 */
typedef struct
{
    /** The name of the action. */
    const char *action_name;
    /** The owner type to show this action for */
    GncOwnerType owner_type;
} action_owners_struct;

static action_owners_struct action_owners[] =
{
    { "OTEditVendorAction",            GNC_OWNER_VENDOR },
    { "OTEditCustomerAction",          GNC_OWNER_CUSTOMER },
    { "OTEditEmployeeAction",          GNC_OWNER_EMPLOYEE },
    { "OTNewVendorAction",             GNC_OWNER_VENDOR },
    { "OTNewCustomerAction",           GNC_OWNER_CUSTOMER },
    { "OTNewEmployeeAction",           GNC_OWNER_EMPLOYEE },
    { "OTNewBillAction",               GNC_OWNER_VENDOR },
    { "OTNewInvoiceAction",            GNC_OWNER_CUSTOMER },
    { "OTNewVoucherAction",            GNC_OWNER_EMPLOYEE },
    { "OTVendorListingReportAction",   GNC_OWNER_VENDOR },
    { "OTCustomerListingReportAction", GNC_OWNER_CUSTOMER },
    { "OTVendorReportAction",          GNC_OWNER_VENDOR },
    { "OTCustomerReportAction",        GNC_OWNER_CUSTOMER },
    { "OTEmployeeReportAction",        GNC_OWNER_EMPLOYEE },
    { NULL, GNC_OWNER_NONE },
};

GncPluginPage *
gnc_plugin_page_owner_tree_new (GncOwnerType owner_type)
{
    GncPluginPageOwnerTree *plugin_page;

    GncPluginPageOwnerTreePrivate *priv;
    const GList *item;

    GtkActionGroup *action_group;
    GtkAction    *action;
    GValue        gvalue = { 0 };
    gint          i;

    g_return_val_if_fail( (owner_type != GNC_OWNER_UNDEFINED)
                          && (owner_type != GNC_OWNER_NONE), NULL);
    ENTER(" ");

    /* Is there an existing page? */
    item = gnc_gobject_tracking_get_list(GNC_PLUGIN_PAGE_OWNER_TREE_NAME);
    for ( ; item; item = g_list_next(item))
    {
        plugin_page = (GncPluginPageOwnerTree *)item->data;
        priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(plugin_page);
        if (priv->owner_type == owner_type)
        {
            LEAVE("existing %s tree page %p", gncOwnerTypeToQofIdType(owner_type), plugin_page);
            return GNC_PLUGIN_PAGE(plugin_page);
        }
    }

    plugin_page = g_object_new(GNC_TYPE_PLUGIN_PAGE_OWNER_TREE, NULL);

    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(plugin_page);
    priv->owner_type = owner_type;

    /* Hide menu and toolbar items that are not relevant for the active owner list */
    action_group = gnc_plugin_page_get_action_group(GNC_PLUGIN_PAGE(plugin_page));
    g_value_init (&gvalue, G_TYPE_BOOLEAN);
    for (i = 0; action_owners[i].action_name; i++)
    {
        action = gtk_action_group_get_action (action_group, action_owners[i].action_name);
        g_value_set_boolean (&gvalue, (priv->owner_type == action_owners[i].owner_type) );
        g_object_set_property (G_OBJECT(action), "visible", &gvalue);
    }

    LEAVE("new %s tree page %p", gncOwnerTypeToQofIdType(owner_type), plugin_page);
    return GNC_PLUGIN_PAGE(plugin_page);
}


static gboolean
gnc_plugin_page_owner_focus (GtkTreeView *tree_view)
{
    if (GTK_IS_TREE_VIEW(tree_view))
    {
        if (!gtk_widget_is_focus (GTK_WIDGET(tree_view)))
            gtk_widget_grab_focus (GTK_WIDGET(tree_view));
    }
    return FALSE;
}


/**
 * Whenever the current page is changed, if an owner page is
 * the current page, set focus on the treeview.
 */
static void
gnc_plugin_page_owner_main_window_page_changed (GncMainWindow *window,
        GncPluginPage *plugin_page, gpointer user_data)
{
    // We continue only if the plugin_page is a valid
    if (!plugin_page || !GNC_IS_PLUGIN_PAGE(plugin_page))
        return;

    if (gnc_main_window_get_current_page (window) == plugin_page)
    {
        GncPluginPageOwnerTree *page;
        GncPluginPageOwnerTreePrivate *priv;

        if (!GNC_IS_PLUGIN_PAGE_OWNER_TREE(plugin_page))
            return;

        page = GNC_PLUGIN_PAGE_OWNER_TREE(plugin_page);
        priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(page);

        // The page changed signal is emitted multiple times so we need
        // to use an idle_add to change the focus to the tree view
        g_idle_remove_by_data (GTK_TREE_VIEW (priv->tree_view));
        g_idle_add ((GSourceFunc)gnc_plugin_page_owner_focus,
                      GTK_TREE_VIEW (priv->tree_view));
    }
}

G_DEFINE_TYPE_WITH_PRIVATE(GncPluginPageOwnerTree, gnc_plugin_page_owner_tree, GNC_TYPE_PLUGIN_PAGE)

static void
gnc_plugin_page_owner_tree_class_init (GncPluginPageOwnerTreeClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

    parent_class = g_type_class_peek_parent (klass);

    object_class->finalize = gnc_plugin_page_owner_tree_finalize;

    gnc_plugin_class->tab_icon        = GNC_ICON_ACCOUNT;
    gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_OWNER_TREE_NAME;
    gnc_plugin_class->create_widget   = gnc_plugin_page_owner_tree_create_widget;
    gnc_plugin_class->destroy_widget  = gnc_plugin_page_owner_tree_destroy_widget;
    gnc_plugin_class->save_page       = gnc_plugin_page_owner_tree_save_page;
    gnc_plugin_class->recreate_page   = gnc_plugin_page_owner_tree_recreate_page;

    plugin_page_signals[OWNER_SELECTED] =
        g_signal_new ("owner_selected",
                      G_OBJECT_CLASS_TYPE (object_class),
                      G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GncPluginPageOwnerTreeClass, owner_selected),
                      NULL, NULL,
                      g_cclosure_marshal_VOID__POINTER,
                      G_TYPE_NONE, 1,
                      G_TYPE_POINTER);
}

static void
gnc_plugin_page_owner_tree_init (GncPluginPageOwnerTree *plugin_page)
{
    GtkActionGroup *action_group;
    GncPluginPageOwnerTreePrivate *priv;
    GncPluginPage *parent;

    ENTER("page %p", plugin_page);
    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(plugin_page);

    /* Init parent declared variables */
    parent = GNC_PLUGIN_PAGE(plugin_page);
    g_object_set(G_OBJECT(plugin_page),
                 "page-name",      _("Owners"),
                 "page-uri",       "default:",
                 "ui-description", "gnc-plugin-page-owner-tree-ui.xml",
                 NULL);
    g_signal_connect (G_OBJECT (plugin_page), "selected",
                      G_CALLBACK (gnc_plugin_page_owner_tree_selected), plugin_page);

    /* change me when the system supports multiple books */
    gnc_plugin_page_add_book(parent, gnc_get_current_book());

    /* Create menu and toolbar information */
    action_group =
        gnc_plugin_page_create_action_group(parent,
                                            "GncPluginPageOwnerTreeActions");
    gtk_action_group_add_actions(action_group,
                                 gnc_plugin_page_owner_tree_actions,
                                 gnc_plugin_page_owner_tree_n_actions,
                                 plugin_page);
    gnc_plugin_init_short_names (action_group, toolbar_labels);


    /* Init filter */
    priv->fd.show_inactive = TRUE;
    priv->fd.show_zero_total = TRUE;

    LEAVE("page %p, priv %p, action group %p",
          plugin_page, priv, action_group);
}

static void
gnc_plugin_page_owner_tree_finalize (GObject *object)
{
    GncPluginPageOwnerTree *page;
    GncPluginPageOwnerTreePrivate *priv;

    ENTER("object %p", object);
    page = GNC_PLUGIN_PAGE_OWNER_TREE (object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE_OWNER_TREE (page));
    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(page);
    g_return_if_fail (priv != NULL);

    G_OBJECT_CLASS (parent_class)->finalize (object);
    LEAVE(" ");
}

static void update_inactive_actions(GncPluginPage *plugin_page)
{
    GtkActionGroup *action_group;
    gboolean is_sensitive = !qof_book_is_readonly(gnc_get_current_book());

    // We are readonly - so we have to switch particular actions to inactive.
    g_return_if_fail(plugin_page);
    g_return_if_fail(GNC_IS_PLUGIN_PAGE(plugin_page));

    /* Get the action group */
    action_group = gnc_plugin_page_get_action_group(plugin_page);
    g_return_if_fail(GTK_IS_ACTION_GROUP (action_group));

    /* Set the action's sensitivity */
    gnc_plugin_update_actions (action_group, readonly_inactive_actions,
                               "sensitive", is_sensitive);
}

static void
gnc_plugin_page_owner_tree_selected (GObject *object, gpointer user_data)
{
    GncPluginPage *page = GNC_PLUGIN_PAGE (object);
    g_return_if_fail (GNC_IS_PLUGIN_PAGE (page));
    update_inactive_actions(page);
}


GncOwner *
gnc_plugin_page_owner_tree_get_current_owner (GncPluginPageOwnerTree *page)
{
    GncPluginPageOwnerTreePrivate *priv;
    GncOwner *owner;

    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(page);
    ENTER("page %p (tree view %p)", page, priv->tree_view);
    owner = gnc_tree_view_owner_get_selected_owner (GNC_TREE_VIEW_OWNER(priv->tree_view));
    if (owner == NULL)
    {
        LEAVE("no owner");
        return NULL;
    }

    LEAVE("owner %p", owner);
    return owner;
}


/* Virtual Functions */

static void
gnc_plugin_page_owner_refresh_cb (GHashTable *changes, gpointer user_data)
{
    GncPluginPageOwnerTree *page = user_data;
    GncPluginPageOwnerTreePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_OWNER_TREE(page));

    /* We're only looking for forced updates here. */
    if (changes)
        return;

    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(page);
    gtk_widget_queue_draw(priv->widget);
}

static void
gnc_plugin_page_owner_tree_close_cb (gpointer user_data)
{
    GncPluginPage *plugin_page;

    plugin_page = GNC_PLUGIN_PAGE(user_data);
    gnc_main_window_close_page(plugin_page);
}

static GtkWidget *
gnc_plugin_page_owner_tree_create_widget (GncPluginPage *plugin_page)
{
    GncPluginPageOwnerTree *page;
    GncPluginPageOwnerTreePrivate *priv;
    GncMainWindow  *window;
    GtkTreeSelection *selection;
    GtkTreeView *tree_view;
    GtkWidget *scrolled_window;
    GtkTreeViewColumn *col;
    const gchar *state_section = NULL;
    gchar* label = "";
    const gchar *style_label = NULL;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_OWNER_TREE (plugin_page);
    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(page);
    if (priv->widget != NULL)
    {
        LEAVE("widget = %p", priv->widget);
        return priv->widget;
    }

    priv->widget = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (priv->widget), FALSE);
    gtk_widget_show (priv->widget);

    // Set the style context for this page so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(priv->widget), "GncBusinessPage");

    scrolled_window = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_show (scrolled_window);
    gtk_box_pack_start (GTK_BOX (priv->widget), scrolled_window,
                        TRUE, TRUE, 0);

    tree_view = gnc_tree_view_owner_new(priv->owner_type);

    /* Show default columns */
    col = gnc_tree_view_find_column_by_name(
              GNC_TREE_VIEW(tree_view), GNC_OWNER_TREE_ID_COL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    col = gnc_tree_view_find_column_by_name(
              GNC_TREE_VIEW(tree_view), GNC_OWNER_TREE_ADDRESS_1_COL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    col = gnc_tree_view_find_column_by_name(
              GNC_TREE_VIEW(tree_view), GNC_OWNER_TREE_ADDRESS_2_COL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    col = gnc_tree_view_find_column_by_name(
              GNC_TREE_VIEW(tree_view), GNC_OWNER_TREE_PHONE_COL);
    g_object_set_data(G_OBJECT(col), DEFAULT_VISIBLE, GINT_TO_POINTER(1));
    gnc_tree_view_configure_columns(GNC_TREE_VIEW(tree_view));

    switch (priv->owner_type)
    {
    case GNC_OWNER_NONE :
    case GNC_OWNER_UNDEFINED :
        PWARN("missing owner_type");
        label = _("Unknown");
        style_label = "GncUnknown";
        break;
    case GNC_OWNER_CUSTOMER :
        label = _("Customers");
        state_section = "Customers Overview";
        style_label = "GncCustomers";
        break;
    case GNC_OWNER_JOB :
        label = _("Jobs");
        state_section = "Jobs Overview";
        style_label = "GncJobs";
        break;
    case GNC_OWNER_VENDOR :
        label = _("Vendors");
        state_section = "Vendors Overview";
        style_label = "GncVendors";
        break;
    case GNC_OWNER_EMPLOYEE :
        label = _("Employees");
        state_section = "Employees Overview";
        style_label = "GncEmployees";
        break;
    }

    // Set a secondary style context for this page so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(priv->widget), style_label);

    g_object_set(G_OBJECT(tree_view), "state-section", state_section,
                                      "show-column-menu", TRUE,
                                      NULL);

    g_object_set(G_OBJECT(plugin_page), "page-name", label, NULL);

    priv->tree_view = tree_view;
    selection = gtk_tree_view_get_selection(tree_view);
    g_signal_connect (G_OBJECT (selection), "changed",
                      G_CALLBACK (gnc_plugin_page_owner_tree_selection_changed_cb), page);
    g_signal_connect (G_OBJECT (tree_view), "button-press-event",
                      G_CALLBACK (gnc_plugin_page_owner_tree_button_press_cb), page);
    g_signal_connect (G_OBJECT (tree_view), "row-activated",
                      G_CALLBACK (gnc_plugin_page_owner_tree_double_click_cb), page);

    gtk_tree_view_set_headers_visible(tree_view, TRUE);
    gnc_plugin_page_owner_tree_selection_changed_cb (NULL, page);
    gtk_widget_show (GTK_WIDGET (tree_view));
    gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET(tree_view));

    priv->fd.tree_view = GNC_TREE_VIEW_OWNER(priv->tree_view);
    gnc_tree_view_owner_set_filter (
        GNC_TREE_VIEW_OWNER(tree_view),
        gnc_plugin_page_owner_tree_filter_owners, &priv->fd, NULL);

    priv->component_id =
        gnc_register_gui_component(PLUGIN_PAGE_ACCT_TREE_CM_CLASS,
                                   gnc_plugin_page_owner_refresh_cb,
                                   gnc_plugin_page_owner_tree_close_cb,
                                   page);
    gnc_gui_component_set_session (priv->component_id,
                                   gnc_get_current_session());

    window = GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
    g_signal_connect(window, "page_changed",
                     G_CALLBACK(gnc_plugin_page_owner_main_window_page_changed),
                     plugin_page);

    LEAVE("widget = %p", priv->widget);
    return priv->widget;
}

static void
gnc_plugin_page_owner_tree_destroy_widget (GncPluginPage *plugin_page)
{
    GncPluginPageOwnerTree *page;
    GncPluginPageOwnerTreePrivate *priv;

    ENTER("page %p", plugin_page);
    page = GNC_PLUGIN_PAGE_OWNER_TREE (plugin_page);
    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(page);

    // Remove the page focus idle function if present
    g_idle_remove_by_data (GTK_TREE_VIEW (priv->tree_view));

    if (priv->widget)
    {
        g_object_unref(G_OBJECT(priv->widget));
        priv->widget = NULL;
    }

    if (priv->component_id)
    {
        gnc_unregister_gui_component(priv->component_id);
        priv->component_id = 0;
    }

    LEAVE("widget destroyed");
}

#define OWNER_TYPE_LABEL     "OwnerType"

/** Save enough information about this owner tree page that it can
 *  be recreated next time the user starts gnucash.
 *
 *  @param plugin_page The page to save.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be written.
 *
 *  @param group_name The group name to use when saving data. */
static void
gnc_plugin_page_owner_tree_save_page (GncPluginPage *plugin_page,
                                      GKeyFile *key_file,
                                      const gchar *group_name)
{
    GncPluginPageOwnerTree *owner_page;
    GncPluginPageOwnerTreePrivate *priv;

    g_return_if_fail (GNC_IS_PLUGIN_PAGE_OWNER_TREE(plugin_page));
    g_return_if_fail (key_file != NULL);
    g_return_if_fail (group_name != NULL);

    ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
          group_name);

    owner_page = GNC_PLUGIN_PAGE_OWNER_TREE(plugin_page);
    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(owner_page);

    g_key_file_set_integer(key_file, group_name, OWNER_TYPE_LABEL,
                           priv->owner_type);

    gnc_tree_view_owner_save(GNC_TREE_VIEW_OWNER(priv->tree_view),
                             &priv->fd, key_file, group_name);
    LEAVE(" ");
}



/** Create a new owner tree page based on the information saved
 *  during a previous instantiation of gnucash.
 *
 *  @param window The window where this page should be installed.
 *
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *
 *  @param group_name The group name to use when restoring data. */
static GncPluginPage *
gnc_plugin_page_owner_tree_recreate_page (GtkWidget *window,
        GKeyFile *key_file,
        const gchar *group_name)
{
    GncPluginPageOwnerTree *owner_page;
    GncPluginPageOwnerTreePrivate *priv;
    GncPluginPage *page;
    GncOwnerType owner_type;

    g_return_val_if_fail(key_file, NULL);
    g_return_val_if_fail(group_name, NULL);
    ENTER("key_file %p, group_name %s", key_file, group_name);

    /* Create the new page. */
    owner_type = g_key_file_get_integer(key_file, group_name, OWNER_TYPE_LABEL, NULL);
    page = gnc_plugin_page_owner_tree_new(owner_type);
    owner_page = GNC_PLUGIN_PAGE_OWNER_TREE(page);
    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(owner_page);

    /* Install it now so we can then manipulate the created widget */
    gnc_main_window_open_page(GNC_MAIN_WINDOW(window), page);

    gnc_tree_view_owner_restore(GNC_TREE_VIEW_OWNER(priv->tree_view),
                                &priv->fd, key_file, group_name, owner_type);
    LEAVE(" ");
    return page;
}

/* Wrapper function to open the proper edit dialog, depending on the owner type */
static void gnc_ui_owner_edit (GtkWindow *parent, GncOwner *owner)
{
    if (NULL == owner) return;

    switch (owner->type)
    {
    case GNC_OWNER_NONE :
    case GNC_OWNER_UNDEFINED :
        break;
    case GNC_OWNER_CUSTOMER :
    {
        gnc_ui_customer_edit (parent, owner->owner.customer);
        break;
    }
    case GNC_OWNER_JOB :
    {
        gnc_ui_job_edit (parent, owner->owner.job);
        break;
    }
    case GNC_OWNER_VENDOR :
    {
        gnc_ui_vendor_edit (parent, owner->owner.vendor);
        break;
    }
    case GNC_OWNER_EMPLOYEE :
    {
        gnc_ui_employee_edit (parent, owner->owner.employee);
        break;
    }
    }
}


/* Callbacks */

/** This button press handler calls the common button press handler
 *  for all pages.  The GtkTreeView eats all button presses and
 *  doesn't pass them up the widget tree, even when doesn't do
 *  anything with them.  The only way to get access to the button
 *  presses in an owner tree page is here on the tree view widget.
 *  Button presses on all other pages are caught by the signal
 *  registered in gnc-main-window.c. */
static gboolean
gnc_plugin_page_owner_tree_button_press_cb (GtkWidget *widget,
        GdkEventButton *event,
        GncPluginPage *page)
{
    g_return_val_if_fail(GNC_IS_PLUGIN_PAGE(page), FALSE);

    ENTER("widget %p, event %p, page %p", widget, event, page);
    gnc_main_window_button_press_cb(widget, event, page);
    LEAVE(" ");

    /* Always return FALSE.  This will let the tree view callback run as
     * well which will select the item under the cursor.  By the time
     * the user sees the menu both callbacks will have run and the menu
     * actions will operate on the just-selected owner. */
    return FALSE;
}

static void
gnc_plugin_page_owner_tree_double_click_cb (GtkTreeView        *treeview,
        GtkTreePath        *path,
        GtkTreeViewColumn  *col,
        GncPluginPageOwnerTree *page)
{
    gnc_plugin_page_owner_tree_cmd_owner_report (NULL, page);
}

static void
gnc_plugin_page_owner_tree_selection_changed_cb (GtkTreeSelection *selection,
        GncPluginPageOwnerTree *page)
{
    GtkActionGroup *action_group;
    GtkTreeView *view;
    GncOwner *owner = NULL;
    gboolean sensitive;
    gboolean is_readwrite = !qof_book_is_readonly(gnc_get_current_book());

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_OWNER_TREE(page));

    if (!selection)
    {
        sensitive = FALSE;
    }
    else
    {
        g_return_if_fail(GTK_IS_TREE_SELECTION(selection));
        view = gtk_tree_selection_get_tree_view (selection);
        owner = gnc_tree_view_owner_get_selected_owner (GNC_TREE_VIEW_OWNER(view));
        sensitive = (owner != NULL);
    }

    action_group = gnc_plugin_page_get_action_group(GNC_PLUGIN_PAGE(page));
    gnc_plugin_update_actions (action_group, actions_requiring_owner_always,
                               "sensitive", sensitive);
    gnc_plugin_update_actions (action_group, actions_requiring_owner_rw,
                               "sensitive", sensitive && is_readwrite);
    g_signal_emit (page, plugin_page_signals[OWNER_SELECTED], 0, owner);
}

/******************************************************************/
/*                     Report helper functions                    */
/******************************************************************/

static int
build_aging_report (GncOwnerType owner_type)
{
    gchar *report_name = NULL;
    gchar *report_title = NULL;
    SCM args;
    SCM func;
    SCM arg;

    args = SCM_EOL;

    switch (owner_type)
    {
    case GNC_OWNER_NONE :
    case GNC_OWNER_UNDEFINED :
    case GNC_OWNER_EMPLOYEE :
    case GNC_OWNER_JOB :
    {
        return -1;
    }
    case GNC_OWNER_VENDOR :
    {
        report_name  = "gnc:payables-report-create";
        report_title = _("Vendor Listing");
        break;
    }
    case GNC_OWNER_CUSTOMER :
    {
        report_name = "gnc:receivables-report-create";
        report_title = _("Customer Listing");
        break;
    }
    }

    /* Find report generator function in guile */
    func = scm_c_eval_string (report_name);
    g_return_val_if_fail (scm_is_procedure (func), -1);

    /* Option Show zero's ? - Yes for the listing report */
    arg = SCM_BOOL_T;
    args = scm_cons (arg, args);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);

    /* Option Report title */
    arg = scm_from_utf8_string (report_title);
    args = scm_cons (arg, args);

    /* Option Account - Using False to select default account
     *
     * XXX I'm not sure if it would make sense to use another
     *     account than default */
    arg = SCM_BOOL_F;
    args = scm_cons (arg, args);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);


    /* Apply the function to the args */
    arg = scm_apply (func, args, SCM_EOL);
    g_return_val_if_fail (scm_is_exact (arg), -1);

    return scm_to_int (arg);
}

static int build_owner_report (GncOwner *owner, Account *acc)
{
    SCM args;
    SCM func;
    SCM arg;

    g_return_val_if_fail (owner, -1);

    args = SCM_EOL;

    func = scm_c_eval_string ("gnc:owner-report-create");
    g_return_val_if_fail (scm_is_procedure (func), -1);

    if (acc)
    {
        swig_type_info * qtype = SWIG_TypeQuery("_p_Account");
        g_return_val_if_fail (qtype, -1);

        arg = SWIG_NewPointerObj(acc, qtype, 0);
        g_return_val_if_fail (arg != SCM_UNDEFINED, -1);
        args = scm_cons (arg, args);
    }
    else
    {
        args = scm_cons (SCM_BOOL_F, args);
    }

    arg = SWIG_NewPointerObj(owner, SWIG_TypeQuery("_p__gncOwner"), 0);
    g_return_val_if_fail (arg != SCM_UNDEFINED, -1);
    args = scm_cons (arg, args);

    /* Apply the function to the args */
    arg = scm_apply (func, args, SCM_EOL);
    g_return_val_if_fail (scm_is_exact (arg), -1);
    return scm_to_int (arg);
}


/************************************************************/
/*                     Command callbacks                    */
/************************************************************/

static void
gnc_plugin_page_owner_tree_cmd_new_owner (GtkAction *action, GncPluginPageOwnerTree *page)
{
    GncPluginPageOwnerTreePrivate *priv;
    GtkWindow *parent;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_OWNER_TREE(page));

    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE (page);
    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));

    switch (priv->owner_type)
    {
    case GNC_OWNER_NONE :
    case GNC_OWNER_UNDEFINED :
        break;
    case GNC_OWNER_CUSTOMER :
    {
        gnc_ui_customer_new (parent, gnc_get_current_book ());
        break;
    }
    case GNC_OWNER_JOB :
    {
        /* XXX currently not properly implemented, so disabled for now
        gnc_ui_job_new (owner, gnc_get_current_book ()); */
        break;
    }
    case GNC_OWNER_VENDOR :
    {
        gnc_ui_vendor_new (parent, gnc_get_current_book ());
        break;
    }
    case GNC_OWNER_EMPLOYEE :
    {
        gnc_ui_employee_new (parent, gnc_get_current_book ());
        break;
    }
    }
}

static void
gnc_plugin_page_owner_tree_cmd_edit_owner (GtkAction *action, GncPluginPageOwnerTree *page)
{
    GtkWindow *parent;
    GncOwner *owner = gnc_plugin_page_owner_tree_get_current_owner (page);
    if (NULL == owner) return;

    ENTER("action %p, page %p", action, page);

    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    gnc_ui_owner_edit (parent, owner);

    LEAVE(" ");
}

#if 0 /* Disabled due to crash */
static void
gnc_plugin_page_owner_tree_cmd_delete_owner (GtkAction *action, GncPluginPageOwnerTree *page)
{
    GncOwner *owner = gnc_plugin_page_owner_tree_get_current_owner (page);
    gchar *owner_name;
    GtkWidget *window;
    GtkWidget *dialog = NULL;
    gint response;
    GList* list;

    if (NULL == owner) return;

    /* If the owner has objects referring to it, show the list - the owner can't be deleted until these
       references are dealt with. */
    list = qof_instance_get_referring_object_list(QOF_INSTANCE(gncOwnerGetUndefined(owner)));
    if (list != NULL)
    {
#define EXPLANATION "The list below shows objects which make use of the owner which you want to delete.\nBefore you can delete it, you must either delete those objects or else modify them so they make use\nof another owner"

        gnc_ui_object_references_show( _(EXPLANATION), list);
        g_list_free(list);
        return;
    }

    window = gnc_plugin_page_get_window(GNC_PLUGIN_PAGE(page));
    owner_name = g_strdup (gncOwnerGetName(owner));
    if (!owner_name)
    {
        owner_name = g_strdup (_("(no name)"));
    }

    /*
     * Present a message to the user which specifies what will be
     * deleted, then ask for verification.
     */
    {
        char *message = g_strdup_printf(_("The owner %s will be deleted.\nAre you sure you want to do this?"), owner_name);

        dialog =  gtk_message_dialog_new(GTK_WINDOW(window),
                                         GTK_DIALOG_DESTROY_WITH_PARENT,
                                         GTK_MESSAGE_QUESTION,
                                         GTK_BUTTONS_NONE,
                                         "%s", message);
        g_free(message);
        gtk_dialog_add_buttons(GTK_DIALOG(dialog),
                               _("_Cancel"), GTK_RESPONSE_CANCEL,
                               _("_Delete"), GTK_RESPONSE_ACCEPT,
                               (gchar *)NULL);
        gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_CANCEL);
        response = gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);

        if (GTK_RESPONSE_ACCEPT == response)
        {
            /* FIXME The code below results in a crash.
             *       The corresponding menu item/toolbar button is disabled until this is fixed. */
            gnc_set_busy_cursor(NULL, TRUE);
            gnc_suspend_gui_refresh ();
            gncOwnerBeginEdit (owner);
            gncOwnerDestroy (owner);
            gnc_resume_gui_refresh ();
            gnc_unset_busy_cursor(NULL);
        }
    }
    g_free(owner_name);
}
#endif /* Disabled due to crash */

/*********************/

static void
gnc_plugin_page_owner_tree_cmd_view_filter_by (GtkAction *action,
        GncPluginPageOwnerTree *page)
{
    GncPluginPageOwnerTreePrivate *priv;

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_OWNER_TREE(page));
    ENTER("(action %p, page %p)", action, page);

    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(page);
    owner_filter_dialog_create(&priv->fd, GNC_PLUGIN_PAGE(page));
    LEAVE(" ");
}


static void
gnc_plugin_page_owner_tree_cmd_new_invoice (GtkAction *action,
        GncPluginPageOwnerTree *page)
{
    GncPluginPageOwnerTreePrivate *priv;
    GncOwner current_owner;
    GtkWindow *parent;

    ENTER("action %p, page %p", action, page);

    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(page);
    switch (priv->owner_type)
    {
    case GNC_OWNER_NONE :
    case GNC_OWNER_UNDEFINED :
        gncOwnerInitUndefined(&current_owner, NULL);
        break;
    case GNC_OWNER_CUSTOMER :
    {
        gncOwnerInitCustomer(&current_owner,
                             gncOwnerGetCustomer(gnc_plugin_page_owner_tree_get_current_owner (page)) );
        break;
    }
    case GNC_OWNER_JOB :
    {
        gncOwnerInitJob(&current_owner,
                        gncOwnerGetJob(gnc_plugin_page_owner_tree_get_current_owner (page)) );
        break;
    }
    case GNC_OWNER_VENDOR :
    {
        gncOwnerInitVendor(&current_owner,
                           gncOwnerGetVendor(gnc_plugin_page_owner_tree_get_current_owner (page)) );
        break;
    }
    case GNC_OWNER_EMPLOYEE :
    {
        gncOwnerInitEmployee(&current_owner,
                             gncOwnerGetEmployee(gnc_plugin_page_owner_tree_get_current_owner (page)) );
        break;
    }
    }

    parent = GTK_WINDOW (gnc_plugin_page_get_window (GNC_PLUGIN_PAGE (page)));
    if (gncOwnerGetType(&current_owner) != GNC_OWNER_UNDEFINED)
        gnc_ui_invoice_new (parent, &current_owner, gnc_get_current_book ());

    LEAVE(" ");
}

static void
gnc_plugin_page_owner_tree_cmd_owners_report (GtkAction *action,
        GncPluginPageOwnerTree *plugin_page)
{
    GncPluginPageOwnerTreePrivate *priv;
    int id;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_OWNER_TREE(plugin_page));

    priv = GNC_PLUGIN_PAGE_OWNER_TREE_GET_PRIVATE(plugin_page);
    id = build_aging_report (priv->owner_type);
    if (id >= 0)
    {
        GncMainWindow *window;
        window = GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
        gnc_main_window_open_report(id, window);
    }

    LEAVE(" ");
}

static void
gnc_plugin_page_owner_tree_cmd_owner_report (GtkAction *action,
        GncPluginPageOwnerTree *plugin_page)
{
    GncOwner *current_owner;
    int id;

    ENTER("(action %p, plugin_page %p)", action, plugin_page);

    g_return_if_fail(GNC_IS_PLUGIN_PAGE_OWNER_TREE(plugin_page));

    current_owner = gnc_plugin_page_owner_tree_get_current_owner (plugin_page);
    id = build_owner_report (current_owner, NULL);
    if (id >= 0)
    {
        GncMainWindow *window;
        window = GNC_MAIN_WINDOW(GNC_PLUGIN_PAGE(plugin_page)->window);
        gnc_main_window_open_report(id, window);
    }

    LEAVE(" ");
}

/** @} */
/** @} */
