/* 
 * gnc-plugin-page-sx-list.c
 *
 * Copyright (C) 2006 Josh Sled <jsled@asynchronous.org>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
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

/* todo:
 * - ui, actions, menus
 * - icon
 */

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageSxList A SX List Plugin Page
    @{ */
/** @brief Functions providing the SX List as a plugin page.
    @author Josh Sled <jsled@asynchronous.org>
*/

#include "config.h"

#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <glade/glade-xml.h>
#ifndef HAVE_GLIB26
#include "gkeyfile.h"
#endif
#include "gnc-exp-parser.h"
#include "gnc-engine.h"
#include "Transaction.h"
#include "Split.h"
#include "gnc-commodity.h"
#include "gnc-event.h"
#include "gnc-dense-cal.h"
#include "gnc-icons.h"
#include "gnc-plugin-page-sx-list.h"
#include "gnc-ui-util.h"
#include "gnc-main-window.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "SX-book.h"
#include "gnc-book.h"
#include "dialog-sx-editor.h"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#define PLUGIN_PAGE_SX_LIST_CM_CLASS "plugin-page-sx-list"
#define GCONF_SECTION "window/pages/sx_list"

typedef struct GncPluginPageSxListPrivate
{
     GtkWidget* widget;
     gint gnc_component_id;
     gint gppsl_event_handler_id;

     GladeXML* gxml;
     GncSxInstanceDenseCalAdapter *dense_cal_model;
     GncDenseCal* gdcal;

     GncSxInstanceModel* instances;
     GncSxListTreeModelAdapter* tree_model;
     GtkTreeView* tree_view;

} GncPluginPageSxListPrivate;

#define GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(o)  \
   (G_TYPE_INSTANCE_GET_PRIVATE ((o), GNC_TYPE_PLUGIN_PAGE_SX_LIST, GncPluginPageSxListPrivate))

static GObjectClass *parent_class = NULL;

/* ------------------------------------------------------------ */

GType gnc_sx_instance_model_get_type(void);
static void gnc_sx_instance_model_class_init (GncSxInstanceModelClass *klass);
static void gnc_sx_instance_model_init(GTypeInstance *instance, gpointer klass);
static GncSxInstanceModel* gnc_sx_instance_model_new(void);

static GncSxInstance* gnc_sx_instance_new(GncSxInstances *parent, GncSxInstanceType type, GDate *date, gint sequence_num);

static void sxsl_get_sx_vars(SchedXaction *sx, GHashTable *var_hash);
static gint _get_vars_helper(Transaction *txn, void *var_hash_data);
static void clear_variable_numerics(gpointer key, gpointer value, gpointer data);
static int parse_vars_from_formula(const char *formula, GHashTable *varHash, gnc_numeric *result);

static void gnc_util_copy_hash_table(GHashTable *from, GHashTable *to);

static void _gnc_sx_instance_event_handler(QofEntity *ent, QofEventId event_type, gpointer user_data, gpointer evt_data);

#define GNC_TYPE_SX_INSTANCE_MODEL	      (gnc_sx_instance_model_get_type ())
#define GNC_SX_INSTANCE_MODEL(obj)	      (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_INSTANCE_MODEL, GncSxInstanceModel))
#define GNC_SX_INSTANCE_MODEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_INSTANCE_MODEL, GncSxInstanceModelClass))
#define GNC_IS_SX_INSTANCE_MODEL(obj)	      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_INSTANCE_MODEL))
#define GNC_IS_SX_INSTANCE_MODEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_INSTANCE_MODEL))
#define GNC_SX_INSTANCE_MODEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_INSTANCE_MODEL, GncSxInstanceModelClass))

GncSxInstanceModel* gnc_sx_get_instances(GDate *range_end);

/* ------------------------------------------------------------ */

typedef struct _GncSxInstanceDenseCalAdapterClass
{
  GObjectClass parent;
} GncSxInstanceDenseCalAdapterClass;

struct _GncSxInstanceDenseCalAdapter 
{
  GObject parent;

  GncSxInstanceModel *instances;
};

GncSxInstanceDenseCalAdapter* gnc_sx_instance_dense_cal_adapter_new(GncSxInstanceModel *instances);
GType gnc_sx_instance_dense_cal_adapter_get_type(void);
static GList* gsidca_get_contained(GncDenseCalModel *model);
static gchar* gsidca_get_name(GncDenseCalModel *model, guint tag);
static gchar* gsidca_get_info(GncDenseCalModel *model, guint tag);
static gint gsidca_get_instance_count(GncDenseCalModel *model, guint tag);
static void gsidca_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date);

#define GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER	          (gnc_sx_instance_dense_cal_adapter_get_type ())
#define GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(obj)	          (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, GncSxInstanceDenseCalAdapter))
#define GNC_SX_INSTANCE_DENSE_CAL_ADAPTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, GncSxInstanceDenseCalAdapterClass))
#define GNC_IS_SX_INSTANCE_DENSE_CAL_ADAPTER(obj)	  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER))
#define GNC_IS_SX_INSTANCE_DENSE_CAL_ADAPTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER))
#define GNC_SX_INSTANCE_DENSE_CAL_ADAPTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, GncSxInstanceDenseCalAdapterClass))

/* ------------------------------------------------------------ */

struct _GncSxListTreeModelAdapter
{
     GObject parent;

     /* protected */
     GncSxInstanceModel *instances;
     GtkTreeStore *real;
};

typedef struct _GncSxListTreeModelAdapterClass
{
     GObjectClass parent;
} GncSxListTreeModelAdapterClass;

GType gnc_sx_list_tree_model_adapter_get_type(void);
static void gnc_sx_list_tree_model_adapter_class_init(GncSxListTreeModelAdapterClass *klass);
static void gnc_sx_list_tree_model_adapter_interface_init(gpointer g_iface, gpointer iface_data);
static void gnc_sx_list_tree_model_adapter_init(GTypeInstance *instance, gpointer klass);
GncSxListTreeModelAdapter* gnc_sx_list_tree_model_adapter_new(GncSxInstanceModel *instances);

GncSxInstances* gnc_sx_list_tree_model_adapter_get_sx_instances(GncSxListTreeModelAdapter *model, GtkTreeIter *iter);

#define GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER	       (gnc_sx_list_tree_model_adapter_get_type ())
#define GNC_SX_LIST_TREE_MODEL_ADAPTER(obj)	       (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, GncSxListTreeModelAdapter))
#define GNC_SX_LIST_TREE_MODEL_ADAPTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, GncSxListTreeModelAdapterClass))
#define GNC_IS_SX_LIST_TREE_MODEL_ADAPTER(obj)	       (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER))
#define GNC_IS_SX_LIST_TREE_MODEL_ADAPTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER))
#define GNC_SX_LIST_TREE_MODEL_ADAPTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, GncSxListTreeModelAdapterClass))

/* ------------------------------------------------------------ */

typedef gpointer (*GMapFunc)(gpointer data, gpointer user_data);
static GList*
g_list_map(GList* list, GMapFunc fn, gpointer user_data)
{
     GList *rtn = NULL;
     for (; list != NULL; list = list->next)
     {
          rtn = g_list_append(rtn, (*fn)(list->data, user_data));
     }
     return rtn;
}

/************************************************************
 *                        Prototypes                        *
 ************************************************************/
/* Plugin Actions */
static void gnc_plugin_page_sx_list_class_init (GncPluginPageSxListClass *klass);
static void gnc_plugin_page_sx_list_init (GncPluginPageSxList *plugin_page);
static void gnc_plugin_page_sx_list_finalize (GObject *object);

static GtkWidget *gnc_plugin_page_sx_list_create_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_sx_list_destroy_widget (GncPluginPage *plugin_page);
static void gnc_plugin_page_sx_list_save_page (GncPluginPage *plugin_page, GKeyFile *file, const gchar *group);
static GncPluginPage *gnc_plugin_page_sx_list_recreate_page (GtkWidget *window, GKeyFile *file, const gchar *group);

static void gppsl_event_handler(QofEntity *ent, QofEventId event_type, gpointer user_data, gpointer evt_data);

static void gppsl_row_activated_cb(GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer user_data);

/* Callbacks */
static void gnc_plugin_page_sx_list_cmd_new(GtkAction *action, GncPluginPageSxList *page);
static void gnc_plugin_page_sx_list_cmd_edit(GtkAction *action, GncPluginPageSxList *page);
static void gnc_plugin_page_sx_list_cmd_delete(GtkAction *action, GncPluginPageSxList *page);

/* Command callbacks */

static GtkActionEntry gnc_plugin_page_sx_list_actions [] = {
     { "SxListAction", NULL, N_("Scheduled"), NULL, NULL, NULL },
     { "SxListNewAction", GNC_STOCK_NEW_ACCOUNT, N_("New"), NULL,
       N_("Create a new scheduled transaction"), G_CALLBACK(gnc_plugin_page_sx_list_cmd_new) },
     { "SxListEditAction", GNC_STOCK_EDIT_ACCOUNT, N_("Edit"), NULL,
       N_("Edit the selected scheduled transaction"), G_CALLBACK(gnc_plugin_page_sx_list_cmd_edit) },
     { "SxListDeleteAction", GNC_STOCK_DELETE_ACCOUNT, N_("Delete"), NULL,
       N_("Delete the selected scheduled transaction"), G_CALLBACK(gnc_plugin_page_sx_list_cmd_delete) },
};
/** The number of actions provided by this plugin. */
static guint gnc_plugin_page_sx_list_n_actions = G_N_ELEMENTS (gnc_plugin_page_sx_list_actions);

GType
gnc_plugin_page_sx_list_get_type (void)
{
     static GType gnc_plugin_page_sx_list_type = 0;

     if (gnc_plugin_page_sx_list_type == 0) {
          static const GTypeInfo our_info = {
               sizeof (GncPluginPageSxListClass),
               NULL,
               NULL,
               (GClassInitFunc) gnc_plugin_page_sx_list_class_init,
               NULL,
               NULL,
               sizeof (GncPluginPageSxList),
               0,
               (GInstanceInitFunc) gnc_plugin_page_sx_list_init
          };
		
          gnc_plugin_page_sx_list_type = g_type_register_static (GNC_TYPE_PLUGIN_PAGE,
                                                                 GNC_PLUGIN_PAGE_SX_LIST_NAME,
                                                                 &our_info, 0);
     }

     return gnc_plugin_page_sx_list_type;
}

GncPluginPage *
gnc_plugin_page_sx_list_new (void)
{
     GncPluginPageSxList *plugin_page;
     plugin_page = g_object_new(GNC_TYPE_PLUGIN_PAGE_SX_LIST, NULL);
     return GNC_PLUGIN_PAGE(plugin_page);
}

static void
gnc_plugin_page_sx_list_class_init (GncPluginPageSxListClass *klass)
{
     GObjectClass *object_class = G_OBJECT_CLASS(klass);
     GncPluginPageClass *gnc_plugin_class = GNC_PLUGIN_PAGE_CLASS(klass);

     parent_class = g_type_class_peek_parent(klass);

     object_class->finalize = gnc_plugin_page_sx_list_finalize;

     gnc_plugin_class->tab_icon        = GNC_STOCK_ACCOUNT;
     gnc_plugin_class->plugin_name     = GNC_PLUGIN_PAGE_SX_LIST_NAME;
     gnc_plugin_class->create_widget   = gnc_plugin_page_sx_list_create_widget;
     gnc_plugin_class->destroy_widget  = gnc_plugin_page_sx_list_destroy_widget;
     gnc_plugin_class->save_page       = gnc_plugin_page_sx_list_save_page;
     gnc_plugin_class->recreate_page   = gnc_plugin_page_sx_list_recreate_page;

     g_type_class_add_private(klass, sizeof(GncPluginPageSxListPrivate));
}

static void
gnc_plugin_page_sx_list_init (GncPluginPageSxList *plugin_page)
{
     GtkActionGroup *action_group;
     GncPluginPageSxListPrivate *priv;
     GncPluginPage *parent;

     ENTER("page %p", plugin_page);
     priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(plugin_page);

     /* Init parent declared variables */
     parent = GNC_PLUGIN_PAGE(plugin_page);
     g_object_set(G_OBJECT(plugin_page),
                  "page-name",      _("Scheduled Transactions"),
                  "page-uri",       "default:",
                  "ui-description", "gnc-plugin-page-sx-list-ui.xml",
                  NULL);

     /* change me when the system supports multiple books */
     gnc_plugin_page_add_book(parent, gnc_get_current_book());

     /* Create menu and toolbar information */
     action_group =
          gnc_plugin_page_create_action_group(parent,
                                              "GncPluginPageSxListActions");
     gtk_action_group_add_actions(action_group,
                                  gnc_plugin_page_sx_list_actions,
                                  gnc_plugin_page_sx_list_n_actions,
                                  plugin_page);
     /* gnc_plugin_init_short_names (action_group, toolbar_labels); */

     LEAVE("page %p, priv %p, action group %p",
           plugin_page, priv, action_group);
}

static void
gnc_plugin_page_sx_list_finalize (GObject *object)
{
     GncPluginPageSxList *page;
     GncPluginPageSxListPrivate *priv;

     ENTER("object %p", object);
     page = GNC_PLUGIN_PAGE_SX_LIST (object);
     g_return_if_fail (GNC_IS_PLUGIN_PAGE_SX_LIST (page));
     priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
     g_return_if_fail (priv != NULL);

     G_OBJECT_CLASS (parent_class)->finalize (object);
     LEAVE(" ");
}

/* Virtual Functions */
static void
gnc_plugin_page_sx_list_refresh_cb (GHashTable *changes, gpointer user_data)
{
     GncPluginPageSxList *page = user_data;
     GncPluginPageSxListPrivate *priv;

     g_return_if_fail(GNC_IS_PLUGIN_PAGE_SX_LIST(page));

     /* We're only looking for forced updates here. */
     if (changes)
          return;

     priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
     gtk_widget_queue_draw(priv->widget);
}

static void
gnc_plugin_page_sx_list_close_cb (gpointer user_data)
{
     GncPluginPage *plugin_page;
     GncPluginPageSxList *page;

     plugin_page = GNC_PLUGIN_PAGE(user_data);
     page = GNC_PLUGIN_PAGE_SX_LIST (plugin_page);
     gnc_main_window_close_page(plugin_page);
}

static void
gppsl_selection_changed_cb(GtkTreeSelection *selection, gpointer user_data)
{
     GncPluginPage *page;
     GtkAction *edit_action, *delete_action;
     gboolean selection_state = TRUE;

     page = GNC_PLUGIN_PAGE(user_data);
     edit_action = gnc_plugin_page_get_action(page, "SxListEditAction");
     delete_action = gnc_plugin_page_get_action(page, "SxListDeleteAction");
     selection_state
          = gtk_tree_selection_count_selected_rows(selection) == 0
          ? FALSE
          : TRUE;
     gtk_action_set_sensitive(edit_action, selection_state);
     gtk_action_set_sensitive(delete_action, selection_state);
}

static GtkWidget *
gnc_plugin_page_sx_list_create_widget (GncPluginPage *plugin_page)
{
     GncPluginPageSxList *page;
     GncPluginPageSxListPrivate *priv;

     ENTER("page %p", plugin_page);
     page = GNC_PLUGIN_PAGE_SX_LIST(plugin_page);
     priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
     if (priv->widget != NULL) {
          LEAVE("widget = %p", priv->widget);
          return priv->widget;
     }

     priv->gxml = gnc_glade_xml_new("sched-xact.glade", "sx-list-vbox");
     priv->widget = glade_xml_get_widget(priv->gxml, "sx-list-vbox");

     {
          GDate end;
          g_date_clear(&end, 1);
          g_date_set_time_t(&end, time(NULL));
          g_date_add_years(&end, 1);
          priv->instances = GNC_SX_INSTANCE_MODEL(gnc_sx_get_instances(&end));
     }

     {
          GtkAction *edit_action, *delete_action;
          edit_action = gnc_plugin_page_get_action(GNC_PLUGIN_PAGE(page), "SxListEditAction");
          delete_action = gnc_plugin_page_get_action(GNC_PLUGIN_PAGE(page), "SxListDeleteAction");
          gtk_action_set_sensitive(edit_action, FALSE);
          gtk_action_set_sensitive(delete_action, FALSE);
     }

     {
          GtkCellRenderer *renderer;
          GtkTreeViewColumn *column;
          GtkTreeSelection *selection;

          priv->tree_model = gnc_sx_list_tree_model_adapter_new(priv->instances);
          priv->tree_view = GTK_TREE_VIEW(glade_xml_get_widget(priv->gxml, "sx_list"));
          gtk_tree_view_set_model(priv->tree_view, GTK_TREE_MODEL(priv->tree_model));

          renderer = gtk_cell_renderer_text_new();
          column = gtk_tree_view_column_new_with_attributes("Name", renderer, "text", 0, NULL);
          gtk_tree_view_append_column(priv->tree_view, column);

          renderer = gtk_cell_renderer_text_new();
          column = gtk_tree_view_column_new_with_attributes("Frequency", renderer, "text", 1, NULL);
          gtk_tree_view_append_column(priv->tree_view, column);

          renderer = gtk_cell_renderer_text_new();
          column = gtk_tree_view_column_new_with_attributes("Last Occur", renderer, "text", 2, NULL);
          gtk_tree_view_append_column(priv->tree_view, column);

          renderer = gtk_cell_renderer_text_new();
          column = gtk_tree_view_column_new_with_attributes("Next Occur", renderer, "text", 3, NULL);
          gtk_tree_view_append_column(priv->tree_view, column);

          selection = gtk_tree_view_get_selection(priv->tree_view);
          gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
          g_signal_connect(G_OBJECT(selection), "changed", (GCallback)gppsl_selection_changed_cb, (gpointer)page);

          g_signal_connect(G_OBJECT(priv->tree_view), "row-activated", (GCallback)gppsl_row_activated_cb, (gpointer)page);
     }

     {
          GtkWidget *w;

          priv->dense_cal_model = gnc_sx_instance_dense_cal_adapter_new(GNC_SX_INSTANCE_MODEL(priv->instances));
          priv->gdcal = GNC_DENSE_CAL(gnc_dense_cal_new_with_model(GNC_DENSE_CAL_MODEL(priv->dense_cal_model)));
          gnc_dense_cal_set_months_per_col(priv->gdcal, 4);
          gnc_dense_cal_set_num_months(priv->gdcal, 12);

          w = glade_xml_get_widget(priv->gxml, "upcoming_cal_hbox");
          gtk_container_add(GTK_CONTAINER(w), GTK_WIDGET(priv->gdcal));
          gtk_widget_show_all(w);
     }

     priv->gppsl_event_handler_id = qof_event_register_handler(gppsl_event_handler, page);
     gnc_register_gui_component("plugin-page-sx-list",
                                gnc_plugin_page_sx_list_refresh_cb,
                                gnc_plugin_page_sx_list_close_cb,
                                page);

     /* @@fixme */
     /* gnc_restore_window_size(SX_LIST_GCONF_SECTION, GTK_WINDOW(priv->widget)); */

     return priv->widget;
}

static void
gnc_plugin_page_sx_list_destroy_widget (GncPluginPage *plugin_page)
{
     GncPluginPageSxList *page;
     GncPluginPageSxListPrivate *priv;

     ENTER("page %p", plugin_page);
     page = GNC_PLUGIN_PAGE_SX_LIST (plugin_page);
     priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

     if (priv->widget) {
          g_object_unref(G_OBJECT(priv->widget));
          priv->widget = NULL;
     }

     if (priv->gnc_component_id) {
          gnc_unregister_gui_component(priv->gnc_component_id);
          priv->gnc_component_id = 0;
     }

     LEAVE("widget destroyed");
}

static void
gppsl_event_handler(QofEntity *ent, QofEventId event_type, gpointer user_data, gpointer evt_data)
{
     /* if (type != SX)
        return; */
     /* - correlate SX to tree_store data */
     /* - update || add || remove */
     return;
}

/**
 * Save enough information about this page that it can be recreated next time
 * the user starts gnucash.
 * @param page The page to save.
 * @param key_file A pointer to the GKeyFile data structure where the
 * page information should be written.
 * @param group_name The group name to use when saving data.
 **/
static void
gnc_plugin_page_sx_list_save_page (GncPluginPage *plugin_page,
                                   GKeyFile *key_file,
                                   const gchar *group_name)
{
     GncPluginPageSxList *page;
     GncPluginPageSxListPrivate *priv;
	
     g_return_if_fail(GNC_IS_PLUGIN_PAGE_SX_LIST(plugin_page));
     g_return_if_fail(key_file != NULL);
     g_return_if_fail(group_name != NULL);

     ENTER("page %p, key_file %p, group_name %s", plugin_page, key_file,
           group_name);

     page = GNC_PLUGIN_PAGE_SX_LIST(plugin_page);
     priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

#if 0
     gnc_tree_view_account_save(GNC_TREE_VIEW_ACCOUNT(priv->tree_view), 
                                &priv->fd, key_file, group_name);
#endif /* 0 */
     LEAVE(" ");
}



/**
 * Create a new sx list page based on the information saved during a previous
 * instantiation of gnucash.
 *  @param window The window where this page should be installed.
 *  @param key_file A pointer to the GKeyFile data structure where the
 *  page information should be read.
 *  @param group_name The group name to use when restoring data.
 **/
static GncPluginPage *
gnc_plugin_page_sx_list_recreate_page (GtkWidget *window,
                                       GKeyFile *key_file,
                                       const gchar *group_name)
{
     GncPluginPageSxList *page;
     GncPluginPageSxListPrivate *priv;
	
     g_return_val_if_fail(key_file, NULL);
     g_return_val_if_fail(group_name, NULL);
     ENTER("key_file %p, group_name %s", key_file, group_name);

     /* Create the new page. */
     page = GNC_PLUGIN_PAGE_SX_LIST(gnc_plugin_page_sx_list_new());
     priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);

     /* Install it now so we can them manipulate the created widget */
     gnc_main_window_open_page(GNC_MAIN_WINDOW(window), GNC_PLUGIN_PAGE(page));

#if 0
     gnc_tree_view_account_restore(GNC_TREE_VIEW_ACCOUNT(priv->tree_view), 
                                   &priv->fd, key_file, group_name);
#endif /* 0 */
     LEAVE(" ");
     return GNC_PLUGIN_PAGE(page);
}


/* Callbacks */

static SchedXaction*
_sx_for_path(gpointer data, gpointer user_data)
{
     GtkTreeIter iter;
     GncSxListTreeModelAdapter *model = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
     gtk_tree_model_get_iter(GTK_TREE_MODEL(model), &iter, (GtkTreePath*)data);
     return gnc_sx_list_tree_model_adapter_get_sx_instances(model, &iter)->sx;
}

static void
gnc_plugin_page_sx_list_cmd_new(GtkAction *action, GncPluginPageSxList *page)
{
     FreqSpec *fs;
     SchedXaction *new_sx;
     gboolean new_sx_flag = TRUE;

     new_sx = xaccSchedXactionMalloc(gnc_get_current_book());
     /* Give decent initial FreqSpec for SX */
     fs = xaccSchedXactionGetFreqSpec(new_sx);
     {
       GDate *now;
       now = g_date_new();
       g_date_set_time_t(now, time(NULL));
       xaccFreqSpecSetMonthly(fs, now, 1);
       xaccFreqSpecSetUIType(fs, UIFREQ_MONTHLY);
       g_date_free(now);
     }
     gnc_ui_scheduled_xaction_editor_dialog_create(new_sx, new_sx_flag);
}

static void
_edit_sx(gpointer data, gpointer user_data)
{
     gnc_ui_scheduled_xaction_editor_dialog_create((SchedXaction*)data, FALSE);
}

static void
gnc_plugin_page_sx_list_cmd_edit(GtkAction *action, GncPluginPageSxList *page)
{
     GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
     GtkTreeSelection *selection;
     GList *selected_paths, *to_edit;
     GtkTreeModel *model;

     selection = gtk_tree_view_get_selection(priv->tree_view);
     selected_paths = gtk_tree_selection_get_selected_rows(selection, &model);
     if (g_list_length(selected_paths) == 0)
     {
          PERR("no selection edit.");
          return;
     }

     to_edit = g_list_map(selected_paths, (GMapFunc)_sx_for_path, model);
     g_list_foreach(to_edit, (GFunc)_edit_sx, NULL);
     g_list_free(to_edit);
     g_list_foreach(selected_paths, (GFunc)gtk_tree_path_free, NULL);
     g_list_free(selected_paths);
}

static void
gppsl_row_activated_cb(GtkTreeView *tree_view,
                       GtkTreePath *path,
                       GtkTreeViewColumn *column,
                       gpointer user_data)
{
     GncPluginPageSxList *page = GNC_PLUGIN_PAGE_SX_LIST(user_data);
     GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
     SchedXaction *sx = _sx_for_path(path, priv->tree_model);
     gnc_ui_scheduled_xaction_editor_dialog_create(sx, FALSE);
}


static void
_destroy_sx(gpointer data, gpointer user_data)
{
     SchedXactions *sxes;
     SchedXaction *sx = (SchedXaction*)data;
     GNCBook *book;
     book = gnc_get_current_book();
     sxes = gnc_book_get_schedxactions(book);
     gnc_sxes_del_sx(sxes, sx);
     xaccSchedXactionFree(sx);
}

static void
gnc_plugin_page_sx_list_cmd_delete(GtkAction *action, GncPluginPageSxList *page)
{
     GncPluginPageSxListPrivate *priv = GNC_PLUGIN_PAGE_SX_LIST_GET_PRIVATE(page);
     GtkTreeSelection *selection;
     GList *selected_paths, *to_delete = NULL;
     GtkTreeModel *model;

     /* @@fixme -- add (suppressible?) confirmation dialog */
     
     selection = gtk_tree_view_get_selection(priv->tree_view);
     selected_paths = gtk_tree_selection_get_selected_rows(selection, &model);
     if (g_list_length(selected_paths) == 0)
     {
          PERR("no selection for delete.");
          return;
     }

     to_delete = g_list_map(selected_paths, (GMapFunc)_sx_for_path, model);
     {
          GList *list;
          for (list = to_delete; list != NULL; list = list->next)
          {
               DEBUG("to-delete [%s]\n", xaccSchedXactionGetName((SchedXaction*)list->data));
          }
     }
     g_list_foreach(to_delete, (GFunc)_destroy_sx, NULL);
     g_list_free(to_delete);
     g_list_foreach(selected_paths, (GFunc)gtk_tree_path_free, NULL);
     g_list_free(selected_paths);
}

#if 0 /* compare/sort fns */
static gint
gnc_sxd_clist_compare_sx_name( GtkCList *cl, gconstpointer a, gconstpointer b )
{
        SchedXaction *sxa, *sxb;

        sxa = (SchedXaction*)(((GtkCListRow*)a)->data);
        sxb = (SchedXaction*)(((GtkCListRow*)b)->data);
        g_assert( sxa || sxb );
        if ( !sxa ) {
                return 1;
        }
        if ( !sxb ) {
                return -1;
        }
        return strcmp( xaccSchedXactionGetName( sxa ),
                       xaccSchedXactionGetName( sxb ) );
}

static gint
gnc_sxd_clist_compare_sx_freq( GtkCList *cl,
                               gconstpointer a,
                               gconstpointer b )
{
        SchedXaction *sxa, *sxb;

        g_assert( a || b );
        if ( !a ) return 1;
        if ( !b ) return -1;
        sxa = (SchedXaction*)((GtkCListRow*)a)->data;
        sxb = (SchedXaction*)((GtkCListRow*)b)->data;
        g_assert( sxa || sxb );
        if ( !sxa ) return 1;
        if ( !sxb ) return -1;
        return gnc_freq_spec_compare( xaccSchedXactionGetFreqSpec( sxa ),
                                      xaccSchedXactionGetFreqSpec( sxb ) );
}

static gint
gnc_sxd_clist_compare_sx_next_occur( GtkCList *cl,
                                     gconstpointer a,
                                     gconstpointer b )
{
        SchedXaction *sxa, *sxb;
        GDate gda, gdb;

        sxa = (SchedXaction*)((GtkCListRow*)a)->data;
        sxb = (SchedXaction*)((GtkCListRow*)b)->data;

        g_assert( sxa || sxb );
        if ( !sxa ) {
                return 1;
        }
        if ( !sxb ) {
                return -1;
        }
        g_assert( sxa && sxb );

        gda = xaccSchedXactionGetNextInstance( sxa, NULL );
        gdb = xaccSchedXactionGetNextInstance( sxb, NULL );

        if ( ! ( g_date_valid(&gda) && g_date_valid(&gdb) ) ) {
                return 0;
        }
        if ( !g_date_valid(&gda) ) {
                return 1;
        }
        if ( !g_date_valid(&gdb) ) {
                return -1;
        }
        return g_date_compare( &gda, &gdb );
}

#endif /* 0 - compare/sort fns */

/* ------------------------------------------------------------ */

static int
parse_vars_from_formula(const char *formula,
                        GHashTable *varHash,
                        gnc_numeric *result)
{
     gnc_numeric num;
     char *errLoc;
     int toRet = 0;

     if (!gnc_exp_parser_parse_separate_vars(formula, &num, &errLoc, varHash))
     {
          toRet = -1;
     }

     if (result != NULL)
     {
          *result = num;
     }

     return toRet;
}

static void
clear_variable_numerics(gpointer key, gpointer value, gpointer data)
{
     g_free((gnc_numeric*)value);
     g_hash_table_insert((GHashTable*)data, key, NULL);
}

static gint
_get_vars_helper(Transaction *txn, void *var_hash_data)
{
     GHashTable *var_hash = (GHashTable*)var_hash_data;
     GList *split_list;
     kvp_frame *kvpf;
     kvp_value *kvp_val;
     Split *s;
     char *str;
     gnc_commodity *first_cmdty = NULL;

     split_list = xaccTransGetSplitList(txn);
     if (split_list == NULL)
     {
          return 1;
     }

     for ( ; split_list; split_list = split_list->next)
     {
          gnc_commodity *split_cmdty = NULL;
          GUID *acct_guid;
          Account *acct;

          s = (Split*)split_list->data;
          kvpf = xaccSplitGetSlots(s);
          kvp_val = kvp_frame_get_slot_path(kvpf,
                                            GNC_SX_ID,
                                            GNC_SX_ACCOUNT,
                                            NULL);
          acct_guid = kvp_value_get_guid(kvp_val);
          acct = xaccAccountLookup(acct_guid, gnc_get_current_book());
          split_cmdty = xaccAccountGetCommodity(acct);
          if (first_cmdty == NULL)
          {
               first_cmdty = split_cmdty;
          }
                
          if (! gnc_commodity_equal(split_cmdty, first_cmdty))
          {
               gnc_numeric *tmp_num;
               GString *var_name = g_string_sized_new(16);
               g_string_printf(var_name, "%s -> %s",
                               gnc_commodity_get_mnemonic(split_cmdty),
                               gnc_commodity_get_mnemonic(first_cmdty));
               tmp_num = g_new0(gnc_numeric, 1);
               *tmp_num = gnc_numeric_create(0, 1);
               g_hash_table_insert(var_hash, g_strdup(var_name->str), tmp_num);
               g_string_free(var_name, TRUE);
          }

          // existing... ------------------------------------------
          kvp_val = kvp_frame_get_slot_path(kvpf,
                                            GNC_SX_ID,
                                            GNC_SX_CREDIT_FORMULA,
                                            NULL);
          if (kvp_val != NULL)
          {
               str = kvp_value_get_string(kvp_val);
               if (str && strlen(str) != 0)
               {
                    parse_vars_from_formula(str, var_hash, NULL);
               }
          }

          kvp_val = kvp_frame_get_slot_path(kvpf,
                                            GNC_SX_ID,
                                            GNC_SX_DEBIT_FORMULA,
                                            NULL);
          if (kvp_val != NULL)
          {
               str = kvp_value_get_string(kvp_val);
               if (str && strlen(str) != 0)
               {
                    parse_vars_from_formula(str, var_hash, NULL);
               }
          }
     }

     return 0;
}

static void
sxsl_get_sx_vars(SchedXaction *sx, GHashTable *var_hash)
{
     AccountGroup *template_group;
     Account *sx_template_acct;
     const char *sx_guid_str;

     template_group = gnc_book_get_template_group(gnc_get_current_book());
     sx_guid_str = guid_to_string(xaccSchedXactionGetGUID(sx));
     /* Get account named after guid string. */
     sx_template_acct = xaccGetAccountFromName(template_group, sx_guid_str);
     xaccAccountForEachTransaction(sx_template_acct, _get_vars_helper, var_hash);

     // @@fixme - This should actually create GncSxVariable structures.
     g_hash_table_foreach(var_hash, clear_variable_numerics, (gpointer)var_hash);
}

static void
_clone_hash_entry(gpointer key, gpointer value, gpointer user_data)
{
     GHashTable *to = (GHashTable*)user_data;
     g_hash_table_insert(to, key, value);
}

static void
gnc_util_copy_hash_table(GHashTable *from, GHashTable *to)
{
     g_hash_table_foreach(from, _clone_hash_entry, (gpointer)to);
}

static GncSxInstance*
gnc_sx_instance_new(GncSxInstances *parent, GncSxInstanceType type, GDate *date, gint sequence_num)
{
     GncSxInstance *rtn = g_new0(GncSxInstance, 1);
     rtn->parent = parent;
     rtn->type = type;
     g_date_clear(&rtn->date, 1);
     rtn->date = *date;

     if (! parent->variable_names_parsed)
     {
          parent->variable_names = g_hash_table_new(g_str_hash, g_str_equal);
          sxsl_get_sx_vars(parent->sx, parent->variable_names);
          parent->variable_names_parsed = TRUE;

          // @@fixme: add sequence_num as `i`, non-editable
     }

     rtn->variable_bindings = g_hash_table_new(g_str_hash, g_str_equal);
     gnc_util_copy_hash_table(parent->variable_names, rtn->variable_bindings);
     return rtn;
}

static GncSxInstances*
_gnc_sx_gen_instances(gpointer *data, gpointer user_data)
{
     GncSxInstances *instances = g_new0(GncSxInstances, 1);
     SchedXaction *sx = (SchedXaction*)data;
     GDate *range_end = (GDate*)user_data;
     GDate creation_end, remind_end;
     GDate cur_date;
     void *sequence_ctx;

     instances->sx = sx;

     creation_end = *range_end;
     g_date_add_days(&creation_end, xaccSchedXactionGetAdvanceCreation(sx));
     remind_end = creation_end;
     g_date_add_days(&remind_end, xaccSchedXactionGetAdvanceReminder(sx));

     /* postponed */
     {
          GList *postponed = gnc_sx_get_defer_instances(sx);
          for ( ; postponed != NULL; postponed = g_list_next(postponed))
          {
               GDate inst_date;
               int seq_num;
               GncSxInstance *inst;

               inst_date = xaccSchedXactionGetNextInstance(sx, postponed->data);
               seq_num = gnc_sx_get_instance_count(sx, postponed->data);
               inst = gnc_sx_instance_new(instances, POSTPONED, &inst_date, seq_num);
               instances->list = g_list_append(instances->list, inst);
          }
     }

     /* to-create */
     g_date_clear(&cur_date, 1);
     sequence_ctx = gnc_sx_create_temporal_state(sx);
     cur_date = xaccSchedXactionGetInstanceAfter(sx, &cur_date, sequence_ctx);
     instances->next_instance_date = cur_date;
     while (g_date_valid(&cur_date) && g_date_compare(&cur_date, &creation_end) <= 0)
     {
          GncSxInstance *inst;
          int seq_num;
          seq_num = gnc_sx_get_instance_count(sx, sequence_ctx);
          inst = gnc_sx_instance_new(instances, TO_CREATE, &cur_date, seq_num);
          instances->list = g_list_append(instances->list, inst);
          gnc_sx_incr_temporal_state(sx, sequence_ctx);
          cur_date = xaccSchedXactionGetInstanceAfter(sx, &cur_date, sequence_ctx);
     }

     /* reminders */
     while (g_date_valid(&cur_date) && g_date_compare(&cur_date, &remind_end) <= 0)
     {
          GncSxInstance *inst;
          int seq_num;
          seq_num = gnc_sx_get_instance_count(sx, sequence_ctx);
          inst = gnc_sx_instance_new(instances, REMINDER, &cur_date, seq_num);
          instances->list = g_list_append(instances->list, inst);
          gnc_sx_incr_temporal_state(sx, sequence_ctx);
          cur_date = xaccSchedXactionGetInstanceAfter(sx, &cur_date, sequence_ctx);
     }

     return instances;
}

GncSxInstanceModel*
gnc_sx_get_instances(GDate *range_end)
{
     GncSxInstanceModel *instances;
     GList *sxes;

     g_assert(range_end != NULL);
     g_assert(g_date_valid(range_end));

     instances = gnc_sx_instance_model_new();
     instances->range_end = *range_end;
     sxes = gnc_book_get_schedxactions(gnc_get_current_book())->sx_list;
     instances->sx_instance_list = g_list_map(sxes, (GMapFunc)_gnc_sx_gen_instances, (gpointer)range_end);

     return instances;
}

static GncSxInstanceModel*
gnc_sx_instance_model_new(void)
{
     return GNC_SX_INSTANCE_MODEL(g_object_new(GNC_TYPE_SX_INSTANCE_MODEL, NULL));
}

GType
gnc_sx_instance_model_get_type(void)
{
     static GType type = 0;
     if (type == 0) {
          static const GTypeInfo info = {
               sizeof (GncSxInstanceModelClass),
               NULL,   /* base_init */
               NULL,   /* base_finalize */
               (GClassInitFunc)gnc_sx_instance_model_class_init,   /* class_init */
               NULL,   /* class_finalize */
               NULL,   /* class_data */
               sizeof (GncSxInstanceModel),
               0,      /* n_preallocs */
               (GInstanceInitFunc)gnc_sx_instance_model_init    /* instance_init */
          };
          type = g_type_register_static (G_TYPE_OBJECT,
                                         "GncSxInstanceModelType",
                                         &info, 0);
     }
     return type;
}

static void
gnc_sx_instance_model_dispose (GObject *object)
{
     printf("dispose\n");
}

static void
gnc_sx_instance_model_finalize (GObject *object)
{
     printf("finalize\n");
}

static void
gnc_sx_instance_model_class_init (GncSxInstanceModelClass *klass)
{
     GObjectClass *object_class = G_OBJECT_CLASS(klass);
     object_class->dispose = gnc_sx_instance_model_dispose;
     object_class->finalize = gnc_sx_instance_model_finalize;

     klass->removing_signal_id =
          g_signal_new("removing",
                       GNC_TYPE_SX_INSTANCE_MODEL,
                       G_SIGNAL_RUN_FIRST,
                       0, /* class offset */
                       NULL, /* accumulator */
                       NULL, /* accum data */
                       g_cclosure_marshal_VOID__POINTER,
                       G_TYPE_NONE,
                       1,
                       G_TYPE_POINTER);

     klass->updated_signal_id =
          g_signal_new("updated",
                       GNC_TYPE_SX_INSTANCE_MODEL,
                       G_SIGNAL_RUN_FIRST,
                       0, /* class offset */
                       NULL, /* accumulator */
                       NULL, /* accum data */
                       g_cclosure_marshal_VOID__VOID,
                       G_TYPE_NONE,
                       0, NULL);

     klass->added_signal_id =
          g_signal_new("added",
                       GNC_TYPE_SX_INSTANCE_MODEL,
                       G_SIGNAL_RUN_FIRST,
                       0, /* class offset */
                       NULL, /* accumulator */
                       NULL, /* accum data */
                       g_cclosure_marshal_VOID__POINTER,
                       G_TYPE_NONE,
                       1,
                       G_TYPE_POINTER);
}

static void
gnc_sx_instance_model_init(GTypeInstance *instance, gpointer klass)
{
     GncSxInstanceModel *inst = (GncSxInstanceModel*)instance;

     g_date_clear(&inst->range_end, 1);
     inst->sx_instance_list = NULL;
     inst->qof_event_handler_id = qof_event_register_handler(_gnc_sx_instance_event_handler, inst);
}

static void
_gnc_sx_instance_event_handler(QofEntity *ent, QofEventId event_type, gpointer user_data, gpointer evt_data)
{
     GncSxInstanceModel *instances = GNC_SX_INSTANCE_MODEL(user_data);

     /* selection rules {
     //   (gnc_collection_get_schedxaction_list(book), GNC_EVENT_ITEM_ADDED)
     //   (gnc_collection_get_schedxaction_list(book), GNC_EVENT_ITEM_REMOVED)
     //   (GNC_IS_SX(ent), QOF_EVENT_MODIFIED)
     // } */
     if (!(GNC_IS_SX(ent) || GNC_IS_SXES(ent)))
          return;

     if (GNC_IS_SX(ent))
     {
          SchedXaction *sx;
          sx = GNC_SX(ent);
          if (event_type & QOF_EVENT_MODIFY)
          {
               /* @re-generate instance, update*/
          }
          /* else { unsupported event type; ignore } */
     }
     else if (GNC_IS_SXES(ent))
     {
          SchedXactions *sxes = GNC_SXES(ent);
          SchedXaction *sx = GNC_SX(evt_data);

          sxes = NULL;
          if (event_type & GNC_EVENT_ITEM_REMOVED)
          {
               gpointer sx_instance_to_remove = NULL;
               GList *list;

               /* find, remove, update */
               for (list = instances->sx_instance_list; list != NULL; list = list->next)
               {
                    if (sx == ((GncSxInstances*)list->data)->sx)
                    {
                         sx_instance_to_remove = list->data;
                         break;
                    }
               }
               if (sx_instance_to_remove != NULL)
               {
                    g_signal_emit_by_name(instances, "removing", GUINT_TO_POINTER(GPOINTER_TO_UINT(((GncSxInstances*)sx_instance_to_remove)->sx)));
                    instances->sx_instance_list = g_list_remove(instances->sx_instance_list, sx_instance_to_remove);
                    g_signal_emit_by_name(instances, "updated"); // @@fixme remove
               }
               else { printf("err\n"); }
          }
          else if (event_type & GNC_EVENT_ITEM_ADDED)
          {
               /* generate instances, add to instance list, emit update. */
               instances->sx_instance_list
                    = g_list_append(instances->sx_instance_list,
                                    (*_gnc_sx_gen_instances)((gpointer)sx, (gpointer)&instances->range_end));
               g_signal_emit_by_name(instances, "added", GUINT_TO_POINTER(GPOINTER_TO_UINT(sx)));
               g_signal_emit_by_name(instances, "updated"); // @fixme remove
          }
          /* else { printf("unsupported event type [%d]\n", event_type); } */
     }
}

/* ------------------------------------------------------------ */

static void
gnc_sx_instance_dense_cal_adapter_class_init(GncSxInstanceDenseCalAdapterClass *klass)
{
     ; /* nop */
}

static void
gnc_sx_instance_dense_cal_adapter_init(GTypeInstance *instance, gpointer klass)
{
     /*GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(instance);*/
     ; /* nop */
}

static void
gnc_sx_instance_dense_cal_adapter_interface_init(gpointer g_iface, gpointer iface_data)
{
     GncDenseCalModelIface *iface = (GncDenseCalModelIface*)g_iface;
     iface->get_contained = gsidca_get_contained;
     iface->get_name = gsidca_get_name;
     iface->get_info = gsidca_get_info;
     iface->get_instance_count = gsidca_get_instance_count;
     iface->get_instance = gsidca_get_instance;
}

static void
gsidca_instances_added_cb(GncSxInstanceModel *model, gpointer instance_added, gpointer user_data)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
     g_signal_emit_by_name(adapter, "added", GPOINTER_TO_UINT(instance_added));
}

static void
gsidca_instances_updated_cb(GncSxInstanceModel *model, gpointer user_data)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
     // @@fixme figure out which; emit appropriate signal.
     GList *exposed_tags;
     printf("instances updated\n");
     for (exposed_tags = gsidca_get_contained(GNC_DENSE_CAL_MODEL(adapter)); exposed_tags != NULL; exposed_tags = exposed_tags->next)
     {
          g_signal_emit_by_name(adapter, "update", GPOINTER_TO_UINT(exposed_tags->data));
     }
}

static void
gsidca_instances_removing_cb(GncSxInstanceModel *model, gpointer instance_to_be_removed, gpointer user_data)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(user_data);
     printf("removing instance...\n");
     g_signal_emit_by_name(adapter, "removing", GPOINTER_TO_UINT(instance_to_be_removed));
}

GncSxInstanceDenseCalAdapter*
gnc_sx_instance_dense_cal_adapter_new(GncSxInstanceModel *instances)
{
     GncSxInstanceDenseCalAdapter *adapter = g_object_new(GNC_TYPE_SX_INSTANCE_DENSE_CAL_ADAPTER, NULL);
     adapter->instances = instances;
     g_signal_connect(instances, "added", (GCallback)gsidca_instances_added_cb, adapter);
     g_signal_connect(instances, "updated", (GCallback)gsidca_instances_updated_cb, adapter);
     g_signal_connect(instances, "removing", (GCallback)gsidca_instances_removing_cb, adapter);
     return adapter;
}

GType
gnc_sx_instance_dense_cal_adapter_get_type(void)
{
     static GType type = 0;
     if (type == 0)
     {
          static const GTypeInfo info = {
               sizeof (GncSxInstanceDenseCalAdapterClass),
               NULL, /* base init */
               NULL, /* base finalize */
               (GClassInitFunc)gnc_sx_instance_dense_cal_adapter_class_init,
               NULL, /* class finalize */
               NULL, /* class data */
               sizeof(GncSxInstanceDenseCalAdapter),
               0, /* n_preallocs */
               (GInstanceInitFunc)gnc_sx_instance_dense_cal_adapter_init
          };
          static const GInterfaceInfo iDenseCalModelInfo = {
               (GInterfaceInitFunc)gnc_sx_instance_dense_cal_adapter_interface_init,
               NULL, /* interface finalize */
               NULL, /* interface data */
          };

          type = g_type_register_static (G_TYPE_OBJECT,
                                         "GncSxInstanceDenseCalAdapterType",
                                         &info, 0);
          g_type_add_interface_static(type,
                                      GNC_TYPE_DENSE_CAL_MODEL,
                                      &iDenseCalModelInfo);
     }
     return type;
}

static gint
gsidca_find_sx_with_tag(gconstpointer list_data,
                        gconstpointer find_data)
{
     GncSxInstances *sx_instances = (GncSxInstances*)list_data;
     return (GUINT_TO_POINTER(GPOINTER_TO_UINT(sx_instances->sx)) == find_data ? 0 : 1);
}

static GList*
gsidca_get_contained(GncDenseCalModel *model)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     //"removing return g_list_map(instances->sxes, sx_to_tag, null);
     GList *list = NULL, *sxes;
     for (sxes = adapter->instances->sx_instance_list; sxes != NULL; sxes = sxes->next)
     {
          GncSxInstances *sx_instances = (GncSxInstances*)sxes->data;
          list = g_list_append(list, GUINT_TO_POINTER(GPOINTER_TO_UINT(sx_instances->sx)));
     }
     return list;
}

static gchar*
gsidca_get_name(GncDenseCalModel *model, guint tag)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     GncSxInstances *insts
          = (GncSxInstances*)g_list_find_custom(adapter->instances->sx_instance_list, GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
     if (insts == NULL)
          return NULL;
     return xaccSchedXactionGetName(insts->sx);
}

static gchar*
gsidca_get_info(GncDenseCalModel *model, guint tag)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     // g_list_find(instances->sxes, {sx_to_tag, tag}).get_freq_spec().get_freq_str();
     FreqSpec *spec;
     GString *info;
     gchar *info_str;
     GncSxInstances *insts
          = (GncSxInstances*)g_list_find_custom(adapter->instances->sx_instance_list, GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
     if (insts == NULL)
          return NULL;
     spec = xaccSchedXactionGetFreqSpec(insts->sx);
     info = g_string_sized_new(16);
     xaccFreqSpecGetFreqStr(spec, info);
     info_str = info->str; // @fixme leaked... :/
     g_string_free(info, FALSE);
     return info_str;
}

static gint
gsidca_get_instance_count(GncDenseCalModel *model, guint tag)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     // g_list_find(instances->sxes, {sx_to_tag, tag}).length();
     GncSxInstances *insts
          = (GncSxInstances*)g_list_find_custom(adapter->instances->sx_instance_list, GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
     if (insts == NULL)
          return 0;
     return g_list_length(insts->list);
}

static void
gsidca_get_instance(GncDenseCalModel *model, guint tag, gint instance_index, GDate *date)
{
     GncSxInstanceDenseCalAdapter *adapter = GNC_SX_INSTANCE_DENSE_CAL_ADAPTER(model);
     GncSxInstance *inst;
     GncSxInstances *insts
          = (GncSxInstances*)g_list_find_custom(adapter->instances->sx_instance_list, GUINT_TO_POINTER(tag), gsidca_find_sx_with_tag)->data;
     if (insts == NULL)
          return;
     inst = (GncSxInstance*)g_list_nth_data(insts->list, instance_index);
     g_date_valid(&inst->date);
     *date = inst->date;
     g_date_valid(date);
}

/* ------------------------------------------------------------ */

GType
gnc_sx_list_tree_model_adapter_get_type(void)
{
     static GType type = 0;
     if (type == 0) {
          static const GTypeInfo info = {
               sizeof (GncSxListTreeModelAdapterClass),
               NULL,   /* base_init */
               NULL,   /* base_finalize */
               (GClassInitFunc)gnc_sx_list_tree_model_adapter_class_init,   /* class_init */
               NULL,   /* class_finalize */
               NULL,   /* class_data */
               sizeof (GncSxListTreeModelAdapter),
               0,      /* n_preallocs */
               (GInstanceInitFunc)gnc_sx_list_tree_model_adapter_init    /* instance_init */
          };
          static const GInterfaceInfo itreeModel_info = {
               (GInterfaceInitFunc) gnc_sx_list_tree_model_adapter_interface_init,    /* interface_init */
               NULL,               /* interface_finalize */
               NULL                /* interface_data */
          };

          type = g_type_register_static (G_TYPE_OBJECT,
                                         "GncSxListTreeModelAdapterType",
                                         &info, 0);
          g_type_add_interface_static(type,
                                      GTK_TYPE_TREE_MODEL,
                                      &itreeModel_info);
     }
     return type;
}

static void
gnc_sx_list_tree_model_adapter_class_init(GncSxListTreeModelAdapterClass *klass)
{
     ; /* nop */
}

static GtkTreeModelFlags
gsltma_get_flags(GtkTreeModel *tree_model)
{
     return gtk_tree_model_get_flags(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real));
}

static gint
gsltma_get_n_columns(GtkTreeModel *tree_model)
{
     return gtk_tree_model_get_n_columns(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real));
}

static GType
gsltma_get_column_type(GtkTreeModel *tree_model, gint index)
{
     return gtk_tree_model_get_column_type(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), index);
}

static gboolean
gsltma_get_iter(GtkTreeModel *tree_model,
                GtkTreeIter *iter,
                GtkTreePath *path)
{
     return gtk_tree_model_get_iter(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, path);
}

static GtkTreePath*
gsltma_get_path(GtkTreeModel *tree_model,
                GtkTreeIter *iter)
{
     return gtk_tree_model_get_path(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsltma_get_value(GtkTreeModel *tree_model,
                 GtkTreeIter *iter,
                 gint column,
                 GValue *value)
{
     gtk_tree_model_get_value(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, column, value);
}

static gboolean
gsltma_iter_next(GtkTreeModel *tree_model,
                 GtkTreeIter *iter)
{
     return gtk_tree_model_iter_next(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gboolean
gsltma_iter_children(GtkTreeModel *tree_model,
                     GtkTreeIter *iter,
                     GtkTreeIter *parent)
{
     return gtk_tree_model_iter_children(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, parent);
}

static gboolean
gsltma_iter_has_child(GtkTreeModel *tree_model,
                      GtkTreeIter *iter)
{
     return gtk_tree_model_iter_has_child(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gint
gsltma_iter_n_children(GtkTreeModel *tree_model,
                       GtkTreeIter *iter)
{
     return gtk_tree_model_iter_n_children(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static gboolean
gsltma_iter_nth_child(GtkTreeModel *tree_model,
                      GtkTreeIter *iter,
                      GtkTreeIter *parent,
                      gint n)
{
     return gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, parent, n);
}

static gboolean
gsltma_iter_parent(GtkTreeModel *tree_model,
                   GtkTreeIter *iter,
                   GtkTreeIter *child)
{
     return gtk_tree_model_iter_parent(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter, child);
}

static void
gsltma_ref_node(GtkTreeModel *tree_model,
                GtkTreeIter *iter)
{
     gtk_tree_model_ref_node(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gsltma_unref_node(GtkTreeModel *tree_model,
                  GtkTreeIter *iter)
{
     gtk_tree_model_unref_node(GTK_TREE_MODEL(GNC_SX_LIST_TREE_MODEL_ADAPTER(tree_model)->real), iter);
}

static void
gnc_sx_list_tree_model_adapter_interface_init(gpointer g_iface, gpointer iface_data)
{
     GtkTreeModelIface *tree_model = (GtkTreeModelIface*)g_iface;
     tree_model->get_flags = gsltma_get_flags;
     tree_model->get_n_columns = gsltma_get_n_columns;
     tree_model->get_column_type = gsltma_get_column_type;
     tree_model->get_iter = gsltma_get_iter;
     tree_model->get_path = gsltma_get_path;
     tree_model->get_value = gsltma_get_value;
     tree_model->iter_next = gsltma_iter_next;
     tree_model->iter_children = gsltma_iter_children;
     tree_model->iter_has_child = gsltma_iter_has_child;
     tree_model->iter_n_children = gsltma_iter_n_children;
     tree_model->iter_nth_child = gsltma_iter_nth_child;
     tree_model->iter_parent = gsltma_iter_parent;
     tree_model->ref_node = gsltma_ref_node;
     tree_model->unref_node = gsltma_unref_node;
}

static void
gsltma_proxy_row_changed(GtkTreeModel *treemodel,
                         GtkTreePath *arg1,
                         GtkTreeIter *arg2,
                         gpointer user_data)
{
     g_signal_emit_by_name(user_data, "row-changed", arg1, arg2);
}

static void
gsltma_proxy_row_deleted(GtkTreeModel *treemodel,
                         GtkTreePath *arg1,
                         gpointer user_data)
{
     g_signal_emit_by_name(user_data, "row-deleted", arg1);
}

static void
gsltma_proxy_row_has_child_toggled(GtkTreeModel *treemodel,
                                   GtkTreePath *arg1,
                                   GtkTreeIter *arg2,
                                   gpointer user_data)
{
     g_signal_emit_by_name(user_data, "row-has-child-toggled", arg1, arg2);
}

static void
gsltma_proxy_row_inserted(GtkTreeModel *treemodel,
                          GtkTreePath *arg1,
                          GtkTreeIter *arg2,
                          gpointer user_data)
{
     g_signal_emit_by_name(user_data, "row-inserted", arg1, arg2);
}

static void
gsltma_proxy_rows_reordered(GtkTreeModel *treemodel,
                            GtkTreePath *arg1,
                            GtkTreeIter *arg2,
                            gpointer arg3,
                            gpointer user_data)
{
     g_signal_emit_by_name(user_data, "rows-reordered", arg1, arg2, arg3);
}

static void
gnc_sx_list_tree_model_adapter_init(GTypeInstance *instance, gpointer klass)
{
     GncSxListTreeModelAdapter *adapter = GNC_SX_LIST_TREE_MODEL_ADAPTER(instance);
     adapter->real = gtk_tree_store_new(4, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

     g_signal_connect(adapter->real, "row-changed", G_CALLBACK(gsltma_proxy_row_changed), adapter);
     g_signal_connect(adapter->real, "row-deleted", G_CALLBACK(gsltma_proxy_row_deleted), adapter);
     g_signal_connect(adapter->real, "row-has-child-toggled", G_CALLBACK(gsltma_proxy_row_has_child_toggled), adapter);
     g_signal_connect(adapter->real, "row-inserted", G_CALLBACK(gsltma_proxy_row_inserted), adapter);
     g_signal_connect(adapter->real, "rows-reordered", G_CALLBACK(gsltma_proxy_rows_reordered), adapter);
}

static void
gsltma_populate_tree_store(GncSxListTreeModelAdapter *model)
{
     GtkTreeIter iter;
     GList *list;

     for (list = model->instances->sx_instance_list; list != NULL; list = list->next)
     {
          GncSxInstances *instances = (GncSxInstances*)list->data;
          FreqSpec *fs;
          GString *frequency_str;
          char last_occur_date_buf[MAX_DATE_LENGTH+1];
          char next_occur_date_buf[MAX_DATE_LENGTH+1];

          frequency_str = g_string_sized_new(32);
          fs = xaccSchedXactionGetFreqSpec(instances->sx);
          xaccFreqSpecGetFreqStr(fs, frequency_str);

          {
               GDate *last_occur = xaccSchedXactionGetLastOccurDate(instances->sx);
               if (last_occur == NULL || !g_date_valid(last_occur))
               {
                    g_stpcpy(last_occur_date_buf, "never");
               }
               else
               {
                    qof_print_gdate(last_occur_date_buf,
                                    MAX_DATE_LENGTH,
                                    last_occur);
               }
          }

          qof_print_gdate(next_occur_date_buf, MAX_DATE_LENGTH, &instances->next_instance_date);

          gtk_tree_store_append(model->real, &iter, NULL);
          gtk_tree_store_set(model->real, &iter,
                             0, xaccSchedXactionGetName(instances->sx),
                             1, frequency_str->str,
                             2, last_occur_date_buf,
                             3, next_occur_date_buf,
                             -1);
          g_string_free(frequency_str, TRUE);
     }
}

static void
gsltma_updated_cb(GncSxInstanceModel *instances, gpointer user_data)
{
     GncSxListTreeModelAdapter *model = GNC_SX_LIST_TREE_MODEL_ADAPTER(user_data);
     printf("update\n");
     gtk_tree_store_clear(model->real);
     gsltma_populate_tree_store(model);
}

GncSxListTreeModelAdapter*
gnc_sx_list_tree_model_adapter_new(GncSxInstanceModel *instances)
{
     GncSxListTreeModelAdapter *rtn;

     rtn = GNC_SX_LIST_TREE_MODEL_ADAPTER(g_object_new(GNC_TYPE_SX_LIST_TREE_MODEL_ADAPTER, NULL));
     rtn->instances = instances;

     gsltma_populate_tree_store(rtn);

     g_signal_connect(G_OBJECT(rtn->instances), "updated", (GCallback)gsltma_updated_cb, (gpointer)rtn);

     return rtn;
}

GncSxInstances*
gnc_sx_list_tree_model_adapter_get_sx_instances(GncSxListTreeModelAdapter *model, GtkTreeIter *iter)
{
     GtkTreePath *path;
     gint *indices;
     gint index;

     path = gtk_tree_model_get_path(GTK_TREE_MODEL(model), iter);
     if (gtk_tree_path_get_depth(path) > 1)
     {
          gtk_tree_path_free(path);
          return NULL;
     }
     indices = gtk_tree_path_get_indices(path);
     index = indices[0];

     gtk_tree_path_free(path);
     return (GncSxInstances*)g_list_nth_data(model->instances->sx_instance_list, index);
}

/** @} */
/** @} */
