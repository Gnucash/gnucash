/* gnc-plugin-page-report.c
 *
 * Copyright (C) 2004 Joshua Sled <jsled@asynchronous.org>
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>

#include "egg-menu-merge.h"
#include "gnc-plugin-page-report.h"
#include "messages.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"

static short module = MOD_GUI;

static GObjectClass *parent_class = NULL;
static GList *active_pages = NULL;

static void gnc_plugin_page_report_class_init( GncPluginPageReportClass *klass );
static void gnc_plugin_page_report_init( GncPluginPageReport *plugin_page );
static void gnc_plugin_page_report_finalize (GObject *object);

static GtkWidget* gnc_plugin_page_report_create_widget( GncPluginPage *plugin_page );
static void gnc_plugin_page_report_destroy_widget( GncPluginPage *plugin_page );
static void gnc_plugin_page_report_merge_actions( GncPluginPage *plugin_page, EggMenuMerge *merge );
static void gnc_plugin_page_report_unmerge_actions( GncPluginPage *plugin_page, EggMenuMerge *merge );
static void gnc_plugin_page_report_destroy_widget( GncPluginPage *plugin_page );

struct GncPluginPageReportPrivate
{
        //ExtensionInfo *extInf;
};

GType
gnc_plugin_page_report_get_type (void)
{
	static GType gnc_plugin_page_report_type = 0;

	if (gnc_plugin_page_report_type == 0) {
		static const GTypeInfo our_info = {
			sizeof (GncPluginPageReportClass),
			NULL,
			NULL,
			(GClassInitFunc) gnc_plugin_page_report_class_init,
			NULL,
			NULL,
			sizeof (GncPluginPageReport),
			0,
			(GInstanceInitFunc) gnc_plugin_page_report_init
		};
		
		gnc_plugin_page_report_type = g_type_register_static (GNC_TYPE_PLUGIN_PAGE,
                                                                      "GncPluginPageReport",
                                                                      &our_info, 0);
	}

	return gnc_plugin_page_report_type;
}

static void
gnc_plugin_page_report_class_init (GncPluginPageReportClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GncPluginPageClass *gnc_plugin_page_class = GNC_PLUGIN_PAGE_CLASS(klass);

	parent_class = g_type_class_peek_parent (klass);

	object_class->finalize = gnc_plugin_page_report_finalize;

        // FIXME: stock reporting icon?
	//gnc_plugin_page_class->tab_icon        = GNC_STOCK_ACCOUNT;
	gnc_plugin_page_class->plugin_name     = GNC_PLUGIN_PAGE_REPORT_NAME;

	gnc_plugin_page_class->create_widget   = gnc_plugin_page_report_create_widget;
	gnc_plugin_page_class->destroy_widget  = gnc_plugin_page_report_destroy_widget;
	gnc_plugin_page_class->merge_actions   = gnc_plugin_page_report_merge_actions;
	gnc_plugin_page_class->unmerge_actions = gnc_plugin_page_report_unmerge_actions;

/* JSLED: report-selected?
	plugin_page_signals[ACCOUNT_SELECTED] =
	  g_signal_new ("account_selected",
			G_OBJECT_CLASS_TYPE (object_class),
			G_SIGNAL_RUN_FIRST,
			G_STRUCT_OFFSET (GncPluginPageReportClass, account_selected),
			NULL, NULL,
			g_cclosure_marshal_VOID__POINTER,
			G_TYPE_NONE, 1,
			G_TYPE_POINTER);
*/
}

static void
gnc_plugin_page_report_finalize (GObject *object)
{
	GncPluginPageReport *page;
	GncPluginPageReportPrivate *priv;

	ENTER("object %p", object);
	page = GNC_PLUGIN_PAGE_REPORT (object);
	g_return_if_fail (GNC_IS_PLUGIN_PAGE_REPORT (page));
	priv = page->priv;
	g_return_if_fail (priv != NULL);

	active_pages = g_list_remove (active_pages, page);

	G_OBJECT_CLASS (parent_class)->finalize (object);
	LEAVE(" ");
}

static
GtkWidget*
gnc_plugin_page_report_create_widget( GncPluginPage *plugin_page )
{
        // FIXME: add a notebook tab to the window; display the report.
        return NULL;
}


static
void
gnc_plugin_page_report_destroy_widget( GncPluginPage *plugin_page )
{
        // FIXME: cleanup resources.
}

static
void
gnc_plugin_page_report_merge_actions( GncPluginPage *plugin_page,
                                      EggMenuMerge *merge )
{
        // FIXME: ui-merge
}

static
void
gnc_plugin_page_report_unmerge_actions( GncPluginPage *plugin_page,
                                        EggMenuMerge *merge )
{
        // FIXME: ui-merge
}

static void
gnc_plugin_page_report_init ( GncPluginPageReport *plugin_page )
{
        // JSLED: +FIXME
	//EggActionGroup *action_group;
	GncPluginPageReportPrivate *priv;
	GncPluginPage *parent;

	priv = plugin_page->priv = g_new0 (GncPluginPageReportPrivate, 1);

	/* Init parent declared variables */
	parent = GNC_PLUGIN_PAGE(plugin_page);
        // FIXME: + _(Report:) + priv->ext->name; 
	parent->title       = g_strdup(_("Report"));
        // FIXME: + _(Report:) + priv->ext->name; 
	parent->tab_name    = g_strdup(_("Report"));
        // FIXME: URI? gnc_report://classs/type ?
	parent->uri         = g_strdup("default:");

	/* change me when the system supports multiple books */
	gnc_plugin_page_add_book(parent, gnc_get_current_book());

	/* Create menu and toolbar information */
/*
        JSLED: FIXME
	action_group = egg_action_group_new ("GncPluginPageAccountTreeActions");
	priv->action_group = action_group;
	egg_action_group_add_actions (action_group,
				      gnc_plugin_page_account_tree_actions,
				      gnc_plugin_page_account_tree_n_actions,
				      plugin_page);
	gnc_gnome_utils_init_short_names (action_group, short_labels);
*/

	active_pages = g_list_append (active_pages, plugin_page);

        // JSLED: -FIXME
}

GncPluginPage*
gnc_plugin_page_report_new( int reportId )
{
	GncPluginPageReport *plugin_page;
	plugin_page = g_object_new (GNC_TYPE_PLUGIN_PAGE_REPORT, NULL);
        DEBUG( "creating report with id %d", reportId );
	return GNC_PLUGIN_PAGE (plugin_page);
}

