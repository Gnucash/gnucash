/* 
 * gnc-plugin-page.h -- A page, which can be added to the GnuCash main window.
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
 */

#ifndef __GNC_PLUGIN_PAGE_H
#define __GNC_PLUGIN_PAGE_H

#include <gdk/gdkpixbuf.h>
#include "egg-menu-merge.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE          (gnc_plugin_page_get_type ())
#define GNC_PLUGIN_PAGE(o)            (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_PLUGIN_PAGE, GncPluginPage))
#define GNC_IS_PLUGIN_PAGE(o)         (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNC_TYPE_PLUGIN_PAGE))
#define GNC_PLUGIN_PAGE_GET_IFACE(o)  (G_TYPE_INSTANCE_GET_INTERFACE ((o), GNC_TYPE_PLUGIN_PAGE, GncPluginPageIface))

/* typedefs & structures */
typedef struct GncPluginPage GncPluginPage; /* dummy typedef */

typedef struct {
	GTypeInterface parent;

	/* Signals */
	void (* inserted) (GncPluginPage *plugin_page);
	void (* removed) (GncPluginPage *plugin_page);
	void (* selected) (GncPluginPage *plugin_page);
	void (* unselected) (GncPluginPage *plugin_page);

	/* Virtual Table */
	GtkWidget *(* create_widget) (GncPluginPage *plugin_page);

	void (* merge_actions) (GncPluginPage *plugin_page, EggMenuMerge *merge);
	void (* unmerge_actions) (GncPluginPage *plugin_page, EggMenuMerge *merge);

	G_CONST_RETURN gchar *(* get_title) (GncPluginPage *plugin_page);
	G_CONST_RETURN gchar *(* get_icon) (GncPluginPage *plugin_page);

	G_CONST_RETURN gchar *(* get_plugin_name) (GncPluginPage *plugin_page);
	G_CONST_RETURN gchar *(* get_uri) (GncPluginPage *plugin_page);
} GncPluginPageIface;

/* function prototypes */
GType                 gnc_plugin_page_get_type        (void);

GtkWidget            *gnc_plugin_page_create_widget   (GncPluginPage *plugin_page);

void                  gnc_plugin_page_merge_actions   (GncPluginPage *plugin_page,
                                                       EggMenuMerge *merge);
void                  gnc_plugin_page_unmerge_actions (GncPluginPage *plugin_page,
                                                       EggMenuMerge *merge);

G_CONST_RETURN gchar *gnc_plugin_page_get_title       (GncPluginPage *plugin_page);
G_CONST_RETURN gchar *gnc_plugin_page_get_icon        (GncPluginPage *plugin_page);

G_CONST_RETURN gchar *gnc_plugin_page_get_plugin_name (GncPluginPage *plugin_page);
G_CONST_RETURN gchar *gnc_plugin_page_get_uri         (GncPluginPage *plugin_page);

/* Signals */
void                  gnc_plugin_page_inserted        (GncPluginPage *plugin_page);
void                  gnc_plugin_page_removed         (GncPluginPage *plugin_page);
void                  gnc_plugin_page_selected        (GncPluginPage *plugin_page);
void                  gnc_plugin_page_unselected      (GncPluginPage *plugin_page);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_H */
