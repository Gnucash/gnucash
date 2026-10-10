/********************************************************************\
 * gnc-plugin-page-tracelog.h : in-app viewer for the trace log     *
 *                                                                  *
 * Copyright 2026 GnuCash contributors                              *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of version 2 and/or version 3 of the   *
 * GNU General Public License as published by the Free Software     *
 * Foundation.                                                      *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program.  If not, see                            *
 * <https://www.gnu.org/licenses/>.                                 *
\********************************************************************/

/** @addtogroup ContentPlugins
    @{ */
/** @addtogroup GncPluginPageTracelog A Trace Log Plugin Page
    @{ */
/** @brief Renders the current session's trace log as a report-style tab. */

#ifndef __GNC_PLUGIN_PAGE_TRACELOG_H
#define __GNC_PLUGIN_PAGE_TRACELOG_H

#include <glib.h>
#include <gtk/gtk.h>
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_TRACELOG            (gnc_plugin_page_tracelog_get_type ())
#define GNC_PLUGIN_PAGE_TRACELOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GNC_TYPE_PLUGIN_PAGE_TRACELOG, GncPluginPageTracelog))
#define GNC_PLUGIN_PAGE_TRACELOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), GNC_TYPE_PLUGIN_PAGE_TRACELOG, GncPluginPageTracelogClass))
#define GNC_IS_PLUGIN_PAGE_TRACELOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GNC_TYPE_PLUGIN_PAGE_TRACELOG))
#define GNC_IS_PLUGIN_PAGE_TRACELOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), GNC_TYPE_PLUGIN_PAGE_TRACELOG))
#define GNC_PLUGIN_PAGE_TRACELOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), GNC_TYPE_PLUGIN_PAGE_TRACELOG, GncPluginPageTracelogClass))

#define GNC_PLUGIN_PAGE_TRACELOG_NAME "GncPluginPageTracelog"

/* typedefs & structures */
typedef struct
{
    GncPluginPage gnc_plugin_page;
} GncPluginPageTracelog;

typedef struct
{
    GncPluginPageClass gnc_plugin_page;
} GncPluginPageTracelogClass;

/* function prototypes */

/** Retrieve the type number for a "trace log" plugin page. */
GType gnc_plugin_page_tracelog_get_type (void);

/** @return The newly created plugin page. */
GncPluginPage *gnc_plugin_page_tracelog_new (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_TRACELOG_H */
/** @} */
/** @} */
