/**
 * gnc-plugin-page-report.h -- A GncPlugin page for a report.
 *
 * Copyright (C) 2004 Joshua Sled
 * Author: Joshua Sled <jsled@asynchronous.org>
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
 **/
  
/** @addtogroup UI
    @{ */
/** @file gnc-plugin-page-report.h 
    @brief  Report page.
    @author Copyright (C) 2004 Joshua Sled <jsled@asynchronous.org>
*/

#ifndef __GNC_PLUGIN_PAGE_REPORT_H
#define __GNC_PLUGIN_PAGE_REPORT_H

#include <gtk/gtkwindow.h>
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_REPORT            (gnc_plugin_page_report_get_type ())
#define GNC_PLUGIN_PAGE_REPORT(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_REPORT, GncPluginPageReport))
#define GNC_PLUGIN_PAGE_REPORT_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_REPORT, GncPluginPageReportClass))
#define GNC_IS_PLUGIN_PAGE_REPORT(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_REPORT))
#define GNC_IS_PLUGIN_PAGE_REPORT_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_REPORT))
#define GNC_PLUGIN_PAGE_REPORT_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_REPORT, GncPluginPageReportClass))

#define GNC_PLUGIN_PAGE_REPORT_NAME "gnc-plugin-page-report"

/* typedefs & structures */
typedef struct GncPluginPageReportPrivate GncPluginPageReportPrivate;

typedef struct {
	GncPluginPage parent;
        
	GncPluginPageReportPrivate *priv;
} GncPluginPageReport;

typedef struct {
        GncPluginPageClass parent;

	/* callbacks */
  /*
    void (*account_selected) (GncPluginPage	 *page,
    Account	 *account);
  */

} GncPluginPageReportClass;

/* function prototypes */

/**
 * @return the type number for an "report" plugin page.
 **/
GType gnc_plugin_page_report_get_type( void );

/**
 * @param reportId The scheme-defined report identifier
 * @return a new "report" plugin page.
 */
GncPluginPage *gnc_plugin_page_report_new( int reportId );

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_REPORT_H */
/** @} */
