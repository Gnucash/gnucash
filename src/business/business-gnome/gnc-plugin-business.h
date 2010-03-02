/*
 * gnc-plugin-business.h --
 *
 * Copyright (C) 2003 Jan Arne Petersen
 * Author: Jan Arne Petersen <jpetersen@uni-bonn.de>
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

#ifndef __GNC_PLUGIN_BUSINESS_H
#define __GNC_PLUGIN_BUSINESS_H

#include <gtk/gtkwindow.h>

#include "gnc-plugin.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_BUSINESS            (gnc_plugin_business_get_type ())
#define GNC_PLUGIN_BUSINESS(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_BUSINESS, GncPluginBusiness))
#define GNC_PLUGIN_BUSINESS_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_BUSINESS, GncPluginBusinessClass))
#define GNC_IS_PLUGIN_BUSINESS(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_BUSINESS))
#define GNC_IS_PLUGIN_BUSINESS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_BUSINESS))
#define GNC_PLUGIN_BUSINESS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_BUSINESS, GncPluginBusinessClass))

#define GNC_PLUGIN_BUSINESS_NAME "gnc-plugin-business"

/* typedefs & structures */
typedef struct
{
    GncPlugin gnc_plugin;
} GncPluginBusiness;

typedef struct
{
    GncPluginClass gnc_plugin;
} GncPluginBusinessClass;

/* function prototypes */
GType      gnc_plugin_business_get_type (void);

GncPlugin *gnc_plugin_business_new      (void);

G_END_DECLS

GncMainWindow *gnc_plugin_business_get_window (void);
void gnc_invoice_remind_bills_due (void);
void gnc_invoice_remind_bills_due_cb (void);

#endif /* __GNC_PLUGIN_BUSINESS_H */
