/* 
 * gnc-plugin-page-register.h -- 
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#ifndef __GNC_PLUGIN_PAGE_REGISTER_H
#define __GNC_PLUGIN_PAGE_REGISTER_H

#include <gtk/gtkwindow.h>

#include "Account.h"
#include "gnc-plugin-page.h"

G_BEGIN_DECLS

/* type macros */
#define GNC_TYPE_PLUGIN_PAGE_REGISTER            (gnc_plugin_page_register_get_type ())
#define GNC_PLUGIN_PAGE_REGISTER(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER, GncPluginPageRegister))
#define GNC_PLUGIN_PAGE_REGISTER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_PLUGIN_PAGE_REGISTER, GncPluginPageRegisterClass))
#define GNC_IS_PLUGIN_PAGE_REGISTER(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER))
#define GNC_IS_PLUGIN_PAGE_REGISTER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_PLUGIN_PAGE_REGISTER))
#define GNC_PLUGIN_PAGE_REGISTER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_PLUGIN_PAGE_REGISTER, GncPluginPageRegisterClass))

#define GNC_PLUGIN_PAGE_REGISTER_NAME "gnc-plugin-page-register"

/* typedefs & structures */
typedef struct GncPluginPageRegisterPrivate GncPluginPageRegisterPrivate;

typedef struct {
	GObject parent;

	GncPluginPageRegisterPrivate *priv;
} GncPluginPageRegister;

typedef struct {
	GObjectClass parent;
} GncPluginPageRegisterClass;

/* function prototypes */
GType          gnc_plugin_page_register_get_type (void);

GncPluginPage *gnc_plugin_page_register_new  (Account *account, gboolean subaccounts);
GncPluginPage *gnc_plugin_page_register_new_gl  (void);

G_END_DECLS

#endif /* __GNC_PLUGIN_PAGE_REGISTER_H */
