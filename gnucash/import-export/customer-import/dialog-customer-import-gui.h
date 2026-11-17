/*
 * dialog-customer-import-gui.h --
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
 * along with this program; if not, see <https://www.gnu.org/licenses/>
 */

/**
 * @addtogroup Tools
 * @{
 * @file
 * @brief GUI handling for customer import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 */

#ifndef GNC_PLUGIN_customer_import_gui_H
#define GNC_PLUGIN_customer_import_gui_H

#include <glib.h>

G_BEGIN_DECLS

typedef struct _customer_import_gui CustomerImportGui;

/**
 * File chooser
 */
CustomerImportGui *gnc_plugin_customer_import_showGUI (GtkWindow *parent);

G_END_DECLS

#endif /* GNC_PLUGIN_customer_import_gui_H */

/** @} */
