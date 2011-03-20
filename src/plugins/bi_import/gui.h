/*
 * gui.h --
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

/**
 * @addtogroup Tools
 * @{
 * @file gui.h
 * @brief GUI handling for bi import plugin
 * @author Copyright (C) 2009 Sebastian Held <sebastian.held@gmx.de>
 */

#ifndef GNC_PLUGIN_bi_import_gui_H
#define GNC_PLUGIN_bi_import_gui_H

#include <glib.h>

G_BEGIN_DECLS

typedef struct _bi_import_gui BillImportGui;

/**
 * File chooser
 */
BillImportGui *gnc_plugin_bi_import_showGUI(void);

G_END_DECLS

#endif /* GNC_PLUGIN_bi_import_gui_H */

/** @} */
