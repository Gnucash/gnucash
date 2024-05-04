/********************************************************************
 * gnc-commodity-edit.h -- Commodity editing widget                 *
 *                                                                  *
 * Copyright (C) Dave Peticolas <dave@krondo.com>                   *
 * Copyright (C) Derek Atkins <warlord@MIT.EDU>                     *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *******************************************************************/
/*
  @NOTATION@
 */

#ifndef GNC_COMMODITY_EDIT_H
#define GNC_COMMODITY_EDIT_H

#include "gnc-commodity.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Callback function to return the printable string of a commodity */
const char * gnc_commodity_edit_get_string (gpointer ptr);

/* Callback function to popup a new selection (modal) dialog.
 *
 * The generic argument is a pointer to a dialog_commodity_mode
 * enum. This tells the dialog how to limit the namespaces provided.
 */
gpointer gnc_commodity_edit_new_select (gpointer arg, gpointer ptr,
                                        GtkWidget *toplevel);

#ifdef __cplusplus
}
#endif

#endif
