/* Copyright (C) 2019 Adrian Panella <ianchi74@outlook.com>
 *
 * GnuCash is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef __GNC_CELL_RENDERER_TEXT_FLAG_H__
#define __GNC_CELL_RENDERER_TEXT_FLAG_H__
#include <gtk/gtk.h>
G_BEGIN_DECLS
/* GTK4 factory for a textual value with an optional state icon. */
GtkListItemFactory *gnc_cell_renderer_text_flag_new (void);
G_END_DECLS
#endif
