/********************************************************************
 * gnc-allocation-categories.hpp -- allocation category editor API
 * Copyright (C) 2026 Brent McBride (mcbridebt@hotmail.com)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 ********************************************************************/

#ifndef GNC_ALLOCATION_CATEGORIES_HPP
#define GNC_ALLOCATION_CATEGORIES_HPP

#include <gtk/gtk.h>

class GncOption;

/** Attach an allocation category editor to an options grid.
 * @param option Account or security category option owned by the dialog.
 * @param grid Options page receiving the editor.
 * @param row Grid row at which to attach the editor.
 */
void gnc_create_allocation_categories_widget(GncOption& option, GtkGrid* grid, int row);

#endif