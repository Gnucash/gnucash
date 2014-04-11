/********************************************************************\
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
 *                                                                  *
\********************************************************************/

/* pricecell-gnome.h
 *
 * Implements gnome dependent price cell functions.
 */

#ifndef PRICE_CELL_GNOME_H
#define PRICE_CELL_GNOME_H

BasicCell * gnc_price_cell_gnome_new (void);

/* fixme: find a better home for this. */
void gnc_basic_cell_insert_decimal(BasicCell *cell,
                                   char decimal_point,
                                   int *cursor_position,
                                   int *start_selection,
                                   int *end_selection);

#endif
