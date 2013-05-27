/********************************************************************\
 * gtable.h -- glib -- basic datatype for 2D array of values        *
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
 *                                                                  *
\********************************************************************/

#ifndef G_TABLE_H
#define G_TABLE_H

#include <glib.h>


/* This is the API for GTables, a datatype for 2-dimensional tables
 * with automatic resizing and memory management.
 *
 * HACK ALERT -- this thing should proably become a part of glib (??)
 */

typedef struct GTable GTable;

typedef void (*g_table_entry_constructor) (gpointer entry, gpointer user_data);
typedef void (*g_table_entry_destroyer)   (gpointer entry, gpointer user_data);


/* Create a new table with the given entry constructor and destroyer.
 * Both functions must be given. They are used to initialize the table
 * entries and free unneeded memory when resizing and destroying. */
GTable * g_table_new (guint entry_size,
                      g_table_entry_constructor constructor,
                      g_table_entry_destroyer destroyer,
                      gpointer user_data);

/* Free the table and all associated table elements. */
void     g_table_destroy (GTable *gtable);

/* Return the element at the given row and column. If the coordinates
 * are out-of-bounds, return NULL */
gpointer g_table_index (GTable *gtable, int row, int col);

/* Resize the table, allocating and deallocating extra table
 * members if needed. The relationship between table members
 * before and after resizing is undefined, except in the case
 * where the number of rows changes, but not the number of
 * columns. In that case, higher-numbered rows are discarded
 * first. */
void     g_table_resize (GTable *gtable, int rows, int cols);

/* Return the number of table rows. */
int      g_table_rows (GTable *gtable);

/* Return the number of table columns. */
int      g_table_cols (GTable *gtable);

#endif
