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

#ifndef GNUCASH_CURSOR_H
#define GNUCASH_CURSOR_H

#include <glib.h>
#include "gnucash-sheet.h"

/** @ingroup Register
 * @addtogroup Gnome
 * @{
 */
/** @file gnucash-cursor.h
 * @brief Public declarations for GnucashCursor class
 */
#define GNUCASH_TYPE_CURSOR     (gnucash_cursor_get_type ())
#define GNUCASH_CURSOR(obj)     (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNUCASH_TYPE_CURSOR, GnucashCursor))
#define GNUCASH_CURSOR_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_CURSOR, GnucashCursorClass))
#define GNUCASH_IS_CURSOR(obj)  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNUCASH_TYPE_CURSOR))

#define GNUCASH_TYPE_ITEM_CURSOR     (gnucash_item_cursor_get_type ())
#define GNUCASH_ITEM_CURSOR(obj)     (G_TYPE_CHECK_INSTANCE_CAST ((obj), GNUCASH_TYPE_ITEM_CURSOR, GnucashItemCursor))
#define GNUCASH_ITEM_CURSOR_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_ITEM_CURSOR, GnucashItemCursorClass))
#define GNUCASH_IS_ITEM_CURSOR(o)    (G_TYPE_CHECK_INSTANCE_TYPE ((o), GNUCASH_TYPE_ITEM_CURSOR))


GType    gnucash_item_cursor_get_type (void);
GType    gnucash_cursor_get_type (void);


enum
{
    GNUCASH_CURSOR_CELL,
    GNUCASH_CURSOR_BLOCK,
    GNUCASH_CURSOR_NUM
};


typedef struct
{
    gint row;
    gint col;

    /* precomputed pixel coords for the item cursor*/
    gint x, y, w, h;
} GnucashCursorCell;


typedef struct
{
    GObject obj;

    GnucashCursorCell cell;
    GnucashSheet *sheet;

    gint row;
    gint col;
    /* precomputed pixel coords for the block cursor*/
    gint x, y, w, h;

    SheetBlockStyle *style;
} GnucashCursor;

typedef struct
{
    GObjectClass parent_class;
} GnucashCursorClass;


GnucashCursor *gnucash_cursor_new (GnucashSheet *sheet);

void gnucash_cursor_get_virt (GnucashCursor *cursor,
                              VirtualLocation *virt_loc);

void gnucash_cursor_set (GnucashCursor *cursor, VirtualLocation virt_loc);

void gnucash_cursor_set_style (GnucashCursor  *cursor, SheetBlockStyle *style);

void gnucash_cursor_configure (GnucashCursor *cursor);

/** @} */
#endif /* GNUCASH_CURSOR_H */
