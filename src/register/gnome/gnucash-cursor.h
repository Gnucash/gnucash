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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#ifndef GNUCASH_CURSOR_H
#define GNUCASH_CURSOR_H

#include <gnome.h>


#define GNUCASH_TYPE_CURSOR     (gnucash_cursor_get_type ())
#define GNUCASH_CURSOR(obj)     (GTK_CHECK_CAST((obj), GNUCASH_TYPE_CURSOR, GnucashCursor))
#define GNUCASH_CURSOR_CLASS(k) (GTK_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_CURSOR))
#define GNUCASH_IS_CURSOR(o)    (GTK_CHECK_TYPE((o), GNUCASH_TYPE_CURSOR))

#define GNUCASH_TYPE_ITEM_CURSOR     (gnucash_item_cursor_get_type ())
#define GNUCASH_ITEM_CURSOR(obj)     (GTK_CHECK_CAST((obj), GNUCASH_TYPE_ITEM_CURSOR, GnucashItemCursor))
#define GNUCASH_ITEM_CURSOR_CLASS(k) (GTK_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_ITEM_CURSOR))
#define GNUCASH_IS_ITEM_CURSOR(o)    (GTK_CHECK_TYPE((o), GNUCASH_TYPE_ITEM_CURSOR))


GtkType    gnucash_item_cursor_get_type (void);
GtkType    gnucash_cursor_get_type (void);


enum {
        GNUCASH_CURSOR_CELL,
        GNUCASH_CURSOR_BLOCK,
        GNUCASH_CURSOR_MAX,
};


typedef struct {
        GnomeCanvasItem canvas_item;

        gint type;

        gint row;
        gint col;

        /* precomputed pixel coords for the item cursor*/
        gint      x, y, w, h;

        gint      visible;

} GnucashItemCursor;


typedef struct 
{
        GnomeCanvasGroup canvas_group;

        GnomeCanvasItem *cursor[GNUCASH_CURSOR_MAX];

        GnucashSheet *sheet;
        GnucashGrid *grid;

        /* precomputed pixel coords for the block cursor*/
        gint      x, y, w, h;

        GdkGC    *gc;
        SheetBlockStyle *style;
} GnucashCursor;


typedef struct {
        GnomeCanvasItemClass parent_class;
} GnucashItemCursorClass;


typedef struct 
{
        GnomeCanvasGroupClass parent_class;
} GnucashCursorClass;


GnomeCanvasItem *gnucash_cursor_new (GnomeCanvasGroup *parent);

void gnucash_cursor_get_phys (GnucashCursor *cursor, int *phys_row,
			      int *phys_col);
void gnucash_cursor_get_virt (GnucashCursor *cursor, int *virt_row,
			      int *virt_col, int *cell_row, int *cell_col);

void gnucash_cursor_set  (GnucashCursor *cursor,
                          gint virt_row, gint virt_col,
                          gint cell_row, gint cell_col);

void gnucash_cursor_set_style (GnucashCursor  *cursor, SheetBlockStyle *style);

void gnucash_cursor_configure (GnucashCursor *cursor);


#endif /* GNUCASH_CURSOR_H */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
