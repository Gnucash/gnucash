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

#ifndef GNUCASH_GRID_H
#define GNUCASH_GRID_H

#include <gnome.h>

#include "table-gnome.h"
#include "table-allgui.h"


#define GNUCASH_TYPE_GRID     (gnucash_grid_get_type ())
#define GNUCASH_GRID(obj)     (GTK_CHECK_CAST((obj), GNUCASH_TYPE_GRID, GnucashGrid))
#define GNUCASH_GRID_CLASS(k) (GTK_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_GRID))
#define GNUCASH_IS_GRID(o)    (GTK_CHECK_TYPE((o), GNUCASH_TYPE_GRID))


typedef struct {
        GnomeCanvasItem canvas_item;

        GnucashSheet *sheet;

        /* The first and last displayed block */
        int        top_block;
        int        bottom_block;
	
        /* Offset from spreadsheet origin in units */
        long       top_offset;
        long       left_offset;

        GdkGC      *grid_gc;	/* Draw grid gc */
        GdkGC      *fill_gc;	/* Default background fill gc */
        GdkGC      *gc;		/* Color used for the cell */
	
        GdkColor   background;
        GdkColor   grid_color;
        GdkColor   default_color;

        GdkFont *normal_font;
        GdkFont *italic_font;
} GnucashGrid;


typedef struct {
        GnomeCanvasItemClass parent_class;
} GnucashGridClass;


GtkType    gnucash_grid_get_type (void);
GtkWidget *gnucash_grid_new 	 (GnucashSheet *sheet);

gint gnucash_grid_find_block_origin_by_pixel (GnucashGrid *grid,
                                              gint x, gint y,
                                              gint *virt_row, gint *virt_col,
                                              gint *o_x, gint *o_y);

gint gnucash_grid_find_cell_origin_by_pixel (GnucashGrid *grid,
                                             gint x, gint y,
                                             int *virt_row, int *virt_col,
                                             int *cell_row, int *cell_col,
                                             int *o_x, int *o_y);


#endif /* GNUCASH_GRID_H */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
