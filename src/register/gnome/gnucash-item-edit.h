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

#ifndef GNUMERIC_ITEM_EDIT_H
#define GNUMERIC_ITEM_EDIT_H


#include "gnucash-sheet.h"
#include "gnucash-color.h"
#include "gnucash-grid.h"
#include "gnucash-cursor.h"

#define ITEM_EDIT(obj)          (GTK_CHECK_CAST((obj), item_edit_get_type (), ItemEdit))
#define ITEM_EDIT_CLASS(k)      (GTK_CHECK_CLASS_CAST ((k), item_edit_get_type (), ItemEditClass))
#define IS_ITEM_EDIT(o)         (GTK_CHECK_TYPE((o), item_edit_get_type ()))


typedef struct 
{
        GnomeCanvasItem canvas_item;

        /* The editor whose status we reflect on the spreadsheet */
        GtkWidget  *editor;
        guint      signal;     	/* the signal we connect */
        guint      signal2;	   /* the other signal we connect */

        GtkWidget *combo_widget;
        GnomeCanvasItem *combo_item; 
        GnucashSheet *sheet;
        GdkGC *gc;

        /* Where are we */
        int cell_col, cell_row, virt_col, virt_row;

        SheetBlockStyle  *style;
} ItemEdit;


GtkType item_edit_get_type (void);

void item_edit_configure (ItemEdit *item_edit);

void item_edit_get_pixel_coords (ItemEdit *item_edit, int *x, int *y,
				 int *w, int *h);

GnomeCanvasItem *item_edit_new (GnomeCanvasGroup *parent,
				GnucashSheet *sheet, GtkWidget *entry);


typedef struct {
        GnomeCanvasItemClass parent_class;
} ItemEditClass;


#endif /* GNUMERIC_ITEM_EDIT_H */


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
