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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNUCASH_HEADER_H
#define GNUCASH_HEADER_H

#include <gnome.h>


#define GNUCASH_TYPE_HEADER     (gnucash_header_get_type ())
#define GNUCASH_HEADER(obj)     (GTK_CHECK_CAST((obj), GNUCASH_TYPE_HEADER, GnucashHeader))
#define GNUCASH_HEADER_CLASS(k) (GTK_CHECK_CLASS_CAST ((k), GNUCASH_TYPE_HEADER))
#define GNUCASH_IS_HEADER(o)    (GTK_CHECK_TYPE((o), GNUCASH_TYPE_HEADER))

GtkType    gnucash_header_get_type (void);

typedef struct {
        GnomeCanvasItem canvas_item;

        GnucashSheet *sheet;
        SheetBlockStyle *style;

        char *cursor_name;

        int num_phys_rows;

        int in_resize;
        int resize_col_width;
        int resize_x;
        int resize_col;

        gboolean needs_ungrab;

        int height;
        int width;

        GdkGC *gc;
        GdkCursor *normal_cursor;
        GdkCursor *resize_cursor;
} GnucashHeader;


typedef struct {
        GnomeCanvasItemClass parent_class;
} GnucashHeaderClass;


GtkWidget *gnucash_header_new (GnucashSheet *sheet);
void gnucash_header_reconfigure (GnucashHeader *header);

void gnucash_header_set_header_rows (GnucashHeader *header,
                                     int num_phys_rows);

#endif /* GNUCASH_HEADER_H */

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
