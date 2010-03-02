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

#ifndef GNUCASH_HEADER_H
#define GNUCASH_HEADER_H

#include <gnome.h>


#define GNC_TYPE_HEADER     (gnc_header_get_type ())
#define GNC_HEADER(o)       (G_TYPE_CHECK_INSTANCE_CAST((o), GNC_TYPE_HEADER, GncHeader))
#define GNC_HEADER_CLASS(k) (G_TYPE_CHECK_CLASS_CAST ((k), GNC_TYPE_HEADER, GncHeaderClass))
#define GNC_IS_HEADER(o)    (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_HEADER))

GType    gnc_header_get_type (void);

typedef struct
{
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
} GncHeader;


typedef struct
{
    GnomeCanvasItemClass parent_class;
} GncHeaderClass;


GtkWidget *gnc_header_new (GnucashSheet *sheet);
void gnc_header_reconfigure (GncHeader *header);

void gnc_header_set_header_rows (GncHeader *header,
                                 int num_phys_rows);

#endif /* GNUCASH_HEADER_H */

/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
