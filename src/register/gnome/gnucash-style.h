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

#ifndef GNUCASH_STYLE_H
#define GNUCASH_STYLE_H

#include <gnome.h>

#include "splitreg.h"
#include "gnucash-sheet.h"

void gnucash_style_init (void);

void gnucash_sheet_style_init(void);
void gnucash_sheet_style_set_dimensions (GnucashSheet *sheet,
					 SheetBlockStyle *style);

void gnucash_sheet_style_destroy (SheetBlockStyle *style);

SheetBlockStyle * gnucash_sheet_style_compile (GnucashSheet *sheet,
					       CellBlock *cellblock,
                                               gint cursor_type);

SheetBlockStyle *gnucash_sheet_get_style (GnucashSheet *sheet, gint vrow,
					  gint vcol);

SheetBlockStyle *gnucash_sheet_get_style_from_table (GnucashSheet *sheet,
						     gint vrow, gint vcol);

void gnucash_sheet_style_get_cell_pixel_rel_coords (SheetBlockStyle *style,
                                                    gint cell_row,
						    gint cell_col,
                                                    gint *x, gint *y,
						    gint *w, gint *h);

void gnucash_style_ref (SheetBlockStyle *style);
void gnucash_style_unref (SheetBlockStyle *style);

extern GdkFont *gnucash_default_font;
extern GdkFont *gnucash_italic_font;


#endif


/*
  Local Variables:
  c-basic-offset: 8
  End:
*/
