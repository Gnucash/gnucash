/*
 * FILE:
 * combocell.c
 *
 * FUNCTION:
 * implement motif portions of a pull-down combo widget
 * embedded in a table cell.
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas <linas@linas.org>
 * Copyright (c) 1998-1999 Rob Browning <rlb@cs.utexas.edu>
 */
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

/*
   TODO: We have no use for the generic ComboCell->menuitems.  These
   should probably be killed.  Each GUI should probably handle it's
   own strings.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gtk/gtk.h>

#include "gtksheet.h"
#include "gtksheetentry.h"

#include "table-allgui.h"
#include "table-gtk.h"
#include "util.h"
#include "combocell.h"


/* Some GUI-private date that is inappropriate for 
 * the public interface.  In this impelmentation, 
 * it holds gtk data that we need.  */

#define SET(cell,str) { 		   \
  if ((cell)->value) free ((cell)->value); \
  (cell)->value = strdup (str);            \
}

typedef struct _PopBox {
  GList *menustrings;
  GtkSheet *sheet;
} PopBox;

static void realizeCombo (BasicCell *bcell, void *w, int width);
static void moveCombo (BasicCell *bcell, int phys_row, int phys_col);
static void destroyCombo (BasicCell *bcell);
static void setComboValue (BasicCell *bcell, const char *value);
static const char * enterCombo (BasicCell *bcell, const char *value);
static const char * leaveCombo (BasicCell *bcell, const char *value);

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GTK_REG;

/* =============================================== */

ComboCell *xaccMallocComboCell (void) {
  ComboCell * cell;
  cell = (ComboCell *) malloc (sizeof (ComboCell));
  xaccInitComboCell (cell);
  return cell;
}

void xaccInitComboCell (ComboCell *cell) {
  xaccInitBasicCell(&(cell->cell));
  cell->cell.realize = realizeCombo;
  cell->cell.set_value = setComboValue;
  cell->cell.destroy = destroyCombo;
  {
    PopBox *box = (PopBox *) malloc (sizeof (PopBox));
    box->sheet   = NULL;
    box->menustrings = NULL;
    cell->cell.gui_private = box;
  }
}

/* =============================================== */

static void
destroyCombo (BasicCell *bcell) {
  ComboCell *cell = (ComboCell *) bcell;
  
  if (!(cell->cell.realize)) {
    /* allow the widget to be shown again */
    cell->cell.realize = realizeCombo;
    cell->cell.move = NULL;
    cell->cell.enter_cell = NULL;
    cell->cell.leave_cell = NULL;
    cell->cell.destroy = NULL;
  }  
}

/* =============================================== */

void xaccDestroyComboCell (ComboCell *cell) {
  PopBox *box = (PopBox *) (cell->cell.gui_private);
  
  destroyCombo (&(cell->cell));
  
  g_list_foreach(box->menustrings, (GFunc) g_free, NULL);
  g_list_free(box->menustrings);

  free (box);
  box = NULL;

  cell->cell.gui_private = NULL;
  cell->cell.realize = NULL;
  cell->cell.set_value = NULL;
  
  xaccDestroyBasicCell (&(cell->cell));
}

/* =============================================== */

void 
xaccAddComboCellMenuItem (ComboCell *cell, char * menustr) { 
  if (!cell) return;
  if (!menustr) return;

  {
    PopBox *box = (PopBox *) cell->cell.gui_private;
    box->menustrings = g_list_append(box->menustrings, g_strdup(menustr));
    
    /* if we are adding the menu item to a cell that is already
       realized, then alose add it to the widget directly.  */

    if(box->sheet) {
      if(GTK_IS_COMBO(gtk_sheet_get_entry(box->sheet))) {
        GtkCombo *combobox = GTK_COMBO(box->sheet->sheet_entry);
        gtk_combo_set_popdown_strings(combobox, box->menustrings);
      }
    }
  }
}

/* =============================================== */
/* not only do we set the cell contents, but we 
 * make the gui reflect the right value too.
 */

void 
xaccSetComboCellValue (ComboCell *cell, const char * str) {
  PopBox *box = (PopBox *) (cell->cell.gui_private);  

  if(!str) str = "";
  SET (&(cell->cell), str);

  if(box->sheet) {
    gtk_sheet_set_cell_text(box->sheet,
                            box->sheet->active_cell.row,
                            box->sheet->active_cell.col,
                            str);
  }
}

/* =============================================== */

static void
setComboValue (BasicCell *_cell, const char *str) {
  ComboCell * cell = (ComboCell *) _cell;
  xaccSetComboCellValue(cell, str);
}


/* =============================================== */

static void
realizeCombo (BasicCell *bcell, void *data, int pixel_width) {
  GtkSheet *sheet = (GtkSheet *) data;
  ComboCell *cell = (ComboCell *) bcell;
  PopBox *box = cell->cell.gui_private;
  
  /* initialize gui-specific, private data */
  box->sheet   = sheet;
  
  /* to mark cell as realized, remove the realize method */
  cell->cell.realize = NULL;
  cell->cell.move = moveCombo;
  cell->cell.enter_cell = enterCombo;
  cell->cell.leave_cell = leaveCombo;
  cell->cell.destroy = destroyCombo;
}

/* =============================================== */

static void
moveCombo (BasicCell *bcell, int phys_row, int phys_col) {
  /* no op with gtksheet */
  return;
}

/* =============================================== */

static const char *
enterCombo (BasicCell *bcell, const char *value) {

  ComboCell *cell = (ComboCell *) bcell;
  PopBox *box = (PopBox *) (cell->cell.gui_private);

  PINFO("ComboBox(%p): enter value (%s)\n", cell, value);
  
  gtk_sheet_change_entry(box->sheet, gtk_combo_get_type());
  
  {
    GtkCombo *combobox = GTK_COMBO(box->sheet->sheet_entry);

    /* Add all the strings... */
    gtk_combo_set_popdown_strings(combobox, box->menustrings);

    gtk_sheet_set_cell_text(box->sheet,
                            box->sheet->active_cell.row,
                            box->sheet->active_cell.col,
                            value); 
  }
  
  return NULL;
}

/* =============================================== */

static const char *
leaveCombo (BasicCell *bcell, const char *value) {
  ComboCell *cell = (ComboCell *) bcell;
  PopBox *box = (PopBox *) (cell->cell.gui_private);
  GtkCombo *combobox = GTK_COMBO(box->sheet->sheet_entry);

  gtk_sheet_change_entry((box->sheet), gtk_shentry_get_type());  
    
  SET (&(cell->cell), value);
  cell->cell.changed = 0xffffffff;
  
  return NULL;
}

/* =============== end of file =================== */
