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
 * Copyright (c) 1998 Rob Browning <rlb@cs.utexas.edu>
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gtk/gtk.h>
#include "table-allgui.h"
#include "table-gtk.h"


#include "combocell.h"

/* Some GUI-private date that is inappropriate for 
 * the public interface.  In this impelmentation, 
 * it holds gtk data that we need.
 */

#define SET(cell,str) { 			\
   if ((cell)->value) free ((cell)->value);	\
   (cell)->value = strdup (str);		\
}

typedef struct _PopBox {
  GtkCombo *combobox;
  GList *menustrings;
  Table *table;
  int currow;
  int curcol;
} PopBox;

//static void selectCB (Widget w, XtPointer cd, XtPointer cb );

static void realizeCombo (BasicCell *bcell, void *w, int width);
static void moveCombo (BasicCell *bcell, int phys_row, int phys_col);
static void destroyCombo (BasicCell *bcell);
static void setComboValue (BasicCell *bcell, const char *value);
static const char * enterCombo (BasicCell *bcell, const char *value);
static const char * leaveCombo (BasicCell *bcell, const char *value);

/* =============================================== */

ComboCell *xaccMallocComboCell (void)
{
  ComboCell * cell;
  cell = (ComboCell *) malloc (sizeof (ComboCell));
  xaccInitComboCell (cell);
  return cell;
}

void xaccInitComboCell (ComboCell *cell)
{
  xaccInitBasicCell ( &(cell->cell));
  cell->cell.realize = realizeCombo;
  cell->cell.set_value = setComboValue;
  cell->cell.destroy = destroyCombo;
  {
    PopBox *box = (PopBox *) malloc (sizeof (PopBox));
    box->table   = NULL;
    box->menustrings = NULL;
    box->combobox = GTK_COMBO(gtk_combo_new());
    gtk_widget_ref(GTK_WIDGET(box->combobox));
    gtk_combo_set_value_in_list(box->combobox, 1, 0);
    gtk_widget_show(GTK_WIDGET(box->combobox));
    cell->cell.gui_private = box;
  }
}

/* =============================================== */

static
void destroyCombo (BasicCell *bcell)
{
  ComboCell *cell = (ComboCell *) bcell;
  PopBox *box = (PopBox *) (cell->cell.gui_private);
  
  /* HACK: I had to put the three extra tests in the guard so that
     we don't get a segfault on register window closes.  I haven't checked
     to be sure this is exactly the right thing to do, but it works.

     Actually, almost all of the combobox (and table-gtk for that
     matter) code is an ugly hack that should go away when we have a
     real table widget... */

  if (!(cell->cell.realize) && box && box->table && box->table->entry_frame) {
    gtk_container_remove(GTK_CONTAINER(box->table->entry_frame),
                         GTK_WIDGET(box->combobox));
    gtk_container_add(GTK_CONTAINER(box->table->entry_frame),
                      GTK_WIDGET(box->table->entry_widget));
    
    /* allow the widget to be shown again */
    cell->cell.realize = realizeCombo;
    cell->cell.move = NULL;
    cell->cell.enter_cell = NULL;
    cell->cell.leave_cell = NULL;
    cell->cell.destroy = NULL;
  }  
}

/* =============================================== */

void xaccDestroyComboCell (ComboCell *cell)
{
  PopBox *box = (PopBox *) (cell->cell.gui_private);
  
  destroyCombo (&(cell->cell));
  
  gtk_widget_destroy(GTK_WIDGET(box->combobox));
  gtk_widget_unref(GTK_WIDGET(box->combobox));

  g_list_foreach(box->menustrings, (GFunc) g_free, NULL);
  g_list_free(box->menustrings);

  free (box);
  box = NULL;

  cell->cell.gui_private = NULL;
  cell->cell.realize = NULL;
  cell->cell.set_value = NULL;
  
  xaccDestroyBasicCell ( &(cell->cell));
}

/* =============================================== */

void 
xaccAddComboCellMenuItem (ComboCell *cell, char * menustr)
{
  PopBox *box = (PopBox *) cell->cell.gui_private;
  GtkList *comboitems = GTK_LIST(box->combobox->list); 

  if (!cell) return;
  if (!menustr) return;

  box->menustrings = g_list_append(box->menustrings, g_strdup(menustr));
  fprintf(stderr, "Adding item: %s\n", menustr);
  
  gtk_combo_set_popdown_strings(box->combobox, box->menustrings);
}

/* =============================================== */
/* not only do we set the cell contents, but we 
 * make the gui reflect the right value too.
 */

void 
xaccSetComboCellValue (ComboCell *cell, const char * str)
{
  PopBox * box;
  
  SET (&(cell->cell), str);
  box = (PopBox *) (cell->cell.gui_private);
  
  if(!str) str = "";
  
  gtk_entry_set_text(GTK_ENTRY(box->combobox->entry), str);
}
  
/* =============================================== */

static void
setComboValue (BasicCell *_cell, const char *str)
{
   ComboCell * cell = (ComboCell *) _cell;
   xaccSetComboCellValue (cell, str);
}


/* =============================================== */

static
void realizeCombo (BasicCell *bcell, void *data, int pixel_width)
{
  Table *table = (Table *) data;
  ComboCell *cell = (ComboCell *) bcell;
  PopBox *box = cell->cell.gui_private;
  
  /* initialize gui-specific, private data */
  box->table   = table;
  box->currow   = -1;
  box->curcol   = -1;
  
  /* to mark cell as realized, remove the realize method */
  cell->cell.realize = NULL;
  cell->cell.move = moveCombo;
  cell->cell.enter_cell = enterCombo;
  cell->cell.leave_cell = leaveCombo;
  cell->cell.destroy = destroyCombo;
}

/* =============================================== */

static
void moveCombo (BasicCell *bcell, int phys_row, int phys_col)
{
  ComboCell *cell = (ComboCell *) bcell;
  PopBox *box = cell->cell.gui_private;
  box->currow = phys_row;
  box->curcol = phys_col;
}

/* =============================================== */

static
const char * enterCombo (BasicCell *bcell, const char *value)
{
  ComboCell *cell = (ComboCell *) bcell;
  PopBox *box = (PopBox *) (cell->cell.gui_private);
  int phys_row = box->currow;
  int phys_col = box->curcol;
  char *choice;

  /* if the new position is valid, go to it, 
   * otherwise, unmanage the widget */

  if ((0 <= phys_row) && (0 <= phys_col)) {
    
    /* Get the current cell contents, and set the
     * combobox menu selection to match the contents. 
     * We could use the value passed in, but things should
     * be consitent, so we don't need it. */
    choice = cell->cell.value;
    
    if(!choice) choice = "";
    
    if(GTK_WIDGET(box->table->entry_widget)->parent == 
       box->table->entry_frame) {
      gtk_container_remove(GTK_CONTAINER(box->table->entry_frame),
                           GTK_WIDGET(box->table->entry_widget));
    }
    gtk_container_add(GTK_CONTAINER(box->table->entry_frame),
                      GTK_WIDGET(box->combobox));
    
    gtk_entry_set_text(GTK_ENTRY(box->combobox->entry), choice);
  }  else {
    gtk_container_remove(GTK_CONTAINER(box->table->entry_frame),
                         GTK_WIDGET(box->combobox));
    gtk_container_add(GTK_CONTAINER(box->table->entry_frame),
                      GTK_WIDGET(box->table->entry_widget));
  }
  
  return NULL;
}

/* =============================================== */

static
const char * leaveCombo (BasicCell *bcell, const char *value)
{
  ComboCell *cell = (ComboCell *) bcell;
  PopBox *box = (PopBox *) (cell->cell.gui_private);
  gchar *text;
  
  /* check for a valid mapping of the widget.  
     Note that if the combo box value is set to 
     a string that is not in the combo box menu
     (for example, the empty string ""), then the 
     combobox will issue an XmCR_UNSELECT event.
     This typically happens while loading the array.
     We want to ignore these. */
  if ((0 > box->currow) || (0 > box->curcol)) return NULL;
  
  text = gtk_entry_get_text(GTK_ENTRY(box->combobox->entry));
  
  /* be sure to set the string into the matrix widget as well,
     so that we don't end up blanking out the cell when we 
     unmap the combobox widget */
  gtk_clist_set_text(GTK_CLIST(box->table->table_widget),
                     box->currow - 1,
                     box->curcol,
                     text); 
  
  SET (&(cell->cell), text);
  cell->cell.changed = 0xffffffff;
  gtk_container_remove(GTK_CONTAINER(box->table->entry_frame),
                       GTK_WIDGET(box->combobox));
  gtk_container_add(GTK_CONTAINER(box->table->entry_frame),
                    GTK_WIDGET(box->table->entry_widget));
  
  return NULL;
}

/* =============== end of file =================== */
