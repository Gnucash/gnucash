/*
 * FILE:
 * table-gtk.c
 *
 * FUNCTION:
 * Implements the infrastructure for the displayed table.
 * This is the Gtk implementation;
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
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

/*
  TODO:
  
  Everywhere I use row + 1, it should be row + num_header_rows

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>
#include <gtk/gtk.h>

#include "cellblock.h"
#include "table-allgui.h"
#include "table-gtk.h"

#if 0

/* The XrmQuarks are used to figure out the direction of
 * traversal from cell to cell */

//static XrmQuark QPointer, QLeft, QRight, QUp, QDown;
//static Boolean haveQuarks = False;

#endif

/* ==================================================== */

Table * 
xaccMallocTable (void)
{
   Table *table;
   table = (Table *) malloc (sizeof (Table));
   xaccInitTable (table);
   return table;
}

/* ==================================================== */

void 
xaccInitTable (Table * table)
{
   table->table_widget = NULL;
   table->entry_frame = NULL;
   table->entry_widget = NULL;

   table->current_col = -1;  /* coords ignoring header lines */
   table->current_row = -1;
   table->prev_entry_text = NULL;
   
   table->next_tab_group = 0;

   table->num_phys_rows = -1;
   table->num_phys_cols = -1;
   table->num_virt_rows = -1;
   table->num_virt_cols = -1;

   table->current_cursor = NULL;
   table->current_cursor_virt_row = -1;
   table->current_cursor_virt_col = -1;
   table->current_cursor_phys_row = -1;
   table->current_cursor_phys_col = -1;

   table->move_cursor = NULL;
   table->client_data = NULL;

   table->entries = NULL;
   table->locators = NULL;
   table->user_data = NULL;
   table->handlers = NULL;

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;
}

/* ==================================================== */

void 
xaccDestroyTable (Table * table)
{
   /* free the gui-independent parts */
   xaccFreeTableEntries (table);

   /* Let GTK know we're finished with this */
   if(table->table_widget) gtk_widget_unref(table->table_widget);
   if(table->entry_frame) gtk_widget_unref(table->entry_frame);
   if(table->entry_widget) gtk_widget_unref(table->entry_widget);
   table->table_widget = NULL;
   table->entry_frame = NULL;
   table->entry_widget = NULL;

   g_free(table->prev_entry_text); table->prev_entry_text = NULL;

   /* intialize vars to null value so that any access is voided. */
   xaccInitTable (table);
   free (table);
}

/* ==================================================== */

void 
xaccSetTableSize (Table * table, int phys_rows, int phys_cols,
                                 int virt_rows, int virt_cols)
{
   xaccTableResize (table, phys_rows, phys_cols, virt_rows, virt_cols);

   /* invalidate the "previous" traversed cell */
   table->prev_phys_traverse_row = -1;
   table->prev_phys_traverse_col = -1;

   /* invalidate the current cursor position, if needed */
   if ((table->current_cursor_virt_row >= table->num_virt_rows) ||
       (table->current_cursor_virt_col >= table->num_virt_cols)) {
      table->current_cursor_virt_row = -1;
      table->current_cursor_virt_col = -1;
      table->current_cursor_phys_row = -1;
      table->current_cursor_phys_col = -1;
      table->current_cursor = NULL;
   }
}

/* ==================================================== */

void
xaccNextTabGroup (Table *table, GtkWidget * w)
{
   table->next_tab_group = w;
}

static int
verify_cell_interaction_OK(Table *table, const int row, const int col)
{
  /* This was cellCB in a previous (Motif) life. We don't do nearly
     the checking that the Xbae code did because we presume that the
     row/column has to be valid if the user could have clicked on it.
     That's reasonable given the way that CList works.  For one thing,
     header rows are handled separately. */
  
  const CellBlock *arr = table->current_cursor;
  int rel_row, rel_col;
  int invalid = 0;
  
  /* make sure that we've repositioned the cursor to this location,
   * and that we've dragged along any subsidiary GUI's with us. 
   * Do this *before* we use the value of the "current cursor"
   */
  xaccVerifyCursorPosition (table, row, col);

  arr = table->current_cursor;

  /* compute the cell location */
  rel_row = table->locators[row][col]->phys_row_offset;
  rel_col = table->locators[row][col]->phys_col_offset;

  /* verify that cursor offsets are valid.  This may occur if 
   * the app that is using the table has a paritally initialized
   * cursor. (probably due to a prograing error, but maybe they 
   * meant to do this). */
  invalid = invalid || (0 > rel_row);
  invalid = invalid || (0 > rel_col);

  /* check for a cell handler, but only if cell adress is valid */
  /* GTK may not need all these checks, but they don't hurt */
  if (arr && !invalid) {
    if (! (arr->cells[rel_row][rel_col])) {
      invalid = TRUE;
    } else {
      /* if cell is marked as output-only,
       * then don't call callbacks */
      if (0 == (arr->cells[rel_row][rel_col])->input_output) {
        invalid = TRUE;
      }
    }
  } else {
    invalid = TRUE;
  }

  return(!invalid);
}

/* ==================================================== */
/* this callback assumes that basic error checking has already
 * been performed. */

static void
cell_entered(Table *table, const int row, const int col)
{
  CellBlock *arr;
  int rel_row, rel_col;
  const char * (*enter) (BasicCell *, const char *);
  gchar *text;
  GtkCList *cl = GTK_CLIST(table->table_widget);
  
  fprintf(stderr, "cell_entered: %d %d\n", row - 1, col);
  
  gtk_clist_get_text(cl, row - 1, col, &text);
  text = g_strdup(text);  /* This is OK and required b/c GTK kills the
                             old pointer before using the one you
                             give it. */
  
  { 
    GtkWidget *w = GTK_WIDGET(cl);
    GdkColor *background = &w->style->bg[GTK_STATE_NORMAL];
    GdkBitmap *mask;
    GdkPixmap *pm = gdk_pixmap_create_from_xpm(w->window, &mask, background,
                                               "/home/rlb/xacc/src/register/"
                                               "left_arrow_small.xpm");
    gtk_clist_set_pixtext(cl, row - 1, col, text, 2, pm, mask);
    gdk_pixmap_unref(pm);
    gdk_bitmap_unref(mask);
  }
  
  gtk_frame_set_label(GTK_FRAME(table->entry_frame),
                      cl->column[col].title);

  /* Have to block and unblock here because we don't want a callback
     for this "edit" */
  {
    const guint handler_id =
      (guint) gtk_object_get_user_data(GTK_OBJECT(table->entry_widget));
    
    gtk_signal_handler_block(GTK_OBJECT(table->entry_widget), handler_id);
    gtk_entry_set_text(GTK_ENTRY(table->entry_widget), text);
    gtk_signal_handler_unblock(GTK_OBJECT(table->entry_widget), handler_id);
  }

  g_free(table->prev_entry_text);
  table->prev_entry_text = text;
  
  fprintf(stderr,
          "  current_cursor->phys: %d %d\n"
          "  current_cursor->virt: %d %d\n"          
          "  text: %s\n",
          table->current_cursor_phys_row,
          table->current_cursor_phys_col,
          table->current_cursor_virt_row,
          table->current_cursor_virt_col,
          text);

   xaccVerifyCursorPosition (table, row, col);
   
   arr = table->current_cursor;
   
   rel_row = table->locators[row][col]->phys_row_offset;
   rel_col = table->locators[row][col]->phys_col_offset;
   
   /* OK, if there is a callback for this cell, call it */
   enter = arr->cells[rel_row][rel_col]->enter_cell;
   if (enter) {
     const char *val;
     char *retval;
     
     val = table->entries[row][col];
     retval = (char *) enter (arr->cells[rel_row][rel_col], val);
     if (NULL == retval) retval = (char *) val;
     if (val != retval) {
       if (table->entries[row][col]) free (table->entries[row][col]);
       table->entries[row][col] = retval;
       (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
       
       /* ??? Should this be setting the entry or the clist cell? */
       gtk_entry_set_text(GTK_ENTRY(table->entry_widget), retval);
     }
   }
   
   /* record this position as the cell that will be
    * traversed out of if a traverse even happens */
   table->prev_phys_traverse_row = row;
   table->prev_phys_traverse_col = col;
}

static void
compute_string_single_change(const gchar *a, const gchar *b, gchar **result) {
  /* Compute the change from a to b assuming that the changed region
     is contiguous.  This is only a guess, the solution is
     ambiguous. */
  
  const gint a_len = strlen(a); 
  const gint b_len = strlen(b); 
  const gchar *afptr = a, *bfptr = b;
  const gchar *arptr = a + a_len;
  const gchar *brptr = b + b_len;
  
  while(*afptr && *bfptr && (*afptr == *bfptr)) {
    afptr++;
    bfptr++;
  }
  
  while((arptr != afptr) && (brptr != bfptr) && (*arptr == *brptr)) {
    arptr--;
    brptr--;
  }
  if(a_len == b_len) brptr++;

  if(bfptr == brptr) {
    /* deletion or nothing */
    *result = NULL;
    return;
  } else {
    const gint length = (brptr - bfptr);
    *result = (char *) g_malloc(length * sizeof(char) + 1);
    strncpy(*result, bfptr, length);
    (*result)[length] = '\0';
    return;
  }
}
    
/* ==================================================== */
/* this routine calls the individual cell callbacks */

static void
cell_modified(Table *table, const int row, const int col)
{

  CellBlock *arr;
  GtkEntry *entry = GTK_ENTRY(table->entry_widget);
  int rel_row, rel_col;
  const char * (*mv) (BasicCell *, 
                      const char *, 
                      const char *, 
                      const char *);
  const char *oldval, *newval, *retval, *final_text;
  gchar *change = NULL;

  arr = table->current_cursor;
  
  /* compute the cell location */
  rel_row = table->locators[row][col]->phys_row_offset;
  rel_col = table->locators[row][col]->phys_col_offset;
  
  /* accept edits by default, unless the cell handler rejects them */
  /* cbs->verify->doit = True; */
  
  oldval = table->prev_entry_text;
  newval = strdup(gtk_entry_get_text(entry));
  final_text = newval;
  
  if(oldval) compute_string_single_change(oldval, newval, &change);

  fprintf(stderr, "   CHANGES: (%s -> %s) <=> [%s]\n",
          oldval, newval, change);
  
  if(strcmp(newval, oldval) == 0) {
    g_free((gchar *) change);
    return;
  }

  /* OK, if there is a callback for this cell, call it */
  mv = arr->cells[rel_row][rel_col]->modify_verify;

  if (mv) {
    retval = (*mv) (arr->cells[rel_row][rel_col], oldval, change, newval);
    
    /* if the callback returned a non-null value, allow the edit */
    if (retval) {
      
      /* update data. bounds check done earlier */
      free (table->entries[row][col]);
      table->entries[row][col] = (char *) retval;
      (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
      
      /* if the callback modified the display string,
       * update the display cell as well */
      if (retval != newval) {
        {
          /* Don't want a signal for this change */
          const guint id =
            (guint) gtk_object_get_user_data(GTK_OBJECT(table->entry_widget));
          gtk_signal_handler_block(GTK_OBJECT(table->entry_widget), id);
          gtk_entry_set_text(entry, retval);
          gtk_signal_handler_unblock(GTK_OBJECT(table->entry_widget), id);
        }
        final_text = retval;
        /*XbaeMatrixSetCursorPosition (mw, (cbs->verify->endPos) +1);*/
        free((char *) newval);
      }
    } else {
      free((char *) newval);
    }
  } else {
    /* update data. bounds check done earlier */
    free (table->entries[row][col]);
    table->entries[row][col] = strdup(newval);
    (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
  }
  g_free(table->prev_entry_text);
  table->prev_entry_text = g_strdup(final_text);
  g_free((gchar *) change);
}

/* ==================================================== */

static void
cell_left(Table *table, const int row, const int col)
{
  CellBlock *arr;
  int rel_row, rel_col;
  const char * (*leave) (BasicCell *, const char *);
  char * newval;
  const char *val;
  gchar *text;
  GdkBitmap *mask;
  GtkCList *cl = GTK_CLIST(table->table_widget);

  fprintf(stderr, "cell_left: %d %d\n", row - 1, col);

  if(gtk_clist_get_pixtext(cl, row - 1, col, &text, NULL, NULL, &mask)) {
    /* we need to get rid of the pixmap -- this will only fail on the
       first click when there was no previous cell (but we default to
       0 0) */
    text = g_strdup(text);  /* This is OK and required b/c we're about to
                               kill this pointer via set_text. */
    gtk_clist_set_text(cl, row - 1, col, text);
    g_free(text);
  }
  
  arr = table->current_cursor;
  
  /* compute the cell location */
  rel_row = table->locators[row][col]->phys_row_offset;
  rel_col = table->locators[row][col]->phys_col_offset;

  val = gtk_entry_get_text(GTK_ENTRY(table->entry_widget));
  
  /* OK, if there is a callback for this cell, call it */
  leave = arr->cells[rel_row][rel_col]->leave_cell;
  if (leave) {
    const char *retval;
    
    retval = leave (arr->cells[rel_row][rel_col], val);
    
    newval = (char *) retval;
    if (NULL == retval) newval = strdup (val);
    if (val == retval) newval = strdup (val);
    
    /* if the leave() routine declared a new string, lets use it */
    if ( retval && (retval != val)) {
      gtk_clist_set_text(cl, row - 1, col, (gchar *) retval);
    } 
  } else {
    newval = strdup(val);
  }

  /* Commit the change to the clist when we leave the cell */
  gtk_clist_set_text(GTK_CLIST(table->table_widget), row - 1, col, newval);
  
  /* save whatever was returned; but lets check for  
   * changes to avoid roiling the cells too much */
  if (table->entries[row][col]) {
    if (strcmp (table->entries[row][col], newval)) {
      free (table->entries[row][col]);
      table->entries[row][col] = newval;
      (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
    } else {
      /* leave() allocated memory, which we will not be using ... */
      free(newval);
    }
  } else {
    table->entries[row][col] = newval;
    (arr->cells[rel_row][rel_col])->changed = 0xffffffff;
  }
}

#if 0

/* ==================================================== */

static void
traverseCB (GtkWidget * mw, gpointer cd, gpointer cb)
{
   Table *table;
   CellBlock *arr;
   XbaeMatrixTraverseCellCallbackStruct *cbs;
   int row, col;
   int rel_row, rel_col;

   table = (Table *) cd;
   arr = table->current_cursor;
   cbs = (XbaeMatrixTraverseCellCallbackStruct *) cb;

   row = cbs->row;
   col = cbs->column;

   /* If the quark is zero, then it is likely that we are
    * here because we traversed out of a cell that had a 
    * ComboBox in it.  The ComboCell is clever enough to put
    * us back into the register after tabing out of it.
    * However, its not (cannot be) clever enough to pretend
    * that it was a tab group in the register.  Thus,
    * we will emulate that we left a tab group in the register
    * to get here.  To put it more simply, we just set the 
    * row and column to that of the ComboCell, which we had
    * previously recorded, and continue on as if nothing 
    * happened.  
    * BTW -- note that we are emulating a normal, right-moving tab. 
    * Backwards tabs are broken. 
    */
   if (NULLQUARK == cbs->qparam) {
      if ((0==row) && (0==col)) {
        if ((0 <= table->prev_phys_traverse_row) && 
            (0 <= table->prev_phys_traverse_col)) {
          cbs->qparam = QRight;
          row = table->prev_phys_traverse_row;
          col = table->prev_phys_traverse_col;
        }
      }
   }

   xaccVerifyCursorPosition (table, row, col);

   /* compute the cell location */
   rel_row = table->locators[row][col]->phys_row_offset;
   rel_col = table->locators[row][col]->phys_col_offset;

   /* process right-moving traversals */
   if (QRight == cbs->qparam) {
      int next_row = arr->right_traverse_r[rel_row][rel_col];
      int next_col = arr->right_traverse_c[rel_row][rel_col];

      /* if we are at the end of the traversal chain,
       * hop out of this tab group, and into the next.
       */
      if ((0 > next_row) || (0 > next_col)) {
         /* reverse the sign of next_row, col to be positive. */
         cbs->next_row    = row - rel_row - next_row -1; 
         cbs->next_column = col - rel_col - next_col -1;
         cbs->qparam      = NULLQUARK; 
         if (table->next_tab_group) {
            XmProcessTraversal (table->next_tab_group, 
                                XmTRAVERSE_CURRENT); 
         }
      } else {
         cbs->next_row    = row - rel_row + next_row; 
         cbs->next_column = col - rel_col + next_col;
      }
   } 

   table->prev_phys_traverse_row = cbs->next_row;
   table->prev_phys_traverse_col = cbs->next_column;
}

#endif

static int counter;

static void
table_edit_entry_cb(GtkEntry *entry, gpointer user_data) {
  Table *table = (Table *) user_data;
  const int row = table->current_row;
  const int col = table->current_col;

  fprintf(stderr, "table_edit_entry_cb:\n");
  fprintf(stderr, "  curpos: %d selstart %d  selend %d has_select: %d\n",
          GTK_EDITABLE(entry)->current_pos,
          GTK_EDITABLE(entry)->selection_start_pos,
          GTK_EDITABLE(entry)->selection_end_pos,
          GTK_EDITABLE(entry)->has_selection
          );

  if(!verify_cell_interaction_OK(table, row + 1, col)) return;

  {
    const int xxx = counter++;
    printf("  cm: in %d\n", xxx);
    cell_modified(table, row + 1, col);
    printf("  cm: out %d\n", xxx);
  }
}

static void
table_select_row_cb(GtkCList *cl, gint row, gint column, GdkEventButton *e,
                    gpointer user_data) {
  Table *table = (Table *) user_data;

  fprintf(stderr, "table_select_row_cb: %d %d\n", row, column);

  //gtk_clist_unselect_row(cl, row, column);

  if(!verify_cell_interaction_OK(table, row + 1, column)) return;

  if(table->current_col != -1) {
    cell_left(table, table->current_row + 1, table->current_col);
  }

  table->current_col = column;
  table->current_row = row;

  {
    const int xxx = counter++;
    printf("  ce: in %d\n", xxx);
    cell_entered(table, row + 1, column);
    printf("  ce: out %d\n", xxx);
  }
}

/* ==================================================== */

GtkWidget *
xaccCreateTable (Table *table, GtkWidget * parent) 
{
  CellBlock *curs;
  unsigned char * alignments;
  short * widths;
  int num_header_rows = 0;
  int i;
  
  if (!table) return 0;

  if(table->table_widget != NULL) {
    fprintf(stderr,
            "Error: detected internal corruption in xaccCreateTable, "
            "aborting\n");
    return 0;
  }

#if 0  
  /* if quarks have not yet been initialized for this 
   * application, initialize them now. */
  if (!haveQuarks) {
    QPointer = XrmPermStringToQuark ("Pointer");
    QLeft    = XrmPermStringToQuark ("Left");
    QRight   = XrmPermStringToQuark ("Right");
    QUp      = XrmPermStringToQuark ("Up");
    QDown    = XrmPermStringToQuark ("Down");
    haveQuarks = True;
  }
#endif
  
  /* The 0'th row of the handlers is defined as the header */
  alignments = NULL;
  widths = NULL;
  curs = table->handlers[0][0];
  alignments = curs->alignments;
  widths = curs->widths;
  num_header_rows = curs->numRows;
  
  /* copy header data into entries cache */
  xaccRefreshHeader (table);
  
  /* create the matrix widget */
  if(num_header_rows > 1) {
    fprintf(stderr,
            "gnucash: GTK implementation only supports one header row.\n");
    num_header_rows = 1;
  }
  
  {
    /* TODO: Handle unrefs in destructor */

    GtkWidget *vbox;
    GtkWidget * reg;

    /* We don't ref this vbox because we never use it in our code again */
    vbox = gtk_vbox_new(FALSE, 0);
    gtk_container_add(GTK_CONTAINER(parent), vbox);
    
    if(num_header_rows == 0) {
      reg = gtk_clist_new(table->num_phys_cols);
      gtk_widget_ref(reg);
    } else {        
      reg = gtk_clist_new_with_titles(table->num_phys_cols,
                                      table->entries[0]);
      gtk_widget_ref(reg);
      gtk_clist_freeze(GTK_CLIST(reg));
      for(i = 0; i < table->num_phys_cols; i++) {
        
        /* Widths are in units of characters, not pixels, so we have
           this hack.  It should be fixed later... */
        gtk_clist_set_column_width(GTK_CLIST(reg), i, widths[i] * 5);
      }
      gtk_clist_thaw(GTK_CLIST(reg));
    }
    
    gtk_clist_freeze(GTK_CLIST(reg));
    for(i = num_header_rows; i < table->num_phys_rows; i++) {
      gtk_clist_append(GTK_CLIST(reg), table->entries[i]);
    }
    gtk_clist_set_selection_mode(GTK_CLIST(reg), GTK_SELECTION_BROWSE);
    gtk_clist_thaw(GTK_CLIST(reg));
    
    /* size?
       alignments?
       grid type?
       shadow type?
    */
    
    gtk_signal_connect (GTK_OBJECT (reg), "select_row",
                        GTK_SIGNAL_FUNC (table_select_row_cb),
                        (gpointer) table);
    
    // unselect is mostly useless for us since it doesn't get called when
    // you click on a different cell in the same row.
    //gtk_signal_connect (GTK_OBJECT (reg), "unselect_row",
    //                    GTK_SIGNAL_FUNC (table_unselect_row_cb),
    //                    (gpointer) table);

    gtk_box_pack_start(GTK_BOX(vbox), reg, TRUE, TRUE, 0);
    gtk_widget_show(reg);
    
    table->table_widget = reg;

    {
      GtkWidget *entry_frame = NULL;
      GtkWidget *entry_widget = NULL;
      
      entry_frame = gtk_frame_new("<none>");
      gtk_widget_ref(entry_frame);
      entry_widget = gtk_entry_new();
      gtk_widget_ref(entry_widget);

      gtk_container_add(GTK_CONTAINER(entry_frame), entry_widget);
      gtk_box_pack_start(GTK_BOX(vbox), entry_frame, FALSE, FALSE, 0);
      
      {
        const guint handler_id = 
          gtk_signal_connect (GTK_OBJECT(entry_widget), "changed",
                              GTK_SIGNAL_FUNC(table_edit_entry_cb),
                              (gpointer) table);
        
        gtk_object_set_user_data(GTK_OBJECT(entry_widget),
                                 (gpointer) handler_id);
      }

      gtk_widget_show(entry_widget);
      gtk_widget_show(entry_frame);

      gtk_widget_show(vbox);
      table->entry_frame = entry_frame;
      table->entry_widget = entry_widget;
    }
  }
  
  /* if any of the cells have GUI specific components that need 
   * initialization, initialize them now. 
   * The cell realize method, if present on a cell,
   * is how that cell can find out that now is the time to 
   * initialize that GUI.
   */
  
  curs = table->current_cursor;
  if (curs) {
    int i,j;
    
    for (i=0; i<curs->numRows; i++) {
      for (j=0; j<curs->numCols; j++) {
        BasicCell *cell;
        cell = curs->cells[i][j];
        if (cell) {
          void (*xt_realize) (BasicCell *,  void *gui, int pixel_width);
          xt_realize = cell->realize;
          if (xt_realize) {
            int pixel_width;
            /* cl->column[col].width */
            /*pixel_width = XbaeMatrixGetColumnPixelWidth (reg, j);*/
            xt_realize (cell, ((void *) table), 0);
            /*xt_realize (cell, ((void *) reg), pixel_width);*/
          }
        }
      }
    }
  }
  return (table->table_widget);
}

/* ==================================================== */

void        
xaccRefreshTableGUI (Table * table)
{
  CellBlock *curs;
  GtkWidget * reg;
  int num_header_rows = 0;
  int i, j;
  
  printf("xaccRefreshTableGUI(%p)\n", table);
  if (!table) return;

  /* The 0'th row of the handlers is defined as the header */
  curs = table->handlers[0][0];
  num_header_rows = curs->numRows;
  reg = table->table_widget;

  printf (" refresh numphysrows=%d numphyscols=%d \n",
          table->num_phys_rows, table->num_phys_cols);
  
  for (i = num_header_rows; i < table->num_phys_rows; i++) {
    printf ("cell %d act:%s descr: %s \n",
            i, table->entries[i][2],
            table->entries[i][3]);
  }

  gtk_clist_freeze(GTK_CLIST(reg));

  while(GTK_CLIST(reg)->rows < (table->num_phys_rows - num_header_rows)) {
    gtk_clist_append(GTK_CLIST(reg), NULL);
  }

  for(i = num_header_rows; i < table->num_phys_rows; i++)
  {
    for(j = 0; j < table->num_phys_cols; j++) {
      gtk_clist_set_text(GTK_CLIST(reg), i - num_header_rows, j,
                         table->entries[i][j]);
    }
  }
  gtk_clist_thaw(GTK_CLIST(reg)); 
}

/* ================== end of file ======================= */

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c
  c-indentation-style: gnu
  eval: (c-set-offset 'substatement-open 0)
  End:
*/
