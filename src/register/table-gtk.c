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

#include "gtksheet.h"

#include "cellblock.h"
#include "table-allgui.h"
#include "table-gtk.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

/* ==================================================== */

void
xaccNextTabGroup (Table *table, GtkWidget * w) {
   table->next_tab_group = w;
}

/* ==================================================== */

#if 0

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
      if (0 == (XACC_CELL_ALLOW_INPUT &
                ((arr->cells[rel_row][rel_col])->input_output))) {
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
cell_entered(Table *table, const int row, const int col) {

#if OLD_CLIST_REG
  
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

#endif
}

/* ==================================================== */

static void
compute_string_single_change(const gchar *a, const gchar *b, gchar **result) {
  /* Compute the change from a to b assuming that the changed region
     is contiguous.  This is only a guess, the solution is
     inherently ambiguous. */
  
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
#if OLD_CLIST_REG

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

#endif
}

/* ==================================================== */

static void
cell_left(Table *table, const int row, const int col) {

#if OLD_CLIST_REG

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

#endif

}
#endif

#if 0

/* ==================================================== */

static void
traverseCB (GtkWidget * mw, gpointer cd, gpointer cb) {

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

/* ==================================================== */

static void
table_activate_cell_cb(GtkSheet *s, gint row, gint column, gpointer data) {
  L("activate_cell %d %d\n", row, column);

  


}

static void
table_set_cell_cb(GtkSheet *s, gint row, gint column, gpointer data) {
  L("set_cell %d %d\n", row, column);
}

static void
table_changed_cb(GtkSheet *s, gint row, gint column, gpointer data) {
  L("changed %d %d\n", row, column);
}

#if 0

static int counter;

static void
table_edit_entry_cb(GtkEntry *entry, gpointer user_data) {

#if OLD_CLIST_REG

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

#endif

}

/* ==================================================== */

static void
table_select_row_cb(GtkCList *cl, gint row, gint column, GdkEventButton *e,
                    gpointer user_data) {

#if OLD_CLIST_REG

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

#endif
}

#endif

/* ==================================================== */

GtkWidget *
xaccCreateTable (Table *table, GtkWidget *parent)  {
  
  CellBlock *curs;
  unsigned char * alignments;
  short * widths;
  int num_header_rows = 0;
  
  if (!table) return 0;

  if(table->table_widget != NULL) {
    fprintf(stderr,
            "Error: detected internal corruption in xaccCreateTable, "
            "aborting\n");
    return 0;
  }

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

    reg = gtk_sheet_new(table->num_phys_rows - num_header_rows,
                        table->num_phys_cols,
                        "THE REGISTER");
    gtk_sheet_hide_row_titles(GTK_SHEET(reg));
    gtk_sheet_show_column_titles(GTK_SHEET(reg));    
    gtk_widget_ref(reg);

    gtk_sheet_freeze(GTK_SHEET(reg));

    /* Set the column titles */
    if(num_header_rows == 1) {
      int col;
      for(col = 0; col < table->num_phys_cols; col++) {
        gtk_sheet_set_column_title(GTK_SHEET(reg), col, table->entries[0][col]);
      }
    }

    {
      int row, col;
      
      /* Set the column widths */
      for(col = 0; col < table->num_phys_cols; col++) {
        /* HACK: Widths are in units of characters, not pixels, so we
           have this.  It should be fixed later... */
        const int char_width =
          gdk_char_width (GTK_WIDGET(reg)->style->font,(gchar)'X');
        const int col_width = (widths[col] < 2) ? 2 : widths[col];

        gtk_sheet_set_column_width(GTK_SHEET(reg),
                                   col,
                                   col_width * char_width);
      }
      
      /* GTK_SHEET_SET_FLAGS(reg, GTK_SHEET_FLAGS(reg) & SHEET_AUTORESIZE); */

      /* Set the cell contents */
      for(row = num_header_rows; row < table->num_phys_rows; row++) {
        for(col = 0; col < table->num_phys_cols; col++) {
          gtk_sheet_set_cell(GTK_SHEET(reg),
                             row, col,
                             GTK_JUSTIFY_LEFT,
                             table->entries[row][col]);
        }
      }
    }
    gtk_sheet_thaw(GTK_SHEET(reg));
    
    gtk_box_pack_start(GTK_BOX(vbox), reg, TRUE, TRUE, 0);
    gtk_widget_show(reg);
    gtk_widget_show(vbox);
    
    table->table_widget = reg;


    gtk_signal_connect (GTK_OBJECT (reg), "activate_cell",
                        GTK_SIGNAL_FUNC (table_activate_cell_cb),
                        (gpointer) table);
    gtk_signal_connect (GTK_OBJECT (reg), "set_cell",
                        GTK_SIGNAL_FUNC (table_set_cell_cb),
                        (gpointer) table);
    gtk_signal_connect (GTK_OBJECT (reg), "changed",
                        GTK_SIGNAL_FUNC (table_changed_cb),
                        (gpointer) table);
    
#if OLD_CLIST_REG    
    
    gtk_signal_connect (GTK_OBJECT (reg), "select_row",
                        GTK_SIGNAL_FUNC (table_select_row_cb),
                        (gpointer) table);
    
    // unselect is mostly useless for us since it doesn't get called when
    // you click on a different cell in the same row.
    //gtk_signal_connect (GTK_OBJECT (reg), "unselect_row",
    //                    GTK_SIGNAL_FUNC (table_unselect_row_cb),
    //                    (gpointer) table);

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
#endif
  }
  
  /* initialize any cell gui elements now, if any */
  xaccCreateCursor (table, table->current_cursor);

  return (table->table_widget);
}

/* ==================================================== */
/* if any of the cells have GUI specific components that 
 * need initialization, initialize them now. 
 * The cell realize method, if present on a cell,
 * is how that cell can find out that now is the time to 
 * initialize that GUI.
 */

void        
xaccCreateCursor (Table * table, CellBlock *curs)
{ 
  int i,j;

  if (!curs || !table) return;
    
  for (i=0; i<curs->numRows; i++) {
    for (j=0; j<curs->numCols; j++) {
      BasicCell *cell;
      cell = curs->cells[i][j];
      if (cell) {
        void (*gtk_realize) (BasicCell *,  void *gui, int pixel_width);
        gtk_realize = cell->realize;
        if (gtk_realize) {
          /* int pixel_width; */
          /* cl->column[col].width */
          /*pixel_width = XbaeMatrixGetColumnPixelWidth (reg, j);*/
          /*gtk_realize (cell, ((void *) reg), pixel_width);*/
          gtk_realize (cell, ((void *) table), 0);
        }
      }
    }
  }
}

/* ==================================================== */

static inline void
set_cell_color(GtkSheet *reg, Table *table, guint row, guint col,
               const uint32 argb,
               void (*setter)(GtkSheet *s, GtkSheetRange r, GdkColor *c)) {

  /* You might be able to make this much faster by caching colors
     rather than re-allocating them every time, but this is much
     quicker to implement, and it's easier to check for correctness.
     See the motif code for the other approach.  Also, note that
     *both* implementations could be sped up if there was a finer
     grained interface where the engine would just tell the UI to
     change the color on particular items when appropriate, but at the
     cost of added complexity. */

  gboolean success;
  GdkColor color;
  GdkColormap *cmap;
  
  color.red = (argb & 0xff0000) >> 8;
  color.green = argb & 0xff00;
  color.blue = (argb & 0xff) << 8;
  
  cmap = gtk_widget_get_colormap(GTK_WIDGET(reg));
  I(cmap);
  
  success = gdk_color_alloc(cmap, &color);
  I(success);
  
  {
    GtkSheetRange range;
    range.row0 = range.rowi = row;
    range.col0 = range.coli = col;
    setter(reg, range, &color);
    /* HACK: I need to figure out the color allocation semantics...*/
    /* gdk_color_free(&color); */
  }
}




void        
xaccRefreshTableGUI (Table * table) {
  
  if (!table) return;
  if (!(table->table_widget)) return;

  DEBUGCMD ({int i;
  printf (" refresh numphysrows=%d numphyscols=%d =========================\n", 
     table->num_phys_rows,table->num_phys_cols);
     for (i=0; i<table->num_phys_rows; i++) {
     printf ("cell %d\tcolor: 0x%x\tact:%s\tdescr: %s\tpay: %s\n", i, 
     table->bg_colors[i][3], 
     table->entries[i][2],
     table->entries[i][3],
     table->entries[i][5]);
     }});

  {
    /* The 0'th row of the handlers is defined as the header */
    GtkSheet *reg = GTK_SHEET(table->table_widget);    
    CellBlock *curs = table->handlers[0][0];
    int num_header_rows = curs->numRows;
    
    gtk_sheet_freeze(reg);
    
    /* Adjust table to have the right number of rows */
    {
      /* maxrow is actually the maximum index, not the total number of rows */
      glong diffrows =
        (reg->maxrow + 1) - (table->num_phys_rows - num_header_rows);
      gint table_row, row, col;
      
      if(diffrows > 0) {
        gtk_sheet_add_row(reg, diffrows);
      } else if(diffrows < 0) {
        gtk_sheet_delete_rows(reg, 0, diffrows);
      }
      
      /* Set the per-cell contents, colors, etc. */
      for(table_row = num_header_rows, row = 0;
          table_row < table->num_phys_rows;
          table_row++, row++) {
        for(col = 0; col < table->num_phys_cols; col++) {
          
          gtk_sheet_set_cell(reg, row, col, GTK_JUSTIFY_LEFT,
                             table->entries[table_row][col]);


          /* You might be able to make this *much* faster by caching
             colors. */
          set_cell_color(reg, table, row, col,
                         table->bg_colors[table_row][col],
                         gtk_sheet_range_set_background);

          set_cell_color(reg, table, row, col,
                         table->fg_colors[table_row][col],
                         gtk_sheet_range_set_foreground);

          set_cell_color(reg, table, row, col,
                         /* this really should be win->default_style->black */
                         0,
                         gtk_sheet_range_set_border_color);

          /* Turn on all the borders */
          {
            GtkSheetRange r;
            r.row0 = row;
            r.rowi = row + 1;
            r.col0 = col;
            r.coli = col + 1;
            
            gtk_sheet_range_set_border(reg, r,
                                       CELL_TOP_BORDER | CELL_BOTTOM_BORDER |
                                       CELL_LEFT_BORDER | CELL_RIGHT_BORDER,
                                       1, GDK_LINE_SOLID);
          }
        }
      }

#if 0
      /* This is more efficient than the above, but it won't work
         unless gtksheet changes its semantics */
      
      /* Turn on all the borders */
      for(row = 0; row <= reg->maxrow; row++) {
        GtkSheetRange r;
        r.row0 = row;
        r.rowi = row + 1;
        r.col0 = 0;
        r.coli = reg->maxcol;

        L("Setting border for (%d %d) (%d %d)\n",
          r.row0, r.rowi, r.col0, r.coli);
        gtk_sheet_range_set_border(reg, r, CELL_TOP_BORDER, 1, GDK_LINE_SOLID);
      }
      for(col = 0; col <= reg->maxcol; col++) {
        GtkSheetRange r;
        r.row0 = 0;
        r.rowi = reg->maxrow;
        r.col0 = col;
        r.coli = col + 1;
        
        gtk_sheet_range_set_border(reg, r, CELL_LEFT_BORDER, 1, GDK_LINE_SOLID);
      }
#endif
    }
    gtk_sheet_thaw(reg); 
  }
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
