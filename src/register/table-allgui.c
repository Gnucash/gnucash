/*
 * FILE:
 * table-allgui.c
 *
 * FUNCTION:
 * Implements the gui-independent parts of the table infrastructure.
 *
 * HISTORY:
 * Copyright (c) 1988 Linas Vepstas
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

#include <stdlib.h>

#include "cellblock.h"
#include "table-allgui.h"

/* ==================================================== */
/* in C, we don't have templates. So cook up a $define that acts like a
 * template.  This one will resize a 2D array.
 */

#define RESIZE_ARR(table_rows,table_cols,new_rows,new_cols,arr,type,null_val) \
{									\
   int old_rows, old_cols;						\
   int i,j;								\
									\
   /* save old table size */						\
   old_rows = table_rows;						\
   old_cols = table_cols;						\
									\
   table_rows = new_rows;						\
   table_cols = new_cols;						\
									\
   /* realloc to get the new table size.  Note that the */		\
   /* new table may be wider or slimmer, taller or shorter. */		\
   if (old_rows >= new_rows) {						\
      if (old_cols >= new_cols) {					\
									\
         /* if we are here, new table has fewer cols */			\
         /* simply truncate columns */					\
         for (i=0; i<new_rows; i++) {					\
            for (j=new_cols; j<old_cols; j++) {				\
               free (arr[i][j]);					\
               arr[i][j] = NULL;					\
            }								\
         }								\
      } else {								\
									\
         /* if we are here, the new table has more */			\
         /* columns. Realloc the columns.  */				\
         for (i=0; i<new_rows; i++) {					\
            type **old_row;						\
									\
            old_row = arr[i];						\
            arr[i] = (type **) malloc (new_cols * sizeof (type *));	\
            for (j=0; j<old_cols; j++) {				\
               arr[i][j] = old_row[j];					\
            }								\
            for (j=old_cols; j<new_cols; j++) {				\
               arr[i][j] = null_val;					\
            }								\
            free (old_row);						\
         }								\
      }									\
									\
      /* new table has fewer rows.  Simply truncate the rows */		\
      for (i=new_rows; i<old_rows; i++) {				\
         for (j=0; j<old_cols; j++) {					\
            free (arr[i][j]);						\
         }								\
         free (arr[i]);							\
         arr[i] = NULL;							\
      }									\
									\
   } else {								\
      type ***old_entries;						\
									\
      if (old_cols >= new_cols) {					\
									\
         /* new table has fewer columns. */ 				\
         /* Simply truncate the columns  */				\
         for (i=0; i<old_rows; i++) {					\
            for (j=new_cols; j<old_cols; j++) {				\
               free (arr[i][j]);					\
               arr[i][j] = NULL;					\
            }								\
         }								\
      } else {								\
									\
         /* if we are here, the new table has more */			\
         /* columns. Realloc the columns.  */				\
         for (i=0; i<old_rows; i++) {					\
            type **old_row;						\
									\
            old_row = arr[i];						\
            arr[i] = (type **) malloc (new_cols * sizeof (type *));	\
            for (j=0; j<old_cols; j++) {				\
               arr[i][j] = old_row[j];					\
            }								\
            for (j=old_cols; j<new_cols; j++) {				\
               arr[i][j] = null_val;					\
            }								\
            free (old_row);						\
         }								\
      }									\
									\
      /* now, add all new rows */					\
      old_entries = arr;						\
      arr = (type ***) malloc (new_rows * sizeof (type **));		\
      for (i=0; i<old_rows; i++) {					\
         arr[i] = old_entries[i];					\
      }									\
      if (old_entries) free (old_entries);				\
									\
      for (i=old_rows; i<new_rows; i++) {				\
         arr[i] = (type **) malloc (new_cols * sizeof (type *));	\
         for (j=0; j<new_cols; j++) {					\
            arr[i][j] = null_val;					\
         }								\
      }									\
   }									\
}

/* ==================================================== */

void 
xaccTableResizeStringArr (Table * table, int new_phys_rows, int new_phys_cols)
{
   RESIZE_ARR ((table->num_phys_rows),
               (table->num_phys_cols),
               new_phys_rows,
               new_phys_cols,
               (table->entries),
               char,
               (strdup ("")));
}

/* ==================================================== */

void 
xaccTableResizeUserData (Table * table, int new_virt_rows, int new_virt_cols)
{
   RESIZE_ARR ((table->num_virt_rows),
               (table->num_virt_cols),
               new_virt_rows,
               new_virt_cols,
               (table->user_data),
               void,
               (NULL));
}

/* ==================================================== */

void
xaccTableCount (Table *table, CellBlock *curse)
{
   if (!table) return;
   if (!curse) return;
   
   /* increment rows */
   table->cnt_phys_rows += curse->numRows;
   table->cnt_virt_rows ++;

   /* copy columns */
   table->cnt_phys_cols = curse->numCols;
}

/* ==================================================== */

void
xaccAddCursor (Table *table, CellBlock *curs)
{
}

/* ================== end of file ======================= */
