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

void 
xaccTableResizeStringArr (Table * table, int num_phys_rows, int num_phys_cols)
{
   int old_phys_rows, old_phys_cols;
   int i,j;

   /* save old table size */
   old_phys_rows = table->num_phys_rows;
   old_phys_cols = table->num_phys_cols;

   table->num_phys_rows = num_phys_rows;
   table->num_phys_cols = num_phys_cols;

   /* realloc to get the new table size.  Note that the
    * new table may be wider or slimmer, taller or shorter. */
   if (old_phys_rows >= num_phys_rows) {
      if (old_phys_cols >= num_phys_cols) {

         /* if we are here, new table has fewer cols 
          * simply truncate columns */
         for (i=0; i<num_phys_rows; i++) {
            for (j=num_phys_cols; j<old_phys_cols; j++) {
               free (table->entries[i][j]);
               table->entries[i][j] = NULL;
            }
         }
      } else {

         /* if we are here, the new table has more
          * columns. Realloc the columns.  */
         for (i=0; i<num_phys_rows; i++) {
            char **old_row;

            old_row = table->entries[i];
            table->entries[i] = (char **) malloc (num_phys_cols * sizeof (char *));
            for (j=0; j<old_phys_cols; j++) {
               table->entries[i][j] = old_row[j];
            }
            for (j=old_phys_cols; j<num_phys_cols; j++) {
               table->entries[i][j] = strdup ("");
            }
            free (old_row);
         }
      }

      /* new table has fewer rows.  Simply truncate the rows */
      for (i=num_phys_rows; i<old_phys_rows; i++) {
         for (j=0; j<old_phys_cols; j++) {
            free (table->entries[i][j]);
         }
         free (table->entries[i]);
         table->entries[i] = NULL;
      }

   } else {
      char ***old_entries;

      if (old_phys_cols >= num_phys_cols) {

         /* new table has fewer columns. 
          * Simply truncate the columns */
         for (i=0; i<old_phys_rows; i++) {
            for (j=num_phys_cols; j<old_phys_cols; j++) {
               free (table->entries[i][j]);
               table->entries[i][j] = NULL;
            }
         }
      } else {

         /* if we are here, the new table has more
          * columns. Realloc the columns.  */
         for (i=0; i<old_phys_rows; i++) {
            char **old_row;

            old_row = table->entries[i];
            table->entries[i] = (char **) malloc (num_phys_cols * sizeof (char *));
            for (j=0; j<old_phys_cols; j++) {
               table->entries[i][j] = old_row[j];
            }
            for (j=old_phys_cols; j<num_phys_cols; j++) {
               table->entries[i][j] = strdup ("");
            }
            free (old_row);
         }
      }

      /* now, add all new rows */
      old_entries = table->entries;
      table->entries = (char ***) malloc (num_phys_rows * sizeof (char **));
      for (i=0; i<old_phys_rows; i++) {
         table->entries[i] = old_entries[i];
      }
      if (old_entries) free (old_entries);

      for (i=old_phys_rows; i<num_phys_rows; i++) {
         table->entries[i] = (char **) malloc (num_phys_cols * sizeof (char *));
         for (j=0; j<num_phys_cols; j++) {
            table->entries[i][j] = strdup ("");
         }
      }
   }
}

/* ==================================================== */

void 
xaccTableResizeUserData (Table * table, int new_virt_rows, int new_virt_cols)
{
   int old_virt_rows, old_virt_cols;
   int i,j;

   /* save old table size */
   old_virt_rows = table->num_virt_rows;
   old_virt_cols = table->num_virt_cols;

   table->num_virt_rows = new_virt_rows;
   table->num_virt_cols = new_virt_cols;

   /* realloc to get the new table size.  Note that the
    * new table may be wider or slimmer, taller or shorter. */
   if (old_virt_rows >= new_virt_rows) {
      if (old_virt_cols >= new_virt_cols) {

         /* if we are here, new table has fewer cols 
          * simply truncate columns */
         for (i=0; i<new_virt_rows; i++) {
            for (j=new_virt_cols; j<old_virt_cols; j++) {
               table->user_data[i][j] = NULL;
            }
         }
      } else {

         /* if we are here, the new table has more
          * columns. Realloc the columns.  */
         for (i=0; i<new_virt_rows; i++) {
            void **old_row;

            old_row = table->user_data[i];
            table->user_data[i] = (void **) malloc (new_virt_cols * sizeof (void *));
            for (j=0; j<old_virt_cols; j++) {
               table->user_data[i][j] = old_row[j];
            }
            for (j=old_virt_cols; j<new_virt_cols; j++) {
               table->user_data[i][j] = NULL;
            }
            free (old_row);
         }
      }

      /* new table has fewer rows.  Simply truncate the rows */
      for (i=new_virt_rows; i<old_virt_rows; i++) {
         free (table->user_data[i]);
         table->user_data[i] = NULL;
      }

   } else {
      void ***old_user_data;

      if (old_virt_cols >= new_virt_cols) {

         /* new table has fewer columns. 
          * Simply truncate the columns */
         for (i=0; i<old_virt_rows; i++) {
            for (j=new_virt_cols; j<old_virt_cols; j++) {
               table->user_data[i][j] = NULL;
            }
         }
      } else {

         /* if we are here, the new table has more
          * columns. Realloc the columns.  */
         for (i=0; i<old_virt_rows; i++) {
            void **old_row;

            old_row = table->user_data[i];
            table->user_data[i] = (void **) malloc (new_virt_cols * sizeof (void *));
            for (j=0; j<old_virt_cols; j++) {
               table->user_data[i][j] = old_row[j];
            }
            for (j=old_virt_cols; j<new_virt_cols; j++) {
               table->user_data[i][j] = NULL;
            }
            free (old_row);
         }
      }

      /* now, add all new rows */
      old_user_data = table->user_data;
      table->user_data = (void ***) malloc (new_virt_rows * sizeof (void **));
      for (i=0; i<old_virt_rows; i++) {
         table->user_data[i] = old_user_data[i];
      }
      if (old_user_data) free (old_user_data);

      for (i=old_virt_rows; i<new_virt_rows; i++) {
         table->user_data[i] = (void **) malloc (new_virt_cols * sizeof (void *));
         for (j=0; j<new_virt_cols; j++) {
            table->user_data[i][j] = NULL;
         }
      }
   }
}

/* ================== end of file ======================= */
