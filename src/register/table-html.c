/*
 * FILE:
 * table-html.c
 *
 * FUNCTION:
 * Implements the infrastructure for the displayed table.
 * This is just a sample hack for printing html.  Its cheesey,
 * for several reasons:  
 *
 * (1) HTML should never be put in the same file as C code.
 *     Some sort of template file should be used.
 *
 * (2) The data printed hre is obtained from the displayed
 *     ledger/register.  Thus, it accurately reflects
 *     what's in the ledger, but is otherwise a cheesy way 
 *     of doing a report.  Real report generators should
 *     get the financial data straight from the engine,
 *     not from the register.
 *
 * But this code is fun, so what the hey.
 *
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
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

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>


#include "cellblock.h"
#include "table-allgui.h"
#include "table-html.h"

/* ==================================================== */

CellBlock *
getcurs (Table *table, int prow) 
{
   int vrow;
   vrow = table->locators[prow][0]->virt_row;
   return (table->handlers[vrow][0]);
}

void        
xaccTableDumpHTML (Table * table, int fd)
{
   FILE * fh;
   int i, j;
   CellBlock *curs, *head, *trans;

   /* fd could be a file descriptor for a file, or for a socket. */
   fh = fdopen (fd, "a");
   if (!fh) {
      int norr = errno;
      printf ("Error: xaccTableDumpHTML(): can't open fd=%d \n", fd);
      printf ("(%d) %s \n", norr, strerror (norr));
      return;
   }

   /* print the header */
   fprintf (fh, 
      "<html>\n"
      "<head><title>GnuCash Regsiter</title></head>\n"
      "<body bgcolor=#FFFFFF>\n"
      "<h1>GnuCash Register</h1>\n"
      "<table>\n");
   
   /* print the table header rows */
   head = getcurs (table, 0);
   i = 0;
   do {
      fprintf (fh, "<tr>\n");
      for (j=0; j<table->num_phys_cols; j++) {
         fprintf (fh, "<th>%s</th>", table->entries[i][j]);
      }
      fprintf (fh, "\n</tr>");

      i++;
      curs = getcurs (table, i);
   } while (head == curs);
   trans = curs;

   /* print the body of the table */
   for (; i<table->num_phys_rows; i++) {
      curs = getcurs (table, i);

      if (trans == curs) {
         fprintf (fh, "<tr bgcolor=#AAAAAA>\n");
      } else {
         fprintf (fh, "<tr>\n");
      }

      for (j=0; j<table->num_phys_cols; j++) {
        if (0x0 == table->entries[i][j][0]) {
           fprintf (fh, "<td>&nbsp;</td>");
        } else {
           fprintf (fh, "<td>%s</td>", table->entries[i][j]);
        }
      }
      fprintf (fh, "\n</tr>");
   }

   fprintf (fh, "</table></body></html>");
   fflush (fh);
}

/* ==================================================== */

void        
xaccTablePrintHTML (Table * table, char *filename)
{
   int fd;

   fd = open (filename, O_CREAT | O_APPEND | O_WRONLY);
   if (0 > fd) {
      int norr = errno;
      printf ("Error: xaccTablePrintHTML(): can't open file %s\n", filename);
      printf ("(%d) %s \n", norr, strerror (norr));
      return;
   }
   xaccTableDumpHTML (table, fd);

   close (fd);
}

/* ================== end of file ======================= */
