/********************************************************************\
 * table-html.c -- print table as html                              *
 *                                                                  *
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

/*
 * FILE:
 * table-html.c
 *
 * FUNCTION:
 * Implements the infrastructure for the displayed table.
 * This is just a sample hack for printing html to a file,
 * or by acting as a web server.  Its cheesey, for several
 * reasons:  
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
 * (3) The so-called "webserver" has less intelligence 
 *     than a mosquito.
 *
 * But this code is fun, so what the hey.
 *
 *
 * HISTORY:
 * Copyright (c) 1998 Linas Vepstas
 */

#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>


#include "cellblock.h"
#include "table-allgui.h"
#include "table-html.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER; 

/* ==================================================== */

CellBlock *
getcurs (Table *table, int prow) 
{
   int vrow;
   vrow = table->locators[prow][0]->virt_row;
   return (table->handlers[vrow][0]);
}

int        
xaccTableDumpHTML (Table * table, int fd)
{
   FILE * fh;
   int i, j;
   CellBlock *curs, *head, *trans;
   int cnt = 0;

   if ((!table) || (0 > fd)) return 0;

   /* fd could be a file descriptor for a file, or for a socket. */
   fh = fdopen (fd, "a");
   if (!fh) {
      int norr = errno;
      PERR ("can't open fd=%d \n"
            "\t(%d) %s \n", fd, norr, strerror (norr));
      return 0;
   }

   /* print the header */
   cnt += fprintf (fh, 
      "<html>\n"
      "<head><title>GnuCash Regsiter</title></head>\n"
      "<body bgcolor=#FFFFFF>\n"
      "<h1>GnuCash Register</h1>\n"
      "<table>\n");
   
   /* print the table header rows */
   head = getcurs (table, 0);
   i = 0;
   do {
      cnt += fprintf (fh, "<tr>\n");
      for (j=0; j<table->num_phys_cols; j++) {
         cnt += fprintf (fh, "<th>%s</th>", table->entries[i][j]);
      }
      cnt += fprintf (fh, "\n</tr>");

      i++;
      curs = getcurs (table, i);
   } while (head == curs);
   trans = curs;

   /* print the body of the table */
   for (; i<table->num_phys_rows; i++) {
      curs = getcurs (table, i);

      if (trans == curs) {
         cnt += fprintf (fh, "<tr bgcolor=#AAAAAA>\n");
      } else {
         cnt += fprintf (fh, "<tr>\n");
      }

      for (j=0; j<table->num_phys_cols; j++) {
        if (0x0 == table->entries[i][j][0]) {
           cnt += fprintf (fh, "<td>&nbsp;</td>");
        } else {
           cnt += fprintf (fh, "<td>%s</td>", table->entries[i][j]);
        }
      }
      cnt += fprintf (fh, "\n</tr>");
   }

   cnt += fprintf (fh, "</table></body></html>");
   fflush (fh);

   return cnt;
}

/* ==================================================== */

int
xaccTablePrintHTML (Table * table, char *filename)
{
   int fd;
   int cnt;

   if ((!table) || (!filename)) return 0;

   fd = open (filename, O_CREAT | O_APPEND | O_WRONLY, S_IRUSR | S_IWUSR);
   if (0 > fd) {
      int norr = errno;
      PERR ("can't open file %s\n"
            "\t(%d) %s\n", filename, norr, strerror (norr));
      return 0;
   }
   cnt = xaccTableDumpHTML (table, fd);
   close (fd);
   return (cnt);
}

/* ==================================================== */
/* implement a cheesy web server */
/* maybe not the worlds smallest web server, but close */

#define CHKERR(val, msg) {						\
   if (0 > val) {							\
      int norr = errno;							\
      PERR ( msg "\n"							\
            "(%d) %s \n", norr, strerror (norr));			\
      if (pid) return;							\
      exit (0);								\
   }									\
}

void        
xaccTableWebServeHTML (Table * table, unsigned short port)
{
   int listen_fd, accept_fd;
   pid_t pid;
   int rc;
   struct sockaddr_in myaddr;
   struct sockaddr clientsock;
   int clientaddrsize = sizeof (struct sockaddr);
   fd_set readfds;
   struct timeval timeout;
   char buff[255];
   int cnt;

   if (!table) return;

   /* don't slow down the parent, fork into background */
   pid = fork(); 
   if (0 < pid) return;  /* parent */
   CHKERR (pid, "cant fork");

   /* create a socket */
   listen_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
   CHKERR (listen_fd, "cant listen");

   bzero ((void*) &myaddr, sizeof (struct sockaddr_in));
   myaddr.sin_family = AF_INET;
   myaddr.sin_addr.s_addr = htonl (INADDR_ANY);
   myaddr.sin_port = htons (port);   /* WWW server is port 80 but that is usperuser only */
   rc = bind (listen_fd, (struct sockaddr *) &myaddr, sizeof (struct sockaddr));
   CHKERR (rc, "cant bind");
   
   rc = listen (listen_fd, 10);
   CHKERR (rc, "cant listen");

   /* if no one connects to us after 8 minutes, just exit */
   FD_ZERO (&readfds);
   FD_SET (listen_fd, &readfds);
   timeout.tv_sec = 500;
   timeout.tv_usec = 0;
   rc = select (160, &readfds, NULL, NULL, &timeout);
   CHKERR (rc, "cant select");
   if (0 == rc) exit (0);
 
   accept_fd = accept (listen_fd, &clientsock, &clientaddrsize);
   CHKERR (rc, "cant accept");
 
   /* count the number of chars, for content-length */
   cnt = xaccTablePrintHTML (table, "/dev/null");
   snprintf (buff, 255,
      "HTTP/1.0 200 OK\n"
      "Connection: close\n"
      "Content-Length: %d\n"
      "Content-Type: text/html\n\n", cnt);
   write (accept_fd,  buff, sizeof (buff));
   xaccTableDumpHTML (table, accept_fd);

   /* linger for some @#$%^ reason, otherwise browser complains */
   sleep (30);
   shutdown (accept_fd, 1);
   close (accept_fd);
   close (listen_fd);
   exit (0);
}

/* ================== end of file ======================= */
