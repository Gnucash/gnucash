/*-*-gnucash-c-*-****************************************************\
 * MenuCommands.c -- just what it says                              *
 * Copyright (C) 1998 Jeremy Collins                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
\********************************************************************/

#include "MenuCommands.h"
// #incldue "FileDialog.h"

/* hack alert --  the right way to imoplement the missing function
 * below is to move the file src/motif/FileDialog.c to some
 * GUI-neutral directory, and dual-compile it for motif and for
 * gtk.  Then simple invoke gncFileOpen, etc. and Viola, instant
 * function! We are done!
 */

void
file_cmd_open (GtkWidget *widget, gpointer data)
{
  // gncFileOpen();
}

void
file_cmd_import (GtkWidget *widget, gpointer data)
{
   // gncFileQIFImport();
}

void
file_cmd_save(GtkWidget *widget, gpointer data)
{
   // gncFileSave();
}

void file_cmd_quit (GtkWidget *widget, gpointer data)
{
  // gncFileQuit();
  //gnucash_shutdown(NULL, NULL);
  gtk_main_quit();
}
