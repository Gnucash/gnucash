/*-*-gnucash-c-*-****************************************************\
 * MenuCommands.c -- just what it says                              *
 * Copyright (C) 1997 Robin D. Clark                                *
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
#include "MainWindow.h"

void
file_cmd_open (GtkWidget *widget, gpointer data)
{
   // gtk_widget_show ( filebox );
}

void
file_cmd_import (GtkWidget *widget, gpointer data)
{
   // gtk_widget_show (import_filebox);
}

void
file_cmd_save(GtkWidget *widget, gpointer data)
{
  /* hack alert -- Somehow make sure all in-progress edits get committed! */
//  if (NULL == datafile) {
//    fprintf(stderr, "Can't save file.  No open file\n");
    return;
//  }
//  xaccWriteAccountGroup(datafile, topgroup);
//  xaccAccountGroupMarkSaved(topgroup);
}

void file_cmd_quit (GtkWidget *widget, gpointer data)
{
  //gnucash_shutdown(NULL, NULL);
  gtk_main_quit();
}
