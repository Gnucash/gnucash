/********************************************************************\
 * window-main-menu.h -- just what is says                          *
 * Copyright (C) 1998 Jeremy Collins                                *
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

#ifndef __WINDOW_MAIN_MENU_H__
#define __WINDOW_MAIN_MENU_H__

#include <gtk/gtk.h>

#include "config.h"

/** STRUCTS *********************************************************/

/** PROTOTYPES ******************************************************/
void gnucash_shutdown (GtkWidget *widget, gpointer *data); 
void file_cmd_open (GtkWidget *widget, gpointer data);
void file_cmd_import (GtkWidget *widget, gpointer data);
void file_cmd_quit (GtkWidget *widget, gpointer data);
void file_cmd_save (GtkWidget *widget, gpointer data);
/* void prepare_app ( void ); */

/** GLOBALS *********************************************************/
//extern char  *helpPath;
extern GtkWidget   *app;

#endif

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c-modet
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/ 
