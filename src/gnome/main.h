/********************************************************************\
 * main.h -- main for xacc (X-Accountant)                           *
 * Copyright (C) 1997 Robin D. Clark                                *
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
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#ifndef __MAIN_H__
#define __MAIN_H__

#include <gtk/gtk.h>

#include "config.h"

#include "main.h"
#include "FileIO.h"
#include "Group.h"
#include "util.h"
#include "MainWindow.h" 


/** HELP STUFF: *****************************************************/
#define HELP_VAR     "XACC_HELP"
#define HELP_ROOT    "./Docs/"
#define HH_ABOUT     "xacc-about.html"
#define HH_ACC       "xacc-accwin.html"
#define HH_REGWIN    "xacc-regwin.html"
#define HH_RECNWIN   "xacc-recnwin.html"
#define HH_ADJBWIN   "xacc-adjbwin.html"
#define HH_MAIN      "xacc-main.html"
#define HH_GPL       "xacc-gpl.html"

/** STRUCTS *********************************************************/

/** PROTOTYPES ******************************************************/
void destroy (GtkWidget *widget, gpointer *data);
void file_cmd_open (GtkWidget *widget, gpointer data);

/** GLOBALS *********************************************************/
extern char  *helpPath;

#endif

/*
  Local Variables:
  tab-width: 2
  indent-tabs-mode: nil
  mode: c-mode
  c-indentation-style: gnu
  eval: (c-set-offset 'block-open '-)
  End:
*/
