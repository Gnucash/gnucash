/********************************************************************\
 * MainWindow.h -- the main window, and associated helper functions * 
 *                 and callback functions for xacc (X-Accountant    *
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

#ifndef __MAINWINDOW_H__
#define __MAINWINDOW_H__

#include <gtk/gtk.h>
#include "Group.h"
#include "Account.h"
#include "config.h"


/** PROTOTYPES ******************************************************/
void refreshMainWindow( void );
void main_window_init(AccountGroup *);
void xaccMainWindowAddAccount ( GtkWidget * );


/** GLOBALS *********************************************************/
enum {
  FMB_NEW,
  FMB_OPEN,
  FMB_IMPORT,
  FMB_SAVE,
  FMB_SAVEAS,
  FMB_QUIT,
};
enum {
  AMB_NEW,
  AMB_OPEN,
  AMB_LEDGER,
  AMB_EDIT,
  AMB_DEL,
  AMB_TRNS,
  AMB_RPRT,
  AMB_SHOW,
  AMB_CAT,
};
enum {
  HMB_ABOUT,
  HMB_ACC,
  HMB_REGWIN,
  HMB_RECNWIN,
  HMB_ADJBWIN,
  HMB_MAIN,
  HMB_LIC,
};

/* The following defines need to be changed later to reflect
 * changes in the buttons.
 */
#define open    0
#define close   1
#define button3 2
#define button4 3
#define exit    4

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
