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

/** HELP STUFF: *****************************************************/
#define HELP_VAR     "XACC_HELP"
#define HH_ABOUT     "xacc-about.html"
#define HH_ACC       "xacc-accwin.html"
#define HH_REGWIN    "xacc-regwin.html"
#define HH_RECNWIN   "xacc-recnwin.html"
#define HH_ADJBWIN   "xacc-adjbwin.html"
#define HH_MAIN      "xacc-main.html"
#define HH_GPL       "xacc-gpl.html"


/** STRUCTURES ******************************************************/
struct _main_window 
{
  GtkWidget 	*maintree;
  GtkWidget     *root_item;
};

typedef struct _main_window main_window;

/** PROTOTYPES ******************************************************/
void gnc_ui_refreshMainWindow( void );
void gnc_ui_mainWindow(AccountGroup *);
void xaccMainWindowAddAccount ( GtkWidget * );
void gnc_ui_refresh_tree ( void );
void gnc_ui_acct_tree_fill ( GtkWidget *, AccountGroup *, int );

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
