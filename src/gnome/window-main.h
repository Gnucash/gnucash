/*******************************************************************\
 * window-main.h -- public GNOME main window functions              *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
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
\********************************************************************/

#ifndef __WINDOW_MAIN_H__
#define __WINDOW_MAIN_H__

#include "mainwindow-account-tree.h"

/** PROTOTYPES ******************************************************/

void mainWindow(void);

GNCMainWinAccountTree * gnc_get_current_account_tree(void);

void gnc_ui_mainWindow_save_size(void);
void gnc_default_ui_start(void);
int gnucash_ui_init(void);

#endif
