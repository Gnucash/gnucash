/********************************************************************
 * window-acct-tree.h -- public account-tree-window functions       *
 * Copyright (C) 1998,1999 Linas Vepstas                            *
 * Copyright (C) 2001 Bill Gribble <grib@gnumatic.com>              *
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
 ********************************************************************/

#ifndef WINDOW_ACCT_TREE_H
#define WINDOW_ACCT_TREE_H

#include "mainwindow-account-tree.h"
#include "window-main.h"

typedef struct GNCAcctTreeWin_p GNCAcctTreeWin;

GNCAcctTreeWin * gnc_acct_tree_window_new(const gchar * url);
void             gnc_acct_tree_window_destroy(GNCAcctTreeWin * win);
GtkWidget      * gnc_acct_tree_window_get_widget(GNCAcctTreeWin * win);
void             gnc_acct_tree_window_create_menu(GNCAcctTreeWin * win,
                                                  GNCMainChildInfo * child);
void             gnc_acct_tree_window_create_toolbar(GNCAcctTreeWin * win,
                                                     GNCMainChildInfo * child);
Account        * gnc_acct_tree_window_get_current_account(GNCAcctTreeWin * w);
GnomeMDIChild  * gnc_acct_tree_window_create_child(const gchar * url);
void             gnc_main_window_open_accounts(gint toplevel); 
SCM              gnc_acct_tree_window_get_options(GNCAcctTreeWin * win);
int              gnc_acct_tree_window_get_id(GNCAcctTreeWin * win);
#endif
