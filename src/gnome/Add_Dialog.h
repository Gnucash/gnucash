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

#ifndef __ADD_DIALOG_H__
#define __ADD_DIALOG_H__

#include "Group.h"
#include "Account.h"
#include "main.h"
#include "MainWindow.h"

struct _add_account_dialog 
{
  GnomeDialog 	*dialog;
  GtkWidget 	*main_vbox;
  GtkWidget 	*box2;
  GtkWidget	*box3;
  GtkWidget	*box4;
  GtkWidget 	*frame;
  
  GSList 	*group;

  GtkWidget 	*label;
  GtkWidget 	*textbox_name;
  GtkWidget	*textbox_description;

  GtkWidget	*separator;

  Account	*parent_account;
  gint		*type;

};

typedef struct _add_account_dialog add_account_dialog;

void 			create_add_account_dialog ( AccountGroup * );
add_account_dialog 	*add_account_dialog_init ( void );
void	 		add_account_dialog_destroy ( GtkWidget *, GnomeDialog * );

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
