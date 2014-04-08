/* MenuBar.h -- GTK menu functions for xacc (X-Accountant) Copyright
   (C) 1998 Rob Browning <rlb@cs.utexas.edu>
                                                                   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.
                                                                   
   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
                                                                   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
                                                                   
   Author: Rob Browning                                           
   Internet: rlb@cs.utexas.edu                                      
*/

#ifndef __MENUBAR_H__
#define __MENUBAR_H__

typedef GtkMenuFactory MenuBarGroup;
typedef GtkMenuFactory MenuBar;

/* Order must be: create group, create menu bar, add items. */

MenuBarGroup *menuBarGroupCreate();
void          menuBarGroupAddItems(MenuBarGroup *group,
                                   GtkMenuEntry items[], guint nitems);

MenuBar      *menuBarCreate(MenuBarGroup *group, const gchar menuBarName[]);
GtkWidget    *menuBarGroupFindItem(MenuBarGroup *group, const gchar path[]); 

GtkWidget    *menuBarGetWidget(MenuBar *mb);

#endif
