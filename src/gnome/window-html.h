/********************************************************************\
 * window-html -- an html window for gnucash.                       *
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

#ifndef __WINDOW_HTML_H__
#define __WINDOW_HTML_H__

#include <gnome.h>
#include <gtk-xmhtml/gtk-xmhtml.h>


typedef struct _HTMLWindow HTMLWindow;
typedef struct _HTMLData HTMLData;
typedef void * HTMLUserData;

typedef void (*HTMLDestroyUserDataFunc)(HTMLUserData);

typedef HTMLData* (*HTMLAnchorCB)(XmHTMLAnchorCallbackStruct *acbs,
                                  HTMLUserData user_data);

typedef void (*HTMLJumpCB)(HTMLUserData user_data,
                           char **text, char **label);


HTMLUserData gnc_html_window_user_data(HTMLWindow *hw);

HTMLWindow * gnc_html_window_new(HTMLAnchorCB anchor_cb, HTMLJumpCB jump_cb);


void         gnc_html_window_destroy(HTMLWindow *hw);

HTMLData * gnc_html_data_new(const char *title, HTMLUserData user_data,
                             HTMLDestroyUserDataFunc destroy,
                             GnomeUIInfo *user_buttons,
                             int num_user_buttons);

void htmlWindow(GtkWidget   *parent,
                HTMLWindow **hwp,
                HTMLData    *data);

void gnc_html_load(HTMLWindow *hw);


#endif
