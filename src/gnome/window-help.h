/********************************************************************\
 * window-help.h -- a help window for hypertext help.               *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef __GNC_HELP_WINDOW_H__
#define __GNC_HELP_WINDOW_H__

#include <gnome.h>

#include "gnc-html.h"

typedef struct _gnc_help_window gnc_help_window;

/** PROTOTYPES ******************************************************/

void helpWindow(GtkWidget *parent, const char *title, const char * htmlfile);

gnc_help_window  * gnc_help_window_new(void);
void             gnc_help_window_destroy(gnc_help_window * help);
void             gnc_help_window_show_help(gnc_help_window * hw, 
                                           const gchar * loc,
                                           const gchar * label);

#endif
