/********************************************************************\
 * xtutil.h -- utility functions that are used everywhere else for  *
 *           xacc (X-Accountant)                                    *
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

#ifndef __XACC_XT_UTIL_H__
#define __XACC_XT_UTIL_H__

#include <gnome.h>
#include <stdlib.h>

#include "config.h"

/** PROTOTYPES ******************************************************/

#if 0

void    dateCB( Widget mw, XtPointer cd, XtPointer cb );
void    amountCB( Widget mw, XtPointer cd, XtPointer cb );
void    noeditCB( Widget mw, XtPointer cd, XtPointer cb );
void    destroyShellCB( Widget w, XtPointer cd, XtPointer cb );
void    setBusyCursor( GtkWidget *w );
void    unsetBusyCursor( GtkWidget *w );
void    errorBox( GtkWidget *parent, const char *message );

#endif

gboolean verifyBox( GtkWidget *parent, const char *text );

#endif /* __XACC_XT_UTIL_H__ */
