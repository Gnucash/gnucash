/********************************************************************\
 * ui-callbacks.h                                                   *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1999 Rob Browning <rlb@cs.utexas.edu>              *
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

#ifndef __UI_CALLBACKS_H__
#define __UI_CALLBACKS_H__

#include <gnc-common.h>

void refreshMainWindow( void );


/* Only implemented in GNOME version right now. */
int queryBox(const char *text,
             int default_answer,
             gncBoolean yes_allowed,
             gncBoolean ok_allowed,
             gncBoolean no_allowed,
             gncBoolean cancel_allowed);

/* deprecated... replaced by queryBox in GNOME version */
gncBoolean  verifyBox(const char *text);

void        errorBox( const char *message );
void        setBusyCursor( gncUIWidget w );
void        unsetBusyCursor( gncUIWidget w );

#endif
