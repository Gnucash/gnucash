/********************************************************************
 * top-level.h -- public gnucash UI functions                       *
 * Copyright (C) 2001 Bill Gribble <grib@gnumatic.com>              *
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
 ********************************************************************/

#ifndef TOP_LEVEL_H
#define TOP_LEVEL_H

#include <glib.h>
#include <guile/gh.h>

#include "window-main.h"

gboolean      gnucash_ui_is_running (void);
gboolean      gnucash_ui_is_terminating (void);
SCM           gnc_gui_init (SCM command_line);
void          gnc_gui_shutdown (void);
void          gnc_gui_destroy (void);
int           gnc_ui_start_event_loop (void);
gboolean      gnc_reverse_balance_type (GNCAccountType type);
gboolean      gnc_reverse_balance (Account *account);

#endif
