/********************************************************************\
 * top-level.h -- main for gnucash (X-Accountant)                   *
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

#ifndef __TOP_LEVEL_H__
#define __TOP_LEVEL_H__

#include "config.h"
#include "Group.h"
#include "Session.h"
#include "gnc-common.h"

/** HELP STUFF: *****************************************************/
#define HH_ABOUT     "xacc-about.html"
#define HH_ACC       "xacc-accwin.html"
#define HH_REGWIN    "xacc-regwin.html"
#define HH_RECNWIN   "xacc-recnwin.html"
#define HH_ADJBWIN   "xacc-adjbwin.html"
#define HH_MAIN      "xacc-main.html"
#define HH_GPL       "xacc-gpl.html"
#define HH_GLOBPREFS "xacc-globalprefs.html"
#define HH_ACCEDIT   "xacc-accountedit.html"
#define HH_QIFIMPORT "xacc-qif-import.html"
#define HH_PRINTCHECK "xacc-print-check.html"
#define HH_FIND_TRANSACTIONS "xacc-find-transactions.html"
#define HH_PRINT "xacc-print.html"

/** STRUCTS *********************************************************/

/** PROTOTYPES ******************************************************/

/** GLOBALS *********************************************************/
extern Session *current_session;

#if defined(MOTIF)
  extern gncUIWidget gnc_get_ui_data();
#elif defined(GNOME)
  extern gncUIWidget gnc_get_ui_data();
#endif

extern void gnc_ui_shutdown();

#endif
