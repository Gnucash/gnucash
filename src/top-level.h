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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef __TOP_LEVEL_H__
#define __TOP_LEVEL_H__

#include "config.h"
#include "Group.h"
#include "Session.h"
#include "gnc-common.h"
#include "gnc-ui-common.h"


/** HELP STUFF: *****************************************************/
#define HH_ABOUT             "xacc-about.html"
#define HH_ACC               "xacc-newacctwin.html"
#define HH_REGWIN            "xacc-regwin.html"
#define HH_RECNWIN           "xacc-recnwin.html"
#define HH_ADJBWIN           "xacc-adjbwin.html"
#define HH_MAIN              "index.html"
#define HH_GPL               "xacc-gpl.html"
#define HH_GLOBPREFS         "xacc-preferences.html"
#define HH_ACCEDIT           "xacc-accountedit.html"
#define HH_QIFIMPORT         "xacc-qif-import.html"
#define HH_PRINTCHECK        "xacc-print-check.html"
#define HH_FIND_TRANSACTIONS "xacc-locatingtxns.html"
#define HH_PRINT             "xacc-print.html"


/** PROTOTYPES ******************************************************/

gncUIWidget gnc_get_ui_data();

void gnc_ui_shutdown();

gboolean gnc_reverse_balance(Account *account);
gboolean gnc_reverse_balance_type(GNCAccountType type);

#endif
