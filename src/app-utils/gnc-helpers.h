/********************************************************************\
 * gnc-helpers.h -- gnucash g-wrap helper functions                 *
 * Copyright (C) 2000 Linas Vepstas                                 *
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

#ifndef GNC_HELPERS
#define GNC_HELPERS

#include <guile/gh.h>

#include "gnc-ui-util.h"

SCM  gnc_printinfo2scm(GNCPrintAmountInfo info);
GNCPrintAmountInfo gnc_scm2printinfo(SCM info_scm);
int  gnc_printinfo_p(SCM info_scm);

const char * gnc_get_account_separator_string (void);

SCM gnc_parse_amount_helper (const char * string, gboolean monetary);

#endif
