/********************************************************************\
 * gnc-helpers.h -- gnucash app-util helper functions               *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_HELPERS
#define GNC_HELPERS

#include <libguile.h>

#include "gnc-ui-util.h"

SCM  gnc_printinfo2scm(GNCPrintAmountInfo info);
GNCPrintAmountInfo gnc_scm2printinfo(SCM info_scm);
int  gnc_printinfo_p(SCM info_scm);

/** Given a pointer to a gnc-commodity data structure, build a Scheme
 *  list containing the data needed by the code in price-quotes.scm.
 *  This prevents flipping back and forth from Scheme to C while
 *  extracting values from a pointer.
 *
 * @param com A pointer to the commodity to convert.
 *
 * @return A pointer to a Scheme list, or SCM_EOL on error.
 */
SCM  gnc_quoteinfo2scm(gnc_commodity *com);

SCM gnc_parse_amount_helper (const char * string, gboolean monetary);

#endif
