/********************************************************************\
 * gnc-helpers.c -- gnucash app-util helper functions               *
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

#include <config.h>

#include <libguile.h>
#include <string.h>
#include "swig-runtime.h"
#include "guile-mappings.h"

#include "gnc-engine.h"
#include "gnc-engine-guile.h"
#include "gnc-helpers.h"
#include "gnc-ui-util.h"


/* Type converters for GNCPrintAmountInfo */
SCM
gnc_printinfo2scm(GNCPrintAmountInfo info)
{
    SCM info_scm = SCM_EOL;

    info_scm = scm_cons (SCM_BOOL (info.round), info_scm);
    info_scm = scm_cons (SCM_BOOL (info.force_fit), info_scm);
    info_scm = scm_cons (SCM_BOOL (info.monetary), info_scm);
    info_scm = scm_cons (SCM_BOOL (info.use_locale), info_scm);
    info_scm = scm_cons (SCM_BOOL (info.use_symbol), info_scm);
    info_scm = scm_cons (SCM_BOOL (info.use_separators), info_scm);

    info_scm = scm_cons (scm_from_int (info.min_decimal_places), info_scm);
    info_scm = scm_cons (scm_from_int (info.max_decimal_places), info_scm);

    info_scm = scm_cons (gnc_commodity_to_scm (info.commodity), info_scm);

    info_scm = scm_cons (scm_from_locale_symbol ("print-info"), info_scm);

    return info_scm;
}

GNCPrintAmountInfo
gnc_scm2printinfo(SCM info_scm)
{
    GNCPrintAmountInfo info;

    /* skip type */
    info_scm = SCM_CDR (info_scm);
    info.commodity = gnc_scm_to_commodity (SCM_CAR (info_scm));

    info_scm = SCM_CDR (info_scm);
    info.max_decimal_places = scm_to_int (SCM_CAR (info_scm));

    info_scm = SCM_CDR (info_scm);
    info.min_decimal_places = scm_to_int (SCM_CAR (info_scm));

    info_scm = SCM_CDR (info_scm);
    info.use_separators = scm_is_true (SCM_CAR (info_scm));

    info_scm = SCM_CDR (info_scm);
    info.use_symbol = scm_is_true (SCM_CAR (info_scm));

    info_scm = SCM_CDR (info_scm);
    info.use_locale = scm_is_true (SCM_CAR (info_scm));

    info_scm = SCM_CDR (info_scm);
    info.monetary = scm_is_true (SCM_CAR (info_scm));

    info_scm = SCM_CDR (info_scm);
    info.force_fit = scm_is_true (SCM_CAR (info_scm));

    info_scm = SCM_CDR (info_scm);
    info.round = scm_is_true (SCM_CAR (info_scm));

    return info;
}
