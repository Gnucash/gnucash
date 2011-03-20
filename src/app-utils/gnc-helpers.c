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

#include "config.h"

#include <libguile.h>
#include "guile-mappings.h"
#include <string.h>
#include "swig-runtime.h"

#include "gnc-engine.h"
#include "engine-helpers.h"
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

    info_scm = scm_cons (scm_int2num (info.min_decimal_places), info_scm);
    info_scm = scm_cons (scm_int2num (info.max_decimal_places), info_scm);

    info_scm = scm_cons (gnc_commodity_to_scm (info.commodity), info_scm);

    info_scm = scm_cons (scm_str2symbol ("print-info"), info_scm);

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
    info.max_decimal_places = scm_num2int (SCM_CAR (info_scm), SCM_ARG1,
                                           G_STRFUNC);

    info_scm = SCM_CDR (info_scm);
    info.min_decimal_places = scm_num2int (SCM_CAR (info_scm), SCM_ARG1,
                                           G_STRFUNC);

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

int
gnc_printinfo_p(SCM info_scm)
{
    const gchar *symbol;

    if (!scm_is_list(info_scm) || scm_is_null(info_scm))
        return 0;

    info_scm = SCM_CAR (info_scm);
    if (!scm_is_symbol (info_scm))
        return 0;

    symbol = SCM_SYMBOL_CHARS (info_scm);
    if (symbol == NULL)
        return 0;

    return (strcmp (symbol, "print-info") == 0);
}

/* This is a scaled down version of the routine that would be needed
 * to fully convert a gnc-commodity to a scheme data structure.  In an
 * attempt to optimize the speed of price quote retrieval, this
 * routine only converts the fields that price-quotes.scm uses. Since
 * it converts these fields all at once, it should prevent multiple
 * transitions back and forth from Scheme to C to extract
 * the data from a pointers to a gnc-commodity (the older method).
 * This is *not* a reversible conversion as it drops data.
 *
 * When this routine was written, gnucash retrieved all quotes into
 * the user's default currency.  (Did earlier version do any
 * different?)  This routine inserts that default currency into the
 * returned structure as another optimization.
 */
SCM
gnc_quoteinfo2scm(gnc_commodity *comm)
{
    gnc_quote_source *source;
    const char *name, *tz;
    SCM info_scm = SCM_EOL, comm_scm, def_comm_scm;

    if (!comm)
        return SCM_EOL;

    source = gnc_commodity_get_quote_source (comm);
    name = gnc_quote_source_get_internal_name (source);
    tz = gnc_commodity_get_quote_tz (comm);
    comm_scm = SWIG_NewPointerObj(comm, SWIG_TypeQuery("_p_gnc_commodity"), 0);
    def_comm_scm = SWIG_NewPointerObj(gnc_default_currency (),
                                      SWIG_TypeQuery("_p_gnc_commodity"), 0);

    if (tz)
        info_scm = scm_cons (scm_makfrom0str (tz), info_scm);
    else
        info_scm = scm_cons (SCM_BOOL_F, info_scm);
    info_scm = scm_cons (def_comm_scm, info_scm);
    info_scm = scm_cons (comm_scm, info_scm);
    info_scm = scm_cons (scm_makfrom0str (name), info_scm);
    return info_scm;
}

SCM
gnc_parse_amount_helper (const char * string, gboolean monetary)
{
    gnc_numeric result;
    gboolean ok;

    g_return_val_if_fail (string, SCM_BOOL_F);

    ok = xaccParseAmount (string, monetary, &result, NULL);
    if (!ok)
        return SCM_BOOL_F;

    return gnc_numeric_to_scm (result);
}
