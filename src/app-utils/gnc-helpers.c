/********************************************************************\
 * gnc-helpers.c -- gnucash g-wrap helper functions                 *
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

#include "config.h"

#include "gnc-ui-util.h"
#include "engine-helpers.h"
#include <guile/gh.h>

#include <string.h>

/* Type converters for GNCPrintAmountInfo */
SCM
gnc_printinfo2scm(GNCPrintAmountInfo info)
{
  SCM info_scm = SCM_EOL;

  info_scm = gh_cons (gh_bool2scm (info.monetary), info_scm);
  info_scm = gh_cons (gh_bool2scm (info.use_locale), info_scm);
  info_scm = gh_cons (gh_bool2scm (info.use_symbol), info_scm);
  info_scm = gh_cons (gh_bool2scm (info.use_separators), info_scm);

  info_scm = gh_cons (gh_int2scm (info.min_decimal_places), info_scm);
  info_scm = gh_cons (gh_int2scm (info.max_decimal_places), info_scm);

  info_scm = gh_cons (gnc_commodity_to_scm (info.commodity), info_scm);

  info_scm = gh_cons (gh_symbol2scm ("print-info"), info_scm);

  return info_scm;
}

GNCPrintAmountInfo
gnc_scm2printinfo(SCM info_scm)
{
  GNCPrintAmountInfo info;

  /* skip type */
  info_scm = gh_cdr (info_scm);
  info.commodity = gnc_scm_to_commodity (gh_car (info_scm));

  info_scm = gh_cdr (info_scm);
  info.max_decimal_places = gh_scm2int (gh_car (info_scm));

  info_scm = gh_cdr (info_scm);
  info.min_decimal_places = gh_scm2int (gh_car (info_scm));

  info_scm = gh_cdr (info_scm);
  info.use_separators = gh_scm2bool (gh_car (info_scm));

  info_scm = gh_cdr (info_scm);
  info.use_symbol = gh_scm2bool (gh_car (info_scm));

  info_scm = gh_cdr (info_scm);
  info.use_locale = gh_scm2bool (gh_car (info_scm));

  info_scm = gh_cdr (info_scm);
  info.monetary = gh_scm2bool (gh_car (info_scm));

  return info;
}

int
gnc_printinfo_p(SCM info_scm)
{
  char *symbol;
  int retval;

  if (!gh_list_p(info_scm) || gh_null_p(info_scm))
    return 0;

  info_scm = gh_car (info_scm);
  if (!gh_symbol_p (info_scm))
    return 0;

  symbol = gh_symbol2newstr (info_scm, NULL);
  if (symbol == NULL)
    return 0;

  retval = strcmp (symbol, "print-info") == 0;

  free (symbol);

  return retval;
}

