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

#include <guile/gh.h>
#include <string.h>

#include "gnc-engine-util.h"
#include "engine-helpers.h"
#include "gnc-helpers.h"
#include "gnc-ui-util.h"


static short module = MOD_SX;

/* Type converters for GNCPrintAmountInfo */
SCM
gnc_printinfo2scm(GNCPrintAmountInfo info)
{
  SCM info_scm = SCM_EOL;

  info_scm = gh_cons (gh_bool2scm (info.round), info_scm);
  info_scm = gh_cons (gh_bool2scm (info.force_fit), info_scm);
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

  info_scm = gh_cdr (info_scm);
  info.force_fit = gh_scm2bool (gh_car (info_scm));

  info_scm = gh_cdr (info_scm);
  info.round = gh_scm2bool (gh_car (info_scm));

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

const char *
gnc_get_account_separator_string (void)
{
  static char sep[2];

  sep[0] = gnc_get_account_separator ();
  sep[1] = '\0';

  return sep;
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

gint
g_date_equals( gconstpointer gda, gconstpointer gdb )
{
  if ( !g_date_valid( (GDate*)gda )
       || !g_date_valid( (GDate*)gdb ) ) {
    DEBUG( "invalid: %p(%s), %p(%s)",
           gda, ( g_date_valid((GDate*)gda) ? "" : "*" ),
           gdb, ( g_date_valid((GDate*)gdb) ? "" : "*" ) );
  }
  return ( g_date_compare( (GDate*)gda, (GDate*)gdb )
           == 0 ? TRUE : FALSE );
}

guint
g_date_hash( gconstpointer gd )
{
  gint val = (g_date_year( (GDate*)gd ) * 10000)
    + (g_date_month( (GDate*)gd ) * 100)
    + g_date_day( (GDate*)gd );
  return g_int_hash( &val );
}
