/********************************************************************\
 * tip-of-the-day.c -- C wrappers for scheme "tip of the day" fns   *
 * Copyright (C) 2000 Robert Merkel <rgmerk@mira.net>               *

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
\********************************************************************/

#include "config.h"

#include <guile/gh.h>

#include "tip-of-the-day.h"


/** Implementations ***************************************************/

/********************************************************************\
 * gnc_get_current_tip                                              *
 *    get the current tip                                           *
 *                                                                  *
 * Returns: A freshly malloc string containing the tip              *
\********************************************************************/

char *
gnc_get_current_tip(void)
{
  SCM tip_scm;
  tip_scm = gh_eval_str("(gnc:get-current-tip)");
  return gh_scm2newstr(tip_scm, NULL);
}

/********************************************************************\
 * gnc_increment_tip                                                *
 *    set the current tip to be the next one in the sequence        *
 *                                                                  *
 * Returns: nothing                                                 *
\********************************************************************/

void
gnc_increment_tip(void)
{
  gh_eval_str("(gnc:increment-tip-number)");
  return;
}

/********************************************************************\
 * gnc_decrement_tip                                                *
 *    set the current tip to the previous one in the sequence       *
 *                                                                  *
 * Returns: Nothing                                                 *
\********************************************************************/

void
gnc_decrement_tip(void)
{
  gh_eval_str("(gnc:decrement-tip-number)");
  return;
}

/********************** END OF FILE *********************************\
\********************************************************************/
