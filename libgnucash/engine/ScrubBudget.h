/********************************************************************\
 * ScrubBudget.h -- fix budget amount signs                         *
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

#ifndef _GNC_SCRUBBUDGET_H_
#define _GNC_SCRUBBUDGET_H_

#include <glib.h>
#include <qofbook.h>

/* ================================================================ */

/** Fix budget signs
 * For GnuCash 5.0 onwards - fix budget signs
 * A feature is set if we have completed reversal.
 *
 * @param book The book to scrub
 *
 * returns TRUE if budgets were scrubbed
 * returns FALSE if feature already set, or no budgets.
 */
gboolean gnc_maybe_scrub_all_budget_signs (QofBook *book);


#endif // _GNC_SCRUBBUDGET_H_
