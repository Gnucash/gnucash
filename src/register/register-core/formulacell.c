/********************************************************************\
 * formulacell.c -- Formula entry/display cell                      *
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
\********************************************************************/

#include "config.h"

#include <ctype.h>
#include <string.h>

#include "gnc-engine-util.h"
#include "gnc-ui-util.h"

#include "basiccell.h"
#include "formulacell.h"

static short module = MOD_SX;

static void gnc_formula_cell_init( FormulaCell *fc );

static gboolean gnc_formula_cell_enter( BasicCell *_cell,
                                        int *cursor_position,
                                        int *start_selection,
                                        int *end_selection );

static void gnc_formula_cell_leave( BasicCell *_cell );

static void gnc_formula_cell_modify_verify( BasicCell *_cell, 
                                            const GdkWChar *change,
                                            int change_len,
                                            const GdkWChar *newval,
                                            int newval_len,
                                            int *cursor_position,
                                            int *start_selection,
                                            int *end_selection );

static void gnc_formula_cell_set_value_internal( BasicCell *_cell,
                                                 const char *str );



BasicCell*
gnc_formula_cell_new(void)
{
  FormulaCell *fc = g_new0( FormulaCell, 1 );
  gnc_formula_cell_init( fc );
  return &fc->cell;
}

static
void
gnc_formula_cell_init( FormulaCell *fc )
{
  gnc_basic_cell_init (&(fc->cell));

  fc->print_info         = gnc_default_print_info (FALSE);

  fc->cell.enter_cell    = gnc_formula_cell_enter;
  fc->cell.modify_verify = gnc_formula_cell_modify_verify;
  fc->cell.set_value     = gnc_formula_cell_set_value_internal;
}

void
gnc_formula_cell_set_value( FormulaCell *fc,
                            const char *newVal )
{
  DEBUG( "Got value \"%s\"", newVal );
  gnc_formula_cell_set_value_internal( &fc->cell, newVal );
}

static
gboolean
gnc_formula_cell_enter( BasicCell *_cell,
                        int *cursor_position,
                        int *start_selection,
                        int *end_selection )
{
  DEBUG( "%d, %d, %d", *cursor_position, *start_selection, *end_selection );
  *cursor_position = -1;
  *start_selection = 0;
  *end_selection   = -1;
  return TRUE;
}

static
void
gnc_formula_cell_leave( BasicCell *_cell )
{
  char *str;
  FormulaCell *fc = (FormulaCell*)_cell;
  DEBUG( "leaving.." );
  str = fc->cell.value;
  gnc_basic_cell_set_value_internal( &fc->cell, str );
}

static
void
gnc_formula_cell_modify_verify( BasicCell *_cell, 
                                const GdkWChar *change,
                                int change_len,
                                const GdkWChar *newval,
                                int newval_len,
                                int *cursor_position,
                                int *start_selection,
                                int *end_selection )
{
  FormulaCell *cell = (FormulaCell *)_cell;
  struct lconv *lc = gnc_localeconv ();
  const char *toks = "+-*/=()_";
  char decimal_point;
  char thousands_sep;
  int i;

  DEBUG( "%s, %d, %s, %d, %d, %d, %d",
         change, change_len, newval, newval_len,
         *cursor_position, *start_selection, *end_selection );

  /* accept the newval string if user action was delete */
  if (change == NULL)
  {
    gnc_basic_cell_set_wcvalue_internal( &cell->cell, newval );
    return;
  }

  if (cell->print_info.monetary)
    decimal_point = lc->mon_decimal_point[0];
  else
    decimal_point = lc->decimal_point[0];

  if (cell->print_info.monetary)
    thousands_sep = lc->mon_thousands_sep[0];
  else
    thousands_sep = lc->thousands_sep[0];

  for (i = 0; i < change_len; i++)
    if (!isdigit(change[i]) &&
        !isspace(change[i]) &&
        !isalpha(change[i]) &&
        (decimal_point != change[i]) &&
        (thousands_sep != change[i]) &&
        (strchr (toks, change[i]) == NULL))
      return;

  gnc_basic_cell_set_wcvalue_internal( &cell->cell, newval );
}

static
void
gnc_formula_cell_set_value_internal( BasicCell *_cell,
                                     const char *str )
{
  FormulaCell *fc = (FormulaCell*)_cell;
  DEBUG( "internal string: %s", str );
  gnc_basic_cell_set_value_internal( &fc->cell, str );
}
