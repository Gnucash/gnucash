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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include "gnc-engine.h"
#include "gnc-ui-util.h"

#include "basiccell.h"
#include "formulacell.h"

static QofLogModule log_module = GNC_MOD_SX;

static void gnc_formula_cell_init( FormulaCell *fc );

static gboolean gnc_formula_cell_enter( BasicCell *_cell,
                                        int *cursor_position,
                                        int *start_selection,
                                        int *end_selection );

#if 0
static void gnc_formula_cell_leave( BasicCell *_cell );
#endif

static void gnc_formula_cell_modify_verify( BasicCell *_cell, 
                                            const char *change,
                                            int change_len,
                                            const char *newval,
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

#if 0
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
#endif

static
void
gnc_formula_cell_modify_verify( BasicCell *_cell,
                                const char *change,
                                int change_len,
                                const char *newval,
                                int newval_len,
                                int *cursor_position,
                                int *start_selection,
                                int *end_selection )
{
  FormulaCell *cell = (FormulaCell *)_cell;
  struct lconv *lc = gnc_localeconv ();
  const char *toks = "+-*/=()_:";
  unsigned char decimal_point;
  unsigned char thousands_sep;
  const char *c;
  gunichar uc;

  DEBUG( "%s, %d, %s, %d, %d, %d, %d",
         (gchar *)change, change_len, (gchar *)newval, newval_len,
         *cursor_position, *start_selection, *end_selection );

  /* accept the newval string if user action was delete */
  if (change == NULL)
  {
    gnc_basic_cell_set_value_internal( &cell->cell, newval );
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

   c = change;
   while (*c)
   {
      uc = g_utf8_get_char (c);
      if (!g_unichar_isdigit (uc) &&
          !g_unichar_isspace (uc) &&
          !g_unichar_isalpha (uc) &&
          (decimal_point != uc) &&
          (thousands_sep != uc) &&
          (g_utf8_strchr (toks, -1, uc) == NULL))
        return;
      c = g_utf8_next_char (c);
   }

  gnc_basic_cell_set_value_internal( &cell->cell, newval );
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
