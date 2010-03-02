/********************************************************************\
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

/* formulacell-gnome.c
 *
 * Implements Gnome-dependent formula-cell functions :
 *
 * Often the decimal key in the keypad is not mapped to the correct locale
 * decimal point, the function PriceDirect handle this case.
 */

#include "config.h"

#include <gnome.h>
#include <locale.h>

#include "gnc-engine.h"

#include "gnc-exp-parser.h"
#include "gnc-ui-util.h"

#include "formulacell.h"
#include "formulacell-gnome.h"
#include "pricecell-gnome.h"

//static QofLogModule log_module = GNC_MOD_REGISTER;

static
gboolean
gnc_formula_cell_direct_update( BasicCell *bcell,
                                int *cursor_position,
                                int *start_selection,
                                int *end_selection,
                                void *gui_data )
{
    FormulaCell *cell = (FormulaCell *)bcell;
    GdkEventKey *event = gui_data;
    struct lconv *lc;
    gboolean is_return;

    if (event->type != GDK_KEY_PRESS)
        return FALSE;

    lc = gnc_localeconv ();

    is_return = FALSE;

    /* FIXME!! This code is almost identical (except for GDK_KP_Enter
     * handling) to pricecell-gnome.c:gnc_price_cell_direct_update.  I write
     * this after fixing a bug where one copy was kept up to date, and the
     * other not.  So, fix this.
     */
    switch (event->keyval)
    {
    case GDK_Return:
        if (!(event->state &
                (GDK_CONTROL_MASK | GDK_MOD1_MASK | GDK_SHIFT_MASK)))
            is_return = TRUE;
        /* FALL THROUGH TO NEXT CASE */

    case GDK_KP_Enter:
    {
        gnc_formula_cell_set_value( cell, cell->cell.value );

        /* If it's not a plain return, stay put. This
         * allows a 'calculator' style operation using
         * keypad enter where you can keep entering more
         * items to add, say. */
        return !is_return;
    }

    case GDK_KP_Decimal:
        break;

    default:
        return FALSE;
    }

    gnc_basic_cell_insert_decimal(bcell,
                                  cell->print_info.monetary
                                  ? lc->mon_decimal_point[0]
                                  : lc->decimal_point[0],
                                  cursor_position,
                                  start_selection,
                                  end_selection);

    return TRUE;
}

BasicCell *
gnc_formula_cell_gnome_new (void)
{
    BasicCell *cell;

    cell = gnc_formula_cell_new ();
    cell->direct_update = gnc_formula_cell_direct_update;
    return cell;
}
