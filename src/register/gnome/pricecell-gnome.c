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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

/* pricecell-gnome.c
 *
 * Implements gnome dependent price cell functions :
 * 
 * often the decimal key in the keypad is not maped to the correct locale 
 * decimal point, the function PriceDirect handle this case.  
 *
 */

#include "config.h"

#include <gnome.h>
#include <locale.h>

#include "pricecell.h"
#include "gnc-exp-parser.h"
#include "gnc-ui-util.h"


static gboolean
PriceDirect (BasicCell *bcell,
	     int *cursor_position,
	     int *start_selection,
	     int *end_selection,
	     void *gui_data)
{
    PriceCell *cell = (PriceCell *) bcell;
    GdkEventKey *event = gui_data;
    char decimal_point;
    struct lconv *lc;
    GdkWChar *newval;
    int i;

    if (event->type != GDK_KEY_PRESS)
	return FALSE;

    lc = gnc_localeconv ();

    switch (event->keyval)
    {
        case GDK_Return:
            if (!cell->need_to_parse)
                return FALSE;

            if (!(event->state &
                  (GDK_CONTROL_MASK | GDK_MOD1_MASK | GDK_SHIFT_MASK)))
                return FALSE;

        case GDK_KP_Enter:
            {
                char *error_loc;
                gnc_numeric amount;

                if (!cell->need_to_parse)
                    return FALSE;

                if (gnc_exp_parser_parse(cell->cell.value,
                                         &amount, &error_loc))
                    xaccSetPriceCellValue (cell, amount);
                else
                    *cursor_position = error_loc - cell->cell.value;

                return TRUE;
            }

        case GDK_KP_Decimal:
            break;

        default:
            return FALSE;
    }

    if (cell->print_info.monetary)
	decimal_point = lc->mon_decimal_point[0];
    else
	decimal_point = lc->decimal_point[0];

    /* allocate space for newval_ptr : oldval + one letter ( the
       decimal_point ) */
    newval = g_new (GdkWChar, bcell->value_len + 2);

    /* copy oldval up to the cursor position */
    for (i = 0; i < *cursor_position; i++)
        newval[i] = bcell->value_w[i];

    /* insert the decimal_point at cursor position */
    newval[*cursor_position] = decimal_point;

    for (i = *cursor_position + 1; i < bcell->value_len + 1; i++)
        newval[i] = bcell->value_w[i - 1];

    newval[bcell->value_len + 1] = 0;

    /* update the cursor position */
    (*cursor_position)++;

    xaccSetBasicCellWCValueInternal (bcell, newval);

    g_free (newval);

    cell->need_to_parse = TRUE;

    return TRUE;
}


void xaccPriceGUIInit (PriceCell *cell);

void
xaccPriceGUIInit (PriceCell *cell)
{
    if (cell == NULL)
	return;

    cell->cell.direct_update = PriceDirect;
}

/* =============== end of file =================== */

/*
  Local Variables:
  c-basic-offset: 4
  tab-width: 8
  End:
*/
