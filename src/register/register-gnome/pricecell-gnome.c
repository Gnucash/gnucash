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

/* pricecell-gnome.c
 *
 * Implements gnome dependent price cell functions :
 * 
 * Often the decimal key in the keypad is not mapped to the correct locale 
 * decimal point, the function PriceDirect handle this case.  
 */

#include "config.h"

#include <gnome.h>
#include <locale.h>

#include "gnc-exp-parser.h"
#include "gnc-ui-util.h"
#include "pricecell.h"
#include "pricecell-gnome.h"


static gboolean
gnc_price_cell_direct_update (BasicCell *bcell,
                              int *cursor_position,
                              int *start_selection,
                              int *end_selection,
                              void *gui_data)
{
    PriceCell *cell = (PriceCell *) bcell;
    GdkEventKey *event = gui_data;
    char decimal_point;
    struct lconv *lc;
    GString *newval_gs;
    gboolean is_return;
    int i;
    const char *c;
    gunichar uc;

    if (event->type != GDK_KEY_PRESS)
	return FALSE;

    lc = gnc_localeconv ();

    is_return = FALSE;

    switch (event->keyval)
    {
        case GDK_Return:
            if (!(event->state &
                  (GDK_CONTROL_MASK | GDK_MOD1_MASK | GDK_SHIFT_MASK)))
                is_return = TRUE;

        case GDK_KP_Enter:
            {
                char *error_loc;
                gnc_numeric amount;
                gboolean parse_ok;
                gboolean changed = FALSE;

                if (!cell->need_to_parse)
                    return FALSE;

                parse_ok = gnc_exp_parser_parse (cell->cell.value,
                                                 &amount, &error_loc);

                if (parse_ok)
                    changed = gnc_price_cell_set_value (cell, amount);
                else if (!cell->cell.value || cell->cell.value[0] == '\0')
                    changed = gnc_price_cell_set_value (cell,
                                                        gnc_numeric_zero ());
                else
                    *cursor_position = error_loc - cell->cell.value;

                /* If there is a problem with the parse, swallow
                 * the key so we stay put. */
                if (!parse_ok)
                    return TRUE;

                /* If nothing has changed, let the key cause a
                 * cursor activation no matter what. */
                if (!changed)
                    return FALSE;

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

    if (cell->print_info.monetary)
	decimal_point = lc->mon_decimal_point[0];
    else
	decimal_point = lc->decimal_point[0];

    /* allocate space for newval_ptr : oldval + one letter ( the
       decimal_point ) */
    newval_gs = g_string_new("");
    /* copy oldval up to the cursor position */
    i = 0;
    c = bcell->value;
    while (*c && (i < *cursor_position))
    {
        uc = g_utf8_get_char (c);
        g_string_append_unichar (newval_gs, uc);
        c = g_utf8_next_char (c);
        i++;
    }

    /* insert the decimal_point at cursor position */
    g_string_append_c (newval_gs, decimal_point);
    i++;
    c = g_utf8_next_char (c);
    
    /* copy oldval after cursor position */
    while (*c) 
    {
        uc = g_utf8_get_char (c);
        g_string_append_unichar (newval_gs, uc);
        c = g_utf8_next_char (c);
    }

    /* update the cursor position */
    (*cursor_position)++;

    gnc_basic_cell_set_value_internal (bcell, newval_gs->str);

    g_string_free (newval_gs, TRUE);
    
    cell->need_to_parse = TRUE;

    return TRUE;
}

BasicCell *
gnc_price_cell_gnome_new (void)
{
  BasicCell *cell;

  cell = gnc_price_cell_new ();

  cell->direct_update = gnc_price_cell_direct_update;

  return cell;
}
