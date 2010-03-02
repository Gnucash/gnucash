/********************************************************************\
 * pricecell.c -- price input/display cell                          *
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

/*
 * FILE:
 * pricecell.c
 *
 * FUNCTION:
 * Implements the price cell
 *
 * HISTORY:
 * Copyright (c) 1998, 1999, 2000 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include "config.h"

#include <glib.h>
#include <glib/gi18n.h>
#include <locale.h>
#include <string.h>

#include "gnc-exp-parser.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

#include "basiccell.h"
#include "pricecell.h"


static void gnc_price_cell_init (PriceCell *cell);
static void gnc_price_cell_set_value_internal (BasicCell *bcell,
        const char *value);
static const char * gnc_price_cell_print_value (PriceCell *cell);


static gboolean
gnc_price_cell_enter (BasicCell *_cell,
                      int *cursor_position,
                      int *start_selection,
                      int *end_selection)
{
    *cursor_position = -1;
    *start_selection = 0;
    *end_selection   = -1;

    return TRUE;
}

static void
gnc_price_cell_modify_verify (BasicCell *_cell,
                              const char *change,
                              int change_len,
                              const char *newval,
                              int newval_len,
                              int *cursor_position,
                              int *start_selection,
                              int *end_selection)
{
    PriceCell *cell = (PriceCell *) _cell;
    struct lconv *lc = gnc_localeconv ();
    const char *toks = "+-*/=()_";
    gunichar decimal_point;
    gunichar thousands_sep;
    const char *c;
    gunichar uc;

    /* accept the newval string if user action was delete */
    if (change == NULL)
    {
        gnc_basic_cell_set_value_internal (_cell, newval);
        cell->need_to_parse = TRUE;
        return;
    }

    if (cell->print_info.monetary)
        decimal_point = g_utf8_get_char(lc->mon_decimal_point);
    else
        decimal_point = g_utf8_get_char(lc->decimal_point);

    if (cell->print_info.monetary)
        thousands_sep = g_utf8_get_char(lc->mon_thousands_sep);
    else
        thousands_sep = g_utf8_get_char(lc->thousands_sep);

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

    gnc_basic_cell_set_value_internal (_cell, newval);
    cell->need_to_parse = TRUE;
}

static gint
gnc_price_cell_parse (PriceCell *cell, gboolean update_value)
{
    const char *newval;
    char *oldval;
    gnc_numeric amount;

    if (!cell->need_to_parse)
        return -1;

    oldval = cell->cell.value;
    if (oldval == NULL)
        oldval = "";

    {
        char *err_location = NULL;
        if (strlen(g_strstrip(cell->cell.value)) == 0)
        {
            cell->amount = gnc_numeric_zero ();
        }
        else if (gnc_exp_parser_parse (cell->cell.value, &amount, &err_location))
        {
            if (cell->fraction > 0)
                amount = gnc_numeric_convert (amount, cell->fraction, GNC_RND_ROUND);

            cell->amount = amount;
        }
        else
        {
            return (err_location - cell->cell.value);
        }
    }

    if (!update_value)
        return -1;

    newval = gnc_price_cell_print_value (cell);

    /* If they are identical do nothing */
    if (strcmp(newval, oldval) == 0)
        return -1;

    /* Otherwise, change it */
    gnc_basic_cell_set_value_internal (&cell->cell, newval);
    return -1;
}

static void
gnc_price_cell_leave (BasicCell *_cell)
{
    gint error_position = -1;
    PriceCell *cell = (PriceCell *) _cell;

    error_position = gnc_price_cell_parse (cell, TRUE);
    if (error_position != -1)
    {
        gnc_warning_dialog(NULL, _("An error occurred while processing %s."),
                           cell->cell.value);
    }

}

BasicCell *
gnc_price_cell_new (void)
{
    PriceCell *cell;

    cell = g_new0 (PriceCell, 1);

    gnc_price_cell_init (cell);

    return &cell->cell;
}

void
gnc_price_cell_init (PriceCell *cell)
{
    gnc_basic_cell_init (&(cell->cell));

    cell->amount = gnc_numeric_zero ();
    cell->fraction = 0;
    cell->blank_zero = TRUE;

    cell->print_info = gnc_default_print_info (FALSE);

    cell->need_to_parse = FALSE;

    cell->cell.enter_cell = gnc_price_cell_enter;
    cell->cell.modify_verify = gnc_price_cell_modify_verify;
    cell->cell.leave_cell = gnc_price_cell_leave;
    cell->cell.set_value = gnc_price_cell_set_value_internal;
}

static const char *
gnc_price_cell_print_value (PriceCell *cell)
{
    if (cell->blank_zero && gnc_numeric_zero_p (cell->amount))
        return "";

    return xaccPrintAmount (cell->amount, cell->print_info);
}

gnc_numeric
gnc_price_cell_get_value (PriceCell *cell)
{
    if (cell == NULL)
        return gnc_numeric_zero ();

    gnc_price_cell_parse (cell, FALSE);

    return cell->amount;
}

gboolean
gnc_price_cell_set_value (PriceCell * cell, gnc_numeric amount)
{
    const char *buff;

    if (cell == NULL)
        return FALSE;

    if (cell->fraction > 0)
        amount = gnc_numeric_convert (amount, cell->fraction, GNC_RND_ROUND);

    cell->amount = amount;
    buff = gnc_price_cell_print_value (cell);
    cell->need_to_parse = FALSE;

    if (safe_strcmp (buff, cell->cell.value) == 0)
        return FALSE;

    gnc_basic_cell_set_value_internal (&cell->cell, buff);

    return TRUE;
}

void
gnc_price_cell_set_fraction (PriceCell *cell, int fraction)
{
    if (cell == NULL)
        return;

    cell->fraction = ABS (fraction);
}

void
gnc_price_cell_blank (PriceCell *cell)
{
    if (cell == NULL)
        return;

    cell->amount = gnc_numeric_zero ();
    cell->need_to_parse = FALSE;

    gnc_basic_cell_set_value_internal (&cell->cell, "");
}

void
gnc_price_cell_set_blank_zero (PriceCell *cell, gboolean blank_zero)
{
    if (cell == NULL)
        return;

    cell->blank_zero = blank_zero;
}

void
gnc_price_cell_set_print_info (PriceCell *cell, GNCPrintAmountInfo print_info)
{
    if (cell == NULL)
        return;

    cell->print_info = print_info;
}

void
gnc_price_cell_set_debt_credit_value (PriceCell * debit,
                                      PriceCell * credit,
                                      gnc_numeric amount)
{
    /* debits are positive, credits are negative */
    if (gnc_numeric_positive_p (amount))
    {
        gnc_price_cell_set_value (debit, amount);
        gnc_price_cell_set_value (credit, gnc_numeric_zero ());
    }
    else
    {
        gnc_price_cell_set_value (debit, gnc_numeric_zero ());
        gnc_price_cell_set_value (credit, gnc_numeric_neg (amount));
    }
}

static void
gnc_price_cell_set_value_internal (BasicCell *_cell, const char *str)
{
    PriceCell *cell = (PriceCell *) _cell;
    gnc_numeric amount;

    if (str == NULL)
        str = "";

    if (*str == '\0')
        gnc_price_cell_set_value (cell, gnc_numeric_zero ());
    else if (gnc_exp_parser_parse (str, &amount, NULL))
        gnc_price_cell_set_value (cell, amount);
}
