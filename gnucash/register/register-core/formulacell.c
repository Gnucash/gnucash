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

#include <config.h>

#include <glib/gi18n.h>

#include "gnc-exp-parser.h"
#include "gnc-locale-utils.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

#include "basiccell.h"
#include "formulacell.h"

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "gnc.register.core.formulacell"

static void gnc_formula_cell_init( FormulaCell *fc );

static gboolean gnc_formula_cell_enter( BasicCell *_cell,
                                        int *cursor_position,
                                        int *start_selection,
                                        int *end_selection );

static void gnc_formula_cell_leave( BasicCell *_cell );

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
    fc->cell.leave_cell    = gnc_formula_cell_leave;
}

void
gnc_formula_cell_set_value( FormulaCell *fc,
                            const char *newVal )
{
    g_debug("got value [%s]", newVal);
    gnc_formula_cell_set_value_internal( &fc->cell, newVal );
}

static
gboolean
gnc_formula_cell_enter( BasicCell *_cell,
                        int *cursor_position,
                        int *start_selection,
                        int *end_selection )
{
    g_debug("%d, %d, %d", *cursor_position, *start_selection, *end_selection);
    *cursor_position = -1;
    *start_selection = 0;
    *end_selection   = -1;
    return TRUE;
}

static void
gnc_formula_cell_leave(BasicCell *_cell)
{
    char *str;
    FormulaCell *fc = (FormulaCell*)_cell;
    str = fc->cell.value;
    {
        char *error_location = NULL;
        gnc_numeric amount;
        if (str != NULL
                && strlen(str) != 0
                && !gnc_exp_parser_parse(str, &amount, &error_location))
        {
            gint error_position = error_location - str;
            gnc_warning_dialog (gnc_ui_get_main_window (NULL),
                                _("An error occurred while processing '%s' at position %d"),
                                str, error_position);
        }
    }

    gnc_basic_cell_set_value_internal( &fc->cell, str );
}

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
    gunichar decimal_point;
    gunichar thousands_sep;
    const char *c;
    gunichar uc;

    gchar *filtered_newval;
    const gchar *symbol = NULL;
    gchar *tokens;

    g_debug("%s, %d, %s, %d, %d, %d, %d",
            change ? (gchar *)change : "(null)", change_len,
            newval ? (gchar *)newval : "(null)", newval_len,
            *cursor_position, *start_selection, *end_selection);

    /* accept the newval string if user action was delete */
    if (change == NULL)
    {
        gnc_basic_cell_set_value_internal( &cell->cell, newval );
        // Remove any selection.
        *end_selection = *start_selection = *cursor_position;
        return;
    }

    if (cell->print_info.monetary)
    {
        const gnc_commodity *comm = cell->print_info.commodity;

        decimal_point = g_utf8_get_char(lc->mon_decimal_point);
        thousands_sep = g_utf8_get_char(lc->mon_thousands_sep);

        if (comm)
            symbol = gnc_commodity_get_nice_symbol (comm);
        else
            symbol = gnc_commodity_get_nice_symbol (gnc_default_currency ());

        tokens = g_strconcat (toks, symbol, NULL);
    }
    else
    {
        decimal_point = g_utf8_get_char(lc->decimal_point);
        thousands_sep = g_utf8_get_char(lc->thousands_sep);

        tokens = g_strdup (toks);
    }

    c = change;
    while (*c)
    {
        uc = g_utf8_get_char (c);
        if (!g_unichar_isdigit (uc) &&
            !g_unichar_isspace (uc) &&
            !g_unichar_isalpha (uc) &&
            (decimal_point != uc) &&
            (thousands_sep != uc) &&
            (g_utf8_strchr (tokens, -1, uc) == NULL))
         {
            g_free (tokens);
            return;
         }
        c = g_utf8_next_char (c);
    }

    filtered_newval = gnc_filter_text_for_currency_symbol (symbol, newval);
    gnc_basic_cell_set_value_internal (&cell->cell, filtered_newval);
    g_free (filtered_newval);
    g_free (tokens);
}

static
void
gnc_formula_cell_set_value_internal( BasicCell *_cell,
                                     const char *str )
{
    FormulaCell *fc = (FormulaCell*)_cell;
    g_debug("internal string: [%s]", str);
    gnc_basic_cell_set_value_internal( &fc->cell, str );
}
