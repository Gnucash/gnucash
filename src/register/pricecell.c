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

#include <ctype.h>
#include <string.h>
#include <locale.h>
#include <string.h>

#include "gnc-common.h"
#include "gnc-exp-parser.h"
#include "gnc-engine-util.h"
#include "gnc-ui-util.h"

#include "basiccell.h"
#include "pricecell.h"


/* GUI-dependent */
extern void xaccPriceGUIInit (PriceCell *);

static void xaccInitPriceCell (PriceCell *cell);
static void PriceSetValue (BasicCell *, const char *);
static const char * xaccPriceCellPrintValue (PriceCell *cell);


/* ================================================ */

/* set the color of the text to red, if the value is negative */
/* hack alert -- the actual color should probably be configurable */
#define COLORIZE(cell,amount) {			\
   if ((0.0 > amount) && !DEQ(amount, 0.0)) {	\
      /* red */					\
      cell->cell.fg_color = 0xff0000;		\
   } else {					\
      /* black */				\
      cell->cell.fg_color = 0x0;		\
   }						\
}

#define SET(cell,str) { 			\
   g_free ((cell)->value);	                \
   (cell)->value = g_strdup (str);		\
}

/* ================================================ */

static gboolean
PriceEnter (BasicCell *_cell,
            int *cursor_position,
            int *start_selection,
            int *end_selection)
{
  *cursor_position = -1;
  *start_selection = 0;
  *end_selection   = -1;

  return TRUE;
}

/* ================================================ */
/* This callback only allows numbers with a single
 * decimal point in them */

static void
PriceMV (BasicCell *_cell, 
         const char *change, 
         const char *newval,
         int *cursor_position,
         int *start_selection,
         int *end_selection)
{
  PriceCell *cell = (PriceCell *) _cell;
  struct lconv *lc = gnc_localeconv();
  const char *toks = "+-*/=()";
  char decimal_point;
  char thousands_sep;
  int i;

  /* accept the newval string if user action was delete */
  if (change == NULL)
  {
    SET ((&(cell->cell)), newval);
    cell->need_to_parse = TRUE;
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

  for (i = 0; change[i] != '\0'; i++)
    if (!isdigit(change[i]) &&
        !isspace(change[i]) &&
        (decimal_point != change[i]) &&
        (thousands_sep != change[i]) &&
        (strchr (toks, change[i]) == NULL))
      return;

  SET ((&(cell->cell)), newval);
  cell->need_to_parse = TRUE;
}

/* ================================================ */

static void
PriceParse (PriceCell *cell)
{
  const char *newval;
  char *oldval;
  gnc_numeric amount;

  if (!cell->need_to_parse)
    return;

  oldval = cell->cell.value;
  if (oldval == NULL)
    oldval = "";

  if (gnc_exp_parser_parse (cell->cell.value, &amount, NULL))
    cell->amount = gnc_numeric_to_double (amount);
  else
    cell->amount = 0.0;

  COLORIZE (cell, cell->amount);

  newval = xaccPriceCellPrintValue(cell);

  /* If they are identical do nothing */
  if (strcmp(newval, oldval) == 0)
    return;

  /* Otherwise, change it */
  SET ((&(cell->cell)), newval);
}

/* ================================================ */

static void
PriceLeave (BasicCell *_cell) 
{
  PriceCell *cell = (PriceCell *) _cell;

  PriceParse (cell);
}

/* ================================================ */

static char *
PriceHelp (BasicCell *bcell)
{
  if ((bcell->value != NULL) && (bcell->value[0] != 0))
  {
    const char *help_str;

    help_str = bcell->value;

    return g_strdup(help_str);
  }

  if (bcell->blank_help != NULL)
    return g_strdup(bcell->blank_help);

  return NULL;
}

/* ================================================ */

PriceCell *
xaccMallocPriceCell (void)
{
   PriceCell *cell;

   cell = g_new(PriceCell, 1);

   xaccInitPriceCell (cell);

   return cell;
}

/* ================================================ */

static void
xaccInitPriceCell (PriceCell *cell)
{
   xaccInitBasicCell (&(cell->cell));

   cell->amount = 0.0;
   cell->blank_zero = TRUE;

   cell->print_info = gnc_default_print_info (FALSE);

   cell->need_to_parse = FALSE;

   SET (&(cell->cell), "");
   COLORIZE (cell, 0.0);

   cell->cell.use_fg_color = TRUE;
   cell->cell.enter_cell = PriceEnter;
   cell->cell.modify_verify = PriceMV;
   cell->cell.leave_cell = PriceLeave;
   cell->cell.set_value = PriceSetValue;
   cell->cell.get_help_value = PriceHelp;

   xaccPriceGUIInit (cell);
}

/* ================================================ */

void
xaccDestroyPriceCell (PriceCell *cell)
{
  cell->amount = 0.0;
  xaccDestroyBasicCell (&(cell->cell));
}

/* ================================================ */

static const char *
xaccPriceCellPrintValue (PriceCell *cell)
{
  if (cell->blank_zero && DEQ(cell->amount, 0.0))
    return "";

  return DxaccPrintAmount(cell->amount, cell->print_info);
}

/* ================================================ */

double
xaccGetPriceCellValue (PriceCell *cell)
{
  if (cell == NULL)
    return 0.0;

  PriceParse (cell);

  return cell->amount;
}

void
xaccSetPriceCellValue (PriceCell * cell, double amount)
{
  const char *buff;

  if (cell == NULL)
    return;

  cell->amount = amount;
  buff = xaccPriceCellPrintValue (cell);
  cell->need_to_parse = FALSE;

  SET (&(cell->cell), buff);

  /* set the cell color to red if the value is negative */
  COLORIZE (cell, amount);
}

void
xaccSetPriceCellBlank (PriceCell *cell)
{
  if (cell == NULL)
    return;

  cell->amount = 0.0;
  cell->need_to_parse = FALSE;

  SET (&(cell->cell), "");

  COLORIZE (cell, 0.0);
}

/* ================================================ */

void
xaccSetPriceCellPrintInfo (PriceCell *cell, GNCPrintAmountInfo print_info)
{
  if (cell == NULL)
    return;

  cell->print_info = print_info;
}

/* ================================================ */

void
xaccSetPriceCellBlankZero (PriceCell *cell, gboolean blank_zero)
{
  if (cell == NULL)
    return;

  cell->blank_zero = blank_zero;
}

/* ================================================ */

void xaccSetDebCredCellValue (PriceCell * debit,
                              PriceCell * credit,
                              double amount)
{
  /* debits are positive, credits are negative */
  if (amount > 0.0) {
    xaccSetPriceCellValue (debit, amount);
    xaccSetPriceCellValue (credit, 0.0);
  } else {
    xaccSetPriceCellValue (debit, 0.0);
    xaccSetPriceCellValue (credit, -amount);
  }
}

/* ================================================ */

static void 
PriceSetValue (BasicCell *_cell, const char *str)
{
  PriceCell *cell = (PriceCell *) _cell;
  gnc_numeric amount;

  if (str == NULL)
    str = "";

  if (*str == '\0')
    xaccSetPriceCellValue (cell, 0.0);
  else if (gnc_exp_parser_parse (str, &amount, NULL))
    xaccSetPriceCellValue (cell, gnc_numeric_to_double (amount));
}

/* --------------- end of file ---------------------- */
