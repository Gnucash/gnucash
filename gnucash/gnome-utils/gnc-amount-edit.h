/********************************************************************\
 * gnc-amount-edit.h -- amount editor widget                        *
 *                                                                  *
 * Copyright (C) 2000 Dave Peticolas <dave@krondo.com>              *
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
/*
  @NOTATION@
 */

#ifndef GNC_AMOUNT_EDIT_H
#define GNC_AMOUNT_EDIT_H

#include "qof.h"
#include "gnc-ui-util.h"

#include <gtk/gtk.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GNC_TYPE_AMOUNT_EDIT          (gnc_amount_edit_get_type ())
G_DECLARE_FINAL_TYPE (GNCAmountEdit, gnc_amount_edit, GNC, AMOUNT_EDIT, GtkBox)

/**
 * gnc_amount_edit_new:
 *
 * Creates a new GNCAmountEdit widget which can be used to provide
 * an easy to use way for entering amounts, allowing the user to
 * enter and evaluate expressions.
 *
 * Returns a GNCAmountEdit widget.
 */
GtkWidget *gnc_amount_edit_new (void);

/**
 * gnc_amount_edit_gtk_entry:
 * @gae: The GNCAmountEdit widget
 *
 * Returns the gtk entry of the widget..
 */
GtkWidget *gnc_amount_edit_gtk_entry (GNCAmountEdit *gae);

/**
 * gnc_amount_edit_set_amount:
 * @gae: The GNCAmountEdit widget
 * @amount: The amount to set as gnc_numeric
 *
 * Returns nothing.
 */
void gnc_amount_edit_set_amount (GNCAmountEdit *gae,
                                 gnc_numeric amount);

/**
 * gnc_amount_edit_set_damount:
 * @gae: The GNCAmountEdit widget
 * @amount: The amount to set as a double
 *
 * Returns nothing.
 */
void gnc_amount_edit_set_damount (GNCAmountEdit *gae,
                                  double amount);

/**
 * gnc_amount_edit_get_amount:
 * @gae: The GNCAmountEdit widget
 *
 * Returns the amount entered in the GNCAmountEdit widget as
 * a gnc_numeric, parsing the expression if necessary.
 * The result of parsing replaces the expression.
 */
gnc_numeric gnc_amount_edit_get_amount (GNCAmountEdit *gae);

/**
 * gnc_amount_edit_get_damount:
 * @gae: The GNCAmountEdit widget
 *
 * Returns the amount entered in the GNCAmountEdit widget as
 * a double, parsing the expression if necessary.
 * The result of parsing replaces the expression.
 */
double gnc_amount_edit_get_damount (GNCAmountEdit *gae);

/**
 * gnc_amount_edit_expr_is_valid
 * @gae: The GNCAmountEdit widget
 * @amount: parameter to hold the value of the parsed expression
 * @empty_ok: if true, an empty field is skipped, otherwise an empty field
 *            parses as 0
 * @error: if error location information is available it will be stored 
 *         in this variable. Set it to NULL if you don't want the error.
 * 
 * If needed, parse the expression in the amount entry. If there's no
 * parsing error, it returns the amount, otherwise it returns the
 * position in the expression where the error occurred.
 *
 * Return *  0 if the parsing was successful (note that if !empty_ok,
 *             an empty field will parse to 0)
 *        * -1 if the field is empty and that's ok (empty_ok)
 *        *  1 parsing failed
 */
gint gnc_amount_edit_expr_is_valid (GNCAmountEdit *gae,
                                    gnc_numeric *amount,
                                    gboolean empty_ok,
                                    GError **error);

/**
 * gnc_amount_edit_evaluate
 * @gae: The GNCAmountEdit widget
 * @error: if error location information is available it will be stored 
 *         in this variable. Set it to NULL if you don't want the error.
 *
 * If needed, parse the expression in the amount entry
 * and replace the expression with the result of evaluation.
 * If there is a parsing error, don't change the expression entry,
 * but do put the cursor at the point of the error.
 *
 * Return TRUE if parsing was successful or there was no need to parse.
 */
gboolean gnc_amount_edit_evaluate (GNCAmountEdit *gae, GError **error);

/**
 * gnc_amount_edit_set_print_flags:
 * @gae: The GNCAmountEdit widget
 * @print_flags: The print flags to set
 *
 * Returns nothing.
 */
void gnc_amount_edit_set_print_info (GNCAmountEdit *gae,
                                     GNCPrintAmountInfo print_info);

/**
 * gnc_amount_edit_set_fraction:
 * @gae: The GNCAmountEdit widget
 * @fraction: The fraction to set
 *
 * Returns nothing.
 */
void gnc_amount_edit_set_fraction (GNCAmountEdit *gae, int fraction);

/**
 * gnc_amount_edit_set_evaluate_on_enter:
 * @gae: The GNCAmountEdit widget
 * @evaluate_on_enter: The flag value to set
 *
 * Returns nothing.
 */
void gnc_amount_edit_set_evaluate_on_enter (GNCAmountEdit *gae,
                                            gboolean evaluate_on_enter);

/**
 * gnc_amount_edit_set_validate_on_change:
 * @gae: The GNCAmountEdit widget
 * @validate_on_change: The flag value to set
 *
 * Returns nothing.
 */
void gnc_amount_edit_set_validate_on_change (GNCAmountEdit *gae,
                                             gboolean validate_on_change);

/**
 * gnc_amount_edit_select_region:
 * @gae: The GNCAmountEdit widget
 * @start_pos: start of region
 * @end_pos: end of region
 *
 * Returns nothing.
 */
void gnc_amount_edit_select_region (GNCAmountEdit *gae,
                                    gint start_pos,
                                    gint end_pos);

/**
 * gnc_amount_edit_show_warning_symbol:
 * @gae: The GNCAmountEdit widget
 * @show: default is TRUE to show warning symbol, FALSE to not.
 *
 * Returns nothing.
 */
void gnc_amount_edit_show_warning_symbol (GNCAmountEdit *gae, gboolean show);

/**
 * gnc_amount_edit_make_mnemonic_target:
 * @gae: The GNCAmountEdit widget
 * @label: The label whose access key should set focus to this widget.
 *
 * Returns nothing.
 */
void gnc_amount_edit_make_mnemonic_target (GNCAmountEdit *gae, GtkWidget *label);

#ifdef __cplusplus
}
#endif

#endif
