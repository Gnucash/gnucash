/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * validation.c: Implementation of validation.
 *
 * Copyright (C) Jody Goldberg <jody@gnome.org>
 *
 * based on work by
 *	 Almer S. Tigelaar <almer@gnome.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <gnumeric-config.h>
#include <glib/gi18n.h>
#include "gnumeric.h"
#include "numbers.h"
#include "mathfunc.h"
#include "validation.h"
#include "expr.h"
#include "mstyle.h"
#include "workbook.h"
#include "sheet.h"
#include "cell.h"
#include "value.h"
#include "number-match.h"
#include "workbook-control.h"
#include "parse-util.h"

#include <math.h>
#include <string.h>

/**
 * validation_new :
 *
 * @title : will be copied.
 * @msg   : will be copied.
 * @expr0 : absorb the reference to the expression.
 * @expr1 : absorb the reference to the expression.
 */
GnmValidation *
validation_new (ValidationStyle style,
		ValidationType  type,
		ValidationOp    op,
		char const *title, char const *msg,
		GnmExpr const *expr0, GnmExpr const *expr1,
		gboolean allow_blank, gboolean use_dropdown)
{
	GnmValidation *v;

	if (type == VALIDATION_TYPE_CUSTOM && op != VALIDATION_OP_NONE) {
		/* This can happen if an .xls file was saved as a .gnumeric.  */
		g_warning ("VALIDATION_TYPE_CUSTOM needs to go with VALIDATION_OP_NONE.  Fixing.");
		op = VALIDATION_OP_NONE;
	}

	v = g_new0 (GnmValidation, 1);
	v->ref_count = 1;

	v->title = title ? gnm_string_get (title) : NULL;
	v->msg   = msg ? gnm_string_get (msg) : NULL;
	v->expr[0] = expr0;
	v->expr[1] = expr1;
	v->style   = style;
	v->type    = type;
	v->op      = op;
	v->allow_blank  = (allow_blank != FALSE);
	v->use_dropdown = (use_dropdown != FALSE);

	return v;
}

void
validation_ref (GnmValidation *v)
{
	g_return_if_fail (v != NULL);
	v->ref_count++;
}

void
validation_unref (GnmValidation *v)
{
	g_return_if_fail (v != NULL);

	v->ref_count--;

	if (v->ref_count < 1) {
		int i;

		if (v->title != NULL) {
			gnm_string_unref (v->title);
			v->title = NULL;
		}
		if (v->msg != NULL) {
			gnm_string_unref (v->msg);
			v->msg = NULL;
		}
		for (i = 0 ; i < 2 ; i++)
			if (v->expr[i] != NULL) {
				gnm_expr_unref (v->expr[i]);
				v->expr[i] = NULL;
			}
		g_free (v);
	}
}

/**
 * validation_eval:
 *
 * Either pass @expr or @val.
 * The parameters will be validated against the
 * validation set in the GnmStyle if applicable.
 **/
ValidationStatus
validation_eval (WorkbookControl *wbc, GnmStyle const *mstyle,
		 Sheet *sheet, GnmCellPos const *pos, gboolean *showed_dialog)
{
	GnmValidation *v;
	GnmCell	   *cell;
	char	   *msg = NULL;
	gboolean    allocated_msg = FALSE;
	ValidationStatus result;

	v = mstyle_get_validation (mstyle);
	if (v == NULL)
		return VALIDATION_STATUS_VALID;

	if (v->style == VALIDATION_TYPE_ANY)
		return VALIDATION_STATUS_VALID;

	cell = sheet_cell_get (sheet, pos->col, pos->row);
	if (cell != NULL)
		dependent_eval (CELL_TO_DEP (cell));

	if (cell_is_empty (cell)) {
		if (v->allow_blank)
			return VALIDATION_STATUS_VALID;
		msg = g_strdup_printf (_("Cell %s is not permitted to be blank"),
				       cell_name (cell));
	} else {
		GnmExpr const *val_expr = NULL, *expr = NULL;
		GnmValue *val = cell->value;

		switch (v->type) {
		case VALIDATION_TYPE_ANY :
			return VALIDATION_STATUS_VALID;

		case VALIDATION_TYPE_AS_INT :
		case VALIDATION_TYPE_AS_NUMBER :
		case VALIDATION_TYPE_AS_DATE :		/* What the hell does this do */
		case VALIDATION_TYPE_AS_TIME : {	/* What the hell does this do */
			GnmValue *res = NULL;
			/* we know it is not empty */
			if (val->type == VALUE_ERROR) {
				msg = g_strdup_printf (_("'%s' is an error"),
						       value_peek_string (val));
				break;
			} else if (val->type == VALUE_STRING) {
				char const *s = value_peek_string (val);
				res = format_match_number (s, NULL,
					workbook_date_conv (sheet->workbook));
				if (res == NULL) {
					char const *fmt;
					/* FIXME what else is needed */
					if (v->type == VALIDATION_TYPE_AS_DATE) {
						fmt = N_("'%s' is not a valid date");
					} else if (v->type == VALIDATION_TYPE_AS_TIME) {
						fmt = N_("'%s' is not a valid time");
					} else
						fmt = N_("'%s' is not a number");
					msg = g_strdup_printf (_(fmt), s);
					break;
				}
			} else
				res = value_dup (val);

			if (v->type == VALIDATION_TYPE_AS_INT &&
			    res != NULL && res->type == VALUE_FLOAT) {
				gnm_float f = value_get_as_float (res);
				gboolean isint = gnumabs (f - gnumeric_fake_round (f)) < 1e-10;
				if (!isint) {
					char const *valstr = value_peek_string (val);
					msg = g_strdup_printf (_("'%s' is not an integer"), valstr);
					break;
				}
			}

			val_expr = gnm_expr_new_constant (res);
			break;
		}

		case VALIDATION_TYPE_IN_LIST :
#ifdef GOG_WARN_TODO
#warning TODO
#endif
			return VALIDATION_STATUS_VALID;

		case VALIDATION_TYPE_TEXT_LENGTH :
			/* XL appears to use a very basic value -> string mapping that
			 * ignores formatting.
			 * eg len (12/13/01) == len (37238) = 5
			 * This seems wrong for
			 */
			val_expr = gnm_expr_new_constant (
				value_new_int (strlen (value_peek_string (val))));
			break;

		case VALIDATION_TYPE_CUSTOM :
			expr = v->expr[0];
			if (expr == NULL)
				return VALIDATION_STATUS_VALID;
			gnm_expr_ref (expr);
			break;
		}

		if (msg == NULL && expr == NULL) {
			GnmExprOp op;

			g_return_val_if_fail (val_expr != NULL, VALIDATION_STATUS_VALID);

			switch (v->op) {
			case VALIDATION_OP_EQUAL :	 op = GNM_EXPR_OP_EQUAL;	break;
			case VALIDATION_OP_NOT_EQUAL :	 op = GNM_EXPR_OP_NOT_EQUAL;	break;
			case VALIDATION_OP_GT :		 op = GNM_EXPR_OP_GT;		break;
			case VALIDATION_OP_NOT_BETWEEN :
			case VALIDATION_OP_LT :		 op = GNM_EXPR_OP_LT;		break;
			case VALIDATION_OP_BETWEEN :
			case VALIDATION_OP_GTE :	 op = GNM_EXPR_OP_GTE;		break;
			case VALIDATION_OP_LTE :	 op = GNM_EXPR_OP_LTE;		break;
			default :
				g_warning ("Invalid validation operator %d", v->op);
				return VALIDATION_STATUS_VALID;
			}

			if (v->expr [0] == NULL)
				return VALIDATION_STATUS_VALID;

			gnm_expr_ref (v->expr[0]);
			expr = gnm_expr_new_binary (val_expr, op, v->expr[0]);
		}

		if (expr != NULL) {
			GnmParsePos  pp;
			GnmEvalPos   ep;
			char	 *expr_str;
			GnmValue    *val;
			gboolean  dummy, valid;

			eval_pos_init_cell (&ep, cell);
			val = gnm_expr_eval (expr, &ep, GNM_EXPR_EVAL_SCALAR_NON_EMPTY);
			valid = value_get_as_bool (val, &dummy);
			value_release (val);

			if (valid && v->op != VALIDATION_OP_BETWEEN) {
				gnm_expr_unref (expr);
				return VALIDATION_STATUS_VALID;
			}

			if ((v->op == VALIDATION_OP_BETWEEN && valid) ||
			    v->op == VALIDATION_OP_NOT_BETWEEN) {
				g_return_val_if_fail (v->expr[1] != NULL, VALIDATION_STATUS_VALID);

				gnm_expr_ref (val_expr);
				gnm_expr_ref (v->expr[1]);
				gnm_expr_unref (expr);
				expr = gnm_expr_new_binary (val_expr,
					(v->op == VALIDATION_OP_BETWEEN) ? GNM_EXPR_OP_LTE : GNM_EXPR_OP_GT,
					v->expr[1]);
				val = gnm_expr_eval (expr, &ep, GNM_EXPR_EVAL_SCALAR_NON_EMPTY);
				valid = value_get_as_bool (val, &dummy);
				value_release (val);
				if (valid) {
					gnm_expr_unref (expr);
					return VALIDATION_STATUS_VALID;
				}
			}

			expr_str = gnm_expr_as_string (expr,
				parse_pos_init_evalpos (&pp, &ep),
				gnm_expr_conventions_default);
			msg = g_strdup_printf (_("%s is not true."), expr_str);
			g_free (expr_str);
			gnm_expr_unref (expr);
		}
	}

	if (v->msg != NULL && v->msg->str[0] != '\0') {
		if (msg != NULL)
			g_free (msg);
		msg = v->msg->str;
	} else  {
		if (msg != NULL)
			allocated_msg = TRUE;
		else
			msg = _("That value is invalid.\n"
				"Restrictions have been placed on this cell's contents.");
	}

	if (showed_dialog != NULL)
		*showed_dialog = TRUE;
	result = wb_control_validation_msg (wbc, v->style,
		(v->title != NULL && v->title->str[0] != '\0')
			? v->title->str
			: _("Gnumeric: Validation"),
		msg);
	if (allocated_msg)
		g_free (msg);
	return result;
}
