/********************************************************************\
 * gnc-exp-parser.c -- Implementation of expression parsing for     *
 *                     GnuCash using the routines in 'calculation'. *
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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
\********************************************************************/

#include "config.h"

#include <ctype.h>

#include "finproto.h"
#include "fin_spl_protos.h"
#include "gnc-exp-parser.h"
#include "messages.h"
#include "util.h"


/** Data Types *****************************************************/

typedef struct ParserNum
{
  double value;
} ParserNum;


/** Static Globals *************************************************/
static GHashTable *variable_bindings = NULL;
static ParseError  last_error = PARSER_NO_ERROR;
static gboolean    parser_inited = FALSE;


/** Implementations ************************************************/

void
gnc_exp_parser_init (void)
{
  if (parser_inited)
    gnc_exp_parser_shutdown ();

  variable_bindings = g_hash_table_new (g_str_hash, g_str_equal);

  parser_inited = TRUE;
}

static gboolean
remove_binding (gpointer key, gpointer value, gpointer not_used)
{
  g_free(key);
  g_free(value);

  return TRUE;
}

void
gnc_exp_parser_shutdown (void)
{
  if (!parser_inited)
    return;

  g_hash_table_foreach_remove (variable_bindings, remove_binding, NULL);
  g_hash_table_destroy (variable_bindings);
  variable_bindings = NULL;

  last_error = PARSER_NO_ERROR;

  parser_inited = FALSE;
}

static void
prepend_name (gpointer key, gpointer value, gpointer data)
{
  GList **list = data;

  *list = g_list_prepend (*list, key);
}

GList *
gnc_exp_parser_get_variable_names (void)
{
  GList *names = NULL;

  if (!parser_inited)
    return NULL;

  g_hash_table_foreach (variable_bindings, prepend_name, &names);

  return names;
}

void
gnc_exp_parser_remove_variable (const char *variable_name)
{
  gpointer key;
  gpointer value;

  if (!parser_inited)
    return;

  if (variable_name == NULL)
    return;

  if (g_hash_table_lookup_extended (variable_bindings, variable_name,
                                    &key, &value))
  {
    g_hash_table_remove (variable_bindings, key);
    g_free(key);
    g_free(value);
  }
}

void
gnc_exp_parser_remove_variable_names (GList * variable_names)
{
  if (!parser_inited)
    return;

  while (variable_names != NULL)
  {
    gnc_exp_parser_remove_variable (variable_names->data);
    variable_names = variable_names->next;
  }
}

gboolean
gnc_exp_parser_get_value (const char * variable_name, double *value_p)
{
  ParserNum *pnum;

  if (!parser_inited)
    return FALSE;

  if (variable_name == NULL)
    return FALSE;

  pnum = g_hash_table_lookup (variable_bindings, variable_name);
  if (pnum == NULL)
    return FALSE;

  if (value_p != NULL)
    *value_p = pnum->value;

  return TRUE;
}

void
gnc_exp_parser_set_value (const char * variable_name, double value)
{
  char *key;
  ParserNum *pnum;

  if (variable_name == NULL)
    return;

  if (!parser_inited)
    gnc_exp_parser_init ();

  gnc_exp_parser_remove_variable (variable_name);

  key = g_strdup (variable_name);

  pnum = g_new(ParserNum, 1);
  pnum->value = value;

  g_hash_table_insert (variable_bindings, key, pnum);
}

static void
make_predefined_vars_helper (gpointer key, gpointer value, gpointer data)
{
  var_store_ptr *vars_p = data;
  var_store_ptr var;

  var = g_new0 (var_store, 1);

  var->variable_name = key;
  var->value = value;
  var->next_var = *vars_p;

  *vars_p = var;
}

static var_store_ptr
make_predefined_variables (void)
{
  var_store_ptr vars = NULL;

  g_hash_table_foreach (variable_bindings, make_predefined_vars_helper, &vars);

  return vars;
}

static void
free_predefined_variables (var_store_ptr vars)
{
  var_store_ptr next;

  while (vars != NULL)
  {
    next = vars->next_var;

    g_free(vars->value);
    vars->value = NULL;

    g_free(vars);

    vars = next;
  }
}

static void
update_variables (var_store_ptr vars)
{
  for ( ; vars ; vars = vars->next_var )
  {
    ParserNum *pnum = vars->value;
    if (pnum != NULL)
      gnc_exp_parser_set_value (vars->variable_name, pnum->value);
  }
}

static void *
trans_numeric(const char *digit_str,
              char        radix_point,
              char        group_char,
              char      **rstr)
{
  ParserNum *pnum;
  double value;

  if (digit_str == NULL)
    return NULL;

  if (!xaccParseAmount (digit_str, TRUE, &value, rstr))
    return NULL;

  pnum = g_new0(ParserNum, 1);
  pnum->value = value;

  return pnum;
}

static void *
numeric_ops(char op_sym,
            void *left_value,
            void *right_value)
{
  ParserNum *left = left_value;
  ParserNum *right = right_value;
  ParserNum *result;

  if ((left == NULL) || (right == NULL))
    return NULL;

  result = (op_sym == ASN_OP) ? left : g_new(ParserNum, 1);

  switch (op_sym)
  {
    case ADD_OP:
      result->value = left->value + right->value;
      break;
    case SUB_OP:
      result->value = left->value - right->value;
      break;
    case DIV_OP:
      result->value = left->value / right->value;
      break;
    case MUL_OP:
      result->value = left->value * right->value;
      break;
    case ASN_OP:
      result->value = right->value;
      break;
  }

  return result;
}

static void *
negate_numeric(void *value)
{
  ParserNum *result = value;

  if (value == NULL)
    return NULL;

  result->value = -result->value;

  return result;
}

gboolean
gnc_exp_parser_parse (const char * expression, double *value_p,
                      char **error_loc_p)
{
  parser_env_ptr pe;
  var_store_ptr vars;
  struct lconv *lc;
  var_store result;
  char * error_loc;
  ParserNum *pnum;

  if (expression == NULL)
    return FALSE;

  if (!parser_inited)
    gnc_exp_parser_init ();

  result.variable_name = NULL;
  result.value = NULL;
  result.next_var = NULL;

  vars = make_predefined_variables ();
  lc = gnc_localeconv ();

  pe = init_parser (vars, *lc->mon_decimal_point, *lc->mon_thousands_sep,
                    trans_numeric, numeric_ops, negate_numeric, g_free);

  error_loc = parse_string (&result, expression, pe);

  pnum = result.value;

  if (error_loc == NULL)
  {
    if ((value_p != NULL) && (pnum != NULL))
      *value_p = pnum->value;

    if (error_loc_p != NULL)
      *error_loc_p = NULL;

    last_error = PARSER_NO_ERROR;
  }
  else
  {
    if (error_loc_p != NULL)
      *error_loc_p = error_loc;

    last_error = get_parse_error (pe);
  }

  update_variables (vars);
  update_variables (parser_get_vars (pe));

  free_predefined_variables (vars);

  exit_parser (pe);

  return (error_loc == NULL);
}

const char *
gnc_exp_parser_error_string (void)
{
  switch (last_error)
  {
    default:
    case PARSER_NO_ERROR:
      return NULL;
    case UNBALANCED_PARENS:
      return PARSER_UNBALANCED_PARENS;
    case STACK_OVERFLOW:
      return PARSER_STACK_OVERFLOW;
    case STACK_UNDERFLOW:
      return PARSER_STACK_OVERFLOW;
    case UNDEFINED_CHARACTER:
      return PARSER_UNDEFINED_CHARACTER;
    case NOT_A_VARIABLE:
      return PARSER_NOT_A_VARIABLE;
  }
}
