/***************************************************************************
                          expression-parser.c  -  description
                             -------------------
    begin                : Wednesday June 21 2000
    email                : tboldt@attglobal.net
    Author               : Terry D. Boldt
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

/*
 *  Functions to parse arthmetic expressions
 *  6-21-2000
 */

/* expression parser/evaluator use:
 *
 * Before describing the parser per se, I want to describe the
 * structures used to contain the results returned from the
 * parser. The structure is defined in "finvar.h":
 *
 * typedef struct var_store *var_store_ptr;
 *
 * typedef struct var_store {
 *     char *variable_name;
 *     char  use_flag;
 *     char  assign_flag;
 *     void          *value;
 *     var_strore_ptr next_var;
 * } var_store;
 *
 * The "use_flag" variable is for internal use of the parser and can
 * be ignored by the user. The "variable_name" variable possibly
 * points to a string containing the name of the value returned, a
 * "variable name". If NULL, then this is a temporary value. The
 * "value" variable points to a user defined structure containing the
 * numeric value of the variable.
 *
 * In designing and writing the parser, I decided early on that the
 * parser should be an "expression parser/evaluator" and that the
 * actual arithmetic was the responsibility of the caller/user.
 *
 * I decided that the parser should be totally independent of the
 * numeric representation used, and thus the exact details of how the
 * arithmetic was performed. To accomplish this, four functions are
 * supplied by the user/caller:
 *
 * 1: trans_numeric - this function translates the text string into a
 * numeric in the desired representation and returns a pointer to the
 * representation as a (void *) this function has four parameters
 * passed:
 *
 *                     1: digit_str -- the actual text string of the
 *                     numeric to be converted to the internal
 *                     representation
 *
 *                     2: radix_point -- the ASCII character used to
 *                     represent the radix point
 *
 *                     3: group character -- the ASCII character used
 *                     to separate and group digits to the left of the
 *                     radix
 *
 *                     4: rstr -- a pointer to a location in which to
 *                     return a pointer to the first character not
 *                     part of the numeric string translated If this
 *                     pointer is NULL, do not return a value. This
 *                     parameter is the same as the second parameter
 *                     of the standard C library functions "strtod" or
 *                     "strtol"
 *
 * 2: numeric_ops - this function does the actual arithmetic on two
 * numeric quantities in internal representation. It has three
 * parameters passed:
 *
 *                     1: op_sym -- the numeric operation to be
 *                     performed. The possible values are defined
 *                     in "finvar.h" and are:
 *
 *                                 ADD_OP - addition
 *                                 SUB_OP - subtraction
 *                                 DIV_OP - division
 *                                 MUL_OP - multiplication
 *                                 ASN_OP - assignment
 *
 *                     2: left_value - the left hand operand of the
 *                     binary operator
 *
 *                     3: right_value - the right hand operand of
 *                     the binary operator Note: left_value and
 *                     right_value are passed as (void *). This
 *                     function is responsible for casting to the
 *                     proper type to use.  Note: this function should
 *                     make no assumptions about overwriting or
 *                     re-using either left_value or right_value,
 *                     except for ASN_OP. Both values passed must be
 *                     left unchanged by any operation except ASN_OP.
 *                     This function is also responsible for
 *                     allocating/freeing memory as necessary to
 *                     perform the designated function and returning
 *                     the result.  I STRONGLY suggest that the result
 *                     be returned in dynamically allocated memory. If
 *                     static memory is used, the parser has no means
 *                     of copying the returned result or managing
 *                     static memory to prevent overwriting the result
 *                     and invalidating the result.
 *
 * 3: negate_numeric - this function negates the value passed (as a (void *))
 *
 * 4: free_numeric - this function is responsible for freeing memory
 * used by the internal numeric representation.
 *
 * I have included the file "numeric_ops.c" containing the above
 * functions for the usual "double" and "int" representation of
 * numerics. The functions perform integer or floating point
 * operations as appropriate for the string entered by the user. The
 * division operation is done in "double" since I do not think that
 * anybody really wants (9 / 2) to equal 4 instead of 4.5 for
 * financial operations. These functions use the structure defined in
 * finvar.h:
 *
 * typedef struct numeric *numeric_ptr;
 * typedef struct numeric {
 *     char  type;
 *     union {
 *         long int  int_value;
 *         double    dbl_value;
 *     } value;
 * } numeric;
 *
 * to contain all numeric values. The variable "type" in this
 * structure can have the values:
 *
 *     INT_TYPE
 *     DBL_TYPE
 *
 * which are defined in "finvar.h".
 *
 * All "named variables", variables defined by the user for storing
 * intermediate results for future reference/use, and temporary
 * variables used by the parser use the variable storage structure,
 * var_store, defined above. The result of parsing and evaluating the
 * string passed are returned in a variable storage structure
 * specified by the caller.
 *
 * If the returned variable value is not named, i.e., "variable_name
 * == NULL", then the user/caller is responsible for freeing the
 * memory used by the internal representation of the numeric value.
 * If, however, "variable_name != NULL", freeing the memory used by
 * the internal numeric representation will cause a segmentation fault
 * later, when the parser attempts to free the memory through a call
 * to "free_numeric". In addition, freeing the memory will probably
 * invalidate the numeric value contained therein and lead to
 * pernicuous results when the value is used.
 *
 * If "variable_name != NULL", the user/caller should never attempt to
 * free this memory, that is the sole responsibility of the parser.
 *
 * It may be that the calling function has certain "variables" that
 * need to be "pre-defined" for the user to manipulate.  In essence
 * the function "pre-defining" variables sets up a linked list of
 * variable storage structures with the proper "names" and numeric
 * values. The number of "pre-defined" variables and a pointer to the
 * structure array is passed to the parser in the initialization
 * call. After the parser is eventually exited, the calling function
 * is responsible for freeing any memory used by the "pre-defined"
 * variables and their final numeric representation.
 *
 * A second design goal of the parser was that it should be callable
 * concurrently by multiple modules independently. That each module
 * should be capable of using differing "pre-defined" variables and
 * user defined variables and even internal numeric representations.
 * To that end the calling module must first initialize the parser
 * with a call to "init_parser".  This call creates the parser
 * internal structure for subsequent calls to the parser proper.  The
 * structure created and returned must then be passed to subsequent
 * calls to the parser.  When no further calls to the parser are to be
 * made, the module then calls "exit_parser" with the pointer returned
 * by "init_parser", so that the parser may release dynamically
 * allocated memory.
 *
 * The parser recognizes the following binary operators:
 *
 *      +
 *      -
 *      /
 *      *
 *      =
 *      +=
 *      -=
 *      /=
 *      *=
 *
 * In addition, the unary operators
 *
 *  +
 *  -
 *
 * are recognized. All numerics are initially recognized as positive
 * numbers. If negative, the unary '-' operator is applied. This saves
 * the logic of having to recogize strings as
 *
 *   -123
 *
 * The logic recognizes "-" and "123" separately. The '-' unary
 * operator is then applied to negate the numeric. This also has the
 * advanatge that the same logic can be used for
 *
 *  -123
 *  +123.45
 *  +uvar
 *  -uvar
 *
 *  In each case, the appropriate unary operator is applied to obtain
 *  the desired * result with no increase in the parsing logic. Thus
 *  keeping things as simple as possible.
 *
 * The parser also follows the C practice that the assignment
 * operators return a value. Thus, allowing multiple assignments and
 * assignment within expressions. The following expressions are all
 * valid:
 *
 *  nni = 123
 *  hnk = nni = 23.45
 *  jkl = 5 * (nj = 68.9)
 *
 * The first time variables are used in an expression, they are
 * initialized to zero, 0. Thus, even if the following variables have
 * not been assigned a value previously, the following expressions are
 * valid:
 *
 * nni *= 123
 *   above results in zero in nni
 * jk += 45.6
 *   above results in 45.6 in jk
 * 56.8 - tyh
 *   result of above is 56.8
 * tgh - 45.7
 *   above the same as
 * -45.7
 *
 * After parsing the above expressions the variables nni, jk, tyh and
 * tgh would all be defined.
 *
 * There are six parser functions needed to use the parser/evaluator:
 *
 * Note: in the last five functions, in the function paramter (void
 * *vp), "vp" is the pointer returned by the "init_parser" function.
 *
 * void *init_parser(var_store_ptr  predefined_vars,
 *                   char  radix_point,
 *                   char  group_char,
 *                   void          *trans_numeric(char  *digit_str,
 *                                                char   radix_point,
 *                                                char   group_char,
 *                                                char **rstr),
 *                   void          *numeric_ops(char  op_sym,
 *                                              void          *left_value,
 *                                              void          *right_value),
 *                   void          *negate_numeric(void *value),
 *                   void           free_numeric(void *numeric_value));
 *
 *         This function is called by the module/function/whatever to
 *         initialize the parser. The parser returns a pointer to a
 *         structure that contains all relevant information for
 *         parsering strings. The pointer is returned as (void *)
 *         since all information is and should remain pertinent only
 *         to the parser. The calling function(s) should never rely on
 *         manipulating any information inside this structure
 *         directly, since it may and could change in the future.  --
 *         The first parameter is a pointer to a the first element in
 *         a linked list of "pre-defined" variables the caller wishes
 *         to use with subsequent calls to the parser.  -- The second
 *         parameter is the radix character to use in numeric strings
 *         in subsequent calls to the parser.  -- the third parameter
 *         is the optional character used for grouping digits to the
 *         left of the radix.  -- The fourth, fifth, sixth and seventh
 *         parameters are the functions I descibed above for the
 *         internal numeric representation desired by the calling
 *         function(s).
 *
 * void                     exit_parser(
 *                                      void *vp);
 *
 *         This function is called to exit the parser and free all
 *         dynamically allocated memory used by the parser for an
 *         internal stack and user defined variables.
 *
 * unsigned                 get_parse_error(
 *                                          void *vp);
 *
 *         If the parser is successful in complete parsing and
 *         evaluating the string passed to 'parse_string' below, that
 *         functions returns a NULL pointer. If, however, an error is
 *         encountered in parsing/evaluating the string, the
 *         'parse_string' function returns a pointer to the character
 *         which caused the error.  This call returns an unsigned
 *         integer designating the error encountered. The possible
 *         values are defined in the "finvar.h" file.
 *
 * var_store_ptr            parser_get_vars(
 *                                          void *vp)
 *
 *         This function returns a pointer to the first element of a
 *         linked list of variable storage structures containing the
 *         user defined named variables if any exist.  NULL is
 *         returned if none exist. The calling function should not
 *         alter the variable names.  The numeric values may be
 *         altered if the calling function author really knows what
 *         they are doing.
 *
 * unsigned                 delete_var(
 *                                     char *var_name,
 *                                     void *vp);
 *
 *         This function will delete the user defined named variable
 *         with a name identical to the name string passed in the
 *         first parameter. If no user defined variable exists with an
 *         identical name, zero, 0, is returned. If the delete
 *         operation is successful, one, 1, is returned.
 *
 * char           *parse_string(
 *                                       var_store_ptr value,
 *                                       char *string,
 *                                       void *vp);
 *
 *         This function parses the string passed in the second
 *         parameter and returns a pointer to the last character not
 *         recognized upon a parsing error. If no error occurred, NULL
 *         is returned. The first parameter is a pointer to a variable
 *         storage structure to contain the result of the
 *         parser/evaluator.
 *
 * Note: The parser/evaluator uses a simple recursive descent
 * parser. I decided on this type for the simple reason that for a
 * simple four function calculator a recursive descent parser is, in
 * my opnion, the easiest to construct. I also think that recursive
 * descent parsers are easier for the human to understand and thus
 * maintain.
 *
 * Also, the parser uses a stack which is dynamically allocated in
 * memory and can grow as needed.  I have not provided any mechanism
 * for shrinking the stack. The initial stack size is set at 50
 * slots. I really do not anticipate that under normal and even most
 * extreme cases, that it will ever approach that size in actual
 * use. Under "normal" operation, the stack will probably never exceed
 * 3 or 4 slots in size and 50 slots is probably an overkill for
 * normal use. However, since the stack is pointers and not entire
 * structures, a stack size of 50 slots is not that much memory and
 * can be tolerated by most users. Thus, a mechanism for shrinking the
 * stack will probably never be needed.
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <glib.h>

#define EXPRESSION_PARSER_STATICS
#include "finvar.h"

/* structure to hold parser environment - environment particular to
 * each caller */
typedef struct parser_env
{
  unsigned stack_cnt;
  unsigned stack_size;
  var_store_ptr *stack;

  var_store_ptr predefined_vars;

  var_store_ptr named_vars;

  var_store_ptr unnamed_vars;

  const char *parse_str;
  char radix_point;
  char group_char;
  char name[50];

  char Token;
  char asn_op;

  char *tokens;
  char *token_tail;

  ParseError error_code;

  void *numeric_value;

  void *(*trans_numeric) (const char *digit_str,
			  char radix_point, char group_char, char **rstr);
  void *(*numeric_ops) (char op_sym, void *left_value, void *right_value);
  void *(*negate_numeric) (void *value);
  void (*free_numeric) (void *numeric_value);
}
parser_env;

#include "finproto.h"
#include "fin_static_proto.h"
#include "fin_spl_protos.h"

#define VAR_TOKEN 'V'
#define NUM_TOKEN 'I'

#define STACK_INIT 50

#define UNNAMED_VARS 100

#define NAMED_INCR 5

static char allowed_operators[] = "+-*/()=";

parser_env_ptr
init_parser (var_store_ptr predefined_vars,
	     char radix_point,
	     char group_char,
	     void *trans_numeric (const char *digit_str,
				  char radix_point,
				  char group_char,
				  char **rstr),
	     void *numeric_ops (char op_sym,
				void *left_value,
				void *right_value),
	     void *negate_numeric (void *value),
	     void free_numeric (void *numeric_value))
{
  parser_env_ptr pe = g_new0 (parser_env, 1);

  pe->predefined_vars = predefined_vars;

  pe->stack = g_new0 (var_store_ptr, STACK_INIT);
  pe->stack_size = STACK_INIT;

  pe->radix_point = radix_point;
  pe->group_char = group_char;

  pe->numeric_value = NULL;

  pe->trans_numeric = trans_numeric;
  pe->numeric_ops = numeric_ops;
  pe->negate_numeric = negate_numeric;
  pe->free_numeric = free_numeric;

  return pe;
}				/* init_parser */

void
exit_parser (parser_env_ptr pe)
{
  var_store_ptr vars, bv;

  if (pe == NULL)
    return;

  for (vars = pe->named_vars; vars; vars = bv)
  {
    g_free (vars->variable_name);
    vars->variable_name = NULL;

    if (vars->value)
      pe->free_numeric (vars->value);
    vars->value = NULL;

    bv = vars->next_var;
    g_free (vars);
  }				/* endfor */

  pe->named_vars = NULL;

  g_free (pe->stack);
  pe->stack = NULL;

  g_free (pe->tokens);
  pe->tokens = NULL;
  pe->token_tail = NULL;

  if (pe->numeric_value)
    pe->free_numeric (pe->numeric_value);
  pe->numeric_value = NULL;

  g_free (pe);
}				/* exit_parser */

/* return parser error code */
ParseError get_parse_error (parser_env_ptr pe)
{
  if (pe == NULL)
    return PARSER_NO_ERROR;

  return pe->error_code;
}				/* get_parse_error */

/* return linked list of named variables which have been defined */
var_store_ptr parser_get_vars (parser_env_ptr pe)
{
  if (pe == NULL)
    return NULL;

  return pe->named_vars;
}				/* get_vars */

/* function to delete variable with specified name from named variables
 * if it exists. If it exists return TRUE, 1, else return FALSE, 0 */
unsigned
delete_var (char *var_name, parser_env_ptr pe)
{
  unsigned ret = FALSE;
  var_store_ptr nv, tv;

  if (pe == NULL)
    return FALSE;

  for (nv = pe->named_vars, tv = NULL; nv; tv = nv, nv = nv->next_var)
  {
    if (strcmp (nv->variable_name, var_name) == 0)
    {
      if (tv)
	tv->next_var = nv->next_var;
      else
	pe->named_vars = nv->next_var;

      g_free (nv->variable_name);
      nv->variable_name = NULL;

      pe->free_numeric (nv->value);
      nv->value = NULL;

      g_free (nv);

      ret = TRUE;
      break;
    }				/* endif */
  }				/* endfor */

  return ret;
}				/* delete_var */

/* parse string passed using parser environment passed return
 * evaluated value in numeric structure passed, return NULL if no
 * parse error. If parse error, return pointer to character at which
 * error occured. */
char *
parse_string (var_store_ptr value, const char *string, parser_env_ptr pe)
{
  var_store_ptr retv;
  var_store unnamed_vars[UNNAMED_VARS];

  if (!pe || !string)
    return NULL;

  pe->unnamed_vars = unnamed_vars;
  memset (unnamed_vars, 0, UNNAMED_VARS * sizeof (var_store));

  pe->parse_str = string;
  pe->error_code = PARSER_NO_ERROR;

  g_free (pe->tokens);
  pe->tokens = g_new0(char, strlen (string) + 1);
  pe->token_tail = pe->tokens;

  next_token (pe);

  if (!pe->error_code)
    assignment_op (pe);

  if (!pe->error_code)
  {
    /* interpret (num) as -num */
    if (strcmp (pe->tokens, "(I)") == 0)
    {
      var_store_ptr val;

      val = pop (pe);
      pe->negate_numeric (val->value);
      push (val, pe);
    }
  }

  if (pe->Token == EOS)
  {
    if ((pe->stack_cnt) && (retv = pop (pe)))
    {
      if (value != NULL)
	*value = *retv;
      pe->parse_str = NULL;
    }
    else
      pe->error_code = STACK_UNDERFLOW;
  }

  pe->stack_cnt = 0;
  pe->unnamed_vars = NULL;

  return (char *) pe->parse_str;
}				/* expression */

/* pop value off value stack */
static var_store_ptr
pop (parser_env_ptr pe)
{
  var_store_ptr val;

  if (pe->stack_cnt)
    val = pe->stack[--(pe->stack_cnt)];
  else
  {
    val = NULL;
    pe->error_code = STACK_UNDERFLOW;
  }				/* endif */

  return val;
}				/* pop */

/* push value onto value stack */
static var_store_ptr
push (var_store_ptr push_value, parser_env_ptr pe)
{
  if (pe->stack_cnt > pe->stack_size)
  {
    pe->stack_size += STACK_INIT;
    pe->stack = g_realloc (pe->stack,
			   pe->stack_size * sizeof (var_store_ptr));
  }				/* endif */

  pe->stack[(pe->stack_cnt)++] = push_value;

  return push_value;
}				/* push */

/* get/set variable with specified name - nothing fancy just scan each
 * variable in linked list checking for a string match return variable
 * found if match create new variable if none found */
static var_store_ptr
get_named_var (parser_env_ptr pe)
{
  var_store_ptr retp = NULL, bv;

  for (retp = pe->predefined_vars, bv = NULL; retp; retp = retp->next_var)
    if (strcmp (retp->variable_name, pe->name) == 0)
      break;

  if (!retp && pe->named_vars)
    for (retp = pe->named_vars; retp; bv = retp, retp = retp->next_var)
      if (strcmp (retp->variable_name, pe->name) == 0)
	break;

  if (!retp)
  {
    retp = g_new0 (var_store, 1);
    if (!pe->named_vars)
      pe->named_vars = retp;
    else
      bv->next_var = retp;
    retp->variable_name = g_strdup (pe->name);
    retp->value =
      pe->trans_numeric ("0", pe->radix_point, pe->group_char, NULL);
  }

  return retp;
}				/* get_var */

/* get un-named temporary variable */
static var_store_ptr
get_unnamed_var (parser_env_ptr pe)
{
  var_store_ptr retp = NULL;
  unsigned cntr;

  for (cntr = 0; cntr < UNNAMED_VARS; cntr++)
    if (pe->unnamed_vars[cntr].use_flag == UNUSED_VAR)
    {
      retp = &(pe->unnamed_vars[cntr]);
      retp->variable_name = NULL;
      retp->use_flag = USED_VAR;
      if (retp->value)
      {
	pe->free_numeric (retp->value);
        retp->value = NULL;
      }				/* endif */
      break;
    }				/* endif */

  if (retp == NULL)
    pe->error_code = PARSER_OUT_OF_MEMORY;

  return retp;
}				/* get_unnamed_var */

/* mark un-named temporary variable unused */
static void
free_var (var_store_ptr value, parser_env_ptr pe)
{
  if (value == NULL)
    return;

  /* first check that not a named variable */
  if (value->variable_name != NULL)
    return;

  value->use_flag = UNUSED_VAR;

  if (value->value)
  {
    pe->free_numeric (value->value);
    value->value = NULL;
  }
}				/* free_var */

static void
add_token (parser_env_ptr pe, char token)
{
  *pe->token_tail = pe->Token = token;
  pe->token_tail++;
}

/* parse next token from string */
static void
next_token (parser_env_ptr pe)
{
  char *nstr;
  const char *str_parse = pe->parse_str;
  void *number;

  while (isspace (*str_parse))
    str_parse++;

  pe->asn_op = EOS;

  /* test for end of string */
  if (!*str_parse)
  {
    add_token (pe, EOS);
  }
  /* test for name */
  else if (isalpha (*str_parse) || (*str_parse == '_'))
  {
    add_token (pe, VAR_TOKEN);
    nstr = pe->name;
    do
    {
      *nstr++ = *str_parse++;
    }
    while ((*str_parse == '_') ||
           isalpha (*str_parse) ||
           isdigit (*str_parse));

    *nstr = EOS;
  }
  /* test for possible operator */
  else if (strchr (allowed_operators, *str_parse))
  {
    add_token (pe, *str_parse++);
    if (*str_parse == ASN_OP)
    {
      if (pe->Token != ASN_OP)
      {
        str_parse++;
        pe->asn_op = pe->Token;
        add_token (pe, ASN_OP);
      }
      else
        pe->error_code = UNDEFINED_CHARACTER;
    }				/* endif */
  }
  /* test for numeric token */
  else if ((number = pe->trans_numeric (str_parse, pe->radix_point,
                                        pe->group_char, &nstr)))
  {
    add_token (pe, NUM_TOKEN);
    pe->numeric_value = number;
    str_parse = nstr;
  }
  /* unrecognized character - error */
  else
  {
    add_token (pe, *str_parse);
    pe->error_code = UNDEFINED_CHARACTER;
  }				/* endif */

  pe->parse_str = str_parse;
}				/* next_token */

/* evaluate assignment operators,
 * =
 * +=
 * -=
 * \=
 * *=
 */
static void
assignment_op (parser_env_ptr pe)
{
  var_store_ptr vl;		/* left value       */
  var_store_ptr vr;		/* right value      */
  char ao;

  add_sub_op (pe);
  if (pe->error_code)
    return;

  while (pe->Token == ASN_OP)
  {
    vl = pop (pe);
    if (pe->error_code)
      return;

    ao = pe->asn_op;

    if (vl->variable_name)
    {
      next_token (pe);
      if (pe->error_code)
      {
        free_var (vl, pe);
        return;
      }

      assignment_op (pe);
      if (pe->error_code)
      {
        free_var (vl, pe);
        return;
      }

      vr = pop (pe);
      if (pe->error_code)
      {
        free_var (vl, pe);
        return;
      }

      vl->assign_flag = ASSIGNED_TO;

      if (ao)
      {
        void *temp;

	temp = vl->value;
	vl->value = pe->numeric_ops (ao, vl->value, vr->value);
	pe->free_numeric (temp);
      }
      else if (vl != vr)
      {
	if (!vr->variable_name)
	{
	  pe->free_numeric (vl->value);
	  vl->value = vr->value;
	  vr->value = NULL;
	}
	else
	  pe->numeric_ops (ASN_OP, vl->value, vr->value);

	free_var (vr, pe);
      }				/* endif */

      push (vl, pe);
    }
    else
    {
      add_token (pe, EOS);	/* error !!!!!!!!!! */
      pe->error_code = NOT_A_VARIABLE;
      free_var (vl, pe);
    }				/* endif */
  }				/* endwhile */
}				/* assignment_op */

/* evaluate addition, subtraction operators */
static void
add_sub_op (parser_env_ptr pe)
{
  var_store_ptr vl;	/* left value   */
  var_store_ptr vr;	/* right value  */
  var_store_ptr rslt;   /* result       */
  char op;

  multiply_divide_op (pe);
  if (pe->error_code)
    return;

  while ((pe->Token == ADD_OP) || (pe->Token == SUB_OP))
  {
    op = pe->Token;

    vl = pop (pe);
    if (pe->error_code)
      return;

    next_token (pe);
    if (pe->error_code)
    {
      free_var (vl, pe);
      return;
    }

    multiply_divide_op (pe);
    if (pe->error_code)
    {
      free_var (vl, pe);
      return;
    }

    vr = pop (pe);
    if (pe->error_code)
    {
      free_var (vl, pe);
      return;
    }

    rslt = get_unnamed_var (pe);
    if (pe->error_code)
    {
      free_var (vl, pe);
      free_var (vr, pe);
      return;
    }

    rslt->value = pe->numeric_ops (op, vl->value, vr->value);

    free_var (vl, pe);
    free_var (vr, pe);

    push (rslt, pe);
  }				/* endwhile */
}				/* add_sub_op */

/* evaluate multiplication, division operators */
static void
multiply_divide_op (parser_env_ptr pe)
{
  var_store_ptr vl;	/* left value   */
  var_store_ptr vr;	/* right value  */
  var_store_ptr rslt;   /* result       */
  char op;

  primary_exp (pe);
  if (pe->error_code)
    return;

  while ((pe->Token == MUL_OP) || (pe->Token == DIV_OP))
  {
    op = pe->Token;

    vl = pop (pe);
    if (pe->error_code)
      return;

    next_token (pe);
    if (pe->error_code)
    {
      free_var (vl, pe);
      return;
    }

    primary_exp (pe);
    if (pe->error_code)
    {
      free_var (vl, pe);
      return;
    }

    vr = pop (pe);
    if (pe->error_code)
    {
      free_var (vl, pe);
      return;
    }

    rslt = get_unnamed_var (pe);
    if (pe->error_code)
    {
      free_var (vl, pe);
      free_var (vr, pe);
      return;
    }

    rslt->value = pe->numeric_ops (op, vl->value, vr->value);

    free_var (vl, pe);
    free_var (vr, pe);

    push (rslt, pe);
  }				/* endwhile */
}				/* multiply_divide_op */

/* evaluate:
 *  unary '+' and '-'
 *  named variables
 *  numerics
 *  grouped expressions, "()"
 */
static void
primary_exp (parser_env_ptr pe)
{
  var_store_ptr rslt = NULL;
  char LToken = pe->Token;

  next_token (pe);
  if (pe->error_code)
    return;

  switch (LToken)
  {
    case '(':
      /*add_sub_op(pe);   */
      assignment_op (pe);
      if (pe->error_code)
        return;

      if (pe->Token == ')')
      {
        rslt = pop (pe);
        if (pe->error_code)
          return;

        next_token (pe);
        if (pe->error_code)
          return;
      }
      else
      {
        add_token (pe, EOS);	/* error here */
        pe->error_code = UNBALANCED_PARENS;
      }				/* endif */

      break;

    case ADD_OP:
    case SUB_OP:
      primary_exp (pe);
      if (pe->error_code)
        return;

      rslt = pop (pe);
      if (pe->error_code)
        return;

      if (LToken == SUB_OP)
        pe->negate_numeric (rslt->value);

      break;

    case NUM_TOKEN:
      rslt = get_unnamed_var (pe);
      if (pe->error_code)
        return;

      rslt->value = pe->numeric_value;
      pe->numeric_value = NULL;
      break;

    case VAR_TOKEN:
      rslt = get_named_var (pe);
      break;
  }				/* endswitch */

  if (rslt != NULL)
    push (rslt, pe);
}				/* primary_exp */
