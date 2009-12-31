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

#include <glib.h>
#include <glib/gi18n.h>
#include <libguile.h>
#include <ctype.h>
#include <locale.h>
#include <string.h>

#include "gfec.h"
#include "finproto.h"
#include "fin_spl_protos.h"
#include "gnc-filepath-utils.h"
#include "gnc-gkeyfile-utils.h"
#include "gnc-exp-parser.h"
#include "gnc-ui-util.h"
#include "guile-mappings.h"

#define GROUP_NAME "Variables"

static QofLogModule log_module = GNC_MOD_GUI;

/** Data Types *****************************************************/

typedef struct ParserNum
{
    gnc_numeric value;
} ParserNum;


/** Static Globals *************************************************/
static GHashTable   *variable_bindings = NULL;
static ParseError    last_error        = PARSER_NO_ERROR;
static GNCParseError last_gncp_error   = NO_ERR;
static gboolean      parser_inited     = FALSE;


/** Implementations ************************************************/

static gchar *
gnc_exp_parser_filname (void)
{
    return gnc_build_dotgnucash_path("expressions-2.0");
}

void
gnc_exp_parser_init ( void )
{
    gnc_exp_parser_real_init( TRUE );
}

void
gnc_exp_parser_real_init ( gboolean addPredefined )
{
    gchar *filename, **keys, **key, *str_value;
    GKeyFile *key_file;
    gnc_numeric value;

    if (parser_inited)
        gnc_exp_parser_shutdown ();

    variable_bindings = g_hash_table_new (g_str_hash, g_str_equal);

    /* This comes after the statics have been initialized. Not at the end! */
    parser_inited = TRUE;

    if ( addPredefined )
    {
        filename = gnc_exp_parser_filname();
        key_file = gnc_key_file_load_from_file(filename, TRUE, FALSE, NULL);
        if (key_file)
        {
            keys = g_key_file_get_keys(key_file, GROUP_NAME, NULL, NULL);
            for (key = keys; key && *key; key++)
            {
                str_value = g_key_file_get_string(key_file, GROUP_NAME, *key, NULL);
                if (str_value && string_to_gnc_numeric(str_value, &value))
                {
                    gnc_exp_parser_set_value (*key, gnc_numeric_reduce (value));
                }
            }
            g_strfreev(keys);
            g_key_file_free(key_file);
        }
        g_free(filename);
    }
}

static gboolean
remove_binding (gpointer key, gpointer value, gpointer not_used)
{
    g_free(key);
    g_free(value);

    return TRUE;
}

static void
set_one_key (gpointer key, gpointer value, gpointer data)
{
    char *name = key;
    ParserNum *pnum = value;
    char *num_str;

    num_str = gnc_numeric_to_string (gnc_numeric_reduce (pnum->value));
    g_key_file_set_string ((GKeyFile *)data, GROUP_NAME, name, num_str);
    g_free (num_str);
}

void
gnc_exp_parser_shutdown (void)
{
    GKeyFile* key_file;
    gchar *filename;

    if (!parser_inited)
        return;

    filename = gnc_exp_parser_filname();
    key_file = g_key_file_new();
    g_hash_table_foreach (variable_bindings, set_one_key, key_file);
    g_key_file_set_comment(key_file, GROUP_NAME, NULL,
                           " Variables are in the form 'name=value'",
                           NULL);
    gnc_key_file_save_to_file(filename, key_file, NULL);
    g_key_file_free(key_file);
    g_free(filename);

    g_hash_table_foreach_remove (variable_bindings, remove_binding, NULL);
    g_hash_table_destroy (variable_bindings);
    variable_bindings = NULL;

    last_error = PARSER_NO_ERROR;
    last_gncp_error = NO_ERR;

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
gnc_exp_parser_get_value (const char * variable_name, gnc_numeric *value_p)
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
gnc_exp_parser_set_value (const char * variable_name, gnc_numeric value)
{
    char *key;
    ParserNum *pnum;

    if (variable_name == NULL)
        return;

    if (!parser_inited)
        gnc_exp_parser_init ();

    gnc_exp_parser_remove_variable (variable_name);

    key = g_strdup (variable_name);

    pnum = g_new0(ParserNum, 1);
    pnum->value = value;

    g_hash_table_insert (variable_bindings, key, pnum);
}

static void
make_predefined_vars_helper (gpointer key, gpointer value, gpointer data)
{
    var_store_ptr *vars_p = data;
    ParserNum *pnum_old = value;
    var_store_ptr var;
    ParserNum *pnum;

    var = g_new0 (var_store, 1);

    pnum = g_new0 (ParserNum, 1);
    *pnum = *pnum_old;

    var->variable_name = g_strdup(key);
    var->value = pnum;
    var->next_var = *vars_p;

    *vars_p = var;
}

static void
make_predefined_vars_from_external_helper( gpointer key, gpointer value, gpointer data )
{
    ParserNum *pnum = g_new0( ParserNum, 1 );
    if ( value != NULL )
        pnum->value = *(gnc_numeric*)value;

    make_predefined_vars_helper( key, pnum, data );
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

        g_free(vars->variable_name);
        vars->variable_name = NULL;

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

static char* _function_evaluation_error_msg = NULL;
static void
_exception_handler(const char *error_message)
{
    _function_evaluation_error_msg = (char*)error_message;
}

static
void*
func_op(const char *fname, int argc, void **argv)
{
    SCM scmFn, scmArgs, scmTmp;
    int i;
    var_store *vs;
    gchar *str;
    gnc_numeric n, *result;
    GString *realFnName;

    realFnName = g_string_sized_new( strlen(fname) + 5 );
    g_string_printf( realFnName, "gnc:%s", fname );
    scmFn = scm_internal_catch(SCM_BOOL_T,
                               (scm_t_catch_body)scm_c_eval_string, realFnName->str,
                               scm_handle_by_message_noexit, NULL);
    g_string_free( realFnName, TRUE );
    if (!scm_is_procedure(scmFn))
    {
        /* FIXME: handle errors correctly. */
        printf( "gnc:\"%s\" is not a scm procedure\n", fname );
        return NULL;
    }
    scmArgs = scm_listify( SCM_UNDEFINED );
    for ( i = 0; i < argc; i++ )
    {
        /* cons together back-to-front. */
        vs = (var_store*)argv[argc - i - 1];
        switch ( vs->type )
        {
        case VST_NUMERIC:
            n = *(gnc_numeric*)(vs->value);
            scmTmp = scm_make_real( gnc_numeric_to_double( n ) );
            break;
        case VST_STRING:
            str = (char*)(vs->value);
            scmTmp = scm_mem2string( str, strlen(str) );
            break;
        default:
            /* FIXME: error */
            printf( "argument %d not a numeric or string [type = %d]\n",
                    i, vs->type );
            return NULL;
            break; /* notreached */
        }
        scmArgs = scm_cons( scmTmp, scmArgs );
    }

    //scmTmp = scm_apply(scmFn, scmArgs , SCM_EOL);
    scmTmp = gfec_apply(scmFn, scmArgs, _exception_handler);
    if (_function_evaluation_error_msg != NULL)
    {
        PERR("function eval error: [%s]\n", _function_evaluation_error_msg);
        _function_evaluation_error_msg = NULL;
        return NULL;
    }

    result = g_new0( gnc_numeric, 1 );
    *result = double_to_gnc_numeric( scm_num2dbl(scmTmp, G_STRFUNC),
                                     GNC_DENOM_AUTO,
                                     GNC_DENOM_SIGFIGS(6) | GNC_RND_ROUND );
    /* FIXME: cleanup scmArgs = scm_list, cons'ed cells? */
    return (void*)result;
}

static void *
trans_numeric(const char *digit_str,
              gchar      *radix_point,
              gchar      *group_char,
              char      **rstr)
{
    ParserNum *pnum;
    gnc_numeric value;

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

    result = (op_sym == ASN_OP) ? left : g_new0(ParserNum, 1);

    switch (op_sym)
    {
    case ADD_OP:
        result->value = gnc_numeric_add (left->value, right->value,
                                         GNC_DENOM_AUTO, GNC_DENOM_EXACT);
        break;
    case SUB_OP:
        result->value = gnc_numeric_sub (left->value, right->value,
                                         GNC_DENOM_AUTO, GNC_DENOM_EXACT);
        break;
    case DIV_OP:
        result->value = gnc_numeric_div (left->value, right->value,
                                         GNC_DENOM_AUTO, GNC_DENOM_EXACT);
        break;
    case MUL_OP:
        result->value = gnc_numeric_mul (left->value, right->value,
                                         GNC_DENOM_AUTO, GNC_DENOM_EXACT);
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

    result->value = gnc_numeric_neg (result->value);

    return result;
}

static
void
gnc_ep_tmpvarhash_check_vals( gpointer key, gpointer value, gpointer user_data )
{
    gboolean *allVarsHaveValues = (gboolean*)user_data;
    gnc_numeric *num = (gnc_numeric*)value;
    *allVarsHaveValues &= ( num && gnc_numeric_check( *num ) != GNC_ERROR_ARG );
}

static
void
gnc_ep_tmpvarhash_clean( gpointer key, gpointer value, gpointer user_data )
{
    if ( key )
    {
        g_free( (gchar*)key );
    }
    if ( value )
    {
        g_free( (gnc_numeric*)value );
    }
}

gboolean
gnc_exp_parser_parse( const char * expression, gnc_numeric *value_p,
                      char **error_loc_p )
{
    GHashTable *tmpVarHash;
    gboolean ret, toRet = TRUE;
    gboolean allVarsHaveValues = TRUE;

    tmpVarHash = g_hash_table_new( g_str_hash, g_str_equal );
    ret = gnc_exp_parser_parse_separate_vars( expression, value_p,
            error_loc_p, tmpVarHash );
    if ( !ret )
    {
        toRet = ret;
        goto cleanup;
    }

    g_hash_table_foreach( tmpVarHash,
                          gnc_ep_tmpvarhash_check_vals,
                          &allVarsHaveValues );
    if ( !allVarsHaveValues )
    {
        toRet = FALSE;
        last_gncp_error = VARIABLE_IN_EXP;
    }

cleanup:
    g_hash_table_foreach( tmpVarHash, gnc_ep_tmpvarhash_clean, NULL );
    g_hash_table_destroy( tmpVarHash );

    return toRet;
}

gboolean
gnc_exp_parser_parse_separate_vars (const char * expression,
                                    gnc_numeric *value_p,
                                    char **error_loc_p,
                                    GHashTable *varHash )
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
        gnc_exp_parser_real_init ( (varHash == NULL) );

    result.variable_name = NULL;
    result.value = NULL;
    result.next_var = NULL;

    vars = make_predefined_variables ();

    if ( varHash != NULL )
    {
        g_hash_table_foreach( varHash, make_predefined_vars_from_external_helper, &vars);
    }

    lc = gnc_localeconv ();

    pe = init_parser (vars, lc->mon_decimal_point, lc->mon_thousands_sep,
                      trans_numeric, numeric_ops, negate_numeric, g_free,
                      func_op);

    error_loc = parse_string (&result, expression, pe);

    pnum = result.value;

    if (error_loc == NULL)
    {
        if (gnc_numeric_check (pnum->value))
        {
            if (error_loc_p != NULL)
                *error_loc_p = (char *) expression;

            last_error = NUMERIC_ERROR;
        }
        else
        {
            if (pnum)
            {
                if (value_p)
                    *value_p = gnc_numeric_reduce (pnum->value);

                if (!result.variable_name)
                    g_free (pnum);
            }

            if (error_loc_p != NULL)
                *error_loc_p = NULL;

            last_error = PARSER_NO_ERROR;
        }
    }
    else
    {
        if (error_loc_p != NULL)
            *error_loc_p = error_loc;

        last_error = get_parse_error (pe);
    }

    if ( varHash != NULL )
    {
        var_store_ptr newVars;
        gpointer maybeKey, maybeValue;
        gnc_numeric *numericValue;

        newVars = parser_get_vars( pe );
        for ( ; newVars ; newVars = newVars->next_var )
        {
            pnum = newVars->value;
            if ( g_hash_table_lookup_extended( varHash, newVars->variable_name,
                                               &maybeKey, &maybeValue ) )
            {
                g_hash_table_remove( varHash, maybeKey );
                g_free( maybeKey );
                g_free( maybeValue );
            }
            numericValue = g_new0( gnc_numeric, 1 );
            *numericValue = ((ParserNum*)newVars->value)->value;
            // WTF?
            // numericValue = NULL;
            g_hash_table_insert( varHash,
                                 g_strdup(newVars->variable_name),
                                 numericValue );
        }
    }
    else
    {
        update_variables (vars);
    }

    free_predefined_variables (vars);

    exit_parser (pe);

    return last_error == PARSER_NO_ERROR;
}

const char *
gnc_exp_parser_error_string (void)
{
    if ( last_error == PARSER_NO_ERROR )
    {
        switch ( last_gncp_error )
        {
        default:
        case NO_ERR:
            return NULL;
            break;
        case VARIABLE_IN_EXP:
            return _("Illegal variable in expression." );
            break;
        }
    }

    switch (last_error)
    {
    default:
    case PARSER_NO_ERROR:
        return NULL;
    case UNBALANCED_PARENS:
        return _("Unbalanced parenthesis");
    case STACK_OVERFLOW:
        return _("Stack overflow");
    case STACK_UNDERFLOW:
        return _("Stack underflow");
    case UNDEFINED_CHARACTER:
        return _("Undefined character");
    case NOT_A_VARIABLE:
        return _("Not a variable");
    case NOT_A_FUNC:
        return _("Not a defined function");
    case PARSER_OUT_OF_MEMORY:
        return _("Out of memory");
    case NUMERIC_ERROR:
        return _("Numeric error");
    }
}
