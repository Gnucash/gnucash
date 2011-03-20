/********************************************************************\
 * gnc-exp-parser.h -- Interface to expression parsing for GnuCash  *
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

#ifndef GNC_EXP_PARSER_H
#define GNC_EXP_PARSER_H

#include <glib.h>

#include "qof.h"

/**
 * The errors which can be determined at the gnc-exp-parser level.
 **/
typedef enum
{
    NO_ERR,
    VARIABLE_IN_EXP,
    NUM_ERRORS
} GNCParseError;

/* Initialize the expression parser. If this function is not
 * called before one of the other parsing routines (other than
 * gnc_exp_parser_shutdown), it will be called if needed.
 */
void gnc_exp_parser_init (void);

/**
 * The real init function, which takes an option to add the pre-defined vars
 * to the variable table.  This option is used by
 * gnc_exp_parser_parse_seperate_vars [itself used by the Scheduled
 * Transaction code] to parse without using any "predefined" application
 * variables.
 **/
void gnc_exp_parser_real_init( gboolean addPredefined );

/* Shutdown the expression parser and free any associated memory in the ParserState. */
void gnc_exp_parser_shutdown (void);

/* Return a list of variable names which are currently defined
 * in the parser. The names should not be modified or freed. */
GList * gnc_exp_parser_get_variable_names (void);

/* Undefine the variable name if it is already defined. */
void gnc_exp_parser_remove_variable (const char *variable_name);

/* Undefine every variable name appearing in the list. Variables in
 * the list which are not defined are ignored. */
void gnc_exp_parser_remove_variable_names (GList * variable_names);

/* Return TRUE if the variable is defined, FALSE otherwise. If defined
 * and value_p != NULL, return the value in *value_p, otherwise, *value_p
 * is unchanged. */
gboolean gnc_exp_parser_get_value (const char * variable_name,
                                   gnc_numeric *value_p);

/* Set the value of the variable to the given value. If the variable is
 * not already defined, it will be after the call. */
void gnc_exp_parser_set_value (const char * variable_name,
                               gnc_numeric value);

/* Parse the given expression using the current variable definitions.
 * If the parse was successful, return TRUE and, if value_p is
 * non-NULL, return the value of the resulting expression in *value_p.
 * Otherwise, return FALSE and *value_p is unchanged. If FALSE is
 * returned and error_loc_p is non-NULL, *error_loc_p is set to the
 * character in expression where parsing aborted. If TRUE is returned
 * and error_loc_p is non-NULL, *error_loc_p is set to NULL. */
gboolean gnc_exp_parser_parse (const char * expression,
                               gnc_numeric *value_p,
                               char **error_loc_p );

/**
 * Parses as per gnc_exp_parser_parse, but with an optional last argument
 * dealing with a non-shared variable list and state, local to the expression
 * being parsed.  This is a hashTable of variable names mapping to
 * gnc_numeric pointers.
 *
 * @note It is the CALLER'S RESPONSIBILITY to g_free() both the keys and
 * values of varHash when done.
 **/
gboolean gnc_exp_parser_parse_separate_vars (const char * expression,
        gnc_numeric *value_p,
        char **error_loc_p,
        GHashTable *varHash );

/* If the last parse returned FALSE, return an error string describing
 * the problem. Otherwise, return NULL. */
const char * gnc_exp_parser_error_string (void);

#endif
