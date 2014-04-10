/********************************************************************\
 * argv-list-converters.h                                           *
 * Copyright (C) 2000 Gnumatic, Inc                                 *
 * Copyright (C) 2000 James LewisMoss                               *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef ARGV_LIST_CONVERTERS_H
#define ARGV_LIST_CONVERTERS_H


/*
 * This function takes a SCM value.  Determines whether it is a list
 * and whether that list contains only strings and returns a null
 * terminated array of strings (char*'s)
 */
char** gnc_scheme_list_to_nulltermcharpp(int prelen, const char **prepend,
                                         SCM list);


/*
 * This function takes a length and char** and makes a scheme list
 * with similar contents
 */
SCM gnc_argvarr_to_scheme_list(int argc, const char** argv);

/*
 * Frees the strings and the argv array
 */
void gnc_free_argv(char** argv);

/*
 * print out the argv array in a nice manner
 */
void print_argv(char **argv);

/*
 * get the length of null terminated char* array
 */
int argv_length(char** nulltermlist);

#endif
