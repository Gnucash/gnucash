/***************************************************************************
 *              -------------------
 *    create   : Tue Jul 11 20:21:18 2000
 *    copyright: (C) 2000 by Terry D. Boldt
 *    email    : tboldt@attglobal.net
 *              -------------------
 ***************************************************************************/
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
/***************************************************************************
 *  Global Function Prototypes
 *  Tue Jul 11 20:21:18 2000
 *
 ***************************************************************************/

#ifndef NUMERIC_OPS_H
#define NUMERIC_OPS_H

void *trans_numeric(const char *str, /* pointer to string to translate */
                    char radix_point, /* radix character */
                    char group_char, /* grouping character to left of radix */
                    char **endstr); /* where to return pointer to first
                                     * unrecognized character */

void free_numeric(void *numeric_value);

void *negate_numeric(void *value);

void *numeric_ops(char op_symbol,
                  void *l_value,
                  void *r_value);

#endif
