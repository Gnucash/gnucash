/*
 * qif-parse.h -- routines for parsing pieces of a QIF file
 *
 * Written By:	Derek Atkins  <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef QIF_PARSE_H
#define QIF_PARSE_H

#include "qif-import.h"

void qif_register_handler(QifType type, QifHandler handler);
void qif_parse_bangtype(QifContext ctx, const char *line);

gboolean
qif_parse_split_category(const char* str,
                         char** cat, gboolean *cat_is_acct, char** cat_class,
                         char** miscx_cat, gboolean *miscx_cat_is_acct,
                         char **miscx_class);

gboolean qif_parse_numeric(QifLine line, gnc_numeric *num);
QifRecnFlag qif_parse_cleared(QifLine line);
QifAction qif_parse_action(QifLine line);

/* The caller should never destroy this list */
GList * qif_parse_acct_type(const char *str, gint lineno);
GList * qif_parse_acct_type_guess(QifType type);

/* Parse all objects */
void qif_parse_all(QifContext ctx, gpointer ui_args);

#endif /* QIF_PARSE_H */
