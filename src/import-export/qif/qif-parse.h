/*
 * qif-parse.h -- routines for parsing pieces of a QIF file
 *
 * Written By:	Derek Atkins  <derek@ihtfp.com>
 *
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

#endif /* QIF_PARSE_H */
