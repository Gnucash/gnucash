/********************************************************************\
 * qofquerycore-p.h -- Private API for providing core Query data types *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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
 *                                                                  *
\********************************************************************/

#ifndef QOF_QUERYCOREP_H
#define QOF_QUERYCOREP_H

#include <sys/types.h>
#include <time.h>
#include <regex.h>
#include <string.h>

#include "qofquerycore.h"

/* Initalize the Query Core registry and install the default type handlers */
void qof_query_core_init(void);
void qof_query_core_shutdown (void);

/*
 * An arbitrary Query Predicate.  Given the object and the
 * particular parameter get-function (obtained from the registry by
 * the Query internals), compare the object's parameter to the
 * predicate data.
 */
typedef gint (*QofQueryPredicateFunc) (gpointer object,
                                       QofParam *getter,
                                       QofQueryPredData *pdata);

/* Lookup functions */
QofQueryPredicateFunc qof_query_core_get_predicate (gchar const *type);
QofCompareFunc qof_query_core_get_compare (gchar const *type);

/* Compare two predicates */
gboolean qof_query_core_predicate_equal (const QofQueryPredData *p1, const QofQueryPredData *p2);

/* Predicate Data Structures:
 *
 * These are defined such that you can cast between these types and
 * a QofQueryPredData.
 *
 * Note that these are provided for READ ONLY PURPOSES.  You should NEVER
 * write into these structures, change them, or use them to create a
 * Query.
 */

typedef struct
{
    QofQueryPredData	pd;
    QofStringMatch	options;
    gboolean		is_regex;
    gchar *		matchstring;
    regex_t		compiled;
} query_string_def, *query_string_t;

typedef struct
{
    QofQueryPredData	pd;
    QofDateMatch	options;
    Timespec	date;
} query_date_def, *query_date_t;

typedef struct
{
    QofQueryPredData	pd;
    QofNumericMatch	options;
    gnc_numeric		amount;
} query_numeric_def, *query_numeric_t;

typedef struct
{
    QofQueryPredData	pd;
    QofGuidMatch	options;
    GList *	guids;
} query_guid_def, *query_guid_t;

typedef struct
{
    QofQueryPredData	pd;
    gint32	val;
} query_int32_def, *query_int32_t;

typedef struct
{
    QofQueryPredData	pd;
    gint64	val;
} query_int64_def, *query_int64_t;

typedef struct
{
    QofQueryPredData	pd;
    double	val;
} query_double_def, *query_double_t;

typedef struct
{
    QofQueryPredData	pd;
    gboolean	val;
} query_boolean_def, *query_boolean_t;

typedef struct
{
    QofQueryPredData	pd;
    QofCharMatch	options;
    gchar *	char_list;
} query_char_def, *query_char_t;

typedef struct
{
    QofQueryPredData	pd;
    GSList *	path;
    KvpValue *	value;
} query_kvp_def, *query_kvp_t;

typedef struct
{
    QofQueryPredData pd;
    QofGuidMatch  options;
    QofCollection *coll;
    GList *guids;
} query_coll_def, *query_coll_t;

typedef struct
{
    QofQueryPredData pd;
    QofGuidMatch options;
    const GncGUID *guid;
    GList * guids;
} query_choice_def, *query_choice_t;

#endif /* QOF_QUERYCOREP_H */
