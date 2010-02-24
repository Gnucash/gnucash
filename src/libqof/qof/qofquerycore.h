/********************************************************************\
 * qofquerycore.h -- API for providing core Query data types        *
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

/** @addtogroup Query
    @{ */

/** @file qofquerycore.h
    @brief API for providing core Query data types
    @author Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
*/

#ifndef QOF_QUERYCORE_H
#define QOF_QUERYCORE_H

#include "gnc-numeric.h"
#include "gnc-date.h"
#include "kvp_frame.h"
#include "qofclass.h"

/**
 * PREDICATE DATA TYPES: All the predicate data types are rolled up into
 * the union type PredicateData.  The "type" field specifies which type
 * the union is.
 */
typedef struct _QofQueryPredData QofQueryPredData;

/** Standard Query comparitors, for how to compare objects in a predicate.
 *  Note that not all core types implement all comparitors
 */
typedef enum
{
    QOF_COMPARE_LT = 1,
    QOF_COMPARE_LTE,
    QOF_COMPARE_EQUAL,
    QOF_COMPARE_GT,
    QOF_COMPARE_GTE,
    QOF_COMPARE_NEQ
} QofQueryCompare;

/** List of known core query data-types...
 *  Each core query type defines it's set of optional "comparitor qualifiers".
 */
/* Comparisons for QOF_TYPE_STRING */
typedef enum
{
    QOF_STRING_MATCH_NORMAL = 1,
    QOF_STRING_MATCH_CASEINSENSITIVE
} QofStringMatch;

/** Comparisons for QOF_TYPE_DATE
 * The QOF_DATE_MATCH_DAY comparison rounds the two time
 *     values to mid-day and then compares these rounded values.
 * The QOF_DATE_MATCH_NORMAL comparison matches the time values,
 *     down to the second.
 */

typedef enum
{
    QOF_DATE_MATCH_NORMAL = 1,
    QOF_DATE_MATCH_DAY
} QofDateMatch;

/** Comparisons for QOF_TYPE_NUMERIC, QOF_TYPE_DEBCRED
 *
 * XXX Should be deprecated, or at least wrapped up as a convenience
 * function,  this is based on the old bill gribble code, which assumed
 * the amount was always positive, and then specified a funds-flow
 * direction (credit, debit, or either).
 *
 * The point being that 'match credit' is equivalent to the compound
 * predicate (amount >= 0) && (amount 'op' value) while the  'match
 * debit' predicate is equivalent to (amount <= 0) && (abs(amount) 'op' value)
*/

typedef enum
{
    QOF_NUMERIC_MATCH_DEBIT = 1,
    QOF_NUMERIC_MATCH_CREDIT,
    QOF_NUMERIC_MATCH_ANY
} QofNumericMatch;

/* Comparisons for QOF_TYPE_GUID */
typedef enum
{
    /** These expect a single object and expect the
     * QofAccessFunc returns GUID* */
    QOF_GUID_MATCH_ANY = 1,
    QOF_GUID_MATCH_NONE,
    QOF_GUID_MATCH_NULL,
    /** These expect a GList* of objects and calls the QofAccessFunc routine
     * on each item in the list to obtain a GUID* for each object */
    QOF_GUID_MATCH_ALL,
    /** These expect a single object and expect the QofAccessFunc function
     * to return a GList* of GUID* (the list is the property of the caller) */
    QOF_GUID_MATCH_LIST_ANY,
} QofGuidMatch;

/** A CHAR type is for a RECNCell, Comparisons for QOF_TYPE_CHAR
 *  'ANY' will match any character in the string.
 *
 * Match 'ANY' is a convenience/performance-enhanced predicate
 * for the compound statement (value==char1) || (value==char2) || etc.
 * Match 'NONE' is equivalent to
 * (value != char1) && (value != char2) && etc.
 */
typedef enum
{
    QOF_CHAR_MATCH_ANY = 1,
    QOF_CHAR_MATCH_NONE
} QofCharMatch;

/** No extended comparisons for QOF_TYPE_INT32, QOF_TYPE_INT64,
 *  QOF_TYPE_DOUBLE, QOF_TYPE_BOOLEAN, QOF_TYPE_KVP
 */

/** Head of Predicate Data structures.  All PData must start like this. */
struct _QofQueryPredData
{
    QofType               type_name;  /* QOF_TYPE_* */
    QofQueryCompare       how;
};


/** @name Core Data Type Predicates
    @{ */
QofQueryPredData *qof_query_string_predicate (QofQueryCompare how,
        const gchar *str,
        QofStringMatch options,
        gboolean is_regex);

QofQueryPredData *qof_query_date_predicate (QofQueryCompare how,
        QofDateMatch options,
        Timespec date);

QofQueryPredData *qof_query_numeric_predicate (QofQueryCompare how,
        QofNumericMatch options,
        gnc_numeric value);

QofQueryPredData *qof_query_guid_predicate (QofGuidMatch options, GList *guids);
QofQueryPredData *qof_query_int32_predicate (QofQueryCompare how, gint32 val);
QofQueryPredData *qof_query_int64_predicate (QofQueryCompare how, gint64 val);
QofQueryPredData *qof_query_double_predicate (QofQueryCompare how, double val);
QofQueryPredData *qof_query_boolean_predicate (QofQueryCompare how, gboolean val);
QofQueryPredData *qof_query_char_predicate (QofCharMatch options,
        const gchar *chars);
QofQueryPredData *qof_query_collect_predicate (QofGuidMatch options,
        QofCollection *coll);
QofQueryPredData *qof_query_choice_predicate  (QofGuidMatch options, GList *guids);

/** The qof_query_kvp_predicate() matches the object that has
 *  the value 'value' located at the path 'path'.  In a certain
 *  sense, the 'path' is handled as if it were a paramter.
 */
QofQueryPredData *qof_query_kvp_predicate (QofQueryCompare how,
        GSList *path,
        const KvpValue *value);

/** Same predicate as above, except that 'path' is assumed to be
 * a string containing slash-separated pathname. */
QofQueryPredData *qof_query_kvp_predicate_path (QofQueryCompare how,
        const gchar *path,
        const KvpValue *value);

/** Copy a predicate. */
QofQueryPredData *qof_query_core_predicate_copy (const QofQueryPredData *pdata);

/** Destroy a predicate. */
void qof_query_core_predicate_free (QofQueryPredData *pdata);

/** Retrieve a predicate. */
gboolean qof_query_date_predicate_get_date (const QofQueryPredData *pd, Timespec *date);
/** Return a printable string for a core data object.  Caller needs
 *  to g_free() the returned string.
 */
char * qof_query_core_to_string (QofType, gpointer object, QofParam *getter);

/** Compare two parameter(strings) as if they are numbers!
 *  the two objects, a and b, are the objects being compared
 *  this_param is the QofParam for this parameter in the objects
 */
int qof_string_number_compare_func (gpointer a, gpointer b, gint options,
                                    QofParam *this_param);


#endif /* QOF_QUERYCORE_H */
/* @} */
/* @} */
