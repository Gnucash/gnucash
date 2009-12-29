/********************************************************************\
 * builder.h : Compile SQL queries from C language values           *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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
\********************************************************************/

/*
 * FILE:
 * builder.h
 *
 * FUNCTION:
 * Generic SQL query builder.  This class can be sued to construct
 * a basic sql query statement (of the type 'select', 'update' or
 * 'insert') by simply making C calls indicating the table and the
 * fields to query.
 *
 * Its fairly limited in the range of sql syntax that it supports,
 * but on the other hand, the code here is/should be general enough
 * to work with  any SQL implementation.
 *
 * HISTORY:
 * Linas Vepstas January 2001
 */

#ifndef SQL_BUILDER_H
#define SQL_BUILDER_H

#include "qof.h"

#define SQL_DBL_FMT "%24.18g"

typedef enum
{
    SQL_UPDATE = 'm',  /* m == modify */
    SQL_INSERT = 'a',  /* a == add */
    SQL_SELECT = 'q',  /* q == query */
    SQL_DELETE = 'd'   /* d == drop, delete */
} sqlBuild_QType;

typedef struct _builder sqlBuilder;

sqlBuilder * sqlBuilder_new(void);
void sqlBuilder_destroy (sqlBuilder *);

/* The sqlBuild_Table() routine starts building a new SQL query
 *    on table 'tablename'.  Any previously started query is erased.
 *
 *    When building 'select' type statments, crude table joins are
 *    supported: the 'tablename' can in fact be a comma-separated list
 *    of tables.  This field is copied directly as follows:
 *    "SELECT ... FROM tablename WHERE ..." so anything valid in that
 *    position is tolerated.
 */
void sqlBuild_Table (sqlBuilder *b,
                     const char *tablename,
                     sqlBuild_QType qtype);


/* Set tag-value pairs.  Each of these adds the indicated
 * tag and value to an UPDATE or INSERT statement.  For SELECT
 * statements, val may be NULL (and is ignored in any case).
 */
void sqlBuild_Set_Str   (sqlBuilder *b, const char *tag, const char *val);
void sqlBuild_Set_Char  (sqlBuilder *b, const char *tag, char val);
void sqlBuild_Set_GUID  (sqlBuilder *b, const char *tag, const GUID *val);
void sqlBuild_Set_Date  (sqlBuilder *b, const char *tag, Timespec val);
void sqlBuild_Set_Int64 (sqlBuilder *b, const char *tag, gint64 val);
void sqlBuild_Set_Int32 (sqlBuilder *b, const char *tag, gint32 val);
void sqlBuild_Set_Double(sqlBuilder *b, const char *tag, double val);


/* build the update 'where' clause */
/* typically, the primary tag is used in the where clauses */
/* this where clause is used for both SELECT and UPDATE statements */

void sqlBuild_Where_Str  (sqlBuilder *b, const char *tag, const char *val);
void sqlBuild_Where_GUID (sqlBuilder *b, const char *tag, const GUID *val);
void sqlBuild_Where_Int32 (sqlBuilder *b, const char *tag, gint32 val);


/* The sqlBuild_Query() routine returns a valid SQL query
 *    statement that reflects the set of build calls just made.
 *    This string is freed when sqlBuilder_destroy() is called,
 *    so make a copy if you need it.
 *
 *    This resulting query string is probably general enough to
 *    work with almost any SQL db, I beleive.
 */
const char *sqlBuild_Query (sqlBuilder *b);


#endif /* SQL_BUILDER_H */

