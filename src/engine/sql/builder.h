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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

/*
 * FILE:
 * builder.h
 *
 * FUNCTION:
 * Generic SQL backend query builder
 * Compiles typed tag-value pairs into sql queries
 *
 * HISTORY:
 * Linas Vepstas January 2001
 */

#ifndef __SQL_BUILDER_H__
#define __SQL_BUILDER_H__

#include "date.h"
#include "guid.h"

typedef enum {
   SQL_UPDATE=1,
   SQL_INSERT,
   SQL_SELECT
} sqlBuild_QType;

typedef struct _builder sqlBuilder;

sqlBuilder * sqlBuilder_new(void);
void sqlBuilder_destroy (sqlBuilder *);

/* Start building a new SQL query on table 'tablename' 
 * Previous query is erased.
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


/* Get the completed query string back.  This query string is
 * probably general enough to work with almost any SQL db, 
 * I beleive. */
const char *sqlBuild_Query (sqlBuilder *b);


#endif /* __SQL_BUILDER_H__ */

