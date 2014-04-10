/********************************************************************\
 * qofmap.h -- Map QOF object to SQL tables, and back.              *
 * Copyright (C) 2004 Linas Vepstas <linas@linas.org>               *
 * http://dwi.sourceforge.net                                       *
 *                                                                  *
 * This library is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU Lesser General Public       *
 * License as published by the Free Software Foundation; either     *
 * version 2.1 of the License, or (at your option) any later version.
 *                                                                  *
 * This library is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU Lesser General Public License for more details.              *
 *                                                                  *
 * You should have received a copy of the GNU Lesser General Public *
 * License along with this program; if not, contact:                *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/
/**
 *  @file qofmap.h
 *  Prototype for a new QOF DWI backend .... under development.
 *
 * A QofMap defines how the properties of a QOF Object are mapped
 * to an SQL table.  The QofMap is used store specific Qof Instances
 * as SQL records, and vice-versa: create Qof Instances from SQL
 * records.  The QofMap API consists of two parts: a set of routines
 * used to define the map itself, and a second set of routines to 
 * use that map in various ways.
 */

#ifndef QOF_MAP_H_
#define QOF_MAP_H_

#include <qof/qofbook.h>
#include <qof/qofinstance.h>
#include <qof/guid.h>
#include "database.h"

typedef struct QofMap_s QofMap;

/** Arguments: name of entity (entity type) and SQL table name */
QofMap * qof_map_new (const char *etype, const char *tabname);

/** @{
 *  Routines to define a QofMap
 */
/** Add a link between an SQL fieldname (SQL column name) and QOF property
    name. These will be linked so that the one is stored into the other
    when the object is sync to the database.
 */
void qof_map_add_field (QofMap *qm, const char *fieldname, const char * property);

/** Add a term that uniquely identifies the QOF Object & SQL record.
 * This will be the GUID (that is all that is supported at this time).
 */
void qof_map_add_match (QofMap *qm, const char *fieldname, const char * property);

/** Add a term that indicates the version of the QOF Object/SQL record.
 *  This is used to determine whether the object in local memory is newer
 *  or older than the corresponding record in the database.  Currently,
 *  the only supported type of the version field is a date-time stamp.
 */
void qof_map_add_version_cmp (QofMap *qm, const char *fieldname, const char * property);

/** @} */

/** Set the book in which the object instance will live.
 * The book can be set at any time, there is little penalty
 * to changing the book often.
 */
void qof_map_set_book (QofMap *qm, QofBook *book);

/** Specify the database that will eb used to make the connection */
void qof_map_set_database (QofMap *qm, DuiDatabase *db);

/** Get record from DB, find or create the matching QOF object. */
void qof_map_copy_from_db (QofMap *qm, const GUID *guid);

/** Execute the SQL statement, which is expected to return zero
 *  or more records holding QOF objects.  Find or create the 
 *  the corresponding QOF objects.
 */
void qof_map_copy_multiple_from_db (QofMap *qm, const char * sql_stmt);

/** 'Copy' a QOF Entity from local memory to the SQL database.
 *  Pass in the Guid of the entity that is to be saved to SQL.
 *  'how' should be either "insert" or "update", corresponding to
 *  the SQL INSERT or UPDATE query types.
 */
void qof_map_copy_to_db(QofMap *qm, const GUID *guid, const char * how);

#endif /* QOF_MAP_H_ */
