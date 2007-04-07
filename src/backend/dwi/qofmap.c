/********************************************************************\
 * qofmap.c -- Map QOF object to SQL tables, and back.              *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/
/*
 *  @file qofmap.c
 *  Prototype for a new QOF DWI backend .... under development.
 *
 * Basic parsing works basic, copyin, copyout works.
 */

#include <qof/qofinstance-p.h>
#include "qofmap.h"

#include "database.h"
#include "duifield-qof.h"
#include "duifield-sql.h"
#include "duifieldmap.h"
#include "duiresolver.h"
#include "duitxnquery.h"
#include "duitxnreport.h"
#include "perr.h"

/* ============================================================== */

struct QofMap_s
{
	char   *entity_type;        /**< instance type */
	char   *table_name;         /**< SQL table name */

	/** The 'txnquery' will be used when writing to the database,
	 * we use it to construct the SQL for us, such as INSERT INTO 
	 * and UPDATE .. WHERE
	 */
	DuiTxnQuery  *goto_db;

	/** The 'txnreport' will be used when fetching from the DB.
	 * We will manually generate the SELECT * FROM, and use the 
	 * report to copy from the sql recordset to the QOF object.
	 */
	DuiTxnReport *from_db;

	/** When going to/from the database, we need to match records.
	 * These will always be GUID's on the object side.  We use
	 * the match_fieldname to store teh name of the SQL field that
	 * holds the GUID. */
	char   *match_fieldname;
	char   *match_property;
	DuiFieldMap  *goto_db_sqlmatch;
	DuiFieldMap  *goto_db_qofmatch;
	DuiFieldMap  *from_db_qofmatch;

   /** Versioning */
	DuiFieldMap  *xxxfrom_db_version;

	QofBook *book;              /**< The book in which instance lives */
	DuiDatabase *db;            /**< Handle to the SQL database */
	DuiDBConnection *db_conn;
	DuiResolver  *resolver;

	QofBook *tmp_book;          /**< Temp staging area for write-outs */
};

/* ============================================================== */

QofMap *
qof_map_new (const char *etype, const char *tabname)
{
	QofMap *qm;

	if (!etype || !tabname) return NULL;

	qm = g_new0 (QofMap, 1);
	qm->entity_type = g_strdup (etype);
	qm->table_name = g_strdup (tabname);

	qm->goto_db = dui_txnquery_new();
	qm->from_db = dui_txnreport_new("from", 0);

	qm->goto_db_sqlmatch = NULL;
	qm->goto_db_qofmatch = NULL;
	qm->from_db_qofmatch = NULL;

	dui_txnquery_set_tablename (qm->goto_db, tabname);

	qm->resolver = dui_resolver_new();
	dui_txnquery_set_resolver (qm->goto_db, qm->resolver);
	dui_txnreport_set_resolver (qm->from_db, qm->resolver);

	/* Initialize the more complex parts */
	qm->tmp_book = qof_book_new();

	return qm;
}

/* ============================================================== */

void 
qof_map_destroy (QofMap *qm)
{
	if (!qm) return;

	if (qm->entity_type) g_free (qm->entity_type);
	if (qm->table_name) g_free (qm->table_name);
	if (qm->match_fieldname) g_free (qm->match_fieldname);
	if (qm->match_property) g_free (qm->match_property);

	if (qm->goto_db) dui_txnquery_destroy (qm->goto_db);
	if (qm->from_db) dui_txnreport_destroy (qm->from_db);
	if (qm->resolver) dui_resolver_destroy (qm->resolver);

	if (qm->goto_db_sqlmatch) dui_field_map_destroy (qm->goto_db_sqlmatch);
	if (qm->goto_db_qofmatch) dui_field_map_destroy (qm->goto_db_qofmatch);
	if (qm->from_db_qofmatch) dui_field_map_destroy (qm->from_db_qofmatch);

	// XXX close db connection ???

	qof_book_destroy (qm->tmp_book);
	g_free (qm);
}

/* ============================================================== */

void
qof_map_add_field (QofMap *qm, const char *fieldname, const char * property)
{
	/* Create bi-drectional set of maps */
	DuiFieldMap *from_db_fm = dui_field_map_new();
	DuiFieldMap *goto_db_fm = dui_field_map_new();

	dui_field_set_qof (&from_db_fm->target, qm->entity_type, property);
	dui_field_set_qof (&goto_db_fm->source, qm->entity_type, property);

	dui_field_set_sql (&from_db_fm->source, fieldname);
	dui_field_set_sql (&goto_db_fm->target, fieldname);

	dui_txnreport_add_term (qm->from_db, from_db_fm);
	dui_txnquery_add_term  (qm->goto_db, goto_db_fm);
}

/* ============================================================== */

void
qof_map_add_match (QofMap *qm, const char *fieldname, const char * property)
{
	if (!qm || !fieldname) return;

	if (NULL != qm->goto_db_sqlmatch)
	{
		PERR ("Only one match term per map is supported");
		return;
	}

	qm->match_fieldname = g_strdup (fieldname);
	qm->match_property = g_strdup (property);

	qm->goto_db_sqlmatch = dui_field_map_new();
	qm->goto_db_qofmatch = dui_field_map_new();
	qm->from_db_qofmatch = dui_field_map_new();

	/* When copying to the DB, we need to indicate which object
	 * is to be copied.  The source for the match will be set 
	 * during actual runtime 
	 */
	DuiFieldMap *to_sqlmatch = qm->goto_db_sqlmatch;
	DuiFieldMap *to_qofmatch = qm->goto_db_qofmatch;
	dui_field_set_where (&to_sqlmatch->target, fieldname, "=");
	dui_field_set_qof_match (&to_qofmatch->target, qm->entity_type, property);

	dui_txnquery_add_source_match_term (qm->goto_db, to_qofmatch);
	dui_txnquery_add_term (qm->goto_db, to_sqlmatch);

	/* As above, but for copying from the DB. */
	DuiFieldMap *fr_match = qm->from_db_qofmatch;
	dui_field_set_qof_match (&fr_match->target, qm->entity_type, property);
	dui_txnreport_add_match_term (qm->from_db, fr_match);
}

/* ============================================================== */

void
qof_map_add_version_cmp (QofMap *qm, const char *fieldname, const char * property)
{
	if (!qm || !fieldname) return;

	/* XXX At this time, this behaves just like an ordinary field ... */
	qof_map_add_field (qm, fieldname, property);
}

/* ============================================================== */

void
qof_map_set_book (QofMap *qm, QofBook *book)
{
	qm->book = book;

	/* Each qof field needs to know the book as well. */
	dui_resolver_resolve_qof (qm->resolver, book);
}

void
qof_map_set_database (QofMap *qm, DuiDatabase *db)
{
	qm->db = db;

	/* The SQL writer needs to know the database */
	dui_txnquery_set_database (qm->goto_db, db);

	/* Open the db connection too. */
	qm->db_conn = dui_database_do_realize (db);
}

/* ============================================================== */
/* Get possibly multiple records from DB, and update or create the 
 * matching QOF objects. */

void
qof_map_copy_multiple_from_db (QofMap *qmap, const char * sql_stmt)
{
	if (!qmap || !sql_stmt || 0==sql_stmt[0]) return;

	if (NULL == qmap->from_db_qofmatch)
	{
		PERR ("No match term specified, can't copy");
		return;
	}

	/* Fetch all from the requested table */
	DuiDBRecordSet *recs;
	recs = dui_connection_exec(qmap->db_conn, sql_stmt);

	if (!recs) return;
	if (0 == dui_recordset_rewind (recs)) 
	{
		dui_recordset_free (recs);
		return;
	}

	/* Take the returned SQL records, and update or create the
	 * matching QOF objects.  We do this with a duitxnreport report.  
	 */
	DuiField *fld;
	fld = &qmap->from_db_qofmatch->source;
	dui_field_set_sql (fld, qmap->match_fieldname);

	dui_txnreport_run (qmap->from_db, recs);
	dui_recordset_free (recs);
}

/* ============================================================== */
/* Get record from DB, find or create the matching QOF object. */

void
qof_map_copy_from_db (QofMap *qmap, const GUID *guid)
{
	if (!qmap || !guid) return;

	if (NULL == qmap->from_db_qofmatch)
	{
		PERR ("No match term specified, can't copy");
		return;
	}

	char guidstr[GUID_ENCODING_LENGTH+1];
	guid_to_string_buff (guid, guidstr);

	char * query = g_strdup_printf (
	                    "SELECT * FROM %s WHERE %s=\'%s\';",
	                    qmap->table_name, 
	                    qmap->match_fieldname, 
	                    guidstr);

	/* Fetch all from the requested table */
	DuiDBRecordSet *recs;
	recs = dui_connection_exec(qmap->db_conn, query);
	g_free (query);

	if (!recs) return;
	if (0 == dui_recordset_rewind (recs)) 
	{
		dui_recordset_free (recs);
		return;
	}

	/* Take the returned SQL record, and create a matching QOF object.  
	 * We do this with a duitxnreport report.  
	 */
	DuiField *fld;
	fld = &qmap->from_db_qofmatch->source;
	dui_field_set_const (fld, guidstr);

	dui_txnreport_run (qmap->from_db, recs);
	dui_recordset_free (recs);
}

/* ============================================================== */
/* 'Copy' a QOF Entity from local memory to the SQL database.
 * Pass in the Guid of the entity that is to be saved to SQL.
 */
void 
qof_map_copy_to_db(QofMap *qmap, const GUID *guid, const char *how)
{
	if (!qmap || !guid) return;

	if (NULL == qmap->goto_db_qofmatch)
	{
		PERR ("No match term specified, can't copy");
		return;
	}
	/* 'how' should be either "insert" or "update" */
	dui_txnquery_set_querytype (qmap->goto_db, how);

	char buff[GUID_ENCODING_LENGTH+1];
	guid_to_string_buff (guid, buff);

	/* We use the GUID as our primary key for all lookups/matches */
	DuiField *fld;
	fld = &qmap->goto_db_qofmatch->source;
	dui_field_set_const (fld, buff);

	fld = &qmap->goto_db_sqlmatch->source;
	dui_field_set_const (fld, buff);

	/* Do it: issue the SQL insert/update. */
	DuiDBRecordSet *recs = dui_txnquery_run (qmap->goto_db);
	dui_recordset_free (recs);
}

/* ============================================================== */
/*
write-out sequence:

lock table
get version
if (not exist) insert ;
else
  cmp version
  if (newer) update;
  else copy-from, including rollback.

unlock table
*/

void
write_out (QofMap *qm, QofInstance *inst)
{
	if (!inst) return;

	GUID *guid = &QOF_INSTANCE(inst)->guid;

	dui_connection_lock(qm->db_conn, qm->table_name);
	/* Use a temp book when loading from the database */
	dui_resolver_resolve_qof (qm->resolver, qm->tmp_book);
	qof_map_copy_from_db (qm, guid);

	/* restore the 'real' book */
	dui_resolver_resolve_qof (qm->resolver, qm->book);

	/* See if we got something back from the DB */
	QofCollection *col;
	col = qof_book_get_collection (qm->tmp_book, QOF_INSTANCE(inst)->e_type);
	QofInstance * db_ent = qof_collection_lookup_entity (col, guid);
	QofInstance *db_inst = QOF_INSTANCE(db_ent);

	/* If its not already in the database, then insert it in */
	if (NULL == db_ent)
	{
		struct timespec ts = dui_connection_get_now(qm->db_conn);
		Timespec qts;
		qts.tv_sec = ts.tv_sec;
		qts.tv_nsec = ts.tv_nsec;
		qof_instance_set_last_update (inst, qts);
		qof_map_copy_to_db (qm, guid, "insert");
	}
	else
	{
		/* Found it in the database; but who's got the newer version? */
 		int cmp = qof_instance_version_cmp (db_inst, inst);
		if (0 >= cmp)
		{
			struct timespec ts = dui_connection_get_now(qm->db_conn);
			Timespec qts;
			qts.tv_sec = ts.tv_sec;
			qts.tv_nsec = ts.tv_nsec;
			qof_instance_set_last_update (inst, qts);
			qof_map_copy_to_db (qm, guid, "update");
		}
		else
		{
printf ("duude rollback\n");
		}
	}
	dui_connection_unlock(qm->db_conn, qm->table_name);
}

/* ======================== END OF FILE ========================= */
/* ============================================================== */
