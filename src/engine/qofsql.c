/********************************************************************\
 * qofsql.c -- QOF client-side SQL parser                           *
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
 *                                                                  *
\********************************************************************/

/**
    @file qofsql.c
    @brief QOF client-side SQL parser.
    @author Copyright (C) 2004 Linas Vepstas <linas@linas.org>

*/

#include <stdlib.h>   /* for working atoll */

#include <glib.h>
#include <libsql/sql_parser.h>
#include <qof/kvp_frame.h>
#include <qof/gnc-date.h>
#include <qof/gnc-numeric.h>
#include <qof/gnc-trace.h>
#include <qof/guid.h>
#include <qof/qofbook.h>
#include <qof/qofquery.h>
#include <qof/qofquerycore.h>
#include <qof/qofsql.h>
#include <qof/gnc-engine-util.h>

static short module = MOD_QUERY;

/* =================================================================== */

struct _QofSqlQuery
{
	sql_statement *parse_result;
	QofQuery *qof_query;
	QofBook *book;
	char * single_global_tablename;
	KvpFrame *kvp_join;
};

/* ========================================================== */

QofSqlQuery *
qof_sql_query_new(void)
{
	QofSqlQuery * sqn = (QofSqlQuery *) g_new0 (QofSqlQuery, 1);
	
	sqn->qof_query = NULL;
	sqn->parse_result = NULL;
	sqn->book = NULL;
	sqn->single_global_tablename = NULL;
	sqn->kvp_join = NULL;

	return sqn;
}

/* ========================================================== */

void 
qof_sql_query_destroy (QofSqlQuery *q)
{
	if (!q) return;
	qof_query_destroy (q->qof_query);
	sql_destroy (q->parse_result);
	g_free (q);
}

/* ========================================================== */

QofQuery * 
qof_sql_query_get_query (QofSqlQuery *q)
{
	if (!q) return NULL;
	return q->qof_query;
}

/* ========================================================== */

void 
qof_sql_query_set_book (QofSqlQuery *q, QofBook *book)
{
	if (!q) return;
	q->book = book;
}

/* ========================================================== */

void 
qof_sql_query_set_kvp (QofSqlQuery *q, KvpFrame *kvp)
{
	if (!q) return;
	q->kvp_join = kvp;
}

/* ========================================================== */

static inline void
get_table_and_param (char * str, char **tab, char **param)
{
	char * end = strchr (str, '.');
	if (!end) 
	{
		*tab = 0;
		*param = str;
		return;
	}
	*end = 0;
	*tab = str;
	*param = end+1;
}

static inline char * 
dequote_string (char *str)
{
	size_t len;
	/* strip out quotation marks ...  */
	if (('\'' == str[0]) ||
	    ('\"' == str[0]))
	{
		str ++;
		len = strlen(str);
		str[len-1] = 0;
	}
	return str;
}

static QofQuery *
handle_single_condition (QofSqlQuery *query, sql_condition * cond)
{
	char tmpbuff[128];
	GSList *param_list;
	GList *guid_list;
	QofQueryPredData *pred_data = NULL;
	sql_field_item *sparam, *svalue;
	char * qparam_name, *qvalue_name, *table_name, *param_name;
	char *sep, *path,*str,*p;
	QofQuery *qq;
	KvpValue *kv, *kval;
	KvpValueType kvt;
	QofQueryCompare qop;
	time_t exact;
	int rc, len;
	Timespec ts;
	QofType param_type;
	QofGuidMatch gm;
	
	if (NULL == cond)
	{
		PWARN("missing condition");
		return NULL;
	}
			
	/* -------------------------------- */
	/* field to match, assumed, for now to be on the left */
	/* XXX fix this so it can be either left or right */
	if (NULL == cond->d.pair.left)
	{
		PWARN("missing left paramter");
		return NULL;
	}
	sparam = cond->d.pair.left->item;
	if (SQL_name != sparam->type)
	{
		PWARN("we support only paramter names at this time (parsed %d)",
          sparam->type);
		return NULL;
	}
	qparam_name = sparam->d.name->data;
	if (NULL == qparam_name)
	{
		PWARN ("missing paramter name");
		return NULL;
	}

	/* -------------------------------- */
	/* value to match, assumed, for now, to be on the right. */
	/* XXX fix this so it can be either left or right */
	if (NULL == cond->d.pair.right)
	{
		PWARN ("missing right paramter");
		return NULL;
	}
	svalue = cond->d.pair.right->item;
	if (SQL_name != svalue->type)
	{
		PWARN("we support only simple values (parsed as %d)", svalue->type);
		return NULL;
	}
	qvalue_name = svalue->d.name->data;
	if (NULL == qvalue_name)
	{
		PWARN("missing value");
		return NULL;
	}
	qvalue_name = dequote_string (qvalue_name);
	qvalue_name = (char *) qof_util_whitespace_filter (qvalue_name);

	/* Look to see if its the special KVP value holder.
	 * If it is, look up the value. */
	if (0 == strncasecmp (qvalue_name, "kvp://", 6))
	{
		if (NULL == query->kvp_join)
		{
			PWARN ("missing kvp frame");
			return NULL;
		}
		kv = kvp_frame_get_value (query->kvp_join, qvalue_name+5);
		/* If there's no value, its not an error; 
		 * we just don't do this predicate */
		if (!kv) return NULL;  
		kvt = kvp_value_get_type (kv);

		tmpbuff[0] = 0x0;
		qvalue_name = tmpbuff;
		switch (kvt)
		{
			case KVP_TYPE_GINT64:
			{
				gint64 ival = kvp_value_get_gint64(kv);
				sprintf (tmpbuff, "%lld\n", ival);
				break;
			}
			case KVP_TYPE_DOUBLE:
			{
				double ival = kvp_value_get_double(kv);
				sprintf (tmpbuff, "%26.18g\n", ival);
				break;
			}
			case KVP_TYPE_STRING:
				/* If there's no value, its not an error; 
				 * we just don't do this predicate */
				qvalue_name = kvp_value_get_string (kv);
				if (!qvalue_name) return NULL;
				break;
			case KVP_TYPE_GUID:
			case KVP_TYPE_TIMESPEC:
			case KVP_TYPE_BINARY:
			case KVP_TYPE_GLIST:
			case KVP_TYPE_NUMERIC:
			case KVP_TYPE_FRAME:
				PWARN ("unhandled kvp type=%d", kvt);
				return NULL;
		}
	}

	/* -------------------------------- */
	/* Now start building the QOF paramter */
	param_list = qof_query_build_param_list (qparam_name, NULL);

	/* Get the where-term comparison operator */
	switch (cond->op)
	{
		case SQL_eq:    qop = QOF_COMPARE_EQUAL; break;
		case SQL_gt:    qop = QOF_COMPARE_GT; break;
		case SQL_lt:    qop = QOF_COMPARE_LT; break;
		case SQL_geq:   qop = QOF_COMPARE_GTE; break;
		case SQL_leq:   qop = QOF_COMPARE_LTE; break;
		case SQL_diff:  qop = QOF_COMPARE_NEQ; break;
		default:
			/* XXX for string-type queries, we should be able to
			 * support 'IN' for substring search.  Also regex. */
			PWARN ("Unsupported compare op (parsed as %s)", cond->op);
			return NULL;
	}

	/* OK, need to know the type of the thing being matched 
	 * in order to build the correct predicate.  Get the type 
	 * from the object parameters. */
	get_table_and_param (qparam_name, &table_name, &param_name);
	if (NULL == table_name)
	{
		table_name = query->single_global_tablename;
	}
		
	if (NULL == table_name)
	{
		PWARN ("Need to specify an object class to query");
		return NULL;
	}

	if (FALSE == qof_class_is_registered (table_name)) 
	{
		PWARN ("The query object \'%s\' is not known", table_name);
		return NULL;
	}

	param_type = qof_class_get_parameter_type (table_name, param_name);
	if (!param_type) 
	{
		PWARN ("The parameter \'%s\' on object \'%s\' is not known", 
		       param_name, table_name);
		return NULL;
	}

	if (!strcmp (param_type, QOF_TYPE_STRING))
	{
		pred_data = 
		    qof_query_string_predicate (qop,        /* comparison to make */
		          qvalue_name,                      /* string to match */
		          QOF_STRING_MATCH_CASEINSENSITIVE,  /* case matching */
		          FALSE);                            /* use_regexp */
	}
	else if (!strcmp (param_type, QOF_TYPE_CHAR))
	{
		QofCharMatch cm = QOF_CHAR_MATCH_ANY;
		if (QOF_COMPARE_NEQ == qop) cm = QOF_CHAR_MATCH_NONE;
		pred_data = qof_query_char_predicate (cm, qvalue_name);
	}
	else if (!strcmp (param_type, QOF_TYPE_INT32))
	{
		gint32 ival = atoi (qvalue_name);
		pred_data = qof_query_int32_predicate (qop, ival);
	}
	else if (!strcmp (param_type, QOF_TYPE_INT64))
	{
		gint64 ival = atoll (qvalue_name);
		pred_data = qof_query_int64_predicate (qop, ival);
	}
	else if (!strcmp (param_type, QOF_TYPE_DOUBLE))
	{
		double ival = atof (qvalue_name);
		pred_data = qof_query_double_predicate (qop, ival);
	}
	else if (!strcmp (param_type, QOF_TYPE_BOOLEAN))
	{
		gboolean ival = qof_util_bool_to_int (qvalue_name);
		pred_data = qof_query_boolean_predicate (qop, ival);
	}
	else if (!strcmp (param_type, QOF_TYPE_DATE))
	{
		// XXX FIXME: this doesn't handle time strings, only date strings
		// XXX should also see if we need to do a day-compare or time-compare.
		/* work around highly bogus locale setting */
		qof_date_format_set(QOF_DATE_FORMAT_US);

		rc= qof_scan_date_secs (qvalue_name, &exact);
		if (0 == rc) 
		{
			PWARN ("unable to parse date: %s", qvalue_name);
			return NULL;
		}
		ts.tv_sec = exact;
		ts.tv_nsec = 0;
		pred_data = qof_query_date_predicate (qop, QOF_DATE_MATCH_DAY, ts);
	}
	else if (!strcmp (param_type, QOF_TYPE_NUMERIC))
	{
		gnc_numeric ival;
		string_to_gnc_numeric (qvalue_name, &ival);
		pred_data = qof_query_numeric_predicate (qop, QOF_NUMERIC_MATCH_ANY, ival);
	}
	else if (!strcmp (param_type, QOF_TYPE_DEBCRED))
	{
		// XXX this probably needs some work ... 
		gnc_numeric ival;
		string_to_gnc_numeric (qvalue_name, &ival);
		pred_data = qof_query_numeric_predicate (qop, QOF_NUMERIC_MATCH_ANY, ival);
	}
	else if (!strcmp (param_type, QOF_TYPE_GUID))
	{
		GUID guid;
		gboolean rc = string_to_guid (qvalue_name, &guid);
		if (0 == rc)
		{
			PWARN ("unable to parse guid: %s", qvalue_name);
			return NULL;
		}

		// XXX less, than greater than don't make sense,
		// should check for those bad conditions

		gm = QOF_GUID_MATCH_ANY;
		if (QOF_COMPARE_NEQ == qop) gm = QOF_GUID_MATCH_NONE;
		guid_list = g_list_append (NULL, &guid);
		pred_data = qof_query_guid_predicate (gm, guid_list);

		g_list_free (guid_list);
	}
	else if (!strcmp (param_type, QOF_TYPE_KVP))
	{
		/* We are expecting an encoded value that looks like
		 * /some/path/string:value
		 */
		sep = strchr (qvalue_name, ':');
		if (!sep) return NULL;
		*sep = 0;
		path = qvalue_name;
		str = sep +1;
		/* If str has only digits, we know its a plain number.
		 * If its numbers and a decimal point, assume a float
		 * If its numbers and a slash, assume numeric
		 * If its 32 bytes of hex, assume GUID
		 * If it looks like an iso date ... 
		 * else assume its a string.
		 */
		kval = NULL;
		len = strlen (str);
		if ((32 == len) && (32 == strspn (str, "0123456789abcdef")))
		{
			GUID guid;
			string_to_guid (str, &guid);
			kval = kvp_value_new_guid (&guid);
		}
		else
		if (len == strspn (str, "0123456789"))
		{
			kval = kvp_value_new_gint64 (atoll(str));
		}
		else
		if ((p=strchr (str, '.')) && 
		    ((len-1) == (strspn (str, "0123456789") + 
		                 strspn (p+1, "0123456789"))))
		{
			kval = kvp_value_new_double (atof(str));
		}

		else
		if ((p=strchr (str, '/')) && 
		    ((len-1) == (strspn (str, "0123456789") + 
		                 strspn (p+1, "0123456789"))))
		{
			gnc_numeric num;
			string_to_gnc_numeric (str, &num);
			kval = kvp_value_new_gnc_numeric (num);
		}
		else
		if ((p=strchr (str, '-')) && 
		    (p=strchr (p+1, '-')) && 
		    (p=strchr (p+1, ' ')) && 
		    (p=strchr (p+1, ':')) && 
		    (p=strchr (p+1, ':')))
		{
			kval = kvp_value_new_timespec (gnc_iso8601_to_timespec_gmt(str));
		}

		/* The default handler is a string */
		if (NULL == kval)
		{
			kval = kvp_value_new_string (str);
		}
		pred_data = qof_query_kvp_predicate_path (qop, path, kval);
	}
	else
	{
		PWARN ("The predicate type \"%s\" is unsupported for now", param_type);
		return NULL;
	}

	qq = qof_query_create();
	qof_query_add_term (qq, param_list, pred_data, QOF_QUERY_FIRST_TERM);
	return qq;
}

/* ========================================================== */

static QofQuery *
handle_where (QofSqlQuery *query, sql_where *swear)
{
	QofQueryOp qop;
	QofQuery * qq;
	
	switch (swear->type)
	{
		case SQL_pair:
		{
			QofQuery *qleft = handle_where (query, swear->d.pair.left);
			QofQuery *qright = handle_where (query, swear->d.pair.right);
			if (NULL == qleft) return qright;
			if (NULL == qright) return qleft;
			switch (swear->d.pair.op)
			{
				case SQL_and: qop = QOF_QUERY_AND; break;
				case SQL_or: qop = QOF_QUERY_OR; break;
				/* XXX should add support for nand, nor, xor */
				default: 
					qof_query_destroy (qleft);
					qof_query_destroy (qright);
					return NULL;
			}
			qq = qof_query_merge (qleft, qright, qop);
			qof_query_destroy (qleft);
			qof_query_destroy (qright);
			return qq;
		}
		case SQL_negated:
		{
			QofQuery *qq = handle_where (query, swear->d.negated);
			QofQuery *qneg = qof_query_invert (qq);
			qof_query_destroy (qq);
			return qneg;
		}

		case SQL_single:
		{
			sql_condition * cond = swear->d.single;
			return handle_single_condition (query, cond);
		}
	}
	return NULL;
}

/* ========================================================== */

static void 
handle_sort_order (QofSqlQuery *query, GList *sorder_list)
{
	GSList *qsp[3];
	GList *n;
	gboolean direction[3];
	int i;
	sql_order_field *sorder;
	char * qparam_name;
	
	if (!sorder_list) return;

	for (i=0; i<3; i++)
	{
		qsp[i] = NULL;
		direction[i] = 0;

		if (sorder_list)
		{
			sorder = sorder_list->data;

			/* Set the sort direction */
			if (SQL_asc == sorder->order_type) direction[i] = TRUE;

			/* Find the paramter name */
			qparam_name = NULL;
			n = sorder->name;
			if (n)
			{
				qparam_name = n->data;
				if (qparam_name) 
				{
					qsp[i] = qof_query_build_param_list (qparam_name, NULL);
				}
				n = n->next;   /* next paramter */
			}
			else
			{
				/* if no next paramter, then next order-by */
				sorder_list = sorder_list->next;
			}
		}
	}

	qof_query_set_sort_order (query->qof_query, qsp[0], qsp[1], qsp[2]);
	qof_query_set_sort_increasing (query->qof_query, direction[0],
	                            direction[1], direction[2]);
}

/* ========================================================== */

void 
qof_sql_query_parse (QofSqlQuery *query, const char *str)
{
	GList *tables;
	sql_select_statement *sss;
	sql_where *swear;
	
	if (!query) return;

	/* Delete old query, if any */
   /* XXX FIXME we should also delete the parse_result as well */
	if (query->qof_query)
	{
		qof_query_destroy (query->qof_query);
		query->qof_query = NULL;
	}

	/* Parse the SQL string */
	query->parse_result = sql_parse (str);

	if (!query->parse_result) 
	{
		PWARN ("parse error"); 
		return;
	}

	if (SQL_select != query->parse_result->type)
	{
		PWARN("currently, only SELECT statements are supported, "
		                     "got type=%d", query->parse_result);
		return;
	}

	/* If the user wrote "SELECT * FROM tablename WHERE ..."
	 * then we have a single global tablename.  But if the 
	 * user wrote "SELECT * FROM tableA, tableB WHERE ..."
	 * then we don't have a single unique table-name.
	 */
	tables = sql_statement_get_tables (query->parse_result);
	if (1 == g_list_length (tables))
	{
		query->single_global_tablename = tables->data;
	}

	sss = query->parse_result->statement;
	swear = sss->where;
	if (swear)
	{
		/* Walk over the where terms, turn them into QOF predicates */
		query->qof_query = handle_where (query, swear);
		if (NULL == query->qof_query) return;
	}
	else
	{
		query->qof_query = qof_query_create();
	}

	/* Provide support for different sort orders */
	handle_sort_order (query, sss->order);

	/* We also want to set the type of thing to search for.
	 * If the user said SELECT * FROM ... then we should return
	 * a list of QofEntity.  Otherwise, we return ... ?
	 * XXX all this needs fixing.
	 */
	qof_query_search_for (query->qof_query, query->single_global_tablename);
}

/* ========================================================== */

GList * 
qof_sql_query_run (QofSqlQuery *query, const char *str)
{
	GList *node, *results;

	if (!query) return NULL;

	qof_sql_query_parse (query, str);
	if (NULL == query->qof_query) return NULL;

	qof_query_set_book (query->qof_query, query->book);

	// qof_query_print (query->qof_query);
	results = qof_query_run (query->qof_query);

	return results;
}

GList * 
qof_sql_query_rerun (QofSqlQuery *query)
{
	GList *node, *results;

	if (!query) return NULL;

	if (NULL == query->qof_query) return NULL;

	qof_query_set_book (query->qof_query, query->book);

	// qof_query_print (query->qof_query);
	results = qof_query_run (query->qof_query);

	return results;
}

/* ========================== END OF FILE =================== */
