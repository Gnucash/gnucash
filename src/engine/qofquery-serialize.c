/********************************************************************\
 * qofquery-serialize.c -- Convert QofQuery to XML                  *
 * Copyright (C) 2001,2002,2004 Linas Vepstas <linas@linas.org>     *
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

#include "config.h"

#include "qofquery-serialize.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "kvp_frame.h"

/* ======================================================= */

#define PUT_STR(TOK,VAL) {                           \
   xmlNodePtr node;                                  \
   const char * str = (VAL);                         \
   if (str && 0 != str[0])                           \
   {                                                 \
      node = xmlNewNode (NULL, TOK);                 \
      xmlNodeAddContent(node, str);                  \
      xmlAddChild (topnode, node);                   \
   }                                                 \
}

#define PUT_INT32(TOK,VAL) {                         \
   xmlNodePtr node;                                  \
   char buff[80];                                    \
   g_snprintf (buff, sizeof(buff), "%d", (VAL));     \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, buff);                    \
   xmlAddChild (topnode, node);                      \
}

#define PUT_INT64(TOK,VAL) {                         \
   xmlNodePtr node;                                  \
   char buff[80];                                    \
   g_snprintf (buff, sizeof(buff), "%lld", (VAL));   \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, buff);                    \
   xmlAddChild (topnode, node);                      \
}

#define PUT_DBL(TOK,VAL) {                           \
   xmlNodePtr node;                                  \
   char buff[80];                                    \
   g_snprintf (buff, sizeof(buff), "%.18g", (VAL));  \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, buff);                    \
   xmlAddChild (topnode, node);                      \
}

#define PUT_GUID(TOK,VAL) {                          \
   xmlNodePtr node;                                  \
   char buff[80];                                    \
   guid_to_string_buff ((VAL), buff);                \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, buff);                    \
   xmlAddChild (topnode, node);                      \
}

#define PUT_DATE(TOK,VAL) {                          \
   xmlNodePtr node;                                  \
   char buff[80];                                    \
   gnc_timespec_to_iso8601_buff ((VAL), buff);       \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, buff);                    \
   xmlAddChild (topnode, node);                      \
}

#define PUT_NUMERIC(TOK,VAL) {                       \
   xmlNodePtr node;                                  \
   char *str;                                        \
   str = gnc_numeric_to_string (VAL);                \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, str);                     \
   g_free (str);                                     \
   xmlAddChild (topnode, node);                      \
}

#define PUT_BOOL(TOK,VAL) {                          \
   xmlNodePtr node;                                  \
   gboolean boll = (VAL);                            \
   node = xmlNewNode (NULL, TOK);                    \
   if (boll) {                                       \
      xmlNodeAddContent(node, "T");                  \
   } else {                                          \
      xmlNodeAddContent(node, "F");                  \
   }                                                 \
   xmlAddChild (topnode, node);                      \
}

#define PUT_HOW(TOK,VAL,A,B,C,D,E,F) {               \
   xmlNodePtr node;                                  \
   const char * str = "EQUAL";                       \
   switch (VAL)                                      \
   {                                                 \
      case QOF_COMPARE_##A: str = #A; break;         \
      case QOF_COMPARE_##B: str = #B; break;         \
      case QOF_COMPARE_##C: str = #C; break;         \
      case QOF_COMPARE_##D: str = #D; break;         \
      case QOF_COMPARE_##E: str = #E; break;         \
      case QOF_COMPARE_##F: str = #F; break;         \
   }                                                 \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, str);                     \
   xmlAddChild (topnode, node);                      \
}

#define PUT_MATCH2(TOK,VAL,PFX,A,B) {                \
   xmlNodePtr node;                                  \
   const char * str = #A;                            \
   switch (VAL)                                      \
   {                                                 \
      case QOF_##PFX##_##A: str = #A; break;         \
      case QOF_##PFX##_##B: str = #B; break;         \
   }                                                 \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, str);                     \
   xmlAddChild (topnode, node);                      \
}

#define PUT_MATCH3(TOK,VAL,PFX,A,B,C) {              \
   xmlNodePtr node;                                  \
   const char * str = #A;                            \
   switch (VAL)                                      \
   {                                                 \
      case QOF_##PFX##_##A: str = #A; break;         \
      case QOF_##PFX##_##B: str = #B; break;         \
      case QOF_##PFX##_##C: str = #C; break;         \
   }                                                 \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, str);                     \
   xmlAddChild (topnode, node);                      \
}

#define PUT_MATCH5(TOK,VAL,PFX,A,B,C,D,E) {          \
   xmlNodePtr node;                                  \
   const char * str = #A;                            \
   switch (VAL)                                      \
   {                                                 \
      case QOF_##PFX##_##A: str = #A; break;         \
      case QOF_##PFX##_##B: str = #B; break;         \
      case QOF_##PFX##_##C: str = #C; break;         \
      case QOF_##PFX##_##D: str = #D; break;         \
      case QOF_##PFX##_##E: str = #E; break;         \
   }                                                 \
   node = xmlNewNode (NULL, TOK);                    \
   xmlNodeAddContent(node, str);                     \
   xmlAddChild (topnode, node);                      \
}

/* ======================================================= */

static void
qof_kvp_value_to_xml (KvpValue *kval, xmlNodePtr topnode)
{
	KvpValueType kvt = kvp_value_get_type (kval);

	switch (kvt)
	{
		case KVP_TYPE_GINT64:
			PUT_INT64 ("qofquery:int64", kvp_value_get_gint64(kval));
			break;
		case KVP_TYPE_DOUBLE:
			PUT_DBL ("qofquery:double", kvp_value_get_double(kval));
			break;
		case KVP_TYPE_NUMERIC:
			PUT_NUMERIC ("qofquery:numeric", kvp_value_get_numeric(kval));
			break;
		case KVP_TYPE_GUID:
			PUT_GUID ("qofquery:guid", kvp_value_get_guid(kval));
			break;
		case KVP_TYPE_STRING:
			PUT_STR ("qofquery:string", kvp_value_get_string(kval));
			break;
		case KVP_TYPE_TIMESPEC:
			PUT_DATE ("qofquery:date", kvp_value_get_timespec(kval));
			break;
		case KVP_TYPE_BINARY:
		case KVP_TYPE_GLIST:
		case KVP_TYPE_FRAME:
			// XXX don't know how to support these.
			break;
	}
}

/* ======================================================= */

static xmlNodePtr
qof_query_pred_data_to_xml (QofQueryPredData *pd)
{

	if (!safe_strcmp (pd->type_name, QOF_TYPE_GUID))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-guid");
		/* GUID Predicate doesn't do a PUT_HOW */

		GList *n;
		query_guid_t pdata = (query_guid_t) pd;
		PUT_MATCH5("qofquery:guid-match", pdata->options, 
		                GUID_MATCH, ANY, ALL, NONE, NULL, LIST_ANY);

		for (n = pdata->guids; n; n = n->next)
		{
			PUT_GUID ("qofquery:guid", n->data);
		}
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_STRING))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-string");
		PUT_HOW ("qofquery:compare", pd->how, LT, LTE, EQUAL, GT, GTE, NEQ);

		query_string_t pdata = (query_string_t) pd;
		PUT_MATCH2("qofquery:string-match", pdata->options,
                       STRING_MATCH, NORMAL, CASEINSENSITIVE);
		PUT_BOOL ("qofquery:is-regex", pdata->is_regex);
		PUT_STR ("qofquery:string", pdata->matchstring);
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_NUMERIC))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-numeric");
		PUT_HOW ("qofquery:compare", pd->how, LT, LTE, EQUAL, GT, GTE, NEQ);

		query_numeric_t pdata = (query_numeric_t) pd;
		PUT_MATCH3("qofquery:numeric-match", pdata->options,
		                 NUMERIC_MATCH, DEBIT, CREDIT, ANY);
		
		PUT_NUMERIC ("qofquery:numeric", pdata->amount);
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_KVP))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-kvp");
		PUT_HOW ("qofquery:compare", pd->how, LT, LTE, EQUAL, GT, GTE, NEQ);

		query_kvp_t pdata = (query_kvp_t) pd;
		
		GSList *n;
		for (n=pdata->path; n; n=n->next)
		{
			PUT_STR ("qofquery:kvp-path", n->data);
		}
		qof_kvp_value_to_xml (pdata->value, topnode);
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_DATE))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-date");
		PUT_HOW ("qofquery:compare", pd->how, LT, LTE, EQUAL, GT, GTE, NEQ);

		query_date_t pdata = (query_date_t) pd;
		
		PUT_MATCH2("qofquery:date-match", pdata->options,
		                 DATE_MATCH, NORMAL, ROUNDED);

		PUT_DATE ("qofquery:date", pdata->date);
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_INT64))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-int64");
		PUT_HOW ("qofquery:compare", pd->how, LT, LTE, EQUAL, GT, GTE, NEQ);

		query_int64_t pdata = (query_int64_t) pd;
		PUT_INT64 ("qofquery:int64", pdata->val);
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_INT32))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-int32");
		PUT_HOW ("qofquery:compare", pd->how, LT, LTE, EQUAL, GT, GTE, NEQ);

		query_int32_t pdata = (query_int32_t) pd;
		
		PUT_INT32 ("qofquery:int32", pdata->val);
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_DOUBLE))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-double");
		PUT_HOW ("qofquery:compare", pd->how, LT, LTE, EQUAL, GT, GTE, NEQ);

		query_double_t pdata = (query_double_t) pd;
		
		PUT_DBL ("qofquery:double", pdata->val);
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_BOOLEAN))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-boolean");
		PUT_HOW ("qofquery:compare", pd->how, LT, LTE, EQUAL, GT, GTE, NEQ);

		query_boolean_t pdata = (query_boolean_t) pd;
		
		PUT_BOOL ("qofquery:boolean", pdata->val);
		return topnode;
	}
	if (!safe_strcmp (pd->type_name, QOF_TYPE_CHAR))
	{
		xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:pred-char");
		/* There is no PUT_HOW for char-match */
		query_char_t pdata = (query_char_t) pd;
		
		PUT_MATCH2("qofquery:char-match", pdata->options,
		                 CHAR_MATCH, ANY, NONE);
		
		PUT_STR ("qofquery:char-list", pdata->char_list);
		return topnode;
	}
	return NULL;
}

/* ======================================================= */

static xmlNodePtr
qof_query_param_path_to_xml (GSList *param_path)
{
	xmlNodePtr topnode = xmlNewNode (NULL, "qofquery:param-path");
	GSList *n = param_path;
	for ( ; n; n=n->next)
	{
		QofIdTypeConst path = n->data;
		if (!path) continue;
		PUT_STR ("qofquery:param", path);
	}
	return topnode;
}

/* ======================================================= */

static xmlNodePtr
qof_query_one_term_to_xml (QofQueryTerm *qt)
{
	xmlNodePtr node;
	xmlNodePtr term = xmlNewNode (NULL, "qofquery:term");

	gboolean invert = qof_query_term_is_inverted (qt);
	GSList *path = qof_query_term_get_param_path (qt);
	QofQueryPredData *pd = qof_query_term_get_pred_data (qt);

	xmlNodePtr topnode = term;
	if (invert)
	{
		/* inverter becomes new top mode */
		topnode = xmlNewNode (NULL, "qofquery:invert");
		xmlAddChild (term, topnode);
	}

	node = qof_query_param_path_to_xml (path);
	if (node) xmlAddChild (topnode, node);

	node = qof_query_pred_data_to_xml (pd);
	if (node) xmlAddChild (topnode, node);

	return term;
}

/* ======================================================= */

static xmlNodePtr
qof_query_and_terms_to_xml (GList *and_terms)
{
	xmlNodePtr terms = xmlNewNode (NULL, "qofquery:and-terms");
	GList *n = and_terms;
	for ( ; n; n=n->next)
	{
		QofQueryTerm *qt = n->data;
		if (!qt) continue;

		xmlNodePtr t = qof_query_one_term_to_xml (n->data);
		if (t) xmlAddChild (terms, t);
	}
	return terms;
}

/* ======================================================= */

static xmlNodePtr
qof_query_terms_to_xml (QofQuery *q)
{
	xmlNodePtr terms = NULL;
	GList *n = qof_query_get_terms (q);

	if (!n) return NULL;
	terms = xmlNewNode (NULL, "qofquery:or-terms");

	for ( ; n; n=n->next)
	{
		xmlNodePtr andt = qof_query_and_terms_to_xml (n->data);
		if (andt) xmlAddChild (terms, andt);
	}
	return terms;
}

/* ======================================================= */

static xmlNodePtr
qof_query_sorts_to_xml (QofQuery *q)
{
	QofQuerySort *s[3];
	qof_query_get_sorts (q, &s[0], &s[1], &s[2]);

	if (NULL == s[0]) return NULL;

	xmlNodePtr sortlist = xmlNewNode (NULL, "qofquery:sort-list");
	int i;
	for (i=0; i<3; i++)
	{
		if (NULL == s[i]) continue;

		GSList *plist = qof_query_sort_get_param_path (s[i]);
		if (!plist) continue;

		xmlNodePtr sort = xmlNewNode (NULL, "qofquery:sort");
		xmlAddChild (sortlist, sort);

		xmlNodePtr topnode = sort;

		gboolean increasing = qof_query_sort_get_increasing (s[i]);
		PUT_STR ("qofquery:order", increasing ? "DESCENDING" : "ASCENDING");

		gint opt = qof_query_sort_get_sort_options (s[i]);
		PUT_INT32 ("qofquery:options", opt);

		xmlNodePtr pl = qof_query_param_path_to_xml (plist);
		if (pl) xmlAddChild (sort, pl);
	}

	return sortlist;
}

/* ======================================================= */

static void
do_qof_query_to_xml (QofQuery *q, xmlNodePtr topnode)
{
	QofIdType search_for = qof_query_get_search_for (q);
	PUT_STR ("qofquery:search-for", search_for);

	xmlNodePtr terms = qof_query_terms_to_xml(q);
	if (terms) xmlAddChild (topnode, terms);

	xmlNodePtr sorts = qof_query_sorts_to_xml (q);
	if (sorts) xmlAddChild (topnode, sorts);

	gint max_results = qof_query_get_max_results (q);
	PUT_INT32 ("qofquery:max-results", max_results);
}

/* ======================================================= */

xmlNodePtr
qof_query_to_xml (QofQuery *q)
{
	xmlNodePtr topnode;
	xmlNodePtr node;
	xmlNsPtr   ns;

	topnode = xmlNewNode(NULL, "qof:qofquery");
	xmlSetProp(topnode, "version", "1.0.1");

	// XXX path to DTD is wrong
	// ns = xmlNewNs (topnode, "file:" "/usr/share/lib" "/qofquery.dtd", "qof");

	do_qof_query_to_xml (q, topnode);

	return topnode;
}

/* =============================================================== */

#ifdef UNIT_TEST

#include <stdio.h>
#include <qof/qofsql.h>

int main (int argc, char * argv[])
{
	QofQuery *q;
	QofSqlQuery *sq;

	qof_query_init();
	qof_object_initialize ();

	static QofParam params[] = {
		{ "adate",  QOF_TYPE_DATE, NULL, NULL},
		{ "aint",   QOF_TYPE_INT32, NULL, NULL},
		{ "aint64", QOF_TYPE_INT64, NULL, NULL},
		{ "astr",   QOF_TYPE_STRING, NULL, NULL},
		{ NULL },
	};

	qof_class_register ("GncABC", NULL, params);
	sq = qof_sql_query_new();

	qof_sql_query_parse (sq, 
	    "SELECT * from GncABC WHERE aint = 123 "
	    "or not astr=\'asdf\' "
	    "and aint64 = 9876123456789;");
	// qof_sql_query_parse (sq, "SELECT * from GncABC;");
	q = qof_sql_query_get_query (sq);

	qof_query_print (q);

	xmlDocPtr doc = doc = xmlNewDoc("1.0");
	xmlNodePtr topnode = qof_query_to_xml (q);
	xmlDocSetRootElement(doc,topnode);

	xmlChar *xbuf;
	int bufsz;
	xmlDocDumpFormatMemory (doc, &xbuf, &bufsz, 1);

	printf ("%s\n", xbuf);
	xmlFree (xbuf);
	xmlFreeDoc(doc);

#if 0
printf ("duude\n");
	// xmlOutputBufferPtr xbuf = xmlAllocOutputBuffer (enc);
	xmlOutputBufferPtr xbuf = xmlOutputBufferCreateFile (stdout, NULL);
printf ("duude\n");

	xbuf = xmlOutputBufferCreateFd (1, NULL);
printf ("duude\n");
	xmlNodeDumpOutput (xbuf, NULL, topnode, 99, 99, "iso-8859-1");
	// xmlElemDump (stdout, NULL, topnode);
#endif

	return 0;
}

#endif /* UNIT_TEST */

/* ======================== END OF FILE =================== */
