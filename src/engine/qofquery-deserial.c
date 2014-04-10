/********************************************************************\
 * qofquery-deserial.c -- Convert Qof-Query XML to QofQuery         *
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

// #include "config.h"

#include <stdlib.h>
#include <glib.h>
#include <libxml/parser.h>

#include "qofquery-deserial.h"
#include "qofquery-serialize.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "gnc-engine-util.h"

#define CACHE_INSERT(str)  \
    g_cache_insert(gnc_engine_get_string_cache(), (gpointer)(str))
#define CACHE_REMOVE(str)  \
    g_cache_remove(gnc_engine_get_string_cache(), (gpointer)(str))

/* =========================================================== */

#define GET_TEXT(node)  ({                                   \
   char * sstr = NULL;                                       \
   xmlNodePtr text;                                          \
   text = node->xmlChildrenNode;                             \
   if (text && 0 == strcmp ("text", text->name)) {           \
      sstr = text->content;                                  \
   }                                                         \
   sstr;                                                     \
})

#define GET_STR(SELF,FN,TOK)                                 \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      FN (SELF, str);                                        \
   }                                                         \
   else

#define GET_DBL(SELF,FN,TOK)                                 \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      double rate = atof (str);                              \
      FN (SELF, rate);                                       \
   }                                                         \
   else

#define GET_INT32(SELF,FN,TOK)                               \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      gint32 ival = atoi (str);                              \
      FN (SELF, ival);                                       \
   }                                                         \
   else

#define GET_INT64(SELF,FN,TOK)                               \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      gint64 ival = atoll (str);                             \
      FN (SELF, ival);                                       \
   }                                                         \
   else

#define GET_DATE(SELF,FN,TOK)                                \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      Timespec tval = gnc_iso8601_to_timespec_gmt (str);     \
      FN (SELF, tval);                                       \
   }                                                         \
   else

#define GET_BOOL(SELF,FN,TOK)                                \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      gboolean bval = qof_util_bool_to_int (str);            \
      FN (SELF, bval);                                       \
   }                                                         \
   else

#define GET_NUMERIC(SELF,FN,TOK)                             \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      gnc_numeric nval;                                      \
      string_to_gnc_numeric (str, &nval);                    \
      FN (SELF, nval);                                       \
   }                                                         \
   else

#define GET_GUID(SELF,FN,TOK)                                \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      GUID guid;                                             \
      string_to_guid (str, &guid);                           \
      FN (SELF, &guid);                                      \
   }                                                         \
   else

#define GET_HOW(VAL,TOK,A,B,C,D,E,F)                         \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      int ival = QOF_COMPARE_##A;                            \
      if (!strcmp (#A, str)) ival = QOF_COMPARE_##A;         \
      else if (!strcmp (#B, str)) ival = QOF_COMPARE_##B;    \
      else if (!strcmp (#C, str)) ival = QOF_COMPARE_##C;    \
      else if (!strcmp (#D, str)) ival = QOF_COMPARE_##D;    \
      else if (!strcmp (#E, str)) ival = QOF_COMPARE_##E;    \
      else if (!strcmp (#F, str)) ival = QOF_COMPARE_##F;    \
      VAL = ival;                                            \
   }                                                         \
   else

#define GET_MATCH2(VAL,TOK,PFX,A,B)                          \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      int ival = QOF_##PFX##_##A;                            \
      if (!strcmp (#A, str)) ival = QOF_##PFX##_##A;         \
      else if (!strcmp (#B, str)) ival = QOF_##PFX##_##B;    \
      VAL = ival;                                            \
   }                                                         \
   else

#define GET_MATCH3(VAL,TOK,PFX,A,B,C)                        \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      int ival = QOF_##PFX##_##A;                            \
      if (!strcmp (#A, str)) ival = QOF_##PFX##_##A;         \
      else if (!strcmp (#B, str)) ival = QOF_##PFX##_##B;    \
      else if (!strcmp (#C, str)) ival = QOF_##PFX##_##C;    \
      VAL = ival;                                            \
   }                                                         \
   else

#define GET_MATCH5(VAL,TOK,PFX,A,B,C,D,E)                    \
   if (0 == strcmp (TOK, node->name))                        \
   {                                                         \
      const char *str = GET_TEXT (node);                     \
      int ival = QOF_##PFX##_##A;                            \
      if (!strcmp (#A, str)) ival = QOF_##PFX##_##A;         \
      else if (!strcmp (#B, str)) ival = QOF_##PFX##_##B;    \
      else if (!strcmp (#C, str)) ival = QOF_##PFX##_##C;    \
      else if (!strcmp (#D, str)) ival = QOF_##PFX##_##D;    \
      else if (!strcmp (#E, str)) ival = QOF_##PFX##_##E;    \
      VAL = ival;                                            \
   }                                                         \
   else

/* =============================================================== */
/* Autogen the code for the simple, repetitive predicates */

#define SIMPLE_PRED_HANDLER(SUBRNAME,CTYPE,GETTER,XMLTYPE,PRED) \
static QofQueryPredData *                                       \
SUBRNAME (xmlNodePtr root)                                      \
{                                                               \
	xmlNodePtr xp = root->xmlChildrenNode;                       \
	xmlNodePtr node;                                             \
                                                                \
	QofQueryCompare how = QOF_COMPARE_EQUAL;                     \
	CTYPE val = 0;                                               \
                                                                \
	for (node=xp; node; node = node->next)                       \
	{                                                            \
		if (node->type != XML_ELEMENT_NODE) continue;             \
                                                                \
		GET_HOW (how, "qofquery:compare", LT, LTE, EQUAL, GT, GTE, NEQ); \
		GETTER (0, val=, XMLTYPE);                                \
		{}                                                        \
	}                                                            \
                                                                \
	QofQueryPredData *pred;                                      \
	pred = PRED (how, val);                                      \
	return pred;                                                 \
}

SIMPLE_PRED_HANDLER (qof_query_pred_double_from_xml,
                     double,
                     GET_DBL,
                     "qofquery:double",
	                  qof_query_double_predicate);

SIMPLE_PRED_HANDLER (qof_query_pred_int64_from_xml,
                     gint64,
                     GET_INT64,
                     "qofquery:int64",
	                  qof_query_int64_predicate);

SIMPLE_PRED_HANDLER (qof_query_pred_int32_from_xml,
                     gint32,
                     GET_INT32,
                     "qofquery:int32",
	                  qof_query_int32_predicate);

SIMPLE_PRED_HANDLER (qof_query_pred_boolean_from_xml,
                     gboolean,
                     GET_BOOL,
                     "qofquery:boolean",
	                  qof_query_boolean_predicate);

/* =============================================================== */

static void wrap_new_gint64(KvpValue **v, gint64 value) {
	*v = kvp_value_new_gint64 (value); }
static void wrap_new_double(KvpValue **v, double value) {
	*v = kvp_value_new_double (value); }
static void wrap_new_numeric(KvpValue **v, gnc_numeric value) {
	*v = kvp_value_new_gnc_numeric (value); }
static void wrap_new_string(KvpValue **v, const char * value) {
	*v = kvp_value_new_string (value); }
static void wrap_new_guid(KvpValue **v, const GUID * value) {
	*v = kvp_value_new_guid (value); }
static void wrap_new_timespec(KvpValue **v, Timespec value) {
	*v = kvp_value_new_timespec (value); }


static QofQueryPredData *
qof_query_pred_kvp_from_xml (xmlNodePtr root)
{
	xmlNodePtr xp = root->xmlChildrenNode;
	xmlNodePtr node;

	QofQueryCompare how = QOF_COMPARE_EQUAL;
	GSList *path = NULL;
	KvpValue *value = NULL;

	for (node=xp; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		GET_HOW (how, "qofquery:compare", LT, LTE, EQUAL, GT, GTE, NEQ);
		if (0 == strcmp ("qofquery:kvp-path", node->name))
		{
			const char *str = GET_TEXT (node);
			path = g_slist_append (path, (gpointer) str);
		}
		else
		GET_INT64(&value,   wrap_new_gint64,   "qofquery:int64");
		GET_DBL(&value,     wrap_new_double,   "qofquery:double");
		GET_NUMERIC(&value, wrap_new_numeric,  "qofquery:numeric");
		GET_STR(&value,     wrap_new_string,   "qofquery:string");
		GET_GUID(&value,    wrap_new_guid,     "qofquery:guid");
		GET_DATE(&value,    wrap_new_timespec, "qofquery:date");
	}

	QofQueryPredData *pred;
	pred = qof_query_kvp_predicate (how, path, value);
	g_slist_free (path);
	return pred;
}

/* =============================================================== */

static QofQueryPredData *
qof_query_pred_guid_from_xml (xmlNodePtr root)
{
	xmlNodePtr xp = root->xmlChildrenNode;
	xmlNodePtr node;
   GList *guid_list = NULL;

	QofGuidMatch sm = QOF_GUID_MATCH_ANY;

	for (node=xp; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		/* char pred doesn't have GET_HOW */
		GET_MATCH5 (sm, "qofquery:guid-match", 
		            GUID_MATCH, ANY, NONE, NULL, ALL, LIST_ANY);

		if (0 == strcmp ("qofquery:guid", node->name))
		{
			const char *str = GET_TEXT (node);
			GUID *guid = guid_malloc ();
			gboolean decode = string_to_guid (str, guid);
			if (decode)
			{
				guid_list = g_list_append (guid_list, guid);
			}
			else
			{
				guid_free (guid);
				// XXX error!  let someone know!
			}
		}
	}

	QofQueryPredData *pred;
	pred = qof_query_guid_predicate (sm, guid_list);

	/* The predicate made a copy of everything, so free our stuff */
   GList *n;
	for (n=guid_list; n; n=n->next)
	{
		guid_free (n->data);
	}
   g_list_free (guid_list);
	return pred;
}

/* =============================================================== */

static QofQueryPredData *
qof_query_pred_char_from_xml (xmlNodePtr root)
{
	xmlNodePtr xp = root->xmlChildrenNode;
	xmlNodePtr node;

	QofCharMatch sm = QOF_CHAR_MATCH_ANY;
   const char * char_list = NULL;

	for (node=xp; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		/* char pred doesn't have GET_HOW */
		GET_MATCH2 (sm, "qofquery:char-match", 
		            CHAR_MATCH, ANY, NONE);
		GET_STR (0, char_list=, "qofquery:char-list");
		{}
	}

	QofQueryPredData *pred;
	pred = qof_query_char_predicate (sm, char_list);
	return pred;
}

/* =============================================================== */

static QofQueryPredData *
qof_query_pred_numeric_from_xml (xmlNodePtr root)
{
	xmlNodePtr xp = root->xmlChildrenNode;
	xmlNodePtr node;

	QofQueryCompare how = QOF_COMPARE_EQUAL;
	QofNumericMatch sm = QOF_NUMERIC_MATCH_ANY;
   gnc_numeric num;

	for (node=xp; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		GET_HOW (how, "qofquery:compare", LT, LTE, EQUAL, GT, GTE, NEQ);
		GET_MATCH3 (sm, "qofquery:numeric-match", 
		            NUMERIC_MATCH, DEBIT, CREDIT, ANY);
		GET_NUMERIC (0, num=, "qofquery:numeric");
		{}
	}

	QofQueryPredData *pred;
	pred = qof_query_numeric_predicate (how, sm, num);
	return pred;
}

/* =============================================================== */

static QofQueryPredData *
qof_query_pred_date_from_xml (xmlNodePtr root)
{
	xmlNodePtr xp = root->xmlChildrenNode;
	xmlNodePtr node;

	QofQueryCompare how = QOF_COMPARE_EQUAL;
	QofDateMatch sm = QOF_DATE_MATCH_ROUNDED;
	Timespec date = {0,0};

	for (node=xp; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		GET_HOW (how, "qofquery:compare", LT, LTE, EQUAL, GT, GTE, NEQ);
		GET_MATCH2 (sm, "qofquery:date-match", 
		            DATE_MATCH, NORMAL, ROUNDED);
		GET_DATE (0, date=, "qofquery:date");
		{}
	}

	QofQueryPredData *pred;
	pred = qof_query_date_predicate (how, sm, date);
	return pred;
}

/* =============================================================== */

static QofQueryPredData *
qof_query_pred_string_from_xml (xmlNodePtr root)
{
	xmlNodePtr xp = root->xmlChildrenNode;
	xmlNodePtr node;

	QofQueryCompare how = QOF_COMPARE_EQUAL;
	QofStringMatch sm = QOF_STRING_MATCH_CASEINSENSITIVE;
	gboolean is_regex = FALSE;
	const char *pstr = NULL;

	for (node=xp; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		GET_HOW (how, "qofquery:compare", LT, LTE, EQUAL, GT, GTE, NEQ);
		GET_BOOL (0, is_regex=, "qofquery:is-regex");
		GET_STR (0, pstr=, "qofquery:string");
		GET_MATCH2 (sm, "qofquery:string-match", 
		            STRING_MATCH, NORMAL, CASEINSENSITIVE);
		{}
	}

	QofQueryPredData *pred;
	pred = qof_query_string_predicate (how, pstr, sm , is_regex);
	return pred;
}

/* =============================================================== */

static GSList * 
qof_query_param_path_from_xml (xmlNodePtr root)
{
	xmlNodePtr pterms = root->xmlChildrenNode;
	GSList *plist = NULL;
	xmlNodePtr node;
	for (node=pterms; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		if (0 == strcmp (node->name, "qofquery:param"))
		{
			const char *str = GET_TEXT (node);
			plist = g_slist_append (plist, CACHE_INSERT(str));
		}
	}
	return plist;
}

/* =============================================================== */

static void 
qof_query_term_from_xml (QofQuery *q, xmlNodePtr root)
{
	xmlNodePtr node;
	xmlNodePtr term = root->xmlChildrenNode;
	QofQueryPredData *pred = NULL;
	GSList *path = NULL;

	for (node=term; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;
		if (0 == strcmp (node->name, "qofquery:invert"))
		{
			QofQuery *qt = qof_query_create();
			qof_query_term_from_xml (qt, node);
			QofQuery *qinv = qof_query_invert (qt);
			qof_query_merge_in_place (q, qinv, QOF_QUERY_AND);
			qof_query_destroy (qinv);
			qof_query_destroy (qt);
			return;
		}
		else
		if (0 == strcmp (node->name, "qofquery:param-path"))
		{
			path = qof_query_param_path_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-string"))
		{
			pred = qof_query_pred_string_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-date"))
		{
			pred = qof_query_pred_date_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-numeric"))
		{
			pred = qof_query_pred_numeric_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-int32"))
		{
			pred = qof_query_pred_int32_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-int64"))
		{
			pred = qof_query_pred_int64_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-double"))
		{
			pred = qof_query_pred_double_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-boolean"))
		{
			pred = qof_query_pred_boolean_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-char"))
		{
			pred = qof_query_pred_char_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-guid"))
		{
			pred = qof_query_pred_guid_from_xml (node);
		}
		else
		if (0 == strcmp (node->name, "qofquery:pred-kvp"))
		{
			pred = qof_query_pred_kvp_from_xml (node);
		}
		else
		{
			// warning unhandled predicate type
		}
	}

	/* At this level, the terms should always be anded */
	qof_query_add_term (q, path, pred, QOF_QUERY_AND);
}

/* =============================================================== */

static void 
qof_query_and_terms_from_xml (QofQuery *q, xmlNodePtr root)
{
	xmlNodePtr andterms = root->xmlChildrenNode;
	xmlNodePtr node;
	for (node=andterms; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		if (0 == strcmp (node->name, "qofquery:term"))
		{
			qof_query_term_from_xml (q, node);
		}
	}
}

/* =============================================================== */

static void 
qof_query_or_terms_from_xml (QofQuery *q, xmlNodePtr root)
{
	xmlNodePtr andterms = root->xmlChildrenNode;
	xmlNodePtr node;

	for (node=andterms; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		if (0 == strcmp (node->name, "qofquery:and-terms"))
		{
			QofQuery *qand = qof_query_create ();
			qof_query_and_terms_from_xml (qand, node);
			qof_query_merge_in_place (q, qand, QOF_QUERY_OR);
			qof_query_destroy (qand);
		}
	}
}

/* =============================================================== */

QofQuery *
qof_query_from_xml (xmlNodePtr root)
{
	QofQuery *q;

	if (!root) return NULL;

	xmlChar * version = xmlGetProp(root, "version");
   if (!root->name || strcmp ("qof:qofquery", root->name))
   {
		// XXX something is wrong. warn ... 
      return NULL;
   }

	q = qof_query_create ();

	xmlNodePtr qpart = root->xmlChildrenNode;
	xmlNodePtr node;
	for (node=qpart; node; node = node->next)
	{
		if (node->type != XML_ELEMENT_NODE) continue;

		GET_STR   (q, qof_query_search_for,      "qofquery:search-for");
		GET_INT32 (q, qof_query_set_max_results, "qofquery:max-results");
		if (0 == strcmp (node->name, "qofquery:or-terms"))
		{
			qof_query_or_terms_from_xml (q, node);
		}
		else 
		if (0 == strcmp (node->name, "qofquery:sort-list"))
		{
// XXX unfinished  I'm bored
		}
		else 
		{
			// XXX unknown node type tell someone about it
		}
	}

	return q;
}

/* =============================================================== */

#ifdef UNIT_TEST

#include <stdio.h>
#include <qof/qofsql.h>

int main (int argc, char * argv[])
{
	QofQuery *q, *qnew;
	QofSqlQuery *sq;

	guid_init();
	qof_query_init();
	qof_object_initialize ();

	static QofParam params[] = {
		{ "adate", QOF_TYPE_DATE, NULL, NULL},
		{ "aint", QOF_TYPE_INT32, NULL, NULL},
		{ "aint64", QOF_TYPE_INT64, NULL, NULL},
		{ "aflt", QOF_TYPE_DOUBLE, NULL, NULL},
		{ "abool", QOF_TYPE_BOOLEAN, NULL, NULL},
		{ "astr", QOF_TYPE_STRING, NULL, NULL},
		{ "adate", QOF_TYPE_DATE, NULL, NULL},
		{ "anum", QOF_TYPE_NUMERIC, NULL, NULL},
		{ "achar", QOF_TYPE_CHAR, NULL, NULL},
		{ "aguid", QOF_TYPE_GUID, NULL, NULL},
		{ "akvp", QOF_TYPE_KVP, NULL, NULL},
		{ NULL },
   };

	qof_class_register ("GncABC", NULL, params);
	sq = qof_sql_query_new();

	qof_sql_query_parse (sq, 
	    "SELECT * from GncABC WHERE aint = 123 " 
	    "and not aint64 = 6123123456789 "
	    "or abool = TRUE "
	    "and not aflt >= \'3.14159265358979\' "
	    "and not astr=\'asdf\' "
	    "and adate<\'01-01-01\' "
	    "or anum<\'12301/100\' "
	    "or achar != asdf "
	    "and aguid != abcdef01234567890fedcba987654321 "
	    "and akvp != \'/some/path:abcdef01234567890fedcba987654321\' "
	    "and not akvp != \'/some/path/glop:1234\' "
	    "and akvp = \'/arf/arf/arf:10.234\' "
	    "and akvp != \'/some/other/path:qwerty1234uiop\' "
	    "and not akvp = \'/some/final/path:123401/100\' "
	    );
	// qof_sql_query_parse (sq, "SELECT * from GncABC;");
	q = qof_sql_query_get_query (sq);

	qof_query_print (q);

   xmlNodePtr topnode = qof_query_to_xml (q);

	qnew = qof_query_from_xml (topnode);
	printf ("  ------------------------------------------------------- \n");
	qof_query_print (qnew);

   /* If the before and after trees are the same, the test pases. */
	gboolean eq = qof_query_equal (q, qnew);
	printf ("Are the two equal? answer=%d\n", eq);

#define DOPRINT 1
#ifdef DOPRINT
   xmlDocPtr doc = doc = xmlNewDoc("1.0");
	xmlDocSetRootElement(doc,topnode);

	xmlChar *xbuf;
	int bufsz;
	xmlDocDumpFormatMemory (doc, &xbuf, &bufsz, 1);

	printf ("%s\n", xbuf);
	xmlFree (xbuf);
	xmlFreeDoc(doc);
#endif

	return 0;
}

#endif /* UNIT_TEST */

/* ======================== END OF FILE =================== */
