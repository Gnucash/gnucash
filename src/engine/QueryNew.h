/*
 * QueryNew.h -- API for finding Gnucash objects
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 */

#ifndef GNC_QUERYNEW_H
#define GNC_QUERYNEW_H

#include "GNCId.h"

/* A Query */
typedef struct querynew_s QueryNew;

/* Query Term Operators, for combining Query Terms */
typedef enum {
  QUERY_AND=1,
  QUERY_OR,
  QUERY_NAND,
  QUERY_NOR,
  QUERY_XOR
} QueryOp;


/* Standard Query Term comparitors, for how to process a query term.
 * Note that not all core types implement all comparitors
 */
typedef enum {
  COMPARE_LT = 1,
  COMPARE_LTE,
  COMPARE_EQUAL,
  COMPARE_GT,
  COMPARE_GTE,
  COMPARE_NEQ
} query_compare_t;

#define QUERY_DEFAULT_SORT	"GnucashQueryDefaultSortObject"

/* "Known" Object Parameters */
#define QUERY_PARAM_BOOK	"book"

#define SPLIT_KVP		"kvp"
#define SPLIT_GUID		"guid"
#define SPLIT_DATE_RECONCILED	"date-reconciled"
#define SPLIT_BALANCE		"balance"
#define SPLIT_CLEARED_BALANCE	"cleared-balance"
#define SPLIT_RECONCILED_BALANCE	"reconciled-balance"
#define SPLIT_MEMO		"memo"
#define SPLIT_ACTION		"action"
#define SPLIT_RECONCILE		"reconcile-flag"
#define SPLIT_AMOUNT		"amount"
#define SPLIT_SHARE_PRICE	"share-price"
#define SPLIT_VALUE		"value"
#define SPLIT_TYPE		"type"
#define SPLIT_VOIDED_AMOUNT	"voided-amount"
#define SPLIT_VOIDED_VALUE	"voided-value"
#define SPLIT_TRANS		"trans"
#define SPLIT_ACCOUNT		"account"
#define SPLIT_ACCOUNT_GUID	"account-guid" /* for guid_match_all */

#define TRANS_KVP		"kvp"
#define TRANS_GUID		"guid"
#define TRANS_NUM		"num"
#define TRANS_DESCRIPTON	"desc"
#define TRANS_DATE_ENTERED	"date-entered"
#define TRANS_DATE_POSTED	"date-posted"
#define TRANS_DATE_DUE		"date-due"
#define TRANS_TYPE		"type"
#define TRANS_VOID_STATUS	"void-p"
#define TRANS_VOID_REASON	"void-reason"
#define TRANS_VOID_TIME		"void-time"
#define TRANS_SPLITLIST		"split-list" /* for guid_match_all */

#define ACCOUNT_KVP		"kvp"
#define ACCOUNT_GUID		"guid"
#define ACCOUNT_NAME_		"name"
#define ACCOUNT_CODE_		"code"
#define ACCOUNT_DESCRIPTION_	"desc"
#define ACCOUNT_NOTES_		"notes"
#define ACCOUNT_BALANCE_	"balance"
#define ACCOUNT_CLEARED_BALANCE	"cleared-balance"
#define ACCOUNT_RECONCILED_BALANCE	"reconciled-balance"
#define ACCOUNT_TAX_RELATED	"tax-related-p"

#define BOOK_KVP		"kvp"
#define BOOK_GUID		"guid"

/* Type of Query Core Objects (String, Date, Numeric, GUID, etc. */
typedef const char * QueryCoreType;

/*
 * List of known core query types... 
 * Each core query type defines it's set of optional "comparitor qualifiers".
 */
#define QUERYCORE_STRING	"string"
typedef enum {
  STRING_MATCH_NORMAL = 1,
  STRING_MATCH_CASEINSENSITIVE
} string_match_t;

#define QUERYCORE_DATE		"date"
typedef enum {
  DATE_MATCH_NORMAL = 1,
  DATE_MATCH_ROUNDED
} date_match_t;

#define QUERYCORE_NUMERIC	"numeric"
#define QUERYCORE_DEBCRED	"debcred"
typedef enum {
  NUMERIC_MATCH_DEBIT = 1,
  NUMERIC_MATCH_CREDIT,
  NUMERIC_MATCH_ANY
} numeric_match_t;

#define QUERYCORE_GUID		"guid"
typedef enum {
  GUID_MATCH_ANY = 1,
  GUID_MATCH_NONE,
  GUID_MATCH_NULL,
  GUID_MATCH_ALL		/* You _must_ pass a GList of objects! */
} guid_match_t;

#define QUERYCORE_INT64		"gint64"
#define QUERYCORE_DOUBLE	"double"
#define QUERYCORE_BOOLEAN	"boolean"
#define QUERYCORE_KVP		"kvp"

/* A CHAR type is for a RECNCell */
#define QUERYCORE_CHAR		"character"
typedef enum {
  CHAR_MATCH_ANY = 1,
  CHAR_MATCH_NONE
} char_match_t;

/* Basic API Functions */

/* This is the general function that adds a new Query Term to a query.
 * It will find the 'obj_type' object of the search item and compare
 * the 'param_list' parameter to the predicate data via the comparitor.
 *
 * The param_list is a recursive list of parameters.  For example, you
 * can say 'split->memo' by creating a list of one element, "SPLIT_MEMO".
 * You can say 'split->account->name' by creating a list of two elements,
 * "SPLIT_ACCOUNT" and "ACCOUNT_NAME".
 *
 * For example:
 *
 * acct_name_pred_data = make_string_pred_data(STRING_MATCH_CASEINSENSITIVE,
 *					       account_name);
 * param_list = make_list (SPLIT_ACCOUNT, ACCOUNT_NAME);
 * gncQueryAddTerm (query, param_list, COMPARE_EQUAL,
 *		    acct_name_pred_data, QUERY_AND);
 */

typedef struct query_pred_data *QueryPredData_t;

void gncQueryAddTerm (QueryNew *query, GSList *param_list,
		      QueryPredData_t pred_data, QueryOp op);

void gncQuerySetBook (QueryNew *q, GNCBook *book);
void gncQueryAddGUIDMatch (QueryNew *q, GSList *param_list,
			   const GUID *guid, QueryOp op);
void gncQueryAddGUIDListMatch (QueryNew *q, GSList *param_list,
			       GList *guid_list, guid_match_t options,
			       QueryOp op);

/* Run the query:
 *
 * ex: gncQueryRun (query, GNC_ID_SPLIT);
 */
GList * gncQueryRun (QueryNew *query, GNCIdTypeConst obj_type);

QueryNew * gncQueryCreate (void);
void gncQueryDestroy (QueryNew *q);

void gncQueryClear (QueryNew *query);
void gncQueryPurgeTerms (QueryNew *q, GSList *param_list);
int gncQueryHasTerms (QueryNew *q);
int gncQueryNumTerms (QueryNew *q);
GList * gncQueryGetTerms (QueryNew *q);

QueryNew * gncQueryCopy (QueryNew *q);
QueryNew * gncQueryInvert(QueryNew *q);
QueryNew * gncQueryMerge(QueryNew *q1, QueryNew *q2, QueryOp op);

void gncQuerySetSortOrder (QueryNew *q,
			   GSList *primary_sort_params,
			   GSList *secondary_sort_params,
			   GSList *tertiary_sort_params);

void gncQuerySetSortOptions (QueryNew *q, gint prim_op, gint sec_op,
			     gint tert_op);

void gncQuerySetSortIncreasing (QueryNew *q, gboolean prim_inc,
				gboolean sec_inc, gboolean tert_inc);


void gncQuerySetMaxResults (QueryNew *q, int n);
int gncQueryGetMaxResults (QueryNew *q);

#endif /* GNC_QUERYNEW_H */
