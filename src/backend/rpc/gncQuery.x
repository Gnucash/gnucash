/*
 * FILE:
 * gncQuery.x
 *
 * FUNCTION:
 * The RPC definition for a Gnucash Query
 *
 * HISTORY:
 * Created By:	Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2001, Derek Atkins
 */

#ifndef __GNC_QUERY_X
#define __GNC_QUERY_X

#include "gncAccount.x"
#include "gncGUID.x"

#ifdef RPC_HDR
%#include "Query.h"
#endif

struct gncDatePredicateData {
  enum_t	term_type;	/* pr_type_t */
  int		sense;
  int		usestart;
  gncTimespec	start;
  int		useend;
  gncTimespec	end;
};

struct gncAmountPredicateData {
  enum_t	term_type;	/* pr_type_t */
  int		sense;
  enum_t	how;		/* amt_match_t */
  enum_t	amt_sgn;	/* amt_match_sgn_t */
  double	amount;
};

struct gncAccountPredicateData {
  enum_t		term_type;	/* pr_type_t */
  int			sense;
  enum_t		how;		/* acct_match_t */
  /* Leave out Accts; we can recreate on the server side */
  gnc_guidlist *	acct_guids;	
};

struct gncStringPredicateData {
  enum_t	term_type;	/* pr_type_t */
  int		sense;
  int		case_sens;
  int		use_regexp;
  string	matchstring<>;
  /* XXX: How do I transfer a regex_t? */
};

struct gncClearedPredicateData {
  enum_t	term_type;	/* pr_type_t */
  int		sense;
  enum_t	how;		/* cleared_match_t */
};

struct gncBalancePredicateData {
  enum_t	term_type;	/* pr_type_t */
  int		sense;
  enum_t	how;		/* balance_match_t */
};

struct gncMiscPredicateData {
  enum_t	term_type;	/* pr_type_t */
  int		sense;
  int		how;
  int		data;
}; 

union gncPredicateData switch (enum_t type) { /* pd_type_t */
 case PD_DATE:
   gncDatePredicateData		date;
 case PD_AMOUNT:
   gncAmountPredicateData	amount;
 case PD_ACCOUNT:
   gncAccountPredicateData	acct;
 case PD_STRING:
   gncStringPredicateData	str;
 case PD_CLEARED:
   gncClearedPredicateData	cleared;
 case PD_BALANCE:
   gncBalancePredicateData	balance;
 case PD_MISC:
   gncMiscPredicateData		misc;
};

struct gncQueryTerm {
  gncPredicateData	data;
  int *			p;	/* This 'predicate' is just for compatibility
				 * with Query.h.  We should really check if
				 * it's non-NULL, because we cannot easily
				 * transfer a function-pointer!
				 */
};

/* A list of QueryTerms */
struct gncQTList {
  gncQueryTerm *	qt;
  gncQTList *		next;
};

/* A list of QueryTerm Lists */
struct gncQTOrlist {
  gncQTList *	andlist;
  gncQTOrlist *	next;
};

/* See Query.c for the description */
struct gncQuery {
  gncQTOrlist *		terms;
  enum_t		primary_sort; /* sort_type_t */
  enum_t		secondary_sort; /* sort_type_t */
  enum_t		tertiary_sort; /* sort_type_t */
  bool			primary_increasing;
  bool			secondary_increasing;
  bool			tertiary_increasing;
  int			max_splits;

  /* Results cache */
  int			changed;
  enum_t		last_run_type; /* query_run_t */
  int *			acct_group; /* AccountGroup * */

  int *			split_list; /* GList * */
  int *			xtn_list; /* GList * */
};

#endif /* __GNC_QUERY_X */
