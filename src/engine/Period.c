/*
 * FILE:
 * Period.c
 *
 * FUNCTION:
 * Implement accounting periods.
 *
 * CAUTION: this is currently a non-functioning, experimental implementation
 * of the design described in src/doc/book.txt
 *
 * HISTORY:
 * created by Linas Vepstas November 2001
 * Copyright (c) 2001 Linas Vepstas <linas@linas.org>
 */


#ifndef XACC_PERIOD_H__
#define XACC_PERIOD_H__

#include "gnc-book.h"
#include "gnc-engine.h"
#include "Query.h"

/* The gnc_book_partition() uses the result of the indicated query
 *    to partition an existing book into two parts.  It returns 
 *    a newly created book, containing a copy of all of the accounts,
 *    and it moves all of the transactions returned by the query to 
 *    the copied accounts.  I

An equity transfer is made ...
key-value pairs are set ...
The sched xactions are not copied ...

 */
GNCBook * gnc_book_partition (GNCBook *, Query *);

#endif XACC_PERIOD_H__


GNCBook * 
gnc_book_partition (GNCBook *existing_book, Query *query)
{
   GNCBook *partition_book;
   if (!existing_book || !query) return NULL;

   
   partition_book = gnc_book_new();

   return partition_book;
}


/* ============================= END OF FILE ====================== */
