/********************************************************************\
 * gnc-budget-book.c --  Implementation of the budget/book linkage.      *
 * Copyright (C) 04 sep 2003    Darin Willits <darin@willits.ca>    *
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

/** @file gnc-budget-book.c
 *  @brief Implementation of the book/budget linkage.
 *  @author Created by Darin Willits 04 sep 2003 
 *  @author Copyright (c) 2003 Darin Willits <darin@willits.ca>
 *
 *
 */

// Includes
#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "gnc-engine.h"
#include "gnc-trace.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"

#include "gnc-budget-book-p.h"
#include "gnc-budget-p.h"

#include "qofbook.h"
#include "qofbook-p.h"
#include "qofobject.h"

#define GNC_BUDGETS "gnc_budgets"

/* =========================================================== */

/** Retrieve the budget list structure for the given book. */
GncBudgets* gnc_book_get_budget_list( QofBook *book )
{
  if ( book == NULL ){
    return NULL;
  }
  return qof_book_get_data (book, GNC_BUDGETS);
} 

/** Retrieve the list of budgets for the given book.*/
GList* gnc_book_get_budgets( QofBook *book )
{
  GncBudgets *list;
  if ( book == NULL ){
    return NULL;
  }
  
  list = qof_book_get_data (book, GNC_BUDGETS);
  if (list){
    return list->budget_list;
  }
  return NULL;
}

/** Associate the given budget list to the book.*/
void gnc_book_set_budgets( QofBook *book, GList *newList )
{
  GncBudgets *old_list, *new_list;
  if ( book == NULL ){
    return;
  }

  old_list = qof_book_get_data (book,GNC_BUDGETS);
  if (old_list && old_list->budget_list == newList){
     /* Assume the worst, that any 'set' means the data has 
      * changed, and needs to be saved. */
     old_list->budget_notsaved = TRUE;
     return;
  }
  
  new_list = g_new (GncBudgets, 1);
  new_list->book = book;
  new_list->budget_list = newList;
  new_list->budget_notsaved = TRUE;
  
  if (NULL == newList){
      new_list->budget_notsaved = FALSE;
  }
  
  qof_book_set_data (book, GNC_BUDGETS, new_list);

  g_free (old_list);
}

/** Add the given budget to the list of budgets for the given book. */
void gnc_book_add_budget(QofBook* book, GncBudget* budget)
{
    GList* budgetList;

    if((budget == NULL) || (book == NULL)){
        return;
    }

    budgetList = gnc_book_get_budgets( book );
    budgetList = g_list_append( budgetList, budget);
    gnc_book_set_budgets( book, budgetList );
    
    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_ADD);
}

/** Delete the given budget from the list of budgets for the given book. */
void gnc_book_delete_budget(QofBook* book, GncBudget* budget)
{
    GList* budgetList;

    if((budget == NULL) || (book == NULL)){
        return;
    }

    budgetList = gnc_book_get_budgets( book );
    budgetList = g_list_remove( budgetList, budget);
    gnc_book_set_budgets( book, budgetList );
    
    gnc_engine_gen_event( &budget->inst.entity, GNC_EVENT_REMOVE );
}

/* ============================================================ */



/** ===========================================================
 * gncObject registration functions.
 */

/* This function is called when the book is first initialized.
 * Therefore we se the list of budgets to be NULL.
 */
static void budget_book_begin (QofBook *book)
{
    gnc_book_set_budgets (book, NULL);
}

/* This function is called when the book is being cleaned up.
 * Therefore we se the list of budgets to be NULL.
 */
static void budget_book_end (QofBook *book)
{
    gnc_book_set_budgets(book, NULL);
}

/* ============================================================ */
/* dirty flag stuff */

static gboolean book_budget_notsaved(QofCollection *col)
{
    /** TODO: what is this for again??*/
    return FALSE;
}
  
static void budget_mark_clean(QofCollection *col)
{
    /** TODO: and this one... hummm. */
}

/* Define the QofObject. */
static QofObject budget_object_def = 
{
    interface_version: QOF_OBJECT_VERSION,
    e_type:              GNC_ID_BUDGET,
    type_label:        "BUDGET",
    book_begin:        budget_book_begin,
    book_end:          budget_book_end,
    is_dirty:          book_budget_notsaved,
    mark_clean:        budget_mark_clean,
    foreach:           NULL,
    printable:         NULL,
};

/* Register ourselves with the engine. */
gboolean gnc_budget_register (void)
{
    return qof_object_register (&budget_object_def);
}

/* ============================================================== */
