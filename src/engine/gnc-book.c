/********************************************************************\
 * gnc-book.c -- dataset access (set of accounting books)           *
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
 * gnc-book.c
 *
 * FUNCTION:
 * Encapsulate all the information about a gnucash dataset.
 *
 * HISTORY:
 * Created by Linas Vepstas December 1998
 * Copyright (c) 1998-2001 Linas Vepstas
 * Copyright (c) 2000 Dave Peticolas
 */

#include "config.h"

#include <dlfcn.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <glib.h>

#include "Backend.h"
#include "BackendP.h"
#include "GroupP.h"
#include "SchedXaction.h"
#include "TransLog.h"
#include "engine-helpers.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb-p.h"
#include "DateUtils.h"
#include "gnc-book.h"
#include "gnc-book-p.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-event.h"
#include "gnc-event-p.h"
#include "gnc-module.h"

static short module = MOD_IO;

/* ---------------------------------------------------------------------- */

static void
gnc_book_init (GNCBook *book)
{
  Account *template_acct;

  if (!book) return;

  book->entity_table = xaccEntityTableNew ();

  xaccGUIDNew(&book->guid, book);
  xaccStoreEntity(book->entity_table, book, &book->guid, GNC_ID_BOOK);

  book->kvp_data = kvp_frame_new ();
  book->topgroup = xaccMallocAccountGroup(book);
  book->pricedb = gnc_pricedb_create();

  book->sched_xactions = NULL;
  book->sx_notsaved = FALSE;
  book->template_group = xaccMallocAccountGroup(book);

  /* FIXME: the gnc_engine_commodity_table_new() routine invokes
   * guile/scheme to load the default list of currencies.  This 
   * forces the engine to link to guile, which is an obvious 
   * architecture flaw.  */
  book->commodity_table = gnc_engine_commodity_table_new ();

  xaccGroupSetBook (book->topgroup, book);
  xaccGroupSetBook (book->template_group, book);

  book->data_tables = g_hash_table_new (g_str_hash, g_str_equal);
}

GNCBook *
gnc_book_new (void)
{
  GNCBook *book;

  book = g_new0(GNCBook, 1);
  gnc_book_init(book);

  gnc_engine_generate_event (&book->guid, GNC_EVENT_CREATE);
  return book;
}

void
gnc_book_destroy (GNCBook *book) 
{
  if (!book) return;

  gnc_engine_generate_event (&book->guid, GNC_EVENT_DESTROY);
  xaccRemoveEntity (book->entity_table, &book->guid);

  /* mark the accounts as being freed
   * to avoid tons of balance recomputations. */
  xaccGroupMarkDoFree (book->topgroup);

  xaccFreeAccountGroup (book->topgroup);
  book->topgroup = NULL;

  gnc_pricedb_destroy (book->pricedb);
  book->pricedb = NULL;

  gnc_commodity_table_destroy (book->commodity_table);
  book->commodity_table = NULL;

  /* FIXME: destroy SX data members here, too */

  xaccEntityTableDestroy (book->entity_table);
  book->entity_table = NULL;

  /* FIXME: Make sure the data_table is empty */
  g_hash_table_destroy (book->data_tables);

  xaccLogEnable();

  g_free (book);
  LEAVE(" ");
}

/* ---------------------------------------------------------------------- */

const GUID *
gnc_book_get_guid (GNCBook *book)
{
  if (!book) return NULL;
  return &book->guid;
}

kvp_frame *
gnc_book_get_slots (GNCBook *book)
{
  if (!book) return NULL;
  return book->kvp_data;
}

GNCEntityTable *
gnc_book_get_entity_table (GNCBook *book)
{
  if (!book) return NULL;
  return book->entity_table;
}

/* ---------------------------------------------------------------------- */

gnc_commodity_table *
gnc_book_get_commodity_table(GNCBook *book)
{
  if (!book) return NULL;
  return book->commodity_table;
}

AccountGroup * 
gnc_book_get_group (GNCBook *book)
{
   if (!book) return NULL;
   return book->topgroup;
}

void
gnc_book_set_group (GNCBook *book, AccountGroup *grp)
{
  if (!book) return;

  /* Do not free the old topgroup here unless you also fix
   * all the other uses of gnc_book_set_group! */

  if (book->topgroup == grp)
    return;

  xaccGroupSetBook (book->topgroup, NULL);
  xaccGroupSetBook (grp, book);

  book->topgroup = grp;
}

void
gnc_book_set_backend (GNCBook *book, Backend *be)
{
  if (!book) return;
  book->backend = be;
}

/* ---------------------------------------------------------------------- */

static gboolean
counter_thunk(Transaction *t, void *data)
{
    (*((guint*)data))++;
    return TRUE;
}

guint
gnc_book_count_transactions(GNCBook *book)
{
    guint count = 0;
    xaccGroupForEachTransaction(gnc_book_get_group(book),
                                counter_thunk, (void*)&count);
    return count;
}

/* ---------------------------------------------------------------------- */

GNCPriceDB *
gnc_book_get_pricedb(GNCBook *book)
{
  if(!book) return NULL;
  return book->pricedb;
}

void
gnc_book_set_pricedb(GNCBook *book, GNCPriceDB *db)
{
  if(!book) return;
  book->pricedb = db;
}

/* ---------------------------------------------------------------------- */

GList *
gnc_book_get_schedxactions( GNCBook *book )
{
        if ( book == NULL ) return NULL;
        return book->sched_xactions;
}

void
gnc_book_set_schedxactions( GNCBook *book, GList *newList )
{
  if ( book == NULL ) return;

  book->sched_xactions = newList;
  book->sx_notsaved = TRUE;
}

AccountGroup *
gnc_book_get_template_group( GNCBook *book )
{
  if (!book) return NULL;
  return book->template_group;
}

void
gnc_book_set_template_group (GNCBook *book, AccountGroup *templateGroup)
{
  if (!book) return;

  if (book->template_group == templateGroup)
    return;

  xaccGroupSetBook (book->template_group, NULL);
  xaccGroupSetBook (templateGroup, book);

  book->template_group = templateGroup;
}

Backend * 
xaccGNCBookGetBackend (GNCBook *book)
{
   if (!book) return NULL;
   return book->backend;
}

static void
mark_sx_clean(gpointer data, gpointer user_data)
{
  SchedXaction *sx = (SchedXaction *) data;
  xaccSchedXactionSetDirtyness(sx, FALSE);
  return;
}

static void
book_sxns_mark_saved(GNCBook *book)
{
  book->sx_notsaved = FALSE;
  g_list_foreach(gnc_book_get_schedxactions(book),
		 mark_sx_clean, 
		 NULL);
  return;
}

void
gnc_book_mark_saved(GNCBook *book)
{
  if (!book) return;

  xaccGroupMarkSaved(gnc_book_get_group(book));
  gnc_pricedb_mark_clean(gnc_book_get_pricedb(book));

  xaccGroupMarkSaved(gnc_book_get_template_group(book));
  book_sxns_mark_saved(book);
}

static gboolean
book_sxlist_notsaved(GNCBook *book)
{
  GList *sxlist;
  SchedXaction *sx;
  if(book->sx_notsaved
     ||
     xaccGroupNotSaved(book->template_group)) return TRUE;
 
  for(sxlist = book->sched_xactions;
      sxlist != NULL;
      sxlist = g_list_next(sxlist))
  {
    sx = (SchedXaction *) (sxlist->data);
    if (xaccSchedXactionIsDirty( sx ))
      return TRUE;
  }

  return FALSE;
}
  
gboolean
gnc_book_not_saved(GNCBook *book)
{
  if (!book) return FALSE;

  return(xaccGroupNotSaved(book->topgroup)
         ||
         gnc_pricedb_dirty(book->pricedb)
	 ||
	 book_sxlist_notsaved(book));
}

/* ---------------------------------------------------------------------- */

gboolean
gnc_book_equal (GNCBook *book_1, GNCBook *book_2)
{
  if (book_1 == book_2) return TRUE;
  if (!book_1 || !book_2) return FALSE;

  if (!xaccGroupEqual (gnc_book_get_group (book_1),
                       gnc_book_get_group (book_2),
                       TRUE))
  {
    PWARN ("groups differ");
    return FALSE;
  }

  if (!gnc_pricedb_equal (gnc_book_get_pricedb (book_1),
                          gnc_book_get_pricedb (book_2)))
  {
    PWARN ("price dbs differ");
    return FALSE;
  }

  if (!gnc_commodity_table_equal (gnc_book_get_commodity_table (book_1),
                                  gnc_book_get_commodity_table (book_2)))
  {
    PWARN ("commodity tables differ");
    return FALSE;
  }

  /* FIXME: do scheduled transactions and template group */

  return TRUE;
}

/* ---------------------------------------------------------------------- */

/* Store arbitrary pointers in the GNCBook for data storage extensibility */
void gnc_book_set_data (GNCBook *book, const char *key, gpointer data)
{
  if (!book || !key || !data) return;
  g_hash_table_insert (book->data_tables, (gpointer)key, data);
}

gpointer gnc_book_get_data (GNCBook *book, const char *key)
{
  if (!book || !key) return NULL;
  return g_hash_table_lookup (book->data_tables, (gpointer)key);
}
