/********************************************************************\
 * SX-book.c -- scheduled transaction dataset access                *
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
 * SX-book.c
 *
 * FUNCTION:
 * Anchor Scheduled Transaction Info into the book.
 * See src/doc/books.txt for design overview.
 *
 * HISTORY:
 * Copyright (c) 2003 Linas Vepstas <linas@linas.org>
 */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "gncObject.h"
#include "gnc-engine.h"
#include "gnc-trace.h"
#include "Group.h"
#include "GroupP.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "SX-book-p.h"
#include "qofbook.h"
#include "qofbook-p.h"

static short module = MOD_SX;

/* ====================================================================== */

#define GNC_SCHEDXACTIONS "gnc_schedxactions"
SchedXactions *
gnc_book_get_schedxaction_list( QofBook *book )
{
  if ( book == NULL ) return NULL;
  return qof_book_get_data (book, GNC_SCHEDXACTIONS);
}

GList *
gnc_book_get_schedxactions( QofBook *book )
{
  SchedXactions *list;
  if ( book == NULL ) return NULL;
  list = qof_book_get_data (book, GNC_SCHEDXACTIONS);
  if (list) return list->sx_list;
  return NULL;
}

void
gnc_book_set_schedxactions( QofBook *book, GList *newList )
{
  SchedXactions *old_list, *new_list;
  if ( book == NULL ) return;

  old_list = qof_book_get_data (book, GNC_SCHEDXACTIONS);
  if (old_list && old_list->sx_list == newList) 
  {
     /* Assume the worst, that any 'set' means the data has 
      * changed, and needs to be saved. */
     old_list->sx_notsaved = TRUE;
     return;
  }
  
  new_list = g_new (SchedXactions, 1);
  new_list->book = book;
  new_list->sx_list = newList;
  new_list->sx_notsaved = TRUE;
  if (NULL == newList) new_list->sx_notsaved = FALSE;
  
  qof_book_set_data (book, GNC_SCHEDXACTIONS, new_list);

  g_free (old_list);
}

/* ====================================================================== */

#define GNC_TEMPLATE_GROUP "gnc_template_group"
AccountGroup *
gnc_book_get_template_group( QofBook *book )
{
  if (!book) return NULL;
  return qof_book_get_data (book, GNC_TEMPLATE_GROUP);
}

void
gnc_book_set_template_group (QofBook *book, AccountGroup *templateGroup)
{
  AccountGroup *old_grp;
  if (!book) return;

  if (templateGroup && templateGroup->book != book)
  {
     PERR ("cannot mix and match books freely!");
     return;
  }

  old_grp = gnc_book_get_template_group (book);
  if (old_grp == templateGroup) return;

  qof_book_set_data (book, GNC_TEMPLATE_GROUP, templateGroup);

  xaccAccountGroupBeginEdit (old_grp);
  xaccAccountGroupDestroy (old_grp);
}


/* ====================================================================== */
/* gncObject function implementation and registration */
/* XXX Its not clear to me if the template group and the sched xactions 
 * should be treated together or not.  I got lazy, and mashed them together.
 * For right now, this works. If you feel you need to slit this up into 
 * two separate gnc Objects, that's OK with me.
 */

static void 
sxtt_book_begin (QofBook *book)
{
  gnc_book_set_schedxactions (book, NULL);
  gnc_book_set_template_group (book, xaccMallocAccountGroup(book));
}

static void 
sxtt_book_end (QofBook *book)
{
  gnc_book_set_template_group (book, NULL);
  gnc_book_set_schedxactions (book, NULL);
}

/* ====================================================================== */
/* dirty flag stuff */

static void
mark_sx_clean(gpointer data, gpointer user_data)
{
  SchedXaction *sx = (SchedXaction *) data;
  xaccSchedXactionSetDirtyness(sx, FALSE);
}

static void
book_sxns_mark_saved(QofBook *book)
{
  SchedXactions *sxl;

  sxl = gnc_book_get_schedxaction_list (book);
  if (sxl) sxl->sx_notsaved = FALSE;
  g_list_foreach(gnc_book_get_schedxactions(book),
                 mark_sx_clean, 
                 NULL);
}

static gboolean
book_sxlist_notsaved(QofBook *book)
{
  GList *sxlist;
  SchedXaction *sx;
  SchedXactions *sxl;

  sxl = gnc_book_get_schedxaction_list (book);
  if((sxl && sxl->sx_notsaved)
     ||
     xaccGroupNotSaved(gnc_book_get_template_group(book))) return TRUE;
 
  for(sxlist = gnc_book_get_schedxactions(book);
      sxlist != NULL;
      sxlist = g_list_next(sxlist))
  {
    sx = (SchedXaction *) (sxlist->data);
    if (xaccSchedXactionIsDirty( sx ))
      return TRUE;
  }

  return FALSE;
}
  
static void
sxtt_mark_clean(QofBook *book)
{
  xaccGroupMarkSaved(gnc_book_get_template_group(book));
  book_sxns_mark_saved(book);
}


static GncObject_t sxtt_object_def = 
{
  interface_version: GNC_OBJECT_VERSION,
  name:              GNC_ID_SXTT,
  type_label:        "SXTT",
  book_begin:        sxtt_book_begin,
  book_end:          sxtt_book_end,
  is_dirty:          book_sxlist_notsaved,
  mark_clean:        sxtt_mark_clean,
  foreach:           NULL,
  printable:         NULL,
};

gboolean 
gnc_sxtt_register (void)
{
  return gncObjectRegister (&sxtt_object_def);
}

/* ========================== END OF FILE =============================== */
