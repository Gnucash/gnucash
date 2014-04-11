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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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

#include "gnc-engine.h"
#include "Group.h"
#include "GroupP.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "SX-book-p.h"

static QofLogModule log_module = GNC_MOD_SX;

/* XXX this whole file is crufty, it doesn't really use entities
 * in the most efficient/best way */

/* ====================================================================== */

AccountGroup *
gnc_collection_get_template_group( const QofCollection *col )
{
  return qof_collection_get_data (col);
}

AccountGroup *
gnc_book_get_template_group( QofBook *book )
{
  QofCollection *col;
  if (!book) return NULL;
  col = qof_book_get_collection (book, GNC_ID_SXTG);
  return gnc_collection_get_template_group (col);
}

void
gnc_collection_set_template_group (QofCollection *col,
                                   AccountGroup *templateGroup)
{
  AccountGroup *old_grp;
  if (!col) return;

  old_grp = gnc_collection_get_template_group (col);
  if (old_grp == templateGroup) return;

  qof_collection_set_data (col, templateGroup);

  xaccAccountGroupBeginEdit (old_grp);
  xaccAccountGroupDestroy (old_grp);
}


void
gnc_book_set_template_group (QofBook *book, AccountGroup *templateGroup)
{
  QofCollection *col;
  if (!book) return;

  if (templateGroup && templateGroup->book != book)
  {
     PERR ("cannot mix and match books freely!");
     return;
  }

  col = qof_book_get_collection (book, GNC_ID_SXTG);
  gnc_collection_set_template_group (col, templateGroup);
}


/* ====================================================================== */
/* gncObject function implementation and registration */

static void 
sxtg_book_begin (QofBook *book)
{
  gnc_book_set_template_group (book, xaccMallocAccountGroup(book));
}

static void 
sxtg_book_end (QofBook *book)
{
  gnc_book_set_template_group (book, NULL);
}


static gboolean
sxtg_is_dirty(const QofCollection *col)
{
  return xaccGroupNotSaved(gnc_collection_get_template_group(col));
}
  
static void
sxtg_mark_clean(QofCollection *col)
{
  xaccGroupMarkSaved(gnc_collection_get_template_group(col));
}

static QofObject sxtg_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  e_type:            GNC_ID_SXTG,
  type_label:        "Scheduled Transaction Templates",
  book_begin:        sxtg_book_begin,
  book_end:          sxtg_book_end,
  is_dirty:          sxtg_is_dirty,
  mark_clean:        sxtg_mark_clean,
  foreach:           NULL,
  printable:         NULL,
};

/* ====================================================================== */

SchedXactions *
gnc_collection_get_schedxaction_list(const QofCollection *col)
{
  return qof_collection_get_data (col);
}

GList *
gnc_collection_get_schedxactions(const QofCollection *col)
{
  SchedXactions *list;
  list = qof_collection_get_data (col);
  if (list) return list->sx_list;
  return NULL;
}

GList *
gnc_book_get_schedxactions(QofBook *book)
{
  QofCollection *col;
  col = qof_book_get_collection (book, GNC_ID_SXTT);
  return gnc_collection_get_schedxactions (col);
}

void
gnc_collection_set_schedxactions( QofCollection *col, GList *newList )
{
  SchedXactions *old_list, *new_list;
  if ( col == NULL ) return;

  old_list = qof_collection_get_data (col);
  if (old_list && old_list->sx_list == newList) 
  {
     /* Assume the worst, that any 'set' means the data has 
      * changed, and needs to be saved. */
     old_list->sx_notsaved = TRUE;
     return;
  }
  
  new_list = g_new (SchedXactions, 1);
  new_list->sx_list = newList;
  new_list->sx_notsaved = TRUE;
  if (NULL == newList) new_list->sx_notsaved = FALSE;
  
  qof_collection_set_data (col, new_list);

  g_free (old_list);
}

void
gnc_book_set_schedxactions( QofBook *book, GList *newList )
{
  QofCollection *col;
  if ( book == NULL ) return;

  col = qof_book_get_collection (book, GNC_ID_SXTT);
  gnc_collection_set_schedxactions (col, newList);
}

/* ====================================================================== */
/* SX-trans stuff */

static void 
sxtt_book_begin (QofBook *book)
{
  gnc_book_set_schedxactions (book, NULL);
}

static void 
sxtt_book_end (QofBook *book)
{
  gnc_book_set_schedxactions (book, NULL);
}
static void
mark_sx_clean(gpointer data, gpointer user_data)
{
  SchedXaction *sx = (SchedXaction *) data;
  qof_instance_mark_clean (QOF_INSTANCE(sx));
}

static void
book_sxns_mark_saved(QofCollection *col)
{
  SchedXactions *sxl;

  sxl = gnc_collection_get_schedxaction_list (col);
  if (sxl) sxl->sx_notsaved = FALSE;
  g_list_foreach(gnc_collection_get_schedxactions(col),
                 mark_sx_clean, 
                 NULL);
}

static gboolean
book_sxlist_notsaved(const QofCollection *col)
{
  GList *sxlist;
  SchedXactions *sxl;

  sxl = gnc_collection_get_schedxaction_list (col);
  if((sxl && sxl->sx_notsaved)) return TRUE;
 
  for(sxlist = gnc_collection_get_schedxactions(col);
      sxlist != NULL;
      sxlist = g_list_next(sxlist))
  {
    SchedXaction *sx;
    sx = (SchedXaction *) (sxlist->data);
    if (xaccSchedXactionIsDirty( sx ))
      return TRUE;
  }

  return FALSE;
}
  
static QofObject sxtt_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  e_type:            GNC_ID_SXTT,
  type_label:        "Scheduled Transaction Templates",
  create:            NULL,
  book_begin:        sxtt_book_begin,
  book_end:          sxtt_book_end,
  is_dirty:          book_sxlist_notsaved,
  mark_clean:        book_sxns_mark_saved,
  foreach:           NULL,
  printable:         NULL,
  version_cmp:       NULL,
};

gboolean 
gnc_sxtt_register (void)
{
  return qof_object_register (&sxtg_object_def);
  return qof_object_register (&sxtt_object_def);
}

GList*
gnc_sx_get_sxes_referencing_account(QofBook *book, Account *acct)
{
  GList *rtn = NULL;
  const GUID *acct_guid = xaccAccountGetGUID(acct);
  GList *sx_list = gnc_book_get_schedxactions(book);
  for (; sx_list != NULL; sx_list = sx_list->next)
  {
    SchedXaction *sx = (SchedXaction*)sx_list->data;
    GList *splits = xaccSchedXactionGetSplits(sx);
    for (; splits != NULL; splits = splits->next)
    {
      Split *s = (Split*)splits->data;
      KvpFrame *frame = kvp_frame_get_frame(xaccSplitGetSlots(s), GNC_SX_ID);
      GUID *sx_split_acct_guid = kvp_frame_get_guid(frame, GNC_SX_ACCOUNT);
      if (guid_equal(acct_guid, sx_split_acct_guid))
      {
        rtn = g_list_append(rtn, sx);
      }
    }
  }
  return rtn;
}

/* ========================== END OF FILE =============================== */
