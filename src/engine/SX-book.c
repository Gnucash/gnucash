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
#include "gnc-event.h"

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
       g_critical("cannot mix and match books freely!");
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

SchedXactions*
gnc_collection_get_schedxactions(const QofCollection *col)
{
  SchedXactions *rtn = qof_collection_get_data(col);
  // @@assert(rtn != null);
  return rtn;
}

SchedXactions*
gnc_book_get_schedxactions(QofBook *book)
{
  QofCollection *col;
  col = qof_book_get_collection(book, GNC_ID_SCHEDXACTION);
  return gnc_collection_get_schedxactions(col);
}

void
gnc_sxes_add_sx(SchedXactions *sxes, SchedXaction *sx)
{
  if (g_list_find(sxes->sx_list, sx) != NULL)
    return;
  sxes->sx_list = g_list_append(sxes->sx_list, sx);
  qof_event_gen(&sxes->inst.entity, GNC_EVENT_ITEM_ADDED, (gpointer)sx);
}

void
gnc_sxes_del_sx(SchedXactions *sxes, SchedXaction *sx)
{
  GList *to_remove;
  to_remove = g_list_find(sxes->sx_list, sx);
  if (to_remove == NULL)
    return;
  sxes->sx_list = g_list_delete_link(sxes->sx_list, to_remove);
  qof_event_gen(&sxes->inst.entity, GNC_EVENT_ITEM_REMOVED, (gpointer)sx);
}

/* ====================================================================== */
/* SX-trans stuff */

static void
mark_sx_clean(gpointer data, gpointer user_data)
{
  SchedXaction *sx = (SchedXaction *) data;
  qof_instance_mark_clean (QOF_INSTANCE(sx));
}

static void
book_sxes_setup(QofBook *book)
{
     QofCollection *col;
     SchedXactions *sxes;

     col = qof_book_get_collection(book, GNC_ID_SCHEDXACTION);
     sxes = g_new (SchedXactions, 1);
     qof_instance_init(&sxes->inst, GNC_ID_SXES, book);
     sxes->sx_list = NULL;
     sxes->sx_notsaved = TRUE;
     qof_collection_set_data(col, sxes);
}

static void
book_sxns_mark_saved(QofCollection *col)
{
  SchedXactions *sxl;
  sxl = gnc_collection_get_schedxactions(col);
  if (!sxl)
       return;
  sxl->sx_notsaved = FALSE;
  g_list_foreach(sxl->sx_list,
                 mark_sx_clean, 
                 NULL);
}

static gboolean
book_sxlist_notsaved(const QofCollection *col)
{
  GList *sxlist;
  SchedXactions *sxl;

  sxl = gnc_collection_get_schedxactions(col);
  if (!sxl) return FALSE;
  if((sxl && sxl->sx_notsaved)) return TRUE;
 
  for(sxlist = sxl->sx_list;
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

static QofObject sxes_object_def =
{
  interface_version: QOF_OBJECT_VERSION,
  e_type:            GNC_ID_SXES,
  type_label:        "Scheduled Transactions List",
  create:            NULL,
  book_begin:        book_sxes_setup,
  book_end:          NULL,
  is_dirty:          book_sxlist_notsaved,
  mark_clean:        book_sxns_mark_saved,
  foreach:           NULL,
  printable:         NULL,
  version_cmp:       NULL
};
  
static QofObject sxtt_object_def = 
{
  interface_version: QOF_OBJECT_VERSION,
  e_type:            GNC_ID_SXTT,
  type_label:        "Scheduled Transaction Templates",
  create:            NULL,
  book_begin:        NULL,
  book_end:          NULL,
  is_dirty:          NULL,
  mark_clean:        NULL,
  foreach:           NULL,
  printable:         NULL,
  version_cmp:       NULL,
};

gboolean 
gnc_sxtt_register (void)
{
  if (!qof_object_register(&sxes_object_def))
    return FALSE;
  if (!qof_object_register(&sxtg_object_def))
    return FALSE;
  return qof_object_register(&sxtt_object_def);
}

GList*
gnc_sx_get_sxes_referencing_account(QofBook *book, Account *acct)
{
  GList *rtn = NULL;
  const GUID *acct_guid = xaccAccountGetGUID(acct);
  GList *sx_list = gnc_book_get_schedxactions(book)->sx_list;
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
