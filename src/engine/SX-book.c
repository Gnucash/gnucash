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

#include "gnc-book.h"
#include "gnc-book-p.h"
#include "gnc-engine.h"
#include "gnc-trace.h"
#include "Group.h"
#include "GroupP.h"
#include "SchedXaction.h"
#include "SX-book.h"
#include "SX-book-p.h"

static short module = MOD_SX;

/* ====================================================================== */

#define GNC_SCHEDXACTIONS "gnc_schedxactions"
SchedXactions *
gnc_book_get_schedxaction_list( GNCBook *book )
{
  if ( book == NULL ) return NULL;
  return gnc_book_get_data (book, GNC_SCHEDXACTIONS);
}

GList *
gnc_book_get_schedxactions( GNCBook *book )
{
  SchedXactions *list;
  if ( book == NULL ) return NULL;
  list = gnc_book_get_data (book, GNC_SCHEDXACTIONS);
  if (list) return list->sx_list;
  return NULL;
}

void
gnc_book_set_schedxactions( GNCBook *book, GList *newList )
{
  SchedXactions *old_list, *new_list;
  if ( book == NULL ) return;

  old_list = gnc_book_get_data (book, GNC_SCHEDXACTIONS);
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
  
  gnc_book_set_data (book, GNC_SCHEDXACTIONS, new_list);

  g_free (old_list);
}

/* ====================================================================== */

#define GNC_TEMPLATE_GROUP "gnc_template_group"
AccountGroup *
gnc_book_get_template_group( GNCBook *book )
{
  if (!book) return NULL;
  return gnc_book_get_data (book, GNC_TEMPLATE_GROUP);
}

void
gnc_book_set_template_group (GNCBook *book, AccountGroup *templateGroup)
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

  gnc_book_set_data (book, GNC_TEMPLATE_GROUP, templateGroup);

  xaccAccountGroupBeginEdit (old_grp);
  xaccAccountGroupDestroy (old_grp);
}

/* ========================== END OF FILE =============================== */
