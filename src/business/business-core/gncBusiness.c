/*
 * gncBusiness.c -- Business helper functions 
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "gncBusiness.h"
#include "qofbook.h"


void gncBusinessCreate (QofBook *book, QofIdType mod_name)
{
  GncBookInfo *bi;

  if (!book) return;

  bi = g_new0 (GncBookInfo, 1);
  qof_book_set_data (book, mod_name, bi);
}

void gncBusinessDestroy (QofBook *book, QofIdType mod_name)
{
  GncBookInfo *bi;

  if (!book) return;

  bi = qof_book_get_data (book, mod_name);

  g_free (bi);
}

gboolean gncBusinessIsDirty (QofBook *book, QofIdType mod_name)
{
  GncBookInfo *bi;

  if (!book) return FALSE;

  bi = qof_book_get_data (book, mod_name);
  return bi->is_dirty;
}

void gncBusinessSetDirtyFlag (QofBook *book, QofIdType mod_name,
			      gboolean is_dirty)
{
  GncBookInfo *bi;

  if (!book) return;

  bi = qof_book_get_data (book, mod_name);
  bi->is_dirty = is_dirty;
}

