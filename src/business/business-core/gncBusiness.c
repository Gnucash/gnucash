/*
 * gncBusiness.c -- Business helper functions 
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "gncBusiness.h"
#include "gnc-book-p.h"

struct _iterate {
  foreachObjectCB cb;
  gpointer user_data;
};

static void get_list (gpointer key, gpointer item, gpointer arg)
{
  struct _iterate *iter = arg;
  iter->cb (item, iter->user_data);
}

void gncBusinessForeach (GNCBook *book, GNCIdType mod_name,
			 foreachObjectCB cb, gpointer user_data)
{
  GncBookInfo *bi;
  struct _iterate iter;

  if (!book || !cb) return;

  iter.cb = cb;
  iter.user_data = user_data;

  bi = gnc_book_get_data (book, mod_name);
  if (bi && bi->ht)
    g_hash_table_foreach (bi->ht, get_list, &iter);
}

void gncBusinessCreate (GNCBook *book, GNCIdType mod_name)
{
  GncBookInfo *bi;

  if (!book) return;

  bi = g_new0 (GncBookInfo, 1);
  bi->ht = guid_hash_table_new ();
  gnc_book_set_data (book, mod_name, bi);
}

void gncBusinessDestroy (GNCBook *book, GNCIdType mod_name)
{
  GncBookInfo *bi;

  if (!book) return;

  bi = gnc_book_get_data (book, mod_name);

  /* XXX : Destroy the objects? */
  g_hash_table_destroy (bi->ht);
  g_free (bi);
}

gboolean gncBusinessIsDirty (GNCBook *book, GNCIdType mod_name)
{
  GncBookInfo *bi;

  if (!book) return FALSE;

  bi = gnc_book_get_data (book, mod_name);
  return bi->is_dirty;
}

void gncBusinessSetDirtyFlag (GNCBook *book, GNCIdType mod_name,
			      gboolean is_dirty)
{
  GncBookInfo *bi;

  if (!book) return;

  bi = gnc_book_get_data (book, mod_name);
  bi->is_dirty = is_dirty;
}

void gncBusinessAddObject (GNCBook *book, GNCIdType mod_name,
			   gpointer obj, const GUID *guid)
{
  GncBookInfo *bi;

  xaccStoreEntity (gnc_book_get_entity_table (book), obj, guid, mod_name);
  bi = gnc_book_get_data (book, mod_name);
  g_hash_table_insert (bi->ht, (gpointer)guid, obj);
  bi->is_dirty = TRUE;
}

void gncBusinessRemoveObject (GNCBook *book, GNCIdType mod_name,
			      const GUID *guid)
{
  GncBookInfo *bi;

  xaccRemoveEntity (gnc_book_get_entity_table (book), guid);
  bi = gnc_book_get_data (book, mod_name);
  g_hash_table_remove (bi->ht, guid);
  bi->is_dirty = TRUE;
}
