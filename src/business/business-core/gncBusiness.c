/*
 * gncBusiness.c -- Business helper functions 
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#include "config.h"

#include <glib.h>

#include "gncBusiness.h"

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
  GHashTable *ht;
  struct _iterate iter;

  if (!book || !cb) return;

  iter.cb = cb;
  iter.user_data = user_data;

  ht = gnc_book_get_data (book, mod_name);
  if (ht)
    g_hash_table_foreach (ht, get_list, &iter);
}
