/********************************************************************\
 * QuickFill.h -- the quickfill tree data structure                 *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
 * Copyright (C) 2000 Dave Peticolas                                *
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
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <string.h>

#include "QuickFill.h"
#include "gnc-engine.h"
#include "gnc-engine.h"
#include "gnc-ui-util.h"


struct _QuickFill
{
  char *text;          /* the first matching text string     */
  int len;             /* number of chars in text string     */
  GHashTable *matches; /* array of children in the tree      */
};


/** PROTOTYPES ******************************************************/
static void quickfill_insert_recursive (QuickFill *qf, const char *text,
                                        int depth, QuickFillSort sort);

static void gnc_quickfill_remove_recursive (QuickFill *qf, const gchar *text,
                                            gint depth, QuickFillSort sort);

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_REGISTER;

/********************************************************************\
\********************************************************************/

QuickFill *
gnc_quickfill_new (void)
{
  QuickFill *qf;

  if (sizeof (guint) < sizeof (gunichar))
  {
    PWARN ("Can't use quickfill");
    return NULL;
  }

  qf = g_new (QuickFill, 1);

  qf->text = NULL;
  qf->len = 0;

  qf->matches = g_hash_table_new (g_direct_hash, g_direct_equal);

  return qf;
}

/********************************************************************\
\********************************************************************/

static gboolean
destroy_helper (gpointer key, gpointer value, gpointer data)
{
  gnc_quickfill_destroy (value);
  return TRUE;
}

void
gnc_quickfill_destroy (QuickFill *qf)
{
  if (qf == NULL)
    return;

  g_hash_table_foreach (qf->matches, (GHFunc)destroy_helper, NULL);
  g_hash_table_destroy (qf->matches);
  qf->matches = NULL;

  if (qf->text)
    CACHE_REMOVE(qf->text);
  qf->text = NULL;
  qf->len = 0;

  g_free (qf);
}

void
gnc_quickfill_purge (QuickFill *qf)
{
  if (qf == NULL)
    return;

  g_hash_table_foreach_remove (qf->matches, destroy_helper, NULL);

  if (qf->text)
    CACHE_REMOVE (qf->text);
  qf->text = NULL;
  qf->len = 0;
}

/********************************************************************\
\********************************************************************/

const char *
gnc_quickfill_string (QuickFill *qf)
{
  if (qf == NULL)
    return NULL;

  return qf->text;
}

/********************************************************************\
\********************************************************************/

QuickFill *
gnc_quickfill_get_char_match (QuickFill *qf, gunichar uc)
{
  guint key = g_unichar_toupper (uc);

  if (NULL == qf) return NULL;

  DEBUG ("xaccGetQuickFill(): index = %u\n", key);

  return g_hash_table_lookup (qf->matches, GUINT_TO_POINTER (key));
}

/********************************************************************\
\********************************************************************/

QuickFill *
gnc_quickfill_get_string_len_match (QuickFill *qf,
                                    const char *str, int len)
{
  const char *c;
  gunichar uc;

  if (NULL == qf) return NULL;
  if (NULL == str) return NULL;

  c = str;
  while (*c && (len > 0))
  {
    if (qf == NULL)
      return NULL;

    uc = g_utf8_get_char (c);
    qf = gnc_quickfill_get_char_match (qf, uc);

    c = g_utf8_next_char (c);
    len--;
  }

  return qf;
}

/********************************************************************\
\********************************************************************/

QuickFill *
gnc_quickfill_get_string_match (QuickFill *qf, const char *str)
{
  if (NULL == qf) return NULL;
  if (NULL == str) return NULL;

  return gnc_quickfill_get_string_len_match (qf, str, g_utf8_strlen (str, -1));
}

/********************************************************************\
\********************************************************************/

static void
unique_len_helper (gpointer key, gpointer value, gpointer data)
{
  QuickFill **qf_p = data;

  *qf_p = value;
}

QuickFill *
gnc_quickfill_get_unique_len_match (QuickFill *qf, int *length)
{
  if (length != NULL)
    *length = 0;

  if (qf == NULL)
    return NULL;

  while (1)
  {
    guint count;

    count = g_hash_table_size (qf->matches);

    if (count != 1)
    {
      return qf;
    }

    g_hash_table_foreach (qf->matches, unique_len_helper, &qf);

    if (length != NULL)
      (*length)++;
  }
}

/********************************************************************\
\********************************************************************/

void
gnc_quickfill_insert (QuickFill *qf, const char *text, QuickFillSort sort)
{
  gchar *normalized_str;

  if (NULL == qf) return;
  if (NULL == text) return;


  normalized_str = g_utf8_normalize (text, -1, G_NORMALIZE_NFC);
  quickfill_insert_recursive (qf, normalized_str, 0, sort);
  g_free (normalized_str);
}

/********************************************************************\
\********************************************************************/

static void
quickfill_insert_recursive (QuickFill *qf, const char *text, int depth,
                            QuickFillSort sort)
{
  guint key;
  char *old_text;
  QuickFill *match_qf;
  int len;
  char *key_char;
  gunichar key_char_uc;

  if (qf == NULL)
    return;
  
  if ((text == NULL) || (g_utf8_strlen (text, -1) <= depth))
    return;

  key_char = g_utf8_offset_to_pointer (text, depth);

  key_char_uc = g_utf8_get_char (key_char);
  key = g_unichar_toupper (key_char_uc);

  match_qf = g_hash_table_lookup (qf->matches, GUINT_TO_POINTER (key));
  if (match_qf == NULL)
  {
    match_qf = gnc_quickfill_new ();
    g_hash_table_insert (qf->matches, GUINT_TO_POINTER (key), match_qf);
  }

  old_text = match_qf->text;

  switch (sort)
  {
    case QUICKFILL_ALPHA:
      if (old_text && (g_utf8_collate (text, old_text) >= 0))
        break;

    case QUICKFILL_LIFO:
    default:
      len = g_utf8_strlen (text, -1);

      /* If there's no string there already, just put the new one in. */
      if (old_text == NULL)
      {
        match_qf->text = CACHE_INSERT((gpointer) text);
        match_qf->len = len;
        break;
      }

      /* Leave prefixes in place */
      if ((len > match_qf->len) &&
          (strncmp(text, old_text, strlen(old_text)) == 0))
        break;

      CACHE_REMOVE(old_text);
      match_qf->text = CACHE_INSERT((gpointer) text);
      match_qf->len = len;
      break;
  }

  quickfill_insert_recursive (match_qf, text, ++depth, sort);
}

/********************************************************************\
\********************************************************************/

void
gnc_quickfill_remove (QuickFill *qf, const gchar *text, QuickFillSort sort)
{
  gchar *normalized_str;

  if (qf == NULL) return;
  if (text == NULL) return;

  normalized_str = g_utf8_normalize (text, -1, G_NORMALIZE_NFC);
  gnc_quickfill_remove_recursive (qf, normalized_str, 0, sort);
  g_free (normalized_str);
}

/********************************************************************\
\********************************************************************/

struct _BestText
{
  gchar *text;
  QuickFillSort sort;
};

static void
best_text_helper (gpointer key, gpointer value, gpointer user_data)
{
  QuickFill *qf = value;
  struct _BestText *best = user_data;

  if (best->text == NULL) {
    /* start with the first text */
    best->text = qf->text;

  } else if (best->text == QUICKFILL_LIFO) {
    /* we do not track history, so ignore it */
    return;

  } else if (g_utf8_collate (qf->text, best->text) < 0) {
    /* even better text */
    best->text = qf->text;
  }
}



static void
gnc_quickfill_remove_recursive (QuickFill *qf, const gchar *text, gint depth,
                                QuickFillSort sort)
{
  QuickFill *match_qf;
  gchar *child_text;
  gint child_len;

  child_text = NULL;
  child_len = 0;

  if (depth < g_utf8_strlen (text, -1)) {
    /* process next letter */

    gchar *key_char;
    gunichar key_char_uc;
    guint key;

    key_char = g_utf8_offset_to_pointer (text, depth);
    key_char_uc = g_utf8_get_char (key_char);
    key = g_unichar_toupper (key_char_uc);

    match_qf = g_hash_table_lookup (qf->matches, GUINT_TO_POINTER (key));
    if (match_qf) {
      /* remove text from child qf */
      gnc_quickfill_remove_recursive (match_qf, text, depth + 1, sort);

      if (match_qf->text == NULL) {
        /* text was the only word with a prefix up to match_qf */
        g_hash_table_remove (qf->matches, GUINT_TO_POINTER (key));
        gnc_quickfill_destroy (match_qf);

      } else {
        /* remember remaining best child string */
        child_text = match_qf->text;
        child_len = match_qf->len;
      }
    }
  }

  if (qf->text == NULL)
    return;

  if (strcmp (text, qf->text) == 0) {
    /* the currently best text is about to be removed */

    gchar *best_text = NULL;
    gint best_len = 0;

    if (child_text != NULL) {
      /* other children are pretty good as well */
      best_text = child_text;
      best_len = child_len;

    } else {
      if (g_hash_table_size (qf->matches) != 0) {
        /* otherwise search for another good text */
        struct _BestText bts;
        bts.text = NULL;
        bts.sort = sort;

        g_hash_table_foreach (qf->matches, (GHFunc) best_text_helper, &bts);
        best_text = bts.text;
        best_len = (best_text == NULL) ? 0 : g_utf8_strlen (best_text, -1);
      }
    }

    /* now replace or clear text */
    CACHE_REMOVE(qf->text);
    if (best_text != NULL) {
      qf->text = CACHE_INSERT((gpointer) best_text);
      qf->len = best_len;
    } else {
      qf->text = NULL;
      qf->len = 0;
    }
  }
}

/********************** END OF FILE *********************************   \
\********************************************************************/
