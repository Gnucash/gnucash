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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"

#include <ctype.h>
#include <string.h>

#include "QuickFill.h"
#include "basiccell.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"


struct _QuickFill
{
  char *text;          /* the first matching text string     */
  int len;             /* number of chars in text string     */
  GHashTable *matches; /* array of children in the tree      */
};


/** PROTOTYPES ******************************************************/
static void quickfill_insert_recursive (QuickFill *qf, const GdkWChar *text,
                                        int depth, const char *mb_text,
                                        QuickFillSort sort);

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

/* A cache for quickfill strings */
static GCache * qf_string_cache = NULL;


/********************************************************************\
\********************************************************************/
static guint
quickfill_hash (gconstpointer key)
{
  return GPOINTER_TO_UINT (key);
}

static gint
quickfill_compare (gconstpointer key1, gconstpointer key2)
{
  guint k1 = GPOINTER_TO_UINT (key1);
  guint k2 = GPOINTER_TO_UINT (key2);

  return (k1 == k2);
}

/********************************************************************\
\********************************************************************/
QuickFill *
gnc_quickfill_new (void)
{
  QuickFill *qf;

  if (sizeof (guint) < sizeof (GdkWChar))
  {
    PWARN ("Can't use quickfill");
    return NULL;
  }

  /* For now, use the engine cache. */
  if (qf_string_cache == NULL)
    qf_string_cache = gnc_engine_get_string_cache ();

  qf = g_new (QuickFill, 1);

  qf->text = NULL;
  qf->len = 0;

  qf->matches = g_hash_table_new (quickfill_hash, quickfill_compare);

  return qf;
}

/********************************************************************\
\********************************************************************/
static void
destroy_helper (gpointer key, gpointer value, gpointer data)
{
  gnc_quickfill_destroy (value);
}

void
gnc_quickfill_destroy (QuickFill *qf)
{
  if (qf == NULL)
    return;

  g_hash_table_foreach (qf->matches, destroy_helper, NULL);
  g_hash_table_destroy (qf->matches);
  qf->matches = NULL;

  if (qf->text)
    g_cache_remove (qf_string_cache, qf->text);
  qf->text = NULL;
  qf->len = 0;

  g_free (qf);
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
gnc_quickfill_get_char_match (QuickFill *qf, GdkWChar wc)
{
  guint key = islower (wc) ? toupper (wc) : wc;

  if (qf == NULL)
    return NULL;

  DEBUG ("xaccGetQuickFill(): index = %u\n", key);

  return g_hash_table_lookup (qf->matches, GUINT_TO_POINTER (key));
}

/********************************************************************\
\********************************************************************/
QuickFill *
gnc_quickfill_get_string_len_match (QuickFill *qf,
                                    const GdkWChar *str, int len)
{
  if (str == NULL)
    return NULL;

  while ((*str != 0) && (len > 0))
  {
    if (qf == NULL)
      return NULL;

    qf = gnc_quickfill_get_char_match (qf, *str);

    str++;
    len--;
  }

  return qf;
}

/********************************************************************\
\********************************************************************/
QuickFill *
gnc_quickfill_get_string_match (QuickFill *qf, const GdkWChar *str)
{
  if (str == NULL)
    return NULL;

  return gnc_quickfill_get_string_len_match (qf, str, gnc_wcslen (str));
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
      return qf;

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
  GdkWChar *wc_text;

  if (text)
  {
    if (gnc_mbstowcs (&wc_text, text) < 0)
    {
      PERR ("bad text conversion");
      return;
    }
  }
  else
    wc_text = NULL;

  quickfill_insert_recursive (qf, wc_text, 0, text, sort);

  g_free (wc_text);
}

void
gnc_quickfill_insert_wc (QuickFill *qf, const GdkWChar *text,
                         QuickFillSort sort)
{
  char *mb_text;

  if (text)
  {
    mb_text = gnc_wcstombs (text);
    if (mb_text == NULL)
    {
      PERR ("bad text conversion");
      return;
    }
  }
  else
    mb_text = NULL;

  quickfill_insert_recursive (qf, text, 0, mb_text, sort);

  g_free (mb_text);
}

/********************************************************************\
\********************************************************************/
static void
quickfill_insert_recursive (QuickFill *qf, const GdkWChar *text, int depth,
                            const char *mb_text, QuickFillSort sort)
{
  guint key;
  char *old_text;
  QuickFill *match_qf;
  int len;

  if (qf == NULL)
    return;

  if ((text == NULL) || (text[depth] == 0))
    return;

  key = islower (text[depth]) ? toupper (text[depth]) : text[depth];

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
      if (old_text && (strcoll (mb_text, old_text) >= 0))
        break;

    case QUICKFILL_LIFO:
    default:
      len = gnc_wcslen (text);

      /* If there's no string there already, just put the new one in. */
      if (old_text == NULL)
      {
        match_qf->text = g_cache_insert (qf_string_cache, (gpointer) mb_text);
        match_qf->len = len;
        break;
      }

      /* Leave prefixes in place */
      if ((len > match_qf->len) &&
          (strncmp(mb_text, old_text, strlen(old_text)) == 0))
        break;

      g_cache_remove (qf_string_cache, old_text);
      match_qf->text = g_cache_insert (qf_string_cache, (gpointer) mb_text);
      match_qf->len = len;
      break;
  }

  quickfill_insert_recursive (match_qf, text, ++depth, mb_text, sort);
}

/********************** END OF FILE *********************************\
\********************************************************************/
