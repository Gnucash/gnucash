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

#include <ctype.h>
#include <string.h>

#include "config.h"

#include "QuickFill.h"
#include "gnc-engine-util.h"


struct _QuickFill
{
  char * text;            /* the first matching text string     */
  GHashTable *matches;    /* array of children in the tree      */
};


/** PROTOTYPES ******************************************************/
static void quickfill_insert_recursive (QuickFill *qf, const char *text,
                                        int depth, QuickFillSort sort);

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;


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

  qf = g_new (QuickFill, 1);

  qf->text = NULL;
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

  g_free(qf->text);
  qf->text = NULL;

  g_free(qf);
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
gnc_quickfill_get_char_match (QuickFill *qf, char c)
{
  guint key = toupper(c);

  if (qf == NULL)
    return NULL;

  DEBUG ("xaccGetQuickFill(): index = %u\n", key);

  return g_hash_table_lookup (qf->matches, GUINT_TO_POINTER (key));
}

/********************************************************************\
\********************************************************************/
QuickFill *
gnc_quickfill_get_string_len_match (QuickFill *qf, const char *str, int len)
{
  if (str == NULL)
    return NULL;

  while ((*str != '\0') && (len > 0))
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
gnc_quickfill_get_string_match (QuickFill *qf, const char *str)
{
  if (str == NULL)
    return NULL;

  return gnc_quickfill_get_string_len_match (qf, str, strlen(str));
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
gnc_quickfill_get_unique_len_match (QuickFill *qf, int * length)
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
gnc_quickfill_insert (QuickFill *qf, const char * text, QuickFillSort sort)
{
  quickfill_insert_recursive (qf, text, 0, sort);
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

  if (qf == NULL)
    return;

  if ((text == NULL) || (text[depth] == '\0'))
    return;

  key = toupper(text[depth]);

  match_qf = g_hash_table_lookup (qf->matches, GUINT_TO_POINTER (key));
  if (match_qf == NULL)
  {
    match_qf = gnc_quickfill_new ();
    g_hash_table_insert (qf->matches, GUINT_TO_POINTER (key), match_qf);
  }

  old_text = match_qf->text;

  switch(sort)
  {
    case QUICKFILL_ALPHA:
      if ((old_text != NULL) &&
          (safe_strcmp(text, old_text) >= 0))
        break;
    case QUICKFILL_LIFO:
    default:
      /* If there's no string there already, just put the new one in. */
      if (old_text == NULL)
      {
        match_qf->text = g_strdup (text);
        break;
      }

      /* Leave prefixes in place */
      if ((strlen(text) > strlen(old_text)) &&
          (strncmp(text, old_text, strlen(old_text)) == 0))
        break;

      g_free (old_text);
      match_qf->text = g_strdup (text);
      break;
  }

  quickfill_insert_recursive (match_qf, text, ++depth, sort);
}

/********************** END OF FILE *********************************\
\********************************************************************/
