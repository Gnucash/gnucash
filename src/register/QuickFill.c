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
#include "util.h"


/** PROTOTYPES ******************************************************/
static void qfInsertTextRec(QuickFill *qf, const char * text, int depth,
                            QuickFillSort sort);

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;


/********************************************************************\
\********************************************************************/
static int 
CHAR_TO_INDEX(char c)
{
  int index = toupper(c);

  if (index >= (QFNUM - 1))
    return 0;

  return index + 1;
}

/********************************************************************\
\********************************************************************/
QuickFill *
xaccMallocQuickFill(void)
{
  QuickFill *qf;
  int i;

  qf = g_new(QuickFill, 1);

  for(i = 0; i < QFNUM; i++) 
    qf->qf[i] = NULL;

  qf->text = NULL;

  return qf;
}

/********************************************************************\
\********************************************************************/
void
xaccFreeQuickFill(QuickFill *qf)
{
  int i;

  if (qf == NULL )
    return;

  for(i = 0; i < QFNUM; i++)
  {
    xaccFreeQuickFill(qf->qf[i]);
    qf->qf[i] = NULL;
  }

  if (qf->text != NULL)
    g_free(qf->text);
  qf->text = NULL;

  g_free(qf);
}

/********************************************************************\
\********************************************************************/
QuickFill *
xaccGetQuickFill(QuickFill *qf, char c)
{
  if (qf == NULL)
    return NULL;

  DEBUG("xaccGetQuickFill(): index = %d\n",CHAR_TO_INDEX(c));

  return qf->qf[CHAR_TO_INDEX(c)];
}

/********************************************************************\
\********************************************************************/
QuickFill *
xaccGetQuickFillStrLen(QuickFill *qf, const char *str, int len)
{
  if (str == NULL)
    return NULL;

  while ((*str != '\0') && (len > 0))
  {
    if (qf == NULL)
      return NULL;

    qf = qf->qf[CHAR_TO_INDEX(*str)];
    str++;
    len--;
  }

  return qf;
}

/********************************************************************\
\********************************************************************/
QuickFill *
xaccGetQuickFillStr(QuickFill *qf, const char *str)
{
  if (str == NULL)
    return NULL;

  return xaccGetQuickFillStrLen(qf, str, strlen(str));
}

/********************************************************************\
\********************************************************************/
QuickFill *
xaccGetQuickFillUniqueLen( QuickFill *qf, int * length )
{
  int last = 0;
  int count;
  int i;

  *length = 0;

  if (qf == NULL)
    return NULL;

  while (1)
  {
    count = 0;
    for(i = 0; i < QFNUM; i++)
    {
      if (qf->qf[i] != NULL)
      {
        count++;
        if (count > 1)
          return qf;

        last = i;
      }
    }

    if (count == 0)
      return qf;

    qf = qf->qf[last];
    (*length)++;
  }
}

/********************************************************************\
\********************************************************************/
void
xaccQFInsertText(QuickFill *qf, const char * text, QuickFillSort sort)
{
  qfInsertTextRec(qf, text, 0, sort);
}

/********************************************************************\
\********************************************************************/
static void
qfInsertTextRec( QuickFill *qf, const char *text, int depth,
                 QuickFillSort sort )
{
  int index;
  char *old_text;

  if (qf == NULL)
    return;

  if ((text == NULL) || (text[depth] == '\0'))
    return;

  index = CHAR_TO_INDEX(text[depth]);

  if( qf->qf[index] == NULL )
    qf->qf[index] = xaccMallocQuickFill();

  old_text = qf->qf[index]->text;

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
        qf->qf[index]->text = g_strdup (text);
        break;
      }

      /* Leave prefixes in place */
      if ((strlen(text) > strlen(old_text)) &&
          (strncmp(text, old_text, strlen(old_text)) == 0))
        break;

      g_free (old_text);
      qf->qf[index]->text = g_strdup (text);
      break;
  }

  qfInsertTextRec(qf->qf[index], text, ++depth, sort);
}

/********************** END OF FILE *********************************\
\********************************************************************/
