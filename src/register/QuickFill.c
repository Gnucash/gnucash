/********************************************************************\
 * QuickFill.h -- the quickfill tree data structure                 *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1998 Linas Vepstas                                 *
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
static void qfInsertTextRec( QuickFill *qf, const char * text, int depth,
                             QuickFillSort sort );

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_REGISTER;

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * QuickFill tree structure, in order to encapsulate the knowledge  *
 * of the internals of QuickFill into one file.                     *
\********************************************************************/

static int 
CHAR_TO_INDEX( char c )
{
  int index = toupper(c);

  if (index >= (QFNUM - 1))
    return 0;

  return index + 1;
}

/********************************************************************\
\********************************************************************/
QuickFill *
xaccMallocQuickFill( void )
{
  int i;
  QuickFill *qf = (QuickFill *)malloc(sizeof(QuickFill));
  
  for( i=0; i<QFNUM; i++ ) 
    qf->qf[i] = NULL;
  
  qf->text = NULL;
  
  return qf;
}

/********************************************************************\
\********************************************************************/
void
xaccFreeQuickFill( QuickFill *qf )
{
  int i;

  if (qf == NULL )
    return;

  for( i=0; i<QFNUM; i++ )
  {
    xaccFreeQuickFill( qf->qf[i] );
    qf->qf[i] = NULL;
  }

  if (qf->text != NULL)
    free(qf->text);
  qf->text = NULL;

  free(qf);
}

/********************************************************************\
\********************************************************************/
QuickFill *
xaccGetQuickFill( QuickFill *qf, char c )
{
  if (qf == NULL)
    return NULL;

  DEBUG("xaccGetQuickFill(): index = %d\n",CHAR_TO_INDEX(c));
  return qf->qf[CHAR_TO_INDEX(c)];
}

/********************************************************************\
\********************************************************************/
QuickFill *
xaccGetQuickFillStrLen( QuickFill *qf, const char *str, int len )
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
xaccGetQuickFillStr( QuickFill *qf, const char *str )
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
    for( i=0; i<QFNUM; i++ )
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
xaccQFInsertText( QuickFill *qf, const char * text, QuickFillSort sort )
{
  qfInsertTextRec( qf, text, 0, sort );
}

/********************************************************************\
\********************************************************************/
static void
qfInsertTextRec( QuickFill *qf, const char *text, int depth,
                 QuickFillSort sort )
{
  if (NULL == qf) return;

  if( text )
  {
    if( text[depth] != '\0' )
    {
      int index = CHAR_TO_INDEX( text[depth] );

      if( qf->qf[index] == NULL )
        qf->qf[index] = xaccMallocQuickFill();

      switch(sort)
      {
        case QUICKFILL_ALPHA:
          if ((qf->qf[index]->text != NULL) &&
              (safe_strcmp(text, qf->qf[index]->text) >= 0))
            break;
        case QUICKFILL_LIFO:
        default:
          /* store text in LIFO order, recent
           * stuff shows up before old stuff */
          if (qf->qf[index]->text) free (qf->qf[index]->text);
          qf->qf[index]->text = strdup (text);
          break;
      }

      qfInsertTextRec( qf->qf[index], text, ++depth, sort );
    }
  }
}

/********************** END OF FILE *********************************\
\********************************************************************/
