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
 * along with this program; if not, write to the Free Software      *
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.        *
 *                                                                  *
 *   Author: Rob Clark                                              *
 * Internet: rclark@cs.hmc.edu                                      *
 *  Address: 609 8th Street                                         *
 *           Huntington Beach, CA 92648-4632                        *
\********************************************************************/

#include <ctype.h>
#include <string.h>

#include "config.h"

#include "QuickFill.h"
#include "util.h"

/** PROTOTYPES ******************************************************/
static void qfInsertTextRec( QuickFill *qf, const char * text, int depth );

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
  c = toupper(c)-0x40;
  if( (c & 0x80) || (c >= QFNUM) )
    return 0;
  else
    return c;
  }

/********************************************************************\
\********************************************************************/
QuickFill *
xaccMallocQuickFill( void )
  {
  int i;
  QuickFill *qf = (QuickFill *)malloc(sizeof(QuickFill));
  
  for( i=0; i<QFNUM; i++ ) 
    {
    qf->qf[i] = NULL;
    }
  
  qf->text = NULL;
  
  return qf;
  }

/********************************************************************\
\********************************************************************/
void
xaccFreeQuickFill( QuickFill *qf )
  {
  if( qf != NULL )
    {
    int i;
    
    for( i=0; i<QFNUM; i++ )
      {
      xaccFreeQuickFill( qf->qf[i] );
      }

    free(qf->text);
    free(qf);
    }
  }

/********************************************************************\
\********************************************************************/
QuickFill *
xaccGetQuickFill( QuickFill *qf, char c )
  {
  if( qf != NULL )
    {
    DEBUG("xaccGetQuickFill(): index = %d\n",CHAR_TO_INDEX(c));
    return qf->qf[CHAR_TO_INDEX(c)];
    }
  else
    return NULL;
  }

/********************************************************************\
\********************************************************************/
void
xaccQFInsertText( QuickFill *qf, const char * text )
  {
  qfInsertTextRec( qf, text, 0 );
  }

/********************************************************************\
\********************************************************************/
static void
qfInsertTextRec( QuickFill *qf, const char *text, int depth )
  {
  if (NULL == qf) return;

  if( text )
    {
    if( text[depth] != '\0' )
      {
      int index = CHAR_TO_INDEX( text[depth] );
      
      if( qf->qf[index] == NULL )
        {
        qf->qf[index] = xaccMallocQuickFill();
        }

      /* store text in LIFO order, so that recent
       * stuff shows up before old stuff */
      if (qf->qf[index]->text) free (qf->qf[index]->text);
      qf->qf[index]->text = strdup (text);
      
      qfInsertTextRec( qf->qf[index], text, ++depth );
      }
    }
  }

/********************** END OF FILE *********************************\
\********************************************************************/
