/********************************************************************\
 * ui-callbacks.c -- Generic callback functions for gnucash         *
 *                                                                  *
 * Copyright (C) 1997 Robin D. Clark                                *
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

#include <gnome.h>
 
#include "config.h"

#include "messages.h"
#include "top-level.h"
#include "util.h"

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_GUI;

#if 0

/********************************************************************\
 * dateCB -- ensures the data the user enters in the date field     * 
 *   is in a valid format.                                          * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         cd -                                                     *
 *         cb - the callback struct                                 * 
 * Return: none                                                     * 
\********************************************************************/
void
dateCB( Widget mw, XtPointer cd, XtPointer cb )
  {
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)cb;
  char input;

  /* TODO: ??? add support for date field accelerator keys!! */
  if( cbs->text->ptr != NULL )
    {
    input = (cbs->text->ptr)[0];
  
    switch( input )
      {
      case '/':
      case '.':
      case '-':
        /* Make sure that there is at most two separators */
        {
        String str = XmTextGetString(mw);
        int i,count=0;
        
        for( i=0; str[i] != '\0'; i++ )
          if( str[i] == input )
            count++;
        if( count >= 2 )
          cbs->doit = False;
        }
        break;

      case 0x0:
        /* if delete key (for example) is hit, then input string */
        /* will be an empty string. In such a case, allow the input */
        cbs->doit = True;
        break;

      default:
        /* only accept the input if it is a number */
        cbs->doit = isNum(input);
      }
    }
  }

/********************************************************************\
 * amountCB -- ensures the data entered in the amount field is in   * 
 *   a valid format.                                                * 
 *                                                                  * 
 * Args:   mw - the widget that called us                           * 
 *         data - callback user data                                *
 * Return: none                                                     * 
\********************************************************************/
void
amountCB(GtkWidget *w, gpointer data)
{
  XmTextVerifyCallbackStruct *cbs = (XmTextVerifyCallbackStruct *)cb;
  char input;
  
  if( cbs->text->ptr != NULL )
    {
    input = (cbs->text->ptr)[0];
  
    switch( input )
      {
      case '.':
        /* Make sure that there is only one '.' */
      {
      String str = XmTextGetString(mw);
      int i,count=0;
	
      for( i=0; str[i] != '\0'; i++ )
        if( str[i] == '.' )
          count++;
      if( count >= 1 )
        cbs->doit = False;
      }
      break;
      default:
        /* only accept the input if it is a number */
        cbs->doit = isNum(input);
      }
    }
  }

#endif

/************************* END OF FILE ******************************\
\********************************************************************/
