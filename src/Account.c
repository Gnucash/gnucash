/********************************************************************\
 * Account.c -- the Account data structure                          *
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

#include "util.h"
#include "main.h"
#include "Data.h"
#include "Account.h"
#include "date.h"

extern Data *data;

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * account data structure, in order to encapsulate the knowledge    *
 * of the internals of the Account in one file.                     *
\********************************************************************/

/********************************************************************\
\********************************************************************/
Account *
mallocAccount( void )
  {
  Account *acc = (Account *)_malloc(sizeof(Account));
  
  acc->flags = 0;
  acc->type  = 0;
  
  acc->accountName = NULL;
  acc->description = NULL;
  acc->notes       = NULL;
  
  acc->regData  = NULL;
  acc->recnData = NULL;
  acc->adjBData = NULL;
  acc->qfRoot   = mallocQuickFill();
  
  acc->numTrans    = 0;
  acc->transaction = NULL;         /* Initially there are no transactions
                                    * in this account's transaction
                                    * array */
  
  return acc;
  }

/********************************************************************\
\********************************************************************/
void
freeAccount( Account *acc )
  {
  if( acc != NULL )
    {
    int i;
    
    XtFree(acc->accountName);
    XtFree(acc->description);
    XtFree(acc->notes);
    
    freeQuickFill(acc->qfRoot);
    
    for( i=0; i<acc->numTrans; i++ )
      _free( acc->transaction[i] );
    
    _free( acc->transaction );
    
    _free(acc);
    }
  }

/********************************************************************\
\********************************************************************/
Transaction *
getTransaction( Account *acc, int num )
  {
  if( acc != NULL )
    {
    if( (0 <= num) && (num < acc->numTrans) )
      return acc->transaction[num];
    else
      return NULL;
    }
  else
    return NULL;
  }

/********************************************************************\
\********************************************************************/
Transaction *
removeTransaction( Account *acc, int num )
  {
  Transaction *trans = NULL;
  if( acc != NULL )
    {
    int  i,j;
    Transaction **oldTrans = acc->transaction;

    /* Set this flag, so we know we need to save the data file: */
    if( data != NULL )
      data->saved = False;
    
    acc->numTrans--;
    acc->transaction = (Transaction **)_malloc((acc->numTrans)*
                                               sizeof(Transaction *));
    
    trans = oldTrans[acc->numTrans];/* In case we are deleting last in
                                     * old array */
    for( i=0,j=0; i<acc->numTrans; i++,j++ )
      {
      if( j != num )
        acc->transaction[i] = oldTrans[j];
      else
        {
        trans = oldTrans[j];
        i--;
        }
      }
      
    _free(oldTrans);
    }
  return trans;
  }

/********************************************************************\
\********************************************************************/
int
insertTransaction( Account *acc, Transaction *trans )
  {
  int position=-1;

  if( acc != NULL )
    {
    int  i,j;
    Date *dj,*dt;
    int  inserted = False;
    Transaction **oldTrans = acc->transaction;
    
    /* mark the data file as needing to be saved: */
    if( data != NULL )
      data->saved = False;
    
    acc->numTrans++;
    acc->transaction = (Transaction **)_malloc((acc->numTrans)*
                                               sizeof(Transaction *));
    
    /* dt is the date of the transaction we are inserting, and dj
     * is the date of the "cursor" transaction... we want to insert
     * the new transaction before the first transaction of the same
     * or later date.  The !inserted bit is a bit of a kludge to 
     * make sure we only insert the new transaction once! */
    dt = &(trans->date);
    for( i=0,j=0; i<acc->numTrans; i++,j++ )
      {
      /* if we didn't do this, and we needed to insert into the
       * last spot in the array, we would walk off the end of the
       * old array, which is no good! */
      if( j>=(acc->numTrans-1) )
        {
        position = i;
        acc->transaction[i] = trans;
        break;
        }
      else
        {
        dj = &(oldTrans[j]->date);
        if( (datecmp(dj,dt) > 0) & !inserted )
          {
          position = i;
          acc->transaction[i] = trans;
          j--;
          inserted = True;
          }
        else
          acc->transaction[i] = oldTrans[j];
        }
      }
    
    _free(oldTrans);
    }
  
  if( position != -1 )
    qfInsertTransaction( acc->qfRoot, trans );
  
  return position;
  }






