/********************************************************************\
 * Data.c -- the main data structure of the program                 *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997 Linas Vepstas                                 *
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

#include "Data.h"
#include "Account.h"
#include "util.h"

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * Data data structure, in order to encapsulate the knowledge       *
 * of the internals of the Data in one file.                        *
\********************************************************************/

/********************************************************************\
\********************************************************************/
Data *
mallocData( void )
  {
  Data *data = (Data *)_malloc(sizeof(Data));
  
  data->saved   = True;
  data->new     = False;
  
  data->numAcc  = 0;
  data->account = NULL;
  
  return data;
  }

/********************************************************************\
\********************************************************************/
void
freeData( Data *data )
  {
  if( data != NULL )
    {
    int i;
    
    for( i=0; i<data->numAcc; i++ )
      freeAccount( data->account[i] );
    
    _free( data->account );
    
    _free(data);
    }
  }

/********************************************************************\
\********************************************************************/
Account *
getAccount( Data *data, int num )
  {
  if( data != NULL )
    {
    if( (0 <= num) && (num < data->numAcc) )
      return data->account[num];
    else
      return NULL;
    }
  else
    return NULL;
  }

/********************************************************************\
 * Fetch an account, given only it's ID number                      *
\********************************************************************/

Account *
xaccGetPeerAccountFromID ( Account *acc, int acc_id )
{
  Data * data;
  Account *peer_acc;
  int i;

  if (NULL == acc) return NULL;
  if (-1 >= acc_id) return NULL;

  data = (Data *) acc->data;

  for (i=0; i<data->numAcc; i++) {
    peer_acc = data->account[i];
    if (acc_id == peer_acc->id) return peer_acc;
  }

  return NULL;
}

/********************************************************************\
 * Fetch an account, given it's name                                *
\********************************************************************/

Account *
xaccGetPeerAccountFromName ( Account *acc, char * name )
{
  Data * data;
  Account *peer_acc;
  int i;

  if (NULL == acc) return NULL;
  if (NULL == name) return NULL;

  data = (Data *) acc->data;

  for (i=0; i<data->numAcc; i++) {
    peer_acc = data->account[i];
    if (!strcmp(peer_acc->accountName, name)) return peer_acc;
  }

  return NULL;
}

/********************************************************************\
\********************************************************************/
Account *
removeAccount( Data *data, int num )
  {
  Account *acc = NULL;
  
  if( data != NULL )
    {
    int i,j;
    Account **oldAcc = data->account;

    data->saved = False;
    
    data->numAcc--;
    data->account = (Account **)_malloc((data->numAcc)*sizeof(Account *));
    
    acc = oldAcc[data->numAcc];    /* In case we are deleting last in
				    * old array */
    for( i=0,j=0; i<data->numAcc; i++,j++ )
      {
      if( j != num )
        data->account[i] = oldAcc[j];
      else
        {
        acc = oldAcc[j];
        j--;
        }
      }
    
    _free(oldAcc);
    }
  return acc;
  }

/********************************************************************\
\********************************************************************/
int
insertAccount( Data *data, Account *acc )
  {
  int i=-1;
  Account **oldAcc;
  
  if (NULL == data) return -1;
  if (NULL == acc) return -1;

  /* set back-pointer to the accounts parent */
  acc->data = (struct _data *) data;

  oldAcc = data->account;
    
  data->saved = False;
  
  data->numAcc++;
  data->account = (Account **)_malloc((data->numAcc)*sizeof(Account *));
  
  for( i=0; i<(data->numAcc-1); i++ )
    data->account[i] = oldAcc[i];
  
  data->account[i] = acc;
  
  _free(oldAcc);

  return i;
  }


/****************** END OF FILE *************************************/
