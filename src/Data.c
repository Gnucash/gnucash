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

#include "Account.h"
#include "Data.h"
#include "util.h"

/********************************************************************\
 * Because I can't use C++ for this project, doesn't mean that I    *
 * can't pretend too!  These functions perform actions on the       *
 * AccountGroup data structure, in order to encapsulate the         *
 * knowledge of the internals of the AccountGroup in one file.      *
\********************************************************************/

AccountGroup *topgroup = 0x0;

/********************************************************************\
\********************************************************************/
AccountGroup *
mallocAccountGroup( void )
  {
  AccountGroup *grp = (AccountGroup *)_malloc(sizeof(AccountGroup));
  
  grp->saved   = True;
  grp->new     = False;
  
  grp->numAcc  = 0;
  grp->account = NULL;
  
  grp->numGroups  = 0;
  grp->groups = NULL;
  
  return grp;
  }

/********************************************************************\
\********************************************************************/
void
freeAccountGroup( AccountGroup *grp )
  {
  if( grp != NULL )
    {
    int i;
    
    for( i=0; i<grp->numAcc; i++ )
      freeAccount( grp->account[i] );
    
    for( i=0; i<grp->numGroups; i++ )
      freeAccountGroup( (AccountGroup *) grp->groups[i] );
    
    _free( grp->account );
    
    _free(grp);
    }
  }

/********************************************************************\
\********************************************************************/
Account *
getAccount( AccountGroup *grp, int num )
  {
  if( grp != NULL )
    {
    if( (0 <= num) && (num < grp->numAcc) )
      return grp->account[num];
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
  AccountGroup * grp;
  Account *peer_acc;
  int i;

  if (NULL == acc) return NULL;
  if (-1 >= acc_id) return NULL;

  grp = (AccountGroup *) acc->parent;

  for (i=0; i<grp->numAcc; i++) {
    peer_acc = grp->account[i];
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
  AccountGroup * grp;
  Account *peer_acc;
  int i;

  if (NULL == acc) return NULL;
  if (NULL == name) return NULL;

  grp = (AccountGroup *) acc->parent;

  for (i=0; i<grp->numAcc; i++) {
    peer_acc = grp->account[i];
    if (!strcmp(peer_acc->accountName, name)) return peer_acc;
  }

  return NULL;
}

/********************************************************************\
\********************************************************************/
Account *
removeAccount( AccountGroup *grp, int num )
  {
  Account *acc = NULL;
  
  if( grp != NULL )
    {
    int i,j;
    Account **oldAcc = grp->account;

    grp->saved = False;
    
    grp->numAcc--;
    grp->account = (Account **)_malloc((grp->numAcc)*sizeof(Account *));
    
    acc = oldAcc[grp->numAcc];    /* In case we are deleting last in
				    * old array */
    for( i=0,j=0; i<grp->numAcc; i++,j++ )
      {
      if( j != num )
        grp->account[i] = oldAcc[j];
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
insertAccount( AccountGroup *grp, Account *acc )
  {
  int i=-1;
  Account **oldAcc;
  
  if (NULL == grp) return -1;
  if (NULL == acc) return -1;

  /* set back-pointer to the accounts parent */
  acc->parent = (struct _account_group *) grp;

  oldAcc = grp->account;
    
  grp->saved = False;
  
  grp->numAcc++;
  grp->account = (Account **)_malloc((grp->numAcc)*sizeof(Account *));
  
  for( i=0; i<(grp->numAcc-1); i++ )
    grp->account[i] = oldAcc[i];
  
  grp->account[i] = acc;
  
  _free(oldAcc);

  return i;
  }


/****************** END OF FILE *************************************/
