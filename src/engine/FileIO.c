/********************************************************************\
 * FileIO.c -- read from and writing to a datafile for xacc         *
 *             (X-Accountant)                                       *
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
 *                                                                  *
 *                                                                  *
 * NOTE: the readxxxx/writexxxx functions changed the current       *
 *       position in the file, and so the order which these         *
 *       functions are called in important                          *
 *                                                                  *
 * Version 1 is the original file format                            *
 *                                                                  *
 * Version 2 of the file format supports reading and writing of     *
 * double-entry transactions.                                       *
 *                                                                  *
 * Version 3 of the file format supports actions (Buy, Sell, etc.)  *
 *                                                                  *
 * Version 4 of the file format adds account groups                 *
 *                                                                  *
 * Version 5 of the file format adds splits                         *
 *                                                                  *
 *                                                                  *
 * the format of the data in the file:                              *
 *   file        ::== token Group                                   *
 *   Group       ::== numAccounts (Account)^numAccounts             *
 *   Account     ::== accID flags type accountName description      * 
 *                    notes numTran (Transaction)^numTrans          * 
 *                    numGroups (Group)^numGroups                   *
 *   Transaction ::== num date description source_split             *
 *                    numSplits (Split)^numSplits                   *
 *   Split       ::== memo action reconciled                        *
 *                     amount share_price account                   *
 *   token       ::== int  [the version of file format == VERSION]  * 
 *   numTrans    ::== int                                           * 
 *   numAccounts ::== int                                           * 
 *   accID       ::== int                                           * 
 *   flags       ::== char                                          * 
 *   type        ::== char                                          * 
 *   accountName ::== String                                        *  
 *   description ::== String                                        *  
 *   notes       ::== String                                        *  
 *                                                                  *
 *   num         ::== String                                        * 
 *   date        ::== Date                                          * 
 *   description ::== String                                        * 
 *   memo        ::== String                                        * 
 *   action      ::== String                                        * 
 *   reconciled  ::== char                                          * 
 *   amount      ::== double                                        * 
 *   share_price ::== double                                        * 
 *   account     ::== int                                           *
 *   String      ::== size (char)^size                              * 
 *   size        ::== int                                           * 
 *   Date        ::== year month day                                * 
 *   month       ::== int                                           * 
 *   day         ::== int                                           * 
 *   year        ::== int                                           * 
\********************************************************************/

#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include "config.h"

#include "Account.h"
#include "FileIO.h"
#include "Group.h"
#include "messages.h"
#include "util.h"

#define PERMS   0666
#define WFLAGS  (O_WRONLY | O_CREAT | O_TRUNC)
#define RFLAGS  O_RDONLY
#define VERSION 5

/** GLOBALS *********************************************************/

static int          error_code=0; /* error code, if error occurred */

static AccountGroup *holder;      /* temporary holder for
                                   *  unclassified accounts */
static AccountGroup *maingrp;     /* temporary holder for file
                                   * being read */

/** PROTOTYPES ******************************************************/
static Account     *locateAccount (int acc_id); 
static Account     *springAccount (int acc_id); 

static AccountGroup *readGroup( int fd, Account *, int token );
static Account      *readAccount( int fd, AccountGroup *, int token );
static Transaction  *readTransaction( int fd, Account *, int token );
static Split        *readSplit( int fd, int token );
static char         *readString( int fd, int token );
static Date         *readDate( int fd, int token );

static int writeGroup( int fd, AccountGroup *grp );
static int writeAccount( int fd, Account *account );
static int writeTransaction( int fd, Transaction *trans );
static int writeSplit( int fd, Split *split);
static int writeString( int fd, char *str );
static int writeDate( int fd, Date *date );

/*******************************************************/

int xaccGetFileIOError (void)
{
   return error_code;
}

/*******************************************************/
/* some endian stuff */

/* flip endianness of int, short, etc */
int xaccFlipInt (int val) 
  {
  unsigned int flip;
  flip = (val & 0xff000000) >> 24;
  flip |= (val & 0xff0000) >> 8;
  flip |= (val & 0xff00) << 8;
  flip |= (val & 0xff) << 24;
  return (int) flip;
}

short xaccFlipShort (short val) 
  {
  unsigned short flip;
  flip = (val & 0xff00) >> 8;
  flip |= (val & 0xff) << 8;
  return (short) flip;
}
  
double xaccFlipDouble (double val) 
  {
  union {
     unsigned int i[2];
     double d;
  } u;
  unsigned int w0, w1;
  u.d = val;
  w0 = xaccFlipInt (u.i[0]);
  w1 = xaccFlipInt (u.i[1]);

  u.i[0] = w1;
  u.i[1] = w0;
  return u.d;
}

/* if we are running on a little-endian system, we need to
 * do some endian flipping, because the xacc native data
 * format is big-endian */
#ifndef WORDS_BIGENDIAN
  #define XACC_FLIP_DOUBLE(x) { (x) = xaccFlipDouble (x); }
  #define XACC_FLIP_INT(x) { (x) = xaccFlipInt (x); }
  #define XACC_FLIP_SHORT(x) { (x) = xaccFlipShort (x); }
#else
  #define XACC_FLIP_DOUBLE(x)
  #define XACC_FLIP_INT(x) 
  #define XACC_FLIP_SHORT(x) 
#endif /* WORDS_BIGENDIAN */


/********************************************************************\
 ********************** LOAD DATA ***********************************
\********************************************************************/

/********************************************************************\
 * xaccReadAccountGroup                                                     * 
 *   reads in the data from file datafile                           *
 *                                                                  * 
 * Args:   datafile - the file to load the data from                * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/
AccountGroup *
xaccReadAccountGroup( char *datafile )
  {
  int  fd;
  int  err=0;
  int  token=0;
  int  num_unclaimed;
  AccountGroup *grp = 0x0;
  char buf[BUFSIZE];

  maingrp = 0x0;
  error_code = ERR_FILEIO_NO_ERROR;
  
  fd = open( datafile, RFLAGS, 0 );
  if( fd == -1 ) 
    {
    error_code = ERR_FILEIO_FILE_NOT_FOUND;
    return NULL;
    }
  
  /* Read in the file format token */
  err = read( fd, &token, sizeof(int) );
  if( sizeof(int) != err ) 
    {
    error_code = ERR_FILEIO_FILE_EMPTY;
    close(fd);
    return NULL;
    }
  XACC_FLIP_INT (token);
  
  /* If this is an old file, ask the user if the file
   * should be updated */
  if( VERSION > token ) {
    error_code = ERR_FILEIO_FILE_TOO_OLD;
  }
  
  /* If this is a newer file than we know how to deal
   * with, warn the user */
  if( VERSION < token ) {
    error_code = ERR_FILEIO_FILE_TOO_NEW;
    close(fd);
    return NULL;
  }
  
  holder = xaccMallocAccountGroup();
  grp = readGroup (fd, NULL, token);

  /* the number of unclaimed accounts should be zero if the 
   * read succeeded.  But just in case of a very unlikely 
   * error, try to continue anyway. */
  num_unclaimed = xaccGetNumAccounts (holder);
  if (num_unclaimed) {
    Account *acc;
    error_code = ERR_FILEIO_FILE_BAD_READ;

    /* create a lost account, put the missing accounts there */
    acc = xaccMallocAccount();
    acc -> accountName = strdup (LOST_ACC_STR);
    acc -> children = (struct _account_group *) holder;
    insertAccount (grp, acc);
  } else {
    xaccFreeAccountGroup (holder);
    holder = NULL;
  }

  maingrp = NULL;

  close(fd);
  return grp;
}

/********************************************************************\
 * readGroup                                                 * 
 *   reads in a group of accounts                                   *
 *                                                                  * 
 * Args:                                                            * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/
static AccountGroup *
readGroup (int fd, Account *aparent, int token)
  {
  int  numAcc;
  int  err=0;
  int  i;
  AccountGroup *grp = xaccMallocAccountGroup();
  
  ENTER ("readGroup");

  if (NULL == aparent) {
    maingrp = grp;
  }

  /* read numAccs */
  err = read( fd, &numAcc, sizeof(int) );
  if( sizeof(int) != err ) 
    {
    xaccFreeAccountGroup (grp);
    return NULL;
    }
  XACC_FLIP_INT (numAcc);
  
  INFO_2 ("readGroup(): expecting %d accounts \n", numAcc);

  /* read in the accounts */
  for( i=0; i<numAcc; i++ )
    {
    Account * acc = readAccount( fd, grp, token );
    if( NULL == acc ) {
      printf("Error: readGroup(): Short group read: \n");
      printf("expected %d, got %d accounts\n",numAcc,i);
      break;
      }
    }

  /* if reading an account subgroup, place the subgroup
   * into the parent account */
  grp->parent = aparent;
  if (aparent) {
    aparent->children = grp;
  }
  return grp;
}

/********************************************************************\
 * readAccount                                                      * 
 *   reads in the data for an account from the datafile             *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         acc   - the account structure to be filled in            *
 *         token - the datafile version                             * 
 * Return: error value, 0 if OK, else -1                            * 
\********************************************************************/
static Account *
readAccount( int fd, AccountGroup *grp, int token )
  {
  int err=0;
  int i;
  int numTrans, accID;
  Account *acc;
  char acctype;

  ENTER ("readAccount");
  
  /* version 1 does not store the account number */
  if (1 < token) {
    err = read( fd, &accID, sizeof(int) );
    if( err != sizeof(int) ) { return NULL; }
    XACC_FLIP_INT (accID);
    acc = locateAccount (accID);
  } else {
    acc = xaccMallocAccount();
    insertAccount (holder, acc);
  }
  
  err = read( fd, &(acc->flags), sizeof(char) );
  if( err != sizeof(char) ) { return NULL; }
  
  err = read( fd, &(acctype), sizeof(char) );
  if( err != sizeof(char) ) { return NULL; }
  acc->type = acctype;
  
  acc->accountName = readString( fd, token );
  if( acc->accountName == NULL ) { return NULL; }
  INFO_2 ("readAccount(): reading acct %s \n", acc->accountName);
  
  acc->description = readString( fd, token );
  if( acc->description == NULL ) { return NULL; }
  
  acc->notes = readString( fd, token );
  if( acc->notes == NULL ) { return NULL; }
  
  err = read( fd, &numTrans, sizeof(int) );
  if( err != sizeof(int) ) { return NULL; }
  XACC_FLIP_INT (numTrans);
  
  INFO_2 ("Info: readAccount(): expecting %d transactions \n", numTrans);
  /* read the transactions */
  for( i=0; i<numTrans; i++ )
    {
    Transaction *trans;
    trans = readTransaction( fd, acc, token );
    if( trans == NULL )
      {
      PERR ("readAccount(): Short Transaction Read: \n");
      printf (" expected %d got %d transactions \n",numTrans,i);
      break;
      }
    }
  
  springAccount (acc->id);
  insertAccount (grp, acc);

  /* version 4 is the first file version that introduces
   * sub-accounts */
  if (4 <= token) {
    int numGrps;
    err = read( fd, &numGrps, sizeof(int) );
    if( err != sizeof(int) ) { 
       return NULL; 
    }
    XACC_FLIP_INT (numGrps);
    if (numGrps) {
       readGroup (fd, acc, token);
    }
  }

  return acc;
}

/********************************************************************\
 * locateAccount
 *
 * With the double-entry system, the file may reference accounts 
 * that have not yet been read or properly parented.  Thus, we need 
 * a way of dealing with this, and this routine performs this
 * work. Basically, accounts are requested by thier id.  If an
 * account with the indicated ID does not exist, it is created
 * and placed in a temporary holding cell.  Accounts in the
 * holding cell can be located, (so that transactions can be
 * added to them) and sprung (so that they can be properly
 * parented into a group).
\********************************************************************/

static Account *
locateAccount (int acc_id) 
{
   Account * acc;
   /* negative account ids denote no account */
   if (0 > acc_id) return NULL;   

   /* first, see if we've already created the account */
   acc = xaccGetAccountFromID (maingrp, acc_id);
   if (acc) return acc;

   /* next, see if its an unclaimed account */
   acc = xaccGetAccountFromID (holder, acc_id);
   if (acc) return acc;

   /* if neither, then it does not yet exist.  Create it.
    * Put it in the drunk tank. */
   acc = xaccMallocAccount ();
   acc->id = acc_id;
   insertAccount (holder, acc);

   /* normalize the account numbers -- positive-definite.
    * That is, the unique id must never decrease,
    * nor must it overalp any existing account id */
   if (next_free_unique_account_id <= acc_id) {
      next_free_unique_account_id = acc_id+1;
   }

   return acc;
}
 
static Account *
springAccount (int acc_id) 
{
   Account * acc;
   
   /* first, see if we're confused about the account */
   acc = xaccGetAccountFromID (maingrp, acc_id);
   if (acc) {
      printf ("Internal Error: springAccount(): \n");
      printf ("account already parented \n");
      return NULL;
   }

   /* next, see if we've got it */
   acc = xaccGetAccountFromID (holder, acc_id);
   if (acc) {
      xaccRemoveAccount (acc);
      return acc;
   }

   /* if we got to here, its an error */
   printf ("Internal Error: springAccount(): \n");
   printf ("Couldn't find account \n");
   return NULL;
}
 
/********************************************************************\
 * readTransaction                                                  * 
 *   reads in the data for a transaction from the datafile          *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the transaction structure                                * 
\********************************************************************/

static Transaction *
readTransaction( int fd, Account *acc, int token )
  {
  int err=0;
  int acc_id;
  int i;
  Date *date;
  int dummy_category;
  int numSplits;
  Transaction *trans = 0x0;
  char *tmp;
  char recn;
  double num_shares = 0.0;
  double share_price = 0.0;

  /* create a transaction structure */
  trans = xaccMallocTransaction();
  
  ENTER ("readTransaction");

  trans->num = readString( fd, token );
  if( trans->num == NULL )
    {
    PERR ("Premature end of Transaction at num");
    xaccFreeTransaction(trans);
    return NULL;
    }
  
  date = readDate( fd, token );
  if( date == NULL )
    {
    PERR ("Premature end of Transaction at date");
    xaccFreeTransaction(trans);
    return NULL;
    }
  trans->date = *date;
  _free(date);
  
  trans->description = readString( fd, token );
  if( trans->description == NULL )
    {
    PERR ("Premature end of Transaction at description");
    xaccFreeTransaction(trans);
    return NULL;
    }
  
  /* At version 5, most of the transaction stuff was 
   * moved to splits. Thus, vast majority of stuff below 
   * is skipped 
   */
  if (4 >= token) { 
    tmp = readString( fd, token );
    if( NULL == tmp )
      {
      PERR ("Premature end of Transaction at memo");
      xaccFreeTransaction(trans);
      return NULL;
      }
    xaccTransSetMemo (trans, tmp);
    free (tmp);
    
    /* action first introduced in version 3 of the file format */
    if (3 <= token) {
       tmp = readString( fd, token );
       if( tmp == NULL )
         {
         PERR ("Premature end of Transaction at action");
         xaccFreeTransaction(trans);
         return NULL;
         }
       xaccTransSetAction (trans, tmp);
      }
    
    /* category is now obsolete */
    err = read( fd, &(dummy_category), sizeof(int) );
    if( err != sizeof(int) )
      {
      PERR ("Premature end of Transaction at catagory");
      xaccFreeTransaction(trans);
      return NULL;
      }
    
    err = read( fd, &recn, sizeof(char) );
    if( err != sizeof(char) )
      {
      PERR ("Premature end of Transaction at reconciled");
      xaccFreeTransaction(trans);
      return NULL;
      }
    xaccTransSetReconcile (trans, recn);
    
    if( 1 >= token ) {
      /* Note: this is for version 0 of file format only.
       * What used to be reconciled, is now cleared... transactions
       * aren't reconciled until you get your bank statement, and
       * use the reconcile window to mark the transaction reconciled
       */
      if( YREC == trans->source_split.reconciled ) {
        xaccTransSetReconcile (trans, CREC);
      }
    }
  
    /* make sure the value of trans->reconciled is valid...
     * I have to do this mainly for if I change what NREC and
     * YREC are defined to be... this way it might loose all
     * the reconciled data, but at least the field is valid */
    if( (YREC != trans->source_split.reconciled) && 
        (FREC != trans->source_split.reconciled) &&
        (CREC != trans->source_split.reconciled) ) {
      xaccTransSetReconcile (trans, NREC);
    }
    
    /* Version 1 files stored the amount as an integer,
     * with the amount recorded as pennies.
     * Version 2 and above store the share amounts and 
     * prices as doubles. */
    if (1 == token) {
      int amount;
      err = read( fd, &amount, sizeof(int) );
      if( err != sizeof(int) )
        {
        PERR ("Premature end of Transaction at V1 amount");
        xaccFreeTransaction(trans);
        return NULL;
        }
      XACC_FLIP_INT (amount);
      num_shares = 0.01 * ((double) amount); /* file stores pennies */
      trans->source_split.damount = num_shares;
    } else {
      double damount;
  
      /* first, read number of shares ... */
      err = read( fd, &damount, sizeof(double) );
      if( err != sizeof(double) )
        {
        PERR ("Premature end of Transaction at amount");
        xaccFreeTransaction(trans);
        return NULL;
        }
      XACC_FLIP_DOUBLE (damount);
      num_shares  = damount;
      trans->source_split.damount = num_shares;
  
      /* ... next read the share price ... */
      err = read( fd, &damount, sizeof(double) );
      if( err != sizeof(double) )
        {
        PERR ("Premature end of Transaction at share_price");
        xaccFreeTransaction(trans);
        return NULL;
        }
      XACC_FLIP_DOUBLE (damount);
      share_price = damount;
      trans->source_split.share_price = share_price;
    }  
  
    INFO_2 ("readTransaction(): num_shares %f \n", num_shares);
  
    /* Read the account numbers for double-entry */
    /* These are first used in Version 2 of the file format */
    if (1 < token) {
      Account *peer_acc;
      /* first, read the credit account number */
      err = read( fd, &acc_id, sizeof(int) );
      if( err != sizeof(int) )
        {
        PERR ("Premature end of Transaction at credit");
        xaccFreeTransaction(trans);
        return NULL;
        }
      XACC_FLIP_INT (acc_id);
      INFO_2 ("readTransaction(): credit %d\n", acc_id);
      peer_acc = locateAccount (acc_id);
      trans -> source_split.acc = (struct _account *) peer_acc;
  
      /* insert the split part of the transaction into 
       * the credited account */
      if (peer_acc) xaccInsertSplit( peer_acc, &(trans->source_split) );
  
      /* next read the debit account number */
      err = read( fd, &acc_id, sizeof(int) );
      if( err != sizeof(int) )
        {
        PERR ("Premature end of Transaction at debit");
        xaccFreeTransaction(trans);
        return NULL;
        }
      XACC_FLIP_INT (acc_id);
      INFO_2 ("readTransaction(): debit %d\n", acc_id);
      peer_acc = locateAccount (acc_id);
      if (peer_acc) {
         Split *split;
         split = xaccMallocSplit ();
         xaccTransAppendSplit (trans, split);
         split -> acc = (struct _account *) peer_acc;
         xaccInsertSplit (peer_acc, split);

         /* duplicate many of the attributes in the credit split */
         split->damount = -num_shares;
         split->share_price = share_price;
         split->reconciled = trans->source_split.reconciled;
         free (split->memo);
         split->memo = strdup (trans->source_split.memo);
         free (split->action);
         split->action = strdup (trans->source_split.action);
      }
  
    } else {

      /* Version 1 files did not do double-entry */
      xaccInsertSplit( acc, &(trans->source_split) );
    }
  } else { /* else, read version-5 files */
     Split *split;

     /* first, read the credit split, and copy it in place */
     split = readSplit (fd, token);
     xaccSplitSetMemo ( &(trans->source_split), split->memo);
     xaccSplitSetAction ( &(trans->source_split), split->action);
     xaccSplitSetReconcile ( &(trans->source_split), split->reconciled);
     trans->source_split.damount = split->damount;
     trans->source_split.share_price = split->share_price;
     trans->source_split.acc = split->acc;
     trans->source_split.parent = trans;

     /* then wire it into place */
     xaccInsertSplit( ((Account *) (trans->source_split.acc)), &(trans->source_split) );
     
     /* free the thing that  the read returned */
     split->acc = NULL;
     split->parent = NULL;
     xaccFreeSplit (split);

    /* read number of splits */
    err = read( fd, &(numSplits), sizeof(int) );
    if( err != sizeof(int) )
      {
      PERR ("Premature end of Transaction at num-splits");
      xaccFreeTransaction(trans);
      return NULL;
      }
     XACC_FLIP_INT (numSplits);
    
     for (i=0; i<numSplits; i++) {
        split = readSplit (fd, token);
        split->parent = trans;
        xaccTransAppendSplit( trans, split);
        xaccInsertSplit( ((Account *) (split->acc)), split);
     }
  }
    
  return trans;
}

/********************************************************************\
 * readSplit                                                        * 
 *   reads in the data for a split from the datafile                *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the transaction structure                                * 
\********************************************************************/

static Split *
readSplit ( int fd, int token )
  {
  Account *peer_acc;
  Split *split;
  int err=0;
  int acc_id;
  char *tmp;
  char recn;
  double damount;

  /* create a split structure */
  split = xaccMallocSplit();
  
  ENTER ("readSplit");

  tmp = readString( fd, token );
  if( NULL == tmp )
    {
    PERR ("Premature end of Split at memo");
    xaccFreeSplit(split);
    return NULL;
    }
  xaccSplitSetMemo (split, tmp);
  free (tmp);
  
  tmp = readString( fd, token );
  if( tmp == NULL )
    {
    PERR ("Premature end of Split at action");
    xaccFreeSplit (split);
    return NULL;
    }
  xaccSplitSetAction (split, tmp);
  
  err = read( fd, &recn, sizeof(char) );
  if( err != sizeof(char) )
    {
    PERR ("Premature end of Split at reconciled");
    xaccFreeSplit (split);
    return NULL;
    }
  
  /* make sure the value of split->reconciled is valid...
   * Do this mainly in case we change what NREC and
   * YREC are defined to be... this way it might loose all
   * the reconciled data, but at least the field is valid */
  if( (YREC != recn) && 
      (FREC != recn) &&
      (CREC != recn) ) {
    recn = NREC;
  }
  xaccSplitSetReconcile (split, recn);

  /* first, read number of shares ... */
  err = read( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    {
    PERR ("Premature end of Split at amount");
    xaccFreeSplit (split);
    return NULL;
    }
  XACC_FLIP_DOUBLE (damount);
  split -> damount = damount;

  /* ... next read the share price ... */
  err = read( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    {
    PERR ("Premature end of Split at share_price");
    xaccFreeSplit (split);
    return NULL;
    }
  XACC_FLIP_DOUBLE (damount);
  split->share_price = damount;

  INFO_2 ("readSplit(): num_shares %f \n", damount);

  /* Read the account number */

  err = read( fd, &acc_id, sizeof(int) );
  if( err != sizeof(int) )
    {
    PERR ("Premature end of Split at account");
    xaccFreeSplit (split);
    return NULL;
    }
  XACC_FLIP_INT (acc_id);
  INFO_2 ("readSplit(): account id %d\n", acc_id);
  peer_acc = locateAccount (acc_id);
  split -> acc = (struct _account *) peer_acc;

  return split;
}

/********************************************************************\
 * readString                                                       * 
 *   reads in a string (char *) from the datafile                   *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the string                                               * 
\********************************************************************/
static char *
readString( int fd, int token )
  {
  int  err=0;
  int  size;
  char *str;
  
  err = read( fd, &size, sizeof(int) );
  if( err != sizeof(int) )
    return NULL;
  XACC_FLIP_INT (size);
  
  str = (char *) malloc (size);
  err = read( fd, str, size );
  if( err != size )
    {
    printf( "Error: readString: size = %d err = %d str = %s\n", size, err, str );
    free(str);
    return NULL;
    }
  
  return str;
  }

/********************************************************************\
 * readDate                                                         * 
 *   reads in a Date struct from the datafile                       *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the Date struct                                          * 
\********************************************************************/
static Date *
readDate( int fd, int token )
  {
  int  err=0;
  Date *date = (Date *)_malloc(sizeof(Date));
  
  err = read( fd, &(date->year), sizeof(int) );
  if( err != sizeof(int) )
    {
    _free(date);
    return NULL;
    }
  XACC_FLIP_INT (date->year);
  
  err = read( fd, &(date->month), sizeof(int) );
  if( err != sizeof(int) )
    {
    _free(date);
    return NULL;
    }
  XACC_FLIP_INT (date->month);
  
  err = read( fd, &(date->day), sizeof(int) );
  if( err != sizeof(int) )
    {
    _free(date);
    return NULL;
    }
  XACC_FLIP_INT (date->day);
  
  return date;
  }

/********************************************************************\
 ********************** SAVE DATA ***********************************
\********************************************************************/

static void
xaccResetWriteFlags (AccountGroup *grp) 
{
   int i, numAcc;
   if (!grp) return;

  /* Zero out the write flag on all of the 
   * transactions.  The write_flag is used to determine
   * if a given transaction has already been written 
   * out to the file.  This flag is necessary, since 
   * double-entry transactions appear in two accounts,
   * while they should be written only once to the file.
   * The write_flag is used ONLY by the routines in this
   * module.
   */
  numAcc = grp ->numAcc;
  for( i=0; i<numAcc; i++ ) {
    int n=0;
    Account *acc;
    Split *s;
    acc = getAccount (grp,i) ;

    /* recursively do sub-accounts */
    xaccResetWriteFlags (acc->children);
    
    /* zip over all accounts */
    s = acc->splits[0];
    n=0;
    while (s) {
      Transaction *trans;
      trans = s->parent;
      if (trans) trans -> write_flag = 0;
      n++;
      s = acc->splits[n];
    }
  }

}

/********************************************************************\
 * xaccWriteAccountGroup                                                    * 
 *   flattens the program data and saves it in a file               * 
 *                                                                  * 
 * Args:   datafile - the file to store the data in                 * 
 * Return: -1 on failure                                            * 
\********************************************************************/
int 
xaccWriteAccountGroup( char *datafile, AccountGroup *grp )
  {
  int err = 0;
  int token = VERSION;    /* The file format version */
  int fd;
  
  if (NULL == grp) return -1;

  /* first, zero out the write flag on all of the 
   * transactions.  The write_flag is used to determine
   * if a given transaction has already been written 
   * out to the file.  This flag is necessary, since 
   * double-entry transactions appear in two accounts,
   * while they should be written only once to the file.
   * The write_flag is used ONLY by the routines in this
   * module.
   */
  xaccResetWriteFlags (grp);

  /* now, open the file and start writing */
  fd = open( datafile, WFLAGS, PERMS );
  if( fd == -1 )
    {
    ERROR();
    return -1;
    }
  
  XACC_FLIP_INT (token);
  err = write( fd, &token, sizeof(int) );
  if( err != sizeof(int) )
    {
    ERROR();
    close(fd);
    return -1;
    }

  err = writeGroup (fd, grp);

  close(fd);
  return err;
  }

/********************************************************************\
 * writeGroup                                                * 
 *   writes out a group of accounts to a file                       * 
 *                                                                  * 
 * Args:   fd -- file descriptor                                    *
 *         grp -- account group                                     *
 *                                                                  *
 * Return: -1 on failure                                            * 
\********************************************************************/
static int 
writeGroup (int fd, AccountGroup *grp )
  {
  int i,numAcc;
  int err = 0;

  ENTER ("writeGroup");
  
  if (NULL == grp) return 0;

  numAcc = grp->numAcc;
  XACC_FLIP_INT (numAcc);
  err = write( fd, &numAcc, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  for( i=0; i<grp->numAcc; i++ )
    {
    err = writeAccount( fd, getAccount(grp,i) );
    if( -1 == err )
      return err;
    }
  
  return err;
  }

/********************************************************************\
 * writeAccount                                                     * 
 *   saves the data for an account to the datafile                  *
 *                                                                  * 
 * Args:   fd   - the filedescriptor of the data file               * 
 *         acc  - the account data to save                          * 
 * Return: -1 on failure                                            * 
\********************************************************************/
static int
writeAccount( int fd, Account *acc )
  {
  Transaction *trans;
  Split *s;
  int err=0;
  int i, numUnwrittenTrans, ntrans;
  int acc_id;
  int numChildren = 0;
  char acctype;
  
  INFO_2 ("writeAccount(): writing acct %s \n", acc->accountName);

  acc_id = acc->id;
  XACC_FLIP_INT (acc_id);
  err = write( fd, &acc_id, sizeof(int) );
  if( err != sizeof(int) )
    return -1;

  err = write( fd, &(acc->flags), sizeof(char) );
  if( err != sizeof(char) )
    return -1;
  
  acctype = (char) (acc->type);
  err = write( fd, &(acctype), sizeof(char) );
  if( err != sizeof(char) )
    return -1;
  
  err = writeString( fd, acc->accountName );
  if( -1 == err )
    return err;
  
  err = writeString( fd, acc->description );
  if( -1 == err )
    return err;
  
  err = writeString( fd, acc->notes );
  if( -1 == err )
    return err;
  
  /* figure out numTrans -- it will be less than the total
   * number of transactions in this account, because some 
   * of the double entry transactions will already have been 
   * written.  write_flag values are:
   * 0 == uncounted, unwritten
   * 1 == counted, unwritten
   * 2 == written
   */
  numUnwrittenTrans = 0;
  i=0;
  s = acc->splits[i];
  while (s) {
    trans = s->parent;
    if (0 == trans->write_flag) {
       numUnwrittenTrans ++;
       trans->write_flag = 1;
    }
    i++;
    s = acc->splits[i];
  }
  
  ntrans = numUnwrittenTrans;
  XACC_FLIP_INT (ntrans);
  err = write( fd, &ntrans, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  INFO_2 ("writeAccount(): will write %d trans\n", numUnwrittenTrans);
  i=0;
  s = acc->splits[i];
  while (s) {
    trans = s->parent;
    if (1 == trans->write_flag) {
       err = writeTransaction( fd, trans );
       if (-1 == err) return err;
    }
    i++;
    s = acc->splits[i];
  }

  if (acc->children) {
    numChildren = 1;
  } else {
    numChildren = 0;
  }

  XACC_FLIP_INT (numChildren);
  err = write( fd, &numChildren, sizeof(int) );
  if( err != sizeof(int) ) return -1;

  if (acc->children) {
    err = writeGroup (fd, acc->children);
  }
  
  return err;
}

/********************************************************************\
 * writeTransaction                                                 * 
 *   saves the data for a transaction to the datafile               *
 *                                                                  * 
 * Args:   fd       - the filedescriptor of the data file           * 
 *         trans    - the transaction data to save                  * 
 * Return: -1 on failure                                            * 
\********************************************************************/
static int
writeTransaction( int fd, Transaction *trans )
  {
  Split *s;
  int err=0;
  int i=0;

  ENTER ("writeTransaction");
  /* If we've already written this transaction, don't write 
   * it again.  That is, prevent double-entry transactions 
   * from being written twice 
   */
  if (2 == trans->write_flag) return 4;
  trans->write_flag = 2;
  
  err = writeString( fd, trans->num );
  if( -1 == err ) return err;
  
  err = writeDate( fd, &(trans->date) );
  if( -1 == err ) return err;
  
  err = writeString( fd, trans->description );
  if( -1 == err ) return err;
  
  err = writeSplit( fd, &(trans->source_split) );
  if( -1 == err ) return err;
  
  /* count the number of splits */
  i = 0;
  s = trans->dest_splits[i];
  while (s) {
    i++;
    s = trans->dest_splits[i];
  }
  XACC_FLIP_INT (i);
  err = write( fd, &i, sizeof(int) );
  if( err != sizeof(int) ) return -1;
  
  /* now write the splits */
  i = 0;
  s = trans->dest_splits[i];
  while (s) {
    err = writeSplit (fd, s);
    if( -1 == err ) return err;
    i++;
    s = trans->dest_splits[i];
  }
  
  return err;
  }

/********************************************************************\
 * writeSplit                                                       * 
 *   saves the data for a split to the datafile                     *
 *                                                                  * 
 * Args:   fd       - the filedescriptor of the data file           * 
 *         split    - the split data to save                        * 
 * Return: -1 on failure                                            * 
\********************************************************************/
static int
writeSplit ( int fd, Split *split )
  {
  int err=0;
  int tmp, acc_id;
  double damount;
  Account *xfer_acc;

  ENTER ("writeSplit");
  
  err = writeString( fd, split->memo );
  if( -1 == err )
    return err;
  
  err = writeString( fd, split->action );
  if( -1 == err )
    return err;
  
  err = write( fd, &(split->reconciled), sizeof(char) );
  if( err != sizeof(char) )
    return -1;
  
  damount = split->damount;
  INFO_2 ("writeSplit: amount=%f \n", damount);
  XACC_FLIP_DOUBLE (damount);
  err = write( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    return -1;

  damount = split->share_price;  
  XACC_FLIP_DOUBLE (damount);
  err = write( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    return -1;

  /* write the credited/debted account */
  xfer_acc = (Account *) (split->acc);
  acc_id = -1;
  if (xfer_acc) acc_id = xfer_acc -> id;
  INFO_2 ("writeSplit: credit %d \n", acc_id);
  XACC_FLIP_INT (acc_id);
  err = write( fd, &acc_id, sizeof(int) );
  if( err != sizeof(int) )
    return -1;

  return err;
  }

/********************************************************************\
 * writeString                                                      * 
 *   saves a string to the datafile                                 *
 *                                                                  * 
 * Args:   fd       - the filedescriptor of the data file           * 
 *         str      - the String to save                            * 
 * Return: -1 on failure                                            * 
\********************************************************************/
static int
writeString( int fd, char *str )
  {
  int err=0;
  int size;
  int tmp;

  if (NULL == str) str = "";   /* protect against null arguments */
  
  for( size=0; str[size] != '\0'; size++ )
    {}
  size++;                /* we want to make sure we include the '\0'! 
			  * Otherwise, bad things happen */
  
  tmp = size;
  XACC_FLIP_INT (tmp);
  err = write( fd, &tmp, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  err = write( fd, str, size );
  if( err != size )
    return -1;
  
  return err;
  }

/********************************************************************\
 * writeDate                                                        * 
 *   saves a Date to the datafile                                   *
 *                                                                  * 
 * Args:   fd       - the filedescriptor of the data file           * 
 *         date     - the Date to save                              * 
 * Return: -1 on failure                                            * 
\********************************************************************/
static int
writeDate( int fd, Date *date )
  {
  int err=0;
  int tmp;
  
  tmp = date->year;
  XACC_FLIP_INT (tmp);
  err = write( fd, &tmp, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  tmp = date->month;
  XACC_FLIP_INT (tmp);
  err = write( fd, &tmp, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  tmp = date->day;
  XACC_FLIP_INT (tmp);
  err = write( fd, &tmp, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  return err;
  }

/*********************** END OF FILE *********************************/
