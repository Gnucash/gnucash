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
 *                                                                  *
 * the format of the data in the file:                              *
 *   file        ::== token Group                                   *
 *   Group       ::== numAccounts (Account)^numAccounts             *
 *   Account     ::== accID flags type accountName description      * 
 *                    notes numTran (Transaction)^numTrans          * 
 *                    numGroups (Group)^numGroups                   *
 *   Transaction ::== num date description memo catagory reconciled *
 *                     amount share_price                           *
 *                     credit_account debit_account                 * 
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
 *   catagory    ::== int                                           * 
 *   reconciled  ::== char                                          * 
 *   amount      ::== double                                        * 
 *   share_price ::== double                                        * 
 *   credit_account ::= int                                         *
 *   debit_account  ::= int                                         *
 *   String      ::== size (char)^size                              * 
 *   size        ::== int                                           * 
 *   Date        ::== year month day                                * 
 *   month       ::== int                                           * 
 *   day         ::== int                                           * 
 *   year        ::== int                                           * 
\********************************************************************/

#include <fcntl.h>
#include <unistd.h>
#include <Xm/Xm.h>

#include "config.h"

#include "Account.h"
#include "Data.h"
#include "main.h"
#include "util.h"

#define PERMS   0666
#define WFLAGS  (O_WRONLY | O_CREAT | O_TRUNC)
#define RFLAGS  O_RDONLY
#define VERSION 4

/** GLOBALS *********************************************************/
extern Widget toplevel;

static AccountGroup *holder;     /* temporary holder for
                                  *  unclassified accounts */
static AccountGroup *maingrp;    /* temporary holder for file
                                  * being read */

/** PROTOTYPES ******************************************************/
static Account     *locateAccount (int acc_id); 
static Account     *springAccount (int acc_id); 

static AccountGroup *readGroup( int fd, Account *, int token );
static Account      *readAccount( int fd, AccountGroup *, int token );
static Transaction  *readTransaction( int fd, Account *, int token );
static char         *readString( int fd, int token );
static Date         *readDate( int fd, int token );

static int writeGroup( int fd, AccountGroup *grp );
static int writeAccount( int fd, Account *account );
static int writeTransaction( int fd, Transaction *trans );
static int writeString( int fd, char *str );
static int writeDate( int fd, Date *date );

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
 * readData                                                         * 
 *   reads in the data from file datafile                           *
 *                                                                  * 
 * Args:   datafile - the file to load the data from                * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/
AccountGroup *
readData( char *datafile )
  {
  int  fd;
  int  err=0;
  int  token=0;
  int  num_unclaimed;
  AccountGroup *grp = 0x0;

  maingrp = 0x0;
  
  fd = open( datafile, RFLAGS, 0 );
  if( fd == -1 )
    {
    ERROR();
    return NULL;
    }
  
  /* Read in the file format token */
  err = read( fd, &token, sizeof(int) );
  if( err == -1 )
    {
    ERROR();
    close(fd);
    return NULL;
    }
  XACC_FLIP_INT (token);
  
  /* If this is an old file, ask the user if the file
   * should be updated */
  if( VERSION > token ) {
    if( !verifyBox( toplevel, FILE_TOO_OLD_MSG ) ) {
      close(fd);
      return NULL;
    }
  }
  
  /* If this is a newer file than we know how to deal
   * with, warn the user */
  if( VERSION < token ) {
    if( !verifyBox( toplevel, FILE_TOO_NEW_MSG ) ) {
      close(fd);
      return NULL;
    }
  }
  
  holder = mallocAccountGroup();
  grp = readGroup (fd, NULL, token);

  /* the number of unclaimed accounts should be zero if the 
   * read succeeded.  But just in case of a very unlikely 
   * error, try to continue anyway. */
  num_unclaimed = xaccGetNumAccounts (holder);
  if (num_unclaimed) {
    if ( !verifyBox( toplevel, FILE_BAD_READ_MSG ) ) {
       freeAccountGroup (holder);
       freeAccountGroup (grp);
       grp = NULL;
    } else {
       /* create a lost account, put the missing accounts there */
       Account *acc = mallocAccount();
       acc -> accountName = XtNewString ("Lost Accounts");
       acc -> children = (struct _account_group *) holder;
       insertAccount (grp, acc);
    }
  } else {
    freeAccountGroup (holder);
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
  AccountGroup *grp = mallocAccountGroup();
  
  ENTER ("readGroup");

  if (NULL == aparent) {
    maingrp = grp;
  }

  /* read numAccs */
  err = read( fd, &numAcc, sizeof(int) );
  if( err == -1 )
    {
    freeAccountGroup (grp);
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

  ENTER ("readAccount");
  
  /* version 1 does not store the account number */
  if (1 < token) {
    err = read( fd, &accID, sizeof(int) );
    if( err != sizeof(int) ) { return NULL; }
    XACC_FLIP_INT (accID);
    acc = locateAccount (accID);
  } else {
    acc = mallocAccount();
    insertAccount (holder, acc);
  }
  
  err = read( fd, &(acc->flags), sizeof(char) );
  if( err != sizeof(char) ) { return NULL; }
  
  err = read( fd, &(acc->type), sizeof(char) );
  if( err != sizeof(char) ) { return NULL; }
  
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
   acc = mallocAccount ();
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
  Date *date;
  Transaction *trans = mallocTransaction();
  
  ENTER ("readTransaction");

  trans->num = readString( fd, token );
  if( trans->num == NULL )
    {
    PERR ("Premature end of Transaction at num");
    freeTransaction(trans);
    return NULL;
    }
  
  date = readDate( fd, token );
  if( date == NULL )
    {
    PERR ("Premature end of Transaction at date");
    freeTransaction(trans);
    return NULL;
    }
  trans->date = *date;
  _free(date);
  
  trans->description = readString( fd, token );
  if( trans->description == NULL )
    {
    PERR ("Premature end of Transaction at description");
    freeTransaction(trans);
    return NULL;
    }
  
  trans->memo = readString( fd, token );
  if( trans->memo == NULL )
    {
    PERR ("Premature end of Transaction at memo");
    freeTransaction(trans);
    return NULL;
    }
  
  /* action first introduced in version 3 of the file format */
  if (3 <= token) {
     trans->action = readString( fd, token );
     if( trans->action == NULL )
       {
       PERR ("Premature end of Transaction at memo");
       freeTransaction(trans);
       return NULL;
       }
    }
  
  err = read( fd, &(trans->catagory), sizeof(int) );
  if( err != sizeof(int) )
    {
    PERR ("Premature end of Transaction at catagory");
    freeTransaction(trans);
    return NULL;
    }
  XACC_FLIP_INT (trans->catagory);
  
  err = read( fd, &(trans->reconciled), sizeof(char) );
  if( err != sizeof(char) )
    {
    PERR ("Premature end of Transaction at reconciled");
    freeTransaction(trans);
    return NULL;
    }
  
  /* What used to be reconciled, is now cleared... transactions
   * aren't reconciled until you get your bank statement, and
   * use the reconcile window to mark the transaction reconciled */
  /* hack alert -- ?????????? I don't get it ???????????? */
  if( 0 <= token )
    if( trans->reconciled == YREC )
      trans->reconciled = CREC;
  
  /* make sure the value of trans->reconciled is valid...
   * I have to do this mainly for if I change what NREC and
   * YREC are defined to be... this way it might loose all
   * the reconciled data, but at least the field is valid */
  if( (trans->reconciled != YREC) && (trans->reconciled != CREC) )
    trans->reconciled = NREC;
  
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
      freeTransaction(trans);
      return NULL;
      }
    XACC_FLIP_INT (amount);
    trans->damount = 0.01 * ((double) amount); /* file stores pennies */
  } else {
    double damount;

    /* first, read number of shares ... */
    err = read( fd, &damount, sizeof(double) );
    if( err != sizeof(double) )
      {
      PERR ("Premature end of Transaction at amount");
      freeTransaction(trans);
      return NULL;
      }
    XACC_FLIP_DOUBLE (damount);
    trans->damount = damount;

    /* ... next read the share price ... */
    err = read( fd, &damount, sizeof(double) );
    if( err != sizeof(double) )
      {
      PERR ("Premature end of Transaction at share_price");
      freeTransaction(trans);
      return NULL;
      }
    XACC_FLIP_DOUBLE (damount);
    trans->share_price = damount;
  }  
  INFO_2 ("readTransaction(): amount %f \n", trans->damount);

  /* Read the account numbers for double-entry */
  /* These are first used in Version 2 of the file format */
  if (1 < token) {
    Account *peer_acc;
    /* first, read the credit account number */
    err = read( fd, &acc_id, sizeof(int) );
    if( err != sizeof(int) )
      {
      PERR ("Premature end of Transaction at credit");
      freeTransaction(trans);
      return NULL;
      }
    XACC_FLIP_INT (acc_id);
    INFO_2 ("readTransaction(): credit %d\n", acc_id);
    peer_acc = locateAccount (acc_id);
    trans -> credit = (struct _account *) peer_acc;

    /* insert the transaction into both the debit and 
     * the credit accounts; first the credit ... */
    if (peer_acc) insertTransaction( peer_acc, trans );

    /* next read the debit account number */
    err = read( fd, &acc_id, sizeof(int) );
    if( err != sizeof(int) )
      {
      PERR ("Premature end of Transaction at debit");
      freeTransaction(trans);
      return NULL;
      }
    XACC_FLIP_INT (acc_id);
    INFO_2 ("readTransaction(): debit %d\n", acc_id);
    peer_acc = locateAccount (acc_id);
    trans -> debit = (struct _account *) peer_acc;

    /* insert the transaction into both the debit and 
     * the credit accounts; next, the debit ... */
    if (peer_acc) insertTransaction( peer_acc, trans );
  } else {

    /* Version 1 files did not do double-entry */
    insertTransaction( acc, trans );
  }
  
  return trans;
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
  
  str = (char *)XtMalloc(size);
  err = read( fd, str, size );
  if( err != size )
    {
    printf( "Error: readString: size = %d err = %d str = %s\n", size, err, str );
    _free(str);
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
    Transaction * trans;
    acc = getAccount (grp,i) ;

    /* recursively do sub-accounts */
    xaccResetWriteFlags (acc->children);
    
    /* zip over all accounts */
    trans = getTransaction (acc, 0); 
    n++;
    while (trans) {
      trans->write_flag = 0;
      trans = getTransaction (acc, n); 
      n++;
    }
  }

}

/********************************************************************\
 * writeData                                                        * 
 *   flattens the program data and saves it in a file               * 
 *                                                                  * 
 * Args:   datafile - the file to store the data in                 * 
 * Return: -1 on failure                                            * 
\********************************************************************/
int 
writeData( char *datafile, AccountGroup *grp )
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
    if( err == -1 )
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
  int err=0;
  int i, numUnwrittenTrans, ntrans;
  int acc_id;
  int numChildren = 0;
  
  INFO_2 ("writeAccount(): writing acct %s \n", acc->accountName);

  acc_id = acc->id;
  XACC_FLIP_INT (acc_id);
  err = write( fd, &acc_id, sizeof(int) );
  if( err != sizeof(int) )
    return -1;

  err = write( fd, &(acc->flags), sizeof(char) );
  if( err != sizeof(char) )
    return -1;
  
  err = write( fd, &(acc->type), sizeof(char) );
  if( err != sizeof(char) )
    return -1;
  
  err = writeString( fd, acc->accountName );
  if( err == -1 )
    return err;
  
  err = writeString( fd, acc->description );
  if( err == -1 )
    return err;
  
  err = writeString( fd, acc->notes );
  if( err == -1 )
    return err;
  
  /* figure out numTrans -- it will be less than the total
   * number of transactions in this account, because some 
   * of the double entry transactions will already have been 
   * written. */
  numUnwrittenTrans = 0;
  for( i=0; i<acc->numTrans; i++ ) {
    trans = getTransaction(acc,i);
    if (0 == trans->write_flag) numUnwrittenTrans ++;
  }

  ntrans = numUnwrittenTrans;
  XACC_FLIP_INT (ntrans);
  err = write( fd, &ntrans, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  INFO_2 ("writeAccount(): will write %d trans\n", numUnwrittenTrans);
  for( i=0; i<acc->numTrans; i++ ) {
    trans = getTransaction(acc,i);
    if (0 == trans->write_flag) {
       err = writeTransaction( fd, trans );
    }
    if( err == -1 ) return err;
  }

  if (acc->children) {
    numChildren = 1;
  } else {
    numChildren = 0;
  }

  XACC_FLIP_INT (numChildren);
  err = write( fd, &numChildren, sizeof(int) );
  if( err != sizeof(int) )
    return -1;

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
 *         acc      - the account that the trans came from          * 
 *         trans    - the transaction data to save                  * 
 * Return: -1 on failure                                            * 
\********************************************************************/
static int
writeTransaction( int fd, Transaction *trans )
  {
  int err=0;
  int tmp, acc_id;
  double damount;
  Account *xfer_acc;

  ENTER ("writeTransaction");
  /* If we've already written this transaction, don't write 
   * it again.  That is, prevent double-entry transactions 
   * from being written twice 
   */
  if (trans->write_flag) return 4;
  trans->write_flag = 1;
  
  err = writeString( fd, trans->num );
  if( err == -1 )
    return err;
  
  err = writeDate( fd, &(trans->date) );
  if( err == -1 )
    return err;
  
  err = writeString( fd, trans->description );
  if( err == -1 )
    return err;
  
  err = writeString( fd, trans->memo );
  if( err == -1 )
    return err;
  
  err = writeString( fd, trans->action );
  if( err == -1 )
    return err;
  
  tmp = trans->catagory;
  XACC_FLIP_INT (tmp);
  err = write( fd, &tmp, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  err = write( fd, &(trans->reconciled), sizeof(char) );
  if( err != sizeof(char) )
    return -1;
  
  damount = trans->damount;
  INFO_2 ("writeTransaction: amount=%f \n", damount);
  XACC_FLIP_DOUBLE (damount);
  err = write( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    return -1;

  damount = trans->share_price;  
  XACC_FLIP_DOUBLE (damount);
  err = write( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    return -1;

  /* write the double-entry values */
  xfer_acc = (Account *) (trans->credit);
  acc_id = -1;
  if (xfer_acc) acc_id = xfer_acc -> id;
  INFO_2 ("writeTransaction: credit %d \n", acc_id);
  XACC_FLIP_INT (acc_id);
  err = write( fd, &acc_id, sizeof(int) );
  if( err != sizeof(int) )
    return -1;

  xfer_acc = (Account *) (trans->debit);
  acc_id = -1;
  if (xfer_acc) acc_id = xfer_acc -> id;
  INFO_2 (" writeTransaction: debit %d \n", acc_id);
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
