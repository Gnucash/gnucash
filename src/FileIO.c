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
 * Version 2 of the file format supports reading and writing of     *
 * double-entry transactions.                                       *
 *                                                                  *
 *                                                                  *
 * the format of the data in the file:                              *
 *   file        ::== token numAccounts (Account)^numAccounts       *
 *   Account     ::== num flags type accountName description notes  * 
 *                    numTran (Transaction)^numTrans                * 
 *   Transaction ::== num date description memo catagory reconciled *
 *                     amount share_price                           *
 *                     credit_account debit_account                 * 
 *   token       ::== int  [the version of file format == VERSION]  * 
 *   numTrans    ::== int                                           * 
 *   numAccounts ::== int                                           * 
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

#include <Xm/Xm.h>
#include <fcntl.h>
#include "main.h"
#include "util.h"
#include "Account.h"
#include "Data.h"

#define PERMS   0666
#define WFLAGS  (O_WRONLY | O_CREAT | O_TRUNC)
#define RFLAGS  O_RDONLY
#define VERSION 2

/** GLOBALS *********************************************************/
extern Widget toplevel;

/** PROTOTYPES ******************************************************/
int          readAccount( int fd, Account *, int token );
Transaction *readTransaction( int fd, Account *, int token );
char        *readString( int fd, int token );
Date        *readDate( int fd, int token );

int writeAccount( int fd, Account *account );
int writeTransaction( int fd, Account *, Transaction *trans );
int writeString( int fd, char *str );
int writeDate( int fd, Date *date );

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
  
#define XACC_FLIP_ENDIAN
#ifdef XACC_FLIP_ENDIAN
  #define XACC_FLIP_INT(x) { (x) = xaccFlipInt (x); }
  #define XACC_FLIP_SHORT(x) { (x) = xaccFlipShort (x); }
#else
  #define XACC_FLIP_INT(x) 
  #define XACC_FLIP_SHORT(x) 
#endif /* XACC_FLIP_ENDIAN */

  
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
Data *
readData( char *datafile )
  {
  int  fd;
  int  numAcc;
  int  err=0;
  int  token=0;
  int  i;
  Data *data = mallocData();
  
  fd = open( datafile, RFLAGS, 0 );
  if( fd == -1 )
    {
    ERROR();
    freeData(data);
    return NULL;
    }
  
  /* Read in the file format token */
  err = read( fd, &token, sizeof(int) );
  if( err == -1 )
    {
    ERROR();
    close(fd);
    freeData(data);
    return NULL;
    }
  XACC_FLIP_INT (token);
  
  /* If this is an old file, ask the user if the file
   * should be updated */
  if( token < VERSION )
    {
    char msg[BUFSIZE];
    sprintf( (char *)&msg, FILE_TOO_OLD_MSG );
    if( !verifyBox( toplevel, msg ) )
      return NULL;
    }
  
  /* If this is a newer file than we know how to deal
   * with, warn the user */
  if( token > VERSION )
    {
    char msg[BUFSIZE];
    sprintf( (char *)&msg, FILE_TOO_NEW_MSG );
    if( !verifyBox( toplevel, msg ) )
      return NULL;
    }
  
  /* read numAccs */
  err = read( fd, &numAcc, sizeof(int) );
  if( err == -1 )
    {
    close(fd);
    freeData(data);
    return NULL;
    }
  XACC_FLIP_INT (numAcc);
  
  /* malloc the accounts, in preparation for reading.
   * Mmalloc all of them; they will be needed for up front
   * for inserting the double-entry transactions */
  for( i=0; i<numAcc; i++ )
    {
    Account *acc   = mallocAccount();
    insertAccount( data, acc );
    }

  /* read in the accounts */
  for( i=0; i<numAcc; i++ )
    {
    Account *acc   = getAccount (data, i);
    err = readAccount( fd, acc, token );
    if( -1 == err )
      {
      close(fd);
      printf(" numAcc = %d, i = %d\n",numAcc,i);
      return data;
      }
    }
  
  close(fd);
  return data;
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
int
readAccount( int fd, Account *acc, int token )
  {
  int err=0;
  int i;
  int numTrans, nacc;

  ENTER ("readAccount");
  
  /* version 1 does not store the account number */
  if (1 < token) {
    err = read( fd, &nacc, sizeof(int) );
    if( err != sizeof(int) )
      {
      freeAccount(acc);
      return -1;
      }
    XACC_FLIP_INT (nacc);

    /* normalize the account numbers -- positive-definite.
     * That is, the unique id must never decrease,
     * nor must it overalp any existing account id */
    acc->id = nacc;
    if (next_free_unique_account_id <= nacc) {
      next_free_unique_account_id = nacc+1;
    }
  }
  
  err = read( fd, &(acc->flags), sizeof(char) );
  if( err != sizeof(char) )
    {
    freeAccount(acc);
    return -1;
    }
  
  err = read( fd, &(acc->type), sizeof(char) );
  if( err != sizeof(char) )
    {
    freeAccount(acc);
    return -1;
    }
  
  acc->accountName = readString( fd, token );
  if( acc->accountName == NULL )
    {
    freeAccount(acc);
    return -1;
    }
  
  acc->description = readString( fd, token );
  if( acc->description == NULL )
    {
    freeAccount(acc);
    return -1;
    }
  
  acc->notes = readString( fd, token );
  if( acc->notes == NULL )
    {
    freeAccount(acc);
    return -1;
    }
  
  err = read( fd, &numTrans, sizeof(int) );
  if( err != sizeof(int) )
    {
    freeAccount(acc);
    return -1;
    }
  XACC_FLIP_INT (numTrans);
  
  /* read the transactions */
  for( i=0; i<numTrans; i++ )
    {
    Transaction *trans;
    trans = readTransaction( fd, acc, token );
    if( trans == NULL )
      {
      printf("Error: readAccount: Premature termination: \n");
      printf (" numTrans = %d i = %d\n",numTrans,i);
      return 0;
      }
    }
  
  return 0;
  }

/********************************************************************\
 * readTransaction                                                  * 
 *   reads in the data for a transaction from the datafile          *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the transaction structure                                * 
\********************************************************************/
Transaction *
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
    DEBUG ("Error: Premature end of Transaction at num");
    _free(trans);
    return NULL;
    }
  
  date = readDate( fd, token );
  if( date == NULL )
    {
    DEBUG ("Error: Premature end of Transaction at date");
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  trans->date = *date;
  _free(date);
  
  trans->description = readString( fd, token );
  if( trans->description == NULL )
    {
    DEBUG ("Error: Premature end of Transaction at description");
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  
  trans->memo = readString( fd, token );
  if( trans->memo == NULL )
    {
    DEBUG ("Error: Premature end of Transaction at memo");
    XtFree(trans->description);
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  
  err = read( fd, &(trans->catagory), sizeof(int) );
  if( err != sizeof(int) )
    {
    DEBUG ("Error: Premature end of Transaction at category");
    XtFree(trans->memo);
    XtFree(trans->description);
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  XACC_FLIP_INT (trans->catagory);
  
  err = read( fd, &(trans->reconciled), sizeof(char) );
  if( err != sizeof(char) )
    {
    DEBUG ("Error: Premature end of Transaction at reconciled");
    XtFree(trans->memo);
    XtFree(trans->description);
    XtFree(trans->num);
    _free(trans);
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
  
  if (1 == token) {
    int amount;
    /* version 1 files stored the amount as an integer,
     * with the amount recorded as pennies */
    err = read( fd, &amount, sizeof(int) );
    if( err != sizeof(int) )
      {
      DEBUG ("Error: Premature end of Transaction at V1 amount");
      XtFree(trans->memo);
      XtFree(trans->description);
      XtFree(trans->num);
      _free(trans);
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
      DEBUG ("Error: Premature end of Transaction at amount");
      XtFree(trans->memo);
      XtFree(trans->description);
      XtFree(trans->num);
      _free(trans);
      return NULL;
      }
    trans->damount = damount;

    /* ... next read the share price ... */
    err = read( fd, &damount, sizeof(double) );
    if( err != sizeof(double) )
      {
      DEBUG ("Error: Premature end of Transaction at share_price");
      XtFree(trans->memo);
      XtFree(trans->description);
      XtFree(trans->num);
      _free(trans);
      return NULL;
      }
    trans->share_price = damount;
  }  
  DEBUGCMD(printf ("Info: readTransaction(): amount %f \n", trans->damount));

  /* read the account numbers for double-entry */
  /* these are first used in version 2 of the file format */
  if (1 < token) {
    Account *peer_acc;
    /* first, read the credit account number */
    err = read( fd, &acc_id, sizeof(int) );
    if( err != sizeof(int) )
      {
      DEBUG ("Error: Premature end of Transaction at credit");
      XtFree(trans->memo);
      XtFree(trans->description);
      XtFree(trans->num);
      return NULL;
      }
    XACC_FLIP_INT (acc_id);
    DEBUGCMD (printf ("Info: readTransaction(): credit %d\n", acc_id));
    peer_acc = xaccGetPeerAccountFromID (acc, acc_id);
    trans -> credit = (struct _account *) peer_acc;

    /* insert the transaction into both the debit and 
     * the credit accounts; first the credit ... */
    if (peer_acc) insertTransaction( peer_acc, trans );

    /* next read the debit account number */
    err = read( fd, &acc_id, sizeof(int) );
    if( err != sizeof(int) )
      {
      DEBUG ("Error: Premature end of Transaction at debit");
      XtFree(trans->memo);
      XtFree(trans->description);
      XtFree(trans->num);
      return NULL;
      }
    XACC_FLIP_INT (acc_id);
    DEBUGCMD (printf ("Info: readTransaction(): debit %d\n", acc_id));
    peer_acc = xaccGetPeerAccountFromID (acc, acc_id);
    trans -> debit = (struct _account *) peer_acc;

    /* insert the transaction into both the debit and 
     * the credit accounts; next, the debit ... */
    if (peer_acc) insertTransaction( peer_acc, trans );
  } else {

    /* version 1 files did not do double-entry */
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
char *
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
Date *
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

/********************************************************************\
 * writeData                                                        * 
 *   flattens the program data and saves it in a file               * 
 *                                                                  * 
 * Args:   datafile - the file to store the data in                 * 
 * Return: -1 on failure                                            * 
\********************************************************************/
int 
writeData( char *datafile, Data *data )
  {
  int i,numAcc;
  int err = 0;
  int token = VERSION;    /* The file format version */
  int fd;
  
  if (NULL == data) return -1;

  /* first, zero out the write flag on all of the 
   * transactions */
  numAcc = data ->numAcc;
  for( i=0; i<numAcc; i++ ) {
    int n=0;
    Account *acc;
    Transaction * trans;
    acc = getAccount (data,i) ;
    trans = getTransaction (acc, n); 
    n++;
    while (trans) {
      trans->write_flag = 0;
      trans = getTransaction (acc, n); 
      n++;
    }
  }

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

  numAcc = data->numAcc;
  XACC_FLIP_INT (numAcc);
  err = write( fd, &numAcc, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  for( i=0; i<data->numAcc; i++ )
    {
    err = writeAccount( fd, getAccount(data,i) );
    if( err == -1 )
      return err;
    }
  
  close(fd);
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
int
writeAccount( int fd, Account *acc )
  {
  Transaction *trans;
  int err=0;
  int i,numTrans, ntrans;
  int acc_id;
  
  acc_id = acc->id;
  DEBUGCMD (printf ("Info: writeAccount: writing acct id %d %x \n", acc_id, acc));
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
  numTrans = 0;
  i = 0;
  trans = getTransaction (acc, i);
  while (trans) {
    i++;
    if (0 == trans->write_flag) numTrans ++;
    trans = getTransaction (acc, i);
  }

  ntrans = numTrans;
  XACC_FLIP_INT (ntrans);
  err = write( fd, &ntrans, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  for( i=0; i<numTrans; i++ ) {
    err = writeTransaction( fd, acc, getTransaction(acc,i) );
    if( err == -1 ) return err;
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
int
writeTransaction( int fd, Account * acc, Transaction *trans )
  {
  int err=0;
  int tmp, acc_id;
  double damount;
  Account *xfer_acc;

  ENTER ("writeTransaction");
  /* if we've already written this transaction, don't write it again */
  /* that is, prevent double-entry transactions from being written twice */
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
  
  tmp = trans->catagory;
  XACC_FLIP_INT (tmp);
  err = write( fd, &tmp, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  err = write( fd, &(trans->reconciled), sizeof(char) );
  if( err != sizeof(char) )
    return -1;
  
  damount = trans->damount;
  DEBUGCMD (printf ("Info: writeTransaction: amount=%f \n", damount));
  err = write( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    return -1;

  damount = trans->share_price;  
  err = write( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    return -1;

  /* write the double-entry values */
  xfer_acc = (Account *) (trans->credit);
  acc_id = -1;
  if (xfer_acc) acc_id = xfer_acc -> id;
  DEBUGCMD (printf ("Info: writeTransaction: credit %d \n", acc_id));
  XACC_FLIP_INT (acc_id);
  err = write( fd, &acc_id, sizeof(int) );
  if( err != sizeof(int) )
    return -1;

  xfer_acc = (Account *) (trans->debit);
  acc_id = -1;
  if (xfer_acc) acc_id = xfer_acc -> id;
  DEBUGCMD (printf ("Info: writeTransaction: debit %d \n", acc_id));
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
int
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
int
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
