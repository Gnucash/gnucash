/********************************************************************\
 * FileIO.c -- read from and writing to a datafile for xacc         *
 *             (X-Accountant)                                       *
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
 *                                                                  *
 *                                                                  *
 * NOTE: the readxxxx/writexxxx functions changed the current       *
 *       position in the file, and so the order which these         *
 *       functions are called in important                          *
 *                                                                  *
 * the format of the data in the file:                              *
 *   file        ::== token numAccounts (Account)^numAccounts       *
 *   Account     ::== flags type accountName description notes      * 
 *                    numTran (Transaction)^numTrans                * 
 *   Transaction ::== num date description memo catagory reconciled *
 *                     amount                                       * 
 *   token       ::== int  [the version of file format == VERSION]  * 
 *   numTrans    ::== int                                           * 
 *   numAccounts ::== int                                           * 
 *   flags       ::== char                                          * 
 *   type        ::== char                                          * 
 *   accountName ::== String                                        *  
 *   description ::== String                                        *  
 *   notes       ::== String                                        *  
 *   num         ::== String                                        * 
 *   date        ::== Date                                          * 
 *   description ::== String                                        * 
 *   memo        ::== String                                        * 
 *   catagory    ::== int                                           * 
 *   reconciled  ::== char                                          * 
 *   amount      ::== int                                           * 
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
#define VERSION 1

/** GLOBALS *********************************************************/
extern Widget toplevel;

/** PROTOTYPES ******************************************************/
Account     *readAccount( int fd, int token );
Transaction *readTransaction( int fd, int token );
char        *readString( int fd, int token );
Date        *readDate( int fd, int token );

int writeAccount( int fd, Account *account );
int writeTransaction( int fd, Transaction *trans );
int writeString( int fd, char *str );
int writeDate( int fd, Date *date );

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
  
  /* read in the accounts */
  for( i=0; i<numAcc; i++ )
    {
    Account *acc;
    acc = readAccount( fd, token );
    if( acc == NULL )
      {
      close(fd);
      printf(" numAcc = %d\n i = %d\n",numAcc,i);
      return data;
      }
    else
      insertAccount( data, acc );
    }
  
  close(fd);
  return data;
  }

/********************************************************************\
 * readAccount                                                      * 
 *   reads in the data for an account from the datafile             *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the account structure                                    * 
\********************************************************************/
Account *
readAccount( int fd, int token )
  {
  int err=0;
  int i;
  int numTrans;
  Account     *acc   = mallocAccount();
  
  err = read( fd, &(acc->flags), sizeof(char) );
  if( err != sizeof(char) )
    {
    freeAccount(acc);
    return NULL;
    }
  
  err = read( fd, &(acc->type), sizeof(char) );
  if( err != sizeof(char) )
    {
    freeAccount(acc);
    return NULL;
    }
  
  acc->accountName = readString( fd, token );
  if( acc->accountName == NULL )
    {
    freeAccount(acc);
    return NULL;
    }
  
  acc->description = readString( fd, token );
  if( acc->accountName == NULL )
    {
    freeAccount(acc);
    return NULL;
    }
  
  acc->notes = readString( fd, token );
  if( acc->accountName == NULL )
    {
    freeAccount(acc);
    return NULL;
    }
  
  err = read( fd, &numTrans, sizeof(int) );
  if( err != sizeof(int) )
    {
    freeAccount(acc);
    return NULL;
    }
  
  /* read the transactions */
  for( i=0; i<numTrans; i++ )
    {
    Transaction *trans;
    trans = readTransaction( fd, token );
    if( trans == NULL )
      {
      printf(" numTrans = %d\n i = %d\n",numTrans,i);
      return acc;
      }
    else
      insertTransaction( acc, trans );
    }
  
  return acc;
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
readTransaction( int fd, int token )
  {
  int err=0;
  Date *date;
  Transaction *trans = (Transaction *)_malloc(sizeof(Transaction));
  
  trans->num = readString( fd, token );
  if( trans->num == NULL )
    {
    _free(trans);
    return NULL;
    }
  
  date = readDate( fd, token );
  if( date == NULL )
    {
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  trans->date = *date;
  _free(date);
  
  trans->description = readString( fd, token );
  if( trans->description == NULL )
    {
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  
  trans->memo = readString( fd, token );
  if( trans->memo == NULL )
    {
    XtFree(trans->description);
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  
  err = read( fd, &(trans->catagory), sizeof(int) );
  if( err != sizeof(int) )
    {
    XtFree(trans->memo);
    XtFree(trans->description);
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  
  err = read( fd, &(trans->reconciled), sizeof(char) );
  if( err != sizeof(char) )
    {
    XtFree(trans->memo);
    XtFree(trans->description);
    XtFree(trans->num);
    _free(trans);
    return NULL;
    }
  
  /* What used to be reconciled, is now cleared... transactions
   * aren't reconciled until you get your bank statement, and
   * use the reconcile window to mark the transaction reconciled */
  if( token == 0 )
    if( trans->reconciled == YREC )
      trans->reconciled = CREC;
  
  /* make sure the value of trans->reconciled is valid...
   * I have to do this mainly for if I change what NREC and
   * YREC are defined to be... this way it might loose all
   * the reconciled data, but at least the field is valid */
  if( (trans->reconciled != YREC) && (trans->reconciled != CREC) )
    trans->reconciled = NREC;
  
  err = read( fd, &(trans->amount), sizeof(int) );
  if( err != sizeof(int) )
    {
    XtFree(trans->memo);
    XtFree(trans->description);
    XtFree(trans->num);
    _free(trans);
    return NULL;
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
  
  str = (char *)XtMalloc(size);
  err = read( fd, str, size );
  if( err != size )
    {
    printf( " size = %d\n err = %d\n str = %s\n", size, err, str );
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
  
  err = read( fd, &(date->month), sizeof(int) );
  if( err != sizeof(int) )
    {
    _free(date);
    return NULL;
    }
  
  err = read( fd, &(date->day), sizeof(int) );
  if( err != sizeof(int) )
    {
    _free(date);
    return NULL;
    }
  
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
  
  fd = open( datafile, WFLAGS, PERMS );
  if( fd == -1 )
    {
    ERROR();
    return -1;
    }
  
  err = write( fd, &token, sizeof(int) );
  if( err != sizeof(int) )
    {
    ERROR();
    close(fd);
    return -1;
    }

  numAcc = data->numAcc;
  err = write( fd, &numAcc, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  for( i=0; i<numAcc; i++ )
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
  int i,numTrans;
  
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
  
  /* figure out numTrans */
  for( numTrans = 0; getTransaction(acc,numTrans) != NULL; numTrans++ )
    {}
  
  err = write( fd, &numTrans, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  for( i=0; i<numTrans; i++ )
    {
    err = writeTransaction( fd, getTransaction(acc,i) );
    if( err == -1 )
      return err;
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
int
writeTransaction( int fd, Transaction *trans )
  {
  int err=0;
  
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
  
  err = write( fd, &(trans->catagory), sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  err = write( fd, &(trans->reconciled), sizeof(char) );
  if( err != sizeof(char) )
    return -1;
  
  err = write( fd, &(trans->amount), sizeof(int) );
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
  
  for( size=0; str[size] != '\0'; size++ )
    {}
  size++;                /* we want to make sure we include the '\0'! 
			  * Otherwise, bad things happen */
  
  err = write( fd, &size, sizeof(int) );
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
  
  err = write( fd, &(date->year), sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  err = write( fd, &(date->month), sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  err = write( fd, &(date->day), sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  return err;
  }
