/********************************************************************\
 * QIFIO.c -- read from and writing Quicken Export format files     *
 *            for xacc (X-Accountant)                               *
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
 *   Author: Linas Vepstas                                          *
 * Internet: linas@linas.org                                        *
 *                                                                  *
 * NOTE: This software is *very alpha*, and is likely to core       *
 * dump on unexpected file formats, or otheriwse mangle and         *
 * loose data.  It sort-of works for the one QIF file its been      *
 * tested on ... The contents of this file are not well designed,   *
 * it is just a quick hack ...  a lot more qork is required.        *
 *                                                                  *
 * NOTE: the readxxxx/writexxxx functions changed the current       *
 *       position in the file, and so the order which these         *
 *       functions are called in important                          *
 *                                                                  *
\********************************************************************/

#include <fcntl.h>
#include <string.h>
#include <Xm/Xm.h>

#include "Account.h"
#include "Data.h"
#include "main.h"
#include "util.h"

#define PERMS   0666
#define WFLAGS  (O_WRONLY | O_CREAT | O_TRUNC)
#define RFLAGS  O_RDONLY

#ifdef DEBUG
#undef DEBUG
#endif
#define DEBUG printf

/********************************************************************\
 * xaccReadQIFLine                                                  * 
 *   reads in one line of ASCII, until cr-nl                        *
 *                                                                  * 
 * Args:   fd -- file descriptor                                    * 
 * Return: pointer to static char buff containing ASCII             * 
\********************************************************************/

#define XACC_LINE_BUFF_SIZE 1000
char * xaccReadQIFLine( int fd ) 
{
  static char linebuff [XACC_LINE_BUFF_SIZE+1];
  int n;
  int err;

  /* read chars from file until newline */
  n = 0;
  while (XACC_LINE_BUFF_SIZE > n) {
    err = read( fd, &linebuff[n], sizeof (char) );
    if( sizeof(char) != err )
      {
      return NULL;
      }
    if( '\n' == linebuff[n] ) { n++; break; }
    n++;
  }
  linebuff[n] = 0x0;

  /* if newline not found, bust out */
  if (XACC_LINE_BUFF_SIZE <= n) return NULL;
  return linebuff;
}

/********************************************************************\
 * xaccReadQIFDiscard                                               * 
 *   reads in lines of ASCII, discarding until end of transaction   *
 *                                                                  * 
 * Args:   fd -- file descriptor                                    * 
 * Return: first line of new transaction                            * 
\********************************************************************/

char * xaccReadQIFDiscard( int fd ) 
{
   char * qifline;

   qifline = xaccReadQIFLine (fd);
   if (!qifline) return NULL;
   if ('!' == qifline [0]) return qifline;

   while (qifline) {
     if (!strcmp (qifline, "^^\r\n")) {
        qifline = xaccReadQIFLine (fd);
        return qifline;
     }
     if ('!' == qifline [0]) return qifline;
     qifline = xaccReadQIFLine (fd);
   }
   return qifline;
}
  
/********************************************************************\
 * xaccReadQIFAccount                                               * 
 *   reads in account name, description, etc.                       *
 *                                                                  * 
 * Args:   fd -- file descriptor                                    * 
 * Args:   acc -- account structure to fill in                      * 
 * Return: first new line after end of transaction                  * 
\********************************************************************/

#define XACC_PREP_STRING(str) {			\
        int len = strlen (&qifline[1]); 	\
        len --;					\
        (str) = (char *)XtMalloc (len);		\
        strncpy ((str), &qifline[1], len);	\
        (str)[len-1] = 0x0;			\
}

#define XACC_PREP_NULL_STRING(str) {							\
        if (!(str)) { (str) = (char *)XtMalloc (sizeof(char)); (str)[0] = 0x0; }	\
}

char * xaccReadQIFAccount (int fd, Account * acc) 
{
   char * qifline;

   if (!acc) return NULL;

   acc -> flags = 0x0;    /* flags is byte */
   acc -> type = 0x0;     /* type is byte */
   acc -> accountName = 0x0;  /* string */
   acc -> description = 0x0;  /* string */

   qifline = xaccReadQIFLine (fd);

   if (!qifline) return NULL;
   if ('!' == qifline [0]) return qifline;

   /* scan for account name, description, type */
   while (qifline) {
     if ('N' == qifline [0]) {
        XACC_PREP_STRING (acc->accountName);
     } else 
     if ('D' == qifline [0]) {
        XACC_PREP_STRING (acc->description);
     } else 
     if ('T' == qifline [0]) {

        if (!strcmp (&qifline[1], "Bank\r\n")) {
           acc -> type = BANK;
        } else
        if (!strcmp (&qifline[1], "Invst\r\n")) {
           acc -> type = STOCK;
        } else {
           DEBUG ("Unsupported account type\n");
           DEBUG (&qifline[1]);
           acc -> type = 0x0;            /* hack alert -- */
        }
     } else 

     /* check for end-of-transaction marker */
     if (!strcmp (qifline, "^^\r\n")) {
        qifline = xaccReadQIFLine (fd);
        break;
     } else
     if ('!' == qifline [0]) break;
     qifline = xaccReadQIFLine (fd);
   }

   XACC_PREP_NULL_STRING (acc->accountName);
   XACC_PREP_NULL_STRING (acc->description);

   return qifline;
}

/********************************************************************\
 * xaccParseQIFDate                                                 * 
 *   parses date of the form MM/DD/YY                               *
 *                                                                  * 
 * Args:   date -- pointer to Date structure                        * 
 * Args:   str -- pointer to string rep of date                     * 
 * Return: void                                                     * 
\********************************************************************/

void xaccParseQIFDate (Date * dat, char * str) 
{
   char * tok;

   if (!str) return;
   tok = strchr (str, '/');
   if (!tok) return;
   *tok = 0x0;
   dat->month = atoi (str);

   str = tok+sizeof(char);
   tok = strchr (str, '/');
   if (!tok) return;
   *tok = 0x0;
   dat->day = atoi (str);

   str = tok+sizeof(char);
   tok = strchr (str, '\r');
   if (!tok) return;
   *tok = 0x0;
   dat->year = atoi (str);
   dat->year += 1900;
}

/********************************************************************\
 * xaccParseQIFAmount                                               * 
 *   parses dollar ammount of the form DDD,DDD,DDD.CC               *
 *                                                                  * 
 * Args:   str -- pointer to string rep of sum                      * 
 * Return: int -- in pennies                                        * 
\********************************************************************/

double xaccParseQIFAmount (char * str) 
{
   char * tok;
   double dollars = 0.0;

   if (!str) return 0.0;

   tok = strchr (str, ',');
   if (tok) {
      *tok = 0x0;
      dollars = ((double) (1000 * atoi (str)));
      str = tok+sizeof(char);
   }

   tok = strchr (str, ',');
   if (tok) {
      *tok = 0x0;
      dollars *= 1000.0;
      dollars += ((double) (1000 * atoi (str)));
      str = tok+sizeof(char);
   }

   tok = strchr (str, '.');
   if (tok) {
      *tok = 0x0;
      dollars += ((double) (atoi (str)));
      str = tok+sizeof(char);

      tok = strchr (str, '\r');
      if (!tok) return dollars;
      *tok = 0x0;
      dollars += 0.01 * ((double) atoi (str));
      return dollars;

   } else {
      tok = strchr (str, '\r');
      if (tok) {
         *tok = 0x0;
         dollars += ((double) (atoi (str)));
         return dollars;
      } else {
         return 0.0;
      }
   }

   return dollars;
}

/********************************************************************\
\********************************************************************/

Account *
xaccGetXferQIFAccount (Account *acc, char *qifline)
{
   Account *xfer_acc;
   AccountGroup *rootgrp;
   char * tmp;

   /* remove square brackets from name, remove carriage return ... */
   qifline = &qifline[1];
   if ('[' == qifline[0]) {
      qifline = &qifline[1];
      tmp = strchr (qifline, ']');
      if (tmp) *tmp = 0x0;
   }
   tmp = strchr (qifline, '\r');
   if(tmp) *tmp = 0x0;
   tmp = strchr (qifline, '\n');
   if(tmp) *tmp = 0x0;

   /* see if the account exists */
   rootgrp = xaccGetRootGroupOfAcct (acc);
   xfer_acc = xaccGetAccountFromName (rootgrp, qifline);

   /* if not, create it */
   if (!xfer_acc) {
      xfer_acc = mallocAccount ();
      xfer_acc->accountName = XtNewString (qifline);
      xfer_acc->type = BANK;
      insertAccount (rootgrp, xfer_acc);
   }

   return xfer_acc;
}

/********************************************************************\
\********************************************************************/

Account *
xaccGetSecurityQIFAccount (Account *acc, char *qifline)
{
   Account *xfer_acc;
   AccountGroup *rootgrp;
   char * tmp;

   /* remove square brackets from name, remove carriage return ... */
   qifline = &qifline[1];
   if ('[' == qifline[0]) {
      qifline = &qifline[1];
      tmp = strchr (qifline, ']');
      if (tmp) *tmp = 0x0;
   }
   tmp = strchr (qifline, '\r');
   if(tmp) *tmp = 0x0;
   tmp = strchr (qifline, '\n');
   if(tmp) *tmp = 0x0;

   /* see if the account exists */
   rootgrp = xaccGetRootGroupOfAcct (acc);
   xfer_acc = xaccGetAccountFromName (rootgrp, qifline);

   /* if not, create it */
   if (!xfer_acc) {
      xfer_acc = mallocAccount ();
      xfer_acc->accountName = XtNewString (qifline);
      xfer_acc->type = STOCK;
      xaccInsertSubAccount (acc, xfer_acc);
   }

   return xfer_acc;
}

/********************************************************************\
 * xaccReadQIFTransaction                                           * 
 *   reads in transaction                                           *
 *                                                                  * 
 * NB: this code will have to different, depending on the           *
 * type of transaction (bank, stock, etc.                           * 
 *                                                                  * 
 * Args:   fd -- file descriptor                                    * 
 * Args:   acc -- account structure to fill in                      * 
 * Return: first new line after end of transaction                  * 
\********************************************************************/

#define XACC_ACTION(q,x)			\
   if (!strncmp (&qifline[1], q, strlen(q))) {	\
      trans->action = XtNewString(x);		\
   } else


char * xaccReadQIFTransaction (int fd, Account *acc)
{
   Transaction *trans;
   char * qifline;
   int isneg = 0;
   int got_share_quantity = 0;
   int share_xfer = 0;
   int is_security = 0;
   Account *sub_acc = 0x0;
   Account *xfer_acc = 0x0;

   if (!acc) return NULL;
   trans = (Transaction *)_malloc(sizeof(Transaction));

   trans -> num = 0x0;          /* string */ 
   trans -> description = 0x0;  /* string */
   trans -> memo = 0x0;         /* string */
   trans -> action = 0x0;       /* string */
   trans -> catagory = 0;       /* category is int */
   trans -> damount = 0.0;      /* amount is double */
   trans -> share_price= 1.0;   /* share_price is double */
   trans -> reconciled = NREC;  /* reconciled is byte */
   /* other possible values ... */
   /* trans->reconciled = YREC;  trans->reconciled = CREC; */

   trans -> date.year = 1970;   /* int */
   trans -> date.month = 1;     /* int */
   trans -> date.day = 1;       /* int */

   trans -> debit = NULL;
   trans -> credit = NULL;
  
   qifline = xaccReadQIFLine (fd);

   if (!qifline) {
      xaccRemoveTransaction ((Account *) trans->debit, trans);
      xaccRemoveTransaction ((Account *) trans->credit, trans);
      freeTransaction (trans);
      return NULL;
   }
   if ('!' == qifline [0]) return qifline;

   /* scan for transaction date, description, type */
   while (qifline) {
     /* M == memo field */
     if ('M' == qifline [0]) {  
        XACC_PREP_STRING (trans->memo);
     } else 

     /* P == Payee, for Bank accounts */
     if ('P' == qifline [0]) {   
        XACC_PREP_STRING (trans->description);
     } else

     /* Y == Name of Security */
     if ('Y' == qifline [0]) {   
        XACC_PREP_STRING (trans->description);

        is_security = 1;
        if (share_xfer) {
           /* locate or create the sub-account account */
           sub_acc = xaccGetSecurityQIFAccount (acc, qifline);
        }

     } else

     /* N == check numbers for Banks, but Action for portfolios */
     if ('N' == qifline [0]) {   

        if (!strncmp (qifline, "NSell", 5)) isneg = 1;
        if (!strncmp (qifline, "NSell", 5)) share_xfer = 1;
        if (!strncmp (qifline, "NBuy", 4)) share_xfer = 1;

        /* if a recognized action, convert to our cannonical names */
        XACC_ACTION ("Buy", "Buy")
        XACC_ACTION ("Sell", "Sell")
        XACC_ACTION ("Div", "Div")
        XACC_ACTION ("CGLong", "LTCG")
        XACC_ACTION ("CGShort", "STCG")
        XACC_ACTION ("IntInc", "Int")
        XACC_ACTION ("DEP", "Deposit")
        XACC_ACTION ("XIn", "Deposit")
        XACC_ACTION ("XOut", "Withdraw")
        {
          XACC_PREP_STRING (trans->num);
        }
     } else

     if ('D' == qifline [0]) {   /* D == date */
         xaccParseQIFDate (&(trans->date), &qifline[1]);
     } else 

     if ('T' == qifline [0]) {   /* T == total */

         /* ignore T for stock transactions, since T is a dollar amount */
         if (0 == got_share_quantity) {
            trans -> damount = xaccParseQIFAmount (&qifline[1]);  
            if (isneg) trans -> damount = - (trans->damount);
         }
     } else 

     if ('I' == qifline [0]) {   /* I == share price */
         trans -> share_price = xaccParseQIFAmount (&qifline[1]); 
     } else 

     if ('Q' == qifline [0]) {   /* Q == number of shares */
         trans -> damount = xaccParseQIFAmount (&qifline[1]);  
         if (isneg) trans -> damount = - (trans->damount);
         got_share_quantity = 1;
     } else 

     /* L == name of acount from which transfer occured */
     if ('L' == qifline [0]) {   
         /* locate the transfer account */
         xfer_acc = xaccGetXferQIFAccount (acc, qifline);
     } else 

     /* $ == dollar amount -- always preceeded by 'L' */
     if ('$' == qifline [0]) {   
         /* Currently, it appears that the $ amount is a redundant 
          * number that we can safely ignore.  If we wanted to get
          * fancy, we could use it to double-check the above work,
          * since it appears to always appear the last entry in the
          * transaction. */
        double got, wanted;
        wanted = xaccParseQIFAmount (&qifline[1]);
        got = (trans->damount) * (trans->share_price);
        if (isneg) got = -got;
        if (wanted != got) {
           printf ("QIF Parse Error: wanted %f got %f \n", wanted, got);
        }
     } else 

     if ('O' == qifline [0]) {   /* O == round-off error ??? */
         /* hack alert */
     } else 

     /* check for end-of-transaction marker */
     if (!strcmp (qifline, "^^\r\n")) {
        break;
     } else
     if ('!' == qifline [0]) break;
     qifline = xaccReadQIFLine (fd);
   }

   if ('!' == qifline[0]) {
      xaccRemoveTransaction ((Account *) trans->debit, trans);
      xaccRemoveTransaction ((Account *) trans->credit, trans);
      freeTransaction (trans);
      return qifline;
   }

   XACC_PREP_NULL_STRING (trans->num);
   XACC_PREP_NULL_STRING (trans->memo);
   XACC_PREP_NULL_STRING (trans->description);
   XACC_PREP_NULL_STRING (trans->action);


   /* fundamentally differnt handling for securities and non-securities */
   if (is_security) {

      /* if the transaction  is a sale/purchase of a security, 
       * then it is a defacto transfer between the brokerage account 
       * and the stock account.  */
      if (share_xfer) {
         /* Insert the transaction into the main brokerage 
          * account as a debit, unless an alternate account
          * was specified. */
         if (xfer_acc) {
            trans->debit = (struct _account *) xfer_acc;
            insertTransaction (xfer_acc, trans);
         } else {
            trans->debit = (struct _account *) acc;
            insertTransaction (acc, trans);
         }

         /* normally, the security account is pointed at by 
          * sub_acc; the security account is credited.
          * But, just in case its missing, avoid a core dump */
         if (sub_acc) {
            trans->credit = (struct _account *) sub_acc;
            insertTransaction (sub_acc, trans);
         }
      } else {

         /* else, we are here if its not a share transfer. 
          * It is probably dividend or other income */

         /* if a transfer account is specified, the transfer 
          * account gets the dividend credit; otherwise, the 
          * main account gets it */
         if (xfer_acc) {
            trans->credit = (struct _account *) xfer_acc;
            insertTransaction (xfer_acc, trans);
         } else {
            trans->credit = (struct _account *) acc;
            insertTransaction( acc, trans );
         }
      }

   } else {
      /* if we are here, its not a security, but an ordinary account */
      /* if a transfer account was specified,  it is the debited account */
      if (xfer_acc) {
         trans->debit = (struct _account *) xfer_acc;
         insertTransaction (xfer_acc, trans);
      }

      /* the transaction itself appears as a credit */
      trans->credit = (struct _account *) acc;
      insertTransaction( acc, trans );
   }

   return qifline;
}
  
/********************************************************************\
 * read a sequence of transactions, inserting them into 
 * the indicated account
\********************************************************************/

char * xaccReadQIFTransList (int fd, Account *acc)
{
   char * qifline;

   if (!acc) return 0x0;
   do { 
      qifline = xaccReadQIFTransaction (fd, acc);
   } while (qifline);
   return qifline;
}

/********************************************************************\
 ********************** LOAD DATA ***********************************
\********************************************************************/

/********************************************************************\
 * xaccReadQIFData                                                  * 
 *   reads in the data from file datafile                           *
 *                                                                  * 
 * Args:   datafile - the file to load the data from                * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/
AccountGroup *
xaccReadQIFData( char *datafile )
  {
  int  fd;
  int  skip = 0;
  char * qifline;
  AccountGroup *grp = mallocAccountGroup();
  
  fd = open( datafile, RFLAGS, 0 );
  if( fd == -1 )
    {
    ERROR();
    freeAccountGroup(grp);
    return NULL;
    }
  
  /* read the first line of the file */
  qifline = xaccReadQIFLine (fd); 
  if( NULL == qifline )
    {
    ERROR();
    close(fd);
    freeAccountGroup(grp);
    return NULL;
    }
  
  while (qifline) {
     if (!strcmp (qifline, "!Type:Class\r\n")) {
        DEBUG ("got class\n");
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else

     if (!strcmp (qifline, "!Type:Bank \r\n")) {
        Account *acc   = mallocAccount();
        DEBUG ("got bank\n");

        acc->type = BANK;
        acc->accountName = XtNewString ("Quicken Bank Account");

        insertAccount( grp, acc );
        qifline = xaccReadQIFTransList (fd, acc);
        continue;
     } else

     if (!strcmp (qifline, "!Type:Cat\r\n")) {
        DEBUG ("got category\n");
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else

     if (!strcmp (qifline, "!Type:Invst\r\n")) {
        Account *acc   = mallocAccount();
        DEBUG ("got Invst\n");

        acc->type = BANK;
        acc->accountName = XtNewString ("Quicken Investment Account");

        insertAccount( grp, acc );
        qifline = xaccReadQIFTransList (fd, acc);
        continue;
     } else

     if (!strcmp (qifline, "!Option:AutoSwitch\r\n")) {
        DEBUG ("got autoswitch on\n");
        skip = 1;
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else

     if (!strcmp (qifline, "!Clear:AutoSwitch\r\n")) {
        DEBUG ("got autoswitch clear");
        skip = 0;
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else

     if (!strcmp (qifline, "!Account\r\n")) {
        if (skip) {
           /* loop and read all of the account names and descriptions */
           /* no actual dollar data is expected to be read here ... */
           while (qifline) {
              qifline = xaccReadQIFDiscard (fd);
              if (!qifline) break;
              if ('!' == qifline[0]) break;
           }
        } else {
           /* read account name, followed by dollar data ... */
           Account *acc   = mallocAccount();
           DEBUG ("got account");
           qifline = xaccReadQIFAccount (fd, acc);
           if (!qifline) {  /* free up malloced data if the read bombed. */
              freeAccount(acc); 
              continue;
           }
           insertAccount( grp, acc );
   
           /* spin until start of transaction records */
           /* in theory, in a perfect file, the transaction data follows immediately */
           while (qifline) { 
              if ('!' == qifline[0]) break;
              qifline = xaccReadQIFDiscard (fd);
           }
   
           /* read transactions */
           if (qifline) qifline = xaccReadQIFTransList (fd, acc);
        }    
        continue;
     } else

     if ('!' == qifline[0]) {
        DEBUG ("unknown or unexpected stanza:");
        DEBUG (qifline);
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else {

       qifline = xaccReadQIFDiscard (fd);
      }
   }

   close(fd);
   return grp;
}

/* ========================== END OF FILE ======================= */
