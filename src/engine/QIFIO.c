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
 * NOTE: the readxxxx/writexxxx functions changed the current       *
 *       position in the file, and so the order which these         *
 *       functions are called in important                          *
 *                                                                  *
\********************************************************************/

#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include "config.h"

#include "Account.h"
#include "Group.h"
#include "FileIO.h"
#include "util.h"

#define PERMS   0666
#define WFLAGS  (O_WRONLY | O_CREAT | O_TRUNC)
#define RFLAGS  O_RDONLY

/** GLOBALS *********************************************************/

static int          error_code=0; /* error code, if error occurred */

/*******************************************************/

int xaccGetQIFIOError (void)
{
   return error_code;
}

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

#define NSTRNCMP(x,y) (0==strncmp((x),(y),strlen(y)))

char * xaccReadQIFDiscard( int fd ) 
{
   char * qifline;

   qifline = xaccReadQIFLine (fd);
   if (!qifline) return NULL;
   if ('!' == qifline [0]) return qifline;

   while (qifline) {
     if (NSTRNCMP(qifline, "^^")) {
        qifline = xaccReadQIFLine (fd);
        return qifline;
     } else
     if (NSTRNCMP(qifline, "^\n")) {
        qifline = xaccReadQIFLine (fd);
        return qifline;
     } else
     if (NSTRNCMP(qifline, "^\r")) {
        qifline = xaccReadQIFLine (fd);
        return qifline;
     } else
     if ('!' == qifline [0]) return qifline;
     qifline = xaccReadQIFLine (fd);
   }
   return qifline;
}
  
/********************************************************************\
 * xaccReadQIFCategory                                              * 
 *   reads in category account name, description, etc.              *
 *                                                                  *
 * implementation resembles xaccReadQIFCategory                     *
 *                                                                  * 
 * Args:   fd -- file descriptor                                    * 
 * Args:   acc -- account structure to fill in                      * 
 * Return: first new line after end of transaction                  * 
\********************************************************************/

#define XACC_PREP_STRING(str) {			\
	char * tok;				\
	int len;				\
	tok = strchr (&qifline[1], '\n');	\
	if (tok) *tok = 0x0;			\
	tok = strchr (&qifline[1], '\r');	\
	if (tok) *tok = 0x0;			\
        len = strlen (&qifline[1]); 		\
        (str) = (char *)XtMalloc (len+1);	\
        strncpy ((str), &qifline[1], len);	\
        (str)[len] = 0x0;			\
}

#define XACC_PREP_NULL_STRING(str) {							\
        if (!(str)) { (str) = (char *)XtMalloc (sizeof(char)); (str)[0] = 0x0; }	\
}

char * xaccReadQIFCategory (int fd, Account * acc) 
{
   char * qifline;

   if (!acc) return NULL;

   acc -> flags = 0x0;    /* flags is byte */
   acc -> type = -1;      /* type is byte */
   acc -> accountName = 0x0;  /* string */
   acc -> description = 0x0;  /* string */
   acc -> notes = 0x0;  /* string */

   qifline = xaccReadQIFLine (fd);
   if (!qifline) return NULL;
   if ('!' == qifline [0]) return qifline;

   /* scan for account name, description, type */
   while (qifline) {

     /* N == Name */
     if ('N' == qifline [0]) {
        XACC_PREP_STRING (acc->accountName);
     } else 

     /* D == Description */
     if ('D' == qifline [0]) {
        XACC_PREP_STRING (acc->description);
     } else 

     /* T == Taxable -- this income is taxable */
     if ('T' == qifline [0]) {
     } else 

     /* E == Expense Category */
     if ('E' == qifline [0]) {
        acc->type = EXPENSE;
     } else 

     /* I == Income Category */
     if ('I' == qifline [0]) {
        acc->type = INCOME;
     } else 

     /* R == Tax Rate Indicator; -- some number ... */
     if ('R' == qifline [0]) {
     } else 

     /* check for end-of-transaction marker */
     if (NSTRNCMP(qifline, "^^")) {
        break;
     } else
     if (NSTRNCMP(qifline, "^\n")) {
        break;
     } else
     if (NSTRNCMP(qifline, "^\r")) {
        break;
     } else
     if ('!' == qifline [0]) break;

     qifline = xaccReadQIFLine (fd);
   }

   XACC_PREP_NULL_STRING (acc->accountName);
   XACC_PREP_NULL_STRING (acc->description);
   XACC_PREP_NULL_STRING (acc->notes);

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

char * xaccReadQIFAccount (int fd, Account * acc) 
{
   char * qifline;

   if (!acc) return NULL;

   acc -> flags = 0x0;    /* flags is byte */
   acc -> type = -1;      /* type is byte  */
   acc -> accountName = 0x0;  /* string */
   acc -> description = 0x0;  /* string */
   acc -> notes = 0x0;  /* string */

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

        if (NSTRNCMP (&qifline[1], "Bank")) {
           acc -> type = BANK;
        } else
        if (NSTRNCMP (&qifline[1], "Cash")) {
           acc -> type = CASH;
        } else
        if (NSTRNCMP (&qifline[1], "CCard")) {
           acc -> type = CREDIT;
        } else
        if (NSTRNCMP (&qifline[1], "Invst")) {
           acc -> type = STOCK;
        } else
        if (NSTRNCMP (&qifline[1], "Oth A")) {
           acc -> type = ASSET;
        } else 
        if (NSTRNCMP (&qifline[1], "Oth L")) {
           acc -> type = LIABILITY;
        } else 
        {
           printf ("QIF Parse: Unsupported account type %s \n", &qifline[1]);
           acc -> type = -1;            /* hack alert -- */
        }
     } else 

     /* check for end-of-transaction marker */
     if (NSTRNCMP (qifline, "^^")) {
        break;
     } else
     if (NSTRNCMP (qifline, "^\n")) {
        break;
     } else
     if (NSTRNCMP (qifline, "^\r")) {
        break;
     } else
     if ('!' == qifline [0]) break;
     qifline = xaccReadQIFLine (fd);
   }

   XACC_PREP_NULL_STRING (acc->accountName);
   XACC_PREP_NULL_STRING (acc->description);
   XACC_PREP_NULL_STRING (acc->notes);

   return qifline;
}

/********************************************************************\
 * read a sequence of accounts or categories, inserting them into 
 * the indicated group
\********************************************************************/

char * xaccReadQIFAccList (int fd, AccountGroup *grp, int cat)
{
   char * qifline;
   Account *acc;
   char *str, *tok;

   if (!grp) return 0x0;
   do { 
      acc = xaccMallocAccount();
      if (cat) { 
         qifline = xaccReadQIFCategory (fd, acc);
      } else {
         qifline = xaccReadQIFAccount (fd, acc);
      }
      if ('!' == qifline [0]) break;

      if (-1 == acc->type) {  /* free up malloced data if unknown account type */
         xaccFreeAccount(acc); 
         continue;
      }
      if (!qifline) {  /* free up malloced data if the read bombed. */
         xaccFreeAccount(acc); 
         continue;
      }

      /* check to see if this is a sub-account.
       * Sub-accounts will have a colon in the name */
      str = acc->accountName;
      tok = strchr (str, ':');
      if (tok) {
         Account *parent;

         /* find the parent account, and parent to it */
         *tok = 0x0;
         parent = xaccGetAccountFromName (grp, str);
         *tok = ':';

         if (parent) {

            /* trim off the parent account name ... */
            /* tok += sizeof(char);  leave behind the colon ... */
            str = strdup (tok);
            free (acc->accountName);
            acc->accountName = str;

            xaccInsertSubAccount( parent, acc );
         } else {
            /* we should never get here if the qif file is OK */
            insertAccount( grp, acc );  
         }
      } else {
         insertAccount( grp, acc );
      }

   } while (qifline);


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
   if (!tok) {
      tok = strchr (str, '\n');
      if (!tok) return;
   }
   *tok = 0x0;
   dat->year = atoi (str);
   dat->year += 1900;
}

/********************************************************************\
\********************************************************************/

static int
GuessAccountType (char * qifline)
{
   int acc_type = EXPENSE;

   /* Guessing Bank is dangerous, since it could be "Bank Charges"
    * if (strstr (qifline, "Bank")) {
    *    acc_type = BANK;
    * } else
    */

   if (strstr (qifline, "Bills")) {
      acc_type = EXPENSE;
   } else

   if (strstr (qifline, "Cash")) {
      acc_type = CASH;
   } else

   if (strstr (qifline, "Income")) {
      acc_type = INCOME;
   } else

   if (strstr (qifline, "Card")) {
      acc_type = CREDIT;
   } else

   {
      acc_type = EXPENSE;
   }

   return acc_type;
}

/********************************************************************\
\********************************************************************/

static Account *
GetSubQIFAccount (AccountGroup *rootgrp, char *qifline, int acc_type)
{
   Account *xfer_acc;
   char * sub_ptr;
   int i;

   /* search for colons in name -- this indicates a sub-account */
   sub_ptr = strchr (qifline, ':');
   if (sub_ptr) {
      *sub_ptr = 0;
   }

   /* see if the account exists; but search only one level down,
    * not the full account tree */
   xfer_acc = NULL;
   for (i=0; i<rootgrp->numAcc; i++) {
      Account *acc = rootgrp->account[i];
      if (!strcmp(acc->accountName, qifline)) {
         xfer_acc = acc;
         break;
      }
   }

   /* if not, create it */
   if (!xfer_acc) {
      xfer_acc = xaccMallocAccount ();
      xfer_acc->accountName = strdup (qifline);
      xfer_acc->description = strdup ("");
      xfer_acc->notes = strdup ("");

      if (0 > acc_type) acc_type = GuessAccountType (qifline);
      xfer_acc->type = acc_type;
      insertAccount (rootgrp, xfer_acc);
   }

   /* if this account name had sub-accounts, get those */
   if (sub_ptr) {
      sub_ptr ++;
      rootgrp = xfer_acc->children;
      if (!rootgrp) {
         rootgrp = xaccMallocAccountGroup();
         xfer_acc->children = rootgrp;
         rootgrp->parent = xfer_acc;
      }
      xfer_acc = GetSubQIFAccount (rootgrp, sub_ptr, acc_type);
   }
   return xfer_acc;
}

/********************************************************************\
\********************************************************************/

Account *
xaccGetXferQIFAccount (Account *acc, char *qifline)
{
   Account *xfer_acc;
   AccountGroup *rootgrp;
   char * tmp;
   int acc_type = -1;

   /* remove square brackets from name, remove carriage return ... */
   qifline = &qifline[1];
   if ('[' == qifline[0]) {
      qifline = &qifline[1];
      tmp = strchr (qifline, ']');
      if (tmp) *tmp = 0x0;
      acc_type = BANK;
   }
   tmp = strchr (qifline, '\r');
   if(tmp) *tmp = 0x0;
   tmp = strchr (qifline, '\n');
   if(tmp) *tmp = 0x0;

   /* see if the account exists, create it if not */
   rootgrp = xaccGetAccountRoot (acc);
   xfer_acc = GetSubQIFAccount (rootgrp, qifline, acc_type);

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

   /* hack alert -- should search for colons in name, do an algorithm
    * similar to Xfer routine above  */
   /* see if the account exists */
   rootgrp = xaccGetAccountRoot (acc);
   xfer_acc = xaccGetAccountFromName (rootgrp, qifline);

   /* if not, create it */
   if (!xfer_acc) {
      xfer_acc = xaccMallocAccount ();
      xfer_acc->accountName = strdup (qifline);
      xfer_acc->description = strdup ("");
      xfer_acc->notes = strdup ("");
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

#define XACC_ACTION(q,x)				\
   if (!strncmp (&qifline[1], q, strlen(q))) {		\
      trans->source_split.action = strdup(x);	\
   } else


char * xaccReadQIFTransaction (int fd, Account *acc)
{
   Transaction *trans;
   Split *split = NULL;
   char * qifline;
   int isneg = 0;
   int got_share_quantity = 0;
   int share_xfer = 0;
   int is_security = 0;
   Account *sub_acc = 0x0;
   Account *xfer_acc = 0x0;
   double adjust = 0.0;

   if (!acc) return NULL;

   qifline = xaccReadQIFLine (fd);
   if (!qifline) return NULL;
   if ('!' == qifline [0]) return qifline;

   trans = xaccMallocTransaction ();

   /* scan for transaction date, description, type */
   while (qifline) {
     /* C == Cleared / Reconciled */
     if ('C' == qifline [0]) {  
         /* Quicken uses C* and Cx, while MS Money uses CX.
          * I don't know what * and x are supposed to differentiate 
          */
        xaccTransSetReconcile (trans, CREC);
     } else 

     /* D == date */
     if ('D' == qifline [0]) {  
         xaccParseQIFDate (&(trans->date), &qifline[1]);
     } else 

     /* E == memo for split */
     if ('E' == qifline [0]) {   
        if (split) {
           XACC_PREP_STRING (split->memo);
        }
     } else 

     /* I == share price */
     if ('I' == qifline [0]) {   
         trans -> source_split.share_price = xaccParseUSAmount (&qifline[1]); 
     } else 

     /* L == name of acount from which transfer occured */
     if ('L' == qifline [0]) {   
         /* locate the transfer account */
         xfer_acc = xaccGetXferQIFAccount (acc, qifline);
     } else 

     /* M == memo field */
     if ('M' == qifline [0]) {  
        XACC_PREP_STRING (trans->source_split.memo);
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

     /* O == adjustments */
     /* hack alert -- sometimes adjustments are quite large.
      * I have no clue why, and what to do about it.  For what 
      * its worth, I can prove that Quicken version 3.0 makes 
      * math errors ... */
     if ('O' == qifline [0]) {   
        double pute;
        adjust = xaccParseUSAmount (&qifline[1]);
        pute = (trans->source_split.damount) * (trans->source_split.share_price);
        if (isneg) pute = -pute;

        printf ("QIF Warning: Adjustment of %.2f to amount %.2f not handled \n", adjust, pute);
     } else 

     /* P == Payee, for Bank accounts */
     if ('P' == qifline [0]) {   
        XACC_PREP_STRING (trans->description);
     } else

     /* Q == number of shares */
     if ('Q' == qifline [0]) {   
         trans -> source_split.damount = xaccParseUSAmount (&qifline[1]);  
         if (isneg) trans -> source_split.damount = - (trans->source_split.damount);
         got_share_quantity = 1;
     } else 

     /* S == split */
     if ('S' == qifline [0]) {   
         split = xaccMallocSplit();

         split -> memo = 0x0;         /* string */
         split -> damount = 0.0;      /* amount is double */
         split -> share_price= 1.0;   /* share_price is double */
         split -> reconciled = NREC;  /* reconciled is byte */
         split -> parent = trans;     /* parent transaction */
         split -> acc = (struct _account *) xaccGetXferQIFAccount (acc, qifline);

         xaccTransAppendSplit (trans, split);

         /* hack alert -- we should insert this split into 
          * the split account, and remove the L field */
     } else 

     /* T == total */
     if ('T' == qifline [0]) {   
         /* ignore T for stock transactions, since T is a dollar amount */
         if (0 == got_share_quantity) {
            trans -> source_split.damount = xaccParseUSAmount (&qifline[1]);  
            if (isneg) trans -> source_split.damount = - (trans->source_split.damount);
         }
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

     /* $ == dollar amount */
     if ('$' == qifline [0]) {   
        /* for splits, $ records the part of the total for each split */
        if (split) {
           split -> damount = xaccParseUSAmount (&qifline[1]);  
        } else {
           /* Currently, it appears that the $ amount is a redundant 
            * number that we can safely ignore.  To get fancy,
            * we use it to double-check the above work, since it 
            * appears to always appear the last entry in the
            * transaction.  Round things out to pennies, to 
            * handle round-off errors. 
            */
           double parse, pute;
           int got, wanted;
           parse = xaccParseUSAmount (&qifline[1]);
           pute = (trans->source_split.damount) * (trans->source_split.share_price);
           if (isneg) pute = -pute;
   
           wanted = (int) (100.0 * parse + 0.5);
           got = (int) (100.0 * (pute+adjust) + 0.5);
           if (wanted != got) {
              printf ("QIF Parse Error: wanted %f got %f \n", parse, pute);
           }
        }
     } else 

     /* check for end-of-transaction marker */
     if (NSTRNCMP (qifline, "^^")) {
        break;
     } else
     if (NSTRNCMP (qifline, "^\n")) {
        break;
     } else
     if (NSTRNCMP (qifline, "^\r")) {
        break;
     } else
     if ('!' == qifline [0]) break;
     qifline = xaccReadQIFLine (fd);

   }

   /* at this point, we should see an end-of-transaction marker
    * if we see something else, assume the worst, free the last 
    * transaction, and return */
   if ('!' == qifline[0]) {
      xaccFreeTransaction (trans);
      return qifline;
   }

   XACC_PREP_NULL_STRING (trans->num);
   XACC_PREP_NULL_STRING (trans->description);
   XACC_PREP_NULL_STRING (trans->source_split.memo);
   XACC_PREP_NULL_STRING (trans->source_split.action);


   /* fundamentally differnt handling for securities and non-securities */
   if (is_security) {

      /* if the transaction  is a sale/purchase of a security, 
       * then it is a defacto transfer between the brokerage account 
       * and the stock account.  */
      if (share_xfer) {
         if (!split) {
            split = xaccMallocSplit ();
            xaccTransAppendSplit (trans, split);
         }

         /* Insert the transaction into the main brokerage 
          * account as a debit, unless an alternate account
          * was specified. */
         if (xfer_acc) {
            split->acc = (struct _account *) xfer_acc;
            xaccInsertSplit (xfer_acc, split);
         } else {
            split->acc = (struct _account *) acc;
            xaccInsertSplit (acc, split);
         }

         /* normally, the security account is pointed at by 
          * sub_acc; the security account is credited.
          * But, just in case its missing, avoid a core dump */
         if (sub_acc) {
            trans->source_split.acc = (struct _account *) sub_acc;
            xaccInsertSplit (sub_acc, split);
         }
      } else {

         /* else, we are here if its not a share transfer. 
          * It is probably dividend or other income */

         /* if a transfer account is specified, the transfer 
          * account gets the dividend credit; otherwise, the 
          * main account gets it */
         if (xfer_acc) {
            trans->source_split.acc = (struct _account *) xfer_acc;
            xaccInsertSplit (xfer_acc, &(trans->source_split));
         } else {
            trans->source_split.acc = (struct _account *) acc;
            xaccInsertSplit (acc, &(trans->source_split));
         }
      }

   } else {
      /* if we are here, its not a security, but an ordinary account */
      /* if a transfer account was specified,  it is the debited account */
      if (xfer_acc) {
         if (!split) {
            split = xaccMallocSplit ();
            xaccTransAppendSplit (trans, split);
         }
         split->acc = (struct _account *) xfer_acc;
         xaccInsertSplit (xfer_acc, split);
      }

      /* the transaction itself appears as a credit */
      trans->source_split.acc = (struct _account *) acc;
      xaccInsertSplit (acc, &(trans->source_split));
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
   qifline = xaccReadQIFTransaction (fd, acc);
   while (qifline) {
      if ('!' == qifline[0]) break;
      qifline = xaccReadQIFTransaction (fd, acc);
   } 
   return qifline;
}

/********************************************************************\
 ********************** LOAD DATA ***********************************
\********************************************************************/

/********************************************************************\
 * xaccReadQIFAccountGroup                                                  * 
 *   reads in the data from file datafile                           *
 *                                                                  * 
 * Args:   datafile - the file to load the data from                * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/

#define STRSTR(x,y) ((NSTRNCMP(x,y)) ||  (NSTRNCMP((&(x)[1]), y)))

AccountGroup *
xaccReadQIFAccountGroup( char *datafile )
  {
  int  fd;
  int  skip = 0;
  char * qifline;
  AccountGroup *grp;
  char buf[BUFSIZE];
  
  fd = open( datafile, RFLAGS, 0 );
  if( fd == -1 )
    {
    error_code = ERR_FILEIO_FILE_NOT_FOUND;
    return NULL;
    }
  
  /* read the first line of the file */
  qifline = xaccReadQIFLine (fd); 
  if( NULL == qifline )
    {
    error_code = ERR_FILEIO_FILE_EMPTY;
    close(fd);
    return NULL;
    }

  grp = xaccMallocAccountGroup();
  
  while (qifline) {
     if (STRSTR (qifline, "Type:Bank")) {
        Account *acc   = xaccMallocAccount();
        DEBUG ("got bank\n");

        acc->type = BANK;
        acc->accountName = strdup ("Quicken Bank Account");
        acc->description = strdup ("");
        acc->notes = strdup ("");

        insertAccount( grp, acc );
        qifline = xaccReadQIFTransList (fd, acc);
        continue;
     } else

     if (STRSTR (qifline, "Type:Cat")) {
        DEBUG ("got category\n");
        qifline = xaccReadQIFAccList (fd, grp, 1);
        continue;
     } else

     if (STRSTR (qifline, "Type:Class")) {
        DEBUG ("got class\n");
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else

     if (STRSTR (qifline, "Type:Invst")) {
        Account *acc   = xaccMallocAccount();
        DEBUG ("got Invst\n");

        acc->type = BANK;
        acc->accountName = strdup ("Quicken Investment Account");
        acc->description = strdup ("");
        acc->notes = strdup ("");

        insertAccount( grp, acc );
        qifline = xaccReadQIFTransList (fd, acc);
        continue;
     } else

     if (STRSTR (qifline, "Type:Memorized")) {
        DEBUG ("got memorized\n");
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else

     if (STRSTR (qifline, "Option:AutoSwitch")) {
        DEBUG ("got autoswitch on\n");
        skip = 1;
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else

     if (STRSTR (qifline, "Clear:AutoSwitch")) {
        DEBUG ("got autoswitch clear\n");
        skip = 0;
        qifline = xaccReadQIFDiscard (fd);
        continue;
     } else

     if (STRSTR (qifline, "Account")) {
        if (skip) {
           /* loop and read all of the account names and descriptions */
           /* no actual dollar data is expected to be read here ... */
           while (qifline) {
              qifline = xaccReadQIFAccList (fd, grp, 0);
              if (!qifline) break;
              if ('!' == qifline[0]) break;
           }
        } else {
           /* read account name, followed by dollar data ... */
           Account *preexisting;
           Account *acc   = xaccMallocAccount();
           DEBUG ("got account\n");
           qifline = xaccReadQIFAccount (fd, acc);
           if (!qifline) {  /* free up malloced data if the read bombed. */
              xaccFreeAccount(acc); 
              continue;
           }
           if (-1 == acc->type) {  /* free up malloced data if unknown account type */
              xaccFreeAccount(acc); 
              continue;
           }

           /* check to see if we already know this account;
            * if we do, use it, otherwise create it */
           preexisting = xaccGetAccountFromName (grp, acc->accountName);
           if (preexisting) {
              xaccFreeAccount (acc);
              acc = preexisting;
           } else {
              insertAccount( grp, acc );
           }
   
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
