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

/* hack alert -- stocks probably not handled correctly
 * also, check out a stock split tooo
 */

#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "config.h"

#include "Account.h"
#include "Group.h"
#include "FileIO.h"
#include "Transaction.h"
#include "util.h"

#define PERMS   0666
#define WFLAGS  (O_WRONLY | O_CREAT | O_TRUNC)
#define RFLAGS  O_RDONLY

/* Cleared values found in QIF files */
#define QRECCLEAR '*'     /* Cleared, per Quicken */
#define QRECREC   'x'     /* Reconciled, per Quicken */
#define QRECRECM  'X'     /* Reconciled, per MS Money */
#define QRECBUDG  '?'     /* Budgeted amount, per CBB */
#define QRECBUDP  '!'     /* OLD Budgeted amount per CBB */

/** GLOBALS *********************************************************/

/* XXX hack alert -- this default currency should be made configurable
 * somewhere, somehow, somewhen
 */
static char *gnc_qif_import_currency = "USD";

static int          error_code=0; /* error code, if error occurred */

/* This static indicates the debugging module that this .o belongs to.  */
static short        module = MOD_IO;
static int FindDateDelimiter (char *str);
static void TryToFixDate (struct tm *date);
static int FavorDateType (int value);

/*******************************************************/

int 
xaccGetQIFIOError (void)
{
   /* reset the error code */
   int  rc = error_code;
   error_code = 0;
   return rc;
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
     } else if (NSTRNCMP(qifline, "^\n")) {
        qifline = xaccReadQIFLine (fd);
        return qifline;
     } else if (NSTRNCMP(qifline, "^\r")) {
        qifline = xaccReadQIFLine (fd);
        return qifline;
     } else if ('!' == qifline [0]) return qifline;
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
        (str) = (char *)malloc (len+1);		\
        strncpy ((str), &qifline[1], len);	\
        (str)[len] = 0x0;			\
}

#define XACC_PREP_NULL_STRING(str) {		\
        if (!(str)) { (str) = strdup (""); }	\
}

char * xaccReadQIFCategory (int fd, Account * acc) 
{
   char * qifline;
   char * tmp;

   if (!acc) return NULL;

   xaccAccountBeginEdit (acc, 0);
   xaccAccountSetType (acc, -1);

   qifline = xaccReadQIFLine (fd);
   if (!qifline) return NULL;
   if ('!' == qifline [0]) return qifline;

   /* scan for account name, description, type */
   while (qifline) 
     {

       switch (qifline[0]) {
	 /* N == Name */
         case 'N':
	   XACC_PREP_STRING (tmp);
	   xaccAccountSetName (acc, tmp);
	   break;

	 /* D == Description */
         case 'D':
	   XACC_PREP_STRING (tmp);
	   xaccAccountSetDescription (acc, tmp);
	   break;
  
	 /* T == Taxable -- this income is taxable */
         case 'T':
	   break;

	 /* E == Expense Category */
         case 'E':
	   xaccAccountSetType (acc, EXPENSE);
	   break;

	 /* I == Income Category */
         case 'I':
	   xaccAccountSetType (acc, INCOME);
	   break;

	 /* R == Tax Rate Indicator; -- some number ... */
         case 'R':
	   break;

         /* B == Budget Amount -- not (yet ?) used in GnuCash */
         case 'B':
	   break;

         case '^':
           break;    /* see below */

         default:
           PWARN ("Unknown transaction component %s\n", qifline);
           break;
       }

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

   xaccAccountCommitEdit (acc);
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
   char * tmp;

   if (!acc) return NULL;

   xaccAccountBeginEdit (acc, 0);
   xaccAccountSetType (acc, -1);

   qifline = xaccReadQIFLine (fd);
   if (!qifline) return NULL;
   if ('!' == qifline [0]) return qifline;

   /* scan for account name, description, type */
   while (qifline) 
     {
       switch (qifline[0]) 
	 {
	   case 'N':
	     XACC_PREP_STRING (tmp);
	     xaccAccountSetName (acc, tmp);
	     break;
	   case 'D':
	     XACC_PREP_STRING (tmp);
	     xaccAccountSetDescription (acc, tmp);
	     break;
	   case 'T':
	     if (NSTRNCMP (&qifline[1], "Bank")) {
	       xaccAccountSetType (acc, BANK);
	     } else
	       if (NSTRNCMP (&qifline[1], "Cash")) {
		 xaccAccountSetType (acc, CASH);
	       } else
		 if (NSTRNCMP (&qifline[1], "CCard")) {
		   xaccAccountSetType (acc, CREDIT);
		 } else
		   if (NSTRNCMP (&qifline[1], "Invst")) {
		     xaccAccountSetType (acc, STOCK);
		   } else
		     if (NSTRNCMP (&qifline[1], "Oth A")) {
		       xaccAccountSetType (acc, ASSET);
		     } else 
		       if (NSTRNCMP (&qifline[1], "Oth L")) {
			 xaccAccountSetType (acc, LIABILITY);
		       } else 
			 {
			   printf ("QIF Parse: Unsupported account type %s \n", &qifline[1]);
			   xaccAccountSetType (acc, -1);    /* hack alert -- */
			 }
	   break;
	 }

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

   xaccAccountCommitEdit (acc);
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
      xaccAccountSetCurrency (acc, gnc_qif_import_currency);
      if (cat) { 
         qifline = xaccReadQIFCategory (fd, acc);
      } else {
         qifline = xaccReadQIFAccount (fd, acc);
      }
      if (qifline && ('!' == qifline [0])) break;

      /* free up malloced data if unknown account type */
      if (-1 == xaccAccountGetType (acc)) {  
         xaccFreeAccount(acc); 
         continue;
      }
      if (!qifline) {  /* free up malloced data if the read bombed. */
         xaccFreeAccount(acc); 
         continue;
      }

      /* check to see if this is a sub-account.
       * Sub-accounts will have a colon in the name */
      str = xaccAccountGetName (acc);
      tok = strchr (str, ':');
      if (tok) {
         Account *parent;

         /* find the parent account, and parent to it */
         *tok = 0x0;
         parent = xaccGetAccountFromName (grp, str);
         *tok = ':';

         if (parent) {

            /* trim off the parent account name ... */
            tok += sizeof(char);  /* get rid of the semi-colon */
            xaccAccountSetName (acc, tok);

            xaccInsertSubAccount( parent, acc );
         } else {
            /* we should never get here if the qif file is OK */
            xaccGroupInsertAccount( grp, acc );  
         }
      } else {
         xaccGroupInsertAccount( grp, acc );
      }

   } while (qifline);


   return qifline;
}

/********************************************************************\
 * xaccParseQIFDate                                                 * 
 *   parses date of the form MM/DD/YY                               *
 *                                                                  * 
 * Args:   str -- pointer to string rep of date                     * 
 * Return: time_t -- date in format used by UNIX time_t             * 
\********************************************************************/

time_t
xaccParseQIFDate (char * str) 
{
   char * tok;
   time_t secs;
   struct tm dat;
   int favechar = 0;   /* Which delimiter do we have? */

   if (!str) return 0;  /* If the string is null, we're done. */

   /* First, figure out the delimiter. */
   /* Choices: "." or "-" or "/" */
   favechar = FindDateDelimiter(str);
   if (!favechar) return 0;

   tok = strchr (str, favechar);
   if (!tok) return 0;
   *tok = 0x0;
   dat.tm_mon = atoi (str) - 1;

   str = tok+sizeof(char);
   tok = strchr (str, favechar);
   if (!tok) return 0;
   *tok = 0x0;
   dat.tm_mday = atoi (str);

   str = tok+sizeof(char);
   tok = strchr (str, '\r');
   if (!tok) {
      tok = strchr (str, '\n');
      if (!tok) return 0;
   }
   *tok = 0x0;
   dat.tm_year = atoi (str);

   TryToFixDate(&dat);

   /* a quickie Y2K fix: assume two digit dates with
    * a value less than 50 are in the 21st century. */
   if (50 > dat.tm_year) dat.tm_year += 100;

   dat.tm_sec = 0;
   dat.tm_min = 0;
   dat.tm_hour = 11;

   secs = mktime (&dat);

   return secs;
}


/********************************************************************\
 * FindDateDelimiter                                                *
 *  determines which character is the delimiter for a date          *
 *  typical would be "MM/DD/YY", "MM-DD-YY", "MM.DD.YY"             *
 *                                                                  * 
 * Args:   str -- pointer to string rep of date                     * 
 * Return: int containing '/', '.', '-', or NULL indicating failure * 
\********************************************************************/

static int FindDateDelimiter (char *str) {
  char *tok;
  if (!str) return 0;
  tok = strchr (str, '/');
  if (tok)
    return '/';
  tok = strchr (str, '.');
  if (tok)
    return '.';
  tok = strchr (str, '-');
  if (tok)
    return '-';
  return 0;
}

/********************************************************************\
 * TryToFixDate                                                     *
 *  Swaps around date components based on some heuristics           *
 *                                                                  * 
 * Args:   date -- pointer to time_t structure                      *
 * Return: void                                                     *
\********************************************************************/
static void TryToFixDate (struct tm *date) 
{
  int first, second, third;
  int st_first, st_second, st_third;
  int mon, mday, year;
  int results;
  int which[5] = {0,0,0,0,0};
  
  first = date->tm_mon;
  second = date->tm_mday;
  third = date->tm_year;
  
  /* See what sort of date is favored by each component */
  st_first = FavorDateType(first);
  st_second = FavorDateType(second);
  st_third = FavorDateType(third);
  
  /* Plunk the values down into which[] */
  which[st_first] = 1;
  which[st_second] = 2;
  which[st_third] = 3;
  
  switch (which[4]) {   /* Year */
  case 1:
    year = first;
    break;
  case 2:
    year = second;
    break;
  case 3:
    year = third;
    break;
  default:
    return;   /* No date component looks like a year --> ABORT */
  }
  switch (which[2]) {   /* month */
  case 1:
    mon = first;
    break;
  case 2:
    mon = second;
    break;
  case 3:
    mon = third;
    break;
  default:
    return;   /* No date component looks like a month --> ABORT */
  }
  switch (which[1]) {   /* mday */
  case 1:
    mday = first;
    break;
  case 2:
    mday = second;
    break;
  case 3:
    mday = third;
    break;
  default:
    return;   /* No date component looks like a day of the month - ABORT */
  }
  
  date->tm_mon = mon;
  date->tm_mday = mday;
  date->tm_year = year;
  
  return;    
}

static int FavorDateType (int value) 
{
  int favoring;
  favoring = 2;  /* Month */
  if (value > 30)
    favoring = 4;  /* Year */
  if (value < 0) 
    favoring = 4;  /* Year */
  if (value < 31)
    if (value > 11)
      favoring = 1;  /* Day of month */
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
   int i, nacc;

   /* search for colons in name -- this indicates a sub-account */
   sub_ptr = strchr (qifline, ':');
   if (sub_ptr) {
      *sub_ptr = 0;
   }

   /* see if the account exists; but search only one level down,
    * not the full account tree */
   xfer_acc = NULL;
   nacc = xaccGroupGetNumAccounts (rootgrp);
   for (i=0; i<nacc; i++) {
      Account *acc = xaccGroupGetAccount (rootgrp, i);
      char * acc_name = xaccAccountGetName (acc);
      if (!strcmp(acc_name, qifline)) {
         xfer_acc = acc;
         break;
      }
   }

   /* if not, create it */
   if (!xfer_acc) {
      xfer_acc = xaccMallocAccount ();
      xaccAccountSetName (xfer_acc, qifline);
      xaccAccountSetCurrency (xfer_acc, gnc_qif_import_currency);

      if (0 > acc_type) acc_type = GuessAccountType (qifline);
      xaccAccountSetType (xfer_acc, acc_type);
      xaccGroupInsertAccount (rootgrp, xfer_acc);
   }

   /* if this account name had sub-accounts, get those */
   if (sub_ptr) {
      sub_ptr ++;
      rootgrp = xaccAccountGetChildren (xfer_acc);
      if (!rootgrp) {
         /* inserting a null child has effect of creating empty container */
         xaccInsertSubAccount (xfer_acc, NULL);
         rootgrp = xaccAccountGetChildren (xfer_acc);
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
      xaccAccountSetName (xfer_acc, qifline);
      xaccAccountSetCurrency (xfer_acc, gnc_qif_import_currency);

      xaccAccountSetType (xfer_acc, STOCK);
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
 * Args:   guess_name -- true if we should try and guess the name   *
 *                       based on an opening balance entry          * 
 * Args:   first_trans -- true if this is the first transaction to  *
 *                        be processed in this account              *
 * Return: first new line after end of transaction                  * 
\********************************************************************/

#define XACC_ACTION(q,x)				\
   if (!strncmp (&qifline[1], q, strlen(q))) {		\
      xaccSplitSetAction (source_split, (x));		\
   } else


char * 
xaccReadQIFTransaction (int fd, Account *acc, int guess_name,
			int first_trans)
{
   Transaction *trans;
   Split *source_split;
   Split *split = NULL;
   char * qifline;
   char * tmp;
   int opening_balance = 0;
   int isneg = 0;
   int got_share_quantity = 0;
   int share_xfer = 0;
   int is_security = 0;
   Account *sub_acc = 0x0;
   Account *xfer_acc = 0x0;
   double adjust = 0.0;
   char secondchar;

   if (!acc) return NULL;

   qifline = xaccReadQIFLine (fd);
   if (!qifline) return NULL;

   if ('!' == qifline [0]) return qifline;

   trans = xaccMallocTransaction ();
   xaccTransBeginEdit (trans, 1);
   source_split = xaccTransGetSplit (trans, 0);

   /* scan for transaction date, description, type */
   while (qifline) {
     switch (qifline[0]) 
       {
       case 'C':   /* Cleared flag */
	 secondchar = qifline[1];
	 switch(secondchar) 
	   {
	   case QRECCLEAR:
	     xaccSplitSetReconcile(source_split, CREC);
	     break;
	   case QRECREC:
	   case QRECRECM:
	     xaccSplitSetReconcile(source_split, YREC);
	     break;
	   case QRECBUDP:
	   case QRECBUDG:
	     xaccSplitSetReconcile(source_split, NREC);
	     break;
	   default:
	     xaccSplitSetReconcile(source_split, NREC);
	   }
	 break;
       case 'D':   /* D == date */
	 {
	   time_t secs;
	   secs = xaccParseQIFDate (&qifline[1]);
	   xaccTransSetDateSecs (trans, secs);
	   xaccTransSetDateEnteredSecs (trans, secs);
	 }
	 break;
       case 'E':    /* E == memo for split */
        if (split) {
	  XACC_PREP_STRING (tmp);
	  xaccSplitSetMemo (split, tmp);
        }
	break;
       case 'I':      /* I == share price */
	 {
	   double amt = xaccParseUSAmount (&qifline[1]); 
	   xaccSplitSetSharePrice (source_split, amt);
	 }
	 break;
       case 'L': /* L == name of acount from which transfer occured */
	 /* MSMoney uses a cute trick to overcome the lack of an account name
	  * in the QIF format.  Basically, if the very very first transaction
	  * has a payee field of "Opening Balance", then the L field is the name
	  * of this account, and not the transfer account.  But this only works
	  * for the very, very first transaction. This also seems to be the case
	  * for Quicken 5.0 (and others?).
	  */
	 if (opening_balance) {
	   if (guess_name) {
	     /* remove square brackets from name, remove carriage return ... */
	     qifline = &qifline[1];
	     if ('[' == qifline[0]) {
	       qifline = &qifline[1];
	       tmp = strchr (qifline, ']');
	       if (tmp) *tmp = 0x0;
	     }
	     tmp = strchr (qifline, '\r');
	     if (tmp) *tmp = 0x0;
	     tmp = strchr (qifline, '\n');
	     if (tmp) *tmp = 0x0;
	     xaccAccountSetName (acc, qifline);
	   }
	 } else {
	   /* locate the transfer account */
	   xfer_acc = xaccGetXferQIFAccount (acc, qifline);
	 }
	 break;
       case 'M':
	 /* M == memo field */
	 XACC_PREP_STRING (tmp);
	 xaccSplitSetMemo (source_split, tmp);
	 break;
       case 'N': 
	 /* N == check numbers for Banks, but Action for portfolios */
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
	     XACC_PREP_STRING (tmp);
	     xaccTransSetNum (trans, tmp);
	   }
	 break;
       case 'O':
	 /* O == adjustments */
	 /* hack alert -- sometimes adjustments are quite large.
	  * I have no clue why, and what to do about it.  For what 
	  * it's worth, I can prove that Quicken version 3.0 makes 
	  * math errors ... */
	 {
	   double pute;
	   adjust = xaccParseUSAmount (&qifline[1]);
	   pute = xaccSplitGetValue (source_split);
	   if (isneg) pute = -pute;

	   printf ("QIF Warning: Adjustment of %.2f to amount %.2f not handled \n", adjust, pute);
	 }
	 break;
       case 'P':
	 XACC_PREP_STRING (tmp);
	 xaccTransSetDescription (trans, tmp);
	   
	 /* MSMoney uses a cute trick to overcome the lack of an account name
	  * in the QIF format.  Basically, if the very very first transaction
	  * has a payee field of "Opening Balance", then the L field is the name
	  * of this account, and not the transfer account.  But this only works
	  * for the very, very first transaction. This also seems to be the case
	  * for Quicken 5.0 (and others?).
	  */
	 if (first_trans)
	   if (NSTRNCMP (qifline, "POpening Balance"))
	     opening_balance = GNC_T;
	 break;

       case 'Q':
	 /* Q == number of shares */
	 {
	   double amt = xaccParseUSAmount (&qifline[1]);  
	   if (isneg) amt = -amt;
	   xaccSplitSetShareAmount (source_split, amt);
	   got_share_quantity = 1;
	 }
	 break;
       case 'S':
	 /* S == split, name of debited account */
	 split = xaccMallocSplit();
	 xaccTransAppendSplit (trans, split);
	 xfer_acc = xaccGetXferQIFAccount (acc, qifline);
	 xaccAccountInsertSplit (xfer_acc, split);
	 
         /* set xfer account to NULL, so that we don't
          * end up adding spurious splits */
	 xfer_acc = NULL;
	 break;
	 
       case 'T':
	 /* T == total */
	 /* ignore T for stock transactions, since T is a dollar amount */
	 if (0 == got_share_quantity) {
	   double amt = xaccParseUSAmount (&qifline[1]);  
	   if (isneg) amt = -amt;
	   xaccSplitSetValue (source_split, amt);
	 }
	 break;
       case 'Y':
	 /* Y == Name of Security */
	 XACC_PREP_STRING (tmp);
	 xaccTransSetDescription (trans, tmp);
	 
	 is_security = 1;
	 if (share_xfer) {
	   /* locate or create the sub-account account */
	   sub_acc = xaccGetSecurityQIFAccount (acc, qifline);
	 }
	 break;
       case '$':
	 /* $ == dollar amount */
	 /* for splits, $ records the part of the total for each split */
	 if (split) {
	   double amt = xaccParseUSAmount (&qifline[1]);  
	   amt = -amt;
	   xaccSplitSetValue (split, amt);
	 } else {
	   /* Currently, it appears that the $ amount is a redundant 
	    * number that we can safely ignore.  To get fancy,
	    * we use it to double-check the above work, since it 
	    * appears to always appear as the last entry in the
	    * transaction.  Round things out to pennies, to 
	    * handle round-off errors. 
	    */
	   double parse, pute;
	   int got, wanted;
	   parse = xaccParseUSAmount (&qifline[1]);
	   pute = xaccSplitGetValue (source_split);
	   if (isneg) pute = -pute;
	   
	   wanted = (int) (100.0 * parse + 0.5);
	   got = (int) (100.0 * (pute+adjust) + 0.5);
	     if (wanted != got) {
	       printf ("QIF Parse Error: wanted %f got %f \n", parse, pute);
	     }
	 }
       }
     
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
   if (!qifline || ('!' == qifline[0])) {
     xaccTransDestroy (trans);
     xaccTransCommitEdit (trans);
     return qifline;
   }

   /* fundamentally different handling for securities and non-securities */
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
	 xaccAccountInsertSplit (xfer_acc, split);
       } else {
	 xaccAccountInsertSplit (acc, split);
       }
       
       /* normally, the security account is pointed at by 
	* sub_acc; the security account is credited.
	* But, just in case its missing, avoid a core dump */
       if (sub_acc) {
	 /* xxx hack alert --- is this right ??? */
	 xaccAccountInsertSplit (sub_acc, source_split);
       }
     } else {
       
       /* else, we are here if its not a share transfer. 
	* It is probably dividend or other income */
       
       /* if a transfer account is specified, the transfer 
	* account gets the dividend credit; otherwise, the 
	* main account gets it */
       if (xfer_acc) {
	 xaccAccountInsertSplit (xfer_acc, source_split);
         } else {
	   xaccAccountInsertSplit (acc, source_split);
         }
     }
     
   } else {
      /* if we are here, its not a security, but an ordinary account */
      /* if a transfer account was specified,  it is the debited account */
      if (xfer_acc) {
         if (!split) {
            double amt = xaccSplitGetValue (source_split);
            split = xaccMallocSplit ();
            xaccTransAppendSplit (trans, split);
            xaccSplitSetValue (split, -amt);
         }
         xaccAccountInsertSplit (xfer_acc, split);
      }

      /* the transaction itself appears as a credit */
      xaccAccountInsertSplit (acc, source_split);
   }
   xaccTransCommitEdit (trans);

   return qifline;
}
  
/********************************************************************\
 * read a sequence of transactions, inserting them into 
 * the indicated account
\********************************************************************/

char * xaccReadQIFTransList (int fd, Account *acc, int guess_name)
{
   char * qifline;

   if (!acc) return 0x0;
   qifline = xaccReadQIFTransaction (fd, acc, guess_name, GNC_T);
   while (qifline) {
      if ('!' == qifline[0]) break;
      qifline = xaccReadQIFTransaction (fd, acc, guess_name, GNC_F);
   } 
   return qifline;
}

/********************************************************************\
 ********************** LOAD DATA ***********************************
\********************************************************************/

/********************************************************************\
 * xaccReadQIFAccountGroup                                          * 
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
     int typo = -1;
     char * name = NULL;

     if (STRSTR (qifline, "Type:")) {
        if (STRSTR (qifline, "Type:Bank")) {
           typo = BANK;
           name = "Quicken Bank Account";
        } else
        if (STRSTR (qifline, "Type:Cash")) {
           typo = CASH;
           name = "Quicken Cash Account";
        } else
        if (STRSTR (qifline, "Type:CCard")) {
           typo = CREDIT;
           name = "Quicken Credit Card";
        } else
        if (STRSTR (qifline, "Type:Invst")) {
           typo = STOCK;
           name = "Quicken Investment Account";
        } else
        if (STRSTR (qifline, "Type:Oth A")) {
           typo = ASSET;
           name = "Quicken Asset";
        } else
        if (STRSTR (qifline, "Type:Oth L")) {
           typo = LIABILITY;
           name = "Quicken Liability";
        }
     } 
        
     if (name) {
        Account * acc = xaccMallocAccount();
        xaccAccountSetType (acc, typo);
        xaccAccountSetName (acc, name);
        xaccAccountSetCurrency (acc, gnc_qif_import_currency);

        xaccGroupInsertAccount( grp, acc );
        qifline = xaccReadQIFTransList (fd, acc, GNC_T);
        typo = -1; name = NULL;
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

     if ((STRSTR (qifline, "Type:Memorized")) ||
         (STRSTR (qifline, "Type:Memorised")) ) {
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
           char * acc_name;
           Account *preexisting;
           Account *acc = xaccMallocAccount();

           DEBUG ("got account\n");
           xaccAccountSetCurrency (acc, gnc_qif_import_currency);
           qifline = xaccReadQIFAccount (fd, acc);
           if (!qifline) {  /* free up malloced data if the read bombed. */
              xaccFreeAccount(acc); 
              continue;
           }

           /* check to see if we already know this account;
            * if we do, use it, otherwise create it */
           acc_name = xaccAccountGetName (acc);
           preexisting = xaccGetAccountFromName (grp, acc_name);
           if (preexisting) 
	   {
              xaccFreeAccount (acc);
              acc = preexisting;
           } 
           else if (-1 == xaccAccountGetType (acc))
           {  /* free up malloced data if unknown account type */
              xaccFreeAccount(acc); 
              continue;
           }
           else
           {
              xaccGroupInsertAccount( grp, acc );
           }
   
           /* spin until start of transaction records */
           /* in theory, in a perfect file, the transaction data follows immediately */
           while (qifline) { 
              if ('!' == qifline[0]) break;
              qifline = xaccReadQIFDiscard (fd);
           }
   
           /* read transactions */
           /* note, we have a real account name, so no need to go guessing it. */
           if (qifline) qifline = xaccReadQIFTransList (fd, acc, GNC_F);
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
