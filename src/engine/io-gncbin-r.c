/********************************************************************\
 * io-gncbin.c -- read and write (old format) binary datafile       *
 *             (GnuCash/X-Accountant)                               *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2000 Linas Vepstas <linas@linas.org>          *
 * Copyright (C) 1999-2000 Rob Browning                             *
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
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************
 * NOTE: the readxxxx/writexxxx functions changed the current       *
 *       position in the file, and so the order which these         *
 *       functions are called in important                          *
 *                                                                  *
 * Version 1 is the original file format                            *
 * Version 2 of the file format supports reading and writing of     *
 *    double-entry transactions.                                    *
 * Version 3 of the file format supports actions (Buy, Sell, etc.)  *
 * Version 4 of the file format adds account groups                 *
 * Version 5 of the file format adds splits                         *
 * Version 6 of the file format removes the source split            *
 * Version 7 of the file format adds currency & security types      *
 * Version 8 of the file format adds misc fields                    *
 * Version 9 changes the time format to a 64-bit int                *
 * Version 10 adds auxilliary account info                          *
 *                                                                  *
 * the format of the data in the file:                              *
 *   file        ::== token Group                                   *
 *   Group       ::== numAccounts (Account)^numAccounts             *
 *   Account     ::== accID flags type accountName accountCode      *
 *                    description notes currency security           *
 *                    AccInfo                                       *
 *                    numTran (Transaction)^numTrans                * 
 *                    numGroups (Group)^numGroups                   *
 *   Transaction ::== num date_entered date_posted description      *
 *                    docref numSplits (Split)^numSplits            *
 *   Split       ::== memo action reconciled  date_recned           *
 *                    docref amount share_price account             *
 *   token       ::== int  [the version of file format == VERSION]  * 
 *   numTrans    ::== int                                           * 
 *   numAccounts ::== int                                           * 
 *   accID       ::== int                                           * 
 *   flags       ::== char                                          * 
 *   type        ::== char                                          * 
 *   accountName ::== String                                        *  
 *   accountCode ::== String                                        *  
 *   description ::== String                                        *  
 *   notes       ::== String                                        *  
 *   currency    ::== String                                        *  
 *   security    ::== String                                        *  
 *   AccInfo     ::== (variable, depends on account type, ... )     *
 *                                                                  *
 *   num         ::== String                                        * 
 *   date_entered::== Date                                          * 
 *   date_posted ::== Date                                          * 
 *   date_recned ::== Date                                          * 
 *   description ::== String                                        * 
 *   memo        ::== String                                        * 
 *   action      ::== String                                        * 
 *   docref      ::== String                                        * 
 *   reconciled  ::== char                                          * 
 *   amount      ::== double                                        * 
 *   share_price ::== double                                        * 
 *   account     ::== int                                           *
 *   String      ::== size (char)^size                              * 
 *   size        ::== int                                           * 
 *   Date        ::== seconds nanoseconds                           * 
 *   seconds     ::== signed 64 bit int                             * 
 *   nanoseconds ::== signed 32 bit int                             * 
\********************************************************************/

#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <glib.h>

#include "config.h"

#include "kvp_frame.h"
#include "Account.h"
#include "AccountP.h"
#include "Backend.h"
#include "date.h"
#include "DateUtils.h"
#include "io-gncbin.h"
#include "Group.h"
#include "GroupP.h"
#include "messages.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "GNCIdP.h"
#include "gnc-pricedb.h"

#include "gnc-book-p.h"

#define PERMS   0666
#define WFLAGS  (O_WRONLY | O_CREAT | O_TRUNC)
#define RFLAGS  O_RDONLY

#undef VERSION
#define VERSION 10


/* hack alert the current file format does not support most of the
 * new/improved account & transaction structures
 */

/* This static indicates the debugging module that this .o belongs to.  */
static short module = MOD_IO;

/** GLOBALS *********************************************************/

/* the default currency is used when importin old-style
 * file formats, or when importing files with no currency 
 * specified.  This should probably be something that
 * is configurable from some user config menu.
 */
#define DEFAULT_CURRENCY "USD"

static int          error_code=0; /* error code, if error occurred */

static AccountGroup *holder;      /* temporary holder for
                                   *  unclassified accounts */
static AccountGroup *maingrp;     /* temporary holder for file
                                   * being read */

/* Store mappings from the old/abolished id's to accounts since id's
   have been deleted from Account's.

   These shouldn't be global, but since the rest of this file is
   completely non-reentrant, and since this file is dead once the xml
   format is finished, I'm not going to get too torn up about it...

   FIXME: a bigger issue is whether or not we're being totally kosher
   in our handling of keys and values here.  We use int32s and
   pointers interchangably ATM, and that may be problematic on some
   architectures...

   Only used *during* file read process.
*/

static GHashTable *ids_to_finished_accounts;
static GHashTable *ids_to_unfinished_accounts;

/** PriceDB bits ******************************************************/
/* Commodity prices (stock quotes, etc.) used to be stored as zero
   quantity (damount) splits inside the relevant accounts.  Now we
   used the pricedb.  potential_quotes is where we put the info from
   all these "quote splits" while reading, until we have a chance to
   cram them into the pricedb.  We can't do it any sooner because
   until we finish reading the file, the Account*'s havent' been
   filled out and so their commodities and types may not have been
   initilized yet.

   This shouldn't be a static global but see comments above RE ids_to*
   hashes.  */

typedef struct {
  Split *split;
  gnc_numeric price;
} GNCPotentialQuote;

static GSList     *potential_quotes;

static void
mark_potential_quote(Split *s, double price, double quantity)
{
  static int count = 0;

  if((price != 0) && DEQ(quantity, 0)){
    GNCPotentialQuote *q = g_new0(GNCPotentialQuote, 1);
    q->split = s;
    q->price = double_to_gnc_numeric(price,
				     GNC_DENOM_AUTO,
				     GNC_DENOM_SIGFIGS(9) | GNC_RND_ROUND);
    potential_quotes = g_slist_prepend(potential_quotes, q);
  }
}

static gboolean
cvt_potential_prices_to_pricedb_and_cleanup(GNCPriceDB **prices)
{
  GSList *item = potential_quotes;

  *prices = gnc_pricedb_create();
  if (!*prices) return FALSE;

  while(item)
  {
    GNCPotentialQuote *q = (GNCPotentialQuote *) item->data;
    Account *split_acct = xaccSplitGetAccount(q->split);
    GNCAccountType acct_type = xaccAccountGetType(split_acct);

    /* at this point, we already know it's a split with a zero amount */
    if((acct_type == STOCK) ||
       (acct_type == MUTUAL) ||
       (acct_type == CURRENCY)) {
      /* this is a quote -- file it in the db and kill the split */
      Transaction *txn = xaccSplitGetParent(q->split);
      GNCPrice *price = gnc_price_create();
      Timespec time = xaccTransRetDateEnteredTS(txn);

      gnc_price_set_commodity(price, xaccAccountGetSecurity(split_acct));
      gnc_price_set_currency(price, xaccAccountGetCurrency(split_acct));
      gnc_price_set_time(price, &time);
      gnc_price_set_source(price, "old-file-import");
      gnc_price_set_type(price, "unknown");
      gnc_price_set_value(price, q->price);
      if(!gnc_pricedb_add_price(*prices, price)) {
        PERR("problem adding price to pricedb.\n");
      }
      gnc_price_unref(price);

      xaccTransBeginEdit(txn);
      xaccSplitDestroy(q->split);
      xaccTransCommitEdit(txn);
    }
    g_free(item->data);
    item->data = NULL;
    item = item->next;
  }
  g_slist_free(potential_quotes);
  potential_quotes = NULL;

  return TRUE;
}

/** PROTOTYPES ******************************************************/
static Account     *locateAccount (int acc_id); 

static AccountGroup *readGroup( int fd, Account *, int token );
static Account      *readAccount( int fd, AccountGroup *, int token );
static gboolean      readAccInfo( int fd, Account *, int token );
static Transaction  *readTransaction( int fd, Account *, int token );
static Split        *readSplit( int fd, int token );
static char         *readString( int fd, int token );
static time_t        readDMYDate( int fd, int token );
static int           readTSDate( int fd, Timespec *, int token );

/*******************************************************/
/* backwards compatibility definitions for numeric value 
 * of account type.  These numbers are used (are to be
 * used) nowhere else but here, precisely because they
 * are non-portable.  The values of these defines MUST
 * NOT BE CHANGED; ANY CHANGES WILL BREAK FILE COMPATIBILITY.
 * YOU HAVE BEEN WARNED!!!!
 */

#define FF_BANK 	0
#define FF_CASH 	1
#define FF_ASSET	2
#define FF_CREDIT 	3
#define FF_LIABILITY 	4
#define FF_STOCK	5
#define FF_MUTUAL	6
#define FF_INCOME	7
#define FF_EXPENSE	8
#define FF_EQUITY	9
#define FF_CHECKING	10
#define FF_SAVINGS	11
#define FF_MONEYMRKT	12
#define FF_CREDITLINE	13
#define FF_CURRENCY	14

/*******************************************************/

GNCBackendError
gnc_book_get_binfile_io_error(void)
{
   /* reset the error code */
   int rc = error_code;
   error_code = 0;
   return rc;
}

/*******************************************************/
/* some endian stuff */

/* flip endianness of int, short, etc */
static int
xaccFlipInt (int val) 
{
  guint32 flip;
  flip = (val & 0xff000000) >> 24;
  flip |= (val & 0xff0000) >> 8;
  flip |= (val & 0xff00) << 8;
  flip |= (val & 0xff) << 24;
  return (int) flip;
}

static double
xaccFlipDouble (double val) 
{
  union {
     guint32 i[2];
     double d;
  } u;
  guint32 w0, w1;
  u.d = val;
  w0 = xaccFlipInt (u.i[0]);
  w1 = xaccFlipInt (u.i[1]);

  u.i[0] = w1;
  u.i[1] = w0;
  return u.d;
}

static gint64
xaccFlipLongLong (gint64 val) 
{
  union {
     guint32 i[2];
     gint64 d;
  } u;
  guint32 w0, w1;
  u.d = val;
  w0 = xaccFlipInt (u.i[0]);
  w1 = xaccFlipInt (u.i[1]);

  u.i[0] = w1;
  u.i[1] = w0;
  return u.d;
}

/* if we are running on a little-endian system, we need to
 * do some endian flipping, because the xacc/gnucash native data
 * format is big-endian. In particular, Intel x86 is little-endian. */
#if WORDS_BIGENDIAN
  #define XACC_FLIP_DOUBLE(x)
  #define XACC_FLIP_LONG_LONG(x) 
  #define XACC_FLIP_INT(x) 
  #define XACC_FLIP_SHORT(x) 
#else
  #define XACC_FLIP_DOUBLE(x) { (x) = xaccFlipDouble (x); }
  #define XACC_FLIP_LONG_LONG(x) { (x) = xaccFlipLongLong (x); }
  #define XACC_FLIP_INT(x) { (x) = xaccFlipInt (x); }
  #define XACC_FLIP_SHORT(x) { (x) = xaccFlipShort (x); }
#endif /* WORDS_BIGENDIAN */



/********************************************************************
 * handle legacy currencies : after we load old files, we MUST run the
 * gnucash currency conversion functions to get rid of the
 * GNC_LEGACY_CURRENCIES namespace.
 ********************************************************************/

static gnc_commodity * 
gnc_commodity_import_legacy(const char * currency_name) {
  gnc_commodity * old = NULL;

  if(currency_name && (currency_name[0] != 0) ) {
    old = gnc_commodity_table_lookup(gnc_engine_commodities(),
                                     GNC_COMMODITY_NS_LEGACY,
                                     currency_name);
    
    if(!old) {
      old = gnc_commodity_new(currency_name, 
                              GNC_COMMODITY_NS_LEGACY, currency_name,
                              0, 100000);
      old = gnc_commodity_table_insert(gnc_engine_commodities(), old);
    }
    return old;
  }
  else {
    return NULL;
  }
}


/********************************************************************\
 ********************** LOAD DATA ***********************************
\********************************************************************/

/********************************************************************\
 *   reads in the data from file descriptor                         *
 *                                                                  * 
 * Args:   book -- the book in which to store the data              *
 *         fd -- the file descriptor to read the data from          * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/
static gboolean
gnc_load_financials_from_fd(GNCBook *book, int fd)
{
  int  err=0;
  int  token=0;
  int  num_unclaimed;
  AccountGroup *grp = 0x0;

  maingrp = 0x0;
  error_code = ERR_BACKEND_NO_ERR;

  /* check for valid file descriptor */
  if( 0 > fd ) 
    {
    error_code = ERR_FILEIO_FILE_NOT_FOUND;
    return FALSE;
    }

  /* Read in the file format token */
  err = read( fd, &token, sizeof(int) );
  if( sizeof(int) != err ) 
    {
    error_code = ERR_FILEIO_FILE_EMPTY;
    return FALSE;
    }
  XACC_FLIP_INT (token);
  PINFO ("reading file version %d", token);
  
  /* If this is an old file, ask the user if the file
   * should be updated */
  if( VERSION > token ) {
    error_code = ERR_FILEIO_FILE_TOO_OLD;
  }
  
  /* If this is a newer file than we know how to deal
   * with, warn the user */
  if( VERSION < token ) {
    error_code = ERR_FILEIO_FILE_TOO_NEW;
    return FALSE;
  }
  
  /* FIXME: is this OK (i.e. direct hashes for ints?) */
  ids_to_finished_accounts = g_hash_table_new(g_direct_hash, g_direct_equal);
  if(!ids_to_finished_accounts) {
    error_code = ERR_BACKEND_ALLOC;
    return FALSE;
  }

  ids_to_unfinished_accounts = g_hash_table_new(g_direct_hash, g_direct_equal);
  if(!ids_to_unfinished_accounts) {
    error_code = ERR_BACKEND_ALLOC;

    g_hash_table_destroy(ids_to_finished_accounts);
    ids_to_finished_accounts = NULL;

    return FALSE;
  }

  potential_quotes = NULL;

  /* disable logging during load; otherwise its just a mess */
  xaccLogDisable();
  holder = xaccMallocAccountGroup();
  grp = readGroup (fd, NULL, token);

  /* auto-number the accounts, if they are not already numbered */
  xaccGroupDepthAutoCode (grp);

  /* the number of unclaimed accounts should be zero if the 
   * read succeeded.  But just in case of a very unlikely 
   * error, try to continue anyway. */
  num_unclaimed = xaccGroupGetNumSubAccounts (holder);
  if (num_unclaimed) {
    Account *acc;
    error_code = ERR_FILEIO_FILE_BAD_READ;

    /* create a lost account, put the missing accounts there */
    acc = xaccMallocAccount();
    xaccAccountBeginEdit (acc);
    xaccAccountSetName (acc, _("Lost Accounts"));
    acc -> children = holder;
    xaccAccountCommitEdit (acc);
    xaccGroupInsertAccount (grp, acc);
  } else {
    xaccFreeAccountGroup (holder);
    holder = NULL;
  }

  maingrp = NULL;
  g_hash_table_destroy(ids_to_finished_accounts);
  ids_to_finished_accounts = NULL;
  g_hash_table_destroy(ids_to_unfinished_accounts);
  ids_to_unfinished_accounts = NULL;

  {
    GNCPriceDB *tmpdb;
    if(cvt_potential_prices_to_pricedb_and_cleanup(&tmpdb)) {
      GNCPriceDB *db = gnc_book_get_pricedb(book);
      if(db) gnc_pricedb_destroy(db);
      gnc_book_set_pricedb(book, tmpdb);
    } else {
      PWARN("pricedb import failed.");
      error_code = ERR_BACKEND_MISC;
      gnc_pricedb_destroy(tmpdb);
    }
  }

  /* set up various state that is not normally stored in the byte stream */
  xaccRecomputeGroupBalance (grp);

  xaccLogEnable();

  {
    AccountGroup *g = gnc_book_get_group(book);
    if (g) xaccFreeAccountGroup(g);
    gnc_book_set_group(book, grp);
  }

  /* mark the newly read book as saved, since the act of putting it
   * together will have caused it to be marked up as not-saved.  */
  gnc_book_mark_saved(book);

  return (error_code == ERR_BACKEND_NO_ERR);
}

/********************************************************************\
 * xaccReadAccountGroupFile                                         * 
 *   reads in the data from file datafile                           *
 *                                                                  * 
 * Args:   datafile - the file to load the data from                * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/
void
gnc_book_load_from_binfile(GNCBook *book)
{
  int  fd;

  const gchar *datafile = gnc_book_get_file_path(book);
  if(!datafile) {
    error_code = ERR_BACKEND_MISC;
    return;
  }

  maingrp = 0x0;
  error_code = ERR_BACKEND_NO_ERR;

  fd = open( datafile, RFLAGS, 0 );
  if( 0 > fd ) {
    error_code = ERR_FILEIO_FILE_NOT_FOUND;
    return;
  }

  if(!gnc_load_financials_from_fd(book, fd)) return;

  close(fd);
  return;
}

/********************************************************************\
 * readGroup                                                        * 
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
  
  ENTER (" ");

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
  
  DEBUG ("expecting %d accounts", numAcc);

  /* read in the accounts */
  for( i=0; i<numAcc; i++ )
    {
    Account * acc = readAccount( fd, grp, token );
    if( NULL == acc ) {
      PERR("Short group read: \n"
           "\texpected %d, got %d accounts\n",numAcc,i);
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
  gnc_commodity * currency;
  gnc_commodity * security;
  char * tmp;

  ENTER (" ");
  
  /* version 1 does not store the account number */
  if (1 < token) {
    err = read( fd, &accID, sizeof(int) );
    if( err != sizeof(int) ) { return NULL; }
    XACC_FLIP_INT (accID);
    acc = locateAccount (accID);
    /* locateAccountAlways should always accounts that are open for
       editing in this situation */
  } else {
    acc = xaccMallocAccount();
    xaccGroupInsertAccount (holder, acc);
    xaccAccountBeginEdit (acc);
  }
  
  {
    /* flags are now gone - if you need these, use kv pairs */
    char tmpflags;
    err = read( fd, &tmpflags, sizeof(char) );
    if( err != sizeof(char) ) { return NULL; }
  }
  
  /* if (9999>= token) */ {
    char ff_acctype;
    int acctype;
    err = read( fd, &(ff_acctype), sizeof(char) );
    if( err != sizeof(char) ) { return NULL; }
    switch (ff_acctype) {
      case FF_BANK: 		{ acctype = BANK; 		break; }
      case FF_CASH: 		{ acctype = CASH; 		break; }
      case FF_ASSET: 		{ acctype = ASSET; 		break; }
      case FF_CREDIT: 		{ acctype = CREDIT; 		break; }
      case FF_LIABILITY:	{ acctype = LIABILITY; 		break; }
      case FF_STOCK: 		{ acctype = STOCK; 		break; }
      case FF_MUTUAL: 		{ acctype = MUTUAL; 		break; }
      case FF_INCOME: 		{ acctype = INCOME; 		break; }
      case FF_EXPENSE: 		{ acctype = EXPENSE; 		break; }
      case FF_EQUITY: 		{ acctype = EQUITY; 		break; }
      case FF_CHECKING: 	{ acctype = CHECKING; 		break; }
      case FF_SAVINGS: 		{ acctype = SAVINGS; 		break; }
      case FF_MONEYMRKT: 	{ acctype = MONEYMRKT;	 	break; }
      case FF_CREDITLINE: 	{ acctype = CREDITLINE; 	break; }
      case FF_CURRENCY: 	{ acctype = CURRENCY; 		break; }
      default: return NULL;
    }
    xaccAccountSetType (acc, acctype);
  }
  
  tmp = readString( fd, token );
  if( NULL == tmp) return NULL;
  DEBUG ("reading acct %s", tmp);
  xaccAccountSetName (acc, tmp);
  free (tmp);

  if (8 <= token) {
     tmp = readString( fd, token );
     if( NULL == tmp) return NULL;
     xaccAccountSetCode (acc, tmp);
     free (tmp);
  }
  
  tmp = readString( fd, token );
  if( NULL == tmp ) return NULL;
  xaccAccountSetDescription (acc, tmp);
  free (tmp);
  
  tmp = readString( fd, token );
  if( NULL == tmp ) return NULL;
  if(strlen(tmp) > 0) {
    xaccAccountSetNotes (acc, tmp);
  }
  free (tmp);
  
  /* currency and security strings first introduced 
   * in version 7 of the file format */
  if (7 <= token) {
     tmp = readString( fd, token );
     if( NULL == tmp ) return NULL;
     
     PINFO ("currency is %s", tmp);
     currency = gnc_commodity_import_legacy(tmp);
     xaccAccountSetCurrency (acc, currency);
     
     if(tmp) free (tmp);

     tmp = readString( fd, token );

     if (!tmp || *tmp == '\0')
     {
        GNCAccountType account_type;

        account_type = xaccAccountGetType (acc);

        if (account_type == STOCK  ||
            account_type == MUTUAL ||
            account_type == CURRENCY)
        {
          if (tmp) free (tmp);

          tmp = strdup (xaccAccountGetName (acc));
          if (tmp == NULL) return NULL;
        }
     }

     PINFO ("security is %s", tmp);
     security = gnc_commodity_import_legacy(tmp);
     xaccAccountSetSecurity (acc, security);

     if(tmp) free (tmp);
  } 
  else {
    /* set the default currency when importing old files */
    currency = gnc_commodity_import_legacy(DEFAULT_CURRENCY);
    xaccAccountSetCurrency (acc, currency);
  }

  /* aux account info first appears in version ten files */
  if (10 <= token) {
    if(!readAccInfo(fd, acc, token)) {
      return(NULL);
    }
  }

  err = read( fd, &numTrans, sizeof(int) );
  if( err != sizeof(int) ) { return NULL; }
  XACC_FLIP_INT (numTrans);
  
  DEBUG ("expecting %d transactions", numTrans);
  /* read the transactions */
  for( i=0; i<numTrans; i++ ) {
    Transaction *trans;
    trans = readTransaction( fd, acc, token );
    if(trans == NULL ) {
      PERR ("Short Transaction Read: \n"
            "\texpected %d got %d transactions \n", numTrans, i);
      break;
    }
  }
  
  /* Not needed now.  Since we always look in ids_to_finished_accounts
   * first, it doesn't matter if we don't ever delete anything from
   * unfinished_accounts...
   *
   *  springAccount (acc->id); */

  xaccGroupInsertAccount (grp, acc);

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

  xaccAccountCommitEdit (acc);

  return acc;
}

/********************************************************************\
 * locateAccount
 *
 * With the double-entry system, the file may reference accounts that
 * have not yet been read or properly parented.  Thus, we need a way
 * of dealing with this, and this routine performs this
 * work. Basically, accounts are requested by their id.  If an account
 * with the indicated ID does not exist, it is created and placed in a
 * temporary hash.  Accounts in the temp hash can be located, (so that
 * transactions can be added to them) and sprung (so that they can be
 * properly parented into a group).
 *
 * Also, if locate account creates a new account, it'll be open for
 * editing.
 */

static Account *
locateAccount (int acc_id) {

   Account * acc;
   /* negative account ids denote no account */
   if (0 > acc_id) return NULL;   

   /* first, see if we've already created the account */
   acc = (Account *) g_hash_table_lookup(ids_to_finished_accounts,
                                         (gconstpointer) acc_id);
   if (acc) return acc;

   /* next, see if its an unclaimed account */
   acc = (Account *) g_hash_table_lookup(ids_to_unfinished_accounts,
                                         (gconstpointer) acc_id);
   if (acc) return acc;

   /* if neither, then it does not yet exist.  Create it.
    * Put it in the drunk tank. */
   acc = xaccMallocAccount ();
   xaccAccountBeginEdit(acc);
   g_hash_table_insert(ids_to_unfinished_accounts,
                       (gpointer) acc_id,
                       (gpointer) acc);
   return acc;
}

/********************************************************************\
 * readAccInfo                                                      * 
 *   reads in the auxilliary account info                           *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the accinfo structure                                    * 
\********************************************************************/

static gboolean
readAccInfo(int fd, Account *acc, int token) {

  /* The only data that was ever stored in the account info was a
     price src if it was an investment account.  The AccInfo
     abstraction has been abandoned, and all that stuff will soon be
     handled by other means, so we're just going to cram pricesrc into
     the account kv table for now... */

  GNCAccountType acc_type;
  
  if(!acc) return(FALSE);

  acc_type = xaccAccountGetType(acc);
  if ((acc_type == STOCK) || (acc_type == MUTUAL)) {
    const char *tmp = readString( fd, token );
    if(NULL == tmp) return(FALSE);
    if(strlen(tmp) > 0) xaccAccountSetPriceSrc(acc, tmp);
    free((char *) tmp);
  }
  return(TRUE);
}

static void
xaccTransSetMemo (Transaction *trans, const char *memo)
{
  Split *s;

  if (!trans || !memo) return;

  s = xaccTransGetSplit (trans, 0);
  xaccSplitSetMemo (s, memo);

  if (xaccTransCountSplits (trans) != 2)
    return;

  s = xaccTransGetSplit (trans, 1);
  xaccSplitSetMemo (s, memo);
}

static void
xaccTransSetAction (Transaction *trans, const char *action)
{
  Split *s;

  if (!trans || !action) return;

  s = xaccTransGetSplit (trans, 0);
  xaccSplitSetAction (s, action);

  if (xaccTransCountSplits (trans) != 2)
    return;

  s = xaccTransGetSplit (trans, 1);
  xaccSplitSetAction (s, action);
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
readTransaction( int fd, Account *acc, int revision)
  {
  int err=0;
  int acc_id;
  int i;
  int dummy_category;
  int numSplits;
  Transaction *trans = 0x0;
  char *tmp;
  char recn;
  double num_shares = 0.0;
  double share_price = 0.0;

  ENTER (" ");

  /* create a transaction structure */
  trans = xaccMallocTransaction();
  xaccTransBeginEdit (trans);  

  tmp = readString( fd, revision );
  if (NULL == tmp)
    {
    PERR ("Premature end of Transaction at num");
    xaccTransDestroy(trans);
    xaccTransCommitEdit (trans);
    return NULL;
    }
  xaccTransSetNum (trans, tmp);
  free (tmp);
  
  if (revision <= 7) {
     time_t secs;
     secs = readDMYDate( fd, revision );
     if( 0 == secs )
       {
       PERR ("Premature end of Transaction at date");
       xaccTransDestroy(trans);
       xaccTransCommitEdit (trans);
       return NULL;
       }
     xaccTransSetDateSecs (trans, secs);
     xaccTransSetDateEnteredSecs (trans, secs);
  } else  {
     Timespec ts;
     int rc;

     /* read posted date first ... */
     rc = readTSDate( fd, &ts, revision );
     if( -1 == rc )
       {
       PERR ("Premature end of Transaction at date");
       xaccTransDestroy(trans);
       xaccTransCommitEdit (trans);
       return NULL;
       }
     xaccTransSetDateTS (trans, &ts);

     /* then the entered date ... */
     rc = readTSDate( fd, &ts, revision );
     if( -1 == rc )
       {
       PERR ("Premature end of Transaction at date");
       xaccTransDestroy(trans);
       xaccTransCommitEdit (trans);
       return NULL;
       }
     xaccTransSetDateEnteredTS (trans, &ts);
  }
  
  tmp = readString( fd, revision );
  if( NULL == tmp )
    {
    PERR ("Premature end of Transaction at description");
    xaccTransDestroy(trans);
    xaccTransCommitEdit (trans);
    return NULL;
    }
  PINFO ("description=%s", tmp);
  xaccTransSetDescription (trans, tmp);
  free (tmp);
  
  /* docref first makes an appearance in version 8.  They're now
     deprecated, and we don't think anyone ever used them anyway, but
     to be safe, if we find one, we store it in the old-docref slot, a
     la old-price-source. */
  if (revision >= 8) {
     tmp = readString( fd, revision );
     if( NULL == tmp ) {
       PERR ("Premature end of Transaction at docref");
       xaccTransDestroy(trans);
       xaccTransCommitEdit (trans);
       return NULL;
     }
     if(strlen(tmp) > 0) {
       kvp_value *new_value = kvp_value_new_string(tmp);
       if(!new_value) {
         PERR ("Failed to allocate kvp_value for transaction docref.");
         free(tmp);
         return(NULL);
       }
       kvp_frame_set_slot_nc(xaccTransGetSlots(trans), "old-docref", new_value);
     }
     free (tmp);
  }

  /* At version 5, most of the transaction stuff was 
   * moved to splits. Thus, vast majority of stuff below 
   * is skipped 
   */
  if (revision <= 4) { 
    Split* s;

    tmp = readString( fd, revision );
    if( NULL == tmp )
      {
      PERR ("Premature end of Transaction at memo");
      xaccTransDestroy(trans);
      xaccTransCommitEdit (trans);
      return NULL;
      }

    if(strlen(tmp) > 0) {
      xaccTransSetMemo (trans, tmp);
    }
    free (tmp);
    
    /* action first introduced in version 3 of the file format */
    if (revision >= 3) 
       {
       tmp = readString( fd, revision );
       if( NULL == tmp )
         {
         PERR ("Premature end of Transaction at action");
         xaccTransDestroy (trans);
         xaccTransCommitEdit (trans);
         return NULL;
         }
       xaccTransSetAction (trans, tmp);
       free (tmp);
      }
    
    /* category is now obsolete */
    err = read( fd, &(dummy_category), sizeof(int) );
    if( sizeof(int) != err )
      {
      PERR ("Premature end of Transaction at category");
      xaccTransDestroy (trans);
      xaccTransCommitEdit (trans);
      return NULL;
      }
    
    err = read( fd, &recn, sizeof(char) );
    if( sizeof(char) != err )
      {
      PERR ("Premature end of Transaction at reconciled");
      xaccTransDestroy(trans);
      xaccTransCommitEdit (trans);
      return NULL;
      }

    /* The code below really wants to assume that there are a pair
     * of splits in every transaction, so make it so. 
     */
    s = xaccMallocSplit ();
    xaccTransAppendSplit (trans, s);
    s = xaccMallocSplit ();
    xaccTransAppendSplit (trans, s);
    
    s = xaccTransGetSplit (trans, 0);
    xaccSplitSetReconcile (s, recn);
    s = xaccTransGetSplit (trans, 1);
    xaccSplitSetReconcile (s, recn);

    if(revision <= 1) {
      /* Note: this is for version 0 of file format only.
       * What used to be reconciled, is now cleared... transactions
       * aren't reconciled until you get your bank statement, and
       * use the reconcile window to mark the transaction reconciled
       */
      if( YREC == recn ) {
        s = xaccTransGetSplit (trans, 0);
        xaccSplitSetReconcile (s, CREC);
        s = xaccTransGetSplit (trans, 1);
        xaccSplitSetReconcile (s, CREC);
      }
    }
  
    /* Version 1 files stored the amount as an integer,
     * with the amount recorded as pennies.
     * Version 2 and above store the share amounts and 
     * prices as doubles. */
    if (1 == revision) {
      int amount;
      
      err = read( fd, &amount, sizeof(int) );
      if( sizeof(int) != err )
        {
        PERR ("Premature end of Transaction at V1 amount");
        xaccTransDestroy(trans);
        xaccTransCommitEdit (trans);
        return NULL;
        }
      XACC_FLIP_INT (amount);
      num_shares = 0.01 * ((double) amount); /* file stores pennies */
      s = xaccTransGetSplit (trans, 0);
      DxaccSplitSetShareAmount (s, num_shares);

      /* Version 1 files did not do double-entry */
      s = xaccTransGetSplit (trans, 0);
      xaccAccountInsertSplit( acc, s );
    } else {
      Account *peer_acc;
      double damount;
  
      /* first, read number of shares ... */
      err = read( fd, &damount, sizeof(double) );
      if( sizeof(double) != err )
        {
        PERR ("Premature end of Transaction at amount");
        xaccTransDestroy(trans);
        xaccTransCommitEdit (trans);
        return NULL;
        }
      XACC_FLIP_DOUBLE (damount);
      num_shares  = damount;
  
      /* ... next read the share price ... */
      err = read( fd, &damount, sizeof(double) );
      if( err != sizeof(double) )
        {
        PERR ("Premature end of Transaction at share_price");
        xaccTransDestroy(trans);
        xaccTransCommitEdit (trans);
        return NULL;
        }
      XACC_FLIP_DOUBLE (damount);
      share_price = damount;
      s = xaccTransGetSplit (trans, 0);

      DxaccSplitSetSharePriceAndAmount (s, share_price, num_shares);

      /* Read the account numbers for double-entry */
      /* These are first used in Version 2 of the file format */
      
      /* first, read the credit account number */
      err = read( fd, &acc_id, sizeof(int) );
      if( err != sizeof(int) )
        {
        PERR ("Premature end of Transaction at credit");
        xaccTransDestroy (trans);
        xaccTransCommitEdit (trans);
        return NULL;
        }
      XACC_FLIP_INT (acc_id);
      DEBUG ("credit %d\n", acc_id);
      peer_acc = locateAccount (acc_id);
  
      /* insert the split part of the transaction into 
       * the credited account */
      s = xaccTransGetSplit (trans, 0);
      if (peer_acc) xaccAccountInsertSplit( peer_acc, s);
  
      mark_potential_quote(s, share_price, num_shares);

      /* next read the debit account number */
      err = read( fd, &acc_id, sizeof(int) );
      if( err != sizeof(int) )
        {
        PERR ("Premature end of Transaction at debit");
        xaccTransDestroy(trans);
        xaccTransCommitEdit (trans);
        return NULL;
        }
      XACC_FLIP_INT (acc_id);
      DEBUG ("debit %d\n", acc_id);
      peer_acc = locateAccount (acc_id);
      if (peer_acc) {
         Split *s0 = xaccTransGetSplit (trans, 0);
         Split *s1 = xaccTransGetSplit (trans, 1);
         
         /* duplicate many of the attributes in the credit split */
         DxaccSplitSetSharePriceAndAmount (s1, share_price, -num_shares);
         xaccSplitSetReconcile (s1, xaccSplitGetReconcile (s0));
         xaccSplitSetMemo (s1, xaccSplitGetMemo (s0));
         xaccSplitSetAction (s1, xaccSplitGetAction (s0));
         xaccAccountInsertSplit (peer_acc, s1);
         mark_potential_quote(s1, share_price, -num_shares);
      }
    }
  } else { /* else, read version 5 and above files */

    const char *notes = NULL;

    if (revision == 5) {
      /* Version 5 files included a split that immediately
       * followed the transaction, before the destination splits.
       * Later versions don't have this. */
      Split *split = readSplit (fd, revision);
      xaccTransAppendSplit(trans, split);
    }
    
    /* read number of splits */
    err = read( fd, &(numSplits), sizeof(int) );
    if( err != sizeof(int) ) {
      PERR ("Premature end of Transaction at num-splits");
      xaccTransDestroy(trans);
      xaccTransCommitEdit (trans);
      return NULL;
    }
    XACC_FLIP_INT (numSplits);
    for (i = 0; i < numSplits; i++) {
      Split *split = readSplit(fd, revision);
      xaccTransAppendSplit(trans, split);
      
      if(!notes) {
        notes = xaccSplitGetMemo (split);
        if(notes) xaccTransSetNotes (trans, notes);
      }
    }
  }

  xaccTransCommitEdit (trans);  

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
  double num_shares, share_price;

  ENTER (" ");

  /* create a split structure */
  split = xaccMallocSplit();

  tmp = readString( fd, token );
  if( NULL == tmp )
    {
    PERR ("Premature end of Split at memo");
    xaccSplitDestroy(split);
    return NULL;
    }
  PINFO ("memo=%s", tmp);
  xaccSplitSetMemo (split, tmp);
  free (tmp);
  
  tmp = readString( fd, token );
  if( tmp == NULL )
    {
    PERR ("Premature end of Split at action");
    xaccSplitDestroy (split);
    return NULL;
    }
  xaccSplitSetAction (split, tmp);
  free (tmp);
  
  err = read( fd, &recn, sizeof(char) );
  if( err != sizeof(char) )
    {
    PERR ("Premature end of Split at reconciled");
    xaccSplitDestroy (split);
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

  /* version 8 and newer files store date-reconciled */ 
  if (8 <= token)  {
     Timespec ts;
     int rc;

     rc = readTSDate( fd, &ts, token );
     if( -1 == rc )
       {
       PERR ("Premature end of Split at date");
       xaccSplitDestroy (split);
       return NULL;
       }
     xaccSplitSetDateReconciledTS (split, &ts);
  } else {
     time_t now;
     now = time (0);
     xaccSplitSetDateReconciledSecs (split, now);
  }

  /* docref first makes an appearance in version 8.  They're now
     deprecated, and we don't think anyone ever used them anyway, but
     to be safe, if we find one, we store it in the old-docref slot, a
     la old-price-source. */
  if (8 <= token) {
     tmp = readString( fd, token );
     if( NULL == tmp ) {
       PERR ("Premature end of Split at docref");
       xaccSplitDestroy (split);
       return NULL;
     }
     if(strlen(tmp) > 0) {
       kvp_value *new_value = kvp_value_new_string(tmp);
       if(!new_value) {
         PERR ("Failed to allocate kvp_value for split docref.");
         free(tmp);
         return(NULL);
       }
       kvp_frame_set_slot_nc(xaccSplitGetSlots(split), "old-docref", new_value);
     }
     free (tmp);
  }

  /* first, read number of shares ... */
  err = read( fd, &num_shares, sizeof(double) );
  if( sizeof(double) != err )
    {
    PERR ("Premature end of Split at amount");
    xaccSplitDestroy (split);
    return NULL;
    }
  XACC_FLIP_DOUBLE (num_shares);

  /* ... next read the share price ... */
  err = read( fd, &share_price, sizeof(double) );
  if( sizeof(double) != err )
  {
    PERR ("Premature end of Split at share_price");
    xaccSplitDestroy (split);
    return NULL;
  }
  XACC_FLIP_DOUBLE (share_price);

  DxaccSplitSetSharePriceAndAmount (split, share_price, num_shares);

  DEBUG ("num_shares %f", num_shares);

  /* Read the account number */

  err = read( fd, &acc_id, sizeof(int) );
  if( sizeof(int) != err )
  {
    PERR ("Premature end of Split at account");
    xaccSplitDestroy (split);
    return NULL;
  }
  XACC_FLIP_INT (acc_id);
  DEBUG ("account id %d", acc_id);
  peer_acc = locateAccount (acc_id);
  xaccAccountInsertSplit (peer_acc, split);

  mark_potential_quote(split, share_price, num_shares);
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
  if (!str) {
    PERR("malloc failed on size %d bytes at position %ld\n", size,
         (long int) lseek(fd, 0, SEEK_CUR));
    return NULL;
  }
  err = read( fd, str, size );
  if( err != size )
    {
    PERR("size = %d err = %d str = %s\n", size, err, str );
    free(str);
    return NULL;
    }
  
  return str;
  }

/********************************************************************\
 * readTSDate                                                       * 
 *   reads in a Date struct from the datafile                       *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the Date struct                                          * 
\********************************************************************/
static int
readTSDate( int fd, Timespec *ts, int token )
  {
  int  err=0;
  gint64 secs = 0;   /* 64-bit int */
  gint32 nsecs = 0;
  
  /* secs is a 32-bit in in version 8 & earlier files, 
   * and goes 64-bit in the later files */
  if (8 >= token) 
    {
    gint32 sicks;
    err = read( fd, &sicks, sizeof(gint32) );
    if( err != sizeof(gint32) )
      {
      return -1;
      }
    XACC_FLIP_INT (sicks);
    secs = sicks;
    } 
  else 
    {
    err = read( fd, &secs, sizeof(gint64) );
    if( err != sizeof(gint64) )
      {
      return -1;
      }
    XACC_FLIP_LONG_LONG (secs);
    }
  
  err = read( fd, &nsecs, sizeof(gint32) );
  if( err != sizeof(gint32) )
    {
    return -1;
    }
  XACC_FLIP_INT (nsecs);

  ts->tv_sec = secs;
  ts->tv_nsec = nsecs;
  
  return 2*err;
  }

/********************************************************************\
 * readDMYDate                                                      * 
 *   reads in a Date struct from the datafile                       *
 *                                                                  * 
 * Args:   fd    - the filedescriptor of the data file              * 
 *         token - the datafile version                             * 
 * Return: the Date struct                                          * 
\********************************************************************/
static time_t
readDMYDate( int fd, int token )
  {
  int  err=0;
  int day, month, year;
  time_t secs;
  
  err = read( fd, &year, sizeof(int) );
  if( err != sizeof(int) )
    {
    return 0;
    }
  XACC_FLIP_INT (year);
  
  err = read( fd, &month, sizeof(int) );
  if( err != sizeof(int) )
    {
    return 0;
    }
  XACC_FLIP_INT (month);
  
  err = read( fd, &day, sizeof(int) );
  if( err != sizeof(int) )
    {
    return 0;
    }
  XACC_FLIP_INT (day);
  
  secs = xaccDMYToSec (day, month, year);
  return secs;
  }

#if 0
/* This is legacy code.  It no longer works, but it might be useful to
   leave it here indefinitely as a reference for the old format... */

/********************************************************************\
 ********************** SAVE DATA ***********************************
\********************************************************************/

/********************************************************************\
 * xaccWriteAccountGroup                                            * 
 *   writes out a group of accounts to a file                       * 
 *                                                                  * 
 * Args:   fd -- file descriptor                                    *
 *         grp -- account group                                     *
 *                                                                  *
 * Return: -1 on failure                                            * 
\********************************************************************/
int 
xaccWriteGncBinAccountGroup(FILE *f, AccountGroup *grp )
  {
  int i,numAcc;
  int token = VERSION;    /* The file format version */
  int err = 0;
  int fd = fileno(f);

  ENTER ("\n");
  
  if( 0 > fd )
    {
    ERROR();
    return -1;
    }
  
  XACC_FLIP_INT (token);
  err = write( fd, &token, sizeof(int) );
  if( err != sizeof(int) )
    {
    ERROR();
    return -1;
    }

  if (NULL == grp) {
    numAcc = 0;
  } else {
    numAcc = grp->numAcc;
  }

  XACC_FLIP_INT (numAcc);
  err = write( fd, &numAcc, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  if (NULL == grp) {
    return 0;
  }

  /* zero is no longer valid since we use it to detect a failed
     lookup... */
  next_write_account_id = 1;

  accounts_to_ids = g_hash_table_new(g_direct_hash, g_direct_equal);
  if(!accounts_to_ids) {
    error_code = ERR_BACKEND_ALLOC;
    return(-1);
  }

  /* OK, now zero out the write flag on all of the
   * transactions.  The write_flag is used to determine
   * if a given transaction has already been written
   * out to the file.  This flag is necessary, since
   * double-entry transactions appear in two accounts,
   * while they should be written only once to the file.
   * The write_flag is used ONLY by the routines in this
   * module.
   */
  xaccGroupBeginStagedTransactionTraversals(grp);

  for( i=0; i<grp->numAcc; i++ ) {
    err = writeAccount( fd, xaccGroupGetAccount(grp,i) );
    if( -1 == err ) {
      g_hash_table_destroy(accounts_to_ids);
      return err;
    }
  }
  
  g_hash_table_destroy(accounts_to_ids);
  return err;
  }

/********************************************************************\
 * writeGroup                                                       * 
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

  ENTER ("\n");
  
  if (NULL == grp) return 0;

  numAcc = grp->numAcc;
  XACC_FLIP_INT (numAcc);
  err = write( fd, &numAcc, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  for( i=0; i<grp->numAcc; i++ )
    {
    err = writeAccount( fd, xaccGroupGetAccount(grp,i) );
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
increment_int(Transaction *t, void *data) 
  {
  int val = *((int *) data);
  *((int *) data) = val + 1;
  return 0;
  }

static int
_write_transaction_wrapper_(Transaction *t, void *data)
  {
  return (-1 == writeTransaction(*((int *) data), t));
  }

static gint32
get_or_assign_account_write_id(Account *a) {
  gpointer value = g_hash_table_lookup(accounts_to_ids, (gconstpointer) a);
  gint32 id;

  if(value) {
    id = (gint32) value;
  } else {
    id = next_write_account_id++;
    g_hash_table_insert(accounts_to_ids, (gpointer) a, (gpointer) id);
  }
  return(id);
}

static int
writeAccount( int fd, Account *acc )
  {
  int err=0;
  int numUnwrittenTrans, ntrans;
  int acc_id;
  int numChildren = 0;
  const gnc_commodity * comm; /* hack alert : just for testing! -- bg */
  const char * tmp;
  
  DEBUG ("writing acct %s \n", xaccAccountGetName (acc));

  acc_id = get_or_assign_account_write_id(acc);
  XACC_FLIP_INT (acc_id);
  err = write( fd, &acc_id, sizeof(int) );
  if( err != sizeof(int) )
    return -1;

  {
    /* flags are no longer used - superceeded by kv pairs */
    const char dummyflags = 42;
    err = write(fd, &dummyflags, sizeof(char));
    if(err != sizeof(char)) return -1;
  }

  {
    char ff_acctype;
    int acctype;
    acctype = xaccAccountGetType (acc);
    /* we need to keep the file-format numbers from ever changing,
     * even if the account-type numbering does change.  As a result,
     * we need to build a translation table from one numbering scheme 
     * to the other. 
     */
    switch (acctype) {
      case BANK: 	{ ff_acctype = FF_BANK; 	break; }
      case CASH: 	{ ff_acctype = FF_CASH; 	break; }
      case ASSET: 	{ ff_acctype = FF_ASSET; 	break; }
      case CREDIT: 	{ ff_acctype = FF_CREDIT; 	break; }
      case LIABILITY:	{ ff_acctype = FF_LIABILITY; 	break; }
      case STOCK: 	{ ff_acctype = FF_STOCK; 	break; }
      case MUTUAL: 	{ ff_acctype = FF_MUTUAL; 	break; }
      case INCOME: 	{ ff_acctype = FF_INCOME; 	break; }
      case EXPENSE: 	{ ff_acctype = FF_EXPENSE; 	break; }
      case EQUITY: 	{ ff_acctype = FF_EQUITY; 	break; }
      case CHECKING: 	{ ff_acctype = FF_CHECKING; 	break; }
      case SAVINGS: 	{ ff_acctype = FF_SAVINGS; 	break; }
      case MONEYMRKT:	{ ff_acctype = FF_MONEYMRKT; 	break; }
      case CREDITLINE: 	{ ff_acctype = FF_CREDITLINE; 	break; }
      case CURRENCY: 	{ ff_acctype = FF_CURRENCY; 	break; }
      default: return -1;
    }
    err = write( fd, &(ff_acctype), sizeof(char) );
    if( err != sizeof(char) )
      return -1;
  }

  tmp = xaccAccountGetName (acc);
  err = writeString( fd, tmp );
  if( -1 == err ) return err;
  
  tmp = xaccAccountGetCode (acc);
  err = writeString( fd, tmp );
  if( -1 == err ) return err;
  
  tmp = xaccAccountGetDescription (acc);
  err = writeString( fd, tmp );
  if( -1 == err ) return err;
  
  tmp = xaccAccountGetNotes (acc);
  err = writeString( fd, tmp );
  if( -1 == err ) return err;
  
  comm = xaccAccountGetCurrency (acc);
  err = writeString( fd, gnc_commodity_get_mnemonic(comm));
  if( -1 == err ) return err;
  
  comm = xaccAccountGetSecurity (acc);
  err = writeString( fd, gnc_commodity_get_mnemonic(comm));
  if( -1 == err ) return err;
  
  err = writeAccInfo (fd, acc);    
  if( -1 == err ) return err;

  /* figure out numTrans -- it will be less than the total
   * number of transactions in this account, because some 
   * of the double entry transactions will already have been 
   * written.
   * marker flag values are:
   * 0 == uncounted, unwritten
   * 1 == counted, unwritten
   * 2 == written
   */

  /* Use a marker of 1 for counting numUnwrittenTrans */
  numUnwrittenTrans = 0;
  xaccAccountStagedTransactionTraversal(acc, 1,
                                        increment_int, &numUnwrittenTrans);
  
  ntrans = numUnwrittenTrans;
  XACC_FLIP_INT (ntrans);
  err = write( fd, &ntrans, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  DEBUG ("will write %d trans\n", numUnwrittenTrans);

  if (0 != xaccAccountStagedTransactionTraversal(acc, 2,
                                            _write_transaction_wrapper_, &fd)) {
    return -1;
  }

  if (acc->children) {
    numChildren = 1;
  } else {
    numChildren = 0;
  }

  XACC_FLIP_INT (numChildren);
  err = write( fd, &numChildren, sizeof(int) );
  if( err != sizeof(int) ) return -1;

  if(acc->children) {
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
  Timespec ts;

  ENTER ("\n");
  
  err = writeString( fd, xaccTransGetNum (trans) );
  if( -1 == err ) return err;
  
  xaccTransGetDateTS (trans, &ts);
  err = writeTSDate( fd, &ts);
  if( -1 == err ) return err;
  
  xaccTransGetDateEnteredTS (trans, &ts);
  err = writeTSDate( fd, &ts);
  if( -1 == err ) return err;
  
  err = writeString( fd, xaccTransGetDescription (trans) );
  if( -1 == err ) return err;
  
  /* docrefs are no more... if any are left around, they'll get stored
     with the kvp tables.  We shouldn't be using this write code
     anymore, but just in case, I'll fix this up to DTRT... */
  {
    kvp_value *value = kvp_frame_get_slot(xaccTransGetSlots(trans),
                                          "old-docref");
    const char *docref = NULL;

    if(value) docref = kvp_value_get_string(value);
    if(!docref) docref = "";

    err = writeString(fd, docref);
    if( -1 == err ) return err;
  }

  /* count the number of splits */
  i = xaccTransCountSplits (trans);
  XACC_FLIP_INT (i);
  err = write( fd, &i, sizeof(int) );
  if( err != sizeof(int) ) return -1;
  
  /* now write the splits */
  i = 0;
  s = xaccTransGetSplit (trans, i);
  while (s) {
    err = writeSplit (fd, s);
    if( -1 == err ) return err;
    i++;
    s = xaccTransGetSplit (trans, i);
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
  int acc_id;
  Timespec ts;
  double damount;
  Account *xfer_acc = NULL;
  char recn;

  ENTER ("\n");
  
  err = writeString( fd, xaccSplitGetMemo (split) );
  if( -1 == err )
    return err;
  
  err = writeString( fd, xaccSplitGetAction (split) );
  if( -1 == err )
    return err;
  
  recn = xaccSplitGetReconcile (split);
  err = write( fd, &recn, sizeof(char) );
  if( err != sizeof(char) )
    return -1;

  xaccSplitGetDateReconciledTS (split, &ts);
  err = writeTSDate( fd, &ts);
  if( -1 == err ) return err;
  
  /* docrefs are no more... if any are left around, they'll get stored
     with the kvp tables.  We shouldn't be using this write code
     anymore, but just in case, I'll fix this up to DTRT... */
  {
    kvp_value *value = kvp_frame_get_slot(xaccSplitGetSlots(split),
                                          "old-docref");
    const char *docref = NULL;

    if(value) docref = kvp_value_get_string(value);
    if(!docref) docref = "";

    err = writeString(fd, docref);
    if( -1 == err ) return err;
  }

  damount = DxaccSplitGetShareAmount (split);
  DEBUG ("amount=%f \n", damount);
  XACC_FLIP_DOUBLE (damount);
  err = write( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    return -1;

  damount = DxaccSplitGetSharePrice (split);
  XACC_FLIP_DOUBLE (damount);
  err = write( fd, &damount, sizeof(double) );
  if( err != sizeof(double) )
    return -1;

  /* write the credited/debted account */
  xfer_acc = xaccSplitGetAccount(split);
  acc_id = -1;
  if (xfer_acc) acc_id = get_or_assign_account_write_id(xfer_acc);
  DEBUG ("credit %d \n", acc_id);
  XACC_FLIP_INT (acc_id);
  err = write( fd, &acc_id, sizeof(int) );
  if( err != sizeof(int) )
    return -1;

  return err;
  }

/********************************************************************\
 * writeAccInfo                                                     * 
 *   saves the data for auxilliary account info to the datafile     *
 *                                                                  * 
 * Args:   fd       - the filedescriptor of the data file           * 
 *         iacc     - the aux data to save                          * 
 * Return: -1 on failure                                            * 
\********************************************************************/
static int
writeAccInfo (int fd, Account *acc) {
  /* See comments in readAccInfo */
  GNCAccountType acc_type;
  int err = 0;

  ENTER ("\n");
  if(!acc) return(-1);

  acc_type = xaccAccountGetType(acc);
  if ((acc_type == STOCK) || (acc_type == MUTUAL)) {
    /* We have to write out a price src because the input code will be
       looking for it... */
    const char *price_src = xaccAccountGetPriceSrc(acc);

    if(!price_src) price_src = "";
    err = writeString(fd, price_src);
    if(-1 == err) return err;
  }
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
writeString( int fd, const char *str )
  {
  int err=0;
  int size;
  int tmp;

  if (NULL == str) str = "";   /* protect against null arguments */
  size = strlen (str) + 1;
  
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
 * writeTSDate                                                        * 
 *   saves a Date to the datafile                                   *
 *                                                                  * 
 * Args:   fd       - the filedescriptor of the data file           * 
 *         date     - the Date to save                              * 
 * Return: -1 on failure                                            * 
\********************************************************************/
static int
writeTSDate( int fd, Timespec *ts)
  {
  int err=0;
  int tmp;
  gint64 longtmp;

  /* write 64 bits to file format */
  longtmp = ts->tv_sec;
  XACC_FLIP_LONG_LONG (longtmp);
  err = write( fd, &longtmp, sizeof(gint64) );
  if( err != sizeof(gint64) )
    return -1;
  
  tmp = ts->tv_nsec;
  XACC_FLIP_INT (tmp);
  err = write( fd, &tmp, sizeof(int) );
  if( err != sizeof(int) )
    return -1;
  
  return err;
  }

#endif

/*********************** END OF FILE *********************************/
