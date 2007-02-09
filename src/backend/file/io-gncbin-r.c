/********************************************************************\
 * io-gncbin-r.c -- read (old X-Accountant format) binary datafile  *
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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************
 * @file io-gncbin-r.c
 * @brief read (old X-Accountant format) binary datafile 
 * @author Copyright (C) 1997 Robin D. Clark
 * @author Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>
 * @author Copyright (C) 1999-2000 Rob Browning
 *
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

#include <glib.h>
#include <glib/gi18n.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

#include "config.h"

#include "Account.h"
#include "AccountP.h"
#include "io-gncbin.h"
#include "Group.h"
#include "GroupP.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"

#define PERMS   0666
#define WFLAGS  (O_WRONLY | O_CREAT | O_TRUNC)
#define RFLAGS  O_RDONLY

#undef VERSION
#define VERSION 10


/* The binary file format does not support most of the
 * new/improved account & transaction structures
 */

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_IO;

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


#define EPS  (1.0e-6)
#define DEQEPS(x,y,eps) (((((x)+(eps))>(y)) ? 1 : 0) && ((((x)-(eps))<(y)) ? 1 : 0))
#define DEQ(x,y) DEQEPS(x,y,EPS)

static void
mark_potential_quote(Split *s, double price, double quantity)
{
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
cvt_potential_prices_to_pricedb_and_cleanup(GNCPriceDB **prices,
                                            QofBook *book)
{
  GSList *item = potential_quotes;

  *prices = gnc_book_get_pricedb(book);
  if (!*prices) return FALSE;

  while(item)
  {
    GNCPotentialQuote *q = (GNCPotentialQuote *) item->data;
    Account *split_acct = xaccSplitGetAccount(q->split);

    /* at this point, we already know it's a split with a zero amount */
    if (xaccAccountIsPriced(split_acct)) {
      /* this is a quote -- file it in the db and kill the split */
      Transaction *txn = xaccSplitGetParent(q->split);
      GNCPrice *price = gnc_price_create(book);
      Timespec time = xaccTransRetDatePostedTS(txn);

      gnc_price_begin_edit(price);
      gnc_price_set_commodity(price,
                              DxaccAccountGetSecurity(split_acct));
      gnc_price_set_currency(price,
                             xaccTransGetCurrency(txn));
      gnc_price_set_time(price, time);
      gnc_price_set_source(price, "old-file-import");
      gnc_price_set_type(price, "unknown");
      gnc_price_set_value(price, q->price);
      gnc_price_commit_edit(price);
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
static Account     *locateAccount (int acc_id, QofBook *book); 

static AccountGroup *readGroup( QofBook *, int fd, Account *, int token );
static Account      *readAccount( QofBook *book, int fd,
                                  AccountGroup *, int token );
static gboolean      readAccInfo( int fd, Account *, int token );
static Transaction  *readTransaction( QofBook *book,
                                      int fd, Account *, int token );
static Split        *readSplit( QofBook *book, int fd, int token );
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

QofBackendError
gnc_get_binfile_io_error(void)
{
   /* reset the error code */
   int rc = error_code;
   error_code = 0;
   return rc;
}

/*******************************************************/
/* some endian stuff */

/* if we are running on a little-endian system, we need to
 * do some endian flipping, because the xacc/gnucash native data
 * format is big-endian. In particular, Intel x86 is little-endian. */
#if WORDS_BIGENDIAN
  #define XACC_FLIP_DOUBLE(x)
  #define XACC_FLIP_LONG_LONG(x) 
  #define XACC_FLIP_INT(x) 
  #define XACC_FLIP_SHORT(x) 
#else

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
gnc_commodity_import_legacy(QofBook *book, const char * currency_name)
{
  gnc_commodity_table *table;
  gnc_commodity * old = NULL;

  table = gnc_book_get_commodity_table (book);

  g_return_val_if_fail (table != NULL, NULL);

  if(currency_name && (currency_name[0] != 0) ) {
    old = gnc_commodity_table_lookup(table,
                                     GNC_COMMODITY_NS_LEGACY,
                                     currency_name);
    
    if(!old) {
      old = gnc_commodity_new(book, currency_name,
                              GNC_COMMODITY_NS_LEGACY, currency_name,
                              0, 100000);
      old = gnc_commodity_table_insert(table, old);
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

gboolean gnc_is_bin_file (const gchar *name)
{
  int  fd, err, token;

  err = 0;
  token = 0;
  fd = open( name, RFLAGS, 0 );

  /* check for valid file descriptor */
  if( 0 > fd ) { return FALSE; }

  /* Read in the file format token */
  err = read( fd, &token, sizeof(int) );
  if( sizeof(int) != err ) { return FALSE; }
  XACC_FLIP_INT (token);
  close(fd);
  if(token == VERSION ) { return TRUE; }
  return FALSE;
}

/********************************************************************\
 *   reads in the data from file descriptor                         *
 *                                                                  * 
 * Args:   book -- the book in which to store the data              *
 *         fd -- the file descriptor to read the data from          * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/
static gboolean
gnc_load_financials_from_fd(QofBook *book, int fd)
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
    error_code = ERR_BACKEND_TOO_NEW;
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
  holder = xaccMallocAccountGroup(book);
  grp = readGroup (book, fd, NULL, token);

  /* the number of unclaimed accounts should be zero if the 
   * read succeeded.  But just in case of a very unlikely 
   * error, try to continue anyway. */
  num_unclaimed = xaccGroupGetNumSubAccounts (holder);
  if (num_unclaimed) {
    Account *acc;
    error_code = ERR_FILEIO_FILE_BAD_READ;

    /* create a lost account, put the missing accounts there */
    acc = xaccMallocAccount(book);
    xaccAccountBeginEdit (acc);
    /* Translators: Name of the account where all the missing accounts
       are put into. (FIXME: is this correct?) */
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
    if(!cvt_potential_prices_to_pricedb_and_cleanup(&tmpdb, book))
    {
      PWARN("pricedb import failed.");
      error_code = ERR_BACKEND_MISC;
      gnc_pricedb_destroy(tmpdb);
    }
  }

  xaccLogEnable();

  xaccSetAccountGroup(book, grp);

  /* mark the newly read book as saved, since the act of putting it
   * together will have caused it to be marked up as not-saved.  */
  qof_book_mark_saved(book);

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
qof_session_load_from_binfile(QofBook *book, const char * datafile)
{
  int  fd;

  if(!datafile) {
    error_code = ERR_BACKEND_MISC;
    return;
  }

  maingrp = NULL;
  error_code = ERR_BACKEND_NO_ERR;

  fd = open( datafile, RFLAGS, 0 );
  if( 0 > fd ) {
    error_code = ERR_FILEIO_FILE_NOT_FOUND;
    return;
  }

  xaccDisableDataScrubbing();
  gnc_load_financials_from_fd(book, fd);
  xaccEnableDataScrubbing();

  close(fd);
}

/********************************************************************\
 * readGroup                                                        * 
 *   reads in a group of accounts                                   *
 *                                                                  * 
 * Args:                                                            * 
 * Return: the struct with the program data in it                   * 
\********************************************************************/
static AccountGroup *
readGroup (QofBook *book, int fd, Account *aparent, int token)
  {
  int  numAcc;
  int  err=0;
  int  i;
  AccountGroup *grp = xaccMallocAccountGroup(book);
  
  ENTER (" ");

  if (NULL == aparent) {
    maingrp = grp;
  }

  /* read numAccs */
  err = read( fd, &numAcc, sizeof(int) );
  if ( sizeof(int) != err )
  {
     xaccFreeAccountGroup (grp);
     LEAVE("");
     return NULL;
  }
  XACC_FLIP_INT (numAcc);
  
  DEBUG ("expecting %d accounts", numAcc);

  /* read in the accounts */
  for( i=0; i<numAcc; i++ )
    {
    Account * acc = readAccount( book, fd, grp, token );
    if( NULL == acc ) {
      PERR("Short group read: expected %d, got %d accounts\n",numAcc,i);
      break;
      }
    }

  /* if reading an account subgroup, place the subgroup
   * into the parent account */
  grp->parent = aparent;
  if (aparent) {
    aparent->children = grp;
  }
  LEAVE("");
  return grp;
}

/********************************************************************\
 * readAccount                                                      * 
 *   reads in the data for an account from the datafile             *
 *                                                                  * 
 * Args:   book  - the top-level account object                     *
 *         fd    - the filedescriptor of the data file              * 
 *         acc   - the account structure to be filled in            *
 *         token - the datafile version                             * 
 * Return: error value, 0 if OK, else -1                            * 
\********************************************************************/
static Account *
readAccount( QofBook *book, int fd, AccountGroup *grp, int token )
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
    if( err != sizeof(int) ) { LEAVE(""); return NULL; }
    XACC_FLIP_INT (accID);
    acc = locateAccount (accID, book);
    /* locateAccountAlways should always accounts that are open for
       editing in this situation */
  } else {
    acc = xaccMallocAccount(book);
    xaccGroupInsertAccount (holder, acc);
    xaccAccountBeginEdit (acc);
  }
  
  {
    /* flags are now gone - if you need these, use kv pairs */
    char tmpflags;
    err = read( fd, &tmpflags, sizeof(char) );
    if( err != sizeof(char) ) { LEAVE(""); return NULL; }
  }
  
  /* if (9999>= token) */ {
    char ff_acctype;
    int acctype;
    err = read( fd, &(ff_acctype), sizeof(char) );
    if( err != sizeof(char) ) { LEAVE(""); return NULL; }
    switch (ff_acctype) {
      case FF_BANK: 		{ acctype = ACCT_TYPE_BANK; 		break; }
      case FF_CASH: 		{ acctype = ACCT_TYPE_CASH; 		break; }
      case FF_ASSET: 		{ acctype = ACCT_TYPE_ASSET; 		break; }
      case FF_CREDIT: 		{ acctype = ACCT_TYPE_CREDIT; 		break; }
      case FF_LIABILITY:	{ acctype = ACCT_TYPE_LIABILITY;	break; }
      case FF_STOCK: 		{ acctype = ACCT_TYPE_STOCK; 		break; }
      case FF_MUTUAL: 		{ acctype = ACCT_TYPE_MUTUAL; 		break; }
      case FF_INCOME: 		{ acctype = ACCT_TYPE_INCOME; 		break; }
      case FF_EXPENSE: 		{ acctype = ACCT_TYPE_EXPENSE; 		break; }
      case FF_EQUITY: 		{ acctype = ACCT_TYPE_EQUITY; 		break; }
      case FF_CHECKING: 	{ acctype = ACCT_TYPE_CHECKING; 	break; }
      case FF_SAVINGS: 		{ acctype = ACCT_TYPE_SAVINGS; 		break; }
      case FF_MONEYMRKT: 	{ acctype = ACCT_TYPE_MONEYMRKT; 	break; }
      case FF_CREDITLINE: 	{ acctype = ACCT_TYPE_CREDITLINE; 	break; }
      case FF_CURRENCY: 	{ acctype = ACCT_TYPE_CURRENCY;		break; }
      default: LEAVE(""); return NULL;
    }
    xaccAccountSetType (acc, acctype);
  }
  
  tmp = readString( fd, token );
  if (NULL == tmp) { LEAVE(""); return NULL; }
  DEBUG ("reading acct %s", tmp);
  xaccAccountSetName (acc, tmp);
  g_free (tmp);

  if (8 <= token) {
     tmp = readString( fd, token );
     if (NULL == tmp) { LEAVE(""); return NULL; }
     xaccAccountSetCode (acc, tmp);
     g_free (tmp);
  }
  
  tmp = readString( fd, token );
  if (NULL == tmp ) { LEAVE(""); return NULL; }
  xaccAccountSetDescription (acc, tmp);
  g_free (tmp);
  
  tmp = readString( fd, token );
  if (NULL == tmp ) { LEAVE(""); return NULL; }
  if(strlen(tmp) > 0) {
    xaccAccountSetNotes (acc, tmp);
  }
  g_free (tmp);
  
  /* currency and security strings first introduced 
   * in version 7 of the file format */
  if (7 <= token) {
     tmp = readString( fd, token );
     if( NULL == tmp ) { LEAVE(""); return NULL; }
     
     PINFO ("currency is %s", tmp);
     currency = gnc_commodity_import_legacy(book, tmp);
     DxaccAccountSetCurrency (acc, currency);
     
     if(tmp) g_free (tmp);

     tmp = readString( fd, token );

     if (!tmp || *tmp == '\0')
     {
         if (xaccAccountIsPriced(acc)) {
             if (tmp) g_free (tmp);

             tmp = strdup (xaccAccountGetName (acc));
             if (tmp == NULL) { LEAVE(""); return NULL; }
        }
     }

     PINFO ("security is %s", tmp);
     security = gnc_commodity_import_legacy(book, tmp);
     DxaccAccountSetSecurity (acc, security);

     if(tmp) g_free (tmp);
  } 
  else
  {
    /* set the default currency when importing old files */
    currency = gnc_commodity_import_legacy(book, DEFAULT_CURRENCY);
    DxaccAccountSetCurrency (acc, currency);
  }

  /* aux account info first appears in version ten files */
  if (10 <= token) {
    if(!readAccInfo(fd, acc, token)) {
      LEAVE("");
      return(NULL);
    }
  }

  err = read( fd, &numTrans, sizeof(int) );
  if( err != sizeof(int) ) { LEAVE(""); return NULL; }
  XACC_FLIP_INT (numTrans);
  
  DEBUG ("expecting %d transactions", numTrans);
  /* read the transactions */
  for( i=0; i<numTrans; i++ ) {
    Transaction *trans;
    trans = readTransaction(book, fd, acc, token );
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
       LEAVE("");
       return NULL; 
    }
    XACC_FLIP_INT (numGrps);
    if (numGrps) {
       readGroup (book, fd, acc, token);
    }
  }

  xaccAccountCommitEdit (acc);

  LEAVE("");
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
locateAccount (int acc_id, QofBook *book)
{
   Account * acc;
   /* negative account ids denote no account */
   if (0 > acc_id) return NULL;   

   /* first, see if we've already created the account */
   acc = (Account *) g_hash_table_lookup(ids_to_finished_accounts,
                                         GINT_TO_POINTER(acc_id));
   if (acc) return acc;

   /* next, see if its an unclaimed account */
   acc = (Account *) g_hash_table_lookup(ids_to_unfinished_accounts,
                                         GINT_TO_POINTER(acc_id));
   if (acc) return acc;

   /* if neither, then it does not yet exist.  Create it.
    * Put it in the drunk tank. */
   acc = xaccMallocAccount (book);
   xaccAccountBeginEdit(acc);
   g_hash_table_insert(ids_to_unfinished_accounts,
                       GINT_TO_POINTER(acc_id),
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
  if ((acc_type == ACCT_TYPE_STOCK) || (acc_type == ACCT_TYPE_MUTUAL)) {
    const char *tmp = readString( fd, token );
    if(NULL == tmp) return(FALSE);
    if(strlen(tmp) > 0) dxaccAccountSetPriceSrc(acc, tmp);
    g_free((char *) tmp);
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
readTransaction(QofBook *book, int fd, Account *acc, int revision)
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
  trans = xaccMallocTransaction(book);
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
  g_free (tmp);
  
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
     xaccTransSetDatePostedTS (trans, &ts);

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
  g_free (tmp);
  
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
         g_free(tmp);
         return(NULL);
       }
       kvp_frame_set_slot_nc(xaccTransGetSlots(trans), "old-docref", new_value);
     }
     g_free (tmp);
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
    g_free (tmp);
    
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
       g_free (tmp);
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
    s = xaccMallocSplit (book);
    xaccTransAppendSplit (trans, s);
    s = xaccMallocSplit (book);
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
      peer_acc = locateAccount (acc_id, book);
  
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
      peer_acc = locateAccount (acc_id, book);
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
      Split *split = readSplit (book, fd, revision);
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
      Split *split = readSplit(book, fd, revision);
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
readSplit ( QofBook *book, int fd, int token )
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
  split = xaccMallocSplit(book);

  tmp = readString( fd, token );
  if( NULL == tmp )
    {
    PERR ("Premature end of Split at memo");
    xaccSplitDestroy(split);
    return NULL;
    }
  PINFO ("memo=%s", tmp);
  xaccSplitSetMemo (split, tmp);
  g_free (tmp);
  
  tmp = readString( fd, token );
  if( tmp == NULL )
    {
    PERR ("Premature end of Split at action");
    xaccSplitDestroy (split);
    return NULL;
    }
  xaccSplitSetAction (split, tmp);
  g_free (tmp);
  
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
         g_free(tmp);
         return(NULL);
       }
       kvp_frame_set_slot_nc(xaccSplitGetSlots(split), "old-docref", new_value);
     }
     g_free (tmp);
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
  peer_acc = locateAccount (acc_id, book);
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
  
  str = (char *) g_malloc (size);
  if (!str) {
    PERR("malloc failed on size %d bytes at position %ld\n", size,
         (long int) lseek(fd, 0, SEEK_CUR));
    return NULL;
  }
  err = read( fd, str, size );
  if( err != size )
    {
    PERR("size = %d err = %d str = %s\n", size, err, str );
    g_free(str);
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


/*********************** END OF FILE *********************************/
