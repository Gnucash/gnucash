/********************************************************************\
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
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @internal
	@file import-match-map.c
    @brief Generic import mapper service, maps strings->accounts
    *
    An import mapper service that stores Account Maps for the
    generic importer.  This allows importers to map various
    "strings" to Gnucash accounts in a generic manner.
    @author Copyright (C) 2002,2003 Derek Atkins <derek@ihtfp.com>
 */
#include "config.h"
#include <string.h>
#include <glib.h>
#include "import-match-map.h"
#include "gnc-ui-util.h"
#include "gnc-engine.h"

/********************************************************************\
 *   Constants   *
\********************************************************************/

static QofLogModule log_module = GNC_MOD_IMPORT;


struct _GncImportMatchMap {
  kvp_frame *	frame;
  Account *	acc;
  QofBook *	book;
};

#define IMAP_FRAME		"import-map"
#define IMAP_FRAME_BAYES	"import-map-bayes"

static GncImportMatchMap *
gnc_imap_create_from_frame (kvp_frame *frame, Account *acc, QofBook *book)
{
  GncImportMatchMap *imap;

  g_return_val_if_fail (frame != NULL, NULL);
  g_return_val_if_fail ((acc && !book) || (!acc && book), NULL);

  imap = g_new0(GncImportMatchMap, 1);
  imap->frame = frame;

  /* Cache the book for easy lookups; store the account/book for
   * marking dirtiness
   */
  if (acc)
    book = gnc_account_get_book (acc);
  imap->acc = acc;
  imap->book = book;
  
  return imap;
}

/** Obtain an ImportMatchMap object from an Account or a Book */
GncImportMatchMap * gnc_imap_create_from_account (Account *acc)
{
  kvp_frame * frame;

  if (!acc) return NULL;
  frame = xaccAccountGetSlots (acc);
  g_return_val_if_fail (frame != NULL, NULL);

  return gnc_imap_create_from_frame (frame, acc, NULL);
}

GncImportMatchMap * gnc_imap_create_from_book (QofBook *book)
{
  kvp_frame * frame;

  if (!book) return NULL;
  frame = gnc_book_get_slots (book);
  g_return_val_if_fail (frame != NULL, NULL);

  return gnc_imap_create_from_frame (frame, NULL, book);
}

/** Destroy an import map */
void gnc_imap_destroy (GncImportMatchMap *imap)
{
  if (!imap) return;
  g_free (imap);
}

/** Clear an import map -- this removes ALL entries in the map */
void gnc_imap_clear (GncImportMatchMap *imap)
{
  if (!imap) return;

  /* Clear the IMAP_FRAME kvp */
  kvp_frame_set_slot_path (imap->frame, NULL, IMAP_FRAME);

  /* Clear the bayes kvp, IMAP_FRAME_BAYES */
  kvp_frame_set_slot_path (imap->frame, NULL, IMAP_FRAME_BAYES);

  /* XXX: mark the account (or book) as dirty! */
}

/** Look up an Account in the map */
Account * gnc_imap_find_account (GncImportMatchMap *imap, const char *category,
				 const char *key)
{
  kvp_value *value;
  GUID * guid;

  if (!imap || !key) return NULL;
  if (!category) {
    category = key;
    key = NULL;
  }

  value = kvp_frame_get_slot_path (imap->frame, IMAP_FRAME, category, key, NULL);
  if (!value) return NULL;

  guid = kvp_value_get_guid (value);
  return xaccAccountLookup (guid, imap->book);
}

/** Store an Account in the map */
void gnc_imap_add_account (GncImportMatchMap *imap, const char *category,
			   const char *key, Account *acc)
{
  kvp_value *value;

  if (!imap || !key || !acc || (strlen (key) == 0)) return;
  if (!category) {
    category = key;
    key = NULL;
  }

  value = kvp_value_new_guid (xaccAccountGetGUID (acc));
  g_return_if_fail (value != NULL);

  kvp_frame_set_slot_path (imap->frame, value, IMAP_FRAME, category, key, NULL);
  kvp_value_delete (value);

  /* XXX Mark the account (or book) as dirty! */
}




/*--------------------------------------------------------------------------
 Below here is the bayes transaction to account matching system 
--------------------------------------------------------------------------*/


struct account_token_count
{
  char* account_name;
  gint64 token_count; /**< occurances of a given token for this account_name */
};

/** total_count and the token_count for a given account let us calculate the
 * probability of a given account with any single token
 */
struct token_accounts_info
{
  GList *accounts; /**< array of struct account_token_count */
  gint64 total_count;
};

/** gpointer is a pointer to a struct token_accounts_info
 * \note Can always assume that keys are unique, reduces code in this function
 */
static void buildTokenInfo(const char *key, kvp_value *value, gpointer data)
{
  struct token_accounts_info *tokenInfo = (struct token_accounts_info*)data;
  struct account_token_count* this_account;

  //  PINFO("buildTokenInfo: account '%s', token_count: '%ld'\n", (char*)key,
  //			(long)kvp_value_get_gint64(value));

  /* add the count to the total_count */
  tokenInfo->total_count += kvp_value_get_gint64(value);

  /* allocate a new structure for this account and it's token count */
  this_account = (struct account_token_count*)
    g_new0(struct account_token_count, 1);

  /* fill in the account name and number of tokens found for this account name */
  this_account->account_name = (char*)key;
  this_account->token_count = kvp_value_get_gint64(value);

 /* append onto the glist a pointer to the new account_token_count structure */
  tokenInfo->accounts = g_list_prepend(tokenInfo->accounts, this_account);
}

/** intermediate values used to calculate the bayes probability of a given account
  where p(AB) = (a*b)/[a*b + (1-a)(1-b)], product is (a*b),
  product_difference is (1-a) * (1-b)
 */
struct account_probability
{
  double product; /* product of probabilities */
  double product_difference; /* product of (1-probabilities) */
};

/** convert a hash table of account names and (struct account_probability*)
  into a hash table of 100000x the percentage match value, ie. 10% would be
  0.10 * 100000 = 10000
 */
#define PROBABILITY_FACTOR 100000
static void buildProbabilities(gpointer key, gpointer value, gpointer data)
{
    GHashTable *final_probabilities = (GHashTable*)data;
    struct account_probability *account_p = (struct account_probability*)value;

    /* P(AB) = A*B / [A*B + (1-A)*(1-B)]
     * NOTE: so we only keep track of a running product(A*B*C...)
     * and product difference ((1-A)(1-B)...) 
     */
    gint32 probability =
      (account_p->product /
       (account_p->product + account_p->product_difference))
      * PROBABILITY_FACTOR;

    PINFO("P('%s') = '%d'\n", (char*)key, probability);

    g_hash_table_insert(final_probabilities, key, GINT_TO_POINTER(probability));
}

/** Frees an array of the same time that buildProperties built */
static void freeProbabilities(gpointer key, gpointer value, gpointer data)
{
 /* free up the struct account_probability that was allocated
  * in gnc_imap_find_account_bayes()
  */
  g_free(value);
}

/** holds an account name and its corresponding integer probability
  the integer probability is some factor of 10
 */
struct account_info
{
  char* account_name;
  gint32 probability;
};

/** Find the highest probability and the corresponding account name
    store in data, a (struct account_info*)
    NOTE: this is a g_hash_table_foreach() function for a hash table of entries
    key is a  pointer to the account name, value is a gint32, 100000x
    the probability for this account
*/
static void highestProbability(gpointer key, gpointer value, gpointer data)
{
  struct account_info *account_i = (struct account_info*)data;

  /* if the current probability is greater than the stored, store the current */
  if(GPOINTER_TO_INT(value) > account_i->probability)
    {
      /* Save the new highest probability and the assoaciated account name */
      account_i->probability = GPOINTER_TO_INT(value);
      account_i->account_name = key;
    }
}


#define threshold (.90 * PROBABILITY_FACTOR) /* 90% */

/** Look up an Account in the map */
Account* gnc_imap_find_account_bayes(GncImportMatchMap *imap, GList *tokens)
{
  struct token_accounts_info tokenInfo; /**< holds the accounts and total 
					 * token count for a single token */
  GList *current_token;		        /**< pointer to the current token from the
				         * input GList *tokens */
  GList *current_account_token;		/**< pointer to the struct
					 * account_token_count */
  struct account_token_count *account_c; /**< an account name and the number
					  * of times a token has appeared
					  * for the account */
  struct account_probability *account_p; /**< intermediate storage of values
					  * to compute the bayes probability
					  * of an account */
  GHashTable *running_probabilities = g_hash_table_new(g_str_hash, g_str_equal);
  GHashTable *final_probabilities = g_hash_table_new(g_str_hash, g_str_equal);
  struct account_info account_i;
  kvp_value* value;
  kvp_frame* token_frame;

  ENTER(" ");

  /* check to see if the imap is NULL */
  if(!imap)
  {
    PINFO("imap is null, returning null");
    LEAVE(" ");
    return NULL;
  }

  /* find the probability for each account that contains any of the tokens
   * in the input tokens list
   */
  for(current_token = tokens; current_token; current_token = current_token->next)
    {
      /* zero out the token_accounts_info structure */
      memset(&tokenInfo, 0, sizeof(struct token_accounts_info));

      PINFO("token: '%s'", (char*)current_token->data);
      
      /* find the slot for the given token off of the source account
       * for these tokens, search off of the IMAP_FRAME_BAYES path so
       * we aren't looking from the parent of the entire kvp tree
       */
      value = kvp_frame_get_slot_path(imap->frame, IMAP_FRAME_BAYES,
				      (char*)current_token->data, NULL); 

      /* if value is null we should skip over this token */
      if(!value)
      	continue;

      /* convert the slot(value) into a the frame that contains the
       * list of accounts
       */
      token_frame = kvp_value_get_frame(value);

      /* token_frame should NEVER be null */
      if(!token_frame)
      {
	PERR("token '%s' has no accounts", (char*)current_token->data);
        continue; /* skip over this token */
      }

      /* process the accounts for this token, adding the account if it
       * doesn't already exist or adding to the existing accounts token
       * count if it does
       */
      kvp_frame_for_each_slot(token_frame, buildTokenInfo, &tokenInfo);

      /* for each account we have just found, see if the account already exists
       * in the list of account probabilities, if not add it
       */
      for(current_account_token = tokenInfo.accounts; current_account_token;
	  current_account_token = current_account_token->next)
      {
	/* get the account name and corresponding token count */
	account_c = (struct account_token_count*)current_account_token->data;

	PINFO("account_c->account_name('%s'), "
	      "account_c->token_count('%ld')/total_count('%ld')",
	      account_c->account_name, (long)account_c->token_count,
	      (long)tokenInfo.total_count); 

	account_p = g_hash_table_lookup(running_probabilities,
					account_c->account_name);

	/* if the account exists in the list then continue 
	 * the running probablities
	 */
	if(account_p)
	  {
	    account_p->product =
	      ((double)account_c->token_count / (double)tokenInfo.total_count)
	      * account_p->product;
	    account_p->product_difference =
	      ((double)1 - ((double)account_c->token_count /
			    (double)tokenInfo.total_count))
	      * account_p->product_difference;
	    PINFO("product == %f, product_difference == %f",
		  account_p->product, account_p->product_difference);
	  }
	else
	  {
	    /* add a new entry */
	    PINFO("adding a new entry for this account");
	    account_p = (struct account_probability*)
	      g_new0(struct account_probability, 1);

	    /* set the product and product difference values */
	    account_p->product = ((double)account_c->token_count /
				  (double)tokenInfo.total_count);
	    account_p->product_difference =
	      (double)1 - ((double)account_c->token_count /
			   (double)tokenInfo.total_count);

	    PINFO("product == %f, product_difference == %f",
		  account_p->product, account_p->product_difference);
	    
	    /* add the account name and (struct account_probability*)
	     * to the hash table */
	    g_hash_table_insert(running_probabilities,
				account_c->account_name, account_p);
	  }
      } /* for all accounts in tokenInfo */

      /* free the data in tokenInfo */
      for(current_account_token = tokenInfo.accounts; current_account_token;
	  current_account_token = current_account_token->next)
      {
	/* free up each struct account_token_count we allocated */
	g_free((struct account_token_count*)current_account_token->data);
      }

      g_list_free(tokenInfo.accounts); /* free the accounts GList */
    }

  /* build a hash table of account names and their final probabilities
   * from each entry in the running_probabilties hash table
   */
  g_hash_table_foreach(running_probabilities, buildProbabilities,
		       final_probabilities);

  /* find the highest probabilty and the corresponding account */
  memset(&account_i, 0, sizeof(struct account_info));
  g_hash_table_foreach(final_probabilities, highestProbability, &account_i);

  /* free each element of the running_probabilities hash */
  g_hash_table_foreach(running_probabilities, freeProbabilities, NULL);

  /* free the hash tables */
  g_hash_table_destroy(running_probabilities);
  g_hash_table_destroy(final_probabilities);

  PINFO("highest P('%s') = '%d'",
        account_i.account_name ? account_i.account_name: "(null)",
        account_i.probability);

  /* has this probability met our threshold? */
  if(account_i.probability >= threshold)
    {
      PINFO("found match");
      LEAVE(" ");
      return gnc_account_lookup_by_full_name(gnc_book_get_root_account(imap->book),
                                             account_i.account_name);
    }

  PINFO("no match");
  LEAVE(" ");

  return NULL; /* we didn't meet our threshold, return NULL for an account */
}


/** Updates the imap for a given account using a list of tokens */
void gnc_imap_add_account_bayes(GncImportMatchMap *imap, GList *tokens, Account *acc)
{
  GList *current_token;
  kvp_value *value;
  gint64 token_count;
  char* account_fullname;
  kvp_value *new_value; /* the value that will be added back into the kvp tree */

  ENTER(" ");

  /* if imap is null return */
  if(!imap)
  {
    LEAVE(" ");
    return;
  }

  account_fullname = gnc_account_get_full_name(acc);

  PINFO("account name: '%s'\n", account_fullname);

  /* process each token in the list */
  for(current_token = g_list_first(tokens); current_token;
      current_token = current_token->next)
    {
      /* Jump to next iteration if the pointer is not valid or if the
	 string is empty. In HBCI import we almost always get an empty
	 string, which doesn't work in the kvp loopkup later. So we
	 skip this case here. */
      if (!current_token->data || (*((char*)current_token->data) == '\0'))
           continue;

      /* start off with no tokens for this account */
      token_count = 0;

      PINFO("adding token '%s'\n", (char*)current_token->data);

      /* is this token/account_name already in the kvp tree? */
      value = kvp_frame_get_slot_path(imap->frame, IMAP_FRAME_BAYES,
				      (char*)current_token->data, account_fullname,
				      NULL);

      /* if the token/account is already in the tree, read the current
       * value from the tree and use this for the basis of the value we
       * are putting back
       */
      if(value)
	{
	  PINFO("found existing value of '%ld'\n",
		(long)kvp_value_get_gint64(value));

	  /* convert this value back into an integer */
	  token_count+=kvp_value_get_gint64(value);
	}

      /* increment the token count */
      token_count++;

      /* create a new value */
      new_value = kvp_value_new_gint64(token_count);

      /* insert the value into the kvp tree at
       * /imap->frame/IMAP_FRAME/token_string/account_name_string
       */
      kvp_frame_set_slot_path(imap->frame, new_value, IMAP_FRAME_BAYES, 
			      (char*)current_token->data, account_fullname, NULL);

      /* kvp_frame_set_slot_path() copied the value so we
       * need to delete this one ;-) */
      kvp_value_delete(new_value);
    }

  /* free up the account fullname string */
  g_free(account_fullname);
 
  LEAVE(" ");
}

/** @} */
