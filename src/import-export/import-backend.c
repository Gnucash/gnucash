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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @internal
    @file import-backend.c
    @brief import-backend.c: Generic importer backend implementation (duplicate matching algorithm, action handling,  etc.)
    @author Copyright (C) 2002 Benoit Grégoire
    @author Christian Stimming
*/
 
#define _GNU_SOURCE

#include "config.h"


#include <glib.h>

#include <stdlib.h> 
#include <math.h>
#include "import-backend.h"
#include "import-utilities.h"
#include "Account.h"
#include "dialog-utils.h"
#include "global-options.h"

#include "gnc-engine-util.h"

#include "gnc-ui-util.h"

/********************************************************************\
 *   Constants   *
\********************************************************************/

static short module = MOD_IMPORT;

/********************************************************************\
 *   Constants, should idealy be defined a user preference dialog    *
\********************************************************************/

static const int MATCH_DATE_THRESHOLD=4; /*within 4 days*/
static const int MATCH_DATE_NOT_THRESHOLD = 16;
/**Transaction's who have an online_id kvp frame have been downloaded 
  online can probably be skipped in the match list, since it is very 
  unlikely that they would match a transaction downloaded at a later
  date and yet not have the same online_id.  This also increases
  performance of the matcher. */
static const int SHOW_TRANSACTIONS_WITH_UNIQUE_ID = FALSE;

/********************************************************************\
 *               Structures passed between the functions             *
\********************************************************************/

struct _transactioninfo
{
  Transaction * trans;
  Split * first_split;

  /* GList of GNCImportMatchInfo's, one for each possible duplicate match. */
  GList * match_list;
  GNCImportMatchInfo * selected_match_info;
  gboolean match_selected_manually;

  GNCImportAction action;
  GNCImportAction previous_action;

  /* In case of a single destination account it is stored here. */
  Account *dest_acc;
  gboolean dest_acc_selected_manually;
};

struct _matchinfo
{
  Transaction * trans;
  Split * split;
  //GNC_match_probability probability;
  gint probability;
};

/* Some simple getters and setters for the above data types. */

GList *
gnc_import_TransInfo_get_match_list (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->match_list;
}

Transaction *
gnc_import_TransInfo_get_trans (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->trans;
}

gboolean 
gnc_import_TransInfo_is_balanced (const GNCImportTransInfo *info)
{
 g_assert (info);
 if(gnc_numeric_equal(xaccTransGetImbalance(gnc_import_TransInfo_get_trans(info)),gnc_numeric_zero()))
   {
     return TRUE;
   }
 else
   {
     return FALSE;
   }
}

Split *
gnc_import_TransInfo_get_fsplit (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->first_split;
}

GNCImportMatchInfo *
gnc_import_TransInfo_get_selected_match (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->selected_match_info;
}

void
gnc_import_TransInfo_set_selected_match (GNCImportTransInfo *info,
					 GNCImportMatchInfo *match,
					 gboolean selected_manually)
{
  g_assert (info);
  info->selected_match_info = match;
  info->match_selected_manually = selected_manually;
}

gboolean
gnc_import_TransInfo_get_match_selected_manually (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->match_selected_manually;
}

GNCImportAction
gnc_import_TransInfo_get_action (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->action;
}

void
gnc_import_TransInfo_set_action (GNCImportTransInfo *info, 
				 GNCImportAction action)
{
  g_assert (info);
  if(action!=info->action)
    {
      info->previous_action = info->action;
      info->action = action;
    }
}

Account *
gnc_import_TransInfo_get_destacc (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->dest_acc;
}
void gnc_import_TransInfo_set_destacc (GNCImportTransInfo *info, 
				       Account *acc,
				       gboolean selected_manually)
{
  g_assert (info);
  info->dest_acc = acc;
  info->dest_acc_selected_manually = selected_manually;
}

gboolean
gnc_import_TransInfo_get_destacc_selected_manually (const GNCImportTransInfo *info)
{
  g_assert (info);
  return info->dest_acc_selected_manually;
}

Split * 
gnc_import_MatchInfo_get_split (const GNCImportMatchInfo * info)
{
  g_assert (info);
  return info->split;
}

gint 
gnc_import_MatchInfo_get_probability (const GNCImportMatchInfo * info)
{
  if(info)
    {
      return info->probability;
    }
  else
    {
      return 0;
    }
}

void gnc_import_TransInfo_delete (GNCImportTransInfo *info)
{
  if (info) {
    g_list_free (info->match_list);
    /*If the transaction is still open, it must be destroyed*/
    if(xaccTransIsOpen(info->trans)==TRUE)
      {
	xaccTransDestroy(info->trans);
	xaccTransCommitEdit(info->trans);
      }
    g_free(info);
  }
}

GdkPixmap* gen_probability_pixmap(gint score_original, GNCImportSettings *settings, GtkWidget * widget)
{
  GdkPixmap* retval = NULL;
  gint i, j;
  gint score;
  const gint height = 15;
  const gint width_each_bar = 7;
  gchar * green_bar = ("bggggb ");
  gchar * yellow_bar = ("byyyyb ");
  gchar * red_bar = ("brrrrb ");
  gchar * black_bar = ("bbbbbb ");
  const gint width_first_bar = 1;
  gchar * black_first_bar = ("b");
  const gint num_colors = 5;
  gchar * size_str;
  gchar * none_color_str = g_strdup_printf("  c None");
  gchar * green_color_str = g_strdup_printf("g c green");
  gchar * yellow_color_str = g_strdup_printf("y c yellow");
  gchar * red_color_str = g_strdup_printf("r c red");
  gchar * black_color_str = g_strdup_printf("b c black");
  gchar * xpm[2+num_colors+height];

  g_assert(settings);
  g_assert(widget);
  if (score_original < 0)
    {
      score = 0;
    }
  else
    {
      score=score_original;
    }
  size_str = g_strdup_printf("%d%s%d%s%d%s",(width_each_bar*score)+width_first_bar/*width*/," ",height," ",num_colors," 1"/*characters per pixel*/);

  /*DEBUG("Begin");*/
  xpm[0]=size_str;
  xpm[1]=none_color_str;
  xpm[2]=green_color_str;
  xpm[3]=yellow_color_str;
  xpm[4]=red_color_str; 
  xpm[5]=black_color_str;
  
  for(i=0;i<height;i++)
    {
      xpm[num_colors+1+i]= g_new0(char,(width_each_bar*score)+width_first_bar+1);
      for(j=0;j<=score;j++)
	{
	  if(i==0||i==height-1)
	    {
	      if (j==0)
		{
		  strcat(xpm[num_colors+1+i],black_first_bar);
		}
	      else
		{
	      strcat(xpm[num_colors+1+i],black_bar);
		}
	    }
	  else
	    {
	      if (j==0)
		{
		  strcat(xpm[num_colors+1+i],black_first_bar);
		}
	      else if (j<=gnc_import_Settings_get_add_threshold(settings))
		{
		  strcat(xpm[num_colors+1+i],red_bar);
		}
	      else if (j>=gnc_import_Settings_get_clear_threshold(settings))
		{
		  strcat(xpm[num_colors+1+i],green_bar);
		}
	      else
		{
		  strcat(xpm[num_colors+1+i],yellow_bar);
		}
	    }
	}
    }  
  
  retval =  gdk_pixmap_colormap_create_from_xpm_d    (NULL,
						      gtk_widget_get_colormap(widget),
						      NULL,
						      NULL,
						      xpm);
  for(i=0;i<=num_colors+height;i++)
    { 
      /*DEBUG("free_loop i=%d%s%s",i,": ",xpm[i]);*/
      g_free(xpm[i]);
    }
  
  return retval;
}

/*-************************************************************************
 * MatchMap- related functions (storing and retrieving)
 */
static Account *
matchmap_find_destination (GncImportMatchMap *matchmap, 
				    GNCImportTransInfo *info)
{
  GncImportMatchMap *tmp_map;
  Account *result;
  g_assert (info);
  
  tmp_map = ((matchmap != NULL) ? matchmap : 
	     gnc_imap_create_from_account 
	     (xaccSplitGetAccount
	      (gnc_import_TransInfo_get_fsplit (info))));

  result = gnc_imap_find_account 
    (tmp_map, GNCIMPORT_DESC, 
     xaccTransGetDescription (gnc_import_TransInfo_get_trans (info)));
  if (result == NULL)
    result = gnc_imap_find_account 
      (tmp_map, GNCIMPORT_MEMO, 
       xaccSplitGetMemo (gnc_import_TransInfo_get_fsplit (info)));
  
  if (matchmap == NULL)
    gnc_imap_destroy (tmp_map);

  return result;
}

/** Store the destination account from trans_info in the matchmap. If
    'use_match' is true, the destination account of the selected
    matching/duplicate transaction is used; otherwise, the stored
    destination_acc pointer is used. */
static void 
matchmap_store_destination (GncImportMatchMap *matchmap, 
			    GNCImportTransInfo *trans_info,
			    gboolean use_match)
{
  GncImportMatchMap *tmp_matchmap = NULL;
  Account *dest;
  g_assert (trans_info);

  /* This will store the destination account of the selected match if 
     the recondile match selected has only two split.  Good idea  
     Christian! */ 
  dest = ((use_match) ?
	  xaccSplitGetAccount
	  (xaccSplitGetOtherSplit 
	   (gnc_import_MatchInfo_get_split 
	    (gnc_import_TransInfo_get_selected_match (trans_info)))) :
	  gnc_import_TransInfo_get_destacc (trans_info));
  if (dest == NULL)
    return;
  
  tmp_matchmap = ((matchmap != NULL) ? 
		  matchmap : 
		  gnc_imap_create_from_account 
		  (xaccSplitGetAccount
		   (gnc_import_TransInfo_get_fsplit (trans_info))));

  gnc_imap_add_account (tmp_matchmap, 
			GNCIMPORT_DESC, 
			xaccTransGetDescription 
			(gnc_import_TransInfo_get_trans (trans_info)), 
			dest);
  gnc_imap_add_account (tmp_matchmap, 
			GNCIMPORT_MEMO, 
			xaccSplitGetMemo 
			(gnc_import_TransInfo_get_fsplit (trans_info)),
			dest);

  if (matchmap == NULL)
    gnc_imap_destroy (tmp_matchmap);
}



/** @brief The transaction matching heuristics are here. 
 */
static void split_find_match (GNCImportTransInfo * trans_info,
			      Split * split, 
			      gint display_threshold,
			      double fuzzy_amount_difference)
{
  /* DEBUG("Begin"); */
  
  /*Ignore the split if the transaction is open for edit, meaning it
    was just downloaded.  Ignore the split if the transaction has an
    online ID , unless overriden in prefs (i.e. do not ignore the
    split if the online_id kvp is NULL or if it has zero length). */
  if ((xaccTransIsOpen(xaccSplitGetParent(split)) == FALSE) &&
      ((gnc_import_get_trans_online_id(xaccSplitGetParent(split))==NULL) ||
       (strlen(gnc_import_get_trans_online_id(xaccSplitGetParent(split))) == 0) ||
       SHOW_TRANSACTIONS_WITH_UNIQUE_ID==TRUE))
    {
      GNCImportMatchInfo * match_info;
      gint prob = 0;
      double downloaded_split_amount, match_split_amount;
      time_t match_time, download_time;
      int datediff_day;
    
      /* Matching heuristics */
    
      /* Amount heuristics */
      downloaded_split_amount = 
	gnc_numeric_to_double
	(xaccSplitGetAmount(gnc_import_TransInfo_get_fsplit (trans_info)));
      /*DEBUG(" downloaded_split_amount=%f", downloaded_split_amount);*/
      match_split_amount = gnc_numeric_to_double(xaccSplitGetAmount(split));
      /*DEBUG(" match_split_amount=%f", match_split_amount);*/
      if(gnc_numeric_equal(xaccSplitGetAmount
			   (gnc_import_TransInfo_get_fsplit (trans_info)),
			   xaccSplitGetAmount(split)))
	{
	  prob = prob+3;
	  DEBUG("heuristics:  probability + 3 (amount)");
	}
      else if (fabs (downloaded_split_amount - match_split_amount) <= 
	       fuzzy_amount_difference)
	{
	  /* ATM fees are sometimes added directly in the transaction.
	     So you withdraw 100$ and get charged 101,25$ in the same
	     transaction */ 
	  prob = prob+2;
	  DEBUG("heuristics:  probability + 2 (amount)");
	}
      else
	{
	  /* If a transaction's amount doesn't match within the
	     threshold, it's very unlikely to be the same transaction
	     so we give it an extra -1 penality */
	  prob = prob-1;
	  /* DEBUG("heuristics:  probability - 1 (amount)"); */
	}
      
      /* Date heuristics */
      match_time = xaccTransGetDate (xaccSplitGetParent (split));
      download_time = 
	xaccTransGetDate (gnc_import_TransInfo_get_trans (trans_info));
      datediff_day = abs(match_time - download_time)/86400;
      /* Sorry, there are not really functions around at all that
	 provide for less hacky calculation of days of date
	 differences. Whatever. On the other hand, the difference
	 calculation itself will work regardless of month/year
	 turnarounds. */
      /*DEBUG("diff day %d", datediff_day);*/
      if (datediff_day == 0)
	{
	  prob = prob+2;
	  DEBUG("heuristics:  probability + 2 (date)");
	}
      else if (datediff_day <= MATCH_DATE_THRESHOLD)
	{
	  prob = prob+1;
	  DEBUG("heuristics:  probability + 1 (date)");
	}
      else if (datediff_day > MATCH_DATE_NOT_THRESHOLD)
	{
	  /* Extra penalty if that split lies awfully far away
	     from the given one. */
	  prob = prob-1;
	  /* DEBUG("heuristics:  probability - 1 (date)"); */
	}
      
    
      /* Memo heuristics */  
      if((strcmp(xaccSplitGetMemo(gnc_import_TransInfo_get_fsplit (trans_info)),
		 xaccSplitGetMemo(split))
	  ==0))
	{	
	  /* An exact match of description gives a +2 */
	  prob = prob+2;
	  DEBUG("heuristics:  probability + 2 (memo)");
	}
      else if((strncmp(xaccSplitGetMemo(gnc_import_TransInfo_get_fsplit (trans_info)),
		       xaccSplitGetMemo(split),
		       strlen(xaccSplitGetMemo(split))/2)
	       ==0))
	{
	  /* Very primitive fuzzy match worth +1.  This matches the
	     first 50% of the strings to skip annoying transaction
	     number some banks seem to include in the memo but someone
	     should write something more sophisticated */ 
      	  prob = prob+1;
	  DEBUG("heuristics:  probability + 1 (memo)");	
	}

      /* Description heuristics */  
      if((strcmp(xaccTransGetDescription
		 (gnc_import_TransInfo_get_trans (trans_info)),
		 xaccTransGetDescription(xaccSplitGetParent(split)))
	  ==0))
	{	
	  /*An exact match of Description gives a +2 */
	  prob = prob+2;
	  DEBUG("heuristics:  probability + 2 (description)");
	}
      else if((strncmp(xaccTransGetDescription
		       (gnc_import_TransInfo_get_trans (trans_info)),
		       xaccTransGetDescription(xaccSplitGetParent(split)),
		       strlen(xaccTransGetDescription
			      (gnc_import_TransInfo_get_trans (trans_info)))/2)
	  ==0))
	{
	  /* Very primitive fuzzy match worth +1.  This matches the
	     first 50% of the strings to skip annoying transaction
	     number some banks seem to include in the memo but someone
	     should write something more sophisticated */ 
      	  prob = prob+1;
	  DEBUG("heuristics:  probability + 1 (description)");	
	}

      if ((gnc_import_get_trans_online_id(xaccSplitGetParent(split))!=NULL) &&
	  (strlen(gnc_import_get_trans_online_id(xaccSplitGetParent(split)))>0))
	{
	  /* If the pref is to show match even with online ID's,
	     reverse the confidence value to distinguish them */
	  prob = 0-prob;
	}

      /* Is the probability high enough? Otherwise do nothing and return. */
      if(prob < display_threshold)
	{
	  return;
	}

      /* The probability is high enough, so allocate an object
	 here. Allocating it only when it's actually being used is
	 probably quite some performance gain. */
      match_info = g_new0(GNCImportMatchInfo,1);
    
      match_info->probability = prob;
      match_info->split = split;
      match_info->trans = xaccSplitGetParent(split);
   

      /* Append that to the list. */
      trans_info->match_list = 
	g_list_append(trans_info->match_list,
		      match_info);
    }
}/* end split_find_match */


/** /brief Iterate through all splits of the originating account of the given
   transaction, and find all matching splits there. */
void gnc_import_find_split_matches(GNCImportTransInfo *trans_info,
				   gint process_threshold, 
				   double fuzzy_amount_difference)
{
  GList * list_element;
  g_assert (trans_info);
  
  /* Get list of splits of the originating account. */
  list_element = 
    g_list_first
    (xaccAccountGetSplitList
     (xaccSplitGetAccount (gnc_import_TransInfo_get_fsplit (trans_info))));

  /* Traverse that list, calling split_find_match on each one. Note
     that xaccAccountForEachSplit is declared in Account.h but
     implemented nowhere :-( */
  while(list_element!=NULL)
    {
      split_find_match (trans_info, list_element->data, 
			process_threshold, fuzzy_amount_difference);
      list_element = g_list_next (list_element);
    }
}


/***********************************************************************
 */

/** /brief -- Processes every selected match
   according to its selected action.  */
void
gnc_import_process_trans_clist (GtkCList *clist, 
				GncImportMatchMap *matchmap)
{
  GNCImportTransInfo * trans_info;
  gint row_number = 0, i = 0;
  g_assert (clist);
  
  /* DEBUG("Begin"); */
  gtk_clist_freeze (clist);
  trans_info = 
    (GNCImportTransInfo *) gtk_clist_get_row_data(clist, 0);
  
  for(i = 1; trans_info != NULL; i++)
    {
      g_assert (trans_info);
      /*DEBUG("Iteration %d, action %d, split %s", i, 
	trans_info->action,
	xaccTransGetDescription (gnc_import_TransInfo_get_trans 
	(trans_info)))*/
      switch (gnc_import_TransInfo_get_action (trans_info))
	{
	case GNCImport_SKIP:
	  break;
	case GNCImport_ADD:
	  /* Transaction gets imported. */

	  /* Is the transaction not balanced and there is a non-NULL destination account? */
	  if (gnc_import_TransInfo_is_balanced(trans_info) == FALSE
	      && gnc_import_TransInfo_get_destacc(trans_info) != NULL) {
	    /* Create the 'other' split. */
	    Split *split = 
	      xaccMallocSplit
	      (xaccAccountGetBook
	       (gnc_import_TransInfo_get_destacc (trans_info)));
	    xaccTransAppendSplit
	      (gnc_import_TransInfo_get_trans (trans_info), split);
	    xaccAccountInsertSplit
	      (gnc_import_TransInfo_get_destacc (trans_info), split);
	    xaccSplitSetValue
	      (split, gnc_numeric_neg(xaccSplitGetValue 
				      (gnc_import_TransInfo_get_fsplit (trans_info))));
	    xaccSplitSetMemo (split, _("Auto-Balance split"));

	    /* Store the mapping to the other account in the MatchMap. */
	    matchmap_store_destination (matchmap, trans_info, FALSE);
	  }
	  
	  xaccSplitSetReconcile(gnc_import_TransInfo_get_fsplit (trans_info), CREC);
	  /*Set reconcile date to today*/
	  xaccSplitSetDateReconciledSecs(gnc_import_TransInfo_get_fsplit (trans_info),
					 time(NULL));
	  /* Done editing. */
	  xaccTransCommitEdit 
	    (gnc_import_TransInfo_get_trans (trans_info));
	  break;
	case GNCImport_CLEAR:
	  /* Transaction gets not imported but the matching one gets 
	     reconciled. */
	  if(gnc_import_MatchInfo_get_split 
	     (gnc_import_TransInfo_get_selected_match (trans_info)) ==NULL)
	    {
	      PERR("The split I am trying to reconcile is NULL, shouldn't happen!")
	    }
	  else
	    {
	      /* Reconcile the matching transaction */
	      /*DEBUG("BeginEdit selected_match")*/
	      xaccTransBeginEdit(trans_info->selected_match_info->trans);

	      if (xaccSplitGetReconcile 
		  (trans_info->selected_match_info->split) == NREC)
		xaccSplitSetReconcile
		  (trans_info->selected_match_info->split, CREC);
	      /* Set reconcile date to today */
	      xaccSplitSetDateReconciledSecs
		(trans_info->selected_match_info->split,time(NULL));

	      /* Copy the online id to the reconciled transaction, so
		 the match will be remembered */ 
	      if ((gnc_import_get_trans_online_id(trans_info->trans)
		   != NULL) && 
		  (strlen (gnc_import_get_trans_online_id(trans_info->trans))
		   > 0))
		gnc_import_set_trans_online_id
		  (trans_info->selected_match_info->trans, 
		   gnc_import_get_trans_online_id(trans_info->trans));
	      
	      /* Done editing. */
	      /*DEBUG("CommitEdit selected_match")*/
	      xaccTransCommitEdit
		(trans_info->selected_match_info->trans);
	      
	      /* Store the mapping to the other account in the MatchMap. */
	      matchmap_store_destination (matchmap, trans_info, TRUE);

	      /* Erase the downloaded transaction */
	      xaccTransDestroy(trans_info->trans);
	      DEBUG("CommitEdit trans")
	      xaccTransCommitEdit(trans_info->trans);
	    }
	  break;
	case GNCImport_EDIT:
	    PERR("EDIT action is UNSUPPORTED!")
	  break;
	default:
	  DEBUG("Invalid GNCImportAction for this imported transaction.");
	}

      /* For all actions except SKIP delete this transaction now. */
      if(trans_info->action != GNCImport_SKIP) {
	row_number = gtk_clist_find_row_from_data(clist, trans_info);
	gtk_clist_remove (clist, row_number);
	/* decrement the iteration counter since we've just removed
	   one row. */
	i--;
      }

      /* Get next trans_info, NULL if finished */
      trans_info =
	(GNCImportTransInfo *) gtk_clist_get_row_data(clist, i);
    }
  
  /*DEBUG("End");*/
  gtk_clist_thaw (clist);
  /*DEBUG("Thawed.")*/
}

/********************************************************************\
 * check_trans_online_id() Callback function to be used by
 * gnc_import_exists_online_id.  Takes pointers to two transaction and
 * returns TRUE if their online_id kvp_frame do NOT match, or if both
 * pointers point to the same transaction.
 * \********************************************************************/
static gboolean check_trans_online_id(Transaction *trans1, void *user_data)
{
  Transaction *trans2 = user_data;
  const gchar *online_id1 = gnc_import_get_trans_online_id(trans1);
  const gchar *online_id2 = gnc_import_get_trans_online_id(trans2);

  if ((trans1 == trans2) || (online_id1 == NULL) || 
      (online_id2 == NULL) || (strcmp(online_id1, online_id2) != 0))
    {
      return TRUE;
    }
  else
    {
      //printf("test_trans_online_id(): Duplicate found\n");
      return FALSE;
    }
}

/** Checks whether the given transaction's online_id already exists in
  its parent account. */
gboolean gnc_import_exists_online_id (Transaction *trans)
{
  int i;
  gboolean online_id_exists = FALSE;
  Account *dest_acct;
  Split *source_split;
  
  /* For each split in the transaction, check whether the parent account
     contains a transaction with the same online id. */
  for (i=0; 
       ((source_split = xaccTransGetSplit(trans, i)) != NULL) &&
	 (online_id_exists == FALSE);
       i++)
    {
      /* DEBUG("%s%d%s","Checking split ",i," for duplicates"); */
      dest_acct = xaccSplitGetAccount(source_split);
      online_id_exists = !xaccAccountForEachTransaction(dest_acct,
							check_trans_online_id,
							trans);
    }

  /* If it does, abort the process for this transaction, since it is
     already in the system. */
  if (online_id_exists == TRUE)
    {
      DEBUG("%s","Transaction with same online ID exists, destroying current transaction");
      xaccTransDestroy(trans);
      xaccTransCommitEdit(trans);
    }
  return online_id_exists;
}


/* ******************************************************************
 */

/** Create a new object of GNCImportTransInfo here. */
GNCImportTransInfo *
gnc_import_TransInfo_new (Transaction *trans, GncImportMatchMap *matchmap)
{
  GNCImportTransInfo *transaction_info;
  g_assert (trans);
  
  transaction_info = g_new0(GNCImportTransInfo,1);
      
  transaction_info->trans=trans;
  /* Only use first split, the source split */
  transaction_info->first_split = xaccTransGetSplit(trans,0);
  
  /* Try to find a previously selected destination account 
     string match for the ADD action */
  
  gnc_import_TransInfo_set_destacc (transaction_info, 
				    matchmap_find_destination (matchmap, transaction_info),
				    FALSE); 
  return transaction_info;
}


/** compare_probability() is used by g_list_sort to sort by probability */
static gint compare_probability (gconstpointer a,
				 gconstpointer b)
{
  return(((GNCImportMatchInfo *)b)->probability - 
	 ((GNCImportMatchInfo *)a)->probability);
}

/** Iterates through all splits of the originating account of
 * trans_info. Sorts the resulting list and sets the selected_match
 * and action fields in the trans_info. 
 */
void 
gnc_import_TransInfo_init_matches (GNCImportTransInfo *trans_info,
				   GNCImportSettings *settings)
{
  GNCImportMatchInfo * best_match;
  g_assert (trans_info);
  
  
  /* Find all split matches in originating account. */
  gnc_import_find_split_matches(trans_info,  
				gnc_import_Settings_get_display_threshold (settings),
				gnc_import_Settings_get_fuzzy_amount (settings));
  
  if (trans_info->match_list != NULL) 
    {
      trans_info->match_list = g_list_sort(trans_info->match_list,
					   compare_probability);
      best_match = g_list_nth_data(trans_info->match_list,0);
      gnc_import_TransInfo_set_selected_match (trans_info,
					       best_match,
					       FALSE);
      if(best_match != NULL && 
	 best_match->probability >= gnc_import_Settings_get_clear_threshold(settings))
	{
	  trans_info->action = GNCImport_CLEAR;
	  trans_info->selected_match_info = best_match;
	}
      else if(best_match == NULL ||
	      best_match->probability <= gnc_import_Settings_get_add_threshold(settings) )
	{
	  trans_info->action=GNCImport_ADD;
	}
      else
	{
	  trans_info->action=GNCImport_SKIP;
	}
    }
  else
    trans_info->action = GNCImport_ADD;
  
  trans_info->previous_action=trans_info->action;
}

/** @} */
