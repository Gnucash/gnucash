/********************************************************************\
 * SchedXaction.c -- Scheduled Transaction implementation.          *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
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
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <string.h>

#include "FreqSpec.h"
#include "GNCId.h"
#include "Group.h"
#include "SX-ttinfo.h"
#include "SchedXactionP.h"
#include "Transaction.h"
#include "date.h"
#include "gnc-book-p.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "guid.h"
#include "messages.h"

static short module = MOD_SX;

/** Local data defs *****/
void sxprivtransactionListMapDelete( gpointer data, gpointer user_data );

/** Local Prototypes *****/


static void
xaccSchedXactionInit( SchedXaction *sx, GNCBook *book)
{
        AccountGroup        *ag;
        char                *name;

        sx->entity_table = gnc_book_get_entity_table (book);

        sx->freq = xaccFreqSpecMalloc(book);

        xaccGUIDNew( &sx->guid, book );
        xaccStoreEntity( sx->entity_table, sx,
                         &sx->guid, GNC_ID_SCHEDXACTION );
        g_date_clear( &sx->last_date, 1 );
        g_date_clear( &sx->start_date, 1 );
        g_date_clear( &sx->end_date, 1 );

        sx->num_occurances_total = 0;
        sx->kvp_data = kvp_frame_new();
        sx->autoCreateOption = FALSE;
        sx->autoCreateNotify = FALSE;
        sx->advanceCreateDays = 0;
        sx->advanceRemindDays = 0;
	sx->dirty = TRUE;

        /* create a new template account for our splits */
        sx->template_acct = xaccMallocAccount(book);
        name = guid_to_string( &sx->guid );
        xaccAccountSetName( sx->template_acct, name );
        xaccAccountSetCommodity
          (sx->template_acct,
           gnc_commodity_new( "template", "template",
                              "template", "template", 1, book ) );
	g_free( name );
        xaccAccountSetType( sx->template_acct, BANK );
        ag = gnc_book_get_template_group( book );
        xaccGroupInsertAccount( ag, sx->template_acct );
}

SchedXaction*
xaccSchedXactionMalloc(GNCBook *book)
{
        SchedXaction *sx;

        g_return_val_if_fail (book, NULL);

        sx = g_new0( SchedXaction, 1 );
        xaccSchedXactionInit( sx, book );
        gnc_engine_generate_event( &sx->guid, GNC_EVENT_CREATE );

        return sx;
}



static void
sxprivTransMapDelete( gpointer data, gpointer user_data )
{
  Transaction *t = (Transaction *) data;
  xaccTransBeginEdit( t );
  xaccTransDestroy( t );
  xaccTransCommitEdit( t );
  return;
}


static void
delete_template_trans(SchedXaction *sx)
{
  GList *templ_acct_splits, *curr_split_listref;
  Split *curr_split;
  Transaction *split_trans;
  GList *templ_acct_transactions = NULL;

  templ_acct_splits 
    = xaccAccountGetSplitList(sx->template_acct);
  
  for(curr_split_listref = templ_acct_splits;
      curr_split_listref;
      curr_split_listref = curr_split_listref->next)
  {
    curr_split = (Split *) curr_split_listref->data;
    split_trans = xaccSplitGetParent(curr_split);
    if(! (g_list_find(templ_acct_transactions, split_trans)))
    {
      templ_acct_transactions 
	= g_list_prepend(templ_acct_transactions, split_trans);
    }
  }
  
  g_list_foreach(templ_acct_transactions,
		 sxprivTransMapDelete, 
		 NULL);
  
  return;
}
void
xaccSchedXactionFree( SchedXaction *sx )
{
      
  if ( sx == NULL ) return;
  
  xaccFreqSpecFree( sx->freq );
  gnc_engine_generate_event( &sx->guid, GNC_EVENT_DESTROY );
  xaccRemoveEntity( sx->entity_table, &sx->guid );
  
  if ( sx->name )
    g_free( sx->name );

  /* 
   * we have to delete the transactions in the 
   * template account ourselves
   */
  
  delete_template_trans( sx );
  
  /*
   * xaccAccountDestroy removes the account from
   * its group for us AFAICT
   */
  
  xaccAccountBeginEdit(sx->template_acct);
  xaccAccountDestroy(sx->template_acct);
  
  g_free( sx );
  
  return;   
}





FreqSpec *
xaccSchedXactionGetFreqSpec( SchedXaction *sx )
{
        return sx->freq;
}

void
xaccSchedXactionSetFreqSpec( SchedXaction *sx, FreqSpec *fs )
{
        g_return_if_fail( fs );

	DEBUG("Called xaccSchedXactionSetFreqSpec");
        xaccFreqSpecFree( sx->freq );
        sx->freq = fs;
	sx->dirty = TRUE;
}

gchar *
xaccSchedXactionGetName( SchedXaction *sx )
{
        return sx->name;
}

void
xaccSchedXactionSetName( SchedXaction *sx, const gchar *newName )
{
        g_return_if_fail( newName != NULL );
        if ( sx->name != NULL ) {
                g_free( sx->name );
                sx->name = NULL;
        }
	sx->dirty = TRUE;
        sx->name = g_strdup( newName );
}

GDate*
xaccSchedXactionGetStartDate( SchedXaction *sx )
{
        return &sx->start_date;
}

void
xaccSchedXactionSetStartDate( SchedXaction *sx, GDate* newStart )
{
        sx->start_date = *newStart;
	sx->dirty = TRUE;
}

gboolean
xaccSchedXactionHasEndDate( SchedXaction *sx )
{
        return g_date_valid( &sx->end_date );
}

GDate*
xaccSchedXactionGetEndDate( SchedXaction *sx )
{
        return &sx->end_date;
}

void
xaccSchedXactionSetEndDate( SchedXaction *sx, GDate *newEnd )
{
  if ( g_date_valid( newEnd )
       && g_date_compare( newEnd, &sx->start_date ) < 0 ) {
    /* XXX: I reject the bad data - is this the right 
     * thing to do <rgmerk>.
     * This warning is only human readable - the caller
     * doesn't know the call failed.  This is bad
     */
    PWARN( "New end date before start date" ); 
    return;
  }

  sx->end_date = *newEnd;
  sx->dirty = TRUE;
  return;
}

GDate*
xaccSchedXactionGetLastOccurDate( SchedXaction *sx )
{
        return &sx->last_date;
}

void
xaccSchedXactionSetLastOccurDate( SchedXaction *sx, GDate* newLastOccur )
{
  sx->last_date = *newLastOccur;
  sx->dirty = TRUE;
  return;
}

gboolean
xaccSchedXactionHasOccurDef( SchedXaction *sx )
{
  return ( xaccSchedXactionGetNumOccur( sx ) != 0 );
}

gint
xaccSchedXactionGetNumOccur( SchedXaction *sx )
{
  return sx->num_occurances_total;
}

void
xaccSchedXactionSetNumOccur( SchedXaction *sx, gint newNum )
{
  sx->num_occurances_remain = sx->num_occurances_total = newNum;
  sx->dirty = TRUE;
        
}

gint
xaccSchedXactionGetRemOccur( SchedXaction *sx )
{
  return sx->num_occurances_remain;
}

void
xaccSchedXactionSetRemOccur( SchedXaction *sx,
                             gint numRemain )
{
  /* FIXME This condition can be tightened up */
  if ( numRemain > sx->num_occurances_total ) {
    PWARN("The number remaining is greater than the total occurrences");
  }
  else
  {
    sx->num_occurances_remain = numRemain;
    sx->dirty = TRUE;
  }
  return;
}


kvp_value *
xaccSchedXactionGetSlot( SchedXaction *sx, const char *slot )
{
  if (!sx) 
  {
    return NULL;
  }

  return kvp_frame_get_slot(sx->kvp_data, slot);
}

void
xaccSchedXactionSetSlot( SchedXaction *sx, 
			 const char *slot,
			 const kvp_value *value )
{
  if (!sx)
  {
    return;
  }

  kvp_frame_set_slot( sx->kvp_data, slot, value );
  sx->dirty = TRUE;
  return;
}

kvp_frame*
xaccSchedXactionGetSlots( SchedXaction *sx )
{
        return sx->kvp_data;
}

void
xaccSchedXactionSetSlots( SchedXaction *sx, kvp_frame *frm )
{
  sx->kvp_data = frm;
  sx->dirty = TRUE;
}

const GUID*
xaccSchedXactionGetGUID( SchedXaction *sx )
{
        return &sx->guid;
}

void
xaccSchedXactionSetGUID( SchedXaction *sx, GUID g )
{
  sx->guid = g;
  sx->dirty = TRUE;
}

void
xaccSchedXactionGetAutoCreate( SchedXaction *sx,
                               gboolean *outAutoCreate,
                               gboolean *outNotify )
{
  *outAutoCreate = sx->autoCreateOption;
  *outNotify     = sx->autoCreateNotify;
  return;
}

void
xaccSchedXactionSetAutoCreate( SchedXaction *sx,
                               gboolean newAutoCreate,
                               gboolean newNotify )
{
 
  sx->autoCreateOption = newAutoCreate;
  sx->autoCreateNotify = newNotify; 
  sx->dirty = TRUE;
  return;
}

gint
xaccSchedXactionGetAdvanceCreation( SchedXaction *sx )
{
  return sx->advanceCreateDays;
}

void
xaccSchedXactionSetAdvanceCreation( SchedXaction *sx, gint createDays )
{
  sx->advanceCreateDays = createDays;
  sx->dirty = TRUE;
}

gint
xaccSchedXactionGetAdvanceReminder( SchedXaction *sx )
{
        return sx->advanceRemindDays;
}

void
xaccSchedXactionSetAdvanceReminder( SchedXaction *sx, gint reminderDays )
{
  sx->dirty = TRUE;
  sx->advanceRemindDays = reminderDays;
}


/* FIXME: there is a bug in this, I think */
GDate
xaccSchedXactionGetNextInstance( SchedXaction *sx, void *stateData )
{
        GDate         last_occur, next_occur, tmpDate;

        g_date_clear( &last_occur, 1 );
        g_date_clear( &next_occur, 1 );
        g_date_clear( &tmpDate, 1 );

        if ( g_date_valid( &sx->last_date ) ) {
                last_occur = sx->last_date;
        } 

        if ( g_date_valid( &sx->start_date ) ) {
                if ( g_date_valid(&last_occur) ) {
                        last_occur =
                                ( g_date_compare( &last_occur,
                                                  &sx->start_date ) > 0 ?
                                  last_occur : sx->start_date );
                } else {
                        last_occur = sx->start_date;
                }
        }

        if ( g_date_valid( &sx->start_date )
             && ! g_date_valid( &sx->last_date ) ) {
                /* Think about this for a second, and you realize
                 * that if the start date is _today_, we need a
                 * last-occur date such that the 'next instance' is
                 * after that date... one day should be good.
                 *
                 * This only holds for the first instance [read: if the
                 * last[-occur]_date is invalid. */
                g_date_subtract_days( &last_occur, 1 );
        }

        xaccFreqSpecGetNextInstance( sx->freq, &last_occur, &next_occur );

        /* out-of-bounds check */
        if ( xaccSchedXactionHasEndDate( sx ) ) {
                GDate *end_date = xaccSchedXactionGetEndDate( sx );
                if ( g_date_compare( &next_occur, end_date ) > 0 ) {
                        PINFO( "next_occur past end date" );
                        g_date_clear( &next_occur, 1 );
                }
        } else if ( xaccSchedXactionHasOccurDef( sx ) && stateData ) {
                /* FIXME: does this work? */
                gint remaining;
                remaining = xaccSchedXactionGetRemOccur( sx );
                if ( remaining == 0 ) {
                        PINFO( "no more occurances remain" );
                        g_date_clear( &next_occur, 1 );
                }
        }

        return next_occur;
}

GDate
xaccSchedXactionGetInstanceAfter( SchedXaction *sx,
                                  GDate *date,
                                  void *stateData )
{
        GDate next_occur;

        xaccFreqSpecGetNextInstance( sx->freq, date, &next_occur );

        if ( xaccSchedXactionHasEndDate( sx ) ) {
                GDate *end_date;

                end_date = xaccSchedXactionGetEndDate( sx );
                if ( g_date_compare( &next_occur, end_date ) > 0 ) {
                        PINFO( "next_occur past end_date" );
                        g_date_clear( &next_occur, 1 );
                }
        } else if ( xaccSchedXactionHasOccurDef( sx ) && stateData ) {
                /* gint remaining = xaccSchedXactionGetRemOccur( sx ); */
                gint *remaining = (gint*)stateData;
                DEBUG( "stateData [remaining]: %d", *remaining );
                if ( (*remaining - 1) < 0 ) {
                        PINFO( "next_occur is outside "
                               "reminaing-instances window." );
                        g_date_clear( &next_occur, 1 );
                }
        }
        return next_occur;
}

GList *
xaccSchedXactionGetSplits( SchedXaction *sx )
{
  g_return_val_if_fail( sx, NULL );
  return xaccAccountGetSplitList(sx->template_acct);
}

void
xaccSchedXactionSetDirtyness( SchedXaction *sx, gboolean dirty_p)
{
  sx->dirty = dirty_p;
  return;
}

gboolean
xaccSchedXactionIsDirty(SchedXaction *sx)
{
  return sx->dirty;
}


static Split *
pack_split_info (TTSplitInfo *s_info, Account *parent_acct,
                 Transaction *parent_trans, GNCBook *book)
{
  Split *split;
  kvp_frame *split_frame;
  kvp_value *tmp_value;
  const GUID *acc_guid; 
  
  split = xaccMallocSplit(book);

  xaccSplitSetMemo(split, 
		   gnc_ttsplitinfo_get_memo(s_info));

  xaccSplitSetAction(split,
		     gnc_ttsplitinfo_get_action(s_info));
  

  xaccAccountInsertSplit(parent_acct, 
			 split);

  split_frame = xaccSplitGetSlots(split);
  
  tmp_value 
    = kvp_value_new_string(gnc_ttsplitinfo_get_credit_formula(s_info));

  kvp_frame_set_slot_path(split_frame, 
			  tmp_value,
			  GNC_SX_ID,
			  GNC_SX_CREDIT_FORMULA,
                          NULL);
  kvp_value_delete(tmp_value);
		      
  tmp_value
    = kvp_value_new_string(gnc_ttsplitinfo_get_debit_formula(s_info));
  
  kvp_frame_set_slot_path(split_frame,
			  tmp_value,
			  GNC_SX_ID,
			  GNC_SX_DEBIT_FORMULA,
                          NULL);

  kvp_value_delete(tmp_value);

  acc_guid = xaccAccountGetGUID(gnc_ttsplitinfo_get_account(s_info));

  tmp_value = kvp_value_new_guid(acc_guid);

  kvp_frame_set_slot_path(split_frame,
                          tmp_value,
                          GNC_SX_ID,
                          GNC_SX_ACCOUNT,
                          NULL);

  kvp_value_delete(tmp_value);

  return split;
}
  

void
xaccSchedXactionSetTemplateTrans(SchedXaction *sx, GList *t_t_list,
                                 GNCBook *book)
{
  Transaction *new_trans;
  TTInfo *tti;
  TTSplitInfo *s_info;
  Split *new_split;
  GList *split_list;

  g_return_if_fail (book);

  /* delete any old transactions, if there are any */
  delete_template_trans( sx );

  for(;t_t_list != NULL; t_t_list = t_t_list->next)
  {
    tti = t_t_list->data;

    new_trans = xaccMallocTransaction(book);

    xaccTransBeginEdit(new_trans);

    xaccTransSetDescription(new_trans, 
			    gnc_ttinfo_get_description(tti));

    xaccTransSetNum(new_trans,
		    gnc_ttinfo_get_num(tti));
    xaccTransSetCurrency( new_trans,
                          gnc_ttinfo_get_currency(tti) );

    for(split_list = gnc_ttinfo_get_template_splits(tti);
	split_list;
	split_list = split_list->next)
    {
      s_info = split_list->data;
      new_split = pack_split_info(s_info, sx->template_acct,
                                  new_trans, book);
      xaccTransAppendSplit(new_trans, new_split);
    }
    xaccTransCommitEdit(new_trans);
  }
}

void*
xaccSchedXactionCreateSequenceState( SchedXaction *sx )
{
        void *toRet = NULL;

        if ( xaccSchedXactionHasOccurDef( sx ) ) {
                toRet = g_new0( gint, 1 );
                *(gint*)toRet = xaccSchedXactionGetRemOccur( sx );
                DEBUG( "Returning state data [remaining]: %d", *(gint*)toRet );
        } else {
                DEBUG( "Returning null state data" );
        }
        return toRet;
}

void
xaccSchedXactionIncrSequenceState( SchedXaction *sx,
                                   void *stateData )
{
        if ( xaccSchedXactionHasOccurDef( sx ) ) {
                gint *remaining;
                remaining = (gint*)stateData;
                *remaining = *remaining - 1;
        }
}

void
xaccSchedXactionDestroySequenceState( SchedXaction *sx,
                                      void *stateData )
{
        if ( xaccSchedXactionHasOccurDef( sx ) ) {
                g_free( (gint*)stateData );
        }
}

