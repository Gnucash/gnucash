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

#include "SchedXaction.h"
#include "FreqSpec.h"
#include "GNCIdP.h"
#include "Transaction.h"
#include "TransactionP.h"
#include "date.h"
#include "gnc-engine-util.h"
#include "gnc-event-p.h"
#include "messages.h"
#include "Account.h"
#include "Group.h"
#include "guid.h"
#include "gnc-book.h"
#include "FileDialog.h"

static short module = MOD_SX;

/** Local data defs *****/
void sxprivtransactionListMapDelete( gpointer data, gpointer user_data );

/** Local Prototypes *****/


static void
xaccSchedXactionInit( SchedXaction *sx, GNCBook *book)
{
        AccountGroup        *ag;
        char                *name;
        sx->freq = xaccFreqSpecMalloc();

        xaccGUIDNew( &sx->guid );
        xaccStoreEntity( sx, &sx->guid, GNC_ID_SCHEDXACTION );
        g_date_clear( &sx->last_date, 1 );
        g_date_clear( &sx->start_date, 1 );
        g_date_clear( &sx->end_date, 1 );

        sx->num_occurances_total = -1;
        sx->kvp_data = kvp_frame_new();
        sx->autoCreateOption = FALSE;
        sx->autoCreateNotify = FALSE;
        sx->advanceCreateDays = 0;
        sx->advanceRemindDays = 0;
	sx->dirty = TRUE;

/*        sx->templateSplits = NULL; */
        /* create a new template account for our splits */
        sx->template_acct = xaccMallocAccount();
        name = guid_to_string( &sx->guid );
        xaccAccountSetName( sx->template_acct, name );
        xaccAccountSetCommodity( sx->template_acct,
                                 gnc_commodity_new( "template", "template",
                                                    "template", "template", 1 ) );
	g_free( name );
        xaccAccountSetType( sx->template_acct, BANK );
        ag = gnc_book_get_template_group( book );
        xaccGroupInsertAccount( ag, sx->template_acct );
}

SchedXaction*
xaccSchedXactionMalloc( GNCBook *book )
{
        SchedXaction *sx;
        sx = g_new0( SchedXaction, 1 );
        xaccSchedXactionInit( sx, book );
        gnc_engine_generate_event( &sx->guid, GNC_EVENT_CREATE );
        return sx;
}

void
sxprivtransactionListMapDelete( gpointer data, gpointer user_data )
{
        Transaction *t = (Transaction*)data;
        xaccTransBeginEdit( t );
        xaccTransDestroy( t );
        xaccTransCommitEdit( t );
	return;
}

void
sxprivsplitListMapDelete( gpointer data, gpointer user_data )
{
  Split *s = (Split *) data;
  Transaction *t = xaccSplitGetParent(s);
  xaccTransBeginEdit( t );
  xaccTransDestroy( t );
  xaccTransCommitEdit( t );
}

void
xaccSchedXactionFree( SchedXaction *sx )
{
        AccountGroup *group;
	GList *templ_acct_splits;
        if ( sx == NULL ) return;

        xaccFreqSpecFree( sx->freq );
        gnc_engine_generate_event( &sx->guid, GNC_EVENT_DESTROY );
        xaccRemoveEntity( &sx->guid );

        if ( sx->name )
                g_free( sx->name );

      

	/* 
	 * we have to delete the transactions in the 
	 * template account ourselves
	 */
	
	templ_acct_splits 
	  = xaccAccountGetSplitList(sx->template_acct);

	g_list_foreach(templ_acct_splits, 
		       sxprivsplitListMapDelete,
		       NULL);

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
  if ( g_date_valid( newEnd ) ) {
    if ( g_date_compare( newEnd, &sx->start_date ) < 0 ) {
      /* FIXME:error
	 error( "New end date before start date" ); */
    }
  }
  
  sx->end_date = *newEnd;
  sx->dirty = TRUE;
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
    /* FIXME:error
       error( "more remaining occurances than total" ); */
  }
  sx->num_occurances_remain = numRemain;
  sx->dirty = TRUE;
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

GDate
xaccSchedXactionGetNextInstance( SchedXaction *sx )
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

#if 0
        if ( g_date_valid( &last_occur ) ) {
                g_date_set_time( &tmpDate, time(NULL) );
                last_occur =
                        ( g_date_compare( &last_occur,
                                          &tmpDate ) > 0 ?
                          last_occur : tmpDate );
        } else {
                g_date_set_time( &last_occur, time(NULL) );
        }
#endif /* 0 */
        
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
        return next_occur;
}

GDate xaccSchedXactionGetInstanceAfter( SchedXaction *sx, GDate *date )
{
        GDate next_occur;
        xaccFreqSpecGetNextInstance( sx->freq, date, &next_occur );
        return next_occur;
}

/*
 * XXX: This does what you want, I think <rgmerk>
 */

GList *
xaccSchedXactionGetSplits( SchedXaction *sx )
{
  g_return_val_if_fail( sx, NULL );
  return xaccAccountGetSplitList(sx->template_acct);
}


/* 
 * FIXME: This can be removed, I think <rgmerk>
 */ 

#if 0
void
xaccSchedXactionSetSplits( SchedXaction *sx, GList *newSplits )
{
        g_return_if_fail( sx );
        sx->templateSplits = newSplits;
	return;
}

#endif


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
