/********************************************************************
 * gnc-schedxactions-xml-v2.c -- xml routines for transactions      *
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
 *******************************************************************/

#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "gnc-engine-util.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"

#include "io-gncxml-v2.h"
#include "io-gncxml-gen.h"

#include "sixtp-dom-parsers.h"
#include "SchedXaction.h"

/**
 * The XML output should look something like:
 * <gnc:count-data cd:type="schedXaction">XXX</gnc:count-data>
 * ...
 * <gnc:schedxaction version="1.0.0">
 *   <sx:id type="guid">...</sx:id>
 *   <sx:name>Rent</sx:name>
 *   <sx:autoCreate>y</sx:autoCreate>
 *   <sx:autoCreateNotify>n</sx:autoCreateNotify>
 *   <sx:advanceCreateDays>0</sx:advanceCreateDays>
 *   <sx:advanceRemindDays>5</sx:advanceRemindDays>
 *   <sx:lastOccur>
 *     <gdate>2001-02-28</gdate>
 *   </sx:lastOccur>
 *   <sx:start>
 *     <gdate>2000-12-31</gdate>
 *   </sx:start>
 *   <!-- no end -->
 *   <sx:freq>
 *     <!-- freq spec tree -->
 *   </sx:freq>
 * </gnc:schedxaction>
 * <gnc:schedxaction version="1.0.0">
 *   <sx:id type="guid">...</sx:id>
 *   <sx:name>Loan 1</sx:name>
 *   <sx:manual-conf>f</sx:manual-conf>
 *   <sx:start>
 *      <gdate>2000-12-31</gdate>
 *   </sx:start>
 *   <sx:end type="date">
 *     <gdate>2004-03-20</gdate>
 *   </sx:end>
 *   <sx:freq>
 *     <!-- freqspec tree -->
 *   </sx:freq>
 * </gnc:schedxaction>
 * <gnc:schedxaction version="1.0.0">
 *   <sx:id type="guid">...</sx:id>
 *   <sx:name>Loan 2</sx:name>
 *   <sx:manual-conf>f</sx:manual-conf>
 *   <sx:start>
 *      <gdate>2000-12-31</gdate>
 *   </sx:start>
 *   <sx:end type="num_occur">
 *     <sx:num>42</sx:num>
 *   </sx:end>
 *   <sx:freq>
 *     <!-- freqspec tree -->
 *   </sx:freq>
 * </gnc:schedxaction>
 * 
 * et-cetera...
 * bleh.
 **/

const gchar *schedxaction_version_string = "1.0.0";

xmlNodePtr
gnc_schedXaction_dom_tree_create(SchedXaction *sx)
{
    xmlNodePtr	ret;
    xmlNodePtr	fsNode;
    Timespec	ts;
    GDate	*date;

    /* FIXME: this should be the same as the def in io-gncxml-v2.c */
    ret = xmlNewNode( NULL, "gnc:schedxaction" );

    xmlSetProp( ret, "version", schedxaction_version_string );

    xmlAddChild( ret,
                 guid_to_dom_tree("sx:id",
                                  xaccSchedXactionGetGUID(sx)) );

    xmlNewTextChild( ret, NULL, "sx:name", xaccSchedXactionGetName(sx) );

    //xmlNewTextChild( ret, NULL, "sx:manual-conf",
    //(xaccSchedXactionGetManual(sx) == 1 ? "t" : "f") );

    xmlNewTextChild( ret, NULL, "sx:autoCreate",
                     ( sx->autoCreateOption ? "y" : "n" ) );
    xmlNewTextChild( ret, NULL, "sx:autoCreateNotify",
                     ( sx->autoCreateNotify ? "y" : "n" ) );
    xmlAddChild(ret, int_to_dom_tree("sx:advanceCreateDays",
                                     sx->advanceCreateDays));
    xmlAddChild(ret, int_to_dom_tree("sx:advanceRemindDays",
                                     sx->advanceRemindDays));

    xmlAddChild( ret,
                 gdate_to_dom_tree( "sx:start",
                                    xaccSchedXactionGetStartDate(sx) ) );

    date = xaccSchedXactionGetLastOccurDate(sx);
    if ( g_date_valid( date ) ) {
            xmlAddChild( ret, gdate_to_dom_tree( "sx:last", date ) );
    }

    if ( xaccSchedXactionHasOccurDef(sx) ) {
        xmlAddChild(ret, int_to_dom_tree(
                        "sx:num-occur",
                        (gint32)xaccSchedXactionGetNumOccur(sx)));
        xmlAddChild(ret, int_to_dom_tree(
                        "sx:rem-occur",
                        (gint32)xaccSchedXactionGetRemOccur(sx)));
    } else if ( xaccSchedXactionHasEndDate(sx) ) {
            xmlAddChild( ret,
                         gdate_to_dom_tree( "sx:end",
                                            xaccSchedXactionGetEndDate(sx) ) );
    }

    /* output freq spec */
    fsNode = xmlNewNode(NULL, "sx:freqspec");
    xmlAddChild( fsNode,
                 gnc_freqSpec_dom_tree_create(
                         xaccSchedXactionGetFreqSpec(sx)) );
    xmlAddChild( ret, fsNode );
    
    /* output kvp_frame */
    {
        xmlNodePtr kvpnode =
                kvp_frame_to_dom_tree( "sx:slots",
                                       xaccSchedXactionGetSlots(sx) );
        if ( kvpnode )
        {
            xmlAddChild(ret, kvpnode);
        }
    }

    return ret;
}

static
gboolean
sx_id_handler( xmlNodePtr node, gpointer sx )
{
    GUID        *tmp = dom_tree_to_guid( node );
    g_return_val_if_fail( tmp, FALSE );
    xaccSchedXactionSetGUID( (SchedXaction*)sx, *tmp );
    g_free( tmp );
    return TRUE;
}

static
gboolean
sx_name_handler( xmlNodePtr node, gpointer sx )
{
    gchar *tmp = dom_tree_to_text( node );
    g_return_val_if_fail( tmp, FALSE );
    xaccSchedXactionSetName( (SchedXaction*)sx, tmp );
    g_free( tmp );
    return TRUE;
}

static gboolean
sx_autoCreate_handler( xmlNodePtr node, gpointer sx )
{
        gchar *tmp = dom_tree_to_text( node );
        ((SchedXaction*)sx)->autoCreateOption = (safe_strcmp( tmp, "y" ) == 0 ? TRUE : FALSE );
        return TRUE;
}

static gboolean
sx_notify_handler( xmlNodePtr node, gpointer sx )
{
        gchar *tmp = dom_tree_to_text( node );
        ((SchedXaction*)sx)->autoCreateNotify = (safe_strcmp( tmp, "y" ) == 0 ? TRUE : FALSE );
        return TRUE;
}

static gboolean
sx_advCreate_handler( xmlNodePtr node, gpointer sx )
{
        gint64 advCreate;

        if ( ! dom_tree_to_integer( node, &advCreate ) ) {
                return FALSE;
        }

        xaccSchedXactionSetAdvanceCreation( (SchedXaction*)sx, advCreate );
        return TRUE;
}

static gboolean
sx_advRemind_handler( xmlNodePtr node, gpointer sx )
{
        gint64 advRemind;

        if ( ! dom_tree_to_integer( node, &advRemind ) ) {
                return FALSE;
        }

        xaccSchedXactionSetAdvanceReminder( (SchedXaction*)sx, advRemind );
        return TRUE;
}

static
gboolean
sx_set_date( xmlNodePtr node, SchedXaction *sx,
             void (*settor)( SchedXaction *sx, GDate *d ) )
{
    GDate *date;
    date = dom_tree_to_gdate( node );
    g_return_val_if_fail( date, FALSE );
    (*settor)( sx, date );
    g_date_free( date );

    return TRUE;
}

static
gboolean
sx_start_handler( xmlNodePtr node, gpointer sx )
{
    return sx_set_date( node, (SchedXaction*)sx, 
                        xaccSchedXactionSetStartDate );
}

static
gboolean
sx_last_handler( xmlNodePtr node, gpointer sx )
{
    return sx_set_date( node, (SchedXaction*)sx, 
                        xaccSchedXactionSetLastOccurDate );
}

static
gboolean
sx_end_handler( xmlNodePtr node, gpointer sx )
{
    return sx_set_date( node, (SchedXaction*)sx, 
                        xaccSchedXactionSetEndDate );
}

static
gboolean
sx_freqspec_handler( xmlNodePtr node, gpointer sx )
{
    xmlNodePtr        mark;
    FreqSpec        *fs;

    g_return_val_if_fail( node, FALSE );

    fs = dom_tree_to_freqSpec( xmlGetLastChild( node ) );
    xaccSchedXactionSetFreqSpec( (SchedXaction*)sx, fs );

    return TRUE;
}

#if 0
static
gboolean
sx_manualConf_handler( xmlNodePtr node, gpointer sx )
{
    gchar        *tmp;

    tmp = dom_tree_to_text( node );
    g_return_val_if_fail( tmp, FALSE );

    xaccSchedXactionSetManual( (SchedXaction*)sx,
                               safe_strcmp(tmp, "t") == 0 );

    g_free(tmp);
    return TRUE;
}
#endif //0

static
gboolean
sx_numOccur_handler( xmlNodePtr node, gpointer sx )
{
    gint64        numOccur;

    if ( ! dom_tree_to_integer( node, &numOccur ) ) {
        return FALSE;
    }

    xaccSchedXactionSetNumOccur( (SchedXaction*)sx, numOccur );
    return TRUE;
}

static
gboolean
sx_remOccur_handler( xmlNodePtr node, gpointer sx )
{
    gint64        remOccur;

    if ( ! dom_tree_to_integer( node, &remOccur ) ) {
        return FALSE;
    }

    xaccSchedXactionSetRemOccur( (SchedXaction*)sx, remOccur );
    return TRUE;
}

static
gboolean
sx_slots_handler( xmlNodePtr node, gpointer sx )
{
    return dom_tree_to_kvp_frame_given( node, xaccSchedXactionGetSlots (sx) );
}

struct dom_tree_handler sx_dom_handlers[] = {
    { "sx:id",                sx_id_handler,         1, 0 },
    { "sx:name",              sx_name_handler,       1, 0 },
    { "sx:autoCreate",        sx_autoCreate_handler, 1, 0 },
    { "sx:autoCreateNotify",  sx_notify_handler,     1, 0 },
    { "sx:advanceCreateDays", sx_advCreate_handler,  1, 0 },
    { "sx:advanceRemindDays", sx_advRemind_handler,  1, 0 },
    { "sx:start",             sx_start_handler,      1, 0 },
    { "sx:last",              sx_last_handler,       0, 0 },
    { "sx:num-occur",         sx_numOccur_handler,   0, 0 },
    { "sx:rem-occur",         sx_remOccur_handler,   0, 0 },
    { "sx:end",               sx_end_handler,        0, 0 },
    { "sx:freqspec",          sx_freqspec_handler,   1, 0 },
    { "sx:slots",             sx_slots_handler,      0, 0 },
};

static gboolean
gnc_schedXaction_end_handler(gpointer data_for_children,
                             GSList* data_from_children, GSList* sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
    SchedXaction        *sx;
    gboolean                successful = FALSE;
    xmlNodePtr                achild;
    xmlNodePtr                tree = (xmlNodePtr)data_for_children;
    gxpf_data                *gdata = (gxpf_data*)global_data;

    if ( parent_data ) {
        return TRUE;
    }

    if ( !tag ) {
        return TRUE;
    }

    g_return_val_if_fail( tree, FALSE );

    sx = xaccSchedXactionMalloc( NULL );

    successful = dom_tree_generic_parse( tree, sx_dom_handlers, sx );

    if ( successful ) {
            gdata->cb( tag, gdata->data, sx );
    } else {
            xmlElemDump( stdout, NULL, tree );
            xaccSchedXactionFree( sx );
    }

    xmlFreeNode( tree );

    return successful;
}

sixtp*
gnc_schedXaction_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new( gnc_schedXaction_end_handler, NULL, NULL );
}

static
gboolean
tt_act_handler( xmlNodePtr node, gpointer data)
{
        gnc_template_xaction_data *txd = data;
        Account                *acc;
        acc = dom_tree_to_account( node );
        if ( acc == NULL ) {
                return FALSE;
        } else {
                txd->accts = g_list_append( txd->accts, acc );
        }
        return TRUE;
}

static
gboolean
tt_trn_handler( xmlNodePtr node, gpointer data )
{
        gnc_template_xaction_data *txd = data;
        Transaction        *trn;
        trn = dom_tree_to_transaction( node );
        if ( trn == NULL ) {
                return FALSE;
        } else {
                txd->transactions = g_list_append( txd->transactions, trn );
        }
        return TRUE;
}

struct dom_tree_handler tt_dom_handlers[] = {
        { "gnc:account",     tt_act_handler, 0, 0 },
        { "gnc:transaction", tt_trn_handler, 0, 0 },
};

static gboolean
gnc_template_transaction_end_handler(gpointer data_for_children,
                                     GSList* data_from_children, GSList* sibling_data,
                                     gpointer parent_data, gpointer global_data,
                                     gpointer *result, const gchar *tag)
{
        gboolean                        successful = FALSE;
        xmlNodePtr                        achild;
        xmlNodePtr                        tree = (xmlNodePtr)data_for_children;
        gxpf_data                        *gdata = (gxpf_data*)global_data;
        GList                                *n;
        gnc_template_xaction_data        *txd =
                g_new0( gnc_template_xaction_data, 1 );

        /* the DOM tree will have an account tree [the template group
           and account] and a list of transactions [which will be
           members of the template account].
        
           we want to parse through the dom trees for each, placing
           the null-parent account in the book's template-group slot,
           the others under it, and the transactions as normal. */

        if ( parent_data ) {
                return TRUE;
        }

        if ( !tag ) {
                return TRUE;
        }

        g_return_val_if_fail( tree, FALSE );
        
        successful = dom_tree_generic_parse( tree, tt_dom_handlers, txd );

        if ( successful ) {
                gdata->cb( tag, gdata->data, txd );
        } else {
                xmlElemDump( stdout, NULL, tree );
        }
        
        /* cleanup */
        for ( n = txd->accts; n; n = n->next ) {
                n->data = NULL;
        }
        for ( n = txd->transactions; n; n = n->next ) {
                n->data = NULL;
        }
        g_list_free( txd->accts );
        g_list_free( txd->transactions );
        g_free( txd );
        xmlFreeNode( tree );

        return successful;
}

sixtp*
gnc_template_transaction_sixtp_parser_create( void )
{
        return sixtp_dom_parser_new( gnc_template_transaction_end_handler, NULL, NULL );
}
