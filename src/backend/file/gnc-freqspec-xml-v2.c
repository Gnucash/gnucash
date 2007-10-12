/********************************************************************
 * gnc-freqspec-xml-v2.c -- xml routines for FreqSpecs              *
 * Copyright (C) 2001 Joshua Sled <jsled@asynchronous.org>          *
 * Copyright (C) 2001 Ben Stanley <bds02@uow.edu.au>                *
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
 *******************************************************************/


#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "qof.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"

#include "io-gncxml-v2.h"

#include "sixtp-dom-parsers.h"
#include "SchedXaction.h"
#include "FreqSpec.h"

const gchar *freqspec_version_string = "1.0.0";

struct freqTypeTuple {
    char         *str;
    FreqType        ft;
};

struct freqTypeTuple freqTypeStrs[] = {
    { "none",             INVALID },
    { "once",             ONCE },
    { "daily",            DAILY },
    { "weekly",           WEEKLY },
    { "monthly",          MONTHLY },
    { "month_relative",   MONTH_RELATIVE },
    { "composite",        COMPOSITE },
    { NULL,               -1 },
};

struct uiFreqTypeTuple {
    char        *str;
    UIFreqType        uift;
};

struct uiFreqTypeTuple uiFreqTypeStrs[] = {
    { "none",         UIFREQ_NONE },
    { "once",         UIFREQ_ONCE },
    { "daily",        UIFREQ_DAILY },
    { "daily_mf",     UIFREQ_DAILY_MF },
    { "weekly",       UIFREQ_WEEKLY },
    { "bi_weekly",    UIFREQ_BI_WEEKLY },
    { "semi_monthly", UIFREQ_SEMI_MONTHLY },
    { "monthly",      UIFREQ_MONTHLY },
    { "quarterly",    UIFREQ_QUARTERLY },
    { "tri_anually",  UIFREQ_TRI_ANUALLY },
    { "semi_yearly",  UIFREQ_SEMI_YEARLY },
    { "yearly",       UIFREQ_YEARLY },
    { NULL,           -1 }
};

/**
 * Struct passed around as user-data when parsing the FreqSpec.
 **/
typedef struct
{
        QofBook         *book;            /* Book we're loading into. */

        Recurrence *recurrence;
        GList *recurrence_list;

        /* fields used in the union of unions... :) */
        GDate                 once_day;     /* once */
        gint64                interval;     /* all [except once] */
        gint64                offset;       /* all [except once] */
        gint64                day;          /* monthly or month-relative */
        gint64                occurrence;   /* month-relative */
        GList                *list;         /* composite */
        UIFreqType            uift;
} fsParseData;

static void
fspd_init( fsParseData *fspd )
{
        fspd->list    = NULL;
        fspd->book    = NULL;
        fspd->recurrence = g_new0(Recurrence, 1);
        fspd->recurrence_list = NULL;
        fspd->uift = UIFREQ_NONE;
        fspd->interval
                = fspd->offset
                = fspd->day
                = fspd->occurrence
                = 0;
        g_date_clear( &fspd->once_day, 1 );
}

static struct dom_tree_handler fs_dom_handlers[];

static
gboolean
gnc_fs_handler( xmlNodePtr node, gpointer d )
{
  return dom_tree_generic_parse( node, fs_dom_handlers, d );
}

static
gboolean
fs_uift_handler( xmlNodePtr node, gpointer data)
{
        fsParseData *fspd = data;
        int            i;
        char        *nodeTxt;
        char            *tmp;

        nodeTxt = dom_tree_to_text( node );

        g_return_val_if_fail( nodeTxt, FALSE );
        for ( i=0; (tmp = uiFreqTypeStrs[i].str) != NULL; i++ ) {
                if ( safe_strcmp( nodeTxt, tmp ) == 0 ) {
                        fspd->uift = uiFreqTypeStrs[i].uift;
                        g_free( nodeTxt );
                        return TRUE;
                }
        }
        g_free( nodeTxt );
        return FALSE;
}

static
gboolean
fs_date_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        GDate                *foo;
        foo = dom_tree_to_gdate( node );
        if ( foo == NULL )
                return FALSE;
        fspd->once_day = *foo;
        g_date_free( foo );
        return TRUE;
}

static
gboolean
fs_interval_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        gboolean        ret;
        gint64          foo;

        ret = dom_tree_to_integer( node, &foo );
        if ( ! ret ) {
                return ret;
        }
        fspd->interval = foo;
        return TRUE;
}

static
gboolean
fs_offset_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        gboolean        ret;
        gint64          foo;

        ret = dom_tree_to_integer( node, &foo );
        if ( ! ret )
                return ret;
        fspd->offset = foo;
        return TRUE;
}

static
gboolean
fs_day_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        gboolean        ret;
        gint64          foo;

        ret = dom_tree_to_integer( node, &foo );
        if ( ! ret )
                return ret;
        fspd->day = foo;
        return TRUE;
}

static
gboolean
fs_weekday_handler( xmlNodePtr node, gpointer data)
{
        fsParseData *fspd = data;
        gboolean        ret;
        gint64          foo;
        ret = dom_tree_to_integer( node, &foo );
        if ( !ret )
                return ret;
        fspd->day = foo;
        return TRUE;
}

static
gboolean
fs_occurrence_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        gboolean        ret;
        gint64          foo;
        ret = dom_tree_to_integer( node, &foo );
        if ( !ret )
                return ret;
        fspd->occurrence = foo;
        return TRUE;
}

static
gboolean
fs_subelement_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        GList *recurrences;

        recurrences = dom_tree_freqSpec_to_recurrences(node, fspd->book);
        if (recurrences == NULL)
                return FALSE;

        {
            GList *r_iter;
            for (r_iter = recurrences; r_iter != NULL; r_iter = r_iter->next)
            {
                Recurrence *r = (Recurrence*)r_iter->data;
                GDate recurrence_date;
                if (fspd->uift == UIFREQ_SEMI_MONTHLY)
                {
                    // complementry hack around 'once' freqspects not being valid. :/
                    recurrence_date = recurrenceGetDate(r);
                    recurrenceSet(r, recurrenceGetMultiplier(r), PERIOD_MONTH, &recurrence_date);
                }
                fspd->recurrence_list = g_list_append(fspd->recurrence_list, r);
            }
        }
        return TRUE;
}

struct dom_tree_handler fs_union_dom_handlers[] = {
        { "fs:date",       fs_date_handler,       0, 0 },
        { "fs:interval",   fs_interval_handler,   0, 0 },
        { "fs:offset",     fs_offset_handler,     0, 0 },
        { "fs:day",        fs_day_handler,        0, 0 },
        { "fs:weekday",    fs_weekday_handler,    0, 0 },
        { "fs:occurrence", fs_occurrence_handler, 0, 0 },
        { "gnc:freqspec",  fs_subelement_handler, 0, 0 },
        { NULL, NULL, 0, 0 },
};

static gboolean
fs_none_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        gboolean	successful;
        successful = dom_tree_generic_parse( node,
                                             fs_union_dom_handlers,
                                             fspd );
        return successful;
}

static
gboolean
fs_once_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        gboolean        successful;

        successful = dom_tree_generic_parse( node,
                                             fs_union_dom_handlers,
                                             fspd );
        if ( !successful )
                return FALSE;
        recurrenceSet(fspd->recurrence, 0, PERIOD_ONCE, &fspd->once_day);
        
        return TRUE;
}

static gboolean
fs_daily_handler(xmlNodePtr node, gpointer data)
{
        fsParseData *fspd = data;
        GDate offset_date;
        gboolean        successful;
        successful = dom_tree_generic_parse(node, fs_union_dom_handlers, fspd );
        if (!successful)
             return FALSE;

        g_date_clear(&offset_date, 1);
        g_date_set_julian(&offset_date, fspd->offset == 0 ? 7 : fspd->offset);
        recurrenceSet(fspd->recurrence, fspd->interval, PERIOD_DAY, &offset_date);

        return TRUE;
}

static
gboolean
fs_weekly_handler( xmlNodePtr node, gpointer data )
{
        fsParseData *fspd = data;
        GDate offset_date;
        gboolean        successful;
        successful = dom_tree_generic_parse( node,
                                             fs_union_dom_handlers,
                                             fspd );
        if ( !successful )
                return FALSE;

        g_date_clear(&offset_date, 1);
        g_date_set_julian(&offset_date, fspd->offset == 0 ? 7 : fspd->offset);
        recurrenceSet(fspd->recurrence, fspd->interval, PERIOD_WEEK, &offset_date);

        return TRUE;
}

static
gboolean
fs_monthly_handler( xmlNodePtr node, gpointer data)
{
        fsParseData *fspd = data;
        GDate offset_date;
        gboolean        successful;
        successful = dom_tree_generic_parse( node,
                                             fs_union_dom_handlers,
                                             fspd );
        if ( !successful )
                return FALSE;


        g_date_clear(&offset_date, 1);
        g_date_set_julian(&offset_date, 1);
        g_date_add_months(&offset_date, fspd->offset);
        g_date_set_day(&offset_date, fspd->day);
        if (fspd->uift == UIFREQ_ONCE)
        {
            // hack...
            recurrenceSet(fspd->recurrence, fspd->interval, PERIOD_ONCE, &offset_date);
        }
        else
        {
            recurrenceSet(fspd->recurrence, fspd->interval, PERIOD_MONTH, &offset_date);
        }
        
        return successful;
}

static
gboolean
fs_month_relative_handler( xmlNodePtr node, gpointer data)
{
        g_critical("this was never supported, how is it in the datafile?");
        return FALSE;
}

static
gboolean
fs_guid_handler( xmlNodePtr node, gpointer data)
{
        fsParseData *fspd = data;
        GUID        *guid;
        guid = dom_tree_to_guid( node );
        return TRUE;
}

static
gboolean
fs_composite_handler( xmlNodePtr node, gpointer data)
{
        fsParseData *fspd = data;
        gboolean        successful;
        successful = dom_tree_generic_parse( node,
                                             fs_union_dom_handlers,
                                             fspd );
        return successful;
}

static struct dom_tree_handler fs_dom_handlers[] = {
        { "gnc:freqspec",      gnc_fs_handler,            0, 0 },
        { "fs:ui_type",        fs_uift_handler,           1, 0 },
        { "fs:id",             fs_guid_handler,           1, 0 },
        { "fs:none",           fs_none_handler,           0, 0 },
        { "fs:once",           fs_once_handler,           0, 0 },
        { "fs:daily",          fs_daily_handler,          0, 0 },
        { "fs:weekly",         fs_weekly_handler,         0, 0 },
        { "fs:monthly",        fs_monthly_handler,        0, 0 },
        { "fs:month_relative", fs_month_relative_handler, 0, 0 },
        { "fs:composite",      fs_composite_handler,      0, 0 },
        { NULL, NULL, 0, 0 }
};

static
gboolean
gnc_freqSpec_end_handler(gpointer data_for_children,
                         GSList* data_from_children, GSList* sibling_data,
                         gpointer parent_data, gpointer global_data,
                         gpointer *result, const gchar *tag)
{
        fsParseData                fspd;
        gboolean                successful = FALSE;
        xmlNodePtr                tree = (xmlNodePtr)data_for_children;
        sixtp_gdv2                *globaldata = (sixtp_gdv2*)global_data;

        fspd_init( &fspd );
        fspd.book = globaldata->book;

        /* this won't actually get invoked [FreqSpecs aren't top-level
           elements]; see dom_tree_to_freqSpec(), below. */
        if ( parent_data )
                return TRUE;

        if ( !tag )
                return TRUE;

        g_return_val_if_fail( tree, FALSE );

        successful = dom_tree_generic_parse( tree, fs_dom_handlers, &fspd );
        if (!successful) {
                xmlElemDump( stdout, NULL, tree );
        }

        xmlFreeNode(tree);

        return successful;
}

sixtp*
gnc_freqSpec_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new( gnc_freqSpec_end_handler, NULL, NULL );
}

static void
common_parse(fsParseData *fspd, xmlNodePtr node, QofBook *book)
{
    gboolean        successful;

    fspd->book = book;
    successful = dom_tree_generic_parse( node, fs_dom_handlers, fspd );
    if (!successful)
    {
        xmlElemDump(stdout, NULL, node);
    }
}

GList*
dom_tree_freqSpec_to_recurrences(xmlNodePtr node, QofBook *book)
{
    fsParseData        fspd;
    fspd_init( &fspd );
    common_parse(&fspd, node, book);
    if (fspd.recurrence_list == NULL)
    {
        fspd.recurrence_list = g_list_append(fspd.recurrence_list, fspd.recurrence);
    }
    return fspd.recurrence_list;
}
