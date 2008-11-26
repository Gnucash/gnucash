/*
 * gnc-recurrence-xml-v2.c -- xml routines for Recurrence
 *
 * Copyright (C) 2005 Chris Shoemaker <c.shoemaker@cox.net>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */


#include "config.h"

#include <glib.h>
#include <string.h>

#include "gnc-xml.h"
#include "gnc-xml-helper.h"
#include "qof.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"
#include "io-gncxml-v2.h"
#include "Recurrence.h"

static QofLogModule log_module = GNC_MOD_IO;

const gchar *recurrence_version_string = "1.0.0";
#define recurrence_root          "gnc:recurrence"
#define recurrence_mult          "recurrence:mult"
#define recurrence_period_type   "recurrence:period_type"
#define recurrence_start         "recurrence:start"
#define recurrence_weekend_adj   "recurrence:weekend_adj"

//TODO: I think three of these functions rightly belong in Recurrence.c.

static gboolean
recurrence_period_type_handler(xmlNodePtr node, gpointer d)
{
    PeriodType pt;
    char *nodeTxt;

    nodeTxt = dom_tree_to_text(node);
    g_return_val_if_fail(nodeTxt, FALSE);
    pt = recurrencePeriodTypeFromString(nodeTxt);
    ((Recurrence *) d)->ptype = pt;
    g_free(nodeTxt);
    return (pt != -1);
}

static gboolean
recurrence_start_date_handler(xmlNodePtr node, gpointer r)
{
    GDate *d;

    d = dom_tree_to_gdate(node);
    g_return_val_if_fail(d, FALSE);
    g_return_val_if_fail(g_date_valid(d), FALSE);
    ((Recurrence *) r)->start = *d;
    g_date_free(d);
    return TRUE;
}

static gboolean
recurrence_mult_handler(xmlNodePtr node, gpointer r)
{
    return dom_tree_to_guint16(node, &((Recurrence *)r)->mult);
}

static gboolean
recurrence_weekend_adj_handler(xmlNodePtr node, gpointer d)
{
    WeekendAdjust wadj;
    char *nodeTxt;

    nodeTxt = dom_tree_to_text(node);
    g_return_val_if_fail(nodeTxt, FALSE);
    wadj= recurrenceWeekendAdjustFromString(nodeTxt);
    ((Recurrence *) d)->wadj = wadj;
    g_free(nodeTxt);
    return (wadj != -1);
}

static struct dom_tree_handler recurrence_dom_handlers[] = {
    { recurrence_mult, recurrence_mult_handler, 1, 0 },
    { recurrence_period_type, recurrence_period_type_handler, 1, 0 },
    { recurrence_start, recurrence_start_date_handler, 1, 0 },
    { recurrence_weekend_adj, recurrence_weekend_adj_handler, 1, 0 },
    { NULL, NULL, 0, 0 }
};

Recurrence*
dom_tree_to_recurrence(xmlNodePtr node)
{
    gboolean successful;
    Recurrence *r;

    r = g_new(Recurrence, 1);
    successful = dom_tree_generic_parse (node, recurrence_dom_handlers, r);
    if (!successful) {
        PERR ("failed to parse recurrence node");
        xmlElemDump(stdout, NULL, node);
        g_free(r);
        r = NULL;
    }
    return r;
}

xmlNodePtr
recurrence_to_dom_tree(const gchar *tag, const Recurrence *r)
{
    xmlNodePtr n;
    PeriodType pt;
    GDate d;
    WeekendAdjust wadj;

    n = xmlNewNode(NULL, tag);
    xmlSetProp(n, "version", recurrence_version_string );
    xmlAddChild(n, guint_to_dom_tree(recurrence_mult,
                                     recurrenceGetMultiplier(r)));
    pt = recurrenceGetPeriodType(r);
    xmlAddChild(n, text_to_dom_tree(recurrence_period_type,
                                    recurrencePeriodTypeToString(pt)));
    d = recurrenceGetDate(r);
    xmlAddChild(n, gdate_to_dom_tree(recurrence_start, &d));
    wadj = recurrenceGetWeekendAdjust(r);
    xmlAddChild(n, text_to_dom_tree(recurrence_weekend_adj,
                                    recurrenceWeekendAdjustToString(wadj)));
    return n;
}
