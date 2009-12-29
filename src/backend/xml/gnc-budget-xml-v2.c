/*
 * gnc-budget-xml-v2.c -- budget xml i/o implementation
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
#include <stdlib.h>
#include <string.h>

#include "gnc-xml-helper.h"
#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"
#include "io-gncxml-gen.h"
#include "io-gncxml-v2.h"

static QofLogModule log_module = GNC_MOD_IO;

const gchar *budget_version_string = "2.0.0";

/* ids */
#define gnc_budget_string       "gnc:budget"
#define bgt_id_string           "bgt:id"
#define bgt_name_string         "bgt:name"
#define bgt_description_string  "bgt:description"
#define bgt_num_periods_string  "bgt:num-periods"
#define bgt_recurrence_string   "bgt:recurrence"
#define bgt_slots_string        "bgt:slots"

xmlNodePtr
gnc_budget_dom_tree_create(GncBudget *bgt)
{
    xmlNodePtr ret;
    KvpFrame *kf;

    ENTER ("(budget=%p)", bgt);

    ret = xmlNewNode(NULL, BAD_CAST gnc_budget_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST budget_version_string);

    /* field: GUID */
    xmlAddChild(ret, guid_to_dom_tree(bgt_id_string,
                                      gnc_budget_get_guid(bgt)));
    /* field: char* name */
    xmlAddChild(ret, text_to_dom_tree(bgt_name_string,
                                      gnc_budget_get_name(bgt)));
    /* field: char* description */
    xmlAddChild(ret, text_to_dom_tree(bgt_description_string,
                                      gnc_budget_get_description(bgt)));
    /* field: guint num_periods */
    xmlAddChild(ret, guint_to_dom_tree(bgt_num_periods_string,
                                       gnc_budget_get_num_periods(bgt)));
    /* field: Recurrence*  */
    xmlAddChild(ret, recurrence_to_dom_tree(bgt_recurrence_string,
                                            gnc_budget_get_recurrence(bgt)));
    /* slots */
    kf = qof_instance_get_slots(QOF_INSTANCE(bgt));
    if (kf)
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(bgt_slots_string, kf);
        if (kvpnode)
            xmlAddChild(ret, kvpnode);
    }

    LEAVE (" ");
    return ret;
}

/***********************************************************************/
static inline gboolean
set_string(xmlNodePtr node, GncBudget* bgt,
           void (*func)(GncBudget *bgt, const gchar *txt))
{
    gchar* txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);

    func(bgt, txt);
    g_free(txt);
    return TRUE;
}

static gboolean
budget_id_handler (xmlNodePtr node, gpointer bgt)
{
    GUID *guid;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail(guid, FALSE);
    qof_instance_set_guid(QOF_INSTANCE(bgt), guid);
    g_free(guid);
    return TRUE;
}

static gboolean
budget_name_handler (xmlNodePtr node, gpointer bgt)
{
    return set_string(node, GNC_BUDGET(bgt), gnc_budget_set_name);
}

static gboolean
budget_description_handler (xmlNodePtr node, gpointer bgt)
{
    return set_string(node, GNC_BUDGET(bgt), gnc_budget_set_description);
}

static gboolean
budget_num_periods_handler (xmlNodePtr node, gpointer bgt)
{
    guint num_periods;

    if (dom_tree_to_guint(node, &num_periods))
    {
        gnc_budget_set_num_periods(GNC_BUDGET(bgt), num_periods);
        return TRUE;
    }
    else
        return FALSE;
}

static gboolean
budget_recurrence_handler (xmlNodePtr node, gpointer bgt)
{
    Recurrence *r;

    if ((r = dom_tree_to_recurrence(node)) == NULL)
        return FALSE;

    gnc_budget_set_recurrence(GNC_BUDGET(bgt), r);
    g_free(r);
    return TRUE;
}

static gboolean
budget_slots_handler (xmlNodePtr node, gpointer bgt)
{
    return dom_tree_to_kvp_frame_given(
               node, qof_instance_get_slots(QOF_INSTANCE(bgt)));
}

static struct dom_tree_handler budget_handlers[] =
{
    { bgt_id_string, budget_id_handler, 1, 0 },
    { bgt_name_string, budget_name_handler, 0, 0 },
    { bgt_description_string, budget_description_handler, 0, 0 },
    { bgt_num_periods_string, budget_num_periods_handler, 1, 0 },
    { bgt_recurrence_string, budget_recurrence_handler, 1, 0 },
    { bgt_slots_string, budget_slots_handler, 0, 0},
    { NULL, 0, 0, 0 }
};

static gboolean
gnc_budget_end_handler(gpointer data_for_children,
                       GSList* data_from_children, GSList* sibling_data,
                       gpointer parent_data, gpointer global_data,
                       gpointer *result, const gchar *tag)
{
    GncBudget *bgt;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

    if (parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if (!tag)
    {
        return TRUE;
    }

    g_return_val_if_fail(tree, FALSE);

    bgt = dom_tree_to_budget(tree, book);
    xmlFreeNode(tree);
    if (bgt != NULL)
    {
        /* ends up calling book_callback */
        gdata->cb(tag, gdata->parsedata, bgt);
    }

    return bgt != NULL;
}


GncBudget*
dom_tree_to_budget (xmlNodePtr node, QofBook *book)
{
    GncBudget *bgt;

    bgt = gnc_budget_new(book);
    if (!dom_tree_generic_parse (node, budget_handlers, bgt))
    {
        PERR ("failed to parse budget tree");
        gnc_budget_destroy(bgt);
        bgt = NULL;
    }
    return bgt;
}

sixtp*
gnc_budget_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_budget_end_handler, NULL, NULL);
}
/* ======================  END OF FILE ===================*/
