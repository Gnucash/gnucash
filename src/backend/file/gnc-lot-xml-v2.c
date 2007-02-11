/********************************************************************\
 * gnc-lot-xml-v2.c -- lot xml i/o implementation                   *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
 * Copyright (C) 2002 Linas Vepstas <linas@linas.org>               *
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
\********************************************************************/

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
#include "sixtp-dom-parsers.h"
#include "gnc-lot.h"
#include "gnc-lot-p.h"

static QofLogModule log_module = GNC_MOD_IO;

const gchar *lot_version_string = "2.0.0";

/* ids */
#define gnc_lot_string "gnc:lot"
#define lot_id_string "lot:id"
#define lot_slots_string "lot:slots"

xmlNodePtr
gnc_lot_dom_tree_create(GNCLot *lot)
{
    xmlNodePtr ret;
    kvp_frame *kf;

    ENTER("(lot=%p)", lot);
    ret = xmlNewNode(NULL, BAD_CAST gnc_lot_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST lot_version_string);

    xmlAddChild(ret, guid_to_dom_tree(lot_id_string, gnc_lot_get_guid(lot)));

    kf = gnc_lot_get_slots (lot);
    if (kf)
    {
        xmlNodePtr kvpnode = kvp_frame_to_dom_tree(lot_slots_string, kf);
        if(kvpnode)
        {
            xmlAddChild(ret, kvpnode);
        }
    }

    LEAVE("");
    return ret;
}

/* =================================================================== */

struct lot_pdata
{
  GNCLot  *lot;
  QofBook *book;
};

static gboolean
lot_id_handler (xmlNodePtr node, gpointer p)
{
    struct lot_pdata *pdata = p;
    GUID *guid;

    ENTER("(lot=%p)", pdata->lot);
    guid = dom_tree_to_guid(node);
    gnc_lot_set_guid(pdata->lot, *guid);

    g_free(guid);
    
    LEAVE("");
    return TRUE;
}

static gboolean
lot_slots_handler (xmlNodePtr node, gpointer p)
{
    struct lot_pdata *pdata = p;
    gboolean success;

    ENTER("(lot=%p)", pdata->lot);
    success = dom_tree_to_kvp_frame_given
         (node, gnc_lot_get_slots (pdata->lot));

    LEAVE("");
    g_return_val_if_fail(success, FALSE);
    return TRUE;
}


static struct dom_tree_handler lot_handlers_v2[] = {
    { lot_id_string, lot_id_handler, 1, 0 },
    { lot_slots_string, lot_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static gboolean
gnc_lot_end_handler(gpointer data_for_children,
                        GSList* data_from_children, GSList* sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
    int successful;
    GNCLot *lot;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

    successful = TRUE;

    if(parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if(!tag)
    {
        return TRUE;
    }

    g_return_val_if_fail(tree, FALSE);

    lot = dom_tree_to_lot(tree, book);
    ENTER("(lot=%p)", lot);
    if(lot != NULL)
    {
        gdata->cb(tag, gdata->parsedata, lot);
    }

    xmlFreeNode(tree);

    LEAVE("");
    return lot != NULL;
}

GNCLot*
dom_tree_to_lot (xmlNodePtr node, QofBook *book)
{
    struct lot_pdata pdata;
    GNCLot *lot;
    gboolean successful;

    lot = gnc_lot_new(book);
    ENTER("(lot=%p)", lot);

    pdata.lot = lot;
    pdata.book = book;

    successful = dom_tree_generic_parse (node, lot_handlers_v2,
                                         &pdata);
    if (!successful)
    {
        PERR ("failed to parse lot");
        gnc_lot_destroy (lot);
        lot = NULL;
    }

    LEAVE("");
    return lot;
}

sixtp*
gnc_lot_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_lot_end_handler, NULL, NULL);
}

/* ================== END OF FILE ========================== */
