/********************************************************************
 * gnc-transactions-xml-v2.c -- xml routines for transactions       *
 * Copyright (C) 2001 Rob Browning                                  *
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

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"

#include "sixtp-dom-parsers.h"
#include "AccountP.h"
#include "Account.h"
#include "Group.h"

const gchar *transaction_version_string = "2.0.0";

xmlNodePtr
gnc_transaction_dom_tree_create(const Transaction *com)
{
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, "gnc:transaction");

    xmlSetProp(ret, "version", transaction_version_string);
    

    return ret;
}

/***********************************************************************/


static gboolean
valid_transaction(const Transaction *com)
{
    return TRUE;
}

static gboolean
gnc_transaction_end_handler(gpointer data_for_children,
                            GSList* data_from_children, GSList* sibling_data,
                            gpointer parent_data, gpointer global_data,
                            gpointer *result, const gchar *tag)
{
    Transaction *tran;
    xmlNodePtr achild;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    sixtp_gdv2 *globaldata = (sixtp_gdv2*)global_data;

    if(parent_data)
    {
        return TRUE;
    }


    if(!valid_transaction(com))
    {
        return FALSE;
    }

    globaldata->accTransactionFunc(globaldata, tran);
    
    return TRUE
}


sixtp*
gnc_transaction_sixtp_parser_create(void)
{
    return sixtp_dom_parser_new(gnc_transaction_end_handler);
}
