/********************************************************************
 * sixtp-dom-parsers.h                                              *
 * Copyright 2001 Gnumatic, Inc.                                    *
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
 ********************************************************************/

#ifndef _SIXTP_DOM_PARSERS_H_
#define _SIXTP_DOM_PARSERS_H_

#include "config.h"

#include <glib.h>

#include "gnc-xml-helper.h"

#include "gnc-commodity.h"
#include "kvp_frame.h"
#include "date.h"
#include "gnc-numeric.h"

#include "GNCId.h"

GUID* dom_tree_to_guid(xmlNodePtr node);

gnc_commodity* dom_tree_to_commodity_ref(xmlNodePtr node);
gnc_commodity* associate_commodity_ref_with_engine_commodity(
    gnc_commodity *com);
gnc_commodity *dom_tree_to_commodity_ref_no_engine(xmlNodePtr node);

Timespec* dom_tree_to_timespec(xmlNodePtr node);
gnc_numeric* dom_tree_to_gnc_numeric(xmlNodePtr node);
gchar * dom_tree_to_text(xmlNodePtr tree);
gboolean string_to_binary(const gchar *str,  void **v, guint64 *data_len);
gboolean string_to_integer(const char *content, gint64 *to);

kvp_frame* dom_tree_to_kvp_frame(xmlNodePtr node);
kvp_value* dom_tree_to_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_integer_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_double_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_numeric_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_string_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_guid_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_binary_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_list_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_frame_kvp_value(xmlNodePtr node);

gboolean dom_tree_to_integer(xmlNodePtr node, gint64 *daint);


struct dom_tree_handler
{
    const char *tag;

    gboolean (*handler) (xmlNodePtr, gpointer);

    int required;
    int gotten;
};

gboolean dom_tree_generic_parse(xmlNodePtr node,
                                struct dom_tree_handler *handlers,
                                gpointer data);



#endif /* _SIXTP_DOM_PARSERS_H_ */
