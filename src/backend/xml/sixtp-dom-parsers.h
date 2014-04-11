/********************************************************************
 * sixtp-dom-parsers.h                                              *
 * Copyright (c) 2001 Gnumatic, Inc.                                *
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
 ********************************************************************/

#ifndef SIXTP_DOM_PARSERS_H
#define SIXTP_DOM_PARSERS_H

#include <glib.h>

#include "gnc-xml-helper.h"

#include "gnc-commodity.h"
#include "qof.h"
#include "gnc-budget.h"

GUID* dom_tree_to_guid(xmlNodePtr node);

gnc_commodity* dom_tree_to_commodity_ref(xmlNodePtr node, QofBook *book);
gnc_commodity *dom_tree_to_commodity_ref_no_engine(xmlNodePtr node, QofBook *);

GList* dom_tree_freqSpec_to_recurrences(xmlNodePtr node, QofBook *book);
Recurrence* dom_tree_to_recurrence(xmlNodePtr node);

Timespec dom_tree_to_timespec(xmlNodePtr node);
gboolean dom_tree_valid_timespec(Timespec *ts, const gchar *name);
GDate* dom_tree_to_gdate(xmlNodePtr node);
gnc_numeric* dom_tree_to_gnc_numeric(xmlNodePtr node);
gchar * dom_tree_to_text(xmlNodePtr tree);
gboolean string_to_binary(const gchar *str,  void **v, guint64 *data_len);

gboolean dom_tree_to_kvp_frame_given(xmlNodePtr node, kvp_frame *frame);

kvp_frame* dom_tree_to_kvp_frame(xmlNodePtr node);
kvp_value* dom_tree_to_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_integer_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_double_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_numeric_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_string_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_guid_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_timespec_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_binary_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_list_kvp_value(xmlNodePtr node);
kvp_value* dom_tree_to_frame_kvp_value(xmlNodePtr node);

gboolean dom_tree_to_integer(xmlNodePtr node, gint64 *daint);
gboolean dom_tree_to_guint16(xmlNodePtr node, guint16 *i);
gboolean dom_tree_to_guint(xmlNodePtr node, guint *i);

/* higher level structures */
Account* dom_tree_to_account(xmlNodePtr node, QofBook *book);
QofBook* dom_tree_to_book   (xmlNodePtr node, QofBook *book);
GNCLot*  dom_tree_to_lot    (xmlNodePtr node, QofBook *book);
Transaction* dom_tree_to_transaction(xmlNodePtr node, QofBook *book);
GncBudget* dom_tree_to_budget(xmlNodePtr node, QofBook *book);

struct dom_tree_handler
{
    const char *tag;

    gboolean (*handler) (xmlNodePtr, gpointer data);

    int required;
    int gotten;
};

gboolean dom_tree_generic_parse(xmlNodePtr node,
                                struct dom_tree_handler *handlers,
                                gpointer data);


#endif /* _SIXTP_DOM_PARSERS_H_ */
