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

#include "gnc-commodity.h"
#include "qof.h"
#include "gnc-budget.h"
#include <optional>

#include "gnc-xml-helper.h"
#include "gnc-xml-sax-node.h"

std::optional<GncGUID> dom_tree_to_guid (GncXmlNode* node);

std::string gnc_strstrip (std::string_view sv);

gnc_commodity* dom_tree_to_commodity_ref (GncXmlNode* node, QofBook* book);
gnc_commodity* dom_tree_to_commodity_ref_no_engine (GncXmlNode* node, QofBook*);

GList* dom_tree_freqSpec_to_recurrences (GncXmlNode* node, QofBook* book);
Recurrence* dom_tree_to_recurrence (GncXmlNode* node);

time64 dom_tree_to_time64 (GncXmlNode* node);
gboolean dom_tree_valid_time64 (time64 ts, const char* name);
GDate* dom_tree_to_gdate (GncXmlNode* node);
gnc_numeric dom_tree_to_gnc_numeric (GncXmlNode* node);
std::optional<std::string> dom_tree_to_text (GncXmlNode* tree);
const char* dom_node_to_text (GncXmlNode* node) noexcept;
gboolean string_to_binary (const gchar* str,  void** v, guint64* data_len);
gboolean dom_tree_create_instance_slots (GncXmlNode* node, QofInstance* inst);

gboolean dom_tree_to_integer (GncXmlNode* node, gint64* daint);
gboolean dom_tree_to_guint16 (GncXmlNode* node, guint16* i);
gboolean dom_tree_to_guint (GncXmlNode* node, guint* i);
gboolean dom_tree_to_boolean (GncXmlNode* node, gboolean* b);

/* higher level structures */
Account* dom_tree_to_account (GncXmlNode* node, QofBook* book);
QofBook* dom_tree_to_book (GncXmlNode* node, QofBook* book);
GNCLot*  dom_tree_to_lot (GncXmlNode* node, QofBook* book);
Transaction* dom_tree_to_transaction (GncXmlNode* node, QofBook* book);
GncBudget* dom_tree_to_budget (GncXmlNode* node, QofBook* book);

struct dom_tree_handler
{
    const char* tag;

    gboolean (*handler) (GncXmlNode*, gpointer data);

    int required;
    int gotten;
};

template <typename T, typename F,
          std::enable_if_t<std::is_invocable_r_v<void, F, const char*>, int> = 0>
inline T
apply_xmlnode_text (F&& f, GncXmlNode* node, T default_val = T{})
{
    if (!node)
        return default_val;

    if (auto txt = dom_node_to_text(node))
        return f(txt);

    if (auto txt = dom_tree_to_text(node))
        return f(txt->c_str());

    return default_val;
}

template <typename Obj, typename F,
          std::enable_if_t<std::is_invocable_r_v<void, F, Obj*, const char*>, int> = 0>
inline bool
apply_xmlnode_text (F&& f, Obj* obj, GncXmlNode* node)
{
    auto set_str = [&](auto txt)
    {
        f (obj, txt);
        return true;
    };
    return apply_xmlnode_text<bool> (set_str, node, false);
}

gboolean dom_tree_generic_parse (GncXmlNode* node,
                                 struct dom_tree_handler* handlers,
                                 gpointer data);

#endif /* _SIXTP_DOM_PARSERS_H_ */
