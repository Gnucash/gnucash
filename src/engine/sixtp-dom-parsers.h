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


#endif /* _SIXTP_DOM_PARSERS_H_ */
