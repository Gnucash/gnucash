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

Timespec* dom_tree_to_timespec(xmlNodePtr node);
gnc_numeric* dom_tree_to_gnc_numeric(xmlNodePtr node);
gchar * dom_tree_to_text(xmlNodePtr tree);

kvp_frame* dom_tree_handle_kvp(xmlNodePtr node);

#endif /* _SIXTP_DOM_PARSERS_H_ */
