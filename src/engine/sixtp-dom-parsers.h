#ifndef _SIXTP_DOM_PARSERS_H_
#define _SIXTP_DOM_PARSERS_H_

#include "config.h"

#include <glib.h>

#include "gnc-xml-helper.h"

#include "gnc-commodity.h"
#include "kvp_frame.h"

#include "GNCId.h"

GUID* dom_tree_to_guid(xmlNodePtr node);

gnc_commodity* dom_tree_to_gnc_commodity(xmlNodePtr node);

gboolean dom_tree_handle_kvp(kvp_frame* frame, xmlNodePtr node);

#endif /* _SIXTP_DOM_PARSERS_H_ */
