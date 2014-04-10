/* gnc-owner-xml-v2.h -- Owner XML header
 *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_OWNER_XML_V2_H
#define GNC_OWNER_XML_V2_H

#include "gncOwner.h"
#include "gnc-book.h"

gboolean   gnc_dom_tree_to_owner (xmlNodePtr node, GncOwner *owner,
				  GNCBook *book);
xmlNodePtr gnc_owner_to_dom_tree (const char *tag, GncOwner *addr);

#endif /* GNC_OWNER_XML_V2_H */
