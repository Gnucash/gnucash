/* gnc-address-xml-v2.h -- Address XML header
 *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 */

#ifndef GNC_ADDRESS_XML_V2_H
#define GNC_ADDRESS_XML_V2_H

#include "gncAddress.h"

gboolean   gnc_dom_tree_to_address (xmlNodePtr node, GncAddress *address);
xmlNodePtr gnc_address_to_dom_tree (const char *tag, GncAddress *addr);

#endif /* GNC_ADDRESS_XML_V2_H */
