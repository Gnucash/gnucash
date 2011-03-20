/* gnc-owner-xml-v2.h -- Owner XML header
 *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_OWNER_XML_V2_H
#define GNC_OWNER_XML_V2_H

#include "gncOwner.h"
#include "qof.h"

gboolean   gnc_dom_tree_to_owner (xmlNodePtr node, GncOwner *owner,
                                  QofBook *book);
xmlNodePtr gnc_owner_to_dom_tree (const char *tag, GncOwner *addr);
void gnc_owner_xml_initialize (void);

#endif /* GNC_OWNER_XML_V2_H */
