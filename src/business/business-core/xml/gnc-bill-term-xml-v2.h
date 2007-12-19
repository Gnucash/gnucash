/*
 * gnc-bill-term-xml-v2.h -- billing term xml i/o parser/unparsers
 *
 * Copyright (C) 2000 Derek Atkins <warlord@MIT.EDU>
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

#ifndef GNC_BILLTERM_XML_V2_H
#define GNC_BILLTERM_XML_V2_H

#include "gncBillTerm.h"
void gnc_billterm_xml_initialize (void);
GncBillTerm *gnc_billterm_xml_find_or_create(QofBook *book, GUID *guid);

#endif /* GNC_BILLTERM_XML_V2_H */
