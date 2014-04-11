/********************************************************************\
 * gnc-xml-helper.h -- api for xml helpers                          *
 *                                                                  *
 * Copyright (C) 2001 James LewisMoss <dres@debian.org>             *
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
\********************************************************************/

#ifndef GNC_XML_HELPER_H
#define GNC_XML_HELPER_H

#include <libxml/xmlversion.h>

#  include <libxml/SAX.h>
#  include <libxml/tree.h>
#  include <libxml/parser.h>
#  include <libxml/xmlmemory.h>
#  include <libxml/parserInternals.h>
#  ifndef xmlChildrenNode
#    define xmlChildrenNode children
#  endif /* ifndef xmlChildrenNode */
#  ifndef xmlRootNode
#    define xmlRootNode children
#  endif /* ifndef xmlRootNode */
#  ifndef xmlAttrPropertyValue
#    define xmlAttrPropertyValue children
#  endif /* ifndef xmlAttrPropertyValue */


#endif /* _GNC_XML_HELPER_H_ */
