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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#ifndef GNC_XML_HELPER_H
#define GNC_XML_HELPER_H

#include "config.h"

#ifdef HAVE_XML_VERSION_HEADER
#include <libxml/xmlversion.h>
#endif

#if defined(LIBXML_VERSION) && LIBXML_VERSION >= 20000

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

#else /* defined(LIBXML_VERSION) && LIBXML_VERSION >= 20000 */

#  include <gnome-xml/SAX.h>
#  include <gnome-xml/tree.h>
#  include <gnome-xml/parser.h>
#  include <gnome-xml/xmlmemory.h>
#  include <gnome-xml/parserInternals.h>
#  ifndef xmlChildrenNode
#    define xmlChildrenNode childs
#  endif /* ifndef xmlChildrenNode */
#  ifndef xmlRootNode
#    define xmlRootNode root
#  endif /* ifndef xmlRootNode */
#  ifndef xmlAttrPropertyValue
#    define xmlAttrPropertyValue val
#  endif /* ifndef xmlAttrPropertyValue */

#endif /* defined(LIBXML_VERSION) && LIBXML_VERSION >= 20000 */

#endif /* _GNC_XML_HELPER_H_ */
