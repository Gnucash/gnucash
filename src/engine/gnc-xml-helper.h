#ifndef _GNC_XML_HELPER_H_
#define _GNC_XML_HELPER_H_

#include "config.h"

#ifdef HAVE_XML_VERSION_HEADER
#include <libxml/xmlversion.h>
#endif

#if defined(LIBXML_VERSION) && LIBXML_VERSION >= 20000

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
