#ifndef _GNC_XML_HELPER_H_
#define _GNC_XML_HELPER_H_

#include "config.h"

#ifdef HAVE_XML_VERSION_HEADER
#include <libxml/xmlversion.h>
#endif

#if defined(LIBXML_VERSION) && LIBXML_VERSION >= 20000

#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <libxml/parserInternals.h>
#ifndef xmlChildrenNode
#define xmlChildrenNode children
#define xmlRootNode children
#endif

#else

#include <gnome-xml/tree.h>
#include <gnome-xml/parser.h>
#include <gnome-xml/xmlmemory.h>
#include <gnome-xml/parserInternals.h>
#ifndef xmlChildrenNode
#define xmlChildrenNode childs
#define xmlRootNode root
#endif

#endif

#endif /* _GNC_XML_HELPER_H_ */
