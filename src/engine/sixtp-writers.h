
#ifndef _SIXTP_WRITERS_H_
#define _SIXTP_WRITERS_H_

#include <config.h>

#include <glib.h>

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

#include "Query.h"

gboolean xml_add_account_restorers(xmlNodePtr p, AccountGroup *g);

gboolean xml_add_commodity_restorers(xmlNodePtr p);

gboolean xml_add_commodity_ref(xmlNodePtr p, const char *tag,
                               const gnc_commodity *c);

gboolean xml_add_query_restorers(xmlNodePtr p, Query *q);

gboolean xml_add_txn_and_split_restorers(xmlNodePtr p, AccountGroup *g);



#endif /* _SIXTP_WRITERS_H_ */
