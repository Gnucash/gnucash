
#ifndef _SIXTP_XML_WRITE_UTILS_H_
#define _SIXTP_XML_WRITE_UTILS_H_

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

#include "gnc-numeric.h"
#include "GNCId.h"
#include "date.h"
#include "kvp_frame.h"

gboolean xml_add_str(xmlNodePtr p, const char *tag, const char *str,
                     gboolean include_if_empty);

gboolean xml_add_character(xmlNodePtr p, const char *tag, const char c);

gboolean xml_add_gint64(xmlNodePtr p, const char *tag, const gint64 value);

gboolean xml_add_gint32(xmlNodePtr p, const char *tag, const gint32 value);

gboolean xml_add_double(xmlNodePtr p, const char *tag, const double value);

gboolean xml_add_gnc_numeric(xmlNodePtr p, const char *tag,
                             const gnc_numeric n);

gboolean xml_add_guid(xmlNodePtr p, const char *tag, const GUID *guid);

gboolean xml_add_editable_timespec(xmlNodePtr p, const char *tag,
                                   const Timespec *ts,
                                   gboolean include_if_zero);

gboolean xml_add_kvp_frame(xmlNodePtr p, const char *tag,
                           const kvp_frame *kvpf, gboolean add_if_empty);



#endif /* _SIXTP_XML_WRITE_UTILS_H_ */
