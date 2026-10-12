/********************************************************************\
 * gnc-xml-sax-node.h -- lightweight tree assembled directly from   *
 *                       SAX events, used by the v1/v2 XML readers  *
 *                                                                  *
 * Copyright (C) 2025 The GnuCash Project                          *
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
/** @file gnc-xml-sax-node.h
 *
 * A single GnuCash element (gnc:account, gnc:transaction, a slot, ...)
 * used to be buffered into a real libxml2 xmlNode/xmlDoc subtree by
 * sixtp's generic dom_start_handler/dom_chars_handler before being
 * converted by the dom_tree_to_* functions in sixtp-dom-parsers.cpp.
 *
 * GncXmlNode replaces that libxml2 DOM subtree with a minimal tree
 * built directly from the incoming SAX events (element start,
 * characters, element end) with no libxml2 document/node allocation
 * involved. It intentionally mirrors the handful of libxml2 xmlNode
 * fields (name, type, content, children, next, prev, properties) that
 * the dom_tree_to_* converters rely on, so those converters keep
 * their existing tree-walking logic unchanged and only trade the
 * node type they walk.
 */
#ifndef GNC_XML_SAX_NODE_H
#define GNC_XML_SAX_NODE_H

#include <libxml/tree.h> /* for the xmlElementType enum values only */
#include <cstdio>

struct GncXmlAttr
{
    char* name;
    char* value;
    GncXmlAttr* next;
};

struct GncXmlNode
{
    char* name;
    xmlElementType type;
    char* content;          /* owned; only meaningful on text/comment nodes */
    GncXmlNode* children;   /* first child, or nullptr */
    GncXmlNode* last_child; /* fast append; not for use outside this file */
    GncXmlNode* next;       /* next sibling, or nullptr */
    GncXmlNode* prev;       /* previous sibling, or nullptr */
    GncXmlAttr* properties;
};

/* Create a new, childless element node named 'tag'. */
GncXmlNode* gnc_xml_node_new_element (const char* tag);

/* Create a new, childless element node named 'tag' and append it as
   the last child of 'parent' (which may be nullptr). */
GncXmlNode* gnc_xml_node_new_child (GncXmlNode* parent, const char* tag);

/* Append character data to 'node', merging it into node's existing
   trailing text child if there is one, exactly as libxml2's
   xmlNodeAddContentLen does. */
void gnc_xml_node_add_content (GncXmlNode* node, const char* text, int len);

/* Set (or replace) an attribute on 'node'. */
void gnc_xml_node_set_prop (GncXmlNode* node, const char* name, const char* value);

/* Look up an attribute by name. Returns a newly allocated copy (as if
   by g_strdup) the caller must free with g_free, or nullptr. */
char* gnc_xml_get_prop (const GncXmlNode* node, const char* name);

/* Concatenate the content of a run of sibling text/CDATA nodes, as
   libxml2's xmlNodeListGetString does. Returns a newly allocated
   string the caller must free with g_free, or nullptr if 'children'
   is nullptr. */
char* gnc_xml_node_list_get_string (const GncXmlNode* children);

/* Recursively free 'node' and all its descendants (its next-sibling
   chain is left untouched, matching xmlFreeNode's behavior). */
void gnc_xml_node_free (GncXmlNode* node);

/* Write a simple textual rendition of 'node' to 'out', for debug
   logging in place of the old xmlElemDump calls. */
void gnc_xml_node_dump (FILE* out, const GncXmlNode* node);

/* Recursively convert a real libxml2 subtree (as produced by the
   *_to_dom_tree writers) into an equivalent GncXmlNode tree. Only
   needed where test code exercises a writer's output through a
   dom_tree_to_* reader. */
GncXmlNode* gnc_xml_node_from_libxml (xmlNode* src);

#endif /* GNC_XML_SAX_NODE_H */
