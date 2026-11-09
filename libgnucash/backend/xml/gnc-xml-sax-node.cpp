/********************************************************************\
 * gnc-xml-sax-node.cpp -- lightweight tree assembled directly from *
 *                        SAX events, used by the v1/v2 XML readers *
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
#include <config.h>
#include <glib.h>
#include <cstring>

#include "gnc-xml-sax-node.h"
#include <libxml/tree.h>

GncXmlNode*
gnc_xml_node_new_element (const char* tag)
{
    auto node = g_new0 (GncXmlNode, 1);
    node->name = g_strdup (tag);
    node->type = XML_ELEMENT_NODE;
    return node;
}

GncXmlNode*
gnc_xml_node_new_child (GncXmlNode* parent, const char* tag)
{
    auto node = gnc_xml_node_new_element (tag);

    if (parent)
    {
        if (parent->last_child)
        {
            parent->last_child->next = node;
            node->prev = parent->last_child;
        }
        else
        {
            parent->children = node;
        }
        parent->last_child = node;
    }

    return node;
}

void
gnc_xml_node_add_content (GncXmlNode* node, const char* text, int len)
{
    if (!node || len <= 0)
        return;

    if (node->last_child && node->last_child->type == XML_TEXT_NODE)
    {
        auto text_part = g_strndup (text, len);
        auto full = g_strconcat (node->last_child->content ? node->last_child->content : "",
                                 text_part, nullptr);
        g_free (text_part);
        g_free (node->last_child->content);
        node->last_child->content = full;
        return;
    }

    auto text_node = g_new0 (GncXmlNode, 1);
    text_node->type = XML_TEXT_NODE;
    text_node->name = g_strdup ("text");
    text_node->content = g_strndup (text, len);

    if (node->last_child)
    {
        node->last_child->next = text_node;
        text_node->prev = node->last_child;
    }
    else
    {
        node->children = text_node;
    }
    node->last_child = text_node;
}

void
gnc_xml_node_set_prop (GncXmlNode* node, const char* name, const char* value)
{
    if (!node)
        return;

    for (auto attr = node->properties; attr; attr = attr->next)
    {
        if (g_strcmp0 (attr->name, name) == 0)
        {
            g_free (attr->value);
            attr->value = g_strdup (value);
            return;
        }
    }

    auto attr = g_new0 (GncXmlAttr, 1);
    attr->name = g_strdup (name);
    attr->value = g_strdup (value);
    attr->next = node->properties;
    node->properties = attr;
}

char*
gnc_xml_get_prop (const GncXmlNode* node, const char* name)
{
    if (!node)
        return nullptr;

    for (auto attr = node->properties; attr; attr = attr->next)
    {
        if (g_strcmp0 (attr->name, name) == 0)
            return g_strdup (attr->value);
    }

    return nullptr;
}

char*
gnc_xml_node_list_get_string (const GncXmlNode* children)
{
    if (!children)
        return nullptr;

    GString* str = g_string_new (nullptr);
    for (auto n = children; n; n = n->next)
    {
        if ((n->type == XML_TEXT_NODE || n->type == XML_CDATA_SECTION_NODE) && n->content)
            g_string_append (str, n->content);
    }

    return g_string_free (str, FALSE);
}

void
gnc_xml_node_free (GncXmlNode* node)
{
    if (!node)
        return;

    auto child = node->children;
    while (child)
    {
        auto next = child->next;
        gnc_xml_node_free (child);
        child = next;
    }

    for (auto attr = node->properties; attr; )
    {
        auto next = attr->next;
        g_free (attr->name);
        g_free (attr->value);
        g_free (attr);
        attr = next;
    }

    g_free (node->name);
    g_free (node->content);
    g_free (node);
}

GncXmlNode*
gnc_xml_node_from_libxml (xmlNode* src)
{
    if (!src)
        return nullptr;

    auto node = g_new0 (GncXmlNode, 1);
    node->name = g_strdup (reinterpret_cast<const char*> (src->name));
    node->type = src->type;
    if (src->type != XML_ELEMENT_NODE && src->content)
        node->content = g_strdup (reinterpret_cast<const char*> (src->content));

    for (auto attr = src->properties; attr; attr = attr->next)
    {
        auto value = xmlNodeListGetString (src->doc, attr->children, 1);
        gnc_xml_node_set_prop (node, reinterpret_cast<const char*> (attr->name),
                               reinterpret_cast<const char*> (value));
        xmlFree (value);
    }

    for (auto child = src->children; child; child = child->next)
    {
        auto converted = gnc_xml_node_from_libxml (child);
        if (node->last_child)
        {
            node->last_child->next = converted;
            converted->prev = node->last_child;
        }
        else
        {
            node->children = converted;
        }
        node->last_child = converted;
    }

    return node;
}

void
gnc_xml_node_dump (FILE* out, const GncXmlNode* node)
{
    if (!node)
        return;

    if (node->type == XML_TEXT_NODE || node->type == XML_CDATA_SECTION_NODE)
    {
        if (node->content)
            fputs (node->content, out);
        return;
    }

    fprintf (out, "<%s", node->name ? node->name : "(null)");
    for (auto attr = node->properties; attr; attr = attr->next)
        fprintf (out, " %s=\"%s\"", attr->name, attr->value ? attr->value : "");

    if (!node->children)
    {
        fputs ("/>", out);
        return;
    }

    fputs (">", out);
    for (auto child = node->children; child; child = child->next)
        gnc_xml_node_dump (out, child);
    fprintf (out, "</%s>", node->name ? node->name : "(null)");
}
