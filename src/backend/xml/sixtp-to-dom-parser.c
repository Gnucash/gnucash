/********************************************************************
 * sixtp-to-dom-parser.c                                            *
 * Copyright 2001 Gnumatic, Inc.                                    *
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
 ********************************************************************/

#include "config.h"

#include <glib.h>

#include <ctype.h>

#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp.h"

static xmlNsPtr global_namespace = NULL;

/* Don't pass anything in the data_for_children value to this
   function.  It'll cause a segfault */
static gboolean dom_start_handler(
    GSList* sibling_data, gpointer parent_data, gpointer global_data,
    gpointer *data_for_children, gpointer *result, const gchar *tag,
    gchar **attrs)
{
    xmlNodePtr thing;
    gchar** atptr = attrs;

    if(parent_data == NULL)
    {
        thing = xmlNewNode(global_namespace, BAD_CAST tag);
        /* only publish the result if we're the parent */
        *result = thing;
    }
    else
    {
        thing = xmlNewChild((xmlNodePtr) parent_data,
                            global_namespace,
                            BAD_CAST tag,
                            NULL);
        *result = NULL;
    }
    *data_for_children = thing;

    if(attrs != NULL)
    {
        while(*atptr != 0)
        {
            xmlSetProp(thing, BAD_CAST atptr[0], BAD_CAST atptr[1]);
            atptr += 2;
        }
    }
    return TRUE;
}

static void
dom_fail_handler(gpointer data_for_children,
                 GSList* data_from_children,
                 GSList* sibling_data,
                 gpointer parent_data,
                 gpointer global_data,
                 gpointer *result,
                 const gchar *tag)
{
  if(*result) xmlFreeNode(*result);
}

static gboolean dom_chars_handler(
    GSList *sibling_data, gpointer parent_data, gpointer global_data,
    gpointer *result, const char *text, int length)
{
    if(length > 0)
    {
        xmlNodeAddContentLen((xmlNodePtr)parent_data, BAD_CAST text, length);
    }
    return TRUE;
}

sixtp *
sixtp_dom_parser_new(sixtp_end_handler ender,
                     sixtp_result_handler cleanup_result_by_default_func,
                     sixtp_result_handler cleanup_result_on_fail_func)
{
    sixtp *top_level;

    g_return_val_if_fail(ender, NULL);
    
    if(!(top_level =
         sixtp_set_any(sixtp_new(), FALSE,
                       SIXTP_START_HANDLER_ID, dom_start_handler,
                       SIXTP_CHARACTERS_HANDLER_ID, dom_chars_handler,
                       SIXTP_END_HANDLER_ID, ender,
                       SIXTP_FAIL_HANDLER_ID, dom_fail_handler,
                       SIXTP_NO_MORE_HANDLERS)))
    {
        return NULL;
    }

    if(cleanup_result_by_default_func)
    {
      sixtp_set_cleanup_result(top_level, cleanup_result_by_default_func);
    }

    if(cleanup_result_by_default_func)
    {
      sixtp_set_result_fail(top_level, cleanup_result_on_fail_func);
    }

    if(!sixtp_add_sub_parser(top_level, SIXTP_MAGIC_CATCHER, top_level))
    {
        sixtp_destroy(top_level);
        return NULL;
    }

    return top_level;
}
