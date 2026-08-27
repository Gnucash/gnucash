/********************************************************************
 * sixtp-stack.c                                                    *
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
#include <config.h>
#include "sixtp.h"
#include "sixtp-stack.h"

void
sixtp_stack_frame_print (const sixtp_stack_frame* sf, gint indent, FILE* f)
{
    gchar* is = g_strnfill (indent, ' ');

    fprintf (f, "%s(stack-frame %p\n", is, sf);
    fprintf (f, "%s             (line %d) (col %d)\n", is, sf->line, sf->col);
    fprintf (f, "%s             (parser %p)\n", is, sf->parser);
    fprintf (f, "%s             (tag %s)\n", is,
             sf->tag.empty () ? "(null)" : sf->tag.c_str ());
    fprintf (f, "%s             (data-for-children %p)\n", is,
             sf->data_for_children);

    {
        fprintf (f, "%s             (data-from-children", is);
        for (auto& cr : sf->data_from_children)
        {
            fputc (' ', f);
            sixtp_child_result_print (&cr, f);
        }
        fprintf (f, ")\n");
    }

    fprintf (f, "%s             (frame-data %p))\n", is, sf->frame_data);
    fflush (f);
    g_free (is);
}

void
sixtp_print_frame_stack (const std::vector<sixtp_stack_frame>& stack, FILE* f)
{
    /* stack.back() is the innermost frame, so walk it front-to-back for
       outermost-to-innermost debugging output. */
    int indent = 0;

    for (auto it = stack.rbegin (); it != stack.rend (); ++it)
    {
        sixtp_stack_frame_print (&*it, indent, f);
        indent += 2;
    }
}


/* Parser context */
_sixtp_parser_context_struct::_sixtp_parser_context_struct (
    sixtp* initial_parser, gpointer global_data, gpointer top_level_data)
    : handler (), data (), top_frame_data (top_level_data)
{
    handler.startElement = sixtp_sax_start_handler;
    handler.endElement = sixtp_sax_end_handler;
    handler.characters = sixtp_sax_characters_handler;
    handler.getEntity = sixtp_sax_get_entity_handler;

    data.parsing_ok = TRUE;
    data.global_data = global_data;

    /* reserve some headroom so early pushes (account tree, transaction
       lists, ...) don't force repeated reallocations */
    data.stack.reserve (32);
    data.stack.emplace_back (initial_parser, std::string ());
}

_sixtp_parser_context_struct::~_sixtp_parser_context_struct ()
{
    /* Destroying the vector destroys every remaining frame (and,
       transitively, any child results still attached to them),
       including the top frame at index 0. */
    data.stack.clear ();
    data.saxParserCtxt->userData = NULL;
    data.saxParserCtxt->sax = NULL;
    xmlFreeParserCtxt (data.saxParserCtxt);
    data.saxParserCtxt = NULL;
}

sixtp_parser_context*
sixtp_context_new (sixtp* initial_parser, gpointer global_data,
                   gpointer top_level_data)
{
    auto* ret = new sixtp_parser_context (initial_parser, global_data,
                                          top_level_data);

    /* top is only ever used here, before any further push can move it -
       every later reference to the top frame goes through
       data.stack.front() instead of a cached pointer. */
    sixtp_stack_frame& top = ret->data.stack.back ();

    if (initial_parser->start_handler)
    {
        if (!initial_parser->start_handler (sixtp_no_children (),
                                            &ret->top_frame_data,
                                            &ret->data.global_data,
                                            &top.data_for_children,
                                            &top.frame_data,
                                            NULL, NULL))
        {
            sixtp_handle_catastrophe (&ret->data);
            delete ret;
            return NULL;
        }
    }

    return ret;
}

void
sixtp_context_run_end_handler (sixtp_parser_context* ctxt)
{
    sixtp_stack_frame& top = ctxt->data.stack.front ();

    if (top.parser->end_handler)
    {
        ctxt->data.parsing_ok &=
            top.parser->end_handler (
                top.data_for_children,
                top.data_from_children,
                sixtp_no_children (),
                ctxt->top_frame_data,
                ctxt->data.global_data,
                &top.frame_data,
                NULL);
    }
}
