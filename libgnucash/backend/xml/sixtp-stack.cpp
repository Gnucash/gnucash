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

/* sixtp_stack_frame has no custom destructor and no user-declared
   special member functions: data_from_children (a vector of
   sixtp_child_result) cleans itself up, and tag/data_for_children/
   frame_data are either not owned by the frame or have their
   ownership transferred out before the frame is destroyed. That
   makes it implicitly movable, which is all sixtp_stack_frame_new
   and vector<sixtp_stack_frame> need. */

sixtp_stack_frame
sixtp_stack_frame_new (sixtp* next_parser, char* tag)
{
    sixtp_stack_frame new_frame;
    new_frame.parser = next_parser;
    new_frame.tag = tag;
    new_frame.data_for_children = NULL;
    new_frame.frame_data = NULL;
    new_frame.line = new_frame.col = -1;

    return new_frame;
}

void
sixtp_stack_frame_print (const sixtp_stack_frame* sf, gint indent, FILE* f)
{
    gchar* is = g_strnfill (indent, ' ');

    fprintf (f, "%s(stack-frame %p\n", is, sf);
    fprintf (f, "%s             (line %d) (col %d)\n", is, sf->line, sf->col);
    fprintf (f, "%s             (parser %p)\n", is, sf->parser);
    fprintf (f, "%s             (tag %s)\n", is, sf->tag ? sf->tag : "(null)");
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
sixtp_parser_context*
sixtp_context_new (sixtp* initial_parser, gpointer global_data,
                   gpointer top_level_data)
{
    sixtp_parser_context* ret;

    ret = new sixtp_parser_context ();

    ret->handler.startElement = sixtp_sax_start_handler;
    ret->handler.endElement = sixtp_sax_end_handler;
    ret->handler.characters = sixtp_sax_characters_handler;
    ret->handler.getEntity = sixtp_sax_get_entity_handler;

    ret->data.parsing_ok = TRUE;
    ret->data.global_data = global_data;

    ret->top_frame_data = top_level_data;

    /* reserve some headroom so early pushes (account tree, transaction
       lists, ...) don't force repeated reallocations */
    ret->data.stack.reserve (32);
    ret->data.stack.push_back (sixtp_stack_frame_new (initial_parser, NULL));

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
            sixtp_context_destroy (ret);
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

void
sixtp_context_destroy (sixtp_parser_context* context)
{
    /* Destroying the vector destroys every remaining frame (and,
       transitively, any child results still attached to them),
       including the top frame at index 0. */
    context->data.stack.clear ();
    context->data.saxParserCtxt->userData = NULL;
    context->data.saxParserCtxt->sax = NULL;
    xmlFreeParserCtxt (context->data.saxParserCtxt);
    context->data.saxParserCtxt = NULL;
    delete context;
}
