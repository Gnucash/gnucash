/********************************************************************
 * sixtp-stack.h                                                    *
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

#ifndef SIXTP_STACK_H
#define SIXTP_STACK_H
#include <glib.h>

#include "sixtp.h"

typedef struct sixtp_stack_frame
{
    sixtp* parser;
    std::string tag; /* empty for the top frame. */
    gpointer data_for_children;
    sixtp_child_result_list data_from_children; /* in document order */
    gpointer frame_data;

    /* Line and column [of the start tag]; set during parsing. */
    int line;
    int col;

    sixtp_stack_frame (sixtp* parser_, std::string tag_)
        : parser (parser_), tag (std::move (tag_)), data_for_children (nullptr),
          frame_data (nullptr), line (-1), col (-1)
    {}
    sixtp_stack_frame (const sixtp_stack_frame&) = delete;
    sixtp_stack_frame& operator= (const sixtp_stack_frame&) = delete;
    sixtp_stack_frame (sixtp_stack_frame&&) = default;
    sixtp_stack_frame& operator= (sixtp_stack_frame&&) = default;
    ~sixtp_stack_frame () = default;
} sixtp_stack_frame;

struct _sixtp_parser_context_struct
{
    xmlSAXHandler handler;
    sixtp_sax_data data;
    /* No top_frame pointer here: the top frame is always data.stack.front(),
       and a cached pointer to it would dangle once anything grows
       data.stack (e.g. deeply nested kvp-frames), since data.stack now
       stores frames by value. */
    gpointer top_frame_data;

    /* Wires up the SAX handler vtable, marks the parse as ok so far, and
       pushes the top stack frame. Doesn't run initial_parser's
       start_handler - that can fail, and a constructor has no way to
       report that back to sixtp_context_new, which needs to unwind via
       sixtp_handle_catastrophe/delete rather than leave a half-built
       object for the caller to somehow clean up itself. */
    _sixtp_parser_context_struct (sixtp* initial_parser, gpointer global_data,
                                  gpointer top_level_data);
    _sixtp_parser_context_struct (const _sixtp_parser_context_struct&) = delete;
    _sixtp_parser_context_struct&
        operator= (const _sixtp_parser_context_struct&) = delete;
    ~_sixtp_parser_context_struct ();
};
typedef struct _sixtp_parser_context_struct sixtp_parser_context;

void sixtp_stack_frame_print (const sixtp_stack_frame* sf, gint indent, FILE* f);

void sixtp_print_frame_stack (const std::vector<sixtp_stack_frame>& stack,
                              FILE* f);

sixtp_parser_context* sixtp_context_new (sixtp* initial_parser,
                                         gpointer global_data,
                                         gpointer top_level_data);
void sixtp_context_run_end_handler (sixtp_parser_context* ctxt);

#endif /* _SIXTP_STACK_H_ */
