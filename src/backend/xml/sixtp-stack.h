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
    sixtp *parser;
    gchar *tag;
    gpointer data_for_children;
    GSList *data_from_children; /* in reverse chronological order */
    gpointer frame_data;

    /* Line and column [of the start tag]; set during parsing. */
    int line;
    int col;
} sixtp_stack_frame;

struct _sixtp_parser_context_struct
{
    xmlSAXHandler handler;
    sixtp_sax_data data;
    sixtp_stack_frame *top_frame;
    gpointer top_frame_data;
};
typedef struct _sixtp_parser_context_struct sixtp_parser_context;

void sixtp_stack_frame_destroy(sixtp_stack_frame *sf);

void sixtp_stack_frame_print(sixtp_stack_frame *sf, gint indent, FILE *f);

GSList* sixtp_pop_and_destroy_frame(GSList *frame_stack);

void sixtp_print_frame_stack(GSList *stack, FILE *f);

sixtp_stack_frame* sixtp_stack_frame_new(sixtp* next_parser, char *tag);

sixtp_parser_context* sixtp_context_new(sixtp *initial_parser,
                                        gpointer global_data,
                                        gpointer top_level_data);
void sixtp_context_destroy(sixtp_parser_context* context);
void sixtp_context_run_end_handler(sixtp_parser_context* ctxt);

#endif /* _SIXTP_STACK_H_ */
