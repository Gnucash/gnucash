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

#include "config.h"

#include "sixtp.h"
#include "sixtp-stack.h"

void
sixtp_stack_frame_destroy(sixtp_stack_frame *sf) 
{
  GSList *lp;

  /* cleanup all the child data */
  for(lp = sf->data_from_children; lp; lp = lp->next) 
  {
    sixtp_child_result_destroy((sixtp_child_result *) lp->data);
  }
  g_slist_free(sf->data_from_children);
  sf->data_from_children = NULL;

  g_free(sf);
}

sixtp_stack_frame*
sixtp_stack_frame_new(sixtp* next_parser, char *tag)
{
    sixtp_stack_frame* new_frame;
    
    new_frame = g_new0(sixtp_stack_frame, 1);
    new_frame->parser = next_parser;
    new_frame->tag = tag;
    new_frame->data_for_children = NULL;
    new_frame->data_from_children = NULL;
    new_frame->frame_data = NULL;
    new_frame->line = new_frame->col = -1;

    return new_frame;
}

void
sixtp_stack_frame_print(sixtp_stack_frame *sf, gint indent, FILE *f) 
{
  gchar *is = g_strnfill(indent, ' ');

  fprintf(f, "%s(stack-frame %p\n", is, sf);
  fprintf(f, "%s             (line %d) (col %d)\n", is, sf->line, sf->col );
  fprintf(f, "%s             (parser %p)\n", is, sf->parser);
  fprintf(f, "%s             (tag %s)\n", is, sf->tag ? sf->tag : "(null)");
  fprintf(f, "%s             (data-for-children %p)\n", is,
          sf->data_for_children);

  {
    GSList *lp;
    fprintf(f, "%s             (data-from-children", is);
    for(lp = sf->data_from_children; lp; lp = lp->next) 
    {
      fputc(' ', f);
      sixtp_child_result_print((sixtp_child_result *) lp->data, f);
    }
    fprintf(f, ")\n");
  }

  fprintf(f, "%s             (frame-data %p))\n", is, sf->frame_data);
  fflush(f);
  g_free(is);
}

GSList*
sixtp_pop_and_destroy_frame(GSList *frame_stack) 
{
  sixtp_stack_frame *dead_frame = (sixtp_stack_frame *) frame_stack->data;
  GSList* result;

  result = g_slist_next(frame_stack);
  sixtp_stack_frame_destroy(dead_frame);
  g_slist_free_1(frame_stack);
  return(result);
}

void
sixtp_print_frame_stack(GSList *stack, FILE *f) 
{
  /* first, some debugging output */
  GSList *printcopy = g_slist_reverse(g_slist_copy(stack));
  GSList *lp;
  int indent = 0;
  
  for(lp = printcopy; lp; lp = lp->next) 
  {
    sixtp_stack_frame *frame = (sixtp_stack_frame *) lp->data;
    sixtp_stack_frame_print(frame, indent, f);
    indent += 2;
  }

}


/* Parser context */
sixtp_parser_context*
sixtp_context_new(sixtp *initial_parser, gpointer global_data,
                  gpointer top_level_data)
{
    sixtp_parser_context* ret;

    ret = g_new0(sixtp_parser_context, 1);

    ret->handler.startElement = sixtp_sax_start_handler;
    ret->handler.endElement = sixtp_sax_end_handler;
    ret->handler.characters = sixtp_sax_characters_handler;
    ret->handler.getEntity = sixtp_sax_get_entity_handler;

    ret->data.parsing_ok = TRUE;
    ret->data.stack = NULL;
    ret->data.global_data = global_data;

    ret->top_frame = sixtp_stack_frame_new(initial_parser, NULL);
    
    ret->top_frame_data = top_level_data;

    ret->data.stack = g_slist_prepend(ret->data.stack,
                                      (gpointer) ret->top_frame);

    if(initial_parser->start_handler)
    {
        if(!initial_parser->start_handler(NULL,
                                          &ret->top_frame_data,
                                          &ret->data.global_data,
                                          &ret->top_frame->data_for_children,
                                          &ret->top_frame->frame_data,
                                          NULL, NULL))
        {
            sixtp_handle_catastrophe(&ret->data);
            sixtp_context_destroy(ret);
            return NULL;
        }
    }
    
    return ret;
}

void
sixtp_context_run_end_handler(sixtp_parser_context* ctxt)
{
    if(ctxt->top_frame->parser->end_handler)
    {
        ctxt->data.parsing_ok &=
        ctxt->top_frame->parser->end_handler(
            ctxt->top_frame->data_for_children,
            ctxt->top_frame->data_from_children,
            NULL,
            ctxt->top_frame_data,
            ctxt->data.global_data,
            &ctxt->top_frame->frame_data,
            NULL);
    }
}

void
sixtp_context_destroy(sixtp_parser_context* context)
{
    sixtp_stack_frame_destroy(context->top_frame);
    g_slist_free(context->data.stack);
    context->data.saxParserCtxt->userData = NULL;
    context->data.saxParserCtxt->sax = NULL;    
    xmlFreeParserCtxt(context->data.saxParserCtxt);
    context->data.saxParserCtxt = NULL;
    g_free(context);
}
