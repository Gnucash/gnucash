#include <glib.h>

#include <stdarg.h>

#include "sixtp.h"
#include "sixtp-stack.h"

#include "gnc-engine-util.h"


static short module = MOD_IO;

/************************************************************************/
gboolean
is_child_result_from_node_named(sixtp_child_result *cr, const char *tag) {
  return((cr->type == SIXTP_CHILD_RESULT_NODE)
         &&
         (safe_strcmp(cr->tag, tag) == 0));
}

void
sixtp_child_free_data(sixtp_child_result *result) {
  if(result->data) g_free(result->data);
}


void
sixtp_child_result_destroy(sixtp_child_result *r) {
  if(r->should_cleanup && r->cleanup_handler) {
    r->cleanup_handler(r);
  }
  if(r->type == SIXTP_CHILD_RESULT_NODE) g_free(r->tag);
  g_free(r);
}

void
sixtp_child_result_print(sixtp_child_result *cr, FILE *f) {
  fprintf(f, "((tag %s) (data %p))", cr->tag, cr->data);
}

/************************************************************************/


void
sixtp_set_start(sixtp *parser, sixtp_start_handler start_handler) {
  parser->start_handler = start_handler;
}

void
sixtp_set_before_child(sixtp *parser, sixtp_before_child_handler handler) {
  parser->before_child = handler;
}

void
sixtp_set_after_child(sixtp *parser, sixtp_after_child_handler handler) {
  parser->after_child = handler;
}

void
sixtp_set_end(sixtp *parser, sixtp_end_handler end_handler) {
  parser->end_handler = end_handler;
}

void
sixtp_set_chars(sixtp *parser, sixtp_characters_handler char_handler) {
  parser->characters_handler = char_handler;
}

void
sixtp_set_cleanup_result(sixtp *parser,
                         sixtp_result_handler handler) {
  parser->cleanup_result = handler;
}

void
sixtp_set_cleanup_chars(sixtp *parser,
                        sixtp_result_handler handler) {
  parser->cleanup_chars = handler;
}

void
sixtp_set_fail(sixtp *parser,
               sixtp_fail_handler handler) {
  parser->fail_handler = handler;
}

void
sixtp_set_result_fail(sixtp *parser,
                      sixtp_result_handler handler) {
  parser->result_fail_handler = handler;
}

void
sixtp_set_chars_fail(sixtp *parser,
                     sixtp_result_handler handler) {
  parser->chars_fail_handler = handler;
}

sixtp *
sixtp_new(void) {
  sixtp *s = g_new0(sixtp, 1);

  if(s) {
    s->children = g_hash_table_new(g_str_hash, g_str_equal);
    if(!s->children) {
      g_free(s);
      s = NULL;
    }
  }
  return(s);
}

sixtp*
sixtp_set_any(sixtp *tochange, int cleanup, ...)
{
    va_list ap;
    sixtp_handler_type type;
    
    va_start(ap, cleanup);
    
    if(!tochange)
    {
        g_warning("Null tochange passed\n");
        return NULL;
    }

    do 
    {
        type = va_arg(ap, sixtp_handler_type);

        switch(type)
        {
        case SIXTP_NO_MORE_HANDLERS:
            va_end(ap);
            return tochange;

        case SIXTP_START_HANDLER_ID:
            sixtp_set_start(tochange, va_arg(ap, sixtp_start_handler));
            break;
            
        case SIXTP_BEFORE_CHILD_HANDLER_ID:
            sixtp_set_before_child(tochange,
                                   va_arg(ap, sixtp_before_child_handler));
            break;
            
        case SIXTP_AFTER_CHILD_HANDLER_ID:
            sixtp_set_after_child(tochange,
                                  va_arg(ap, sixtp_after_child_handler));
            break;
            
        case SIXTP_END_HANDLER_ID:
            sixtp_set_end(tochange, va_arg(ap, sixtp_end_handler));
            break;
            
        case SIXTP_CHARACTERS_HANDLER_ID:
            sixtp_set_chars(tochange, va_arg(ap, sixtp_characters_handler));
            break;
            
        case SIXTP_FAIL_HANDLER_ID:
            sixtp_set_fail(tochange, va_arg(ap, sixtp_fail_handler));
            break;
            
        case SIXTP_CLEANUP_RESULT_ID:
            sixtp_set_cleanup_result(tochange,
                                     va_arg(ap, sixtp_result_handler));
            break;
            
        case SIXTP_CLEANUP_CHARS_ID:
            sixtp_set_cleanup_chars(tochange,
                                    va_arg(ap, sixtp_result_handler));
            break;
            
        case SIXTP_RESULT_FAIL_ID:
            sixtp_set_result_fail(tochange, va_arg(ap, sixtp_result_handler));
            break;
            
        case SIXTP_CHARS_FAIL_ID:
            sixtp_set_chars_fail(tochange, va_arg(ap, sixtp_result_handler));
            break;

        default:
            va_end(ap);
            g_warning("Bogus sixtp type %d\n", type);
            if(cleanup)
            {
                sixtp_destroy(tochange);
            }
            return NULL;
        }
    } while(1);

    va_end(ap);
    return tochange;
}

sixtp*
sixtp_new_full(sixtp_start_handler starter,
               sixtp_before_child_handler cdbeforer,
               sixtp_after_child_handler chafterer,
               sixtp_end_handler ender,
               sixtp_characters_handler charer,
               sixtp_fail_handler failer,
               sixtp_result_handler cleanresulter,
               sixtp_result_handler cleancharer,
               sixtp_result_handler resultfailer,
               sixtp_result_handler charsfailer)
{
    sixtp *ret = sixtp_new();
    g_return_val_if_fail(ret, NULL);
    
    sixtp_set_start(ret, starter);
    sixtp_set_before_child(ret, cdbeforer);
    sixtp_set_after_child(ret, chafterer);
    sixtp_set_end(ret, ender);
    sixtp_set_chars(ret, charer);
    sixtp_set_fail(ret, failer);
    sixtp_set_cleanup_result(ret, cleanresulter);
    sixtp_set_cleanup_chars(ret, cleancharer);
    sixtp_set_result_fail(ret, resultfailer);
    sixtp_set_chars_fail(ret, charsfailer);

    return ret;
}
    
static void sixtp_destroy_child(gpointer key, gpointer value,
                                gpointer user_data);

static void
sixtp_destroy_node(sixtp *sp, GHashTable *corpses) {
  g_return_if_fail(sp);
  g_return_if_fail(corpses);
  g_hash_table_foreach(sp->children, sixtp_destroy_child, corpses);
  g_hash_table_destroy(sp->children);
  g_free(sp);
}

static void
sixtp_destroy_child(gpointer key, gpointer value, gpointer user_data) {
  GHashTable *corpses = (GHashTable *) user_data;
  sixtp *child = (sixtp *) value;
  gpointer lookup_key;
  gpointer lookup_value;

  PINFO ("Killing sixtp child under key <%s>", (char *) key); 
  g_free(key);

  if(!corpses) {
    PERR("no corpses in sixtp_destroy_child <%s>\n", (char *) key);
    return;
  }
  if(!child) {
    PERR("no child in sixtp_destroy_child <%s>\n", (char *) key);
    return;
  }

  if(!g_hash_table_lookup_extended(corpses, (gconstpointer) child,
                                   &lookup_key, &lookup_value)) {
    /* haven't killed this one yet. */
    g_hash_table_insert(corpses, child, (gpointer) 1);
    sixtp_destroy_node(child, corpses);
  }
}

void
sixtp_destroy(sixtp *sp) {
  GHashTable *corpses;
  g_return_if_fail(sp);
  corpses = g_hash_table_new(g_direct_hash, g_direct_equal);
  sixtp_destroy_node(sp, corpses);
  g_hash_table_destroy(corpses);
}


/***********************************************************************/

gboolean
sixtp_add_sub_parser(sixtp *parser, const gchar* tag, sixtp *sub_parser) {
  g_return_val_if_fail(parser, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(sub_parser, FALSE);

  g_hash_table_insert(parser->children, g_strdup(tag), (gpointer) sub_parser);
  return(TRUE);
}

/*
 * This is a bit complex because of having to make sure to
 * cleanup things we haven't looked at on an error condition
 */
sixtp*
sixtp_add_some_sub_parsers(sixtp *tochange, int cleanup, ...)
{
    int have_error;
    va_list ap;
    char *tag;
    sixtp *handler;

    va_start(ap, cleanup);

    have_error = 0;
    
    if(!tochange)
    {
        have_error = 1;
    }

    do
    {
        tag = va_arg(ap, char*);
        if(!tag)
        {
            break;
        }

        handler = va_arg(ap, sixtp*);
        if(!handler)
        {
            g_warning("Handler for tag %s is null\n", tag);
            
            if(cleanup)
            {
                sixtp_destroy(tochange);
                tochange = NULL;
                have_error = 1;
            }
            else
            {
                va_end(ap);
                return NULL;
            }
        }

        if (have_error)
        {
            sixtp_destroy(handler);
        }
        else
        {
            sixtp_add_sub_parser(tochange, tag, handler);
        }
    } while (1);

    va_end(ap);
    return tochange;
}

/************************************************************************/

void
sixtp_sax_start_handler(void *user_data,
                        const xmlChar *name,
                        const xmlChar **attrs) {
  sixtp_sax_data *pdata = (sixtp_sax_data *) user_data;
  sixtp_stack_frame *current_frame = NULL;
  sixtp *current_parser = NULL;
  sixtp *next_parser = NULL;
  gchar *next_parser_tag = NULL;
  gboolean lookup_success = FALSE;
  sixtp_stack_frame *new_frame = NULL;
  
  g_return_if_fail(pdata->parsing_ok);

  current_frame = (sixtp_stack_frame *) pdata->stack->data;
  current_parser = current_frame->parser;

  /* Use an extended lookup so we can get *our* copy of the key.
     Since we've strduped it, we know its lifetime... */
  lookup_success = g_hash_table_lookup_extended(current_parser->children,
                                                name,
                                                (gpointer) &next_parser_tag,
                                                (gpointer) &next_parser);
  
  if(!lookup_success) {
    PERR("Tag <%s> not allowed in current context.\n", name);
    pdata->parsing_ok = FALSE;
    return;
  }

  if(current_frame->parser->before_child) {
    GSList *parent_data_from_children = NULL;
    gpointer parent_data_for_children = NULL;

    if(g_slist_length(pdata->stack) > 1) {
      /* we're not in the top level node */
      sixtp_stack_frame *parent_frame =
        (sixtp_stack_frame *) pdata->stack->next->data;
      parent_data_from_children = parent_frame->data_from_children;
      parent_data_from_children = parent_frame->data_for_children;
    }

    pdata->parsing_ok =
      current_frame->parser->before_child(current_frame->data_for_children,
                                          current_frame->data_from_children,
                                          parent_data_from_children,
                                          parent_data_for_children,
                                          pdata->global_data,
                                          &(current_frame->frame_data),
                                          current_frame->tag,
                                          next_parser_tag);
  }

  g_return_if_fail(pdata->parsing_ok);

  /* now allocate the new stack frame and shift to it */
  new_frame = g_new0(sixtp_stack_frame, 1);
  new_frame->parser = next_parser;
  new_frame->tag = next_parser_tag;
  new_frame->data_for_children = NULL;
  new_frame->data_from_children = NULL;
  new_frame->frame_data = NULL;
  
  /*printf("PUSHING FRAME for <%s> parser.\n", next_parser_tag);
    sixtp_stack_frame_print(new_frame, 2, stderr); */
  pdata->stack = g_slist_prepend(pdata->stack, (gpointer) new_frame);
  /*printf("RESULTANT STACK: ");
    sixtp_print_frame_stack(pdata->stack, stderr); */
  
  if(next_parser->start_handler) {
    pdata->parsing_ok =
      next_parser->start_handler(current_frame->data_from_children,
                                 current_frame->data_for_children,
                                 pdata->global_data,
                                 &new_frame->data_for_children,
                                 &new_frame->frame_data,
                                 next_parser_tag,
                                 (gchar**)attrs);
  }
}

void
sixtp_sax_characters_handler(void *user_data, const xmlChar *text, int len) {
  sixtp_sax_data *pdata = (sixtp_sax_data *) user_data;
  sixtp_stack_frame *frame;

  g_return_if_fail(pdata->parsing_ok);

  frame = (sixtp_stack_frame *) pdata->stack->data;
  if(frame->parser->characters_handler) {
    gpointer result = NULL;

    pdata->parsing_ok =
      frame->parser->characters_handler(frame->data_from_children,
                                        frame->data_for_children,
                                        pdata->global_data,
                                        &result,
                                        text,
                                        len);
    if(pdata->parsing_ok) {
      if(result) {
        /* push the result onto the current "child" list. */
        sixtp_child_result *child_data = g_new0(sixtp_child_result, 1);
        
        child_data->type = SIXTP_CHILD_RESULT_CHARS;
        child_data->tag = NULL;
        child_data->data = result;
        child_data->should_cleanup = TRUE;
        child_data->cleanup_handler = frame->parser->cleanup_chars;
        child_data->fail_handler = frame->parser->chars_fail_handler;
        frame->data_from_children = g_slist_prepend(frame->data_from_children,
                                                    child_data);
      }
    }
  }
}

void
sixtp_sax_end_handler(void *user_data, const xmlChar *name) {
  sixtp_sax_data *pdata = (sixtp_sax_data *) user_data;
  sixtp_stack_frame *current_frame;
  sixtp_stack_frame *parent_frame;
  sixtp_child_result *child_result_data = NULL;
  gchar *end_tag = NULL;

  g_return_if_fail(pdata->parsing_ok);

  current_frame = (sixtp_stack_frame *) pdata->stack->data;
  parent_frame = (sixtp_stack_frame *) pdata->stack->next->data;

  /* time to make sure we got the right closing tag.  Is this really
     necessary? */
  if(safe_strcmp(current_frame->tag, name) != 0) {
    pdata->parsing_ok = FALSE;
    return;
  }
  
  /* tag's OK, proceed. */
  if(current_frame->parser->end_handler) {
    pdata->parsing_ok = 
      current_frame->parser->end_handler(current_frame->data_for_children,
                                         current_frame->data_from_children,
                                         parent_frame->data_from_children,
                                         parent_frame->data_for_children,
                                         pdata->global_data,
                                         &current_frame->frame_data,
                                         current_frame->tag);
  }

  g_return_if_fail(pdata->parsing_ok);

  if(current_frame->frame_data) {
    /* push the result onto the parent's child result list. */
    child_result_data = g_new(sixtp_child_result, 1);
    
    child_result_data->type = SIXTP_CHILD_RESULT_NODE;
    child_result_data->tag = g_strdup(current_frame->tag);
    child_result_data->data = current_frame->frame_data;
    child_result_data->should_cleanup = TRUE;
    child_result_data->cleanup_handler = current_frame->parser->cleanup_result;
    child_result_data->fail_handler =
      current_frame->parser->result_fail_handler;
    parent_frame->data_from_children =
      g_slist_prepend(parent_frame->data_from_children, child_result_data);
  }

  /* grab it before it goes away - we shouldn't need to g_strdup
     because this string is held by the parent parser's hash table. */
  end_tag = current_frame->tag;

  PINFO("Finished with end of <%s>", end_tag);

  /*sixtp_print_frame_stack(pdata->stack, stderr);*/

  pdata->stack = sixtp_pop_and_destroy_frame(pdata->stack);

  /* reset pointer after stack pop */
  current_frame = (sixtp_stack_frame *) pdata->stack->data;
  /* reset the parent, checking to see if we're at the top level node */
  parent_frame = (sixtp_stack_frame *) 
    ((g_slist_length(pdata->stack) > 1) ? (pdata->stack->next->data) : NULL);

  if(current_frame->parser->after_child) {      
    /* reset pointer after stack pop */
    GSList *parent_data_from_children = NULL;
    gpointer parent_data_for_children = NULL;

    if(parent_frame) {
      /* we're not in the top level node */
      sixtp_stack_frame *parent_frame =
        (sixtp_stack_frame *) pdata->stack->next->data;
      parent_data_from_children = parent_frame->data_from_children;
      parent_data_from_children = parent_frame->data_for_children;
    }

    pdata->parsing_ok =
      current_frame->parser->after_child(current_frame->data_for_children,
                                         current_frame->data_from_children,
                                         parent_data_from_children,
                                         parent_data_for_children,
                                         pdata->global_data,
                                         &(current_frame->frame_data),
                                         current_frame->tag,
                                         end_tag,
                                         child_result_data);
  }
}
