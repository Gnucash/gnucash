/*
 * io-gncxml-r.c -- read XML-format gnucash data file
 */

#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#  include <time.h>
#else
#  include <time.h>
#endif

#include <glib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <tree.h>
#include <parser.h>
#include <xmlmemory.h>
#include <parserInternals.h>

#include "Account.h"
#include "date.h"
#include "DateUtils.h"
#include "Group.h"
#include "messages.h"
#include "Transaction.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"

#include "io-gncxml.h"

#include "AccountP.h"
#include "TransactionP.h"

/* Hack to get going... */
#include "FileIOP.h"
#include <guile/gh.h>


/* TODO

   create common funcs for repeated "types" of parsers.  i.e. a common
   func for handling guid, gnc_numeric, etc. parsers - just pass in
   string->data and data->string funcs.

   change sixtp_child_result to sixtp_result?

   add generic accumulate chars node constructor - takes end_handler
   as argument - same as for timespec parser...

   document that right now parsing is *extremely* anal - no whitespace
   surrounding data.

   need to add a way to propagate error data upward...

   do we need a way to pass an object up through the tree along with
   its lower level destructor?

   CHECK TO SEE IF WE ALWAYS DTRT IN THE END TAG HANDLER - IE DO WE
   CLEANUP PROPERLY SINCE AT THAT POINT THE FRAME CLEANUP HANDLER WILL
   **NOT** BE CALLED.

   CHECK TO SEE THAT WE'RE SETTING SHOULD CLEANUP TO FALSE EVERYWHERE
   WE SHOULD (i.e. when we use the result)!

   DAMN, why didn't I think of this before.  I should have added a
   parent "after-child" hook.  I think that might have greatly
   simplified some of the nodes...

   Do we also want "before/after" chars handlers?

*/


/* result for a characters handler is shared across all character
   handlers for a given node.  result for start/end pair is shared as
   well. 

   Cleaning up the results returned by children and characters
   handlers is the user's responsibility.

   results have to have cleanup pointers for exceptional failures

   stack frames also have to have cleanup funcs for exceptional
   failures after the start tag handler has been called, but before
   the end tag handler has been called.  If the end tag handler is
   called, but returns FALSE, it is expected that the end tag handler
   has taken care of any cleanup itself.

   result cleanup functions are called for a node's children just
   after the end handler unless should_cleanup has been set to FALSE,
   or unless there's a failure.  If there's a failure, then the
   cleanup is left to the failure handler.


*/

typedef struct _sixtp_child_result sixtp_child_result;

typedef gboolean (*sixtp_start_handler)(GSList* sibling_data,
                                        gpointer parent_data,
                                        gpointer global_data,
                                        gpointer *data_for_children,
                                        gpointer *result,

                                        const gchar *tag);

typedef gboolean (*sixtp_before_child_handler)(gpointer data_for_children,
                                               GSList* data_from_children,
                                               GSList* sibling_data,
                                               gpointer parent_data,
                                               gpointer global_data,
                                               gpointer *result,
                                               
                                               const gchar *tag,
                                               const gchar *child_tag);

typedef gboolean (*sixtp_after_child_handler)(gpointer data_for_children,
                                              GSList* data_from_children,
                                              GSList* sibling_data,
                                              gpointer parent_data,
                                              gpointer global_data,
                                              gpointer *result,
                                              
                                              const gchar *tag,
                                              const gchar *child_tag,
                                              sixtp_child_result *child_result);

typedef gboolean (*sixtp_end_handler)(gpointer data_for_children,
                                      GSList* data_from_children,
                                      GSList* sibling_data,
                                      gpointer parent_data,
                                      gpointer global_data,
                                      gpointer *result,

                                      const gchar *tag);

typedef gboolean (*sixtp_characters_handler)(GSList *sibling_data,
                                             gpointer parent_data,
                                             gpointer global_data,
                                             gpointer *result,

                                             const char *text,
                                             int length);

typedef void (*sixtp_result_handler)(sixtp_child_result *result);

typedef void (*sixtp_fail_handler)(gpointer data_for_children,
                                   GSList* data_from_children,
                                   GSList* sibling_data,
                                   gpointer parent_data,
                                   gpointer global_data,
                                   gpointer *result,
                                   const gchar *tag);

typedef struct sixtp {
  /* If you change this, don't forget to modify all the copy/etc. functions */
  sixtp_start_handler start_handler;
  sixtp_before_child_handler before_child;
  sixtp_after_child_handler after_child;
  sixtp_end_handler end_handler;
  sixtp_characters_handler characters_handler;

  sixtp_fail_handler fail_handler; 
  /* called for failures before the close tag */

  sixtp_result_handler cleanup_result; /* called unless failure */
  sixtp_result_handler cleanup_chars; /* called unless failure */

  sixtp_result_handler result_fail_handler;
  /* called to cleanup results from this node on failure */

  sixtp_result_handler chars_fail_handler;
  /* called to cleanup character results when cleaning up this node's
     children. */

  GHashTable *children;
} sixtp;

typedef enum {
  SIXTP_CHILD_RESULT_CHARS,
  SIXTP_CHILD_RESULT_NODE
} sixtp_child_result_type;

struct _sixtp_child_result {
  sixtp_child_result_type type;
  gchar *tag; /* NULL for a CHARS node. */
  gpointer data;
  gboolean should_cleanup;
  sixtp_result_handler cleanup_handler;
  sixtp_result_handler fail_handler;
};

typedef struct sixtp_sax_data {
  gboolean parsing_ok;
  GSList *stack;
  gpointer global_data;
} sixtp_sax_data;

typedef struct sixtp_stack_frame {
  sixtp *parser;
  gchar *tag;
  gpointer data_for_children;
  GSList *data_from_children; /* in reverse chronological order */
  gpointer frame_data;
} sixtp_stack_frame;

static gboolean
is_child_result_from_node_named(sixtp_child_result *cr, const char *tag) {
  return((cr->type == SIXTP_CHILD_RESULT_NODE)
         &&
         (safe_strcmp(cr->tag, tag) == 0));
}

static void
generic_free_result(sixtp_child_result *result) {
  if(result->data) g_free(result->data);
}

static void
sixtp_child_result_destroy(sixtp_child_result *r) {
  if(r->should_cleanup && r->cleanup_handler) {
    r->cleanup_handler(r);
  }
  if(r->type == SIXTP_CHILD_RESULT_NODE) g_free(r->tag);
  g_free(r);
}

static void
sixtp_stack_frame_destroy(sixtp_stack_frame *sf) {
  GSList *lp;

  /* cleanup all the child data */
  for(lp = sf->data_from_children; lp; lp = lp->next) {
    sixtp_child_result_destroy((sixtp_child_result *) lp->data);
  }
  g_slist_free(sf->data_from_children);
  sf->data_from_children = NULL;

  g_free(sf);
}

static GSList*
sixtp_pop_and_destroy_frame(GSList *frame_stack) {
  sixtp_stack_frame *dead_frame = (sixtp_stack_frame *) frame_stack->data;
  GSList* result;

  result = g_slist_next(frame_stack);
  sixtp_stack_frame_destroy(dead_frame);
  g_slist_free_1(frame_stack);
  return(result);
}

static void
sixtp_child_result_print(sixtp_child_result *cr, FILE *f) {
  fprintf(f, "((tag %s) (data %p))", cr->tag, cr->data);
}

static void
sixtp_stack_frame_print(sixtp_stack_frame *sf, gint indent, FILE *f) {
  gchar *is = g_strnfill(indent, ' ');

  fprintf(f, "%s(stack-frame %p\n", is, sf);
  fprintf(f, "%s             (parser %p)\n", is, sf->parser);
  fprintf(f, "%s             (tag %s)\n", is, sf->tag);
  fprintf(f, "%s             (data-for-children %p)\n", is, sf->data_for_children);

  {
    GSList *lp;
    fprintf(f, "%s             (data-from-children", is);
    for(lp = sf->data_from_children; lp; lp = lp->next) {
      fputc(' ', f);
      sixtp_child_result_print((sixtp_child_result *) lp->data, f);
    }
    fprintf(f, ")\n");
  }

  fprintf(f, "%s             (frame-data %p))\n", is, sf->frame_data);
  fflush(f);
}

static void
sixtp_print_frame_stack(GSList *stack, FILE *f) {
  /* first, some debugging output */
  GSList *printcopy = g_slist_reverse(g_slist_copy(stack));
  GSList *lp;
  int indent = 0;
  
  for(lp = printcopy; lp; lp = lp->next) {
    sixtp_stack_frame *frame = (sixtp_stack_frame *) lp->data;
    sixtp_stack_frame_print(frame, indent, f);
    indent += 2;
  }

}

static xmlEntityPtr
sixtp_sax_get_entity_handler(void *user_data, const CHAR *name) {
  return xmlGetPredefinedEntity(name);
}

static void
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
    fprintf(stderr, "Tag <%s> not allowed in current context.\n", name);
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
                                 next_parser_tag);
  }
}

static void
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

static void
sixtp_sax_end_handler(void *user_data, const xmlChar *name) {
  sixtp_sax_data *pdata = (sixtp_sax_data *) user_data;
  sixtp_stack_frame *current_frame;
  sixtp_stack_frame *parent_frame;
  sixtp_child_result *child_result_data = NULL;
  gchar *end_tag = NULL;

  g_return_if_fail(pdata->parsing_ok);

  current_frame = (sixtp_stack_frame *) pdata->stack->data;
  parent_frame = (sixtp_stack_frame *) pdata->stack->next->data;

  /* time to make sure we got the right closing tag */
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

  /*fprintf(stderr, "Finished with end of <%s>\n", end_tag);*/

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

static sixtp *
sixtp_new() {
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

static void sixtp_destroy_node(sixtp *sp, GHashTable *corpses);

static void
sixtp_destroy_child(gpointer key, gpointer value, gpointer user_data) {
  GHashTable *corpses = (GHashTable *) user_data;
  sixtp *child = (sixtp *) value;
  gpointer lookup_key;
  gpointer lookup_value;

  /* fprintf(stderr, "Killing sixtp child under key <%s>\n", (char *) key); */
  g_free(key);

  if(!corpses) {
    fprintf(stderr, "BAD: no corpses in sixtp_destroy_child <%s>\n",
            (char *) key);
    return;
  }
  if(!child) {
    fprintf(stderr, "BAD: no child in sixtp_destroy_child <%s>\n", (char *) key);
    return;
  }

  if(!g_hash_table_lookup_extended(corpses, (gconstpointer) child,
                                   &lookup_key, &lookup_value)) {
    /* haven't killed this one yet. */
    g_hash_table_insert(corpses, child, (gpointer) 1);
    sixtp_destroy_node(child, corpses);
  }
}

static void
sixtp_destroy_node(sixtp *sp, GHashTable *corpses) {
  g_return_if_fail(sp);
  g_return_if_fail(corpses);
  g_hash_table_foreach(sp->children, sixtp_destroy_child, corpses);
  g_hash_table_destroy(sp->children);
  g_free(sp);
}

static void
sixtp_destroy(sixtp *sp) {
  GHashTable *corpses;
  g_return_if_fail(sp);
  corpses = g_hash_table_new(g_direct_hash, g_direct_equal);
  sixtp_destroy_node(sp, corpses);
  g_hash_table_destroy(corpses);
}

static void
sixtp_set_start(sixtp *parser, sixtp_start_handler start_handler) {
  parser->start_handler = start_handler;
}

static void
sixtp_set_before_child(sixtp *parser, sixtp_before_child_handler handler) {
  parser->before_child = handler;
}

static void
sixtp_set_after_child(sixtp *parser, sixtp_after_child_handler handler) {
  parser->after_child = handler;
}

static void
sixtp_set_end(sixtp *parser, sixtp_end_handler end_handler) {
  parser->end_handler = end_handler;
}

static void
sixtp_set_chars(sixtp *parser, sixtp_characters_handler char_handler) {
  parser->characters_handler = char_handler;
}

static void
sixtp_set_cleanup_result(sixtp *parser,
                         sixtp_result_handler handler) {
  parser->cleanup_result = handler;
}

static void
sixtp_set_cleanup_chars(sixtp *parser,
                        sixtp_result_handler handler) {
  parser->cleanup_chars = handler;
}

static void
sixtp_set_fail(sixtp *parser,
               sixtp_fail_handler handler) {
  parser->fail_handler = handler;
}

static void
sixtp_set_result_fail(sixtp *parser,
                      sixtp_result_handler handler) {
  parser->result_fail_handler = handler;
}

static void
sixtp_set_chars_fail(sixtp *parser,
                     sixtp_result_handler handler) {
  parser->chars_fail_handler = handler;
}



static gboolean
sixtp_add_sub_parser(sixtp *parser, const gchar* tag, sixtp *sub_parser) {
  g_return_val_if_fail(parser, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(sub_parser, FALSE);

  g_hash_table_insert(parser->children, g_strdup(tag), (gpointer) sub_parser);
  return(TRUE);
}

static void
sixtp_handle_catastrophe(sixtp_sax_data *sax_data) {
  /* Something has gone wrong.  To handle it, we have to traverse the
     stack, calling, at each level, the frame failure handler (the
     handler for the current, unfinished block) and then the sibling
     handlers.  The order is reverse chronological - oldest child
     results cleaned up last.  This holds overall as well, stack
     frames are cleaned up in their order on the stack which will be
     youngest to oldest.  */

  GSList *lp;
  GSList **stack = &(sax_data->stack);

  fprintf(stderr, "sixtp: parse failed at \n");
  sixtp_print_frame_stack(sax_data->stack, stderr);

  while(*stack) {
    sixtp_stack_frame *current_frame = (sixtp_stack_frame *) (*stack)->data;
    sixtp_fail_handler fail_handler = current_frame->parser->fail_handler;

    /* cleanup the current frame */
    if(fail_handler) {
      GSList *sibling_data;
      gpointer parent_data;

      if((*stack)->next == NULL) {
        /* This is the top of the stack... */
        parent_data = NULL;
        sibling_data = NULL; 
      } else {
        sixtp_stack_frame *parent_frame =
          (sixtp_stack_frame *) (*stack)->next->data;
        parent_data = parent_frame->data_for_children;
        sibling_data = parent_frame->data_from_children;
      }

      fail_handler(current_frame->data_for_children,
                   current_frame->data_from_children,
                   sibling_data,
                   parent_data,
                   sax_data->global_data,
                   &current_frame->frame_data,
                   current_frame->tag);
    }

    /* now cleanup any children's results */
    for(lp = current_frame->data_from_children; lp; lp = lp->next) {
      sixtp_child_result *cresult = (sixtp_child_result *) lp->data;
      if(cresult->fail_handler) cresult->fail_handler(cresult);
    }

    *stack = sixtp_pop_and_destroy_frame(*stack);
  }
}



static gboolean
sixtp_parse_file(sixtp *sixtp,
                 const char *filename,
                 gpointer data_for_top_level,
                 gpointer global_data,
                 gpointer *parse_result) {
  xmlSAXHandler sax_handler;
  sixtp_sax_data sax_data;
  int sax_result;
  sixtp_stack_frame *top_frame = NULL;
  
  memset(&sax_handler, '\0', sizeof(sax_handler));
  sax_handler.startElement = sixtp_sax_start_handler;
  sax_handler.endElement = sixtp_sax_end_handler;
  sax_handler.characters = sixtp_sax_characters_handler;
  sax_handler.getEntity = sixtp_sax_get_entity_handler;
  
  memset(&sax_data, '\0', sizeof(sixtp_sax_data));
  sax_data.parsing_ok = TRUE;
  sax_data.stack = NULL;
  sax_data.global_data = global_data;
  
  top_frame = g_new0(sixtp_stack_frame, 1);
  top_frame->parser = sixtp;
  top_frame->tag = NULL;
  top_frame->data_from_children = NULL;
  top_frame->data_for_children = NULL;
  top_frame->frame_data = NULL;
  
  sax_data.stack = g_slist_prepend(sax_data.stack, (gpointer) top_frame);
  
  if(sixtp->start_handler) {
    sax_data.parsing_ok =
      sixtp->start_handler(NULL,
                           data_for_top_level,
                           sax_data.global_data,
                           &top_frame->data_for_children,
                           &top_frame->frame_data,
                           NULL);
  }
  
  if(!sax_data.parsing_ok)  {
    sixtp_handle_catastrophe(&sax_data);
    return(FALSE);
  }
  
  sax_result = xmlSAXUserParseFile(&sax_handler, &sax_data, filename);
  
  if(!sax_data.parsing_ok) {
    sixtp_handle_catastrophe(&sax_data);
    return(FALSE);
  }
  
  if(sixtp->end_handler) {
    sax_data.parsing_ok =
      sixtp->end_handler(top_frame->data_for_children,
                         top_frame->data_from_children,
                         NULL,
                         data_for_top_level,
                         sax_data.global_data,
                         &top_frame->frame_data,
                         NULL);
  }
  
  if(!sax_data.parsing_ok) {
    sixtp_handle_catastrophe(&sax_data);
    return(FALSE);
  }
  
  /* put the result where the caller can see it */
  if(top_frame->frame_data) *parse_result = top_frame->frame_data;
  
  {
    GSList *lp;
    for(lp = sax_data.stack; lp; lp = lp->next)
      sixtp_stack_frame_destroy((sixtp_stack_frame *) lp->data);
  }
  g_slist_free(sax_data.stack);
  return(TRUE);
}


typedef enum {
  GNC_PARSE_ERR_NONE,
  GNC_PARSE_ERR_BAD_VERSION,
} GNCParseErr;

typedef struct {
  /* have we gotten the file version yet? */
  gboolean seen_version;
  gint64 version;

  /* top level <gnc-data> parser - we need this so we can set it up
     after we see the file version. */
  sixtp *gnc_parser;

  /* The account group */
  AccountGroup *account_group;
  
  GNCParseErr error;
} GNCParseStatus;

static gboolean
isspace_str(const gchar *str, int nomorethan) {
  const gchar *cursor = str;
  while(*cursor && (nomorethan != 0)) {
    if(!isspace(*cursor)) {
      return(FALSE);
    }
    cursor++;
    nomorethan--;
  }
  return(TRUE);
}

static gboolean
allow_and_ignore_only_whitespace(GSList *sibling_data,
                                 gpointer parent_data,
                                 gpointer global_data,
                                 gpointer *result,
                                 const char *text,
                                 int length) {
  return(isspace_str(text, length));
}

static gboolean
generic_accumulate_chars(GSList *sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         
                         const char *text,
                         int length) {
  gchar *copytxt = g_strndup(text, length);
  g_return_val_if_fail(result, FALSE);

  *result = copytxt;
  return(TRUE);
}


static void
generic_free_data_for_children(gpointer data_for_children,
                               GSList* data_from_children,
                               GSList* sibling_data,
                               gpointer parent_data,
                               gpointer global_data,
                               gpointer *result,
                               const gchar *tag) {
  if(data_for_children) g_free(data_for_children);
}

static gchar *
concatenate_child_result_chars(GSList *data_from_children) {
  GSList *lp;
  gchar *name = g_strdup("");

  g_return_val_if_fail(name, NULL);

  /* child data lists are in reverse chron order */
  data_from_children = g_slist_reverse(g_slist_copy(data_from_children));

  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;
    if(cr->type != SIXTP_CHILD_RESULT_CHARS) {
      g_slist_free (data_from_children);
      g_free(name);
      return(NULL);
    } else {
      char *temp;
      temp = g_strconcat(name, (gchar *) cr->data, NULL);
      g_free (name);
      name = temp;
    }
  }
  g_slist_free (data_from_children);
  return(name);
}

/****************************************************************************/
/* string to data converters...
 */


/*********/
/* double
   
   We have to use guile because AFAICT, libc, and C in general isn't
   smart enough to actually parse it's own output, especially not
   portably (big surprise).

 */

static gboolean
string_to_double(const char *str, double *result) {
  /* FIXME: NOT THREAD SAFE - USES STATIC DATA */
  static SCM string_to_number;
  static gboolean ready = FALSE;

  SCM conversion_result;
  
  g_return_val_if_fail(str, FALSE);
  g_return_val_if_fail(result, FALSE);

  if(!ready) {
    string_to_number = gh_eval_str("string->number");
    scm_protect_object(string_to_number);
    ready = TRUE;
  }
  
  conversion_result = gh_call1(string_to_number, gh_str02scm(str));
  if(!conversion_result == SCM_BOOL_F) {
    return(FALSE);
  }

  *result = gh_scm2double(conversion_result);
  return(TRUE);
}

/*********/
/* gint64
 */
   
static gboolean
string_to_gint64(const gchar *str, gint64 *v) {
  /* convert a string to a gint64.  only whitespace allowed before and after. */
  int num_read;

  /* must use "<" here because %n's effects aren't well defined */
  if(sscanf(str, " %lld %n", v, &num_read) < 1) {
    return(FALSE);
  }

  if(!isspace_str(str + num_read, -1)) return(FALSE);
  return(TRUE);
}

/************/
/* hex string
 */

static gboolean
hex_string_to_binary(const gchar *str,  void **v, guint64 *data_len) {
  /* Convert a hex string to binary.  No whitespace allowed. */
  const gchar *cursor = str;
  guint64 str_len;
  gboolean error = FALSE;
  
  g_return_val_if_fail(str, FALSE);
  g_return_val_if_fail(v, FALSE);
  g_return_val_if_fail(data_len, FALSE);

  str_len = strlen(str);
  /* Since no whitespace is allowed and hex encoding is 2 text chars
     per binary char, the result must be half the input size and the
     input size must be even. */
  if((str_len % 2) != 0) return(FALSE);
  *data_len = 0;
  *v = g_new0(char, str_len / 2);
  
  g_return_val_if_fail(*v, FALSE);

  while(*cursor && *(cursor + 1)) {
    gchar tmpstr[2];
    int tmpint;

    if(isspace(*cursor) || isspace(*(cursor + 1))) {
      error = TRUE;
    } else {
      int num_read;
      tmpstr[0] = *cursor;
      tmpstr[0] = *(cursor + 1);
      
      if((sscanf(tmpstr, "%x%n", &tmpint, &num_read) < 1)
         || (num_read != 2)) {
        error = TRUE;
      } else {
        *((gchar *) (v + *data_len)) = tmpint;
        *data_len += 1;
        cursor += 2;
      }
    }
  }

  if(error || (*data_len != (str_len / 2))) {
    g_free(*v);
    *v = NULL;
    *data_len = 0;
    return(FALSE);
  }

  return(TRUE);
}

/***************************************************************************/
/* simple chars only parser - just grabs all it's contained chars and
   does what you specify in the end handler - if you pass NULL as the
   end handler to simple_chars_only_parser_new, the characters are just
   passed to the parent as a new string.

   input: NA
   returns: gchar array allocated via g_new, etc.

   start: NA
   chars: generic_accumulate_chars.
   end: varies - default is to concatenate all accumulated chars and return.

   cleanup-result: g_free (for chars)
   cleanup-chars: g_free (for chars)
   fail: NA
   result-fail: g_free (for chars)
   chars-fail: g_free (for chars)

 */

static gboolean
generic_return_chars_end_handler(gpointer data_for_children,
                                 GSList* data_from_children,
                                 GSList* sibling_data,
                                 gpointer parent_data,
                                 gpointer global_data,
                                 gpointer *result,
                                 const gchar *tag) {
  gchar *txt = NULL;
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  *result = txt;
  return(TRUE);
}


static sixtp*
simple_chars_only_parser_new(sixtp_end_handler end_handler) {
  sixtp *top_level = sixtp_new();
  
  g_return_val_if_fail(top_level, NULL);
  if(!end_handler) end_handler = generic_return_chars_end_handler;
  sixtp_set_chars(top_level, generic_accumulate_chars);
  sixtp_set_end(top_level, end_handler);
  sixtp_set_cleanup_result(top_level, generic_free_result);
  sixtp_set_cleanup_chars(top_level, generic_free_result);
  sixtp_set_result_fail(top_level, generic_free_result);
  sixtp_set_chars_fail(top_level, generic_free_result);
  return(top_level);
}


/****************************************************************************/
/* <kvp-frame>
   
   A collection of node functions intended to parse a sub-node set
   that looks like this:

     <kvp-frame>
       <s>
         <k>notes</k>
         <string>foo</string>
       </s>
       <s>
         <k>temp</k>
         <gint64>97</gint64>
       </s>
     </kvp-frame>

   and return a kvp_frame*.  The start handler for the top allocates
   the kvp_frame* and passes it to the children.  The <s> blocks add
   slots according to their <k> (key) and value blocks.

   FIXME: right now this is totally inappropriate for cases where we
   want to read in a set of new values that should "merge" with the
   existing values.  This is only appropriate for wholesale
   replacement of the slots.

*/

/* kvp-frame [value] handlers

   Handle the possible values.  Each value handler is expected to
   parse it's subtree and return an appropriate kvp_value* in its
   result.  The <kvp-frame> <slot> handler will then cram it where it
   belongs. */


static void
kvp_value_result_cleanup(sixtp_child_result *cr) {  
  kvp_value *v = (kvp_value *) cr->data;;
  if(v) kvp_value_delete(v);
}

static sixtp*
simple_kvp_value_parser_new(sixtp_end_handler end_handler) {
  sixtp *top_level = sixtp_new();

  g_return_val_if_fail(top_level, NULL);
  sixtp_set_chars(top_level, generic_accumulate_chars);
  sixtp_set_cleanup_chars(top_level, generic_free_result);
  sixtp_set_chars_fail(top_level, generic_free_result);
  sixtp_set_end(top_level, end_handler);
  sixtp_set_cleanup_result(top_level, kvp_value_result_cleanup);
  sixtp_set_result_fail(top_level, kvp_value_result_cleanup);
  return(top_level);
}

/* <gint64> - gint64 kvp_value parser.

   input: NA
   returns: gint64 kvp_value

   start: NA
   chars: generic_accumulate_chars.
   end: convert chars to gint64 kvp_value* if possible and return.

   cleanup-result: kvp_value_delete.
   cleanup-chars: g_free (for chars)
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: g_free (for chars)

 */

static gboolean
gint64_kvp_value_end_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  gchar *txt = NULL;
  gint64 val;
  kvp_value *kvpv;
  gboolean ok;

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_gint64(txt, &val);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  kvpv = kvp_value_new_gint64(val);
  g_return_val_if_fail(kvpv, FALSE);
    
  *result = kvpv;
  return(TRUE);
}

static sixtp*
gint64_kvp_value_parser_new() {
  return(simple_kvp_value_parser_new(gint64_kvp_value_end_handler));
}

/* <double> - double kvp_value parser.

   input: NA
   returns: double kvp_value

   start: NA
   chars: generic_accumulate_chars.
   end: convert chars to double kvp_value* if possible and return.

   cleanup-result: kvp_value_delete.
   cleanup-chars: g_free (for chars)
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: g_free (for chars)

 */

static gboolean
double_kvp_value_end_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  gchar *txt = NULL;
  double val;
  kvp_value *kvpv;
  gboolean ok;

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_double(txt, &val);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  kvpv = kvp_value_new_double(val);
  g_return_val_if_fail(kvpv, FALSE);
    
  *result = kvpv;
  return(TRUE);
}

static sixtp*
double_kvp_value_parser_new() {
  return(simple_kvp_value_parser_new(double_kvp_value_end_handler));
}

/* <numeric> - gnc_numeric kvp_value parser.

   input: NA
   returns: numeric kvp_value

   start: NA
   chars: generic_accumulate_chars.
   end: convert chars to numeric kvp_value* if possible and return.

   cleanup-result: kvp_value_delete.
   cleanup-chars: g_free (for chars)
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: g_free (for chars)

 */

static gboolean
numeric_kvp_value_end_handler(gpointer data_for_children,
                              GSList* data_from_children,
                              GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *result,
                              const gchar *tag) {
  gchar *txt = NULL;
  gnc_numeric val;
  kvp_value *kvpv;
  gboolean ok;

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);

  ok = string_to_gnc_numeric(txt, &val) != NULL;
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  kvpv = kvp_value_new_numeric(val);
  g_return_val_if_fail(kvpv, FALSE);

  *result = kvpv;
  return(TRUE);
}

static sixtp*
numeric_kvp_value_parser_new() {
  return(simple_kvp_value_parser_new(numeric_kvp_value_end_handler));
}

/* <string> - string kvp_value parser.

   input: NA
   returns: string kvp_value

   start: NA
   chars: generic_accumulate_chars.
   end: convert chars to string kvp_value* if possible and return.

   cleanup-result: kvp_value_delete.
   cleanup-chars: g_free (for chars)
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: g_free (for chars)

 */

static gboolean
string_kvp_value_end_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  gchar *txt = NULL;
  kvp_value *kvpv;
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  kvpv = kvp_value_new_string(txt);
  g_free(txt);
  g_return_val_if_fail(kvpv, FALSE);
    
  *result = kvpv;
  return(TRUE);
}

static sixtp*
string_kvp_value_parser_new() {
  return(simple_kvp_value_parser_new(string_kvp_value_end_handler));
}

/* <guid> - guid kvp_value parser.

   input: NA
   returns: guid kvp_value

   start: NA
   chars: generic_accumulate_chars.
   end: convert chars to guid kvp_value* if possible and return.

   cleanup-result: kvp_value_delete.
   cleanup-chars: g_free (for chars)
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: g_free (for chars)

 */

static gboolean
guid_kvp_value_end_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag) {
  gchar *txt = NULL;
  GUID val;
  kvp_value *kvpv;
  gboolean ok;
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &val);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  kvpv = kvp_value_new_guid(&val);
  g_return_val_if_fail(kvpv, FALSE);
    
  *result = kvpv;
  return(TRUE);
}

static sixtp*
guid_kvp_value_parser_new() {
  return(simple_kvp_value_parser_new(guid_kvp_value_end_handler));
}

/*********************************/
/* kvp-frame binary value handlers

   A binary chunk can have a variety of types of children, and these
   children may appear multiple times, but at the moment only <hex>
   children are supported.  The end handler has to take all the
   children's results, concatenate them into one big kvp_value, and
   return it.

   All of the children ATM are expected to return binary kvp_values.  */

/* <hex> (lineage <binary> <s> <kvp-frame>)
   input: NA
   returns: binary kvp_value

   start: NA
   chars: generic_accumulate_chars
   end: convert the chars from hex to binary data and return binary kvp_value.

   cleanup-result: kvp_value_delete
   cleanup-chars: g_free chars
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: g_free chars
   
 */

static gboolean
hex_binary_kvp_value_end_handler(gpointer data_for_children,
                                 GSList* data_from_children,
                                 GSList* sibling_data,
                                 gpointer parent_data,
                                 gpointer global_data,
                                 gpointer *result,
                                 const gchar *tag) {
  gchar *txt = NULL;
  void *val;
  guint64 size;
  kvp_value *kvpv;
  gboolean ok;
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = hex_string_to_binary(txt, &val, &size);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  kvpv = kvp_value_new_binary_nc(val, size);
  g_return_val_if_fail(kvpv, FALSE);
    
  *result = kvpv;
  return(TRUE);
}

static sixtp*
hex_binary_kvp_value_parser_new() {
  return(simple_kvp_value_parser_new(hex_binary_kvp_value_end_handler));
}

/* <binary> (lineage <s> <kvp-frame>)
   input: NA
   returns: binary kvp_value*

   start: NA
   chars: allow_and_ignore_only_whitespace.
   end: concatenate all the binary data from the children -> kvp_value.

   cleanup-result: kvp_value_delete
   cleanup-chars: NA
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: NA

 */

static gboolean
kvp_frame_binary_end_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  void *data;
  guint64 total_size;
  guint64 pos;
  kvp_value *kvpv;
  GSList *lp;
  
  /* at this point, we know that if there are child results, they all
     have to be binary kvp_values. */
  
  /* first see how much data we've got. */
  total_size = 0;
  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;
    kvp_value *kvp = (kvp_value *) cr->data;
    void *tmpdata;
    guint64 tmpsize;

    tmpdata = kvp_value_get_binary(kvp, &tmpsize);
    g_return_val_if_fail(tmpdata, FALSE);
    total_size += tmpsize;
  }

  /* allocate a chunk to hold it all and copy */
  data = g_new(gchar, total_size);
  g_return_val_if_fail(data, FALSE);

  pos = 0;
  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;
    kvp_value *kvp = (kvp_value *) cr->data;
    void *new_data;
    guint64 new_size;

    new_data = kvp_value_get_binary(kvp, &new_size);
    g_return_val_if_fail(new_data, FALSE);
    memcpy((data + pos), new_data, new_size);
    pos += new_size;
  }

  kvpv = kvp_value_new_binary_nc(data, total_size);
  g_return_val_if_fail(kvpv, FALSE);

  *result = kvpv;
  return(TRUE);
}

static sixtp*
binary_kvp_value_parser_new() {
  sixtp *top_level = sixtp_new();
  sixtp *hex_pr;
  
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, kvp_frame_binary_end_handler);
  sixtp_set_cleanup_result(top_level, kvp_value_result_cleanup);
  sixtp_set_result_fail(top_level, kvp_value_result_cleanup);
  
  hex_pr = hex_binary_kvp_value_parser_new();
  if(!hex_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "hex", hex_pr);

  return(top_level);
}

/*********************************/
/* glist kvp-value handler
 */

/* <glist> (lineage <s> <kvp-frame>)
   input: NA
   returns: glist kvp_value

   start: NA
   chars: allow_and_ignore_only_whitespace
   end: convert the child list pointer to a glist kvp_value and return.

   cleanup-result: kvp_value_delete
   cleanup-chars: NA
   fail: NA
   result-fail: kvp_value_delete
   chars-fail: NA
   
 */


static gboolean
glist_kvp_value_end_handler(gpointer data_for_children,
                            GSList* data_from_children,
                            GSList* sibling_data,
                            gpointer parent_data,
                            gpointer global_data,
                            gpointer *result,
                            const gchar *tag) {
  GSList *lp;
  GList *result_glist;
  kvp_value *kvp_result;

  result_glist = NULL;
  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;
    kvp_value *kvp = (kvp_value *) cr->data;

    /* children are in reverse chron order, so this fixes it. */
    result_glist = g_list_prepend(result_glist, kvp);
    cr->should_cleanup = FALSE;
  }

  kvp_result = kvp_value_new_glist_nc(result_glist);
  if(!kvp_result) {
    kvp_glist_delete(result_glist);
  }
  *result = kvp_result;
  return(TRUE);
}

static gboolean
add_all_kvp_value_parsers_as_sub_nodes(sixtp *p,
                                       sixtp *kvp_frame_parser,
                                       sixtp *glist_parser) {
  sixtp *child_pr;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(kvp_frame_parser, FALSE);

  child_pr = gint64_kvp_value_parser_new();
  g_return_val_if_fail(child_pr, FALSE);
  sixtp_add_sub_parser(p, "gint64", child_pr);

  child_pr = double_kvp_value_parser_new();
  g_return_val_if_fail(child_pr, FALSE);
  sixtp_add_sub_parser(p, "double", child_pr);

  child_pr = numeric_kvp_value_parser_new();
  g_return_val_if_fail(child_pr, FALSE);
  sixtp_add_sub_parser(p, "numeric", child_pr);

  child_pr = string_kvp_value_parser_new();
  g_return_val_if_fail(child_pr, FALSE);
  sixtp_add_sub_parser(p, "string", child_pr);

  child_pr = guid_kvp_value_parser_new();
  g_return_val_if_fail(child_pr, FALSE);
  sixtp_add_sub_parser(p, "guid", child_pr);

  child_pr = binary_kvp_value_parser_new();
  g_return_val_if_fail(child_pr, FALSE);
  sixtp_add_sub_parser(p, "binary", child_pr);

  sixtp_add_sub_parser(p, "glist", glist_parser);
  sixtp_add_sub_parser(p, "frame", kvp_frame_parser);

  return(TRUE);
}

static sixtp*
glist_kvp_value_parser_new(sixtp *kvp_frame_parser) {
  sixtp *top_level = sixtp_new();
  
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, glist_kvp_value_end_handler);
  sixtp_set_cleanup_result(top_level, kvp_value_result_cleanup);
  sixtp_set_result_fail(top_level, kvp_value_result_cleanup);
  
  if(!add_all_kvp_value_parsers_as_sub_nodes(top_level,
                                             kvp_frame_parser,
                                             top_level)) {
    sixtp_destroy(top_level);
    return(NULL);
  }

  return(top_level);
}

/*********************************/
/* kvp-frame slot handlers 
   
   handlers for the <s><k>some key</k><[value]>data</[value]> sub-structure.
*/

/* <k> (lineage <s> <kvp-frame>)

   kvp-frame slot key handler - just a generic-string-parser

 */

/* <s> (lineage <kvp-frame>)

   kvp-frame slot handler.

   input: kvp_frame*
   returns: NA

   start: NA
   characters: allow_and_ignore_only_whitespace
   end: check for two children - one must be a <k> - if OK, set slot.

   cleanup-result: NA
   cleanup-chars: NA
   fail: NA
   result-fail: NA
   chars-fail: NA

 */

static gboolean
kvp_frame_slot_end_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag) {
  kvp_frame *f = (kvp_frame *) parent_data;
  GSList *lp;
  guint64 key_node_count;
  gchar *key = NULL;
  sixtp_child_result *value_cr = NULL;
  kvp_value *value = NULL;
  gboolean delete_value = FALSE;

  g_return_val_if_fail(f, FALSE);

  if(g_slist_length(data_from_children) != 2) return(FALSE);

  /* check to see that we got exactly one <key> node */
  lp = data_from_children;
  key_node_count = 0;
  for(lp = data_from_children; lp; lp = lp->next) {
    sixtp_child_result *cr = (sixtp_child_result *) lp->data;

    if(is_child_result_from_node_named(cr, "k")) {
      key = (char *) cr->data;
      key_node_count++;
    } else {
      if(is_child_result_from_node_named(cr, "frame")) {
        kvp_frame *frame = cr->data;
        value = kvp_value_new_frame (frame);
        delete_value = TRUE;
      } else {
        value = cr->data;
        delete_value = FALSE;
      }

      value_cr = cr;
    }
  }

  if(key_node_count != 1) return(FALSE);

  value_cr->should_cleanup = TRUE;
  kvp_frame_set_slot(f, key, value);
  if (delete_value)
    kvp_value_delete (value);
  return(TRUE);
}

static sixtp*
kvp_frame_slot_parser_new(sixtp *kvp_frame_parser) {
  sixtp *top_level = sixtp_new();
  sixtp *child_pr;
  sixtp *glist_pr;

  g_return_val_if_fail(kvp_frame_parser, NULL);
  
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, kvp_frame_slot_end_handler);

  child_pr = simple_chars_only_parser_new(NULL);
  if(!child_pr) { sixtp_destroy(top_level); return(NULL); }
  sixtp_add_sub_parser(top_level, "k", child_pr);

  glist_pr = glist_kvp_value_parser_new(kvp_frame_parser);
  if(!glist_pr) { sixtp_destroy(top_level); return(NULL); }

  if(!add_all_kvp_value_parsers_as_sub_nodes(top_level,
                                             kvp_frame_parser,
                                             glist_pr)) {
    sixtp_destroy(top_level);
    return(NULL);
  }

  return(top_level);
}


/* <kvp-frame> - can be used anywhere.

   input: NA
   returns: kvp_frame*

   start: Allocates kvp_frame* and places in data_for_children.
   characters: none (whitespace only).
   end: put kvp_frame* into result if everything's OK.

   cleanup-result: delete kvp_frame*
   cleanup-chars: NA
   fail: delete kvp_frame*
   result-fail: delete kvp_frame*
   chars-fail: NA

 */

static gboolean
kvp_frame_start_handler(GSList* sibling_data,
                        gpointer parent_data,
                        gpointer global_data,
                        gpointer *data_for_children,
                        gpointer *result,
                        const gchar *tag) {
  kvp_frame *f = kvp_frame_new();
  g_return_val_if_fail(f, FALSE);
  *data_for_children = f;
  return(TRUE);
}

static gboolean
kvp_frame_end_handler(gpointer data_for_children,
                      GSList* data_from_children,
                      GSList* sibling_data,
                      gpointer parent_data,
                      gpointer global_data,
                      gpointer *result,
                      const gchar *tag) {
  
  kvp_frame *f = (kvp_frame *) data_for_children;
  g_return_val_if_fail(f, FALSE);
  *result = f;
  return(TRUE);
}

static void
kvp_frame_fail_handler(gpointer data_for_children,
                       GSList* data_from_children,
                       GSList* sibling_data,
                       gpointer parent_data,
                       gpointer global_data,
                       gpointer *result,
                       const gchar *tag) {  
  kvp_frame *f = (kvp_frame *) data_for_children;
  if(f) kvp_frame_delete(f);
}

static void
kvp_frame_result_cleanup(sixtp_child_result *cr) {  
  kvp_frame *f = (kvp_frame *) cr->data;;
  if(f) kvp_frame_delete(f);
}

static sixtp*
kvp_frame_parser_new() {
  sixtp *top_level = sixtp_new();
  sixtp *child_pr;

  g_return_val_if_fail(top_level, NULL);

  sixtp_set_start(top_level, kvp_frame_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, kvp_frame_end_handler);
  sixtp_set_cleanup_result(top_level, kvp_frame_result_cleanup);
  sixtp_set_result_fail(top_level, kvp_frame_result_cleanup);
  sixtp_set_fail(top_level, kvp_frame_fail_handler);

  child_pr = kvp_frame_slot_parser_new(top_level);
  if(!child_pr) { sixtp_destroy(top_level); return(NULL); }
  sixtp_add_sub_parser(top_level, "s", child_pr);

  return(top_level);
}


/****************************************************************************/
/* generic timespec handler.
   
   A collection of node functions intended to parse a sub-node set
   that looks like this:

     <date-posted>
       <s>Mon, 05 Jun 2000 23:16:19 -0500</s>
       <ns>658864000</ns>
     </date-posted>

   and produce a Timespec*.  The start handler for the top allocates
   the Timespec * and passes it to the children.  The <s> block sets
   the seconds and the <ns> block (if any) sets the nanoseconds.  If
   all goes well, returns the Timespec* as the result.

*/

static gboolean
string_to_timespec_secs(const gchar *str, Timespec *ts) {

  struct tm parsed_time;
  const gchar *strpos;
  time_t parsed_secs;

  memset(&parsed_time, 0, sizeof(struct tm));

  /* If you change this, make sure you also change the output code, if
     necessary. */
  /*fprintf(stderr, "parsing (%s)\n", str);*/
  strpos = strptime(str, "%Y-%m-%d %H:%M:%S", &parsed_time);

  g_return_val_if_fail(strpos, FALSE);

  {
    char sign;
    int h1;
    int h2;
    int m1;
    int m2;
    int num_read;

    /* must use "<" here because %n's effects aren't well defined */
    if(sscanf(strpos, " %c%1d%1d%1d%1d%n",
              &sign,
              &h1,
              &h2,
              &m1,
              &m2,
              &num_read) < 5) {
      return(FALSE);
    }

    if((sign != '+') && (sign != '-')) return(FALSE);
    if(!isspace_str(strpos + num_read, -1)) return(FALSE);

    parsed_time.tm_gmtoff = (h1 * 10 + h2) * 60 * 60;
    parsed_time.tm_gmtoff += (m1 * 10 + m2) * 60;
    if(sign == '-') parsed_time.tm_gmtoff = - parsed_time.tm_gmtoff;
    parsed_time.tm_isdst = -1;
  }

  parsed_secs = mktime(&parsed_time);
  
  if(parsed_secs == (time_t) -1) return(FALSE);
  
  ts->tv_sec = parsed_secs;

  return(TRUE);
}

static gboolean
string_to_timespec_nsecs(const gchar *str, Timespec *ts) {

  long int nanosecs;
  int charcount;
  
  sscanf(str, " %ld %n", &nanosecs, &charcount);

  if(charcount != strlen(str)) return(FALSE);

  ts->tv_nsec = nanosecs;

  return(TRUE);
}

/* Top level timespec node:

   input: user end handler *
   returns: Timespec*

   start: Allocates TimespecParseInfo* for data_for_children.
   characters: none (whitespace only).
   end: g_free TimespecParseInfo + any other actions

   cleanup-result: NA
   cleanup-chars: NA
   fail: g_free data_for_children.
   result-fail: g_free data_for_children.
   chars-fail: NA

 */

typedef struct {
  Timespec ts;
  guint s_block_count;
  guint ns_block_count;
} TimespecParseInfo;

static gboolean
generic_timespec_start_handler(GSList* sibling_data,
                               gpointer parent_data,
                               gpointer global_data,
                               gpointer *data_for_children,
                               gpointer *result,
                               const gchar *tag) {

  TimespecParseInfo *tsp = g_new0(TimespecParseInfo, 1);
  g_return_val_if_fail(tsp, FALSE);
  *data_for_children = tsp;
  return(TRUE);
}

/* You can't use this function directly.  You have to call it from
   your own end handler.  If it returns TRUE, *result will contain the
   new timespec.  Otherwise, you can presume that everything's been
   cleaned up properly and return FALSE.  */
static gboolean
timespec_parse_ok(TimespecParseInfo *info) {

  if((info->s_block_count > 1) || (info->ns_block_count > 1) ||
     ((info->s_block_count == 0) && (info->ns_block_count == 0))) {
    return(FALSE);
  } else {
    return(TRUE);
  }
}

/* generic_timespec_end_handler - must be customized and provided by
   the user. */

/* <s> (parent timespec-node)

   input: TimespecParseInfo *
   returns: NA

   start: NA
   characters: accumulate.
   end: convert characters to secs part of input Timespec and inc s_block_count.

   cleanup-result: NA
   cleanup-chars: g_free data.
   fail: NA
   result-fail: NA
   chars-fail: g_free data.

 */

static gboolean
generic_timespec_secs_end_handler(gpointer data_for_children,
                                  GSList* data_from_children,
                                  GSList* sibling_data,
                                  gpointer parent_data,
                                  gpointer global_data,
                                  gpointer *result,
                                  const gchar *tag) {
  gchar *txt = NULL;
  TimespecParseInfo *info = (TimespecParseInfo *) parent_data;
  gboolean ok;

  g_return_val_if_fail(parent_data, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_timespec_secs(txt, &(info->ts));
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  info->s_block_count++;
  return(TRUE);
}

/* <s> (parent timespec-node)

   input: TimespecParseInfo *
   returns: NA

   start: NA
   characters: accumulate.
   end: convert characters to secs part of input Timespec and inc s_block_count.

   cleanup-result: NA
   cleanup-chars: g_free data.
   fail: NA
   result-fail: NA
   chars-fail: g_free data.

 */

static gboolean
generic_timespec_nsecs_end_handler(gpointer data_for_children,
                                   GSList* data_from_children,
                                   GSList* sibling_data,
                                   gpointer parent_data,
                                   gpointer global_data,
                                   gpointer *result,
                                   const gchar *tag) {
  gchar *txt = NULL;
  TimespecParseInfo *info = (TimespecParseInfo *) parent_data;
  gboolean ok;

  g_return_val_if_fail(parent_data, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_timespec_nsecs(txt, &(info->ts));
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  info->ns_block_count++;
  return(TRUE);
}

static sixtp *
generic_timespec_parser_new(sixtp_end_handler end_handler) {
  sixtp *top_level = sixtp_new();
  sixtp *secs_pr;
  sixtp *nsecs_pr;

  g_return_val_if_fail(top_level, NULL);
  sixtp_set_start(top_level, generic_timespec_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, end_handler);
  sixtp_set_cleanup_result(top_level, generic_free_result);
  sixtp_set_fail(top_level, generic_free_data_for_children);
  sixtp_set_result_fail(top_level, generic_free_result);

  secs_pr = sixtp_new();
  if(!secs_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(secs_pr, generic_accumulate_chars);
  sixtp_set_end(secs_pr, generic_timespec_secs_end_handler);
  sixtp_set_cleanup_chars(secs_pr, generic_free_result);
  sixtp_set_chars_fail(secs_pr, generic_free_result);
  sixtp_add_sub_parser(top_level, "s", secs_pr);

  nsecs_pr = sixtp_new();
  if(!nsecs_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(nsecs_pr, generic_accumulate_chars);
  sixtp_set_end(nsecs_pr, generic_timespec_nsecs_end_handler);
  sixtp_set_cleanup_chars(nsecs_pr, generic_free_result);
  sixtp_set_chars_fail(nsecs_pr, generic_free_result);
  sixtp_add_sub_parser(top_level, "ns", nsecs_pr);

  return(top_level);
}

/****************************************************************************/
/* <ledger-data> (parent <gnc-data>)

   On failure or on normal cleanup, the account group will be killed,
   so if you want it, you better set should_cleanup to false

   input: NA
   
   to-children-via-*result: new AccountGroup*

   returns: an AccountGroup*
   
   start: creates the account group and puts it into *result

   characters: NA

   end: finishes up the account group and leaves it in result.

   cleanup-result: deletes the account group (use should_cleanup to avoid).

   cleanup-chars: NA

   fail: deletes the account group in *result.

   result-fail: same as cleanup-result.

   chars-fail: NA

*/


static gboolean
ledger_data_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag) {
  AccountGroup *ag;

  /* disable logging during load; otherwise its just a mess */
  xaccLogDisable();
  ag = xaccMallocAccountGroup();

  g_return_val_if_fail(ag, FALSE);

  *data_for_children = ag;
  return(ag != NULL);
}

static gboolean
ledger_data_end_handler(gpointer data_for_children,
                        GSList* data_from_children,
                        GSList* sibling_data,
                        gpointer parent_data,
                        gpointer global_data,
                        gpointer *result,
                        const gchar *tag) {
  
  AccountGroup *ag = (AccountGroup *) data_for_children;

  g_return_val_if_fail(ag, FALSE);

  /* mark the newly read group as saved, since the act of putting 
   * it together will have caused it to be marked up as not-saved. 
   */
  xaccGroupMarkSaved (ag);

  /* auto-number the accounts, if they are not already numbered */
  xaccGroupDepthAutoCode (ag);

  /* commit all groups, this completes the BeginEdit started when the
   * account_end_handler finished reading the account.
   */
  xaccAccountGroupCommitEdit (ag);

  /* set up various state that is not normally stored in the byte stream */
  xaccRecomputeGroupBalance (ag);

  xaccLogEnable();

  *result = ag;
  return(TRUE);
}

static void
ledger_data_fail_handler(gpointer data_for_children,
GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag) {  
  AccountGroup *ag = (AccountGroup *) data_for_children;
  if(ag) xaccFreeAccountGroup(ag);
}

static void
ledger_data_result_cleanup(sixtp_child_result *cr) {  
  AccountGroup *ag = (AccountGroup *) cr->data;
  if(ag) xaccFreeAccountGroup(ag);
}

/****************************************************************************/
/* Commodity restorer.

   Right now we just check to see that fields aren't duplicated.  If
   fields don't show up, then we just use "".

   We also check to see that we get a <fraction>.  If not, it's an
   error.

   Example:   
     <commodity>
       <restore>
         <space>NASDAQ</space>
         <id>XYZZY</id>
         <name>Grue Enterprises</name>
         <xcode>XXX</xcode>
         <fraction>100</fraction>
       </restore>
     </commodity>

 */

/**************/
/* <commodity>
 *
 * Does nothing.
*/


/*********************************/
/* <restore> (lineage <commodity>)

   Start handler allocates a gnc_commodity.  The end_handler, if
   everything's OK, crams the commodity into the engine, otherwise it
   deletes it.

   input: NA
   returns: NA

   start: allocate CommodityParseInfo* and put it into data_for_children.
   characters: allow and ignore only whitespace.
   after-child: handle strings from simple chars children.
   end: if OK create gnc_commodity and add to engine.  delete CommodityParseInfo.

   cleanup-result: NA
   cleanup-chars: NA
   fail: delete CommodityParseInfo*.
   result-fail: NA
   chars-fail: NA

 */

typedef struct {
  gchar *space;
  gchar *id;
  gchar *name;
  gchar *xcode;
  gboolean seen_fraction;
  int fraction;
} CommodityParseInfo;

static gboolean
commodity_restore_start_handler(GSList* sibling_data,
                                gpointer parent_data,
                                gpointer global_data,
                                gpointer *data_for_children,
                                gpointer *result,
                                const gchar *tag) {

  CommodityParseInfo *cpi = (CommodityParseInfo *) g_new0(CommodityParseInfo, 1);

  g_return_val_if_fail(cpi, FALSE);

  *data_for_children = cpi;
  return(TRUE);
}

static gboolean
commodity_restore_after_child_handler(gpointer data_for_children,
                                      GSList* data_from_children,
                                      GSList* sibling_data,
                                      gpointer parent_data,
                                      gpointer global_data,
                                      gpointer *result,
                                      
                                      const gchar *tag,
                                      const gchar *child_tag,
                                      sixtp_child_result *child_result) {
  CommodityParseInfo *cpi = (CommodityParseInfo *) data_for_children;

  g_return_val_if_fail(cpi, FALSE);
  g_return_val_if_fail(child_result, FALSE);

  if(strcmp(child_result->tag, "space") == 0) {
    if(cpi->space) return(FALSE);
    cpi->space = (gchar *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "id") == 0) {
    if(cpi->id) return(FALSE);
    cpi->id = (gchar *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "name") == 0) {
    if(cpi->name) return(FALSE);
    cpi->name = (gchar *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "xcode") == 0) {
    if(cpi->xcode) return(FALSE);
    cpi->xcode = (gchar *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "fraction") == 0) {
    gint64 frac;
    gboolean conv_ok;

    if(cpi->seen_fraction) return(FALSE);
    conv_ok = string_to_gint64((gchar *) child_result->data, &frac);
    cpi->fraction = frac;
    cpi->seen_fraction = TRUE;
    child_result->should_cleanup = TRUE;
  } else {
    /* redundant because the parser won't allow any other children */
    return(FALSE);
  }

  return(TRUE);
}

static gboolean
commodity_restore_end_handler(gpointer data_for_children,
                              GSList* data_from_children,
                              GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *result,
                              const gchar *tag) {
  CommodityParseInfo *cpi = (CommodityParseInfo *) data_for_children;
  gboolean ok = FALSE;
  gnc_commodity *comm = NULL;

  g_return_val_if_fail(cpi, FALSE);

  if(cpi->seen_fraction) {
    gnc_commodity *comm;

    if(!cpi->space) cpi->space = g_strdup("");
    if(!cpi->id) cpi->id = g_strdup("");
    if(!cpi->name) cpi->name = g_strdup("");
    if(!cpi->xcode) cpi->xcode = g_strdup("");

    comm = gnc_commodity_new(cpi->name,
                             cpi->space,
                             cpi->id,
                             cpi->xcode,
                             cpi->fraction);
    if(comm) {
      gnc_commodity_table *ctab = gnc_engine_commodities();
      if(ctab) {
        gnc_commodity_table_insert(ctab, comm);
        ok = TRUE;
      }
    }
  }

  g_free(cpi->space);
  g_free(cpi->id);
  g_free(cpi->name);
  g_free(cpi->xcode);
  g_free(cpi);

  if(!ok) g_free(comm);

  return(ok);
}

static sixtp *
commodity_restore_parser_new() {
  sixtp *top_level;
  sixtp *restore_pr;
  sixtp *tmp_pr;

  top_level = sixtp_new();
  g_return_val_if_fail(top_level, NULL);
  
  restore_pr = sixtp_new();
  if(!restore_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_start(restore_pr, commodity_restore_start_handler);
  sixtp_set_chars(restore_pr, allow_and_ignore_only_whitespace);
  sixtp_set_after_child(restore_pr, commodity_restore_after_child_handler);
  sixtp_set_end(restore_pr, commodity_restore_end_handler);

  sixtp_set_fail(restore_pr, generic_free_data_for_children);

  sixtp_add_sub_parser(top_level, "restore", restore_pr);

  tmp_pr = simple_chars_only_parser_new(NULL);
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "space", tmp_pr);

  tmp_pr = simple_chars_only_parser_new(NULL);
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "id", tmp_pr);

  tmp_pr = simple_chars_only_parser_new(NULL);
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "name", tmp_pr);

  tmp_pr = simple_chars_only_parser_new(NULL);
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "xcode", tmp_pr);

  tmp_pr = simple_chars_only_parser_new(NULL);
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "fraction", tmp_pr);

  return(top_level);
}

/****************************************************************************/
/* <account> (parent <ledger-data>)
 
   This block does nothing but pass the ledger-data account group down
   to its children.  It generates no data of its own, so it doesn't
   need any cleanup.

   input: AccountGroup*

   to-children-via-*result: AccountGroup*

   returns: NA
   
   start: pass input to children.

   characters: NA

   end: NA

   cleanup-result: NA

   cleanup-chars: NA

   fail: NA

   result-fail: NA

   chars-fail: NA

 */

static gboolean
account_start_handler(GSList* sibling_data,
                      gpointer parent_data,
                      gpointer global_data,
                      gpointer *data_for_children,
                      gpointer *result,
                      const gchar *tag) {

  /* pass the parent data down to the children */
  *data_for_children = parent_data;
  return(TRUE);
}

/****************************************************************************/
/* <restore> (lineage <account> <ledger-data>)
   
   restores a given account.  We allocate the new account in the
   start block, the children modify it, and in the end block, we see
   if the resultant account is OK, and if so, we add it to the
   ledger-data's account group.
 
   input: AccountGroup*

   to-children-via-*result: new Account*

   returns: NA
   
   start: create new Account*, and leave in for children.

   characters: NA

   end: clear *result

   cleanup-result: NA

   cleanup-chars: NA

   fail: delete Account*

   result-fail: NA

   chars-fail: NA

 */

static gboolean
account_restore_start_handler(GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *data_for_children,
                              gpointer *result,
                              const gchar *tag) {
  Account *acc = xaccMallocAccount();
  
  g_return_val_if_fail(acc, FALSE);
  xaccAccountBeginEdit(acc);

  *data_for_children = acc;
  *result = acc;

  return(TRUE);
}

static gboolean
account_restore_end_handler(gpointer data_for_children,
GSList* data_from_children,
                            GSList* sibling_data,
                            gpointer parent_data,
                            gpointer global_data,
                            gpointer *result,
                            const gchar *tag) {
  AccountGroup *ag = (AccountGroup *) parent_data;
  Account *acc = (Account *) *result;
  AccountGroup *parent_ag;

  g_return_val_if_fail((ag && acc), FALSE);

  /* CHECKME: do we need to xaccAccountRecomputeBalance(acc) here? */
  xaccAccountCommitEdit(acc);

  /* If the account doesn't have a parent yet, just cram it into the
     top level */
  parent_ag = xaccAccountGetParent(acc);

  if(!parent_ag) xaccGroupInsertAccount(ag, acc);

  *result = NULL;

  /* Now return the account to the "edit" state.  At the end of reading
   * all the transactions, we will Commit.  This replaces #splits
   *  rebalances with #accounts rebalances at the end.  A BIG win!
   */
  xaccAccountBeginEdit(acc);
  return(TRUE);
}

static gboolean
account_restore_after_child_handler(gpointer data_for_children,
                                    GSList* data_from_children,
                                    GSList* sibling_data,
                                    gpointer parent_data,
                                    gpointer global_data,
                                    gpointer *result,
                                    
                                    const gchar *tag,
                                    const gchar *child_tag,
                                    sixtp_child_result *child_result) {
  Account *a = (Account *) data_for_children;
  g_return_val_if_fail(a, FALSE);
  if(!child_result) return(TRUE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(TRUE);
  if(strcmp(child_result->tag, "slots") == 0) {
    kvp_frame *f = (kvp_frame *) child_result->data;
    g_return_val_if_fail(f, FALSE);
    if(a->kvp_data) kvp_frame_delete(a->kvp_data);
    a->kvp_data = f;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "currency") == 0) {
    gnc_commodity *com = (gnc_commodity *) child_result->data;
    g_return_val_if_fail(com, FALSE);
    if(xaccAccountGetCurrency(a)) return FALSE;
    xaccAccountSetCurrency(a, com);
    /* let the normal child_result handler clean up com */
  }
  else if(strcmp(child_result->tag, "security") == 0) {
    gnc_commodity *com = (gnc_commodity *) child_result->data;
    g_return_val_if_fail(com, FALSE);
    if(xaccAccountGetSecurity(a)) return FALSE;
    xaccAccountSetSecurity(a, com);
    /* let the normal child_result handler clean up com */
  }

  return(TRUE);
}

static void
account_restore_fail_handler(gpointer data_for_children,
GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  Account *acc = (Account *) *result;
  if(acc) xaccFreeAccount(acc);
}

/****************************************************************************/
/* <name> (lineage <restore> <account>)
   
   restores a given account's name.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account name.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_name_end_handler(gpointer data_for_children,
GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  Account *acc = (Account *) parent_data;
  gchar *name = NULL;

  g_return_val_if_fail(acc, FALSE);

  name = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(name, FALSE);
  
  xaccAccountSetName(acc, name);
  g_free(name);
  return(TRUE);
}

/****************************************************************************/
/* <guid> (lineage <restore> <account>)
   
   restores a given account's guid.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account GUID if not duplicate.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_guid_end_handler(gpointer data_for_children,
GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  GUID gid;
  gboolean ok;

  g_return_val_if_fail(acc, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &gid);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  if(xaccAccountLookup(&gid)) {
    return(FALSE);
  }

  xaccAccountSetGUID(acc, &gid);
  return(TRUE);
}

/****************************************************************************/
/* <type> (lineage <restore> <account>)
   
   restores a given account's type.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account type.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_type_end_handler(gpointer data_for_children,
GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  int type;
  gboolean ok;

  g_return_val_if_fail(acc, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = xaccAccountStringToType(txt, &type);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);
  
  xaccAccountSetType(acc, type);
  return(TRUE);
}

/****************************************************************************/
/* <code> (lineage <restore> <account>)
   
   restores a given account's code.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account type.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_code_end_handler(gpointer data_for_children,
GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) {
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(acc, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccAccountSetCode(acc, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <description> (lineage <restore> <account>)
   
   restores a given account's description.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account description.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.
   restores a given account's description.

 */

static gboolean
acc_restore_description_end_handler(gpointer data_for_children,
GSList* data_from_children,
                                    GSList* sibling_data,
                                    gpointer parent_data,
                                    gpointer global_data,
                                    gpointer *result,
                                    const gchar *tag) {
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(acc, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccAccountSetDescription(acc, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <notes> (lineage <restore> <account>)
   
   restores a given account's notes.
   input: Account*
   returns: NA

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as account notes.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
acc_restore_notes_end_handler(gpointer data_for_children,
GSList* data_from_children,
                              GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *result,
                              const gchar *tag) {
  Account *acc = (Account *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(acc, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccAccountSetNotes(acc, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <parent> (lineage <restore> <account>)
   
   restores a given account's parent.
   input: Account*
   returns: NA

   start: NA

   characters: allow and ignore only whitespace.

   end: check for single <guid> child and if found, use result to set
   account guid.

   cleanup-result: NA
   cleanup-chars: NA
   fail: NA
   result-fail: NA
   chars-fail: NA

 */

static gboolean
acc_restore_parent_end_handler(gpointer data_for_children,
GSList* data_from_children,
                               GSList* sibling_data,
                               gpointer parent_data,
                               gpointer global_data,
                               gpointer *result,
                               const gchar *tag) {

  Account *acc = (Account *) parent_data;
  Account *parent;
  sixtp_child_result *child_result;
  GUID gid;
  
  g_return_val_if_fail(acc, FALSE);

  if(g_slist_length(data_from_children) != 1)
    return(FALSE);

  child_result = (sixtp_child_result *) data_from_children->data;
  
  if(!is_child_result_from_node_named(child_result, "guid"))
    return(FALSE);

  /* otherwise this must be a good result - use it */
  gid = *((GUID *) child_result->data);

  parent = xaccAccountLookup(&gid);
  
  g_return_val_if_fail(parent, FALSE);

  xaccRemoveAccount(acc); /* just to be anal */
  xaccAccountInsertSubAccount(parent, acc);

  return(TRUE);
}

/****************************************************************************/
/* <?> generic guid handler...
   
   Attempts to parse the current accumulated characters data as a guid
   and return it.

   input: NA
   returns: GUID*

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and create and return GUID*, if possible.

   cleanup-result: g_free the GUID*
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: g_free the GUID*
   chars-fail: g_free the result string.

 */

static gboolean
generic_guid_end_handler(gpointer data_for_children,
GSList* data_from_children,
                         GSList* sibling_data,
                         gpointer parent_data,
                         gpointer global_data,
                         gpointer *result,
                         const gchar *tag) {
  gchar *txt = NULL;
  GUID *gid;
  gboolean ok;

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  gid = g_new(GUID, 1);
  if(!gid) {
    g_free(txt);
    return(FALSE);
  }

  ok = string_to_guid(txt, gid);
  g_free(txt);

  if(!ok) {
    g_free(gid);
    return(FALSE);
  }

  *result = gid;
  return(TRUE);
}

static sixtp*
generic_guid_parser_new() {
  sixtp *top_level = sixtp_new();

  g_return_val_if_fail(top_level, NULL);

  sixtp_set_chars(top_level, generic_accumulate_chars);  
  sixtp_set_cleanup_chars(top_level, generic_free_result);
  sixtp_set_chars_fail(top_level, generic_free_result);
  sixtp_set_end(top_level, generic_guid_end_handler);
  sixtp_set_result_fail(top_level, generic_free_result);
  sixtp_set_cleanup_result(top_level, generic_free_result);

  return(top_level);
}

/****************************************************************************/
/* <?> generic gnc_numeric handler...
   
   Attempts to parse the current accumulated characters data as a
   gnc_numeric and return it.

   input: NA
   returns: gnc_numeric*

   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and create and return gnc_numeric*, if possible.

   cleanup-result: g_free the gnc_numeric*
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: g_free the gnc_numeric*
   chars-fail: g_free the result string.

 */

static gboolean
generic_gnc_numeric_end_handler(gpointer data_for_children,
                                GSList* data_from_children,
                                GSList* sibling_data,
                                gpointer parent_data,
                                gpointer global_data,
                                gpointer *result,
                                const gchar *tag) {
  gnc_numeric *num = NULL;
  gchar *txt = NULL;
  gboolean ok = FALSE;

  txt = concatenate_child_result_chars(data_from_children);

  if(txt) {
    num = g_new(gnc_numeric, 1);
    if(num) {
      if(string_to_gnc_numeric(txt, num)) {
        ok = TRUE;
        *result = num;
      }
    }
  }

  g_free(txt);
  if(!ok) g_free(num);

  return(ok);
}

static sixtp*
generic_gnc_numeric_parser_new() {
  sixtp *top_level = sixtp_new();

  g_return_val_if_fail(top_level, NULL);

  sixtp_set_chars(top_level, generic_accumulate_chars);  
  sixtp_set_cleanup_chars(top_level, generic_free_result);
  sixtp_set_chars_fail(top_level, generic_free_result);
  sixtp_set_end(top_level, generic_gnc_numeric_end_handler);
  sixtp_set_result_fail(top_level, generic_free_result);
  sixtp_set_cleanup_result(top_level, generic_free_result);

  return(top_level);
}

/****************************************************************************/
/* generic gnc_commodity lookup handler.
   
   A collection of node functions intended to parse a sub-node set
   that looks like this:

     <security>
       <space>NASDAQ</space>
       <id>ZXDDQ</id>
     </security>

   and produce a gnc_commodity* by looking up the unique combination
   of namespace and ID (mnemonic).

   The start handler for the top allocates a CommodityParseInfo* and
   passes it to the children.  The <space> block sets the namespace
   and the <id> block sets the ID.  The end handler performs the
   lookup.  If all goes well, returns the gnc_commodity* as the
   result.  */

/* Top level gnc_commodity lookup node:

   input: NA
   returns: gnc_commodity*

   start: Allocates CommodityParseInfo* for data_for_children.
   characters: none (whitespace only).
   end: lookup commodity and place into *result, free data_for_children.

   fail: g_free data_for_children (CommodityParseInfo and contents).
   cleanup-chars: NA
   chars-fail: NA
   cleanup-result: NA (we didn't create the gnc_commodity we're returning)
   result-fail: NA

 */

typedef struct {
  gchar *namespace;
  gchar *id;
} CommodityLookupParseInfo;

static gboolean
generic_gnc_commodity_lookup_start_handler(GSList* sibling_data,
                                           gpointer parent_data,
                                           gpointer global_data,
                                           gpointer *data_for_children,
                                           gpointer *result,
                                           const gchar *tag) {
  
  CommodityLookupParseInfo *cpi = g_new0(CommodityLookupParseInfo, 1);
  g_return_val_if_fail(cpi, FALSE);
  *data_for_children = cpi;
  return(TRUE);
}

static gboolean
generic_gnc_commodity_lookup_after_child_handler(gpointer data_for_children,
                                                 GSList* data_from_children,
                                                 GSList* sibling_data,
                                                 gpointer parent_data,
                                                 gpointer global_data,
                                                 gpointer *result,
                                          
                                                 const gchar *tag,
                                                 const gchar *child_tag,
                                                 sixtp_child_result *child_result) {
  CommodityLookupParseInfo *cpi = (CommodityLookupParseInfo *) data_for_children;

  g_return_val_if_fail(cpi, FALSE);
  g_return_val_if_fail(child_result, FALSE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(FALSE);

  if(strcmp(child_result->tag, "space") == 0) {
    if(cpi->namespace) return(FALSE);
    cpi->namespace = (gchar *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "id") == 0) {
    if(cpi->id) return(FALSE);
    cpi->id = (gchar *) child_result->data;
    child_result->should_cleanup = FALSE;
  } else {
    /* redundant because the parser won't allow any other children */
    return(FALSE);
  }

  return(TRUE);
}

static gboolean
generic_gnc_commodity_lookup_end_handler(gpointer data_for_children,
                                         GSList* data_from_children,
                                         GSList* sibling_data,
                                         gpointer parent_data,
                                         gpointer global_data,
                                         gpointer *result,
                                         const gchar *tag) {
  CommodityLookupParseInfo *cpi = (CommodityLookupParseInfo *) data_for_children;
  gboolean ok = FALSE;

  g_return_val_if_fail(cpi, FALSE);

  if(cpi->namespace && cpi->id) {
    gnc_commodity *com =
      gnc_commodity_table_lookup(gnc_engine_commodities(),
                                 cpi->namespace,
                                 cpi->id);
    if(com) {
      *result = com;
      ok = TRUE;
    }
  }

  g_free(cpi->namespace);
  g_free(cpi->id);
  g_free(cpi);
  return(ok);
}


static sixtp *
generic_gnc_commodity_lookup_parser_new() {
  sixtp *top_level = sixtp_new();
  sixtp *namespace_pr;
  sixtp *id_pr;

  g_return_val_if_fail(top_level, NULL);
  sixtp_set_start(top_level, generic_gnc_commodity_lookup_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, generic_gnc_commodity_lookup_end_handler);

  sixtp_set_fail(top_level, generic_free_data_for_children);

  sixtp_set_after_child(top_level,
                        generic_gnc_commodity_lookup_after_child_handler);

  namespace_pr = simple_chars_only_parser_new(NULL);
  if(!namespace_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "space", namespace_pr);

  id_pr = simple_chars_only_parser_new(NULL);
  if(!id_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "id", id_pr);

  return(top_level);
}


/****************************************************************************/
/* <transaction> (parent <ledger-data>)
 
   This block does nothing but pass the ledger-data account group down
   to its children.  It generates no data of its own, so it doesn't
   need any cleanup.

   input: AccountGroup*

   to-children-via-*result: AccountGroup*

   returns: NA
   
   start: pass input to children.

   characters: ignore whitespace only

   end: NA

   cleanup-result: NA

   cleanup-chars: NA

   fail: NA

   result-fail: NA

   chars-fail: NA

 */

static gboolean
transaction_start_handler(GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *data_for_children,
                          gpointer *result,
                          const gchar *tag) {
  
  /* pass the parent data down to the children */
  *data_for_children = parent_data;
  return(TRUE);
}

/****************************************************************************/
/* <restore> (lineage <transaction> <ledger-data>)
   
   restores a given transaction.  We allocate the new transaction in
   the start block, the children modify it, and in the end block, we
   see if the resultant account is OK, and if so, we add it to the
   ledger-data's account group.
 
   from parent: AccountGroup*

   for children: new Transaction*

   result: NA
   
   -----------

   start: create new Transaction*, and store in data_for_children.

   chars: allow and ignore only whitespace.

   end: commit transaction to group if appropriate.

   cleanup-result: NA

   cleanup-chars: NA

   fail: delete Transaction* in data_for_children

   result-fail: NA

   chars-fail: NA

 */

static gboolean
txn_restore_start_handler(GSList* sibling_data,
                            gpointer parent_data,
                            gpointer global_data,
                            gpointer *data_for_children,
                            gpointer *result,
                            const gchar *tag) {
  Transaction *trans = xaccMallocTransaction();

  g_return_val_if_fail(trans, FALSE);

  xaccTransBeginEdit(trans);

  *data_for_children = trans;

  return(TRUE);
}

static gboolean
txn_restore_end_handler(gpointer data_for_children,
                          GSList* data_from_children,
                          GSList* sibling_data,
                          gpointer parent_data,
                          gpointer global_data,
                          gpointer *result,
                          const gchar *tag) {
  AccountGroup *ag = (AccountGroup *) parent_data;
  Transaction *trans = (Transaction *) data_for_children;

  g_return_val_if_fail(trans, FALSE);
  if(!ag) {
    xaccTransDestroy(trans);
    xaccTransCommitEdit(trans);
    return(FALSE);
  }

  if(!xaccTransGetGUID(trans)) {
    /* must at least have a GUID for a restore */
    xaccTransDestroy(trans);
    xaccTransCommitEdit(trans);
    return(FALSE);
  }
    
  /* FIXME: what if the trans has no splits? */
  xaccTransCommitEdit(trans);

  return(TRUE);
}

static gboolean
txn_restore_after_child_handler(gpointer data_for_children,
                                GSList* data_from_children,
                                GSList* sibling_data,
                                gpointer parent_data,
                                gpointer global_data,
                                gpointer *result,
                                
                                const gchar *tag,
                                const gchar *child_tag,
                                sixtp_child_result *child_result) {
  Transaction *trans = (Transaction *) data_for_children;
  g_return_val_if_fail(trans, FALSE);
  if(!child_result) return(TRUE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(TRUE);
  if(strcmp(child_result->tag, "slots") == 0) {
    kvp_frame *f = (kvp_frame *) child_result->data;
    g_return_val_if_fail(f, FALSE);
    if(trans->kvp_data) kvp_frame_delete(trans->kvp_data);
    trans->kvp_data = f;
    child_result->should_cleanup = FALSE;
  }
  return(TRUE);
}

static void
txn_restore_fail_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag) {
  Transaction *trans = (Transaction *) data_for_children;
  if(trans) {
    xaccTransDestroy(trans);
    xaccTransCommitEdit(trans);
  }
}

/****************************************************************************/
/* <guid> (lineage <restore> <transaction>)
   
   restores a given account's guid.

   from parent: Transaction*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as transaction GUID if not duplicate.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_guid_end_handler(gpointer data_for_children,
                               GSList* data_from_children,
                               GSList* sibling_data,
                               gpointer parent_data,
                               gpointer global_data,
                               gpointer *result,
                               const gchar *tag) {
  Transaction *t = (Transaction *) parent_data;
  gchar *txt = NULL;
  GUID gid;
  gboolean ok;

  g_return_val_if_fail(t, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &gid);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  if(xaccTransLookup(&gid)) {
    return(FALSE);
  }

  xaccTransSetGUID(t, &gid);
  return(TRUE);
}

/****************************************************************************/
/* <num> (lineage <restore> <transaction>)
   
   restores a given transaction's num.

   from parent: Transaction*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as transaction num.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_num_end_handler(gpointer data_for_children,
                              GSList* data_from_children,
                              GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *result,
                              const gchar *tag) {
  Transaction *t = (Transaction *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(t, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccTransSetNum(t, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <description> (lineage <restore> <transaction>)
   
   restores a given transaction's description.

   from parent: Transaction*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as transaction description.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_description_end_handler(gpointer data_for_children,
                                      GSList* data_from_children,
                                      GSList* sibling_data,
                                      gpointer parent_data,
                                      gpointer global_data,
                                      gpointer *result,
                                      const gchar *tag) {
  Transaction *t = (Transaction *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(t, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccTransSetDescription(t, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <date-posted> (lineage <restore> <transaction>)
   
   restores a given transaction's posted date.

   Just uses a generic_timespec parser, but with our own end handler.

   end: set date posted.

 */

static gboolean
txn_rest_date_posted_end_handler(gpointer data_for_children,
                                   GSList* data_from_children,
                                   GSList* sibling_data,
                                   gpointer parent_data,
                                   gpointer global_data,
                                   gpointer *result,
                                   const gchar *tag) {
  Transaction *t = (Transaction *) parent_data;
  TimespecParseInfo *info = (TimespecParseInfo *) data_for_children;
  
  g_return_val_if_fail(info, FALSE);
  if(!t || !timespec_parse_ok(info)) {
    g_free(info);
    return(FALSE);
  }

  xaccTransSetDateTS(t, &(info->ts));
  g_free(info);
  return(TRUE);
}

/****************************************************************************/
/* <date-entered> (lineage <restore> <transaction>)
   
   restores a given transaction's entered date.

   Just uses a generic_timespec parser, but with our own end handler.

   end: set date entered.

 */

static gboolean
txn_rest_date_entered_end_handler(gpointer data_for_children,
                                   GSList* data_from_children,
                                   GSList* sibling_data,
                                   gpointer parent_data,
                                   gpointer global_data,
                                   gpointer *result,
                                   const gchar *tag) {
  Transaction *t = (Transaction *) parent_data;
  TimespecParseInfo *info = (TimespecParseInfo *) data_for_children;
  
  g_return_val_if_fail(info, FALSE);
  if(!t || !timespec_parse_ok(info)) {
    g_free(info);
    return(FALSE);
  }

  xaccTransSetDateEnteredTS(t, &(info->ts));
  g_free(info);
  return(TRUE);
}



/****************************************************************************/

/* <split> (lineage <restore> <transaction> <ledger-data>)
   
   Restores a given split.  We allocate the new split in the start
   block, the children modify it, and in the end block, we see if the
   resultant split is OK, and if so, we add it to the input Transaction*
   account group.
 
   from parent: Transaction*
   for children: new Split*
   result: NA
   -----------
   start: create new Split*, and store in data_for_children.
   chars: allow and ignore only whitespace.
   end: commit split to transaction if appropriate.
   cleanup-result: NA
   cleanup-chars: NA
   fail: delete Transaction* in data_for_children
   result-fail: NA
   chars-fail: NA

 */

static gboolean
txn_restore_split_start_handler(GSList* sibling_data,
                                gpointer parent_data,
                                gpointer global_data,
                                gpointer *data_for_children,
                                gpointer *result,
                                const gchar *tag) {
  Split *s = xaccMallocSplit();
  
  g_return_val_if_fail(s, FALSE);
  *data_for_children = s;
  return(TRUE);
}

static gboolean
txn_restore_split_end_handler(gpointer data_for_children,
                              GSList* data_from_children,
                              GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *result,
                              const gchar *tag) {
  Transaction *t = (Transaction *) parent_data;
  Split *s = (Split *) data_for_children;

  g_return_val_if_fail(s, FALSE);
  if(!t) {
    xaccSplitDestroy(s);
    return(FALSE);
  }

  if(!xaccSplitGetGUID(s)) {
    /* must at least have a GUID for a restore */
    xaccSplitDestroy(s);
    return(FALSE);
  }
    
  xaccTransAppendSplit(t, s);
  return(TRUE);
}

static gboolean
txn_restore_split_after_child_handler(gpointer data_for_children,
                                      GSList* data_from_children,
                                      GSList* sibling_data,
                                      gpointer parent_data,
                                      gpointer global_data,
                                      gpointer *result,
                                      
                                      const gchar *tag,
                                      const gchar *child_tag,
                                      sixtp_child_result *child_result) {
  Split *s = (Split *) data_for_children;
  g_return_val_if_fail(s, FALSE);
  if(!child_result) return(TRUE);
  if(child_result->type != SIXTP_CHILD_RESULT_NODE) return(TRUE);

  if(strcmp(child_result->tag, "slots") == 0) {
    kvp_frame *f = (kvp_frame *) child_result->data;
    g_return_val_if_fail(f, FALSE);
    if(s->kvp_data) kvp_frame_delete(s->kvp_data);
    s->kvp_data = f;
    child_result->should_cleanup = FALSE;
  }
  else if(strcmp(child_result->tag, "quantity") == 0) {
    gnc_numeric *n = (gnc_numeric *) child_result->data;
    g_return_val_if_fail(n, FALSE);
    xaccSplitSetShareAmount(s, *n);
    /* let the normal child_result handler clean up n */
  }
  else if(strcmp(child_result->tag, "value") == 0) {
    gnc_numeric *n = (gnc_numeric *) child_result->data;
    g_return_val_if_fail(n, FALSE);
    xaccSplitSetValue(s, *n);
    /* let the normal child_result handler clean up n */
  }

  return(TRUE);
}

static void
txn_restore_split_fail_handler(gpointer data_for_children,
                               GSList* data_from_children,
                               GSList* sibling_data,
                               gpointer parent_data,
                               gpointer global_data,
                               gpointer *result,
                               const gchar *tag) {
  Split *s = (Split *) data_for_children;
  if(s) xaccSplitDestroy(s);
}

/****************************************************************************/
/* <guid> (lineage <split> <restore> <transaction>)
   
   restores a given split's guid.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split GUID if not duplicate.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_guid_end_handler(gpointer data_for_children,
                                   GSList* data_from_children,
                                   GSList* sibling_data,
                                   gpointer parent_data,
                                   gpointer global_data,
                                   gpointer *result,
                                   const gchar *tag) {
  Split *s = (Split *) parent_data;
  gchar *txt = NULL;
  GUID gid;
  gboolean ok;

  g_return_val_if_fail(s, FALSE);

  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &gid);
  g_free(txt);

  g_return_val_if_fail(ok, FALSE);

  if(xaccSplitLookup(&gid)) {
    return(FALSE);
  }

  xaccSplitSetGUID(s, &gid);
  return(TRUE);
}

/****************************************************************************/
/* <memo> (lineage <split> <restore> <transaction>)
   
   restores a given split's memo.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split description.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_memo_end_handler(gpointer data_for_children,
                                   GSList* data_from_children,
                                   GSList* sibling_data,
                                   gpointer parent_data,
                                   gpointer global_data,
                                   gpointer *result,
                                   const gchar *tag) {
  Split *s = (Split *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(s, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccSplitSetMemo(s, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <action> (lineage <split> <restore> <transaction>)
   
   restores a given split's action.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split action.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_action_end_handler(gpointer data_for_children,
                                     GSList* data_from_children,
                                     GSList* sibling_data,
                                     gpointer parent_data,
                                     gpointer global_data,
                                     gpointer *result,
                                     const gchar *tag) {
  Split *s = (Split *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(s, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  xaccSplitSetAction(s, txt);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <reconcile-state> (lineage <split> <restore> <transaction>)
   
   restores a given split's reconcile-state.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split reconcile-state.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_rest_split_reconcile_state_end_handler(gpointer data_for_children,
                                           GSList* data_from_children,
                                           GSList* sibling_data,
                                           gpointer parent_data,
                                           gpointer global_data,
                                           gpointer *result,
                                           const gchar *tag) {
  Split *s = (Split *) parent_data;
  gchar *txt = NULL;
  
  g_return_val_if_fail(s, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  if(strlen(txt) != 1) {
    g_free(txt);
    return(FALSE);
  }

  xaccSplitSetReconcile(s, txt[0]);
  g_free(txt);
  return(TRUE);
}

/****************************************************************************/
/* <reconcile-date> (lineage <split> <restore> <transaction>)
   
   restores a given split's reconcile-date.

   Just uses a generic_timespec parser, but with our own end handler.

   end: set reconcile-date.

 */

static gboolean
txn_rest_split_reconcile_date_end_handler(gpointer data_for_children,
                                          GSList* data_from_children,
                                          GSList* sibling_data,
                                          gpointer parent_data,
                                          gpointer global_data,
                                          gpointer *result,
                                          const gchar *tag) {
  Split *s = (Split *) parent_data;
  TimespecParseInfo *info = (TimespecParseInfo *) data_for_children;
  
  g_return_val_if_fail(info, FALSE);
  if(!s || !timespec_parse_ok(info)) {
    g_free(info);
    return(FALSE);
  }

  xaccSplitSetDateReconciledTS(s, &(info->ts));
  g_free(info);
  return(TRUE);
}

/****************************************************************************/
/* <account> (lineage <split> <restore> <transaction>)
   
   restores a given split's account.

   from parent: Split*
   for children: NA
   result: NA
   -----------
   start: NA
   characters: return string copy for accumulation in end handler.
   end: concatenate all chars and set as split account if GUID OK.

   cleanup-result: NA
   cleanup-chars: g_free the result string.
   fail: NA
   result-fail: NA
   chars-fail: g_free the result string.

 */

static gboolean
txn_restore_split_account_end_handler(gpointer data_for_children,
                                      GSList* data_from_children,
                                      GSList* sibling_data,
                                      gpointer parent_data,
                                      gpointer global_data,
                                      gpointer *result,
                                      const gchar *tag) {
  Split *s = (Split *) parent_data;
  Account *acct;
  gchar *txt = NULL;
  GUID gid;
  gboolean ok;
  
  g_return_val_if_fail(s, FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_guid(txt, &gid);
  g_free(txt);
  
  g_return_val_if_fail(ok, FALSE);
  
  acct = xaccAccountLookup(&gid);
  g_return_val_if_fail(acct, FALSE);

  xaccAccountInsertSplit(acct, s);
  return(TRUE);
}


/****************************************************************************/

static sixtp *
gnc_txn_restore_split_parser_new() {
  sixtp *top_level;
  sixtp *guid_pr;
  sixtp *memo_pr;
  sixtp *action_pr;
  sixtp *reconcile_state_pr;
  sixtp *reconcile_date_pr;
  sixtp *damount_pr;
  sixtp *value_pr;
  sixtp *account_pr;
  sixtp *tmp_pr;
  
  top_level = sixtp_new();
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_start(top_level, txn_restore_split_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_end(top_level, txn_restore_split_end_handler);
  sixtp_set_fail(top_level, txn_restore_split_fail_handler);
  sixtp_set_after_child(top_level, txn_restore_split_after_child_handler);

  /* <restore> <guid> */
  guid_pr = sixtp_new();
  if(!guid_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(guid_pr, generic_accumulate_chars);
  sixtp_set_end(guid_pr, txn_restore_split_guid_end_handler);
  sixtp_set_cleanup_chars(guid_pr, generic_free_result);
  sixtp_set_chars_fail(guid_pr, generic_free_result);
  sixtp_add_sub_parser(top_level, "guid", guid_pr);
  
  /* <restore> <memo> */
  memo_pr = sixtp_new();
  if(!memo_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(memo_pr, generic_accumulate_chars);
  sixtp_set_end(memo_pr, txn_restore_split_memo_end_handler);
  sixtp_set_cleanup_chars(memo_pr, generic_free_result);
  sixtp_set_chars_fail(memo_pr, generic_free_result);
  sixtp_add_sub_parser(top_level, "memo", memo_pr);

  /* <restore> <action> */
  action_pr = sixtp_new();
  if(!action_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(action_pr, generic_accumulate_chars);
  sixtp_set_end(action_pr, txn_restore_split_action_end_handler);
  sixtp_set_cleanup_chars(action_pr, generic_free_result);
  sixtp_set_chars_fail(action_pr, generic_free_result);
  sixtp_add_sub_parser(top_level, "action", action_pr);

  /* <restore> <reconcile-state> */
  reconcile_state_pr = sixtp_new();
  if(!reconcile_state_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(reconcile_state_pr, generic_accumulate_chars);
  sixtp_set_end(reconcile_state_pr,
                txn_rest_split_reconcile_state_end_handler);
  sixtp_set_cleanup_chars(reconcile_state_pr, generic_free_result);
  sixtp_set_chars_fail(reconcile_state_pr, generic_free_result);
  sixtp_add_sub_parser(top_level, "reconcile-state", reconcile_state_pr);

  /* <restore> <reconcile-date> */
  reconcile_date_pr =
    generic_timespec_parser_new(txn_rest_split_reconcile_date_end_handler);
  if(!reconcile_date_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "reconcile-date", reconcile_date_pr);
  
  /* <restore> <quantity> */
  damount_pr = generic_gnc_numeric_parser_new();
  if(!damount_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "quantity", damount_pr);

  /* <restore> <value> */
  value_pr = generic_gnc_numeric_parser_new();
  if(!value_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "value", value_pr);

  /* <restore> <account> */
  account_pr = sixtp_new();
  if(!account_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(account_pr, generic_accumulate_chars);
  sixtp_set_end(account_pr, txn_restore_split_account_end_handler);
  sixtp_set_cleanup_chars(account_pr, generic_free_result);
  sixtp_set_chars_fail(account_pr, generic_free_result);
  sixtp_add_sub_parser(top_level, "account", account_pr);

  /* <restore> <slots> */
  tmp_pr = kvp_frame_parser_new();
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(top_level, "slots", tmp_pr);

  return(top_level);
}

/***************************************************************************/

static sixtp *
gnc_transaction_parser_new() {
  sixtp *top_level;
  sixtp *restore_pr;
  sixtp *guid_pr;
  sixtp *num_pr;
  sixtp *date_posted_pr;
  sixtp *date_entered_pr;
  sixtp *description_pr;
  sixtp *split_pr;
  sixtp *tmp_pr;

  top_level = sixtp_new();
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_start(top_level, transaction_start_handler);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_after_child(top_level, txn_restore_after_child_handler);

  /* <restore> */
  restore_pr = sixtp_new();
  if(!restore_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_start(restore_pr, txn_restore_start_handler);
  sixtp_set_chars(restore_pr, allow_and_ignore_only_whitespace);
  sixtp_set_end(restore_pr, txn_restore_end_handler);
  sixtp_set_after_child(restore_pr, txn_restore_after_child_handler);
  sixtp_set_fail(restore_pr, txn_restore_fail_handler);
  sixtp_add_sub_parser(top_level, "restore", restore_pr);

  /* <restore> <guid> */
  guid_pr = sixtp_new();
  if(!guid_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(guid_pr, generic_accumulate_chars);
  sixtp_set_end(guid_pr, txn_restore_guid_end_handler);
  sixtp_set_cleanup_chars(guid_pr, generic_free_result);
  sixtp_set_chars_fail(guid_pr, generic_free_result);
  sixtp_add_sub_parser(restore_pr, "guid", guid_pr);
  
  /* <restore> <num> */
  num_pr = sixtp_new();
  if(!num_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(num_pr, generic_accumulate_chars);
  sixtp_set_end(num_pr, txn_restore_num_end_handler);
  sixtp_set_cleanup_chars(num_pr, generic_free_result);
  sixtp_set_chars_fail(num_pr, generic_free_result);
  sixtp_add_sub_parser(restore_pr, "num", num_pr);

  /* <restore> <date-posted> */
  date_posted_pr =
    generic_timespec_parser_new(txn_rest_date_posted_end_handler);
  if(!date_posted_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "date-posted", date_posted_pr);
  
  /* <restore> <date-entered> */
  date_entered_pr =
    generic_timespec_parser_new(txn_rest_date_entered_end_handler);
  if(!date_entered_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "date-entered", date_entered_pr);

  /* <restore> <description> */
  description_pr = sixtp_new();
  if(!description_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_set_chars(description_pr, generic_accumulate_chars);
  sixtp_set_end(description_pr, txn_restore_description_end_handler);
  sixtp_set_cleanup_chars(description_pr, generic_free_result);
  sixtp_set_chars_fail(description_pr, generic_free_result);
  sixtp_add_sub_parser(restore_pr, "description", description_pr);

  /* <restore> <slots> */
  tmp_pr = kvp_frame_parser_new();
  if(!tmp_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "slots", tmp_pr);

  /* <restore> <split> */
  split_pr = gnc_txn_restore_split_parser_new();
  if(!split_pr) {
    sixtp_destroy(top_level);
    return(NULL);
  }
  sixtp_add_sub_parser(restore_pr, "split", split_pr);

  return(top_level);
}

/***************************************************************************/

static sixtp*
ledger_data_parser_new() {
  /* FIXME: this may leak memory - need to allocate at usage time as
     with the other parser creation functions above... */

  sixtp *ledger_data_pr = sixtp_new();
  sixtp *acc_pr = sixtp_new();
  sixtp *acc_restore_pr = sixtp_new();
  sixtp *acc_restore_name_pr = sixtp_new();
  sixtp *acc_restore_guid_pr = sixtp_new();
  sixtp *acc_restore_type_pr = sixtp_new();
  sixtp *acc_restore_code_pr = sixtp_new();
  sixtp *acc_restore_description_pr = sixtp_new();
  sixtp *acc_restore_notes_pr = sixtp_new();
  sixtp *acc_restore_currency_pr;
  sixtp *acc_restore_security_pr;
  sixtp *acc_restore_parent_pr = sixtp_new();
  sixtp *acc_restore_parent_guid_pr;
  sixtp *acc_restore_slots_pr;
  sixtp *tmp_pr;

  g_return_val_if_fail(ledger_data_pr, NULL);
  g_return_val_if_fail(acc_pr, NULL);
  g_return_val_if_fail(acc_restore_pr, NULL);
  g_return_val_if_fail(acc_restore_name_pr, NULL);
  g_return_val_if_fail(acc_restore_guid_pr, NULL);
  g_return_val_if_fail(acc_restore_type_pr, NULL);
  g_return_val_if_fail(acc_restore_code_pr, NULL);
  g_return_val_if_fail(acc_restore_description_pr, NULL);
  g_return_val_if_fail(acc_restore_notes_pr, NULL);
  g_return_val_if_fail(acc_restore_parent_pr, NULL);
  
  /* <ledger-data> */
  sixtp_set_start(ledger_data_pr, ledger_data_start_handler);
  sixtp_set_chars(ledger_data_pr, allow_and_ignore_only_whitespace);
  sixtp_set_end(ledger_data_pr, ledger_data_end_handler);
  sixtp_set_cleanup_result(ledger_data_pr, ledger_data_result_cleanup);
  sixtp_set_fail(ledger_data_pr, ledger_data_fail_handler);
  sixtp_set_result_fail(ledger_data_pr, ledger_data_result_cleanup);

  /* <commodity> */
  tmp_pr = commodity_restore_parser_new();
  if(!tmp_pr) {
    sixtp_destroy(ledger_data_pr);
    return(NULL);
  }
  sixtp_add_sub_parser(ledger_data_pr, "commodity", tmp_pr);  

  /* <account> */
  sixtp_set_start(acc_pr, account_start_handler);
  sixtp_set_chars(acc_pr, allow_and_ignore_only_whitespace);
  
  /* <account> <restore> */
  sixtp_set_start(acc_restore_pr, account_restore_start_handler);
  sixtp_set_chars(acc_restore_pr, allow_and_ignore_only_whitespace);
  sixtp_set_end(acc_restore_pr, account_restore_end_handler);
  sixtp_set_fail(acc_restore_pr, account_restore_fail_handler);
  sixtp_set_after_child(acc_restore_pr, account_restore_after_child_handler);
  
  /* <account> <restore> <name> */
  sixtp_set_chars(acc_restore_name_pr, generic_accumulate_chars);  
  sixtp_set_chars_fail(acc_restore_name_pr, generic_free_result);
  sixtp_set_cleanup_chars(acc_restore_name_pr, generic_free_result);
  sixtp_set_end(acc_restore_name_pr, acc_restore_name_end_handler);
  
  /* <account> <restore> <guid> */
  sixtp_set_chars(acc_restore_guid_pr, generic_accumulate_chars);  
  sixtp_set_chars_fail(acc_restore_guid_pr, generic_free_result);
  sixtp_set_cleanup_chars(acc_restore_guid_pr, generic_free_result);
  sixtp_set_end(acc_restore_guid_pr, acc_restore_guid_end_handler);
  
  /* <account> <restore> <type> */
  sixtp_set_chars(acc_restore_type_pr, generic_accumulate_chars);  
  sixtp_set_chars_fail(acc_restore_type_pr, generic_free_result);
  sixtp_set_cleanup_chars(acc_restore_type_pr, generic_free_result);
  sixtp_set_end(acc_restore_type_pr, acc_restore_type_end_handler);
  
  /* <account> <restore> <code> */
  sixtp_set_chars(acc_restore_code_pr, generic_accumulate_chars);  
  sixtp_set_chars_fail(acc_restore_code_pr, generic_free_result);
  sixtp_set_cleanup_chars(acc_restore_code_pr, generic_free_result);
  sixtp_set_end(acc_restore_code_pr, acc_restore_code_end_handler);
  
  /* <account> <restore> <description> */
  sixtp_set_chars(acc_restore_description_pr, generic_accumulate_chars);  
  sixtp_set_chars_fail(acc_restore_description_pr, generic_free_result);
  sixtp_set_cleanup_chars(acc_restore_description_pr, generic_free_result);
  sixtp_set_end(acc_restore_description_pr,
                acc_restore_description_end_handler);
  
  /* <account> <restore> <notes> */
  sixtp_set_chars(acc_restore_notes_pr, generic_accumulate_chars);  
  sixtp_set_chars_fail(acc_restore_notes_pr, generic_free_result);
  sixtp_set_cleanup_chars(acc_restore_notes_pr, generic_free_result);
  sixtp_set_end(acc_restore_notes_pr, acc_restore_notes_end_handler);
  
  /* <account> <restore> <currency> */
  acc_restore_currency_pr = generic_gnc_commodity_lookup_parser_new();
  if(!acc_restore_currency_pr) {
    sixtp_destroy(ledger_data_pr);
    return(NULL);
  }
  sixtp_add_sub_parser(acc_restore_pr, "currency", acc_restore_currency_pr);
  
  /* <account> <restore> <security> */
  acc_restore_security_pr = generic_gnc_commodity_lookup_parser_new();
  if(!acc_restore_security_pr) {
    sixtp_destroy(ledger_data_pr);
    return(NULL);
  }
  sixtp_add_sub_parser(acc_restore_pr, "security", acc_restore_security_pr);

  /* <account> <restore> <parent> */
  sixtp_set_chars(ledger_data_pr, allow_and_ignore_only_whitespace);
  sixtp_set_end(acc_restore_parent_pr, acc_restore_parent_end_handler);
  
  /* <account> <restore> <parent> <guid> */
  acc_restore_parent_guid_pr = generic_guid_parser_new();
  if(!acc_restore_parent_guid_pr) {
    sixtp_destroy(ledger_data_pr);
    return(NULL);
  }
  sixtp_add_sub_parser(acc_restore_parent_pr, "guid",
                       acc_restore_parent_guid_pr);
  
  /* <account> <restore> <slots> */
  acc_restore_slots_pr = kvp_frame_parser_new();
  if(!acc_restore_slots_pr) {
    sixtp_destroy(ledger_data_pr);
    return(NULL);
  }
  sixtp_add_sub_parser(acc_restore_pr, "slots", acc_restore_slots_pr);

  /* <transaction> */
  {
    sixtp *transaction_pr = gnc_transaction_parser_new();
    if(!transaction_pr) {
      /* FIXME: need more cleanup here... */
      return(NULL);
    }
    sixtp_add_sub_parser(ledger_data_pr, "transaction", transaction_pr);
  }

  /* parser structure */
  sixtp_add_sub_parser(acc_restore_pr, "parent", acc_restore_parent_pr);
  sixtp_add_sub_parser(acc_restore_pr, "notes", acc_restore_notes_pr);
  sixtp_add_sub_parser(acc_restore_pr, "description", acc_restore_description_pr);
  sixtp_add_sub_parser(acc_restore_pr, "code", acc_restore_code_pr);
  sixtp_add_sub_parser(acc_restore_pr, "type", acc_restore_type_pr);
  sixtp_add_sub_parser(acc_restore_pr, "guid", acc_restore_guid_pr);
  sixtp_add_sub_parser(acc_restore_pr, "name", acc_restore_name_pr);

  sixtp_add_sub_parser(acc_pr, "restore", acc_restore_pr);
  sixtp_add_sub_parser(ledger_data_pr, "account", acc_pr);


  return(ledger_data_pr);
}


/****************************************************************************/
/* <version> (lineage <gnc>)
   
   Fancy and strange - look for an integer version number.  If we get
   one, then modify the parent parser to handle the input.

   this is a simple_chars_only_parser with an end handler that grabs
   the version number and tweaks the parser, if possible.

 */

static gboolean
gnc_parser_configure_for_input_version(GNCParseStatus *status, gint64 version) {

  status->version = version;
  status->seen_version = TRUE;

  /* Check for a legal version here. */
  if(version != 1) {
    status->error = GNC_PARSE_ERR_BAD_VERSION;
    return(FALSE);
  }
  
  /* Now set up the parser based on the version. */
  
  /* add <ledger-data> */
  {
    sixtp *ledger_data_pr = ledger_data_parser_new();
    g_return_val_if_fail(ledger_data_pr, FALSE);
    sixtp_add_sub_parser(status->gnc_parser, "ledger-data", ledger_data_pr);
  }

  return(TRUE);
}

static gboolean
gnc_version_end_handler(gpointer data_for_children,
                        GSList* data_from_children,
                        GSList* sibling_data,
                        gpointer parent_data,
                        gpointer global_data,
                        gpointer *result,
                        const gchar *tag) {
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  gint64 version;
  gboolean ok;
  gchar *txt;
  
  g_return_val_if_fail(pstatus, FALSE);
  if(pstatus->seen_version) return(FALSE);
  
  txt = concatenate_child_result_chars(data_from_children);
  g_return_val_if_fail(txt, FALSE);
  
  ok = string_to_gint64(txt, &version);
  g_free(txt);
  g_return_val_if_fail(ok, FALSE);

  if(!gnc_parser_configure_for_input_version(pstatus, version)) return(FALSE);

  return(TRUE);
}

static sixtp *gnc_version_parser_new() {
  return(simple_chars_only_parser_new(gnc_version_end_handler));
}

/****************************************************************************/
/* <gnc> (lineage #f)
   
   Takes the results from various children and puts them in the
   global_data result structure.

   from parent: NA
   for children: NA
   result: NA
   -----------
   start: NA
   before-child: make sure we don't get two ledger-data's (not allowed ATM).
   after-child: if a ledger-data child, parse_data->account_group = *result.
   characters: allow_and_ignore_only_whitespace
   end: NA

   cleanup-result: NA
   cleanup-chars: NA
   fail: NA
   result-fail: NA
   chars-fail: NA

 */

static gboolean
gnc_parser_before_child_handler(gpointer data_for_children,
                              GSList* data_from_children,
                              GSList* sibling_data,
                              gpointer parent_data,
                              gpointer global_data,
                              gpointer *result,
                              
                              const gchar *tag,
                              const gchar *child_tag) {  
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  
  g_return_val_if_fail(pstatus, FALSE);
  if(strcmp(child_tag, "ledger-data") == 0) {
    if(pstatus->account_group) {
      return(FALSE);
    }
  }
  return(TRUE);
}

static gboolean
gnc_parser_after_child_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             
                             const gchar *tag,
                             const gchar *child_tag,
                             sixtp_child_result *child_result) {  
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  
  g_return_val_if_fail(pstatus, FALSE);
  if(strcmp(child_tag, "ledger-data") == 0) {
    g_return_val_if_fail(child_result, FALSE);
    g_return_val_if_fail(child_result->data, FALSE);
    pstatus->account_group = (AccountGroup *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
  return(TRUE);
}

static sixtp*
gnc_parser_new() {
  sixtp *top_level = sixtp_new();
  
  g_return_val_if_fail(top_level, NULL);
  sixtp_set_chars(top_level, allow_and_ignore_only_whitespace);
  sixtp_set_before_child(top_level, gnc_parser_before_child_handler);
  sixtp_set_after_child(top_level, gnc_parser_after_child_handler);
  return(top_level);
}

/****************************************************************************/

gboolean
gncxml_read(const gchar *filename,
            AccountGroup **result_group) {
  /* fixme: this should be broken up into sub-functions later. */

  gboolean parse_ok;
  gpointer parse_result;
  sixtp *top_level_pr;
  sixtp *gnc_pr;
  sixtp *gnc_version_pr;
  GNCParseStatus global_parse_status;

  g_return_val_if_fail(filename, FALSE);
  g_return_val_if_fail(result_group, FALSE);

  /* top-level: This is just a dummy node.  It doesn't do anything.
     For now, the result is communicated through the global_data
     parser. */
  top_level_pr = sixtp_new();
  g_return_val_if_fail(top_level_pr, FALSE);
  sixtp_set_chars(top_level_pr, allow_and_ignore_only_whitespace);

  /* <gnc> */
  gnc_pr = gnc_parser_new();
  if(!gnc_pr) {
    sixtp_destroy(top_level_pr);
    return(FALSE);
  }
  sixtp_add_sub_parser(top_level_pr, "gnc", gnc_pr);

  /* <version> */
  gnc_version_pr = gnc_version_parser_new();
  if(!gnc_version_pr) {
    sixtp_destroy(top_level_pr);
    return(FALSE);
  }
  sixtp_add_sub_parser(gnc_pr, "version", gnc_version_pr);

  global_parse_status.seen_version = FALSE;
  global_parse_status.gnc_parser = gnc_pr;
  global_parse_status.account_group = NULL;
  global_parse_status.error = GNC_PARSE_ERR_NONE;
  parse_result = NULL;

  parse_ok = sixtp_parse_file(top_level_pr,
                              filename,
                              NULL,
                              &global_parse_status,
                              &parse_result);
  
  sixtp_destroy(top_level_pr);

  if(parse_ok && global_parse_status.account_group) {
    *result_group = global_parse_status.account_group;
    return(TRUE);
  } else {
    return(FALSE);
  }
}

/****************************************************************************/

gboolean
is_gncxml_file(const gchar *filename) {
  FILE *f = NULL;
  char first_chunk[256];
  const char* cursor = NULL;
  ssize_t num_read;
  gboolean result = FALSE;

  if(!filename) goto cleanup_and_exit;

  f = fopen(filename, "r");
  if(!f) goto cleanup_and_exit;

  num_read = fread(first_chunk, sizeof(char), sizeof(first_chunk) - 1, f);
  if(num_read == 0) goto cleanup_and_exit;
  first_chunk[num_read] = '\0';

  cursor = first_chunk;
  while(*cursor && isspace(*cursor)) cursor++;
  
  if(cursor && strncmp(cursor, "<?xml", 5) == 0) {
    result = TRUE;
  }

 cleanup_and_exit:
  if(f) fclose(f);
  return(result);
}
