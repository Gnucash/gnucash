/*
 * io-gncxml-r.c -- read XML-format gnucash data file
 *
 * Initial code by Rob l. Browning 4Q 2000
 * Tuneups by James Lewis Moss Dec 2000
 * Excessive hacking inas Vepstas January 2001
 *
 */

#include "config.h"

#include <string.h>

#include <glib.h>

#ifdef HAVE_XML_VERSION_HEADER
#include <libxml/xmlversion.h>
#endif

#if defined(LIBXML_VERSION) && LIBXML_VERSION >= 20000

#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlmemory.h>
#include <libxml/parserInternals.h>
#ifndef xmlChildrenNode
#define xmlChildrenNode children
#define xmlRootNode children
#endif

#else

#include <gnome-xml/tree.h>
#include <gnome-xml/parser.h>
#include <gnome-xml/xmlmemory.h>
#include <gnome-xml/parserInternals.h>
#ifndef xmlChildrenNode
#define xmlChildrenNode childs
#define xmlRootNode root
#endif

#endif

#include "Account.h"
#include "Query.h"
#include "gnc-engine-util.h"

#include "io-gncxml.h"

#include "sixtp.h"
#include "sixtp-stack.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"

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

static short module = MOD_IO;

static xmlEntityPtr
sixtp_sax_get_entity_handler(void *user_data, const CHAR *name) {
  return xmlGetPredefinedEntity(name);
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

  PERR("parse failed at \n");
  sixtp_print_frame_stack(sax_data->stack, stderr);

  while(*stack) {
    sixtp_stack_frame *current_frame = (sixtp_stack_frame *) (*stack)->data;

    /* cleanup the current frame */
    if(current_frame->parser->fail_handler) {
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

      current_frame->parser->fail_handler(current_frame->data_for_children,
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
      if(cresult->fail_handler) {
          cresult->fail_handler(cresult);
      }
    }

    *stack = sixtp_pop_and_destroy_frame(*stack);
  }
}

/* ========================================================== */
/* initialize misc structures so that we can call the libxml 
 * parser.
 */

static gboolean
sixtp_setup_parser (sixtp *sixtp,
                    gpointer data_for_top_level,
                    gpointer global_data,
                    xmlSAXHandler *sax_handler,
                    sixtp_sax_data *sax_data,
                    sixtp_stack_frame *top_frame
                    ) 
{
  memset(sax_handler, '\0', sizeof(xmlSAXHandler));
  sax_handler->startElement = sixtp_sax_start_handler;
  sax_handler->endElement = sixtp_sax_end_handler;
  sax_handler->characters = sixtp_sax_characters_handler;
  sax_handler->getEntity = sixtp_sax_get_entity_handler;
  
  memset(sax_data, '\0', sizeof(sixtp_sax_data));
  sax_data->parsing_ok = TRUE;
  sax_data->stack = NULL;
  sax_data->global_data = global_data;
  
  top_frame->parser = sixtp;
  top_frame->tag = NULL;
  top_frame->data_from_children = NULL;
  top_frame->data_for_children = NULL;
  top_frame->frame_data = NULL;
  
  sax_data->stack = g_slist_prepend(sax_data->stack, (gpointer) top_frame);
  
  if(sixtp->start_handler) {
    sax_data->parsing_ok =
      sixtp->start_handler(NULL,
                           data_for_top_level,
                           sax_data->global_data,
                           &top_frame->data_for_children,
                           &top_frame->frame_data,
                           NULL, NULL);
  }
  
  if(!sax_data->parsing_ok)  {
    PERR ("parsing catastrophe");
    sixtp_handle_catastrophe(sax_data);
    return(FALSE);
  }

  return (TRUE);
}
  

/* ========================================================== */
/* misc structure celanup after parsing */

static gboolean
sixtp_teardown_parser(sixtp *sixtp,
                      gpointer data_for_top_level,
                      gpointer global_data,
                      gpointer *parse_result, 
                      sixtp_sax_data *sax_data,
                      sixtp_stack_frame *top_frame)
{
  if(!sax_data->parsing_ok) {
    PERR ("couldn't parse, handle catastrophe");
    sixtp_handle_catastrophe(sax_data);
    return(FALSE);
  }

  if(sixtp->end_handler) {
    sax_data->parsing_ok =
      sixtp->end_handler(top_frame->data_for_children,
                         top_frame->data_from_children,
                         NULL,
                         data_for_top_level,
                         sax_data->global_data,
                         &top_frame->frame_data,
                         NULL);
  
    if(!sax_data->parsing_ok) {
      PERR ("couldn't call end handler, cleanup catastrophe");
      sixtp_handle_catastrophe(sax_data);
      return(FALSE);
    }
  }

  /* put the result where the caller can see it */
  if(top_frame->frame_data) *parse_result = top_frame->frame_data;

  {
    GSList *lp = NULL;
    for(lp = sax_data->stack; lp; lp = lp->next)
      sixtp_stack_frame_destroy((sixtp_stack_frame *) lp->data);
  }
  g_slist_free(sax_data->stack);
  return(TRUE);
}

/* ========================================================== */
/* parse the contents of a file */

static gboolean
sixtp_parse_file(sixtp *sixtp,
                 const char *filename,
                 gpointer data_for_top_level,
                 gpointer global_data,
                 gpointer *parse_result) 
{
  xmlSAXHandler sax_handler;
  sixtp_sax_data sax_data;
  sixtp_stack_frame *top_frame = NULL;
  int rc;
  
  /* hack alert -- XXX -- where is top_frame released?? */
  /* looks like a mem leak to me ... */
  top_frame = g_new0(sixtp_stack_frame, 1);
  rc = sixtp_setup_parser (sixtp,
                           data_for_top_level,
                           global_data,
                           &sax_handler,
                           &sax_data,
                           top_frame);

  if(!rc) return(FALSE);

  xmlSAXUserParseFile(&sax_handler, &sax_data, filename);

  rc = sixtp_teardown_parser(sixtp,
                             data_for_top_level,
                             global_data,
                             parse_result, 
                             &sax_data,
                             top_frame);

  return rc;
}

/* ========================================================== */
/* parse the contents of a buffer in memory */

static gboolean
sixtp_parse_buffer(sixtp *sixtp,
                   char *bufp,
                   int bufsz,
                   gpointer data_for_top_level,
                   gpointer global_data,
                   gpointer *parse_result) 
{
  xmlSAXHandler sax_handler;
  sixtp_sax_data sax_data;
  sixtp_stack_frame *top_frame = NULL;
  int rc;
  
  /* hack alert -- XXX -- where is top_frame released?? */
  /* looks like a mem leak to me ... */
  top_frame = g_new0(sixtp_stack_frame, 1);
  rc = sixtp_setup_parser (sixtp,
                           data_for_top_level,
                           global_data,
                           &sax_handler,
                           &sax_data,
                           top_frame);

  if(!rc) return(FALSE);

  xmlSAXUserParseMemory(&sax_handler, &sax_data, bufp, bufsz);

  rc = sixtp_teardown_parser(sixtp,
                             data_for_top_level,
                             global_data,
                             parse_result, 
                             &sax_data,
                             top_frame);

  return rc;
}

/* ========================================================== */

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

  /* The query */
  Query *query;
  
  GNCParseErr error;
} GNCParseStatus;


/* ================================================================= */
/* <version> (lineage <gnc>)
   
   Fancy and strange - look for an integer version number.  If we get
   one, then modify the parent parser to handle the input.

   this is a simple_chars_only_parser with an end handler that grabs
   the version number and tweaks the parser, if possible.

 */

static gboolean
gnc_parser_configure_for_input_version(GNCParseStatus *status, gint64 version) 
{

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

  /* add <query-server> */
  {
    sixtp *query_server_pr = query_server_parser_new();
    g_return_val_if_fail(query_server_pr, FALSE);
    sixtp_add_sub_parser(status->gnc_parser, "query-server", query_server_pr);
  }

  return(TRUE);
}

static gboolean
gnc_version_end_handler(gpointer data_for_children,
                        GSList  *data_from_children, GSList *sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
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

static sixtp *
gnc_version_parser_new(void) 
{
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

   Similarly, only one query is allowed ... 
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
                              const gchar *child_tag) 
{  
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  
  g_return_val_if_fail(pstatus, FALSE);

  if(strcmp(child_tag, "ledger-data") == 0) {
    if(pstatus->account_group) {
      return(FALSE);
    }
  }

  if(strcmp(child_tag, "query-server") == 0) {
    if(pstatus->query) return(FALSE);
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
                               sixtp_child_result *child_result)
{
  GNCParseStatus *pstatus = (GNCParseStatus *) global_data;
  g_return_val_if_fail(pstatus, FALSE);

  if(strcmp(child_tag, "ledger-data") == 0) 
  {
    g_return_val_if_fail(child_result, FALSE);
    g_return_val_if_fail(child_result->data, FALSE);
    pstatus->account_group = (AccountGroup *) child_result->data;
    child_result->should_cleanup = FALSE;
  }

  if(strcmp(child_tag, "query-server") == 0) 
  {
    g_return_val_if_fail(child_result, FALSE);
    g_return_val_if_fail(child_result->data, FALSE);
    pstatus->query = (Query *) child_result->data;
    child_result->should_cleanup = FALSE;
  }
  return(TRUE);
}

static sixtp*
gnc_parser_new(void) 
{
    return sixtp_set_any(
        sixtp_new(), FALSE,
        SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
        SIXTP_BEFORE_CHILD_HANDLER_ID, gnc_parser_before_child_handler,
        SIXTP_AFTER_CHILD_HANDLER_ID, gnc_parser_after_child_handler,
        SIXTP_NO_MORE_HANDLERS);
}

/* ================================================================== */

static sixtp *
gncxml_setup_for_read (GNCParseStatus *global_parse_status)
{

  sixtp *top_level_pr;
  sixtp *gnc_pr;
  sixtp *gnc_version_pr;

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
    return(NULL);
  }
  sixtp_add_sub_parser(top_level_pr, "gnc", gnc_pr);

  /* <version> */
  gnc_version_pr = gnc_version_parser_new();
  if(!gnc_version_pr) {
    sixtp_destroy(top_level_pr);
    return(NULL);
  }
  sixtp_add_sub_parser(gnc_pr, "version", gnc_version_pr);

  global_parse_status->seen_version = FALSE;
  global_parse_status->gnc_parser = gnc_pr;
  global_parse_status->account_group = NULL;
  global_parse_status->query = NULL;
  global_parse_status->error = GNC_PARSE_ERR_NONE;

  return top_level_pr;
}

/* ================================================================== */

gboolean
gncxml_read(const gchar *filename,
            AccountGroup **result_group) 
{
  gboolean parse_ok;
  gpointer parse_result = NULL;
  sixtp *top_level_pr;
  GNCParseStatus global_parse_status;

  g_return_val_if_fail(filename, FALSE);
  g_return_val_if_fail(result_group, FALSE);

  top_level_pr = gncxml_setup_for_read (&global_parse_status);
  g_return_val_if_fail(top_level_pr, FALSE);

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

/* ================================================================== */

static GNCParseStatus*
read_from_buf(char *bufp, int bufsz)
{
  gboolean parse_ok;
  gpointer parse_result = NULL;
  sixtp *top_level_pr;
  GNCParseStatus *global_parse_status;

  g_return_val_if_fail(bufp, NULL);

  global_parse_status = g_new0(GNCParseStatus, 1);
  g_return_val_if_fail(global_parse_status, NULL);
  
  top_level_pr = gncxml_setup_for_read (global_parse_status);
  g_return_val_if_fail(top_level_pr, NULL);

  parse_ok = sixtp_parse_buffer(top_level_pr,
                                bufp,
                                bufsz,
                                NULL,
                                global_parse_status,
                                &parse_result);
  
  sixtp_destroy(top_level_pr);

  if (parse_ok)
      return global_parse_status;
  else
      return NULL;
}

AccountGroup *
gncxml_read_from_buf (char * bufp, int bufsz)
{
    AccountGroup *ret;
    GNCParseStatus *info;

    info = read_from_buf(bufp, bufsz);
    g_return_val_if_fail(info, NULL);

    ret = info->account_group;

    g_free(info);

    return ret;
}

/* ================================================================== */

Query *
gncxml_read_query (char * bufp, int bufsz)
{
    Query *ret;
    GNCParseStatus *info;

    info = read_from_buf(bufp, bufsz);
    g_return_val_if_fail(info, NULL);

    ret = info->query;

    g_free(info);

    return ret;
}

/* ================================================================== */

gboolean
is_gncxml_file(const gchar *filename) 
{
  FILE *f = NULL;
  char first_chunk[256];
  const char* cursor = NULL;
  ssize_t num_read;
  gboolean result = FALSE;

  g_return_val_if_fail(filename, result);

  f = fopen(filename, "r");
  g_return_val_if_fail(f, result);

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

/* ======================= END OF FILE ============================== */
