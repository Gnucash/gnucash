#include "config.h"

#include <string.h>

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"

#include "kvp_frame.h"

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
kvp_value_result_cleanup(sixtp_child_result *cr)
{  
  kvp_value *v = (kvp_value *) cr->data;;
  if(v) kvp_value_delete(v);
}

static sixtp*
simple_kvp_value_parser_new(sixtp_end_handler end_handler) 
{
    return sixtp_set_any(sixtp_new(), FALSE,
                         SIXTP_CHARACTERS_HANDLER_ID,
                         generic_accumulate_chars,
                         SIXTP_END_HANDLER_ID, end_handler,
                         SIXTP_CLEANUP_RESULT_ID, kvp_value_result_cleanup,
                         SIXTP_CLEANUP_CHARS_ID, sixtp_child_free_data,
                         SIXTP_RESULT_FAIL_ID, kvp_value_result_cleanup,
                         SIXTP_CHARS_FAIL_ID, sixtp_child_free_data,
                         SIXTP_NO_MORE_HANDLERS);
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

/* ------------------------------------------------------------ */
/* generic type copnversion for kvp types */
#define KVP_CVT_VALUE(TYPE)					\
{								\
  gchar *txt = NULL;						\
  TYPE val;							\
  kvp_value *kvpv;						\
  gboolean ok;							\
								\
  txt = concatenate_child_result_chars(data_from_children);	\
  g_return_val_if_fail(txt, FALSE);				\
  								\
  ok = (gboolean) string_to_##TYPE(txt, &val);			\
  g_free(txt);							\
  g_return_val_if_fail(ok, FALSE);				\
								\
  kvpv = kvp_value_new_##TYPE(val);				\
  g_return_val_if_fail(kvpv, FALSE);				\
    								\
  *result = kvpv;						\
  return(TRUE);							\
}
/* ------------------------------------------------------------ */

static gboolean
gint64_kvp_value_end_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) 
{
    KVP_CVT_VALUE (gint64);
}


static sixtp*
gint64_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(gint64_kvp_value_end_handler));
}

static gboolean
double_kvp_value_end_handler(gpointer data_for_children,
                             GSList* data_from_children,
                             GSList* sibling_data,
                             gpointer parent_data,
                             gpointer global_data,
                             gpointer *result,
                             const gchar *tag) 
{
    KVP_CVT_VALUE (double);
}

static sixtp*
double_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(double_kvp_value_end_handler));
}

static gboolean
gnc_numeric_kvp_value_end_handler(gpointer data_for_children,
                                  GSList* data_from_children,
                                  GSList* sibling_data,
                                  gpointer parent_data,
                                  gpointer global_data,
                                  gpointer *result,
                                  const gchar *tag) 
{
    KVP_CVT_VALUE (gnc_numeric);
}

static sixtp*
gnc_numeric_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(gnc_numeric_kvp_value_end_handler));
}

static gboolean
string_kvp_value_end_handler(gpointer data_for_children,
                        GSList* data_from_children,
                        GSList* sibling_data,
                        gpointer parent_data,
                        gpointer global_data,
                        gpointer *result,
                        const gchar *tag) 
{
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
string_kvp_value_parser_new(void) {
  return(simple_kvp_value_parser_new(string_kvp_value_end_handler));
}

/* the guid handler is almost the same as above, but has 
 * inconsistent type handling */
static gboolean
guid_kvp_value_end_handler(gpointer data_for_children,
                           GSList* data_from_children,
                           GSList* sibling_data,
                           gpointer parent_data,
                           gpointer global_data,
                           gpointer *result,
                           const gchar *tag) 
{
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
guid_kvp_value_parser_new(void) {
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
                                 GSList  *data_from_children, GSList *sibling_data,
                                 gpointer parent_data, gpointer global_data,
                                 gpointer *result, const gchar *tag)
{
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
hex_binary_kvp_value_parser_new(void) {
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
                             GSList  *data_from_children, GSList *sibling_data,
                             gpointer parent_data, gpointer global_data,
                             gpointer *result, const gchar *tag)
{
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
binary_kvp_value_parser_new(void) 
{
    return sixtp_add_some_sub_parsers(
        sixtp_set_any(sixtp_new(), FALSE,
                      SIXTP_CHARACTERS_HANDLER_ID,
                      allow_and_ignore_only_whitespace,
                      SIXTP_END_HANDLER_ID, kvp_frame_binary_end_handler,
                      SIXTP_CLEANUP_RESULT_ID, kvp_value_result_cleanup,
                      SIXTP_RESULT_FAIL_ID, kvp_value_result_cleanup,
                      SIXTP_NO_MORE_HANDLERS),
        TRUE,
        "hex", hex_binary_kvp_value_parser_new(),
        0);
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
                            GSList  *data_from_children, GSList *sibling_data,
                            gpointer parent_data, gpointer global_data,
                            gpointer *result, const gchar *tag)
{
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

/* ---------------------------------------------- */
#define KVP_TOKEN(NAME,TOK)			\
  child_pr = NAME##_kvp_value_parser_new();	\
  g_return_val_if_fail(child_pr, FALSE);	\
  sixtp_add_sub_parser(p, TOK, child_pr);
/* ---------------------------------------------- */


static gboolean
add_all_kvp_value_parsers_as_sub_nodes(sixtp *p,
                                       sixtp *kvp_frame_parser,
                                       sixtp *glist_parser) {
  sixtp *child_pr;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(kvp_frame_parser, FALSE);

  KVP_TOKEN(gint64, "gint64");
  KVP_TOKEN(double, "double");
  KVP_TOKEN(gnc_numeric, "numeric");
  KVP_TOKEN(string, "string");
  KVP_TOKEN(guid,   "guid");
  KVP_TOKEN(binary, "binary");

  sixtp_add_sub_parser(p, "glist", glist_parser);
  sixtp_add_sub_parser(p, "frame", kvp_frame_parser);

  return(TRUE);
}

static sixtp*
glist_kvp_value_parser_new(sixtp *kvp_frame_parser) {
    sixtp *top_level = sixtp_set_any(
        sixtp_new(), FALSE,
        SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
        SIXTP_END_HANDLER_ID, glist_kvp_value_end_handler,
        SIXTP_CLEANUP_RESULT_ID, kvp_value_result_cleanup,
        SIXTP_RESULT_FAIL_ID, kvp_value_result_cleanup,
        SIXTP_NO_MORE_HANDLERS);
    if(!top_level)
    {
        return NULL;
    }
  
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
                           GSList  *data_from_children, GSList *sibling_data,
                           gpointer parent_data, gpointer global_data,
                           gpointer *result, const gchar *tag)
{
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
  sixtp *top_level;
  sixtp *child_pr;
  sixtp *glist_pr;

  g_return_val_if_fail(kvp_frame_parser, NULL);

  if(!(top_level = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
           SIXTP_END_HANDLER_ID, kvp_frame_slot_end_handler,
           SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

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
kvp_frame_start_handler(GSList* sibling_data, gpointer parent_data,
                        gpointer global_data, gpointer *data_for_children,
                        gpointer *result, const gchar *tag, gchar **attrs)
{
  kvp_frame *f = kvp_frame_new();
  g_return_val_if_fail(f, FALSE);
  *data_for_children = f;
  return(TRUE);
}

static gboolean
kvp_frame_end_handler(gpointer data_for_children,
                      GSList  *data_from_children, GSList *sibling_data,
                      gpointer parent_data, gpointer global_data,
                      gpointer *result, const gchar *tag)
{
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
                         const gchar *tag)
{
  kvp_frame *f = (kvp_frame *) data_for_children;
  if(f) kvp_frame_delete(f);
}

static void
kvp_frame_result_cleanup(sixtp_child_result *cr)
{
  kvp_frame *f = (kvp_frame *) cr->data;;
  if(f) kvp_frame_delete(f);
}

sixtp*
kvp_frame_parser_new(void) 
{
  sixtp *top_level;

  if(!(top_level = sixtp_set_any(
           sixtp_new(), FALSE,
           SIXTP_START_HANDLER_ID, kvp_frame_start_handler,
           SIXTP_CHARACTERS_HANDLER_ID, allow_and_ignore_only_whitespace,
           SIXTP_END_HANDLER_ID, kvp_frame_end_handler,
           SIXTP_CLEANUP_RESULT_ID, kvp_frame_result_cleanup,
           SIXTP_RESULT_FAIL_ID, kvp_frame_result_cleanup,
           SIXTP_FAIL_HANDLER_ID, kvp_frame_fail_handler,
           SIXTP_NO_MORE_HANDLERS)))
  {
      return NULL;
  }

  if(!(sixtp_add_some_sub_parsers(
           top_level, TRUE,
           "s", kvp_frame_slot_parser_new(top_level),
           0)))
  {
      return NULL;
  }

  return(top_level);
}

