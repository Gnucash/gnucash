

#ifndef _SIXTP_H_
#define _SIXTP_H_

#include <glib.h>
#include <stdio.h>

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

typedef struct _sixtp_child_result sixtp_child_result;

typedef gboolean (*sixtp_start_handler)(GSList* sibling_data,
                                        gpointer parent_data,
                                        gpointer global_data,
                                        gpointer *data_for_children,
                                        gpointer *result,
                                        const gchar *tag,
                                        gchar **attrs);

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


gboolean is_child_result_from_node_named(sixtp_child_result *cr,
                                         const char *tag);
void sixtp_child_free_data(sixtp_child_result *result);
void sixtp_child_result_destroy(sixtp_child_result *r);
void sixtp_child_result_print(sixtp_child_result *cr, FILE *f);

void sixtp_sax_start_handler(void *user_data, const xmlChar *name,
                             const xmlChar **attrs);
void sixtp_sax_characters_handler(void *user_data, const xmlChar *text,
                                  int len);
void sixtp_sax_end_handler(void *user_data, const xmlChar *name);

sixtp* sixtp_new(void);

void sixtp_destroy(sixtp *sp);

void sixtp_set_start(sixtp *parser, sixtp_start_handler start_handler);
void sixtp_set_before_child(sixtp *parser, sixtp_before_child_handler handler);
void sixtp_set_after_child(sixtp *parser, sixtp_after_child_handler handler);
void sixtp_set_end(sixtp *parser, sixtp_end_handler end_handler);
void sixtp_set_chars(sixtp *parser, sixtp_characters_handler char_handler);
void sixtp_set_cleanup_result(sixtp *parser, sixtp_result_handler handler);
void sixtp_set_cleanup_chars(sixtp *parser, sixtp_result_handler handler);
void sixtp_set_fail(sixtp *parser, sixtp_fail_handler handler);
void sixtp_set_result_fail(sixtp *parser, sixtp_result_handler handler);
void sixtp_set_chars_fail(sixtp *parser, sixtp_result_handler handler);

gboolean sixtp_add_sub_parser(sixtp *parser, const gchar* tag,
                              sixtp *sub_parser);


#endif /* _SIXTP_H_ */
