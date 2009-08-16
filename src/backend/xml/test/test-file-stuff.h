/* This file declares testing functions for xml files.
 *
 */

#ifndef TEST_FILE_STUFF_H
#define TEST_FILE_STUFF_H

#include <glib.h>

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-xml-helper.h"
#include "io-gncxml-gen.h"
#include "sixtp.h"



void write_dom_node_to_file(xmlNodePtr node, int fd);

int files_compare(const gchar* f1, const gchar* f2);

gboolean print_dom_tree(gpointer data_for_children, GSList* data_from_children,
                        GSList* sibling_data, gpointer parent_data,
                        gpointer global_data, gpointer *result,
                        const gchar *tag);

/**/
gboolean check_dom_tree_version(xmlNodePtr node, gchar *verstr);
gboolean equals_node_val_vs_string(xmlNodePtr node, const gchar* str);
gboolean equals_node_val_vs_guid(xmlNodePtr node, const GUID *id);
gboolean equals_node_val_vs_commodity(xmlNodePtr node,
                                      const gnc_commodity *com, QofBook *);
gboolean equals_node_val_vs_kvp_frame(xmlNodePtr node, const kvp_frame *frm);
gboolean equals_node_val_vs_date(xmlNodePtr node, const Timespec tm);
gboolean equals_node_val_vs_int(xmlNodePtr node, gint64 val);
gboolean equals_node_val_vs_boolean(xmlNodePtr node, gboolean val);

void
test_files_in_dir(int argc, char **argv, gxpf_callback cb,
                  sixtp *parser, const char *parser_tag,
                  QofBook *book);

#endif
