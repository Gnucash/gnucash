/* Created by bstanley 20010323
 * moved contents from test-stuff.h
 *
 * This file declares testing functions which rely on other parts
 * of gnucash.
 *
 */

#ifndef GNC_TEST_STUFF_H
#define GNC_TEST_STUFF_H

#include "config.h"

#include <glib.h>
#include <stdlib.h>

#include "test-stuff.h"

#include "gnc-xml-helper.h"
#include "io-gncxml-gen.h"
#include "sixtp.h"

#include "gnc-commodity.h"
#include "gnc-pricedb.h"
#include "guid.h"
#include "kvp_frame.h"
#include "date.h"
#include "Account.h"
#include "Query.h"

gchar get_random_character(void);
gchar* get_random_string(void);
Timespec* get_random_timespec(void);
kvp_value* get_random_kvp_value(int type);

struct _bin_data
{
    guchar *data;
    int len;
};
typedef struct _bin_data bin_data;

bin_data* get_random_binary_data(void);

kvp_frame* get_random_kvp_frame(void);
gint64 get_random_gint64(void);
double get_random_double(void);
gnc_numeric get_random_gnc_numeric(void);
gnc_commodity* get_random_gnc_commodity_ref(void);
GUID* get_random_guid(void);
GList* get_random_glist(void);
gint get_random_int_in_range(int start, int end);
const char* get_random_string_in_array(const char* str_list[]);
gboolean get_random_boolean(void);

GNCPrice * get_random_price(void);
Account* get_random_account(void);
Split* get_random_split(gnc_numeric num);
Transaction* get_random_transaction(void);
gnc_commodity* get_random_commodity(void);
const char *get_random_commodity_namespace(void);

Query* get_random_query(void);

/**/
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
                                      const gnc_commodity *com);
gboolean equals_node_val_vs_kvp_frame(xmlNodePtr node, const kvp_frame *frm);
gboolean equals_node_val_vs_date(xmlNodePtr node, const Timespec tm);
gboolean equals_node_val_vs_int(xmlNodePtr node, gint64 val);

void
test_files_in_dir(int argc, char **argv, gxpf_callback cb,
                  sixtp *parser, const char *parser_tag);

#endif /* GNC_TEST_STUFF_H */
