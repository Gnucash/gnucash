/* This file declares testing functions for the engine.
 */

#ifndef TEST_ENGINE_STUFF_H
#define TEST_ENGINE_STUFF_H

#include "config.h"

#include <glib.h>
#include <stdlib.h>

#include "gnc-session.h"
#include "Query.h"
#include "date.h"
#include "gnc-pricedb.h"

Timespec* get_random_timespec(void);
kvp_value* get_random_kvp_value(int type);

typedef struct
{
    guchar *data;
    int len;
} bin_data;

bin_data* get_random_binary_data(void);

kvp_frame* get_random_kvp_frame(void);
gnc_numeric get_random_gnc_numeric(void);
GUID* get_random_guid(void);
GList* get_random_glist(void);

void random_glist_strings_only (gboolean strings_only);
void glist_exclude_type (kvp_value_t kvp_type);
void set_max_kvp_depth (gint max_kvp_depth);
void set_max_kvp_frame_elements (gint max_kvp_frame_elements);
void set_max_group_depth (gint max_group_depth);
void set_max_group_accounts (gint max_group_accounts);

GNCPrice * get_random_price(GNCSession *session);
void make_random_pricedb (GNCSession *session, GNCPriceDB *pdb);
GNCPriceDB * get_random_pricedb(GNCSession *session);
AccountGroup * get_random_group(GNCSession * session);
Account* get_random_account(GNCSession * session);
Split* get_random_split(GNCSession *session, gnc_numeric num);
Transaction* get_random_transaction(GNCSession *session);
gnc_commodity* get_random_commodity(GNCSession *session);
const char *get_random_commodity_namespace(void);

Query* get_random_query(void);

GNCBook * get_random_book (GNCSession *session);
GNCSession * get_random_session (void);

#endif
