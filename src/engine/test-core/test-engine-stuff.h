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
void random_timespec_zero_nsec (gboolean zero_nsec);
void random_timespec_usec_resolution (gboolean usec_resolution);

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
void kvp_exclude_type (kvp_value_t kvp_type);
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
Transaction* get_random_transaction_with_currency(GNCSession *session,
                                                  gnc_commodity *currency);
gnc_commodity* get_random_commodity(GNCSession *session);
const char *get_random_commodity_namespace(void);

typedef enum
{
  RANDOM_QT      = 0,
  SIMPLE_QT      = 1 << 0,
  SPLIT_KVP_QT   = 1 << 1,
  TRANS_KVP_QT   = 1 << 2,
  ACCOUNT_KVP_QT = 1 << 3,
  GUID_QT        = 1 << 4,
  ALL_QT         = (1 << 8) - 1
} TestQueryTypes;

Query * get_random_query(void);
Query * make_trans_query (Transaction *trans, TestQueryTypes query_types);
TestQueryTypes get_random_query_type (void);
void trans_query_include_price (gboolean include_amounts);

GNCBook * get_random_book (GNCSession *session);
GNCSession * get_random_session (void);

void add_random_transactions_to_session (GNCSession *session,
                                         gint num_transactions);

void make_random_changes_to_commodity (gnc_commodity *com);
void make_random_changes_to_commodity_table (gnc_commodity_table *table);
void make_random_changes_to_price (GNCSession *session, GNCPrice *price);
void make_random_changes_to_pricedb (GNCSession *session, GNCPriceDB *pdb);
void make_random_changes_to_split (Split *split);
void make_random_changes_to_transaction (GNCSession *session,
                                         Transaction *trans);
void make_random_changes_to_account (GNCSession *session, Account *account);
void make_random_changes_to_group (GNCSession *session, AccountGroup *group);
void make_random_changes_to_book (GNCSession *session, GNCBook *book);
void make_random_changes_to_session (GNCSession *session);

#endif
