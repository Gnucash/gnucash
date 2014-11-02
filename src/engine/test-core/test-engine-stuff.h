/** @file test-engine-stuff.h
 *  $brief This file declares testing functions for the engine.
 */

#ifndef TEST_ENGINE_STUFF_H
#define TEST_ENGINE_STUFF_H

#include <glib.h>
#include <stdlib.h>

#include "qof.h"
#include "Query.h"
#include "gnc-pricedb.h"
#include "SchedXaction.h"

Timespec* get_random_timespec(void);
void random_timespec_zero_nsec (gboolean zero_nsec);
void random_timespec_usec_resolution (gboolean usec_resolution);

KvpValue* get_random_kvp_value(int type);

typedef struct
{
    guchar *data;
    int len;
} bin_data;

bin_data* get_random_binary_data(void);

KvpFrame* get_random_kvp_frame(void);
gnc_numeric get_random_gnc_numeric(void);
GncGUID* get_random_guid(void);
GList* get_random_glist(void);

void random_glist_strings_only (gboolean strings_only);
void kvp_exclude_type (KvpValueType kvp_type);
void set_max_kvp_depth (gint max_kvp_depth);
void set_max_kvp_frame_elements (gint max_kvp_frame_elements);
void set_max_account_tree_depth (gint max_tree_depth);
void set_max_accounts_per_level (gint max_accounts);

GNCPrice * get_random_price(QofBook *book);
gboolean make_random_pricedb (QofBook *book, GNCPriceDB *pdb);
GNCPriceDB * get_random_pricedb(QofBook *book);
Account * get_random_account_tree(QofBook * book);
Account* get_random_account(QofBook * book);
Split* get_random_split(QofBook *book, Account *account, Transaction *trn);
Transaction* get_random_transaction(QofBook *book);
Transaction* get_random_transaction_with_currency(QofBook *book,
        gnc_commodity *currency,
        GList *account_list);
gnc_commodity* get_random_commodity(QofBook *book);
const char *get_random_commodity_namespace(void);

typedef enum
{
    RANDOM_QT      = 0,
    SIMPLE_QT      = 1 << 0,
    ACCOUNT_QT     = 1 << 1,
    SPLIT_KVP_QT   = 1 << 2,
    TRANS_KVP_QT   = 1 << 3,
    ACCOUNT_KVP_QT = 1 << 4,
    GUID_QT        = 1 << 5,
    ALL_QT         = (1 << 8) - 1
} TestQueryTypes;

QofQuery * get_random_query(void);
QofQuery * make_trans_query (Transaction *trans, TestQueryTypes query_types);
TestQueryTypes get_random_query_type (void);
void trans_query_include_price (gboolean include_amounts);

QofBook * get_random_book (void);
QofSession * get_random_session (void);

void add_random_transactions_to_book (QofBook *book, gint num_transactions);

void make_random_changes_to_commodity (gnc_commodity *com);
void make_random_changes_to_commodity_table (gnc_commodity_table *table);
void make_random_changes_to_price (QofBook *book, GNCPrice *price);
void make_random_changes_to_pricedb (QofBook *book, GNCPriceDB *pdb);
void make_random_changes_to_split (Split *split);
void make_random_changes_to_transaction (QofBook *book,
        Transaction *trans);
void make_random_changes_to_transaction_and_splits (QofBook *book,
        Transaction *trans,
        GList *accounts);
void make_random_changes_to_account (QofBook *book, Account *account);
void make_random_changes_to_level (QofBook *book, Account *parent);
void make_random_changes_to_book (QofBook *book);
void make_random_changes_to_session (QofSession *session);

SchedXaction* add_daily_sx(gchar *name, const GDate *start, const GDate *end, const GDate *last_occur);
SchedXaction* add_once_sx(gchar *name, const GDate *when);
void remove_sx(SchedXaction *sx);

#endif
