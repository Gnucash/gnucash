/* This file declares testing functions for the engine.
 */

#ifndef TEST_ENGINE_STUFF_H
#define TEST_ENGINE_STUFF_H

#include "config.h"

#include <glib.h>
#include <stdlib.h>

#include "gnc-commodity.h"
#include "gnc-pricedb.h"
#include "guid.h"
#include "kvp_frame.h"
#include "date.h"
#include "Account.h"
#include "Query.h"

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
gnc_numeric get_random_gnc_numeric(void);
gnc_commodity* get_random_gnc_commodity_ref(void);
GUID* get_random_guid(void);
GList* get_random_glist(void);

GNCPrice * get_random_price(void);
Account* get_random_account(void);
Split* get_random_split(gnc_numeric num);
Transaction* get_random_transaction(void);
gnc_commodity* get_random_commodity(void);
const char *get_random_commodity_namespace(void);

Query* get_random_query(void);

#endif

