#include <glib.h>
#include <guile/gh.h>

#include "gnc-engine-util.h"

#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-module.h"
#include "test-engine-stuff.h"
#include "test-stuff.h"


static void
test_commodity(void)
{
    gnc_commodity *com;

    {
        com = gnc_commodity_new(NULL, NULL, NULL, NULL, 0);

        gnc_commodity_destroy(com);

        success("commodity new and destroy");
    }

    {
        char *fullname;
        const char *namespace;
        char *mnemonic;
        char *exchange_code;
        int fraction;
        gnc_commodity *com2;
        
        fullname = get_random_string();
        namespace = get_random_commodity_namespace();
        mnemonic = get_random_string();
        exchange_code = get_random_string();
        fraction = get_random_int_in_range(0, 10000);

        com = gnc_commodity_new(fullname, namespace, mnemonic,
                                exchange_code, fraction);

        do_test(
            com != NULL, "commodity with data new and destroy");

        do_test(
            safe_strcmp(fullname, gnc_commodity_get_fullname(com)) == 0,
            "fullnames equal test");

        do_test(
            safe_strcmp(namespace, gnc_commodity_get_namespace(com)) == 0,
            "namespace equal test");

        do_test(
            safe_strcmp(mnemonic, gnc_commodity_get_mnemonic(com)) == 0,
            "mnemonic equal test");

        do_test(
            safe_strcmp(exchange_code, gnc_commodity_get_exchange_code(com)) == 0,
            "exchange code equal test");

        do_test(
            gnc_commodity_get_fraction(com) == fraction,
            "fraction code equal test");

        fullname = get_random_string();
        gnc_commodity_set_fullname(com, fullname);
        do_test(
            safe_strcmp(fullname, gnc_commodity_get_fullname(com)) == 0,
            "reset fullnames equal test");

        namespace = get_random_commodity_namespace();
        gnc_commodity_set_namespace(com, namespace);
        do_test(
            safe_strcmp(namespace, gnc_commodity_get_namespace(com)) == 0,
            "reset namespace equal test");

        mnemonic = get_random_string();
        gnc_commodity_set_mnemonic(com, mnemonic);
        do_test(
            safe_strcmp(mnemonic, gnc_commodity_get_mnemonic(com)) == 0,
            "reset mnemonic equal test");

        exchange_code = get_random_string();
        gnc_commodity_set_exchange_code(com, exchange_code);
        do_test(
            safe_strcmp(exchange_code, gnc_commodity_get_exchange_code(com)) == 0,
            "reset exchange code equal test");

        fraction = get_random_int_in_range(0, 10000);
        gnc_commodity_set_fraction(com, fraction);
        do_test(
            gnc_commodity_get_fraction(com) == fraction,
            "reset fraction code equal test");

        com2 = gnc_commodity_new(fullname, namespace, mnemonic,
                                 exchange_code, fraction);
        do_test(
            gnc_commodity_equiv(com, com2), "commodity equiv");

    }
    
    {
        int i;
        int j;
        gnc_commodity_table *tbl;
        gnc_commodity *coms[20];
        GNCSession *session;

        session = gnc_session_new ();
        tbl = gnc_commodity_table_new ();

        do_test(gnc_commodity_table_get_size(tbl) == 0,
                "test size for 0 table");

        for(i = 0; i < 20; i++)
        {
            coms[i] = get_random_commodity(session);

            do_test(
                gnc_commodity_table_insert(tbl, coms[i]) != NULL,
                "insert test");

            do_test_args(
                gnc_commodity_table_get_size(tbl) == i + 1,
                "test next size table", __FILE__, __LINE__,
                "should be %d and is %d", i + 1,
                gnc_commodity_table_get_size(tbl));

            for(j = 0; j <= i; j++)
            {
                gnc_commodity *testcom;

                do_test(
                    (testcom = gnc_commodity_table_lookup(
                        tbl, gnc_commodity_get_namespace(coms[j]),
                        gnc_commodity_get_mnemonic(coms[j]))) != NULL,
                    "lookup commodity");
                do_test(
                    gnc_commodity_equiv(testcom, coms[j]),
                    "lookup commodity and test equiv");
            }

            do_test(
                gnc_commodity_table_has_namespace(
                    tbl, gnc_commodity_get_namespace(coms[i])),
                "test have namespace");
        }
    }
    
}

static void
main_helper (int argc, char **argv)
{
  gnc_module_load("gnucash/engine", 0);
  test_commodity();
  print_test_results();
  exit(get_rv());
}

int
main (int argc, char **argv)
{
  gh_enter (argc, argv, main_helper);
  return 0;
}
