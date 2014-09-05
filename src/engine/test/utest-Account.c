/********************************************************************
 * utest-Account.c: GLib g_test test suite for Account.c.	    *
 * Copyright 2011 John Ralls <jralls@ceridwen.us>		    *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
********************************************************************/
#include "config.h"
#include <string.h>
#include <glib.h>
#include <unittest-support.h>
#include <gnc-event.h>
#include <gnc-gdate-utils.h>
#include <qofinstance-p.h>
/* Add specific headers for this class */
#include "../Account.h"
#include "../AccountP.h"
#include "../Split.h"
#include "../Transaction.h"
#include "../gnc-lot.h"

#ifdef HAVE_GLIB_2_38
#define _Q "'"
#else
#define _Q "`"
#endif

static const gchar *suitename = "/engine/Account";
void test_suite_account (void);

typedef struct
{
    Account *acct;
    AccountTestFunctions *func;
} Fixture;

typedef struct
{
    GNCAccountType type;
    char *name;
    char *parent;
    char *code;
    char *desc;
    char *color;
    char *notes;
    char *num;
    GNCPolicy *policy;
} AccountParms;

typedef struct
{
    char *memo;
    char *account;
    char reconciled;
    gnc_numeric amount;
    gnc_numeric value;
    guint lotnum;
} SplitParms;

typedef struct
{
    gchar *desc;
    gint date_offset;
    SplitParms splits[2];
} TxnParms;

typedef struct
{
    guint num_accounts;
    AccountParms **accounts;
    guint num_txns;
    TxnParms **txns;
} SetupData;

static AccountParms bad_names[] =
{
    {ACCT_TYPE_NONE, "foo:bar", "", "", "", "", "", "", NULL},
    {ACCT_TYPE_NONE, "baz", "", "", "", "", "", "", NULL},
    {ACCT_TYPE_NONE, "waldo:pepper", "", "", "", "", "", "", NULL}
};

static AccountParms good_names[] =
{
    {ACCT_TYPE_NONE, "foo", "", "", "", "", "", "", NULL},
    {ACCT_TYPE_NONE, "baz", "foo", "", "", "", "", "", NULL},
    {ACCT_TYPE_NONE, "waldo", "baz", "", "", "", "", "", NULL}
};

static SetupData bad_data = {3, (AccountParms**)(&bad_names), 0, NULL};
static SetupData good_data = {3, (AccountParms**)(&good_names), 0, NULL};

static AccountParms some_names[] =
{
    {ACCT_TYPE_NONE, "foo", "root", "", "", "", "", "", NULL},
    {ACCT_TYPE_NONE, "baz", "foo", "", "", "", "", "", NULL},
    {ACCT_TYPE_NONE, "bar", "root", "", "", "", "", "", NULL},
    {ACCT_TYPE_NONE, "meh", "bar", "", "", "", "", "", NULL},

};

static TxnParms some_txns[] =
{
    {
        "waldo", -9, {
            {"waldo_baz", "baz", NREC, { -150000, 100}, {0, 1}, 0},
            {"waldo_meh", "meh", NREC, {150000, 100}, {0, 1}, 0}
        }
    },
    {
        "pepper", -7, {
            {"pepper_baz", "baz", CREC, { -12345, 100}, {0, 1}, 0},
            {"pepper_meh", "meh", CREC, {12345, 100}, {0, 1}, 0}
        }
    },
    {
        "salt", -2, {
            {"salt_baz", "baz", YREC, { -31415, 100}, {0, 1}, 0},
            {"salt_meh", "meh", YREC, {31415, 100}, {0, 1}, 0}
        }
    },
    {
        "pork", 3, {
            {"pork_baz", "baz", YREC, {23746, 100}, {0, 1}, 0},
            {"pork_meh", "meh", YREC, { -23746, 100}, {0, 1}, 0}
        }
    },
    {
        "sausage", 5, {
            {"sausage_baz", "baz", NREC, { -1143, 100}, {0, 1}, 0},
            {"sausage_meh", "meh", NREC, {1143, 100}, {0, 1}, 0}
        }
    }
};

static SetupData some_data = {4, (AccountParms**)(&some_names),
                              5, (TxnParms**)(&some_txns)
                             };

static AccountParms complex_accts[] =
{
    {ACCT_TYPE_EXPENSE, "expense", "root", "3000", "", "", "", "", NULL},
    {ACCT_TYPE_EXPENSE, "ordinary", "expense", "3100", "", "", "", NULL},
    {ACCT_TYPE_EXPENSE, "deductible", "expense", "3200", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "income", "root", "4000", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "taxable", "income", "4100", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "exempt", "income", "4200", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "wage", "taxable", "4150", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "div", "taxable", "4140", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "div1", "taxable", "4140", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "qdiv", "div1", "4141", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "odiv", "div1", "4142", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "int", "taxable", "4160", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "ltcg", "taxable", "4120", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "stcg", "taxable", "4110", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "int", "exempt", "4210", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "div", "exempt", "4230", "", "", "", "", NULL},
    {ACCT_TYPE_INCOME, "gift", "exempt", "4220", "", "", "", "", NULL},
    {ACCT_TYPE_ASSET, "assets", "root", "2000", "", "", "", "", NULL},
    {ACCT_TYPE_BANK, "bank", "assets", "2300", "", "", "", "", NULL},
    {ACCT_TYPE_ASSET, "broker", "assets", "2200", "", "", NULL},
    {ACCT_TYPE_BANK, "money", "broker", "2210", "", "", NULL},
    {ACCT_TYPE_STOCK, "stocks", "broker", "2220", "", "", NULL},
    {ACCT_TYPE_EXPENSE, "food", "ordinary", "3110", "", "", "", NULL},
    {ACCT_TYPE_EXPENSE, "utilities", "ordinary", "3150", "", "", "", NULL},
    {ACCT_TYPE_EXPENSE, "auto", "ordinary", "3120", "", "", "", NULL},
    {ACCT_TYPE_EXPENSE, "mortgage-int", "deductible", "3230", "", "", "", NULL},
    {ACCT_TYPE_EXPENSE, "medical", "deductible", "3220", "", "", "", NULL},
    {ACCT_TYPE_STOCK, "foo", "stocks", "2221", "", "", NULL},
    {ACCT_TYPE_STOCK, "bar", "stocks", "2222", "", "", NULL},
    /* Note the repetition of the stock account "baz". The variations are
     * used to test gnc_account_merge_children. */
    {ACCT_TYPE_STOCK, "baz", "stocks", "2223", "baz", "", NULL},
    {ACCT_TYPE_STOCK, "baz2", "stocks", "2223", "baz", "", NULL},
    {ACCT_TYPE_MUTUAL, "baz", "stocks", "2223", "baz", "", NULL},
    {ACCT_TYPE_STOCK, "baz", "stocks", "2223", "baz company", "", NULL},
    {ACCT_TYPE_STOCK, "baz", "stocks", "2224", "baz", "", NULL},

};

static TxnParms lot_txns[] =
{
    {
        "funding", -12, {
            {"funding_money", "money", NREC, {1000000, 100}, {0, 1}, 0},
            {"funding_gift", "gift", NREC, {1000000, 100}, {0, 1}, 0}
        }
    },
    {
        "waldo", -9, {
            {"waldo_baz", "baz", NREC, {1500, 1}, {150899, 100}, 1},
            {"waldo_money", "money", NREC, { -150899, 100}, {0, 1}, 0}
        }
    },
    {
        "pepper", -7, {
            {"pepper_baz", "baz", CREC, { -500, 1}, { -69101, 100}, 1},
            {"pepper_money", "money", CREC, {69101, 100}, {0, 1}, 0}
        }
    },
    {
        "salt", -2, {
            {"salt_baz", "baz", YREC, {1000, 1}, {120899, 10}, 2},
            {"salt_money", "money", YREC, { -120899, 100}, {0, 1}, 0}
        }
    },
    {
        "pork", 3, {
            {"pork_baz", "baz", YREC, { -1000, 1}, { -79101, 100}, 1},
            {"pork_money", "money", YREC, {79101, 100}, {0, 1}, 0}
        }
    },
    {
        "sausage", 5, {
            {"sausage_baz", "baz", NREC, { -500, 1}, { -74101, 100}, 2},
            {"sausage_mmoney", "money", NREC, {74101, 100}, {0, 1}, 0}
        }
    },
    {
        "pork", 3, {
            {"pork_baz2", "baz2", YREC, { -1000, 1}, { -79101, 100}, 1},
            {"pork_money", "money", YREC, {79101, 100}, {0, 1}, 0}
        }
    },
    {
        "sausage", 5, {
            {"sausage_baz2", "baz2", NREC, { -500, 1}, { -74101, 100}, 2},
            {"sausage_mmoney", "money", NREC, {74101, 100}, {0, 1}, 0}
        }
    },
    {
        "links", 1, {
            {"links_baz", "baz", NREC, {500, 1}, {60899, 100}, 3},
            {"links_mmoney", "money", NREC, { -60899, 100}, {0, 1}, 0}
        }
    }
};

static SetupData complex = {G_N_ELEMENTS (complex_accts),
                            (AccountParms**)(&complex_accts), 0, NULL
                           };

static SetupData complex_data = {G_N_ELEMENTS (complex_accts),
                                 (AccountParms**)(&complex_accts),
                                 G_N_ELEMENTS (lot_txns),
                                 (TxnParms**)(&lot_txns)
                                };

static Split*
insert_split (Account *parent, Transaction *txn, SplitParms *p)
{
    QofBook *book = gnc_account_get_book (parent);
    Split *split = xaccMallocSplit (book);
    Account *acct = gnc_account_lookup_by_name (parent, p->account);
    LotList* lotlist = xaccAccountGetLotList (acct);
    GNCLot* lot = NULL;
    g_assert (acct != NULL);
    xaccSplitSetParent (split, txn);
    xaccSplitSetMemo (split, p->memo);
    xaccSplitSetReconcile (split, (p->reconciled ? p->reconciled : NREC));
    g_object_set (split,
                  "account", acct,
                  "memo", p->memo,
                  "amount", &(p->amount),
                  "value", &(p->value),
                  NULL);

    gnc_account_insert_split (acct, split);
    if (p->lotnum == 0)
        return split;

    if (p->lotnum > g_list_length (lotlist))
        lot = gnc_lot_new (book);
    else
        lot = GNC_LOT (g_list_nth_data (lotlist, p->lotnum - 1));

    gnc_lot_add_split (lot, split);
    return split;
}

static void
setup (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = qof_book_new ();
    Account *root = gnc_account_create_root (book), *acct = NULL;
    SetupData *parms = (SetupData *)pData;
    TxnParms *t_arr;
    AccountParms *p_arr;
    GHashTable *accts = g_hash_table_new (g_str_hash, g_str_equal);
    guint ind;

    g_hash_table_insert (accts, "root", root);
    fixture->func = _utest_account_fill_functions ();
    if (parms == NULL)
    {
        fixture->acct = root;
        return;
    }
    acct = root;

    p_arr  = (AccountParms*)parms->accounts;
    for (ind = 0; ind < parms->num_accounts; ind++)
    {
        Account *child = xaccMallocAccount (book);
        AccountParms p = p_arr[ind];
        if (p.parent && strlen (p.parent) > 0)
        {
            Account *parent = g_hash_table_lookup (accts, p.parent);
            g_assert (parent != NULL);
            gnc_account_append_child (parent, child);
        }
        else
            gnc_account_append_child (acct, child);
        acct = child;
        xaccAccountSetType (acct, p.type);
        if (p.name)
        {
            xaccAccountSetName (acct, p.name);
            g_hash_table_insert (accts, p.name, acct);
        }
        if (p.code)
            xaccAccountSetCode (acct, p.code);
        if (p.desc)
            xaccAccountSetDescription (acct, p.desc);
        if (p.color)
            xaccAccountSetColor (acct, p.color);
        if (p.notes)
            xaccAccountSetNotes (acct, p.notes);
        if (p.num)
            xaccAccountSetLastNum (acct, p.num);
        if (p.policy)
            gnc_account_set_policy (acct, p.policy);
    }

    t_arr  = (TxnParms*)parms->txns;
    for (ind = 0; ind < parms->num_txns; ind++)
    {
        Transaction *txn = xaccMallocTransaction (book);
        TxnParms p = t_arr[ind];
        GDate *date = g_date_new ();
        gnc_gdate_set_time64 (date, gnc_time (NULL));
        xaccTransBeginEdit (txn);
        if (p.desc)
            xaccTransSetDescription (txn, p.desc);
        if (p.date_offset < 0)
            g_date_subtract_days (date, (guint)(-p.date_offset));
        else
            g_date_add_days (date, (guint)(p.date_offset));
        xaccTransSetDatePostedGDate (txn, *date);
        insert_split (root, txn, &p.splits[0]);
        insert_split (root, txn, &p.splits[1]);
        /* xaccTransCommitEdit () does a bunch of scrubbing that we don't need */
        qof_commit_edit (QOF_INSTANCE (txn));
    }
    fixture->acct = acct;
    g_hash_table_destroy (accts);
    gnc_set_account_separator (":");
}

static void
teardown ( Fixture *fixture, gconstpointer pData)
{
    Account *child = fixture->acct;
    qof_book_destroy (gnc_account_get_book (child));
    /* No need to free the last account, qof_book_destroy did that */
    g_free (fixture->func);
}


/* gnc_get_account_separator_string
const gchar *
gnc_get_account_separator_string (void)// C: 31 in 7 SCM: 64 in 6*/
//Simple Getter. No Test.
/* static void
test_gnc_get_account_separator_string (Fixture *fixture, gconstpointer pData)
{
}*/
/* gnc_get_account_separator
gunichar
gnc_get_account_separator (void)// C: 8 in 4 */
//Simple Getter. No Test.
/* gnc_set_account_separator
void
gnc_set_account_separator (const gchar *separator)// C: 5 in 3 */
static void
test_gnc_set_account_separator ()
{
    gchar sep_str[6];
    const gunichar pass_sep = 0xE5A;
    const gunichar fail_sep = 0x92A;
    const gunichar default_sep = ':';
    gunichar sep = gnc_get_account_separator ();

    g_assert (sep == default_sep);
    memset (sep_str, 0, sizeof (sep_str));
    g_unichar_to_utf8 (fail_sep, sep_str);
    gnc_set_account_separator (sep_str);
    sep = gnc_get_account_separator ();
    g_assert (sep != fail_sep);
    g_assert (sep == default_sep);
    memset (sep_str, 0, sizeof (sep_str));
    g_unichar_to_utf8 (pass_sep, sep_str);
    gnc_set_account_separator (sep_str);
    sep = gnc_get_account_separator ();
    g_assert (sep == pass_sep);
}
/* Note: gnc_account_name_violations and gnc_account_list_name_violations should work with unicode. Change the functions and the tests accordingly. */
/* gnc_account_name_violations_errmsg
gchar *gnc_account_name_violations_errmsg (const gchar *separator, GList* invalid_account_names)// C: 6 in 4 */
static void
test_gnc_account_name_violations_errmsg ()
{
    GList *badnames = NULL, *nonames = NULL, *node = NULL;
    gchar *separator = ":", *message, *validation_message, *account_list = NULL;
    /* FUT wants to free the strings, so we alloc them */
    badnames = g_list_prepend (badnames, g_strdup ("Foo:bar"));
    badnames = g_list_prepend (badnames, g_strdup ("baz"));
    badnames = g_list_prepend (badnames, g_strdup ("waldo:pepper"));
    message = gnc_account_name_violations_errmsg (separator, nonames);
    for (node = badnames; node; node = g_list_next (node))
    {
        if (!account_list)
            account_list = g_strdup (node->data);
        else
        {
            gchar *tmp_list = g_strconcat ( account_list, "\n",
                                            node->data, NULL);
            g_free (account_list);
            account_list = tmp_list;
        }
    }
    message = gnc_account_name_violations_errmsg (separator, nonames);
    g_assert (message == NULL);
    validation_message = g_strdup_printf (
        "The separator character \"%s\" is used in one or more account "
        "names.\n\nThis will result in unexpected behaviour. "
        "Either change the account names or choose another separator "
        "character.\n\nBelow you will find the list of invalid account names:\n"
        "%s", separator, account_list);
    message = gnc_account_name_violations_errmsg (separator, badnames);
    g_assert_cmpstr ( message, == , validation_message);
    g_free (validation_message);
    g_free (message);
}
/* This should be qof_BOOK_list_name_violations */
/* gnc_account_list_name_violations
GList *gnc_account_list_name_violations (QofBook *book, const gchar *separator)// C: 6 in 4 */
static void
test_gnc_account_list_name_violations (Fixture *fixture, gconstpointer pData)
{
    guint log_level = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *log_domain = "gnc.engine";
#ifdef __clang__
#define _func "GList *gnc_account_list_name_violations(QofBook *, const gchar *)"
#else
#define _func "gnc_account_list_name_violations"
#endif
    gchar *msg = _func ": assertion " _Q "separator != NULL' failed";
#undef _func
    TestErrorStruct check = { log_level, log_domain, msg, 0 };
    GList *results, *res_iter;
    gchar *sep = ":";
    QofBook *book = gnc_account_get_book (fixture->acct);
    /* Because of GLib bug 653052, we have to set the logging user_data to
     * affect the test_log_fatal_handler
     */
    GLogFunc oldlogger = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    g_assert (gnc_account_list_name_violations (NULL, NULL) == NULL);
    g_assert_cmpint (check.hits, ==, 1);
    g_assert (gnc_account_list_name_violations (book, NULL) == NULL);
    g_assert_cmpint (check.hits, ==, 2);
    g_assert (gnc_account_list_name_violations (NULL, sep) == NULL);
    g_log_set_default_handler (oldlogger, NULL);
    results = gnc_account_list_name_violations (book, sep);
    g_assert_cmpuint (g_list_length (results), == , 2);
    g_assert_cmpint (check.hits, ==, 2);
    for (res_iter = results; res_iter; res_iter = g_list_next (res_iter))
        test_free (res_iter->data);
    g_list_free (results);
}
/* mark_account
void
mark_account (Account *acc)// C: 2 in 1 */
/*Simple passthrough of qof_instance_set_dirty; Don't test.*/
// Not Used
/* gnc_account_init
G_DEFINE_TYPE (Account, gnc_account, QOF_TYPE_INSTANCE)
static void
gnc_account_init (Account* acc)// 1
*/
/* test_gnc_account_create_and_destroy (void):
 * Tests the following functions:
 * gnc_account_init ()
 * gnc_account_class_init ()
 * gnc_account_get_property ()
 * gnc_account_set_property ()
 * xaccAccountGetColor ()
 * xaccAccountGetNotes ()
 *
 * Exercises the following functions, may not thoroughly test them:
 * xaccAccountSetName ()
 * xaccAccountSetCode ()
 * xaccAccountSetDescription ()
 * xaccAccountSetColor ()
 * xaccAccountSetNotes ()
 * xaccAccountSetType ()
 * xaccAccountSetCommodity ()
 * xaccAccountSetCommoditySCU ()
 * xaccAccountSetNotStdSCU ()
 * gnc_account_set_sort_dirty ()
 * gnc_account_set_balance ()
 * gnc_account_set_start_balance ()
 * gnc_account_set_start_cleared_balance ()
 * gnc_account_set_start_reconciled_balance ()
 * gnc_account_set_policy ()
 * xaccAccountSetMark () *** Not Used ***
 * xaccAccountSetTaxRelated ()
 * xaccAccountSetTaxUSCode ()
 * xaccAccountSetTaxUSPayerNameSource ()
 * xaccAccountSetTaxUSCopyNumber ()
 * xaccAccountSetHidden ()
 * xaccAccountSetPlaceholder ()

 * Note very well that this is only the GObject portion of creating
 * and destroying Account objects. There are other parts (which is a
 * major problem), see in particular the note at test_xaccFreeAccount.
 */

static void
test_gnc_account_create_and_destroy (void)
{
    Account *acc = g_object_new (GNC_TYPE_ACCOUNT, NULL);
    gchar *name, *fname, *code, *desc, *color, *notes, *tax_code, *tax_src;
    GNCAccountType type;
    gnc_commodity *commo;
    gint commo_scu, mark;
    gboolean non_std_scu, sort_dirty, bal_dirty, tax_rel, hide, hold;
    gint64 copy_num;
    gnc_numeric *start_bal, *start_clr_bal, *start_rec_bal;
    gnc_numeric *end_bal, *end_clr_bal, *end_rec_bal;
    GNCPolicy *pol;

    g_object_get (acc,
                  "name", &name,
                  "fullname", &fname,
                  "code", &code,
                  "description", &desc,
                  "color", &color,
                  "notes", &notes,
                  "type", &type,
                  "commodity", &commo,
                  "commodity_scu", &commo_scu,
                  "non-std-scu", &non_std_scu,
                  "sort-dirty", &sort_dirty,
                  "balance-dirty", &bal_dirty,
                  "start-balance", &start_bal,
                  "start-cleared-balance", &start_clr_bal,
                  "start-reconciled-balance", &start_rec_bal,
                  "end-balance", &end_bal,
                  "end-cleared-balance", &end_clr_bal,
                  "end-reconciled-balance", &end_rec_bal,
                  "policy", &pol,
                  "acct-mark", &mark,
                  "tax-related", &tax_rel,
                  "tax-code", &tax_code,
                  "tax-source", &tax_src,
                  "tax-copy-number", &copy_num,
                  "hidden", &hide,
                  "placeholder", &hold,
                  NULL);

    g_assert_cmpstr (name, == , "");
    g_assert_cmpstr (fname, == , "");
    g_assert_cmpstr (code, == , "");
    g_assert_cmpstr (desc, == , "");
    g_assert (!color);
    g_assert (!notes);
    g_assert (type == ACCT_TYPE_NONE);
    g_assert (!commo);
    g_assert (!commo_scu);
    g_assert (!non_std_scu);
    g_assert (!sort_dirty);
    g_assert (!bal_dirty);
    g_assert (gnc_numeric_zero_p (*end_bal));
    g_assert (gnc_numeric_zero_p (*end_clr_bal));
    g_assert (gnc_numeric_zero_p (*end_rec_bal));
    g_assert (gnc_numeric_zero_p (*start_bal));
    g_assert (gnc_numeric_zero_p (*start_clr_bal));
    g_assert (gnc_numeric_zero_p (*start_rec_bal));
    g_assert (pol == xaccGetFIFOPolicy ());
    g_assert (!mark);
    g_assert (!tax_rel);
    g_assert (!tax_code);
    g_assert (!tax_src);
    g_assert (copy_num == 1);
    g_assert (!hide);
    g_assert (!hold);
    g_assert (gnc_account_get_parent (acc) == NULL);
    g_assert (gnc_account_get_children (acc) == NULL);
    g_assert (xaccAccountGetLotList (acc) == NULL);
    g_assert (xaccAccountGetSplitList (acc) == NULL);
    g_free (name);
    g_free (fname);
    g_free (code);
    g_free (desc);
    g_free (color);
    g_free (notes);
    g_free (tax_code);
    g_free (tax_src);

    g_object_unref (acc);
    /* There's no good way to test that the object has been properly
     * destroyed; on the other hand, that's GObject's job.
     */

}
/* xaccInitAccount
static void
xaccInitAccount (Account * acc, QofBook *book)// 3
Simple pass-through for qof_instance_init_data ()
*/
/* gnc_account_get_book
QofBook *
gnc_account_get_book (const Account *account)// C: 30 in 21 SCM: 8 in 4
Simple pass-through for qof_instance_get_book ()
*/
/* gnc_coll_get_root_account
static Account *
gnc_coll_get_root_account (QofCollection *col)// 3
Simple pass-through for qof_collection_get_data ()
*/

/* test_gnc_book_set_get_root_account:
 * Tests the following:
 * gnc_coll_set_root_account ()
 * gnc_coll_get_root_account ()
 * gnc_book_set_root_account ()
 * gnc_book_get_root_account ()
 * Note that the first two are static helper functions which
 * generalize the second pair -- but in vain, because they're used
 * only in one place.
 */
static void
test_gnc_book_set_get_root_account (Fixture *fixture, gconstpointer pData)
{
    guint log_level = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *log_domain = "gnc.account";
    gchar *msg = "[gnc_book_set_root_account()] cannot mix and match books freely!";
    TestErrorStruct check = { log_level, log_domain, msg, 0 };
    Account *acc1, *acc2;
    QofBook* book1 = qof_book_new ();
    GLogFunc oldlogger;
    QofBook *book2 = gnc_account_get_book (fixture->acct);
    acc1 = gnc_book_get_root_account (NULL);
    g_assert (!acc1);
    /* Check that an account is created, and that it isn't the same as the
     * one in fixture.
     */
    acc1 = gnc_book_get_root_account (book1);
    g_assert (acc1);
    g_assert (acc1 != fixture->acct);
    /* Now try to set the book's root account to the fixture
     * accout. Should throw an error.
     */
    oldlogger = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler,
                                  &check);
    gnc_book_set_root_account (book1, fixture->acct);
    g_assert (gnc_book_get_root_account (book1) == acc1);
    g_assert_cmpint (check.hits, ==, 1);
    g_log_set_default_handler (oldlogger, NULL);
    /* Check that if we set the same root, it stays set */
    gnc_book_set_root_account (book2, fixture->acct);
    g_assert (gnc_book_get_root_account (book2) == fixture->acct);
    /* Create a new account in book1 and check that we can set it to root */
    acc2 = xaccMallocAccount (book1);
    gnc_book_set_root_account (book1, acc2);
    g_assert (gnc_book_get_root_account (book1) != acc1);
    g_assert (gnc_book_get_root_account (book1) == acc2);
    /* Clean up */
    /* acc1 gets freed by setting the root accout to acc2
        g_object_unref (acc1);
    */
    g_object_unref (book1);
    g_object_unref (acc2);
}

/* xaccMallocAccount
Account *
xaccMallocAccount (QofBook *book)// C: 24 in 17 SCM: 9 in 6*/
static void
test_xaccMallocAccount (void)
{
    QofBook *book = qof_book_new ();
    Account *acc;
    TestSignal signal = test_signal_new (NULL, QOF_EVENT_CREATE, NULL);
    acc = xaccMallocAccount (book);
    g_assert (acc != NULL);
    test_signal_assert_hits (signal, 1);
    test_signal_free (signal);
    g_object_unref (book);
    g_object_unref (acc);
}

/* gnc_account_create_root
Account *
gnc_account_create_root (QofBook *book)// C: 5 in 3 */
static void
test_gnc_account_create_root (void)
{
    QofBook *book = qof_book_new ();
    QofCollection *coll = qof_book_get_collection (book, GNC_ID_ROOT_ACCOUNT);
    Account *acc;
    gchar *name;
    AccountTestFunctions *func = _utest_account_fill_functions ();
    /* Can't use gnc_book_get_root_account, it creates one if it doesn't
     * yet exist */
    g_assert (func->coll_get_root_account (coll) == NULL);
    acc = gnc_account_create_root (book);
    g_assert (acc);
    g_object_get (acc, "name", &name, NULL);
    g_assert_cmpstr (name, == , "Root Account");
    g_assert (gnc_book_get_root_account (book) == acc);
    g_object_unref (book);
    g_object_unref (acc);
    g_free (func);
    g_free (name);
}
/* xaccCloneAccountCommon
static Account *
xaccCloneAccountCommon (const Account *from, QofBook *book)// 3
*/
static void
test_xaccCloneAccount (Fixture *fixture, gconstpointer pData)
{
    Account *clone;
    QofBook *book = gnc_account_get_book (fixture->acct);
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
#ifdef __clang__
#define _func "Account *xaccCloneAccount(const Account *, QofBook *)"
#else
#define _func "xaccCloneAccount"
#endif
    gchar *msg1 = _func ": assertion " _Q "GNC_IS_ACCOUNT(from)' failed";
    gchar *msg2 = _func ": assertion " _Q "QOF_IS_BOOK(book)' failed";
#undef _func
    TestErrorStruct check = { loglevel, "gnc.engine", msg1, 0 };
    GLogFunc oldlogger;
    AccountPrivate *acct_p, *clone_p;
    oldlogger = g_log_set_default_handler ((GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);
    clone = xaccCloneAccount (NULL, book);
    g_assert (clone == NULL);
    g_assert_cmpint (check.hits, ==, 1);
    check.msg = msg2;
    clone = xaccCloneAccount (fixture->acct, NULL);
    g_assert (clone == NULL);
    g_assert_cmpint (check.hits, ==, 2);
    g_log_set_default_handler (oldlogger, NULL);
    /* Now test the real clone */
    clone = xaccCloneAccount (fixture->acct, book);
    g_assert (clone);
    acct_p = fixture->func->get_private (fixture->acct);
    clone_p = fixture->func->get_private (clone);
    g_assert (clone_p->type == acct_p->type);
    g_assert (clone_p->accountName == acct_p->accountName);
    g_assert (clone_p->accountCode == acct_p->accountCode);
    g_assert (clone_p->description == acct_p->description);
    g_assert (kvp_frame_compare (clone->inst.kvp_data,
                                 fixture->acct->inst.kvp_data) == 0);
    g_assert (gnc_commodity_equal (clone_p->commodity, acct_p->commodity));
    g_assert (clone_p->commodity_scu == acct_p->commodity_scu);
    g_assert (clone_p->non_standard_scu == acct_p->non_standard_scu);
    /* Clean Up */
    g_object_unref (clone);

}
/* xaccFreeOneChildAccount
static void
xaccFreeOneChildAccount (Account *acc, gpointer dummy)// 2
No need to test, it's a passthrough for xaccAccountDestroy
*/
/* xaccFreeAccountChildren
static void
xaccFreeAccountChildren (Account *acc)// 3
*/
static void
test_xaccFreeAccountChildren (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    AccountPrivate *priv = fixture->func->get_private (root);
    g_assert_cmpuint (g_list_length (priv->children), > , 0);
    fixture->func->xaccFreeAccountChildren (root);
    /* We'd like to check for the child actually having been freed, but
     * there's not good way to do that. */
    g_assert_cmpuint (g_list_length (priv->children), == , 0);
    qof_book_destroy (gnc_account_get_book (root));
    /* No need to unref the root account, qof_book_destroy did that. */
    g_free (fixture->func);
}
/* xaccFreeAccount
static void
xaccFreeAccount (Account *acc)//
The approved way to call this is via xaccAccountDestroy-->xaccAccountCommitEdit-->qof_instance_commit_edit2-->acc_free-->xaccFreeAccount

Note that none of this is called by gnc_account_dispose, and xaccFreeAccount calls unref on itself. (Not run dispose, unref.)

This "works" as follows:
xaccAccountDestroy sets a "destroying" flag in qof_instance and calls xaccAccountCommitEdit.
xaccAccountCommitEdit checks the flag and finding it true:
   calls xaccFreeACcountChildren
   tests qof_book_shutting_down and either deletes the split list for the account (trusting the Transaction code to delete the splits if it is shutting down) or deletes the actual splits while it's clearing the list. (What happens to the references in the transactions then? It's calling xaccSplitDestroy, not g_oject_unref!)
   Again checking that the book isn't shutting down:
      run destroy_pending_splits_for_account
      destroy all of the lots in the lots list (again, destroy, not unref or even dispose)
  free the lot list (regardless of whether the book is shuttin down) and NULL the pointer
  set the instance dirty
unconditionally calls qof_commit_edit_part2 with acc_free
qof_commit_edit_part2:
   calls the backend's commit if there is one
   calls acc_free
   calls acc_done (which only fires off a qof_event, so we'll ignore it)
acc_free removes the account from the parent if there is one and calls xaccFreeAccount, then calls xaccFreeAccount
xaccFreeAccount calls:
    xaccFreeAccountChildren
    destroys the lots (unless commitEdit did so already) and NULLs the pointer
    destroys the splits unless commitEdit did so already
    removes the name, code, and description strings from the string cache
    zeroes out the priv structure variables
    unrefs the account (should call run_dispose).
dispose calls parent->dispose.

acc_free
*/
/* Aside from being broken (the assert at the end of freeing the splits fails),
   Account deallocation is implemented wrong. We don't run this test, and the function will be replaced when the time comes. */
static void
test_xaccFreeAccount (Fixture *fixture, gconstpointer pData)
{
    gchar *msg1 = "[xaccFreeAccount()]  instead of calling xaccFreeAccount(), please call \n"
                  " xaccAccountBeginEdit(); xaccAccountDestroy(); \n";
#ifdef __clang__
#define _func "int xaccTransGetSplitIndex(const Transaction *, const Split *)"
#else
#define _func "xaccTransGetSplitIndex"
#endif
    gchar *msg2 = _func ": assertion " _Q "trans && split' failed";
#undef _func
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    TestErrorStruct check1 = { loglevel, "gnc.account", msg1, 0 };
    TestErrorStruct check2 = { loglevel, "gnc.engine", msg2, 0 };
    QofBook *book = gnc_account_get_book (fixture->acct);
    Account *parent = gnc_account_get_parent (fixture->acct);
    AccountPrivate *p_priv = fixture->func->get_private (parent);
    const guint numItems = 3;
    guint i = 0;
    guint hdlr1, hdlr2;
    gnc_commodity *commodity = gnc_commodity_new (book, "US Dollar", "CURRENCY", "USD", "0", 100);
    test_add_error (&check1);
    test_add_error (&check2);
    hdlr1 = g_log_set_handler ("gnc.account", loglevel,
                               (GLogFunc)test_checked_handler, &check1);
    hdlr2 = g_log_set_handler ("gnc.engine", loglevel,
                               (GLogFunc)test_checked_handler, &check2);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_list_handler, NULL);
    for (i = 0; i < numItems; i++)
    {
        Split *split = xaccMallocSplit (book);
        xaccSplitSetAccount (split, parent);
        gnc_account_insert_split (parent, split);
        xaccAccountInsertLot (parent, gnc_lot_new (book));
    }
    xaccAccountSetCommodity (parent, commodity);
    /* Check that we've got children, lots, and splits to remove */
    g_assert (p_priv->children != NULL);
    g_assert (p_priv->lots != NULL);
    g_assert (p_priv->splits != NULL);
    g_assert (p_priv->parent != NULL);
    g_assert (p_priv->commodity != NULL);
    g_assert_cmpint (check1.hits, ==, 0);
    g_assert_cmpint (check2.hits, ==, 0);
    /* Now set the other private parts to something so that they can be set back */
    p_priv->cleared_balance = gnc_numeric_create ( 5, 12);
    p_priv->reconciled_balance = gnc_numeric_create ( 5, 12);
    p_priv->balance = gnc_numeric_create ( 5, 12);
    p_priv->balance_dirty = TRUE;
    p_priv->sort_dirty = TRUE;
    fixture->func->xaccFreeAccount (parent);
    g_assert_cmpint (check1.hits, ==, 6);
    g_assert_cmpint (check2.hits, ==, 6);
    /* cleanup what's left */
    g_log_remove_handler ("gnc.account", hdlr1);
    g_log_remove_handler ("gnc.engine", hdlr2);
    test_clear_error_list ();
    qof_book_destroy (book);
    g_free (fixture->func);
}
/* xaccAccountBeginEdit
void
xaccAccountBeginEdit (Account *acc)// C: 80 in 29 SCM: 15 in 9

No test, just a passthrough.
*/
/* static void
test_xaccAccountBeginEdit (Fixture *fixture, gconstpointer pData)
{
}*/
/* on_done
static void on_done (QofInstance *inst)// 2
***Callback for qof_commit_edit_part2
No test, just queues a qof event.
*/
/* static void
test_on_done (Fixture *fixture, gconstpointer pData)
{
}*/
/* on_err
static void on_err (QofInstance *inst, QofBackendError errcode)// 2
***Callback for qof_commit_edit_part2
*/
/* static void
test_on_err (Fixture *fixture, gconstpointer pData)
No test, just a pass-through.
{
}*/
/* acc_free
static void acc_free (QofInstance *inst)// 2
***Callback for qof_commit_edit_part2
No test, just a passthrough -- plus see comment at test_xaccFreeAccount, which is what this is a passtrough of.
*/
/* static void
test_acc_free (Fixture *fixture, gconstpointer pData)
{
}*/
/* destroy_pending_splits_for_account
static void
destroy_pending_splits_for_account (QofInstance *ent, gpointer acc)// 2

Pass-through, no test.
*/
/* static void
test_destroy_pending_splits_for_account (Fixture *fixture, gconstpointer pData)
{
}*/
/* xaccAccountCommitEdit
void
xaccAccountCommitEdit (Account *acc)// C: 65 in 26 SCM: 11 in 7
Also tests:
  xaccAccountDestroy
*/
static void
test_xaccAccountCommitEdit (Fixture *fixture, gconstpointer pData)
{
    gchar *msg1 = "[xaccFreeAccount()]  instead of calling xaccFreeAccount(), please call \n"
                  " xaccAccountBeginEdit(); xaccAccountDestroy(); \n";
#ifdef __clang__
#define _func "int xaccTransGetSplitIndex(const Transaction *, const Split *)"
#else
#define _func "xaccTransGetSplitIndex"
#endif
    gchar *msg2 = _func ": assertion " _Q "trans && split' failed";
#undef _func
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    TestErrorStruct check1 = { loglevel, "gnc.account", msg1, 0 };
    TestErrorStruct check2 = { loglevel, "gnc.engine", msg2, 0 };
    guint hdlr1, hdlr2;
    TestSignal sig1, sig2;
    QofBook *book = gnc_account_get_book (fixture->acct);
    Account *parent = gnc_account_get_parent (fixture->acct);
    AccountPrivate *p_priv = fixture->func->get_private (parent);
    const guint numItems = 3;
    guint i = 0;
    gnc_commodity *commodity = gnc_commodity_new (book, "US Dollar", "CURRENCY", "USD", "0", 100);
    test_add_error (&check1);
    test_add_error (&check2);
    hdlr1 = g_log_set_handler ("gnc.account", loglevel,
                               (GLogFunc)test_checked_handler, &check1);
    hdlr2 = g_log_set_handler ("gnc.engine", loglevel,
                               (GLogFunc)test_checked_handler, &check2);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_list_handler, NULL);
    for (i = 0; i < numItems; i++)
    {
        Split *split = xaccMallocSplit (book);
        xaccSplitSetAccount (split, parent);
        gnc_account_insert_split (parent, split);
        xaccAccountInsertLot (parent, gnc_lot_new (book));
    }
    xaccAccountSetCommodity (parent, commodity);
    /* Check that we've got children, lots, and splits to remove */
    g_assert (p_priv->children != NULL);
    g_assert (p_priv->lots != NULL);
    g_assert (p_priv->splits != NULL);
    g_assert (p_priv->parent != NULL);
    g_assert (p_priv->commodity != NULL);
    g_assert_cmpint (check1.hits, ==, 0);
    g_assert_cmpint (check2.hits, ==, 0);

    sig1 = test_signal_new (&parent->inst, QOF_EVENT_MODIFY, NULL);
    sig2 = test_signal_new (&parent->inst, QOF_EVENT_DESTROY, NULL);
    /* Now we're ready to start testing. */
    xaccAccountBeginEdit (parent);
    xaccAccountCommitEdit (parent);
    /* Make sure that the account didn't get destroyed */
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 0);
    g_assert (p_priv->children != NULL);
    g_assert (p_priv->lots != NULL);
    g_assert (p_priv->splits != NULL);
    g_assert (p_priv->parent != NULL);
    g_assert (p_priv->commodity != NULL);
    g_assert_cmpint (check1.hits, ==, 0);
    g_assert_cmpint (check2.hits, ==, 0);
    /* xaccAccountDestroy destroys the account by calling
     * qof_instance_set_destroying (), then xaccAccountCommitEdit ();
     */
    xaccAccountBeginEdit (parent);
    xaccAccountDestroy (parent);
    /* So this time we make sure that the account is destroyed */
    test_signal_assert_hits (sig1, 2);
    test_signal_assert_hits (sig2, 1);
    g_assert_cmpint (check1.hits, ==, 2);
    g_assert_cmpint (check2.hits, ==, 12);
    /* And clean up */
    test_signal_free (sig1);
    test_signal_free (sig2);
    g_log_remove_handler ("gnc.account", hdlr1);
    g_log_remove_handler ("gnc.engine", hdlr2);
    test_clear_error_list ();
    qof_book_destroy (book);
    g_free (fixture->func);
}
/* xaccAcctChildrenEqual
static gboolean
xaccAcctChildrenEqual (const GList *na,// 2
*/
/* static void
test_xaccAcctChildrenEqual (Fixture *fixture, gconstpointer pData)
{
}*/
/* xaccAccountEqual
gboolean
xaccAccountEqual (const Account *aa, const Account *ab, gboolean check_guids)// C: 8 in 6
Test support only; don't test for now.
*/
/* static void
test_xaccAccountEqual (Fixture *fixture, gconstpointer pData)
{
}*/
/*
  The following are getters and setters, unworthy of testing:
  gnc_account_get_sort_dirty *** Test Only ***
  gnc_account_set_sort_dirty
  gnc_account_get_balance_dirty *** Test Only ***
  gnc_account_set_balance_dirty
*/
/* gnc_account_find_split *** Test Only ***
 */
/* gnc_account_insert_split
gboolean
gnc_account_insert_split (Account *acc, Split *s)// C: 5 in 3

Also tests gnc_account_remove_split ()
*/
static void
test_gnc_account_insert_remove_split (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = gnc_account_get_book (fixture->acct);
    Split *split1 = xaccMallocSplit (book);
    Split *split2 = xaccMallocSplit (book);
    Split *split3 = xaccMallocSplit (book);
    TestSignal sig1, sig2, sig3;
    AccountPrivate *priv = fixture->func->get_private (fixture->acct);
#ifdef __clang__
#define _func "gboolean gnc_account_insert_split(Account *, Split *)"
#else
#define _func "gnc_account_insert_split"
#endif
    gchar *msg1 = _func ": assertion " _Q "GNC_IS_ACCOUNT(acc)' failed";
    gchar *msg2 = _func ": assertion " _Q "GNC_IS_SPLIT(s)' failed";
#undef _func
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
//    gchar *log_domain = "gnc.engine";
    TestErrorStruct check1 = { loglevel, "gnc.engine", msg1, 0 };
    TestErrorStruct check2 = { loglevel, "gnc.engine", msg2, 0 };
    TestErrorStruct check3 = { loglevel, "gnc.engine", NULL, 0 };
    guint logger;
    sig1 = test_signal_new (&fixture->acct->inst, QOF_EVENT_MODIFY, NULL);
    sig2 = test_signal_new (&fixture->acct->inst, GNC_EVENT_ITEM_ADDED, split1);

    test_add_error (&check1);
    test_add_error (&check2);
    logger = g_log_set_handler ("gnc.engine", loglevel,
                                (GLogFunc)test_null_handler, &check3);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_list_handler, NULL);

    /* Check that the call fails with invalid account and split (throws) */
    g_assert (!gnc_account_insert_split (NULL, split1));
    g_assert_cmpuint (g_list_length (priv->splits), == , 0);
    g_assert (!priv->sort_dirty);
    g_assert (!priv->balance_dirty);
    test_signal_assert_hits (sig1, 0);
    test_signal_assert_hits (sig2, 0);
    g_assert (!gnc_account_insert_split (fixture->acct, NULL));
    g_assert_cmpuint (g_list_length (priv->splits), == , 0);
    g_assert (!priv->sort_dirty);
    g_assert (!priv->balance_dirty);
    test_signal_assert_hits (sig1, 0);
    test_signal_assert_hits (sig2, 0);
    /* g_assert (!gnc_account_insert_split (fixture->acct, (Split*)priv)); */
    /* g_assert_cmpuint (g_list_length (priv->splits), == , 0); */
    /* g_assert (!priv->sort_dirty); */
    /* g_assert (!priv->balance_dirty); */
    /* test_signal_assert_hits (sig1, 0); */
    /* test_signal_assert_hits (sig2, 0); */
    g_assert_cmpint (check1.hits, ==, 1);
    g_assert_cmpint (check2.hits, ==, 1);
    g_assert_cmpint (check3.hits, ==, 0);
    g_log_remove_handler ("gnc.engine", logger);
    test_clear_error_list ();

    /* Check that it works the first time */
    g_assert (gnc_account_insert_split (fixture->acct, split1));
    g_assert_cmpuint (g_list_length (priv->splits), == , 1);
    g_assert (!priv->sort_dirty);
    g_assert (priv->balance_dirty);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 1);
    /* Check that it fails if the split has already been added once */
    g_assert (!gnc_account_insert_split (fixture->acct, split1));
    /* Free up hdlr2 and set up hdlr2 */
    test_signal_free (sig2);
    sig3 = test_signal_new (&fixture->acct->inst, GNC_EVENT_ITEM_ADDED, split2);
    /* Now add a second split to the account and check that sort_dirty isn't set. We have to bump the editlevel to force this. */
    g_assert (gnc_account_insert_split (fixture->acct, split2));
    g_assert_cmpuint (g_list_length (priv->splits), == , 2);
    g_assert (!priv->sort_dirty);
    g_assert (priv->balance_dirty);
    test_signal_assert_hits (sig1, 2);
    test_signal_assert_hits (sig3, 1);
    /* One more add, incrementing the editlevel to get sort_dirty set. */
    test_signal_free (sig3);
    sig3 = test_signal_new (&fixture->acct->inst, GNC_EVENT_ITEM_ADDED, split3);
    qof_instance_increase_editlevel (fixture->acct);
    g_assert (gnc_account_insert_split (fixture->acct, split3));
    qof_instance_decrease_editlevel (fixture->acct);
    g_assert_cmpuint (g_list_length (priv->splits), == , 3);
    g_assert (priv->sort_dirty);
    g_assert (priv->balance_dirty);
    test_signal_assert_hits (sig1, 3);
    test_signal_assert_hits (sig3, 1);
    /* Finally delete a split. It's going to recompute the balance, so
     * balance_dirty will be false. */
    test_signal_free (sig3);
    sig3 = test_signal_new (&fixture->acct->inst, GNC_EVENT_ITEM_REMOVED,
                            split3);
    g_assert (gnc_account_remove_split (fixture->acct, split3));
    g_assert_cmpuint (g_list_length (priv->splits), == , 2);
    g_assert (priv->sort_dirty);
    g_assert (!priv->balance_dirty);
    test_signal_assert_hits (sig1, 4);
    test_signal_assert_hits (sig3, 1);
    /* And do it again to make sure that it fails when the split has
     * already been removed */
    g_assert (!gnc_account_remove_split (fixture->acct, split3));
    g_assert_cmpuint (g_list_length (priv->splits), == , 2);
    g_assert (priv->sort_dirty);
    g_assert (!priv->balance_dirty);
    test_signal_assert_hits (sig1, 4);
    test_signal_assert_hits (sig3, 1);

    /* Clean up the handlers */
    test_signal_free (sig3);
    test_signal_free (sig1);
}
/* xaccAccountSortSplits
void
xaccAccountSortSplits (Account *acc, gboolean force)// C: 4 in 2
Make static?
Passthrough, no test.
*/
/* xaccAccountBringUpToDate
static void
xaccAccountBringUpToDate (Account *acc)// 3
Passthrough, no test.
*/
/* xaccAccountSetGUID
void
xaccAccountSetGUID (Account *acc, const GncGUID *guid)// C: 5 in 4
Getter/Setter, no test.
* Naughty function: GUID should be set at object construction and be invariant thereafter.
 */
/* xaccAccountLookup
Account *
xaccAccountLookup (const GncGUID *guid, QofBook *book)// C: 37 in 28 SCM: 2 in 1
Passthrough, no test.
*/
/* More getters/setters:
	xaccAccountGetMark *** Test Only ***
	xaccAccountSetMark *** Not Used ***
	xaccClearMark *** Not Used ***
	xaccClearMarkDown
	gnc_account_get_policy
	gnc_account_set_policy
*/
/* xaccAccountRemoveLot
void
xaccAccountRemoveLot (Account *acc, GNCLot *lot)// C: 6 in 4 */
static void
test_xaccAccountInsertRemoveLot (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = gnc_account_get_book (fixture->acct);
    GNCLot *lot = gnc_lot_new (book);
    Account *parent = gnc_account_get_parent (fixture->acct);
    TestSignal sig1 = test_signal_new (QOF_INSTANCE (lot),
                                       QOF_EVENT_ADD, NULL);
    TestSignal sig2 = test_signal_new (&fixture->acct->inst,
                                       QOF_EVENT_MODIFY, NULL);
    TestSignal sig3 = test_signal_new (QOF_INSTANCE (lot),
                                       QOF_EVENT_REMOVE, NULL);
    TestSignal sig4 = test_signal_new (&parent->inst, QOF_EVENT_MODIFY, NULL);

    AccountPrivate *a_priv = fixture->func->get_private (fixture->acct);
    AccountPrivate *p_priv = fixture->func->get_private (parent);

    g_assert_cmpuint (g_list_length (a_priv->lots), == , 0);
    g_assert_cmpuint (g_list_length (p_priv->lots), == , 0);
    xaccAccountInsertLot (fixture->acct, lot);
    g_assert (gnc_lot_get_account (lot) == fixture->acct);
    g_assert_cmpuint (g_list_length (a_priv->lots), == , 1);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 1);
    /* Make sure that inserting again doesn't do anything */
    xaccAccountInsertLot (fixture->acct, lot);
    g_assert (gnc_lot_get_account (lot) == fixture->acct);
    g_assert_cmpuint (g_list_length (a_priv->lots), == , 1);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 1);
    /* Check that inserting the lot into a different account changes the lot */
    xaccAccountInsertLot (parent, lot);
    g_assert (gnc_lot_get_account (lot) == parent);
    g_assert_cmpuint (g_list_length (a_priv->lots), == , 0);
    g_assert_cmpuint (g_list_length (p_priv->lots), == , 1);
    test_signal_assert_hits (sig1, 2);
    test_signal_assert_hits (sig4, 1);
    test_signal_assert_hits (sig2, 1);
    /* Check that removing the lot works */
    xaccAccountRemoveLot (parent, lot);
    /* The following test should fail, but it doesn't because of an
     * error in the routine: When removing a lot from an account, the
     * account reference in the lot object should be NULLed. */
    g_assert (gnc_lot_get_account (lot) != NULL);
    g_assert_cmpuint (g_list_length (a_priv->lots), == , 0);
    g_assert_cmpuint (g_list_length (p_priv->lots), == , 0);
    test_signal_assert_hits (sig3, 1);
    test_signal_assert_hits (sig4, 2);
    test_signal_assert_hits (sig2, 1);
    /* Check that destroying the lot removes its reference */
    /* Because the lot's account pointer doesn't get nulled when the lot
     * is removed, we have to do that for the next test to work: */
    gnc_lot_set_account (lot, NULL);
    xaccAccountInsertLot (parent, lot);
    g_assert_cmpuint (g_list_length (p_priv->lots), == , 1);
    g_assert (gnc_lot_get_account (lot) == parent);
    gnc_lot_destroy (lot);
    /* Destroying the lot should remove it from the account; Not Happening. */
    g_assert_cmpuint (g_list_length (p_priv->lots), != , 0);
    test_signal_assert_hits (sig1, 3);
    /* We get a modify only on the insert, since there is no remove when
     * the lot is destroyed. */
    test_signal_assert_hits (sig4, 3);
    /* Same thing: There should be a "removed" signal on the lot, but
     * since it isn't removed, no signal. */
    test_signal_assert_hits (sig3, 1);
    test_signal_free (sig1);
    test_signal_free (sig2);
    test_signal_free (sig3);
    test_signal_free (sig4);

}
/*
static void
xaccPreSplitMove (Split *split, gpointer dummy)// 2
static void
xaccPostSplitMove (Split *split, Account *accto)// 2
void
xaccAccountMoveAllSplits (Account *accfrom, Account *accto)// C: 5 in 3

* All of the work is done in other classes which in turn call other
* Account functions (gnc_account_add_split and
* gnc_account_remove_split). Mock classes with the functions that call
* those wouldn't really achieve anything, since the actual functions
* could break by failing to call gnc_account_*_split and the test
* wouldn't know.
*/

/* xaccAccountRecomputeBalance
void
xaccAccountRecomputeBalance (Account * acc)// C: 9 in 5 */
static void
test_xaccAccountRecomputeBalance (Fixture *fixture, gconstpointer pData)
{
    AccountPrivate *priv = fixture->func->get_private (fixture->acct);
    gnc_numeric bal = gnc_numeric_zero (), rec_bal = gnc_numeric_zero (),
                clr_bal = gnc_numeric_zero ();
    SetupData *sdata = (SetupData*)pData;
    TxnParms* t_arr;
    int ind;
    g_assert (sdata != NULL);
    t_arr = (TxnParms*)sdata->txns;
    for (ind = 0; ind < sdata->num_txns; ind++)
    {
        SplitParms p = t_arr[ind].splits[1];
        bal = gnc_numeric_add_fixed (bal, p.amount);
        if (p.reconciled != NREC)
            clr_bal = gnc_numeric_add_fixed (clr_bal, p.amount);
        if (p.reconciled == YREC || p.reconciled == FREC)
            rec_bal = gnc_numeric_add_fixed (rec_bal, p.amount);
    }
    g_assert (gnc_numeric_zero_p (priv->starting_balance));
    g_assert (gnc_numeric_zero_p (priv->balance));
    priv->balance_dirty = TRUE;
    xaccAccountRecomputeBalance (fixture->acct);
    g_assert (gnc_numeric_zero_p (priv->starting_balance));
    g_assert (gnc_numeric_eq (priv->balance, bal));
    g_assert (gnc_numeric_eq (priv->cleared_balance, clr_bal));
    g_assert (gnc_numeric_eq (priv->reconciled_balance, rec_bal));
    g_assert (!priv->balance_dirty);
}

/* xaccAccountOrder
int
xaccAccountOrder (const Account *aa, const Account *ab)// C: 11 in 3 */
static void
test_xaccAccountOrder ( )
{
    Account *aa = NULL, *ab = NULL;
    QofBook *book = qof_book_new ();


    g_assert (xaccAccountOrder (aa, ab) == 0);
    aa = xaccMallocAccount (book);
    g_assert (xaccAccountOrder (aa, ab) == -1);
    g_assert (xaccAccountOrder (ab, aa) == 1);

    ab = xaccMallocAccount (book);
    qof_instance_increase_editlevel (aa);
    qof_instance_increase_editlevel (ab);
    g_object_set (G_OBJECT (aa),
                  "code", "3333",
                  "type", ACCT_TYPE_ASSET,
                  "name", "foo",
                  NULL);
    g_object_set (G_OBJECT (ab),
                  "code", "3333",
                  "type", ACCT_TYPE_ASSET,
                  "name", "foo",
                  NULL);

    g_assert_cmpint (xaccAccountOrder (aa, aa), == , 0);
    g_assert_cmpint (xaccAccountOrder (aa, ab), == ,
                     qof_instance_guid_compare (aa, ab));
    g_object_set (G_OBJECT (ab), "name", "bar", NULL);
    g_assert_cmpint (xaccAccountOrder (aa, ab), > , 0);
    g_object_set (G_OBJECT (ab), "name", "waldo", NULL);
    g_assert_cmpint (xaccAccountOrder (aa, ab), < , 0);

    g_object_set (G_OBJECT (ab), "type", ACCT_TYPE_BANK, NULL);
    g_test_message ("The next test should fail: There's an error in the sort"
                    " sequence that causes ACCT_TYPE_BANK to sort last"
                    " instead of first\n");
    g_assert_cmpint (xaccAccountOrder (aa, ab), < , 0);
    g_object_set (G_OBJECT (ab), "type", ACCT_TYPE_STOCK, NULL);
    g_assert_cmpint (xaccAccountOrder (aa, ab), > , 0);
    g_object_set (G_OBJECT (ab),
                  "type", ACCT_TYPE_INCOME,
                  "name", "bar",
                  NULL);
    g_assert_cmpint (xaccAccountOrder (aa, ab), < , 0);

    g_object_set (G_OBJECT (ab), "code", "2222",
                  "name", "waldo",
                  NULL);
    g_assert_cmpint (xaccAccountOrder (aa, ab), > , 0);
    g_object_set (G_OBJECT (ab),
                  "code", "4444",
                  "type", ACCT_TYPE_STOCK,
                  "name", "bar",
                  NULL);
    g_assert_cmpint (xaccAccountOrder (aa, ab), < , 0);
    qof_instance_decrease_editlevel (aa);
    qof_instance_decrease_editlevel (ab);

    xaccAccountBeginEdit (aa);
    xaccAccountDestroy (aa);
    xaccAccountBeginEdit (ab);
    xaccAccountDestroy (ab);
    g_object_unref (book);
}
/* qof_xaccAccountOrder
static int
qof_xaccAccountOrder (const Account **aa, const Account **ab)// 2
Pass-through
*/
/*
 * The following functions are tested in get/set above:
 * xaccAccountSetType
 * xaccAccountSetName
 * xaccAccountSetCode
 * xaccAccountSetDescription
 * xaccAccountSetColor
 * xaccAccountSetNotes
 * xaccAccountSetCommodity
 * xaccAccountSetCommoditySCU
 * xaccAccountGetCommoditySCUi
 * xaccAccountGetCommoditySCU
 * xaccAccountSetNonStdSCU
 * xaccAccountGetNonStdSCU
 */
/* DxaccAccountSetCurrency
void
DxaccAccountSetCurrency (Account * acc, gnc_commodity * currency)// C: 7 in 5
Deprecated, don't test

*/

/* qofAccountSetParent
static void
qofAccountSetParent (Account *acc, QofInstance *parent)// 2
*/
static void
test_qofAccountSetParent (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *old_parent = gnc_account_get_parent (fixture->acct);
    AccountTestFunctions *func = _utest_account_fill_functions ();
    g_assert (root != old_parent);
    /* qofAccountSetParent doesn't check to see if the parent is already
     * set, nor does gnc_account_append_child, which is the passed-through
     * function.
     */
    func->qofAccountSetParent (fixture->acct, QOF_INSTANCE (root));
    g_assert (root == gnc_account_get_parent (fixture->acct));
    g_assert (qof_instance_get_dirty (QOF_INSTANCE (fixture->acct)));
    g_assert (qof_instance_get_dirty (QOF_INSTANCE (root)));
    g_assert (qof_instance_get_dirty (QOF_INSTANCE (old_parent)));
}
/* gnc_account_append_child
void
gnc_account_append_child (Account *new_parent, Account *child)// C: 29 in 18 SCM: 7 in 4*/
/* gnc_account_remove_child
void
gnc_account_remove_child (Account *parent, Account *child)// C: 4 in 2 */
static void
test_gnc_account_append_remove_child (Fixture *fixture, gconstpointer pData)
{
    QofBook *book = gnc_account_get_book (fixture->acct);
    QofBook *fbook = qof_book_new ();
    Account *froot = gnc_account_create_root (fbook);
    Account *account = xaccMallocAccount (fbook);
    gchar *logdomain = "gnc.account";
    gchar *msg1 = "[gnc_account_append_child()] reparenting accounts across books is not correctly supported\n";
    gchar *msg2 = "[gnc_account_remove_child()] account not a child of parent";
    guint log_handler = 0;
    TestErrorStruct check_warn = {G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL, "gnc.account", msg1, 0 };
    TestErrorStruct check_err = {G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL, "gnc.account", msg2, 0 };
    TestSignal sig1, sig2, sig3;
    AccountTestFunctions *func = _utest_account_fill_functions ();
    AccountPrivate *frpriv = func->get_private (froot),
                    *apriv = func->get_private (fixture->acct);
    const GncGUID *acct_guid = qof_instance_get_guid (QOF_INSTANCE (account));
    sig1 = test_signal_new (QOF_INSTANCE (account), QOF_EVENT_ADD, NULL);
    sig2 = test_signal_new (QOF_INSTANCE (account), QOF_EVENT_DESTROY, NULL);
    sig3 = test_signal_new (QOF_INSTANCE (account), QOF_EVENT_CREATE, NULL);

    gnc_account_append_child (froot, account);
    g_assert (gnc_account_get_parent (account) == froot);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 0);
    test_signal_assert_hits (sig3, 0);
    g_assert_cmpint (check_warn.hits, ==, 0);
    g_assert_cmpint (check_err.hits, ==, 0);
    g_assert (qof_instance_get_dirty (QOF_INSTANCE (froot)));
    g_assert (qof_instance_get_dirty (QOF_INSTANCE (account)));
    g_assert (g_list_find (frpriv->children, account));
    g_assert (qof_collection_lookup_entity (
                  qof_book_get_collection (fbook, GNC_ID_ACCOUNT),
                  acct_guid));
    log_handler = g_log_set_handler (logdomain, G_LOG_LEVEL_WARNING | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION, (GLogFunc)test_null_handler, &check_warn);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check_warn);
    gnc_account_append_child (fixture->acct, account);
    g_log_remove_handler (logdomain, log_handler);
    g_assert_cmpstr (msg1, == , check_warn.msg);
    g_assert (gnc_account_get_parent (account) == fixture->acct);
    test_signal_assert_hits (sig1, 2);
    test_signal_assert_hits (sig2, 1);
    test_signal_assert_hits (sig3, 1);
    g_assert_cmpint (check_warn.hits, ==, 1);
    g_assert_cmpint (check_err.hits, ==, 0);
    g_assert (!qof_collection_lookup_entity (
                  qof_book_get_collection (fbook, GNC_ID_ACCOUNT),
                  acct_guid));
    g_assert (qof_collection_lookup_entity (
                  qof_book_get_collection (book, GNC_ID_ACCOUNT),
                  acct_guid));
    g_assert (qof_instance_get_dirty (QOF_INSTANCE (fixture->acct)));
    g_assert (g_list_find (frpriv->children, account) == NULL);
    g_assert (g_list_find (apriv->children, account));

    test_signal_free (sig1);
    test_signal_free (sig2);
    test_signal_free (sig3);
    sig1 = test_signal_new (&account->inst, QOF_EVENT_REMOVE, NULL);
    sig2 = test_signal_new (&(fixture->acct)->inst, QOF_EVENT_MODIFY, NULL);
    log_handler = g_log_set_handler (logdomain, G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL,
                                     (GLogFunc)test_null_handler, &check_err);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check_err);
    gnc_account_remove_child (froot, account);
    g_log_remove_handler (logdomain, log_handler);

    test_signal_assert_hits (sig1, 0);
    test_signal_assert_hits (sig2, 0);
    g_assert_cmpint (check_err.hits, ==, 1);
    g_assert_cmpint (check_warn.hits, ==, 1);

    gnc_account_remove_child (fixture->acct, account);
    g_assert (gnc_account_get_parent (account) == NULL);
    g_assert (g_list_find (apriv->children, account) == NULL);
    test_signal_assert_hits (sig1, 1);
    test_signal_assert_hits (sig2, 1);
    g_assert_cmpint (check_warn.hits, ==, 1);
    g_assert_cmpint (check_err.hits, ==, 1);
    test_signal_free (sig1);
    test_signal_free (sig2);
    xaccAccountBeginEdit (account);
    xaccAccountDestroy (account);
    xaccAccountBeginEdit (froot);
    xaccAccountDestroy (froot);
    g_object_unref (fbook);

}
/* Simple Getters or passthroughs, no tests:
 * gnc_account_get_parent
 * gnc_account_get_root
 * gnc_account_is_root
 * gnc_account_get_children
 * gnc_account_get_children_sorted
 * gnc_account_n_children
 * gnc_account_child_index
 * gnc_account_nth_child
 */
/* gnc_account_n_descendants
gint
gnc_account_n_descendants (const Account *account)// C: 12 in 6 */
static void
test_gnc_account_n_descendants (Fixture *fixture, gconstpointer pData)
{
    g_assert_cmpint (
        gnc_account_n_descendants (
            gnc_account_get_root (fixture->acct)), == , 4);
}
/* gnc_account_get_current_depth
gint
gnc_account_get_current_depth (const Account *account)// C: 4 in 2 SCM: 12 in 4*/
static void
test_gnc_account_get_current_depth (Fixture *fixture, gconstpointer pData)
{
    g_assert_cmpint (
        gnc_account_get_current_depth (fixture->acct), == , 2);
}
/* gnc_account_get_tree_depth
gint
gnc_account_get_tree_depth (const Account *account)// C: 4 in 2 SCM: 3 in 3*/
static void
test_gnc_account_get_tree_depth (Fixture *fixture, gconstpointer pData)
{
    /* Magic result value based on depth of the "complex" AccountParms array. */
    g_assert_cmpint (
        gnc_account_get_tree_depth (
            gnc_account_get_root (fixture->acct)), == , 5);
    g_assert_cmpint (
        gnc_account_get_tree_depth (fixture->acct), == , 1);
}
/* gnc_account_get_descendants
GList *
gnc_account_get_descendants (const Account *account)// C: 24 in 17 SCM: 6 in 6*/
static void
print_account (gpointer item, gpointer data)
{
    Account *account = (Account *)item;
    gchar *name, *code, *desc;
    GNCAccountType type;
    const gchar *typestr;
    g_object_get (account,
                  "type", &type,
                  "name", &name,
                  "code", &code,
                  "description", &desc,
                  NULL);
    typestr = xaccAccountGetTypeStr (type);
    g_test_message ("%s: %s, %s %s", typestr, code, name, desc);
    g_free (code);
    g_free (name);
    g_free (desc);
}

static void
test_gnc_account_get_descendants (Fixture *fixture, gconstpointer pData)
{

    GList *list = gnc_account_get_descendants (
                      gnc_account_get_root (fixture->acct));
    g_assert (list != NULL);
    g_assert_cmpuint (g_list_length (list), == , 34);
    g_assert_cmpint (g_list_index (list, fixture->acct), == , 33);
    g_list_free (list);
    list = gnc_account_get_descendants (fixture->acct);
    g_assert (list == NULL);
}
/* gnc_account_get_descendants_sorted
GList *
gnc_account_get_descendants_sorted (const Account *account)// C: 6 in 4 SCM: 62 in 46*/
static void
test_gnc_account_get_descendants_sorted (Fixture *fixture, gconstpointer pData)
{
    GList *list = gnc_account_get_descendants_sorted (
                      gnc_account_get_root (fixture->acct));
    g_assert (list != NULL);
    g_list_foreach (list, print_account, NULL);
    g_assert_cmpuint (g_list_length (list), == , 34);
    g_assert_cmpint (g_list_index (list, fixture->acct), == , 10);
    g_list_free (list);
}
/* gnc_account_lookup_by_name
Account *
gnc_account_lookup_by_name (const Account *parent, const char * name)// C: 22 in 12 */
static void
test_gnc_account_lookup_by_name (Fixture *fixture, gconstpointer pData)
{
    Account *root, *target;
    gchar *code;
    root = gnc_account_get_root (fixture->acct);
    target = gnc_account_lookup_by_name (root, "income");
    g_assert (target != NULL);
    g_object_get (target, "code", &code, NULL);
    g_assert_cmpstr (code, == , "4000");
    target = gnc_account_lookup_by_name (target, "int");
    g_assert (target != NULL);
    g_free (code);
    g_object_get (target, "code", &code, NULL);
    g_assert_cmpstr (code, == , "4160");
    target = gnc_account_lookup_by_name (root, "bank");
    g_assert (target != NULL);
    g_free (code);
    g_object_get (target, "code", &code, NULL);
    g_assert_cmpstr (code, == , "2300");
    target = gnc_account_lookup_by_name (target, "int");
    g_assert (target == NULL);
    g_free (code);

}
/* gnc_account_lookup_by_code
Account *
gnc_account_lookup_by_code (const Account *parent, const char * code)// C: 5 in 3 */
static void
test_gnc_account_lookup_by_code (Fixture *fixture, gconstpointer pData)
{
    Account *root, *target;
    gchar *name;
    root = gnc_account_get_root (fixture->acct);
    target = gnc_account_lookup_by_code (root, "3100");
    g_assert (target != NULL);
    g_object_get (target, "name", &name, NULL);
    g_assert_cmpstr (name, == , "ordinary");
    g_free (name);
    target = gnc_account_lookup_by_code (target, "3150");
    g_assert (target != NULL);
    g_object_get (target, "name", &name, NULL);
    g_assert_cmpstr (name, == , "utilities");
    target = gnc_account_lookup_by_code (target, "2100");
    g_assert (target == NULL);
    g_free (name);
}
/* gnc_account_lookup_by_full_name_helper
static Account *
gnc_account_lookup_by_full_name_helper (const Account *parent,// 3
*/
static void
test_gnc_account_lookup_by_full_name_helper ( Fixture *fixture,
        gconstpointer pData )
{
    Account *root, *target;
    gchar *names1[] = {"income", "taxable", "int", NULL};
    gchar *names2[] =  {"income", "exempt", "int", NULL};
    gchar *names3[] = {"expense", "taxable", "int", NULL};
    gchar *code;
    AccountTestFunctions *func = _utest_account_fill_functions ();

    root = gnc_account_get_root (fixture->acct);
    target = func->gnc_account_lookup_by_full_name_helper (root, names1);
    g_assert (target != NULL);
    g_object_get (target, "code", &code, NULL);
    g_assert_cmpstr (code, == , "4160");
    g_free (code);
    target = func->gnc_account_lookup_by_full_name_helper (root, names2);
    g_assert (target != NULL);
    g_object_get (target, "code", &code, NULL);
    g_assert_cmpstr (code, == , "4210");
    target = func->gnc_account_lookup_by_full_name_helper (root, names3);
    g_assert (target == NULL);
    g_free (code);
}
/* gnc_account_lookup_by_full_name
Account *
gnc_account_lookup_by_full_name (const Account *any_acc,// C: 15 in 11 SCM: 8 in 4*/
static void
test_gnc_account_lookup_by_full_name (Fixture *fixture, gconstpointer pData)
{
    Account *root, *target;
    gchar *names1 = "income:taxable:int";
    gchar *names2 =  "income:exempt:int";
    gchar *names3 = "expense:taxable:int";
    gchar *code;

    root = gnc_account_get_root (fixture->acct);
    target = gnc_account_lookup_by_full_name (root, names1);
    g_assert (target != NULL);
    g_object_get (target, "code", &code, NULL);
    g_assert_cmpstr (code, == , "4160");
    g_free (code);
    target = gnc_account_lookup_by_full_name (root, names2);
    g_assert (target != NULL);
    g_object_get (target, "code", &code, NULL);
    g_assert_cmpstr (code, == , "4210");
    target = gnc_account_lookup_by_full_name (root, names3);
    g_assert (target == NULL);
    g_free (code);
}

static void
thunk (Account *s, gpointer data)
{
    guint *counter = (guint*)data;
    g_assert (GNC_IS_ACCOUNT (s));
    ++(*counter);
}

static gpointer
thunk2 (Account *s, gpointer data)
{
    guint *counter = (guint*)data;
    gchar *name;
    g_assert (GNC_IS_ACCOUNT (s));
    g_object_get (G_OBJECT (s), "name", &name, NULL);
    if (!g_strcmp0 (name, "int"))
    {
        g_free (name);
        return s;
    }
    g_free (name);
    ++(*counter);
    return NULL;
}
/* gnc_account_foreach_child
void
gnc_account_foreach_child (const Account *acc,// C: 6 in 3 */
static void
test_gnc_account_foreach_child (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *begin = gnc_account_lookup_by_code (root, "4000");
    guint counter = 0;
    gnc_account_foreach_child (begin, thunk, &counter);
    g_assert_cmpint (counter, == , 2);
}
/* gnc_account_foreach_child_until  *** Not Used ***
gpointer
gnc_account_foreach_child_until (const Account *acc,// C: 4 in 2 */
/*static void
test_gnc_account_foreach_child_until (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *first = gnc_account_lookup_by_code (root, "4000");
    Account *second = gnc_account_lookup_by_code (root, "4100");
    Account *expected = gnc_account_lookup_by_code (root, "4160");
    Account *result;
    guint counter = 0;
    result = gnc_account_foreach_child_until (first, thunk2, &counter);
    g_assert_cmpint (counter, ==, 2);
    g_assert (result == NULL);
    counter = 0;
    result = gnc_account_foreach_child_until (second, thunk2, &counter);
    g_assert (result == expected);
    g_assert_cmpint (counter, ==, 3);
    }*/
/* gnc_account_foreach_descendant
void
gnc_account_foreach_descendant (const Account *acc,// C: 23 in 14 */
static void
test_gnc_account_foreach_descendant (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *begin = gnc_account_lookup_by_code (root, "4000");
    guint counter = 0;
    gnc_account_foreach_descendant (begin, thunk, &counter);
    g_assert_cmpint (counter, == , 13);
}
/* gnc_account_foreach_descendant_until
gpointer
gnc_account_foreach_descendant_until (const Account *acc,// C: 8 in 6 */
static void
test_gnc_account_foreach_descendant_until (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *first = gnc_account_lookup_by_code (root, "2000");
    Account *second = gnc_account_lookup_by_code (root, "4000");
    Account *expected = gnc_account_lookup_by_code (root, "4160");
    Account *result;
    guint counter = 0;
    result = gnc_account_foreach_descendant_until (first, thunk2, &counter);
    g_assert_cmpint (counter, == , 11);
    g_assert (result == NULL);
    counter = 0;
    result = gnc_account_foreach_descendant_until (second, thunk2, &counter);
    g_assert (result == expected);
    g_assert_cmpint (counter, == , 6);
}
/* More getter/setters:
 * xaccAccountGetType
 * qofAccountGetTypeString
 * qofAccountSetType
 * xaccAccountGetName
 * xaccAccountGetCode
 * xaccAccountGetDescription
 * xaccAccountGetColor
 * xaccAccountGetNotes
 * xaccAccountGetCommodity
 * gnc_account_set_start_balance
 * gnc_account_set_start_cleared_balance
 * gnc_account_set_start_reconciled_balance
 * xaccAccountGetBalance
 * xaccAccountGetClearedBalance C: 1
 * xaccAccountGetReconciledBalance
 */
/* gnc_account_get_full_name
gchar *
gnc_account_get_full_name (const Account *account)// C: 38 in 21 SCM: 29 in 19*/
static void
test_gnc_account_get_full_name (Fixture *fixture, gconstpointer pData)
{
    gchar *result;
    result = gnc_account_get_full_name (NULL);
    g_assert (result != NULL);
    g_assert_cmpstr (result, == , "");
    g_free (result);
    result = gnc_account_get_full_name (gnc_account_get_root (fixture->acct));
    g_assert (result != NULL);
    g_assert_cmpstr (result, == , "");
    g_free (result);
    result = gnc_account_get_full_name (fixture->acct);
    g_assert (result != NULL);
    g_assert_cmpstr (result, == , "foo:baz:waldo");
    g_free (result);

}

/* DxaccAccountGetCurrency
gnc_commodity *
DxaccAccountGetCurrency (const Account *acc)// C: 9 in 5
Deprecated, Don't test.
*/
/* xaccAccountGetProjectedMinimumBalance
gnc_numeric
xaccAccountGetProjectedMinimumBalance (const Account *acc)// C: 4 in 2 */
static void
test_xaccAccountGetProjectedMinimumBalance (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric val, bal = gnc_numeric_zero ();
    gfloat dval;
    gfloat dbal = 0.0;
    SetupData *sdata = (SetupData*)pData;
    TxnParms* t_arr;
    int ind;
    gint min_ind = 4;
    g_assert (sdata != NULL);
    t_arr = (TxnParms*)sdata->txns;
    for (ind = 0; ind < min_ind; ind++)
    {
        SplitParms p = t_arr[ind].splits[1];
        bal = gnc_numeric_add_fixed (bal, p.amount);
    }
    dbal = gnc_numeric_to_double (bal);
    val = xaccAccountGetProjectedMinimumBalance (fixture->acct);
    dval = gnc_numeric_to_double (val);
    g_assert_cmpfloat (dval, == , 0.0);
    xaccAccountRecomputeBalance (fixture->acct);
    val = xaccAccountGetProjectedMinimumBalance (fixture->acct);
    dval = gnc_numeric_to_double (val);
    g_assert_cmpfloat (dval, == , dbal);
}
/* xaccAccountGetBalanceAsOfDate
gnc_numeric
xaccAccountGetBalanceAsOfDate (Account *acc, time64 date)// C: 12 in 7 SCM: 4 in 4*/
static void
test_xaccAccountGetBalanceAsOfDate (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric val, bal = gnc_numeric_zero ();
    gfloat dval;
    gfloat dbal = 0.0;
    SetupData *sdata = (SetupData*)pData;
    TxnParms* t_arr;
    int ind;
    gint min_ind = 2;
    gint offset = 24 * 3600 * 3; /* 3 days in seconds */
    g_assert (sdata != NULL);
    t_arr = (TxnParms*)sdata->txns;
    for (ind = 0; ind < min_ind; ind++)
    {
        SplitParms p = t_arr[ind].splits[1];
        bal = gnc_numeric_add_fixed (bal, p.amount);
    }
    dbal = gnc_numeric_to_double (bal);
    xaccAccountRecomputeBalance (fixture->acct);
    val = xaccAccountGetBalanceAsOfDate (fixture->acct,
                                         (gnc_time (NULL) - offset));
    dval = gnc_numeric_to_double (val);
    g_assert_cmpfloat (dval, == , dbal);
}
/* xaccAccountGetPresentBalance
gnc_numeric
xaccAccountGetPresentBalance (const Account *acc)// C: 4 in 2 */
static void
test_xaccAccountGetPresentBalance (Fixture *fixture, gconstpointer pData)
{
    gnc_numeric val, bal = gnc_numeric_zero ();
    gfloat dval;
    gfloat dbal = 0.0;
    SetupData *sdata = (SetupData*)pData;
    TxnParms* t_arr;
    int ind;
    gint min_ind = 3;
    g_assert (sdata != NULL);
    t_arr = (TxnParms*)sdata->txns;
    for (ind = 0; ind < min_ind; ind++)
    {
        SplitParms p = t_arr[ind].splits[1];
        bal = gnc_numeric_add_fixed (bal, p.amount);
    }
    dbal = gnc_numeric_to_double (bal);
    xaccAccountRecomputeBalance (fixture->acct);
    val = xaccAccountGetPresentBalance (fixture->acct);
    dval = gnc_numeric_to_double (val);
    g_assert_cmpfloat (dval, == , dbal);
}
/*
 * xaccAccountConvertBalanceToCurrency
 * xaccAccountConvertBalanceToCurrencyAsOfDate are wrappers around
 * gnc_pricedb_convert_balance_latest_price and
 * gnc_pricedb_convert_balance_nearest_price. Don't test.
 *
 * The rest of these are convenience functions that wrap
 * xaccAccountConvertBalanceToCurrency* with one of the balance getter
 * functions tested immediately above and one of the recursion
 * functions, tested above those. There's no point in testing them.
 *
 * xaccAccountGetXxxBalanceInCurrency
 * xaccAccountGetXxxBalanceAsOfDateInCurrency
 * xaccAccountBalanceHelper
 * xaccAccountBalanceAsOfDateHelper
 * xaccAccountGetXxxBalanceInCurrencyRecursive
 * xaccAccountGetXxxBalanceAsOfDateInCurrencyRecursive
 * xaccAccountGetBalanceInCurrency
 * xaccAccountGetClearedBalanceInCurrency
 * xaccAccountGetReconciledBalanceInCurrency
 * xaccAccountGetPresentBalanceInCurrency
 * xaccAccountGetProjectedMinimumBalanceInCurrency
 * xaccAccountGetBalanceAsOfDateInCurrency
 * xaccAccountGetBalanceChangeForPeriod
 */
/*
 * Yet more getters & setters:
 * xaccAccountGetSplitList
 * xaccAccountGetLotList
 */
/* xaccAccountFindOpenLots
LotList *
xaccAccountFindOpenLots (const Account *acc,// C: 24 in 13 */

static gboolean
bogus_lot_match_func_true (GNCLot *lot, gpointer p_data)
{
    return TRUE;
}

static gboolean
bogus_lot_match_func_false (GNCLot *lot, gpointer p_data)
{
    return FALSE;
}

static guint count_sorts = 0;
static gint
bogus_lot_sort_func (gconstpointer a, gconstpointer b)
{
    ++count_sorts;
    return 0;
}

static void
test_xaccAccountFindOpenLots (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *acct = gnc_account_lookup_by_name (root, "baz");
    LotList* lots;

    g_assert (acct);
    lots = xaccAccountFindOpenLots (acct, NULL, NULL, NULL);
    g_assert (g_list_length (lots) == 2);
    if (lots) g_list_free (lots);
    lots = xaccAccountFindOpenLots (acct, bogus_lot_match_func_true,
                                    NULL, NULL);
    g_assert (g_list_length (lots) == 2);
    if (lots) g_list_free (lots);
    lots = xaccAccountFindOpenLots (acct, bogus_lot_match_func_false,
                                    NULL, NULL);
    g_assert (g_list_length (lots) == 0);
    if (lots) g_list_free (lots);
    lots = xaccAccountFindOpenLots (acct, NULL, NULL, bogus_lot_sort_func);
    g_assert_cmpint (count_sorts, == , 1);
    g_assert (g_list_length (lots) == 2);
    if (lots) g_list_free (lots);
    count_sorts = 0;
}

static gpointer
bogus_for_each_lot_func (GNCLot *lot, gpointer data)
{
    guint *count = data;
    ++*count;
    return (*count > 4 ? lot : NULL);
}

/* xaccAccountForEachLot
gpointer
xaccAccountForEachLot (const Account *acc,// C: 2 in 2 */
static void
test_xaccAccountForEachLot (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *acct = gnc_account_lookup_by_name (root, "baz");
    guint count_calls = 0;

    g_assert (acct);
    xaccAccountForEachLot (acct, bogus_for_each_lot_func, &count_calls);
    g_assert_cmpint (count_calls, == , 3);
    xaccAccountForEachLot (acct, bogus_for_each_lot_func, &count_calls);
    g_assert_cmpint (count_calls, == , 5);
}
/* These getters and setters look in KVP, so I guess their delegators instead:
 * xaccAccountGetTaxRelated
 * xaccAccountSetTaxRelated
 * xaccAccountGetTaxUSCode
 * xaccAccountSetTaxUSCode
 * xaccAccountGetTaxUSPayerNameSource
 * xaccAccountSetTaxUSPayerNameSource
 * xaccAccountGetTaxUSCopyNumber
 * xaccAccountSetTaxUSCopyNumber
 * xaccAccountGetPlaceholder
 * xaccAccountSetPlaceholder
 * xaccAccountGetDescendantPlaceholder
 * xaccAccountGetHidden
 * xaccAccountSetHidden
 * xaccAccountIsHidden
*/
/* xaccAccountHasAncestor
gboolean
xaccAccountHasAncestor (const Account *acc, const Account * ancestor)// C: 5 in 3 */
static void
test_xaccAccountHasAncestor (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *ltcg = gnc_account_lookup_by_name (root, "ltcg");
    Account *income = gnc_account_lookup_by_name (root, "income");
    Account *expense = gnc_account_lookup_by_name (root, "expense");

    g_assert (root);
    g_assert (ltcg);
    g_assert (income);
    g_assert (expense);
    g_assert (xaccAccountHasAncestor (fixture->acct, root));
    g_assert (xaccAccountHasAncestor (ltcg, income));
    g_assert (!xaccAccountHasAncestor (ltcg, expense));

}
/* xaccAccountTypeEnumAsString
 * xaccAccountStringToType
 * xaccAccountStringToEnum
 * xaccAccountGetTypeStr
 * xaccAccountIsPriced
const char *
xaccAccountTypeEnumAsString (GNCAccountType type)// C: 5 in 3 */
static void
test_xaccAccountType_Stuff (void)
{
    GNCAccountType  type;
    gint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *logdomain = "gnc.account";
    gchar *msg1 = g_strdup_printf ("[xaccAccountTypeEnumAsString()] asked to translate unknown account type %d.\n", ACCT_TYPE_LAST);
    gchar *msg2 = "[xaccAccountStringToType()] asked to translate unknown account type string (null).\n";
    gchar *msg3 = "[xaccAccountStringToType()] asked to translate unknown account type string LAST.\n";
    guint loghandler = 0;
    TestErrorStruct check1 = { loglevel, logdomain, msg1, 0 };
    TestErrorStruct check2 = { loglevel, logdomain, msg2, 0 };
    TestErrorStruct check3 = { loglevel, logdomain, msg3, 0 };
    Account *acc = g_object_new (GNC_TYPE_ACCOUNT, NULL);

    for (type = ACCT_TYPE_NONE; type < ACCT_TYPE_LAST; type = type + 1)
    {
        const gchar *type_name = xaccAccountTypeEnumAsString (type);
        const gchar *typestr;
        gchar *typestr_uc;

        g_assert (type_name);
        g_assert_cmpint (xaccAccountStringToEnum (type_name), == , type);
        if (type < 0 || type >= NUM_ACCOUNT_TYPES)
            continue;

        typestr = xaccAccountGetTypeStr (type);
        typestr_uc = g_ascii_strup (typestr, strlen (typestr));
        if (type == ACCT_TYPE_PAYABLE || type == ACCT_TYPE_RECEIVABLE)
        {
            gchar *cmpstr = g_strconcat ("A/", type_name, NULL);
            g_assert_cmpstr (typestr_uc, == , cmpstr);
            g_free (cmpstr);
        }
        else if (type == ACCT_TYPE_CREDIT)
        {
            gchar *cmpstr = g_strconcat (type_name, " CARD", NULL);
            g_assert_cmpstr (typestr_uc, == , cmpstr);
            g_free (cmpstr);
        }
        else if (type == ACCT_TYPE_MUTUAL)
        {
            gchar *cmpstr = g_strconcat (type_name, " FUND", NULL);
            g_assert_cmpstr (typestr_uc, == , cmpstr);
            g_free (cmpstr);
        }
        else
            g_assert_cmpstr (typestr_uc, == , type_name);
        g_free (typestr_uc);

	qof_instance_increase_editlevel (acc);
        g_object_set (acc, "type", type, NULL);
	qof_instance_decrease_editlevel (acc);
        if (type == ACCT_TYPE_STOCK || type == ACCT_TYPE_MUTUAL ||
                type == ACCT_TYPE_CURRENCY)
            g_assert (xaccAccountIsPriced (acc));
        else
            g_assert (!xaccAccountIsPriced (acc));

    }
    g_object_unref (acc);

    loghandler = g_log_set_handler (logdomain, loglevel,
                                    (GLogFunc)test_null_handler, &check1);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check1);
    g_assert (!xaccAccountTypeEnumAsString (ACCT_TYPE_LAST));
    g_assert_cmpint (check1.hits, ==, 1);

    g_log_remove_handler (logdomain, loghandler);
    g_free (msg1);
    loghandler = g_log_set_handler (logdomain, loglevel,
                                    (GLogFunc)test_null_handler, &check2);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check2);
    g_assert (!xaccAccountStringToType (NULL, &type));
    g_assert_cmpint (check2.hits, ==, 1);

    g_log_remove_handler (logdomain, loghandler);
    loghandler = g_log_set_handler (logdomain, loglevel,
                                    (GLogFunc)test_null_handler, &check3);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check3);
    g_assert (!xaccAccountStringToType ("LAST", &type));
    g_assert_cmpint (check3.hits, ==, 1);

    g_log_remove_handler (logdomain, loghandler);


}
/* xaccParentAccountTypesCompatibleWith
 * xaccAccountTypesCompatible
guint32
xaccParentAccountTypesCompatibleWith (GNCAccountType type)// C: 5 in 3 */
static void
test_xaccAccountType_Compatibility (void)
{
    guint32 bank_compat = ((1 << ACCT_TYPE_BANK)       |
                           (1 << ACCT_TYPE_CASH)       |
                           (1 << ACCT_TYPE_ASSET)      |
                           (1 << ACCT_TYPE_STOCK)      |
                           (1 << ACCT_TYPE_MUTUAL)     |
                           (1 << ACCT_TYPE_CURRENCY)   |
                           (1 << ACCT_TYPE_CREDIT)     |
                           (1 << ACCT_TYPE_LIABILITY)  |
                           (1 << ACCT_TYPE_RECEIVABLE) |
                           (1 << ACCT_TYPE_PAYABLE)    |
                           (1 << ACCT_TYPE_ROOT));
    guint32 expense_compat = ((1 << ACCT_TYPE_INCOME)     |
                              (1 << ACCT_TYPE_EXPENSE)    |
                              (1 << ACCT_TYPE_ROOT));
    guint32 equity_compat = ((1 << ACCT_TYPE_EQUITY) | (1 << ACCT_TYPE_ROOT));
    guint32 trading_compat = ((1 << ACCT_TYPE_TRADING) | (1 << ACCT_TYPE_ROOT));
    guint32 compat;
    GNCAccountType  type;
    gchar *msg1 = g_strdup_printf ("[xaccParentAccountTypesCompatibleWith()] bad account type: %d", ACCT_TYPE_ROOT);
    gchar *msg2 = g_strdup_printf ("[xaccParentAccountTypesCompatibleWith()] bad account type: %d", ACCT_TYPE_SAVINGS);
    gchar *logdomain = "gnc.account";
    guint loglevel = G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    TestErrorStruct check1 = { loglevel, logdomain, msg1, 0 };
    TestErrorStruct check2 = { loglevel, logdomain, msg2, 0 };
    gint loghandler;

    for (type = ACCT_TYPE_BANK; type < NUM_ACCOUNT_TYPES; type = type + 1)
    {
        GNCAccountType child;
        if (type == ACCT_TYPE_ROOT)
        {
            loghandler = g_log_set_handler (logdomain, loglevel,
                                            (GLogFunc)test_null_handler, &check1);
            g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler,
                                          &check1);
            compat = xaccParentAccountTypesCompatibleWith (type);
            g_log_remove_handler (logdomain, loghandler);
            g_assert_cmpint (compat, == , 0);
            g_assert_cmpint (check1.hits, ==, 1);
            g_free (msg1);
            continue;
        }
        compat = xaccParentAccountTypesCompatibleWith (type);

        if (type <= ACCT_TYPE_CURRENCY || type == ACCT_TYPE_PAYABLE
                || type == ACCT_TYPE_RECEIVABLE)
            g_assert_cmpint (compat, == , bank_compat);
        else if (type == ACCT_TYPE_INCOME || type == ACCT_TYPE_EXPENSE)
            g_assert_cmpint (compat, == , expense_compat);
        else if (type == ACCT_TYPE_EQUITY)
            g_assert_cmpint (compat, == , equity_compat);
        else if (type == ACCT_TYPE_TRADING)
            g_assert_cmpint (compat, == , trading_compat);
        for (child = ACCT_TYPE_NONE; child < ACCT_TYPE_LAST; child = child + 1)
            if (1 << child & compat)
                g_assert (xaccAccountTypesCompatible (type, child));
            else
                g_assert (!xaccAccountTypesCompatible (type, child));
    }

    loghandler = g_log_set_handler (logdomain, loglevel,
                                    (GLogFunc)test_null_handler, &check2);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check2);
    compat = xaccParentAccountTypesCompatibleWith (type = type + 1);
    g_log_remove_handler (logdomain, loghandler);
    g_assert_cmpint (compat, == , 0);
    g_assert_cmpint (check2.hits, ==, 1);
    g_free (msg2);
}
/* More KVP getters & setters
 * xaccAccountGetReconcileLastDate
 * xaccAccountSetReconcileLastDate
 * xaccAccountGetReconcilePostponeDate
 * xaccAccountSetReconcilePostponeDate
 * xaccAccountGetReconcilePostponeBalance
 * xaccAccountSetReconcilePostponeBalance
 * xaccAccountClearReconcilePostpone
 * xaccAccountGetAutoInterestXfer
 * xaccAccountSetAutoInterestXfer
 * xaccAccountGetLastNum
 * xaccAccountSetLastNum
 * xaccAccountSetReconcileChildrenStatus
 * xaccAccountGetReconcileChildrenStatus
 * xaccAccountGetReconcileLastInterval
 * xaccAccountSetReconcileLastInterval
 * dxaccAccountSetPriceSrc
 * dxaccAccountSetQuoteTZ
 * dxaccAccountGetQuoteTZ
 */
/* finder_help_function
static void
finder_help_function (const Account *acc, const char *description,// 3
Helper function, fully exercised by the following two public functions
*/
/* xaccAccountFindSplitByDesc
Split *
xaccAccountFindSplitByDesc (const Account *acc, const char *description)// C: 5 in 3 */

static void
test_xaccAccountFindSplitByDesc (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *baz = gnc_account_lookup_by_name (root, "baz");
    gchar *memo;
    Split *split = xaccAccountFindSplitByDesc (baz, "pepper");
    g_assert (split);
    g_object_get (split, "memo", &memo, NULL);
    g_assert_cmpstr (memo, == , "pepper_baz");
    g_free (memo);
}
/* xaccAccountFindTransByDesc
Transaction *
xaccAccountFindTransByDesc (const Account *acc, const char *description)// C: 5 in 3 */
static void
test_xaccAccountFindTransByDesc (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *baz = gnc_account_lookup_by_name (root, "baz");
    gchar *desc;
    Transaction *txn = xaccAccountFindTransByDesc (baz, "pepper");
    g_assert (txn);
    g_object_get (txn, "description", &desc, NULL);
    g_assert_cmpstr (desc, == , "pepper");
    g_free (desc);
}
/* gnc_account_join_children
void
gnc_account_join_children (Account *to_parent, Account *from_parent)// C: 4 in 2 SCM: 3 in 3*/
static void
test_gnc_account_join_children (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *broker = gnc_account_lookup_by_name (root, "broker");
    Account *income = gnc_account_lookup_by_name (root, "income");
    gint broker_desc = gnc_account_n_descendants (broker);
    gint income_desc = gnc_account_n_descendants (income);

    g_test_message ("The following should fail because of account type incompatibility. It doesn't, which is a bug.");
    gnc_account_join_children (income, broker);
    g_assert_cmpint (gnc_account_n_descendants (income), == ,
                     broker_desc + income_desc);


}
/* gnc_account_merge_children
void
gnc_account_merge_children (Account *parent)// C: 4 in 2 SCM: 2 in 2*/
static void
test_gnc_account_merge_children (Fixture *fixture, gconstpointer pData)
{
    Account *root = gnc_account_get_root (fixture->acct);
    Account *taxable = gnc_account_lookup_by_name (root, "taxable");
    Account *expense = gnc_account_lookup_by_name (root, "expense");
    Account *div = gnc_account_lookup_by_name (root, "div");
    Account *div1 = gnc_account_lookup_by_name (root, "div1");
    gint taxable_desc = gnc_account_n_descendants (taxable);
    gint expense_desc = gnc_account_n_descendants (expense);
    TestSignal sig4, sig5;
    /* This segment doesn't test because of problems with resetting
     * the accounts on the splits. It will have to be rewritten with a
     * mock Split object
    Account *stocks = gnc_account_lookup_by_name (root, "stocks");
    Account *baz = gnc_account_lookup_by_name (root, "baz");
    Account *baz2 = gnc_account_lookup_by_name (root, "baz2");
    gint stocks_desc = gnc_account_n_descendants (stocks);
    gfloat stocks_balance = gnc_numeric_to_double (
                                xaccAccountGetBalance (stocks));
    gfloat baz_balance = gnc_numeric_to_double (xaccAccountGetBalance (baz));
    gfloat baz2_balance = gnc_numeric_to_double (xaccAccountGetBalance (baz2));
    TestSignal sig1, sig2, sig3;
    gchar *logdomain = "gnc.engine";
    gint loglevel =  G_LOG_LEVEL_CRITICAL | G_LOG_FLAG_FATAL;
    gchar *msg = "[xaccSplitCommitEdit ()] Account grabbed split prematurely.";
    TestErrorStruct check = { loglevel, logdomain, msg, 0 };
    guint hdlr = g_log_set_handler (logdomain, loglevel,
    			   (GLogFunc)test_null_handler, &check);
    g_test_log_set_fatal_handler ((GTestLogFatalFunc)test_checked_handler, &check);

    sig1 = test_signal_new (QOF_INSTANCE (baz), QOF_EVENT_MODIFY, NULL);
    sig2 = test_signal_new (QOF_INSTANCE (baz2), QOF_EVENT_MODIFY, NULL);
    sig3 = test_signal_new (QOF_INSTANCE (baz2), QOF_EVENT_DESTROY, NULL);

    gnc_account_foreach_descendant (stocks, (AccountCb)print_account, NULL);
    g_object_set (baz2, "name", "baz", NULL);
    gnc_account_merge_children (stocks);
    gnc_account_foreach_descendant (stocks, (AccountCb)print_account, NULL);
    g_assert_cmpint (gnc_account_n_descendants (stocks), ==, stocks_desc - 1);
    g_assert_cmpfloat (gnc_numeric_to_double (xaccAccountGetBalance (stocks)),
    	       ==, stocks_balance);
    g_assert_cmpfloat (gnc_numeric_to_double (xaccAccountGetBalance (baz)),
    	       ==, baz_balance + baz2_balance);
    test_signal_assert_hits (sig1, 0);
    test_signal_assert_hits (sig2, 0);
    test_signal_assert_hits (sig3, 1);
    test_signal_free (sig1);
    test_signal_free (sig2);
    test_signal_free (sig3);
    g_log_remove_handler (logdomain, hdlr);
    g_free (msg);
    */
    sig4 = test_signal_new (QOF_INSTANCE (div), QOF_EVENT_MODIFY, NULL);
    sig5 = test_signal_new (QOF_INSTANCE (div1), QOF_EVENT_MODIFY, NULL);
    qof_instance_increase_editlevel (div1);
    g_object_set (div1, "name", "div", NULL);
    qof_instance_decrease_editlevel (div1);
    gnc_account_merge_children (taxable);
    g_assert_cmpint (gnc_account_n_descendants (taxable), == , taxable_desc - 1);
    test_signal_assert_hits (sig4, 1);
    test_signal_assert_hits (sig5, 3);
    test_signal_free (sig4);
    test_signal_free (sig5);
    gnc_account_merge_children (expense);
    g_assert_cmpint (gnc_account_n_descendants (expense), == , expense_desc);
}
/* xaccSplitsBeginStagedTransactionTraversals
 * xaccAccountBeginStagedTransactionTraversals
 * gnc_account_tree_begin_staged_transaction_traversals
 * xaccAccountStagedTransactionTraversal
 * gnc_account_tree_staged_transaction_traversal
 * Static helper functions for xaccAccount (Tree)?ForEach ()
 */
/* xaccTransactionTraverse
gboolean
xaccTransactionTraverse (Transaction *trans, int stage)// Used only by xaccSplitTransactionTraverses (not used) delete.
*/
/* xaccSplitTransactionTraverse
gboolean
xaccSplitTransactionTraverse (Split *split, int stage)// Not used, delete.
*/
/* do_one_split
 * do_one_account
 Trivial helper functions
*/
typedef struct
{
    guint count;
    gchar *name;
} Thunkdata;

static gint
thunk3 (Transaction *txn, gpointer data)
{
    Thunkdata *td = (Thunkdata*)data;
    ++(td->count);
    if (td->name)
    {
        gchar *txn_desc;
        gboolean result;
        g_object_get (txn, "description", &txn_desc, NULL);
        result = g_strcmp0 (td->name, txn_desc) == 0;
        g_free (txn_desc);
        if (result)
            return td->count;
    }
    return 0;
}

/* xaccAccountTreeForEachTransaction
int
xaccAccountTreeForEachTransaction (Account *acc, TransactionCallback proc,
                                   void *data);// C: 302 in 89 SCM: 1158 in 142*/
static void
test_xaccAccountTreeForEachTransaction (Fixture *fixture, gconstpointer pData )
{
    Thunkdata td = {0, NULL};
    Account *root = gnc_account_get_root (fixture->acct);
    gint result;
    result = xaccAccountTreeForEachTransaction (root, thunk3, &td);
    g_assert_cmpint (td.count, == , 9);
    g_assert_cmpint (result, == , 0);
    td.count = 0;
    td.name = "pepper";
    result = xaccAccountTreeForEachTransaction (root, thunk3, &td);
    g_assert_cmpint (td.count, == , result);
    g_assert_cmpint (result, < , 9);
}
/* xaccAccountForEachTransaction
gint
xaccAccountForEachTransaction (const Account *acc, TransactionCallback proc,// C: 8 in 4 */
static void
test_xaccAccountForEachTransaction (Fixture *fixture, gconstpointer pData )
{
    Thunkdata td = {0, NULL};
    Account *root = gnc_account_get_root (fixture->acct);
    Account *money = gnc_account_lookup_by_name (root, "money");
    gint result;
    result = xaccAccountForEachTransaction (root, thunk3, &td);
    g_assert_cmpint (td.count, == , 0);
    g_assert (money);
    result = xaccAccountForEachTransaction (money, thunk3, &td);
    g_assert_cmpint (td.count, == , 9);
    g_assert_cmpint (result, == , 0);
    td.count = 0;
    td.name = "pepper";
    result = xaccAccountForEachTransaction (money, thunk3, &td);
    g_assert_cmpint (td.count, == , result);
    g_assert_cmpint (result, < , 9);
}


void
test_suite_account (void)
{

    GNC_TEST_ADD_FUNC (suitename, "gnc set account separator", test_gnc_set_account_separator);
    GNC_TEST_ADD_FUNC (suitename, "gnc account name violations errmsg", test_gnc_account_name_violations_errmsg);
    GNC_TEST_ADD (suitename, "gnc account list name violations", Fixture, &bad_data, setup, test_gnc_account_list_name_violations,  teardown);
    GNC_TEST_ADD_FUNC (suitename, "account create and destroy", test_gnc_account_create_and_destroy);
    GNC_TEST_ADD (suitename, "book set/get root account", Fixture, NULL, setup, test_gnc_book_set_get_root_account, teardown);
    GNC_TEST_ADD_FUNC (suitename, "xaccMallocAccount", test_xaccMallocAccount);

    GNC_TEST_ADD_FUNC (suitename, "gnc account create root", test_gnc_account_create_root);
    GNC_TEST_ADD (suitename, "xaccCloneAccount", Fixture, NULL, setup, test_xaccCloneAccount,  teardown );
    /*Destroys the account, so we have to do the tear down in the test function */
    GNC_TEST_ADD (suitename, "xaccFreeAccountChildren", Fixture,  &good_data, setup, test_xaccFreeAccountChildren,  NULL);
    /* See comment at the beginning of test_xaccFreeAccount */
    GNC_TEST_ADD (suitename, "xaccFreeAccount", Fixture, &good_data, setup, test_xaccFreeAccount,  NULL );
    GNC_TEST_ADD (suitename, "xaccAccountCommitEdit", Fixture, &good_data, setup, test_xaccAccountCommitEdit,  NULL );
// GNC_TEST_ADD (suitename, "xaccAcctChildrenEqual", Fixture, NULL, setup, test_xaccAcctChildrenEqual,  teardown );
// GNC_TEST_ADD (suitename, "xaccAccountEqual", Fixture, NULL, setup, test_xaccAccountEqual,  teardown );
    GNC_TEST_ADD (suitename, "gnc account insert & remove split", Fixture, NULL, setup, test_gnc_account_insert_remove_split,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccount Insert and Remove Lot", Fixture, &good_data, setup, test_xaccAccountInsertRemoveLot,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountRecomputeBalance", Fixture, &some_data, setup, test_xaccAccountRecomputeBalance,  teardown );
    GNC_TEST_ADD_FUNC (suitename, "xaccAccountOrder", test_xaccAccountOrder );
    GNC_TEST_ADD (suitename, "qofAccountSetParent", Fixture, &some_data, setup, test_qofAccountSetParent,  teardown );
    GNC_TEST_ADD (suitename, "gnc account append/remove child", Fixture, NULL, setup, test_gnc_account_append_remove_child,  teardown );
    GNC_TEST_ADD (suitename, "gnc account n descendants", Fixture, &some_data, setup, test_gnc_account_n_descendants,  teardown );
    GNC_TEST_ADD (suitename, "gnc account get current depth", Fixture, &some_data, setup, test_gnc_account_get_current_depth,  teardown );
    GNC_TEST_ADD (suitename, "gnc account get tree depth", Fixture, &complex, setup, test_gnc_account_get_tree_depth,  teardown );
    GNC_TEST_ADD (suitename, "gnc account get descendants", Fixture, &complex, setup, test_gnc_account_get_descendants,  teardown );
    GNC_TEST_ADD (suitename, "gnc account get descendants sorted", Fixture, &complex, setup, test_gnc_account_get_descendants_sorted,  teardown );
    GNC_TEST_ADD (suitename, "gnc account lookup by name", Fixture, &complex, setup, test_gnc_account_lookup_by_name,  teardown );
    GNC_TEST_ADD (suitename, "gnc account lookup by code", Fixture, &complex, setup, test_gnc_account_lookup_by_code,  teardown );
    GNC_TEST_ADD (suitename, "gnc account lookup by full name helper", Fixture, &complex, setup, test_gnc_account_lookup_by_full_name_helper,  teardown );
    GNC_TEST_ADD (suitename, "gnc account lookup by full name", Fixture, &complex, setup, test_gnc_account_lookup_by_full_name,  teardown );
    GNC_TEST_ADD (suitename, "gnc account foreach child", Fixture, &complex, setup, test_gnc_account_foreach_child,  teardown );
    GNC_TEST_ADD (suitename, "gnc account foreach descendant", Fixture, &complex, setup, test_gnc_account_foreach_descendant,  teardown );
    GNC_TEST_ADD (suitename, "gnc account foreach descendant until", Fixture, &complex, setup, test_gnc_account_foreach_descendant_until,  teardown );
    GNC_TEST_ADD (suitename, "gnc account get full name", Fixture, &good_data, setup, test_gnc_account_get_full_name,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountGetProjectedMinimumBalance", Fixture, &some_data, setup, test_xaccAccountGetProjectedMinimumBalance,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountGetBalanceAsOfDate", Fixture, &some_data, setup, test_xaccAccountGetBalanceAsOfDate,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountGetPresentBalance", Fixture, &some_data, setup, test_xaccAccountGetPresentBalance,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountFindOpenLots", Fixture, &complex_data, setup, test_xaccAccountFindOpenLots,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountForEachLot", Fixture, &complex_data, setup, test_xaccAccountForEachLot,  teardown );

    GNC_TEST_ADD (suitename, "xaccAccountHasAncestor", Fixture, &complex, setup, test_xaccAccountHasAncestor,  teardown );
    GNC_TEST_ADD_FUNC (suitename, "AccountType Stuff", test_xaccAccountType_Stuff );
    GNC_TEST_ADD_FUNC (suitename, "AccountType Compatibility", test_xaccAccountType_Compatibility);
    GNC_TEST_ADD (suitename, "xaccAccountFindSplitByDesc", Fixture, &complex_data, setup, test_xaccAccountFindSplitByDesc,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountFindTransByDesc", Fixture, &complex_data, setup, test_xaccAccountFindTransByDesc,  teardown );
    GNC_TEST_ADD (suitename, "gnc account join children", Fixture, &complex, setup, test_gnc_account_join_children,  teardown );
    GNC_TEST_ADD (suitename, "gnc account merge children", Fixture, &complex_data, setup, test_gnc_account_merge_children,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountForEachTransaction", Fixture, &complex_data, setup, test_xaccAccountForEachTransaction,  teardown );
    GNC_TEST_ADD (suitename, "xaccAccountTreeForEachTransaction", Fixture, &complex_data, setup, test_xaccAccountTreeForEachTransaction,  teardown );


}
