#include <glib.h>
#include <stdio.h>
#include <string.h>

#include "gnc-engine.h"
#include "gnc-engine-util.h"

#include "sixtp-dom-parsers.h"
#include "io-gncxml-v2.h"
#include "sixtp.h"
#include "sixtp-parsers.h"
#include "gnc-xml.h"
#include "gnc-book-p.h"
#include "gnc-pricedb.h"

#include "Group.h"

static void
run_callback(sixtp_gdv2 *data, const char *type)
{
    if(data->countCallback)
    {
        data->countCallback(type, data->counter);
    }
}

static void
clear_up_account_commodity(
    GNCBook *book, Account *act, gnc_commodity * (*getter) (Account *account),
    void (*setter) (Account *account, gnc_commodity *comm))
{
    gnc_commodity *gcom;
    gnc_commodity *com = getter(act);

    if(!com)
    {
        return;
    }
    
    gcom = gnc_commodity_table_lookup(gnc_book_get_commodity_table(book),
                                      gnc_commodity_get_namespace(com),
                                      gnc_commodity_get_mnemonic(com));
    if(!gcom)
    {
        g_warning("unable to find global commodity for %s:%s adding new",
                  gnc_commodity_get_namespace(com),
                  gnc_commodity_get_mnemonic(com));
        gnc_commodity_table_insert(gnc_book_get_commodity_table(book), com);
    }
    else
    {
        gnc_commodity_destroy(com);
        setter(act, gcom);
    }
}

static gboolean
add_account_local(sixtp_gdv2 *data, Account *act)
{
    clear_up_account_commodity(data->book, act,
                               xaccAccountGetCurrency,
                               xaccAccountSetCurrency);
    clear_up_account_commodity(data->book, act,
                               xaccAccountGetSecurity,
                               xaccAccountSetSecurity);
    if(!xaccAccountGetParent(act))
    {
        xaccGroupInsertAccount(gnc_book_get_group(data->book), act);
    }
    data->counter.accounts_loaded++;
    run_callback(data, "account");
    
    return FALSE;
}

static gboolean
add_commodity_local(sixtp_gdv2 *data, gnc_commodity *com)
{
    gnc_commodity_table_insert(gnc_book_get_commodity_table(data->book), com);
    data->counter.commodities_loaded++;
    run_callback(data, "commodities");
    return TRUE;
}

static gboolean
add_transaction_local(sixtp_gdv2 *data, Transaction *trn)
{
    int i;
    Split *spl;
    
    for(i = 0, spl = xaccTransGetSplit(trn, i);
        spl;
        i++, spl = xaccTransGetSplit(trn, i))
    {
        xaccAccountInsertSplit(xaccSplitGetAccount(spl), spl);
    }

    data->counter.transactions_loaded++;
    run_callback(data, "transaction");
    
    return TRUE;
}

static gboolean
add_pricedb_local(sixtp_gdv2 *data, GNCPriceDB *db)
{
    if(gnc_book_get_pricedb(data->book))
    {
        gnc_pricedb_destroy(gnc_book_get_pricedb(data->book));
    }

    /* gnc_pricedb_print_contents(db, stdout); */
    gnc_book_set_pricedb(data->book, db);

    return TRUE;
}

static gboolean
gnc_counter_end_handler(gpointer data_for_children,
                        GSList* data_from_children, GSList* sibling_data,
                        gpointer parent_data, gpointer global_data,
                        gpointer *result, const gchar *tag)
{
    char *strval;
    gint64 val;
    char *type;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    sixtp_gdv2 *gdata = (sixtp_gdv2*)global_data;
    
    if(parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if(!tag)
    {
        return TRUE;
    }
    
    g_return_val_if_fail(tree, FALSE);

    type = xmlGetProp(tree, "cd:type");
    strval = dom_tree_to_text(tree);
    if(!string_to_integer(strval, &val))
    {
        g_warning("string_to_integer failed with input: %s", strval);
        return FALSE;
    }
    
    if(safe_strcmp(type, "transaction") == 0)
    {
        gdata->counter.transactions_total = val;
    }
    else if(safe_strcmp(type, "account") == 0)
    {
        gdata->counter.accounts_total = val;
    }
    else if(safe_strcmp(type, "commodity") == 0)
    {
        gdata->counter.commodities_total = val;
    }
    else
    {
        g_warning("Unknown type: %s", type);
        return FALSE;
    }
    
    return TRUE;
}

static sixtp*
gnc_counter_sixtp_parser_create(void)
{
     return sixtp_dom_parser_new(gnc_counter_end_handler, NULL, NULL);
}

static void
print_counter_data(load_counter data)
{
    printf("Transactions: Total: %d, Loaded: %d\n",
           data.transactions_total, data.transactions_loaded);
    printf("Accounts: Total: %d, Loaded: %d\n",
           data.accounts_total, data.accounts_loaded);
    printf("Commodities: Total: %d, Loaded: %d\n",
           data.commodities_total, data.commodities_loaded);
}

gboolean
gnc_book_load_from_xml_file_v2(
    GNCBook *book,
    void (*countcallback)(const char *type, load_counter count))
{
    sixtp_gdv2 gd;
    sixtp *top_parser;
    sixtp *main_parser;
    gpointer parse_result = NULL;

    gd.book = book;
    gd.counter.accounts_loaded = 0;
    gd.counter.accounts_total = 0;
    gd.counter.commodities_loaded = 0;
    gd.counter.commodities_total = 0;
    gd.counter.transactions_loaded = 0;
    gd.counter.transactions_total = 0;
    gd.counter.prices_loaded = 0;
    gd.counter.prices_total = 0;

    {
        AccountGroup *g = gnc_book_get_group(book);
        if(g) xaccFreeAccountGroup(g);
        gnc_book_set_group(book, xaccMallocAccountGroup());
    }
    
    gd.addAccountFunc = add_account_local;
    gd.addCommodityFunc = add_commodity_local;
    gd.addTransactionFunc = add_transaction_local;
    gd.addPriceDBFunc = add_pricedb_local;
    gd.countCallback = countcallback;
    
    top_parser = sixtp_new();
    main_parser = sixtp_new();
    
    if(!sixtp_add_some_sub_parsers(
        top_parser, TRUE,
        "gnc-v2", main_parser,
        NULL, NULL))
    {
        return FALSE;
    }
    
    if(!sixtp_add_some_sub_parsers(
           main_parser, TRUE,
           "gnc:count-data", gnc_counter_sixtp_parser_create(),
           "gnc:pricedb", gnc_pricedb_sixtp_parser_create(),
           "gnc:commodity", gnc_commodity_sixtp_parser_create(),
           "gnc:account", gnc_account_sixtp_parser_create(),
           "gnc:transaction", gnc_transaction_sixtp_parser_create(),
           NULL, NULL))
    {
        return FALSE;
    }
    
    if(!sixtp_parse_file(top_parser, gnc_book_get_file_path(book),
                         NULL, &gd, &parse_result))
    {
        sixtp_destroy(top_parser);
        return FALSE;
    }
    else
    {
    }
    
    /* DEBUG */
    print_counter_data(gd.counter);

    return TRUE;
}

/***********************************************************************/

static const gchar *emacs_trailer =
"<!-- Local variables: -->\n"
"<!-- mode: xml        -->\n"
"<!-- End:             -->\n";

static void
write_counts(FILE* out, ...)
{
    va_list ap;
    char *type;

    va_start(ap, out);

    type = va_arg(ap, char *);

    while(type)
    {
        xmlNodePtr node;
        char *val;
        int amount = va_arg(ap, int);

        if(amount != 0)
        {
            val = g_strdup_printf("%d", amount);

            node = xmlNewNode(NULL, "gnc:count-data");
            xmlSetProp(node, "cd:type", type);
            xmlNodeAddContent(node, val);

            xmlElemDump(out, NULL, node);
            fprintf(out, "\n");
        
            g_free(val);
            xmlFreeNode(node);
        }
        
        type = va_arg(ap, char *);
    }

    va_end(ap);
}

static gint
compare_namespaces(gconstpointer a, gconstpointer b) {
  const gchar *sa = (const gchar *) a;
  const gchar *sb = (const gchar *) b;
  return(safe_strcmp(sa, sb));
}

static gint
compare_commodity_ids(gconstpointer a, gconstpointer b) {
  const gnc_commodity *ca = (const gnc_commodity *) a;
  const gnc_commodity *cb = (const gnc_commodity *) b;
  return(safe_strcmp(gnc_commodity_get_mnemonic(ca),
                     gnc_commodity_get_mnemonic(cb)));
}

static void
write_commodities(FILE *out, GNCBook *book)
{
    gnc_commodity_table *tbl;
    GList *namespaces;
    GList *lp;

    tbl = gnc_book_get_commodity_table(book);

    namespaces = g_list_sort(gnc_commodity_table_get_namespaces(tbl),
                             compare_namespaces);

    for(lp = namespaces; lp; lp = lp->next) {
        gchar *space;

        if(!lp->data) {
            g_list_free (namespaces);
            return;
        }

        space = (gchar *) lp->data;
        if(strcmp(GNC_COMMODITY_NS_ISO, space) != 0) {
            GList *comms = gnc_commodity_table_get_commodities(tbl, space);
            GList *lp2;

            comms = g_list_sort(comms, compare_commodity_ids);

            for(lp2 = comms; lp2; lp2 = lp2->next) {
                xmlNodePtr comnode = gnc_commodity_dom_tree_create(
                    (gnc_commodity *) lp2->data);

                xmlElemDump(out, NULL, comnode);
                fprintf(out, "\n");

                xmlFreeNode(comnode);
            }

            g_list_free (comms);
        }
    }

    g_list_free (namespaces);

}

static void
write_pricedb(FILE *out, GNCBook *book)
{
    xmlNodePtr node;

    node = gnc_pricedb_dom_tree_create(gnc_book_get_pricedb(book));

    if(!node)
    {
        return;
    }
    
    xmlElemDump(out, NULL, node);
    fprintf(out, "\n");

    xmlFreeNode(node);
}

static void
write_account_group(FILE *out, AccountGroup *grp)
{
    GList *list;
    GList *node;

    list = xaccGroupGetAccountList(grp);

    for (node = list; node; node = node->next) {
        xmlNodePtr accnode;
        AccountGroup *newgrp;
        
        accnode = gnc_account_dom_tree_create((Account*)(node->data));

        xmlElemDump(out, NULL, accnode);
        fprintf(out, "\n");

        xmlFreeNode(accnode);

        newgrp = xaccAccountGetChildren((Account*)(node->data));

        if(grp)
        {
            write_account_group(out, newgrp);
        }
    }
}

static void
write_accounts(FILE *out, GNCBook *book)
{
    write_account_group(out, gnc_book_get_group(book));
}

static gboolean
xml_add_trn_data(Transaction *t, gpointer data) {
    xmlNodePtr node;

    node = gnc_transaction_dom_tree_create(t);

    xmlElemDump((FILE*)data, NULL, node);
    fprintf((FILE*)data, "\n");

    xmlFreeNode(node);
    return TRUE;
}

static void
write_transactions(FILE *out, GNCBook *book)
{
    xaccGroupForEachTransaction(gnc_book_get_group(book),
                                xml_add_trn_data,
                                (gpointer) out);
}

gboolean
gnc_book_write_to_xml_file_v2(GNCBook *book, const char *filename)
{
    FILE *out;

    out = fopen(filename, "w");

    fprintf(out, "<?xml version=\"1.0\"?>\n");
    fprintf(out, "<gnc-v2>\n");

    write_counts(out,
                 "commodity",
                 gnc_commodity_table_get_size(
                     gnc_book_get_commodity_table(book)),
                 "account",
                 xaccGroupGetNumSubAccounts(gnc_book_get_group(book)),
                 "transaction",
                 gnc_book_count_transactions(book),
                 NULL);

    write_commodities(out, book);

    write_pricedb(out, book);

    write_accounts(out, book);

    write_transactions(out, book);

    fprintf(out, "</gnc-v2>\n\n");
    fprintf(out, emacs_trailer);

    fclose(out);
    
    return FALSE;
}

/***********************************************************************/
gboolean
gnc_is_xml_data_file_v2(const gchar *name)
{
    return gnc_is_our_xml_file(name, "gnc-v2");
}

