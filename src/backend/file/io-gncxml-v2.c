/********************************************************************\
 * Copyright (C) 2000,2001 Gnumatic Inc.                            *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
\********************************************************************/

#include "config.h"

#include <glib.h>
#include <stdio.h>
#include <string.h>

#include "gnc-book-p.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"
#include "gnc-pricedb-p.h"
#include "gnc-session.h"
#include "Group.h"
#include "Scrub.h"
#include "Transaction.h"
#include "TransLog.h"

#include "sixtp-dom-parsers.h"
#include "io-gncxml-v2.h"
#include "io-gncxml-gen.h"

#include "sixtp.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "gnc-xml.h"
#include "io-utils.h"


#define GNC_V2_STRING "gnc-v2"

static void
run_callback(sixtp_gdv2 *data, const char *type)
{
    if(data->countCallback)
    {
        data->countCallback(type, data->counter);
    }
}

static void
clear_up_account_commodity_session(
    GNCBook *book, Account *act,
    gnc_commodity * (*getter) (Account *account, GNCBook *book),
    void (*setter) (Account *account, gnc_commodity *comm, GNCBook *book),
    int (*scu_getter) (Account *account),
    void (*scu_setter) (Account *account, int scu))
{
    gnc_commodity_table *tbl;
    gnc_commodity *gcom;
    gnc_commodity *com;
    int old_scu;

    tbl = gnc_book_get_commodity_table (book);

    com = getter (act, book);

    if (scu_getter)
      old_scu = scu_getter(act);
    else
      old_scu = 0;

    if(!com)
    {
        return;
    }
    
    gcom = gnc_commodity_table_lookup(tbl, gnc_commodity_get_namespace(com),
                                      gnc_commodity_get_mnemonic(com));

    if (gcom == com)
    {
        return;
    }
    else if(!gcom)
    {
        g_warning("unable to find global commodity for %s adding new",
                  gnc_commodity_get_unique_name(com));
        gnc_commodity_table_insert(tbl, com);
    }
    else
    {
        gnc_commodity_destroy(com);
        setter(act, gcom, book);
        if (old_scu != 0 && scu_setter)
          scu_setter(act, old_scu);
    }
}

static void
clear_up_account_commodity(
    gnc_commodity_table *tbl, Account *act,
    gnc_commodity * (*getter) (Account *account),
    void (*setter) (Account *account, gnc_commodity *comm),
    int (*scu_getter) (Account *account),
    void (*scu_setter) (Account *account, int scu))
{
    gnc_commodity *gcom;
    gnc_commodity *com = getter(act);
    int old_scu;

    if (scu_getter)
      old_scu = scu_getter(act);
    else
      old_scu = 0;

    if(!com)
    {
        return;
    }
    
    gcom = gnc_commodity_table_lookup(tbl, gnc_commodity_get_namespace(com),
                                      gnc_commodity_get_mnemonic(com));

    if (gcom == com)
    {
        return;
    }
    else if(!gcom)
    {
        g_warning("unable to find global commodity for %s adding new",
                  gnc_commodity_get_unique_name(com));
        gnc_commodity_table_insert(tbl, com);
    }
    else
    {
        gnc_commodity_destroy(com);
        setter(act, gcom);
        if (old_scu != 0 && scu_setter)
          scu_setter(act, old_scu);
    }
}

static void
clear_up_transaction_commodity(
    gnc_commodity_table *tbl, Transaction *trans,
    gnc_commodity * (*getter) (Transaction *trans),
    void (*setter) (Transaction *trans, gnc_commodity *comm))
{
    gnc_commodity *gcom;
    gnc_commodity *com = getter(trans);

    if(!com)
    {
        return;
    }
    
    gcom = gnc_commodity_table_lookup(tbl, gnc_commodity_get_namespace(com),
                                      gnc_commodity_get_mnemonic(com));

    if(gcom == com)
    {
        return;
    }
    else if(!gcom)
    {
        g_warning("unable to find global commodity for %s adding new",
                  gnc_commodity_get_unique_name(com));
        gnc_commodity_table_insert(tbl, com);
    }
    else
    {
        gnc_commodity_destroy(com);
        xaccTransBeginEdit(trans);
        setter(trans, gcom);
        xaccTransCommitEdit(trans);
    }
}

static gboolean
add_account_local(sixtp_gdv2 *data, Account *act)
{
    gnc_commodity_table *table;

    table = gnc_book_get_commodity_table (data->book);

    clear_up_account_commodity_session(data->book, act,
                                       DxaccAccountGetCurrency,
                                       DxaccAccountSetCurrency,
                                       DxaccAccountGetCurrencySCU,
                                       DxaccAccountSetCurrencySCU);

    clear_up_account_commodity_session(data->book, act,
                                       DxaccAccountGetSecurity,
                                       DxaccAccountSetSecurity,
                                       NULL, NULL);

    clear_up_account_commodity(table, act,
                               xaccAccountGetCommodity,
                               xaccAccountSetCommodity,
                               xaccAccountGetCommoditySCU,
                               xaccAccountSetCommoditySCU);

    xaccAccountScrubCommodity (act, data->book);

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
    gnc_commodity_table *table;

    table = gnc_book_get_commodity_table (data->book);

    gnc_commodity_table_insert(table, com);

    data->counter.commodities_loaded++;
    run_callback(data, "commodities");

    return TRUE;
}

static gboolean
add_transaction_local(sixtp_gdv2 *data, Transaction *trn)
{
    gnc_commodity_table *table;
    Split *spl;
    int i;
    
    table = gnc_book_get_commodity_table (data->book);

    clear_up_transaction_commodity(table, trn,
                                   xaccTransGetCurrency,
                                   xaccTransSetCurrency);

    xaccTransScrubCurrency (trn, data->book);

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
add_schedXaction_local(sixtp_gdv2 *data, SchedXaction *sx)
{
    GList *list;

    list = gnc_book_get_schedxactions (data->book);
    list = g_list_append(list, sx);

    gnc_book_set_schedxactions(data->book, list);

    return TRUE;
}

static gboolean
add_template_transaction_local( sixtp_gdv2 *data,
                                gnc_template_xaction_data *txd )
{
    GList *n;
    Account *tmpAcct;
    AccountGroup *acctGroup = NULL;
    GNCBook *book;

    book = data->book;

    /* expect a struct of: */
    /* . template accounts. */
    /* . transactions in those accounts. */
    for ( n = txd->accts; n; n = n->next ) {
        if ( xaccAccountGetParent( (Account*)n->data ) == NULL ) {
            /* remove the gnc_book_init-created account of the same name */
            acctGroup =
            gnc_book_get_template_group(book);
            tmpAcct =
            xaccGetAccountFromName( acctGroup,
                                    xaccAccountGetName( (Account*)n->data ) );
            if ( tmpAcct != NULL ) {
                xaccGroupRemoveAccount( acctGroup, tmpAcct );
            }

            xaccGroupInsertAccount( acctGroup, (Account*)n->data );
        }

    }

    for ( n = txd->transactions; n; n = n->next ) {
        /* insert transactions into accounts */
        add_transaction_local( data, (Transaction*)n->data );
    }

    xaccAccountGroupCommitEdit (acctGroup);

    return TRUE;
}

static gboolean
add_pricedb_local(sixtp_gdv2 *data, GNCPriceDB *db)
{
    GNCBook *book;

    book = data->book;

    if (gnc_book_get_pricedb(book))
    {
        gnc_pricedb_destroy(gnc_book_get_pricedb(book));
    }

    /* gnc_pricedb_print_contents(db, stdout); */
    gnc_book_set_pricedb(book, db);

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
    gxpf_data *gdata = (gxpf_data*)global_data;
    sixtp_gdv2 *sixdata = (sixtp_gdv2*)gdata->parsedata;
    
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
    if(!string_to_gint64(strval, &val))
    {
        g_warning("string_to_gint64 failed with input: %s",
                  strval ? strval : "(null)");
        g_free (strval);
        xmlFree (type);
        return FALSE;
    }
    g_free (strval);

    if(safe_strcmp(type, "transaction") == 0)
    {
        sixdata->counter.transactions_total = val;
    }
    else if(safe_strcmp(type, "account") == 0)
    {
        sixdata->counter.accounts_total = val;
    }
    else if(safe_strcmp(type, "commodity") == 0)
    {
        sixdata->counter.commodities_total = val;
    }
    else if(safe_strcmp(type, "schedxaction") == 0)
    {
        sixdata->counter.schedXactions_total = val;
    }
    else
    {
        g_warning("Unknown type: %s",
                  type ? type : "(null)");
        xmlFree (type);
        return FALSE;
    }

    xmlFree (type);
    xmlFreeNode(tree);

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
    printf("Scheduled Tansactions: Total: %d, Loaded: %d\n",
           data.schedXactions_total, data.schedXactions_loaded);
}

static const char *ACCOUNT_TAG = "gnc:account";
static const char *PRICEDB_TAG = "gnc:pricedb";
static const char *COMMODITY_TAG = "gnc:commodity";
static const char *COUNT_DATA_TAG = "gnc:count-data";
static const char *TRANSACTION_TAG = "gnc:transaction";
static const char *SCHEDXACTION_TAG = "gnc:schedxaction";
static const char *TEMPLATE_TRANSACTION_TAG = "gnc:template-transactions";

static gboolean
generic_callback(const char *tag, gpointer globaldata, gpointer data)
{
    sixtp_gdv2 *gd = (sixtp_gdv2*)globaldata;

    if(safe_strcmp(tag, ACCOUNT_TAG) == 0)
    {
        add_account_local(gd, (Account*)data);
    }
    else if(safe_strcmp(tag, PRICEDB_TAG) == 0)
    {
        add_pricedb_local(gd, (GNCPriceDB*)data);
    }
    else if(safe_strcmp(tag, COMMODITY_TAG) == 0)
    {
        add_commodity_local(gd, (gnc_commodity*)data);
    }
    else if(safe_strcmp(tag, TRANSACTION_TAG) == 0)
    {
        add_transaction_local(gd, (Transaction*)data);
    }
    else if(safe_strcmp(tag, SCHEDXACTION_TAG) == 0)
    {
        add_schedXaction_local(gd, (SchedXaction*)data);
    }
    else if(safe_strcmp(tag, TEMPLATE_TRANSACTION_TAG ) == 0 )
    {
        add_template_transaction_local( gd, (gnc_template_xaction_data*)data );
    }
    return TRUE;
}

gboolean
gnc_session_load_from_xml_file_v2(
    GNCSession *session,
    void (*countcallback)(const char *type, load_counter count))
{
    GNCBook *book;
    sixtp_gdv2 *gd;
    sixtp *top_parser;
    sixtp *main_parser;

    gd = g_new0(sixtp_gdv2, 1);

    book = gnc_session_get_book (session);

    gd->book = book;
    gd->counter.accounts_loaded = 0;
    gd->counter.accounts_total = 0;
    gd->counter.commodities_loaded = 0;
    gd->counter.commodities_total = 0;
    gd->counter.transactions_loaded = 0;
    gd->counter.transactions_total = 0;
    gd->counter.prices_loaded = 0;
    gd->counter.prices_total = 0;
    gd->counter.schedXactions_loaded = 0;
    gd->counter.schedXactions_total = 0;

    gd->countCallback = countcallback;

    top_parser = sixtp_new();
    main_parser = sixtp_new();

    if(!sixtp_add_some_sub_parsers(
        top_parser, TRUE,
        GNC_V2_STRING, main_parser,
        NULL, NULL))
    {
        return FALSE;
    }

    if(!sixtp_add_some_sub_parsers(
           main_parser, TRUE,
           COUNT_DATA_TAG, gnc_counter_sixtp_parser_create(),
           PRICEDB_TAG, gnc_pricedb_sixtp_parser_create(),
           COMMODITY_TAG, gnc_commodity_sixtp_parser_create(),
           ACCOUNT_TAG, gnc_account_sixtp_parser_create(),
           TRANSACTION_TAG, gnc_transaction_sixtp_parser_create(),
           SCHEDXACTION_TAG, gnc_schedXaction_sixtp_parser_create(),
           TEMPLATE_TRANSACTION_TAG, gnc_template_transaction_sixtp_parser_create(),
           NULL, NULL))
    {
        return FALSE;
    }

    /* stop logging while we load */
    xaccLogDisable ();

    if(!gnc_xml_parse_file(top_parser, gnc_session_get_file_path(session),
                           generic_callback, gd, book))
    {
        sixtp_destroy(top_parser);
        xaccLogEnable ();
        return FALSE;
    }

    /* If the parse succeeded, but there is no pricedb,
     * then the file had no pricedb section. However,
     * this routine is expected to put one in the book. */
    if (!gnc_book_get_pricedb (book))
      gnc_book_set_pricedb (book, gnc_pricedb_create (book));

    /* mark the newly read group as saved, since the act of putting 
     * it together will have caused it to be marked up as not-saved. 
     */
    xaccGroupMarkSaved (gnc_book_get_group(book));

    /* also mark the pricedb as saved for the same reasons */
    gnc_pricedb_mark_clean (gnc_book_get_pricedb (book));

    /* Fix account and transaction commodities */
    xaccGroupScrubCommodities (gnc_book_get_group(book), book);

    /* Fix split amount/value */
    xaccGroupScrubSplits (gnc_book_get_group(book));

    /* commit all groups, this completes the BeginEdit started when the
     * account_end_handler finished reading the account.
     */
    xaccAccountGroupCommitEdit (gnc_book_get_group(book));

    /* destroy the parser */
    sixtp_destroy (top_parser);

    /* start logging again */
    xaccLogEnable ();

    g_free(gd);
    /* DEBUG */
    /* print_counter_data(gd.counter); */

    return TRUE;
}

/***********************************************************************/

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

            node = xmlNewNode(NULL, COUNT_DATA_TAG);
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
compare_namespaces(gconstpointer a, gconstpointer b)
{
  const gchar *sa = (const gchar *) a;
  const gchar *sb = (const gchar *) b;
  return(safe_strcmp(sa, sb));
}

static gint
compare_commodity_ids(gconstpointer a, gconstpointer b)
{
  const gnc_commodity *ca = (const gnc_commodity *) a;
  const gnc_commodity *cb = (const gnc_commodity *) b;
  return(safe_strcmp(gnc_commodity_get_mnemonic(ca),
                     gnc_commodity_get_mnemonic(cb)));
}

void
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

static gboolean
xml_add_trn_data(Transaction *t, gpointer data)
{
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

static void
write_template_transaction_data( FILE *out, GNCBook *book )
{
    AccountGroup *ag;

    ag = gnc_book_get_template_group(book);
    if ( xaccGroupGetNumSubAccounts(ag) > 0 )
    {
        fprintf( out, "<%s>\n", TEMPLATE_TRANSACTION_TAG );
        write_account_group( out, ag );
        xaccGroupForEachTransaction( ag, xml_add_trn_data, (gpointer)out );
        fprintf( out, "</%s>\n", TEMPLATE_TRANSACTION_TAG );
    }
}

static void
write_schedXactions( FILE *out, GNCBook *book )
{
    GList *schedXactions;
    SchedXaction *tmpSX;
    xmlNodePtr node;

    /* get list of scheduled transactions from GNCBook */
    schedXactions = gnc_book_get_schedxactions( book );

    if ( schedXactions == NULL )
        return;

    do {
        tmpSX = schedXactions->data;
        node = gnc_schedXaction_dom_tree_create( tmpSX );
        xmlElemDump( out, NULL, node );
        fprintf( out, "\n" );
        xmlFreeNode( node );
    } while ( (schedXactions = schedXactions->next) );
}

static void
write_namespace_decl (FILE *out, const char *namespace)
{
  g_return_if_fail (namespace);
  fprintf(out, " xmlns:%s=\"\"", namespace);
}

static void
write_v2_header (FILE *out)
{
    fprintf(out, "<?xml version=\"1.0\"?>\n");
    fprintf(out, "<" GNC_V2_STRING);
    /*
    write_namespace_decl (out, "cd");
    write_namespace_decl (out, "gnc");
    write_namespace_decl (out, "act");
    write_namespace_decl (out, "cmdty");
    write_namespace_decl (out, "trn");
    write_namespace_decl (out, "ts");
    write_namespace_decl (out, "split");
    write_namespace_decl (out, "sx");
    */
    fprintf(out, ">\n");
}

gboolean
gnc_book_write_to_xml_filehandle_v2(GNCBook *book, FILE *out)
{
    if (!out) return FALSE;

    write_v2_header (out);

    write_counts(out,
                 "commodity",
                 gnc_commodity_table_get_size(
                     gnc_book_get_commodity_table(book)),
                 "account",
                 xaccGroupGetNumSubAccounts(gnc_book_get_group(book)),
                 "transaction",
                 gnc_book_count_transactions(book),
                 "schedxaction",
                 g_list_length( gnc_book_get_schedxactions(book) ),
                 NULL);

    write_commodities(out, book);

    write_pricedb(out, book);

    write_accounts(out, book);

    write_transactions(out, book);

    write_template_transaction_data(out, book);

    write_schedXactions(out, book);

    fprintf(out, "</" GNC_V2_STRING ">\n\n");
    
    return TRUE;
}

gboolean
gnc_book_write_accounts_to_xml_filehandle_v2(GNCBook *book, FILE *out)
{
    if (!out) return FALSE;

    write_v2_header (out);

    write_counts(out,
                 "commodity",
                 gnc_commodity_table_get_size(
                     gnc_book_get_commodity_table(book)),
                 "account",
                 xaccGroupGetNumSubAccounts(gnc_book_get_group(book)),
                 NULL);

    write_commodities(out, book);

    write_accounts(out, book);

    fprintf(out, "</" GNC_V2_STRING ">\n\n");

    return TRUE;
}

gboolean
gnc_book_write_to_xml_file_v2(GNCBook *book, const char *filename)
{
    FILE *out;

    out = fopen(filename, "w");
    gnc_book_write_to_xml_filehandle_v2 (book, out);

    write_emacs_trailer(out);

    if(fclose(out) != 0)
    {
        return FALSE;
    }
    
    return TRUE;
}

/***********************************************************************/
gboolean
gnc_is_xml_data_file_v2(const gchar *name)
{
    return gnc_is_our_xml_file(name, GNC_V2_STRING);
}
