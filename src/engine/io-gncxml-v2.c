#include <glib.h>
#include <stdio.h>
#include <string.h>

#include "gnc-engine.h"
#include "gnc-engine-util.h"

#include "io-gncxml-v2.h"
#include "sixtp.h"
#include "gnc-xml.h"
#include "gnc-book-p.h"

#include "Group.h"

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
                               xaccAccountGetCommodity,
                               xaccAccountSetCommodity);
    clear_up_account_commodity(data->book, act,
                               xaccAccountGetSecurity,
                               xaccAccountSetSecurity);
    if(!xaccAccountGetParent(act))
    {
        xaccGroupInsertAccount(gnc_book_get_group(data->book), act);
    }
    data->counter.accounts_loaded++;
    return FALSE;
}

static gboolean
add_commodity_local(sixtp_gdv2 *data, gnc_commodity *com)
{
    gnc_commodity_table_insert(gnc_book_get_commodity_table(data->book), com);
    data->counter.commodities_loaded++;
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
    return TRUE;
}

static gboolean
add_pricedb_local(sixtp_gdv2 *data, GNCPriceDB *db)
{
    if(!db)
    {
        db = gnc_pricedb_create();
    }

    if(gnc_book_get_pricedb(data->book))
    {
        gnc_pricedb_destroy(gnc_book_get_pricedb(data->book));
    }
    
    gnc_book_set_pricedb(data->book, db);

    return TRUE;
}

static sixtp*
gnc_counter_sixtp_parser_create(void)
{
    return NULL;
}

gboolean
gnc_book_load_from_xml_file_v2(GNCBook *book)
{
    sixtp_gdv2 gd;
    sixtp *parser;
    gpointer parse_result = NULL;
    
    gd.book = book;
    
    {
        AccountGroup *g = gnc_book_get_group(book);
        if(g) xaccFreeAccountGroup(g);
        gnc_book_set_group(book, xaccMallocAccountGroup());
    }
    
    gd.addAccountFunc = add_account_local;
    gd.addCommodityFunc = add_commodity_local;
    gd.addTransactionFunc = add_transaction_local;
    gd.addPriceDBFunc = add_pricedb_local;
    
    parser = sixtp_new();

    if(!sixtp_add_some_sub_parsers(
           parser, TRUE,
           "gnc:count-data", gnc_counter_sixtp_parser_create(),
           "gnc:pricedb", gnc_pricedb_sixtp_parser_create(),
           "gnc:commodity", gnc_commodity_sixtp_parser_create(),
           "gnc:accout", gnc_account_sixtp_parser_create(),
           "gnc:transactions", gnc_transaction_sixtp_parser_create(),
           NULL, NULL))
    {
        return FALSE;
    }

    if(!sixtp_parse_file(parser, gnc_book_get_file_path(book),
                         NULL, &gd, &parse_result))
    {
        sixtp_destroy(parser);
        return FALSE;
    }
    else
    {
    }
    
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

        val = g_strdup_printf("%d", amount);

        node = xmlNewNode(NULL, "gnc:count-data");
        xmlSetProp(node, "cd:type", type);
        xmlNodeAddContent(node, val);

        xmlElemDump(out, NULL, node);
        fprintf(out, "\n");
        
        g_free(val);
        xmlFreeNode(node);

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

    xmlElemDump(out, NULL, node);
    fprintf(out, "\n");

    xmlFreeNode(node);
}

static void
write_accounts(FILE *out, GNCBook *book)
{
    GList *list;
    GList *node;

    list = xaccGroupGetAccountList (gnc_book_get_group(book));

    for (node = list; node; node = node->next) {
        xmlNodePtr accnode;

        accnode = gnc_account_dom_tree_create((Account*)(node->data));

        xmlElemDump(out, NULL, accnode);
        fprintf(out, "\n");

        xmlFreeNode(accnode);
    }
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
                 xaccGroupGetNumAccounts(gnc_book_get_group(book)),
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

static gboolean
eat_whitespace(char **cursor)
{
    while(**cursor && isspace(**cursor))
    {
        *cursor++;
    }

    if(**cursor == '\0')
    {
        return FALSE;
    }
    else
    {
        return TRUE;
    }
}

static gboolean
search_for(char marker, char **cursor)
{
    while(**cursor && **cursor != marker)
    {
        *cursor++;
    }

    if(**cursor == '\0')
    {
        return FALSE;
    }
    else
    {
        return TRUE;
    }
}

gboolean
gnc_is_xml_data_file_v2(const gchar *name)
{
  FILE *f = NULL;
  char first_chunk[256];
  char* cursor = NULL;
  ssize_t num_read;

  g_return_val_if_fail(name, FALSE);

  f = fopen(name, "r");
  g_return_val_if_fail(f, FALSE);

  num_read = fread(first_chunk, sizeof(char), sizeof(first_chunk) - 1, f);
  fclose(f);

  if(num_read == 0) 
  {
      return FALSE;
  }
  
  first_chunk[num_read] = '\0';
  
  cursor = first_chunk;

  if(!eat_whitespace(&cursor))
  {
      return FALSE;
  }
  
  if(strncmp(cursor, "<?xml", 5) == 0) 
  {
      if(!search_for('>', &cursor))
      {
          return FALSE;
      }

      if(!eat_whitespace(&cursor))
      {
          return FALSE;
      }

      if(strncmp(cursor, "<gnc-v2", 7) == 0)
      {
          return TRUE;
      }
      else
      {
          return FALSE;
      }
  }
  else
  {
      return FALSE;
  }
  return FALSE;
}

