/*
 * io-gncxml-w.c -- write XML-format gnucash data file
 */

#define _GNU_SOURCE

#include <glib.h>
#include <stdio.h>
#include <string.h>

#include <tree.h>
#include <parser.h>
#include <xmlmemory.h>
#include <parserInternals.h>

#include "Account.h"
#include "date.h"
#include "DateUtils.h"
#include "Group.h"
#include "messages.h"
#include "Transaction.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"

#include "io-gncxml.h"

#include "AccountP.h" /* just for kvp_data */
#include "TransactionP.h" /* just for kvp_data */

/* Hack to get going... */
#include "FileIOP.h"
#include <guile/gh.h>


/* Pulled from the libxml-1.8.8 header */
#ifndef xmlChildrenNode
#define xmlChildrenNode childs
#define xmlRootNode root
#endif

static const gchar *gncxml_emacs_trailer =
"<!-- Local variables: -->\n"
"<!-- mode: xml        -->\n"
"<!-- End:             -->\n";

static gboolean
xml_add_str(xmlNodePtr p, const char *tag, const char *str,
            gboolean include_if_empty) {
  xmlNodePtr child;

  if(!p) return(FALSE);
  if(!tag) return(FALSE);
  if(!str && !include_if_empty) return(TRUE); 
  if((strlen(str) == 0)  && !include_if_empty) return(TRUE);

  child = xmlNewTextChild(p, NULL, tag, str);
  if(!child) return(FALSE);

  return(TRUE);
}

static gboolean
xml_add_character(xmlNodePtr p, const char *tag, const char c) {
  char str[2];
  str[0] = c;
  str[1] = '\0';
  return(xml_add_str(p, tag, str, FALSE));
}

static gboolean
xml_add_gint64(xmlNodePtr p, const char *tag, const gint64 value) {
  xmlNodePtr val_xml;
  const gchar *numstr;

  if(!p) return(FALSE);
  if(!tag) return(FALSE);
  
  numstr = g_strdup_printf("%lld", value);
  if(!numstr) return(FALSE);
  val_xml = xmlNewTextChild(p, NULL, tag, numstr);
  g_free((char *) numstr);
  if(!val_xml) return(FALSE);
  return(TRUE);
}

static gboolean
xml_add_double(xmlNodePtr p, const char *tag, const double value) {
  /* FIXME: NOT THREAD SAFE - USES STATIC DATA */

  if(!p) return(FALSE);
  if(!tag) return(FALSE);

  {
    static SCM number_to_string;
    static gboolean ready = FALSE;
    const char *numstr;

    if(!ready) {
      number_to_string = gh_eval_str("number->string");
      scm_protect_object(number_to_string);
      ready = TRUE;
    }

    numstr = gh_scm2newstr(gh_call1(number_to_string, gh_double2scm(value)),
                           NULL);
    
    if(!numstr) {
      return(FALSE);
    } else {
      xmlNodePtr child = xmlNewTextChild(p, NULL, tag, numstr);
      free((void *) numstr);
      if(!child) return(FALSE);
    }
  }

    return(TRUE);
}

static gboolean
xml_add_gnc_numeric(xmlNodePtr p, const char *tag, const gnc_numeric n) {
  char *numstr;
  xmlNodePtr child;

  if(!p) return(FALSE);
  if(!tag) return(FALSE);

  /* fprintf(stderr, "WRITE GNUM S: %lld/%lld -> ", n.num, n.denom); */

  numstr = gnc_numeric_to_string(n);
  if(!numstr) return(FALSE);

  /* fprintf(stderr, "%s\n", numstr); */

  child = xmlNewTextChild(p, NULL, tag, numstr);
  g_free(numstr); numstr = FALSE;
  if(!child) return(FALSE);

  return(TRUE);
}


static gboolean
xml_add_guid(xmlNodePtr p, const char *tag, const GUID *guid) {

  if(!p) return(FALSE);
  if(!tag) return(FALSE);
  if(!guid) return(FALSE);

  {
    const char *guidstr;
    xmlNodePtr child;

    if(!guid) {
      guidstr = NULL;
    } else {
      guidstr = guid_to_string(guid);
      if(!guidstr) return(FALSE);
    }

    child = xmlNewTextChild(p, NULL, tag, guidstr);
    if(!child) return(FALSE);
    if(guidstr) free((void *) guidstr);
  }
  return(TRUE);
}

static gboolean
xml_add_editable_timespec(xmlNodePtr p,
                          const char *tag,
                          const Timespec *ts,
                          gboolean include_if_zero) {
  xmlNodePtr timespec_xml;
  xmlNodePtr secs_xml;
  size_t num_written;
  struct tm parsed_time;
  time_t tmp_timet;
  char secs_str[512]; /* This should be way bigger than we need.
                         Still, it's bogus, we ought to have
                         astrftime... */
  
  if(!p) return(FALSE);
  if(!tag) return(FALSE);
  if(!ts) return(FALSE); 
  if(!include_if_zero && (ts->tv_sec == 0) && (ts->tv_nsec == 0)) return TRUE;

  tmp_timet = ts->tv_sec;
  if(!localtime_r(&tmp_timet, &parsed_time)) return(FALSE);

  num_written = strftime(secs_str, sizeof(secs_str),
                         "%Y-%m-%d %H:%M:%S %z",
                         &parsed_time);
  if(num_written == 0) return(FALSE);
  
  timespec_xml= xmlNewTextChild(p, NULL, tag, NULL);
  if(!timespec_xml) return(FALSE);

  secs_xml = xmlNewTextChild(timespec_xml, NULL, "s", secs_str);
  if(!secs_xml) return(FALSE);
  
  if(ts->tv_nsec) {
    xmlNodePtr nsec_xml;
    gchar *nsec_str = g_strdup_printf("%ld", ts->tv_nsec);

    if(!nsec_str) return(FALSE);
    nsec_xml = xmlNewTextChild(timespec_xml, NULL, "ns", nsec_str);
    if(!nsec_xml) return(FALSE);
    g_free(nsec_str);
  }

  return(TRUE);
}

static gboolean
xml_add_commodity_ref(xmlNodePtr p, const char *tag, const gnc_commodity *c) {
  xmlNodePtr c_xml = NULL;
  gboolean ok = FALSE;

  if(p && tag) {
    if(!c) {
      ok = TRUE;
    } else {
      c_xml= xmlNewTextChild(p, NULL, tag, NULL);
      if(c_xml) {
        const gchar *namestr = gnc_commodity_get_namespace(c);
        if(namestr) {
          xmlNodePtr namespace_xml = xmlNewTextChild(c_xml, NULL, "space", namestr);
          if(namespace_xml) {
            const gchar *idstr = gnc_commodity_get_mnemonic(c);
            xmlNodePtr id_xml = xmlNewTextChild(c_xml, NULL, "id", idstr);
            if(id_xml) ok = TRUE;
          }
        }
      }
    }
  }
  
  if(!ok && c_xml) xmlFreeNode(c_xml);
  return(TRUE);
}

static gboolean
xml_add_commodity_restorer(xmlNodePtr p, gnc_commodity *c) {
  xmlNodePtr comm_xml;
  xmlNodePtr rst_xml;

  if(!p) return(FALSE);
  if(!c) return(FALSE);

  comm_xml = xmlNewTextChild(p, NULL, "commodity", NULL);  
  if(!comm_xml) return(FALSE);

  rst_xml = xmlNewTextChild(comm_xml, NULL, "restore", NULL);  
  if(!rst_xml) {
    xmlFreeNode(comm_xml);
    return(FALSE);
  }

  if(!xml_add_str(rst_xml, "space", gnc_commodity_get_namespace(c), FALSE)) {
    xmlFreeNode(comm_xml);
    return(FALSE);
  }
  if(!xml_add_str(rst_xml, "id", gnc_commodity_get_mnemonic(c), FALSE)) {
    xmlFreeNode(comm_xml);
    return(FALSE);
  }
  if(!xml_add_str(rst_xml, "name", gnc_commodity_get_fullname(c), FALSE)) {
    xmlFreeNode(comm_xml);
    return(FALSE);
  }
  if(!xml_add_str(rst_xml, "xcode", gnc_commodity_get_exchange_code(c), FALSE)) {
    xmlFreeNode(comm_xml);
    return(FALSE);
  }
  if(!xml_add_gint64(rst_xml, "fraction", gnc_commodity_get_fraction(c))) {
    xmlFreeNode(comm_xml);
    return(FALSE);
  }

  return(TRUE);
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

static gboolean
xml_add_commodity_restorers(xmlNodePtr p) {
  gnc_commodity_table *commodities;
  GList *namespaces;
  GList *lp;

  if(!p) return(FALSE);

  commodities = gnc_engine_commodities();
  if(!commodities) return(FALSE);

  namespaces = g_list_sort(gnc_commodity_table_get_namespaces(commodities),
                           compare_namespaces);
  

  for(lp = namespaces; lp; lp = lp->next) {
    gchar *space;

    if(!lp->data) {
      g_list_free (namespaces);
      return(FALSE);
    }

    space = (gchar *) lp->data;
    if(strcmp(GNC_COMMODITY_NS_ISO, space) != 0) {
      GList *comms = gnc_commodity_table_get_commodities(commodities, space);
      GList *lp2;

      comms = g_list_sort(comms, compare_commodity_ids);

      for(lp2 = comms; lp2; lp2 = lp2->next) {
        gnc_commodity *com = (gnc_commodity *) lp2->data;

        if(!xml_add_commodity_restorer(p, com)) {
          g_list_free (comms);
          g_list_free (namespaces);
          return(FALSE);
        }
      }

      g_list_free (comms);
    }
  }

  g_list_free (namespaces);

  return(TRUE);
}

static gboolean
xml_add_binary(xmlNodePtr p,
               const char *tag,
               const gchar *format,
               const void *data,
               guint32 size) {

  xmlNodePtr value_xml;

  if(!p) return(FALSE);
  if(!tag) return(FALSE);
  if(!format) return(FALSE);
  if(!data) return(FALSE);

  value_xml = xmlNewTextChild(p, NULL, tag, NULL);
  if(!value_xml) return(FALSE);
  
  if(size == 0) return(TRUE);

  if(0 == strcmp(format, "hex")) {
    /* Write out the chars as hex, buffering them in max 64 character
       lines.  I was going to use xmlNewTextChild, and xmlTextConcat,
       but that doesn't seem to work, and looking at the source,
       xmlNewTextChild doesn't set the node type to a type that
       xmlTextConcat will recognize and allow. */
    
    const guint max_line_len = 64;
    xmlNodePtr data_xml = NULL;
    GString *output;
    guint32 i;
    
    output = g_string_sized_new(max_line_len + 2);
    
    for(i = 0; i < size; i++) {
      g_string_sprintfa(output, "%x", (int) (((char *) data)[i]));
      if(((i + 1) % max_line_len) == 0) {
        data_xml = xmlNewTextChild(value_xml, NULL, "hex", output->str);
        if(!data_xml) {
          return(FALSE);
          g_string_free(output, TRUE);
        }
        g_string_truncate(output, 0);
      }
    }
    
    if(strlen(output->str) > 0) {
      data_xml = xmlNewTextChild(value_xml, NULL, "hex", output->str);
      if(!data_xml) {
        g_string_free(output, TRUE);
        return(FALSE);
      }
    }
    g_string_free(output, TRUE);

  } else {
    fprintf(stderr, "xml_add_binary: unknown output format %s.\n", format);
    return(FALSE);
  }
  return(TRUE);
}

static gboolean xml_add_kvp_value(xmlNodePtr p, kvp_value *val);

static gboolean
xml_add_kvp_glist(xmlNodePtr p, const char *tag, GList *lst) {
  xmlNodePtr list_xml;  
  GList *cursor;

  if(!p) return(FALSE);
  if(!tag) return(FALSE);
  if(!lst) return(FALSE);
  
  list_xml = xmlNewTextChild(p, NULL, tag, NULL);
  if(!list_xml) return(FALSE);

  for(cursor = lst; cursor; cursor = cursor->next) {
    kvp_value * val = (kvp_value *) cursor->data;
    if(!xml_add_kvp_value(list_xml, val)) {
      return(FALSE);
    }
  }
  return(TRUE);
}

static gboolean
xml_add_kvp_frame(xmlNodePtr p, const char *tag,
                  const kvp_frame *kvpf,
                  gboolean add_if_empty);

static gboolean
xml_add_kvp_value(xmlNodePtr p, kvp_value *val) {

  if(!p) return(FALSE);
  if(!val) return(FALSE);
  
  switch(kvp_value_get_type(val)) {
  case KVP_TYPE_GINT64:
    return(xml_add_gint64(p, "gint64", kvp_value_get_gint64(val)));
    break;
  case KVP_TYPE_DOUBLE:
    return(xml_add_double(p, "double", kvp_value_get_double(val)));
    break;
  case KVP_TYPE_NUMERIC:
    return(xml_add_gnc_numeric(p, "numeric", kvp_value_get_numeric(val)));
    break;
  case KVP_TYPE_STRING:
    return(xml_add_str(p, "string", kvp_value_get_string(val), TRUE));
    break;
  case KVP_TYPE_GUID:
    return(xml_add_guid(p, "guid", kvp_value_get_guid(val)));
    break;
  case KVP_TYPE_BINARY:
    {
      guint64 size;
      void *binary_data = kvp_value_get_binary(val, &size);
      if(!binary_data) return(FALSE);
      return(xml_add_binary(p, "binary", "hex", binary_data, size));
    }
    break;
  case KVP_TYPE_GLIST:
    return(xml_add_kvp_glist(p, "glist", kvp_value_get_glist(val)));    
    break;
  case KVP_TYPE_FRAME:
    return(xml_add_kvp_frame(p, "frame", kvp_value_get_frame(val), TRUE));
    break;
  default:
    return(FALSE);
    break;
  };
  
  return(TRUE);
}

static gboolean
xml_add_kvp_slot(xmlNodePtr p, const char *key, kvp_value *val) {
  xmlNodePtr slot_xml;
  xmlNodePtr key_xml;

  if(!p) return(FALSE);
  if(!key) return(FALSE);
  if(!val) return(FALSE);

  slot_xml = xmlNewTextChild(p, NULL, "s", NULL);
  if(!slot_xml) return(FALSE);

  key_xml = xmlNewTextChild(slot_xml, NULL, "k", key);
  if(!key_xml) return(FALSE);

  return(xml_add_kvp_value(slot_xml, val));
}

typedef struct {
  xmlNodePtr node;
  gint64 keycount;
} kvp_value_foreach_info;

static void
xml_add_kvp_value_foreach_adapter(const char *key,
                                  kvp_value *value,
                                  gpointer data) {
  kvp_value_foreach_info *info = (kvp_value_foreach_info *) data;
  xml_add_kvp_slot(info->node, key, value);
  info->keycount++;
}

static gboolean
xml_add_kvp_frame(xmlNodePtr p,
                  const char *tag,
                  const kvp_frame *kvpf,
                  gboolean add_if_empty) {

  xmlNodePtr kvp_xml;
  kvp_value_foreach_info info;

  if(!p) return(FALSE);
  if(!tag) return(FALSE);
  if(!kvpf) return(FALSE); 

  kvp_xml = xmlNewNode(NULL, tag);
  if(!kvp_xml) return(FALSE);

  info.node = kvp_xml;
  info.keycount = 0;
  kvp_frame_for_each_slot((kvp_frame *) kvpf,
                          xml_add_kvp_value_foreach_adapter,
                          &info); 
  if(add_if_empty || info.keycount) {
    xmlAddChild(p, kvp_xml);
  } else {
    xmlFreeNode(kvp_xml);
  }
  
  return(TRUE);
}

static gboolean
xml_add_transaction_split(xmlNodePtr p, Split* s) {
  xmlNodePtr split_xml;

  if(!p) return(FALSE);
  if(!s) return(FALSE);

  split_xml = xmlNewTextChild(p, NULL, "split", NULL);  
  if(!split_xml) return(FALSE);

  if(!xml_add_guid(split_xml, "guid", xaccSplitGetGUID(s)))
    return(FALSE);

  if(!xml_add_str(split_xml, "memo", xaccSplitGetMemo(s), FALSE))
    return(FALSE);

  if(!xml_add_str(split_xml, "action", xaccSplitGetAction(s), FALSE))
    return(FALSE);

  /* reconcile-state */
  { 
    char state = xaccSplitGetReconcile(s);
    if(!xml_add_character(split_xml, "reconcile-state", state))
      return(FALSE);
  }
  
  {
    /* reconcile-date */
    Timespec ts;
    xaccSplitGetDateReconciledTS(s, &ts);
    if(!xml_add_editable_timespec(split_xml, "reconcile-date", &ts, FALSE))
      return(FALSE);
  }

  /* share-amount */
  if(!xml_add_gnc_numeric(split_xml, "value", xaccSplitGetValue(s)))
    return(FALSE);

  /* share-price */
  if(!xml_add_gnc_numeric(split_xml, "quantity", xaccSplitGetShareAmount(s)))
    return(FALSE);

  /* account */
  { 
    Account *acct = xaccSplitGetAccount(s);
    if(acct) {
      if(!xml_add_guid(split_xml, "account", xaccAccountGetGUID(acct)))
        return(FALSE);
    }
  }

  if(s->kvp_data) {
    if(!xml_add_kvp_frame(split_xml, "slots", s->kvp_data, FALSE))
      return(FALSE);
  }

  return(TRUE);
}


static gboolean
xml_add_txn_restore(xmlNodePtr p, Transaction* t) {

  xmlNodePtr txn_xml;
  xmlNodePtr restore_xml;

  if(!p) return(FALSE);
  if(!t) return(FALSE);

  txn_xml = xmlNewTextChild(p, NULL, "transaction", NULL);  
  if(!txn_xml) return(FALSE);

  restore_xml = xmlNewTextChild(txn_xml, NULL, "restore", NULL);  
  if(!restore_xml) return(FALSE);

  if(!xml_add_guid(restore_xml, "guid", xaccTransGetGUID(t)))
    return(FALSE);
  if(!xml_add_str(restore_xml, "num", xaccTransGetNum(t), FALSE))
    return(FALSE);
  {
    Timespec ts;
    xaccTransGetDatePostedTS(t, &ts);
    if(!xml_add_editable_timespec(restore_xml, "date-posted", &ts, FALSE))
      return(FALSE);
  }
  {
    Timespec ts;
    xaccTransGetDateEnteredTS(t, &ts);
    if(!xml_add_editable_timespec(restore_xml, "date-entered", &ts, FALSE))
      return(FALSE);
  }
  if(!xml_add_str(restore_xml, "description", xaccTransGetDescription(t), FALSE))
    return(FALSE);

  if(t->kvp_data) {
    if(!xml_add_kvp_frame(restore_xml, "slots", t->kvp_data, FALSE))
      return(FALSE);
  }

  {
    guint32 n = 0;
    Split *s = xaccTransGetSplit(t, n);

    while(s) {
      if(!xml_add_transaction_split(restore_xml, s)) return(FALSE); 
      n++;
      s = xaccTransGetSplit(t, n);
    }
  }

  return(TRUE);
}

static gboolean
xml_add_txn_restore_adapter(Transaction *t, gpointer data) {
  xmlNodePtr xml_node = (xmlNodePtr) data;
  return(xml_add_txn_restore(xml_node, t));
}

static gboolean
xml_add_txn_and_split_restorers(xmlNodePtr p, AccountGroup *g) {
  return(xaccGroupForEachTransaction(g,
                                     xml_add_txn_restore_adapter,
                                     (gpointer) p));
}

static gboolean
xml_add_account_restorer(xmlNodePtr p, Account* a) {
  xmlNodePtr acct_xml;

  if(!p) return(FALSE);
  if(!a) return(FALSE);

  acct_xml = xmlNewTextChild(p, NULL, "account", NULL);  
  if(!acct_xml) return(FALSE);

  acct_xml = xmlNewTextChild(acct_xml, NULL, "restore", NULL);  
  if(!acct_xml) return(FALSE);
  
  if(!xml_add_str(acct_xml, "name",
                  xaccAccountGetName(a), FALSE))
    return(FALSE);
  if(!xml_add_guid(acct_xml, "guid",
                   xaccAccountGetGUID(a)))
    return(FALSE);
  if(!xml_add_str(acct_xml, "type",
                  xaccAccountTypeEnumAsString(xaccAccountGetType(a)), FALSE))
    return(FALSE);
  if(!xml_add_str(acct_xml, "code",
                  xaccAccountGetCode(a), FALSE))
    return(FALSE);
  if(!xml_add_str(acct_xml, "description",
                  xaccAccountGetDescription(a), FALSE))
    return(FALSE);
  /* Notes field is now in kvp table. */
  if(!xml_add_commodity_ref(acct_xml, "currency", xaccAccountGetCurrency(a)))
    return(FALSE);
  if(!xml_add_commodity_ref(acct_xml, "security", xaccAccountGetSecurity(a)))
    return(FALSE);

  if(a->kvp_data) {
    if(!xml_add_kvp_frame(acct_xml, "slots", a->kvp_data, FALSE))
      return(FALSE);
  }

  {
    Account *parent = xaccAccountGetParentAccount(a);
    if(parent) {
      xmlNodePtr parent_xml = xmlNewTextChild(acct_xml, NULL, "parent", NULL);  
      if(!parent_xml) return(FALSE);
      if(!xml_add_guid(parent_xml, "guid", xaccAccountGetGUID(parent)))
        return(FALSE);
    }
  }

  {
    AccountGroup *g = xaccAccountGetChildren(a);
    if(g) {
      guint32 num_accounts = xaccGroupGetNumAccounts(g);  
      guint32 i = 0;
      while(i < num_accounts) {
        Account *current_acc = xaccGroupGetAccount(g, i);

        if(!xml_add_account_restorer(p, current_acc)) return(FALSE);
        i++;
      }
    }
  }
  return(TRUE);
}
  
static gboolean
xml_add_account_restorers(xmlNodePtr p, AccountGroup *g) {
  guint32 i = 0;
  guint32 num_accounts;
  
  if(!p) return(FALSE);
  if(!g) return(FALSE);

  num_accounts = xaccGroupGetNumAccounts(g); 
  while(i < num_accounts) {
    Account *current_acc = xaccGroupGetAccount(g, i);
    xml_add_account_restorer(p, current_acc);
    i++;
  }
  return(TRUE);
}

static gboolean
gncxml_append_emacs_trailer(const gchar *filename)
{
    FILE *toappend;
    
    toappend = fopen(filename, "a+");
    if(!toappend) 
    {
        fprintf(stderr, "Unable to append emacs trailer: %s\n",
                strerror(errno));
        return 0;
    }
    
    fprintf(toappend, gncxml_emacs_trailer);

    return fclose(toappend);
}
    
gboolean
gncxml_write(AccountGroup *group, const gchar *filename) {
  /* fixme: this should be broken up into sub-functions later. */

  xmlDocPtr doc;
  xmlNodePtr ledger_data;
  xmlNodePtr tmpnode;
  int status;
  
  doc = xmlNewDoc("1.0");
  doc->xmlRootNode = xmlNewDocNode(doc, NULL, "gnc", NULL);
   
  tmpnode = xmlNewTextChild(doc->xmlRootNode, NULL, "version", "1");
  if(!tmpnode) {
    xmlFreeDoc(doc);
    return FALSE;
  }

  ledger_data = xmlNewTextChild(doc->xmlRootNode, NULL, "ledger-data", NULL);
  if(!ledger_data) {
    xmlFreeDoc(doc);
    return FALSE;
  }

  if(!xml_add_commodity_restorers(ledger_data)) {
    xmlFreeDoc(doc);
    return FALSE;
  }

  if(!xml_add_account_restorers(ledger_data, group)) {
    xmlFreeDoc(doc);
    return FALSE;
  }

  if(!xml_add_txn_and_split_restorers(ledger_data, group)) {
    xmlFreeDoc(doc);
    return FALSE;
  }

  xmlIndentTreeOutput = TRUE;

  status = xmlSaveFile(filename, doc);
  xmlFreeDoc(doc);

  gncxml_append_emacs_trailer(filename);
  
  /* FIXME: This gives me a non-zero result, even when everything's fine ???
     status = xmlDocDump(outfile, doc);

     This crashes with the current libxml, but they don't document that
     they close the file, so I don't know why...
     assert(fclose(outfile) == 0);
  */
  return(status != -1);
}
