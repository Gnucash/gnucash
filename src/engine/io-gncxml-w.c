/*
 * io-gncxml-w.c -- write XML-format gnucash data file
 *
 * FUNCTION:
 * Contains routines to write out values for some basic field types
 * Contains routines which specifically write out the account, txn,
 * and split structures, for saving to file.
 * Contains routines for writing out a query, for network transmission.
 *
 * TBD:
 * Much of the contents of this file is 'mundane', and simply
 * dumps C structure contents into xml.  This could probably be
 * automated with a bit of meta-description of the C structs ...
 * e.g. even some simple #define macros might help here ...
 *
 * HISTORY:
 * Initial code by Rob l. Browning 4Q 2000
 * Tuneups by James Lewis Moss Dec 2000
 * Generic I/O hack by Linas Vepstas January 2001
 *
 * Copyright (c) 2000,2001 Gnumatic Incorporated
 */

#define _GNU_SOURCE

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gnome-xml/tree.h>
#include <gnome-xml/parser.h>
#include <gnome-xml/xmlmemory.h>
#include <gnome-xml/parserInternals.h>

#include "Account.h"
#include "date.h"
#include "DateUtils.h"
#include "Group.h"
#include "messages.h"
#include "Query.h"
#include "Transaction.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "gnc-engine-util.h"

#include "io-gncxml.h"

#include "AccountP.h" /* just for kvp_data */
#include "TransactionP.h" /* just for kvp_data */


#ifdef USE_GUILE_FOR_DOUBLE_CONVERSION 
#include <guile/gh.h>
#endif /* USE_GUILE_FOR_DOUBLE_CONVERSION */

static short module = MOD_IO;

/* Pulled from the libxml-1.8.8 header */
#ifndef xmlChildrenNode
#define xmlChildrenNode childs
#define xmlRootNode root
#endif

static const gchar *gncxml_emacs_trailer =
"<!-- Local variables: -->\n"
"<!-- mode: xml        -->\n"
"<!-- End:             -->\n";

/* ============================================================== */

static gboolean
xml_add_str(xmlNodePtr p, const char *tag, const char *str,
            gboolean include_if_empty) {
  xmlNodePtr child;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  if(!str && !include_if_empty) return(TRUE); 
  if((strlen(str) == 0)  && !include_if_empty) return(TRUE);

  child = xmlNewTextChild(p, NULL, tag, str);
  g_return_val_if_fail(child, FALSE);

  return(TRUE);
}

/* ============================================================== */

static gboolean
xml_add_character(xmlNodePtr p, const char *tag, const char c) {
  char str[2];
  str[0] = c;
  str[1] = '\0';
  return(xml_add_str(p, tag, str, FALSE));
}

/* ============================================================== */

static gboolean
xml_add_gint64(xmlNodePtr p, const char *tag, const gint64 value) {
  xmlNodePtr val_xml;
  char num_string[22];

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);

  g_snprintf(num_string, sizeof (num_string), "%lld", value);

  val_xml = xmlNewTextChild(p, NULL, tag, num_string);
  g_return_val_if_fail(val_xml, FALSE);

  return(TRUE);
}

/* ============================================================== */

static gboolean
xml_add_gint32(xmlNodePtr p, const char *tag, const gint32 value) {
  xmlNodePtr val_xml;
  char num_string[22];

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);

  g_snprintf(num_string, sizeof (num_string), "%d", value);

  val_xml = xmlNewTextChild(p, NULL, tag, num_string);
  g_return_val_if_fail(val_xml, FALSE);

  return(TRUE);
}


/* ============================================================== */
/* 
   RLB writes:
   We have to use guile because AFAICT, libc, and C in general isn't
   smart enough to actually parse it's own output, especially not
   portably (big surprise).

   Linas writes:
   I don't understand the claim; I'm just going to use 
   atof or strtod to accomplish this.

 */

static gboolean
xml_add_double(xmlNodePtr p, const char *tag, const double value) 
{
  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);


#ifdef USE_GUILE_FOR_DOUBLE_CONVERSION 
  {
    /* FIXME: NOT THREAD SAFE - USES STATIC DATA */
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
      g_return_val_if_fail(child, FALSE);
    }
  }

#else /* don't USE_GUILE_FOR_DOUBLE_CONVERSION */
  {
    int len;
    char prtbuf[80];
    xmlNodePtr child;
  
    /* we're just going to use plain-old libc for the double conversion.
     * There was some question as to whether libc is accurate enough
     * in its printf function for doubles, but I don't understand
     * how it couldn't be ...
     */
    len = snprintf (prtbuf, 80, "%24.18g", value);
    if (80 <=len) return (FALSE);
    
    child = xmlNewTextChild(p, NULL, tag, prtbuf);
    g_return_val_if_fail(child, FALSE);
  }

#endif /* USE_GUILE_FOR_DOUBLE_CONVERSION */
  
  return(TRUE);
}

/* ============================================================== */

static gboolean
xml_add_gnc_numeric(xmlNodePtr p, const char *tag, const gnc_numeric n) {
  char *numstr;
  xmlNodePtr child;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);

  /* fprintf(stderr, "WRITE GNUM S: %lld/%lld -> ", n.num, n.denom); */

  numstr = gnc_numeric_to_string(n);
  g_return_val_if_fail(numstr, FALSE);

  /* fprintf(stderr, "%s\n", numstr); */

  child = xmlNewTextChild(p, NULL, tag, numstr);
  g_free(numstr); numstr = FALSE;
  g_return_val_if_fail(child, FALSE);

  return(TRUE);
}

/* ============================================================== */

static gboolean
xml_add_guid(xmlNodePtr p, const char *tag, const GUID *guid) {

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(guid, FALSE);

  {
    const char *guidstr;
    xmlNodePtr child;

    if(!guid) {
      guidstr = NULL;
    } else {
      guidstr = guid_to_string(guid);
      g_return_val_if_fail(guidstr, FALSE);
    }

    child = xmlNewTextChild(p, NULL, tag, guidstr);
    g_return_val_if_fail(child, FALSE);
    if(guidstr) free((void *) guidstr);
  }
  return(TRUE);
}

/* ============================================================== */

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
  
  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(ts, FALSE); 
  if(!include_if_zero && (ts->tv_sec == 0) && (ts->tv_nsec == 0)) return TRUE;

  tmp_timet = ts->tv_sec;
  if(!localtime_r(&tmp_timet, &parsed_time)) return(FALSE);

  num_written = strftime(secs_str, sizeof(secs_str),
                         "%Y-%m-%d %H:%M:%S %z",
                         &parsed_time);
  if(num_written == 0) return(FALSE);
  
  timespec_xml= xmlNewTextChild(p, NULL, tag, NULL);
  g_return_val_if_fail(timespec_xml, FALSE);

  secs_xml = xmlNewTextChild(timespec_xml, NULL, "s", secs_str);
  g_return_val_if_fail(secs_xml, FALSE);
  
  if(ts->tv_nsec) {
    xmlNodePtr nsec_xml;
    char num_string[22];

    g_snprintf(num_string, sizeof (num_string), "%ld", ts->tv_nsec);

    nsec_xml = xmlNewTextChild(timespec_xml, NULL, "ns", num_string);
    g_return_val_if_fail(nsec_xml, FALSE);
  }

  return(TRUE);
}

/* ============================================================== */

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

/* ============================================================== */

static gboolean
xml_add_commodity_restorer(xmlNodePtr p, gnc_commodity *c) {
  xmlNodePtr comm_xml;
  xmlNodePtr rst_xml;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(c, FALSE);

  comm_xml = xmlNewTextChild(p, NULL, "commodity", NULL);  
  g_return_val_if_fail(comm_xml, FALSE);

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

/* ============================================================== */

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

  g_return_val_if_fail(p, FALSE);

  commodities = gnc_engine_commodities();
  g_return_val_if_fail(commodities, FALSE);

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

/* ============================================================== */

static gboolean
xml_add_binary(xmlNodePtr p,
               const char *tag,
               const gchar *format,
               const void *data,
               guint32 size) 
{

  xmlNodePtr value_xml;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(format, FALSE);
  g_return_val_if_fail(data, FALSE);

  value_xml = xmlNewTextChild(p, NULL, tag, NULL);
  g_return_val_if_fail(value_xml, FALSE);
  
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
    PERR("unknown output format %s.\n", format);
    return(FALSE);
  }
  return(TRUE);
}

/* ============================================================== */

static gboolean xml_add_kvp_value(xmlNodePtr p, kvp_value *val);

static gboolean
xml_add_kvp_glist(xmlNodePtr p, const char *tag, GList *lst) {
  xmlNodePtr list_xml;  
  GList *cursor;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(lst, FALSE);
  
  list_xml = xmlNewTextChild(p, NULL, tag, NULL);
  g_return_val_if_fail(list_xml, FALSE);

  for(cursor = lst; cursor; cursor = cursor->next) {
    kvp_value * val = (kvp_value *) cursor->data;
    if(!xml_add_kvp_value(list_xml, val)) {
      return(FALSE);
    }
  }
  return(TRUE);
}

/* ============================================================== */

static gboolean
xml_add_kvp_frame(xmlNodePtr p, const char *tag,
                  const kvp_frame *kvpf,
                  gboolean add_if_empty);

static gboolean
xml_add_kvp_value(xmlNodePtr p, kvp_value *val) {

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(val, FALSE);
  
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
      g_return_val_if_fail(binary_data, FALSE);
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

/* ============================================================== */

static gboolean
xml_add_kvp_slot(xmlNodePtr p, const char *key, kvp_value *val) {
  xmlNodePtr slot_xml;
  xmlNodePtr key_xml;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(key, FALSE);
  g_return_val_if_fail(val, FALSE);

  slot_xml = xmlNewTextChild(p, NULL, "s", NULL);
  g_return_val_if_fail(slot_xml, FALSE);

  key_xml = xmlNewTextChild(slot_xml, NULL, "k", key);
  g_return_val_if_fail(key_xml, FALSE);

  return(xml_add_kvp_value(slot_xml, val));
}

/* ============================================================== */

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

/* ============================================================== */

static gboolean
xml_add_kvp_frame(xmlNodePtr p,
                  const char *tag,
                  const kvp_frame *kvpf,
                  gboolean add_if_empty) {

  xmlNodePtr kvp_xml;
  kvp_value_foreach_info info;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(tag, FALSE);
  g_return_val_if_fail(kvpf, FALSE); 

  kvp_xml = xmlNewNode(NULL, tag);
  g_return_val_if_fail(kvp_xml, FALSE);

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

/* ============================================================== */

static gboolean
xml_add_transaction_split(xmlNodePtr p, Split* s) {
  xmlNodePtr split_xml;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(s, FALSE);

  split_xml = xmlNewTextChild(p, NULL, "split", NULL);  
  g_return_val_if_fail(split_xml, FALSE);

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

/* ============================================================== */

static gboolean
xml_add_txn_restore(xmlNodePtr p, Transaction* t) {

  xmlNodePtr txn_xml;
  xmlNodePtr restore_xml;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(t, FALSE);

  txn_xml = xmlNewTextChild(p, NULL, "transaction", NULL);  
  g_return_val_if_fail(txn_xml, FALSE);

  restore_xml = xmlNewTextChild(txn_xml, NULL, "restore", NULL);  
  g_return_val_if_fail(restore_xml, FALSE);

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

/* ============================================================== */

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

/* ============================================================== */
/* write out the xml for each of the fields in an account */

static gboolean
xml_add_account_restorer(xmlNodePtr p, Account* a) {
  xmlNodePtr acct_xml;

  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(a, FALSE);

  acct_xml = xmlNewTextChild(p, NULL, "account", NULL);  
  g_return_val_if_fail(acct_xml, FALSE);

  acct_xml = xmlNewTextChild(acct_xml, NULL, "restore", NULL);  
  g_return_val_if_fail(acct_xml, FALSE);
  
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
      g_return_val_if_fail(parent_xml, FALSE);
      if(!xml_add_guid(parent_xml, "guid", xaccAccountGetGUID(parent)))
        return(FALSE);
    }
  }

  {
    AccountGroup *g = xaccAccountGetChildren(a);
    if(g) {
      GList *list = xaccGroupGetAccountList (g);
      GList *node;

      for (node = list; node; node = node->next) {
        Account *current_acc = node->data;

        if(!xml_add_account_restorer(p, current_acc))
          return(FALSE);
      }
    }
  }
  return(TRUE);
}

/* ============================================================== */
/* loop over all accounts in the group */

static gboolean
xml_add_account_restorers(xmlNodePtr p, AccountGroup *g) {
  GList *list;
  GList *node;
  
  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(g, FALSE);

  list = xaccGroupGetAccountList (g);

  for (node = list; node; node = node->next) {
    Account *current_acc = node->data;
    xml_add_account_restorer(p, current_acc);
  }
  return(TRUE);
}

/* ============================================================== */
/* push query terms into xml */
/* XXX hack alert not all predicates currently implemented */

static gboolean
xml_add_qterm_restorer(xmlNodePtr qxml, QueryTerm *qt) 
{
  int rc;
  xmlNodePtr p = NULL;

  g_return_val_if_fail(qxml, FALSE);
  g_return_val_if_fail(qt, FALSE);

  switch (qt->data.type) {
    case PD_DATE:
       p = xmlNewTextChild(qxml, NULL, "date-pred", NULL);  
       xml_add_gint32(p, "use-start", qt->data.date.use_start);
       xml_add_gint32(p, "use-end", qt->data.date.use_end);
       if (qt->data.date.use_start) {
          xml_add_editable_timespec(p, "start-date", 
                                   &qt->data.date.start, FALSE);
       }
       if (qt->data.date.use_end) {
          xml_add_editable_timespec(p, "end-date", 
                                   &qt->data.date.end, FALSE);
       }
       break;

    case PD_AMOUNT:
       p = xmlNewTextChild(qxml, NULL, "amount-pred", NULL);  
       PERR ("unimplemented");
       break;

    case PD_ACCOUNT: 
       p = xmlNewTextChild(qxml, NULL, "account-pred", NULL);  
       PERR ("unimplemented");
       break;

    case PD_STRING:
       p = xmlNewTextChild(qxml, NULL, "string-pred", NULL);  
       xml_add_gint32(p, "case-sens", qt->data.str.case_sens);
       xml_add_gint32(p, "use-regexp", qt->data.str.use_regexp);
       xml_add_str(p, "matchstring", qt->data.str.matchstring, TRUE);
       break;

    case PD_CLEARED:
       p = xmlNewTextChild(qxml, NULL, "cleared-pred", NULL);  
       PERR ("unimplemented");
       break;

    case PD_BALANCE:
       p = xmlNewTextChild(qxml, NULL, "balance-pred", NULL);  
       PERR ("unimplemented");
       break;

    case PD_MISC:
       p = xmlNewTextChild(qxml, NULL, "misc-pred", NULL);  
       PERR ("unimplemented");
       break;

    default:
  }

  if (!p) return (FALSE);

  rc = xml_add_gint32(p, "sense", qt->sense);
  if (!rc) return(FALSE);

  return(TRUE);
}

/* ============================================================== */
/* loop over all terms in the query */

static gboolean
xml_add_query_restorers(xmlNodePtr p, Query *q) 
{
  xmlNodePtr qxml, restore_xml;
  GList *list;
  GList *node;
  
  g_return_val_if_fail(p, FALSE);
  g_return_val_if_fail(q, FALSE);

  list = xaccQueryGetTerms (q);

  /* write the nested <query> <restore> */
  qxml = xmlNewTextChild(p, NULL, "query", NULL);  
  g_return_val_if_fail(qxml, FALSE);

  restore_xml = xmlNewTextChild(qxml, NULL, "restore", NULL);  
  g_return_val_if_fail(restore_xml, FALSE);

  for (node = list; node; node = node->next) {
    QueryTerm *qt = node->data;
    xml_add_qterm_restorer(restore_xml, qt);
  }
  return(TRUE);
}

/* ============================================================== */

static gboolean
gncxml_append_emacs_trailer(const gchar *filename)
{
    FILE *toappend;
    
    toappend = fopen(filename, "a+");
    if(!toappend) 
    {
        PERR("Unable to append emacs trailer: %s\n", strerror(errno));
        return 0;
    }
    
    fprintf(toappend, gncxml_emacs_trailer);

    return fclose(toappend);
}
    
/* =============================================================== */
/* create a new xml document and poke all the query terms into it. */

static xmlDocPtr
gncxml_new_query_doc (Query *q)
{
  xmlDocPtr doc;
  xmlNodePtr query_server;
  xmlNodePtr tmpnode;
  
  doc = xmlNewDoc("1.0");
  doc->xmlRootNode = xmlNewDocNode(doc, NULL, "gnc", NULL);
   
  tmpnode = xmlNewTextChild(doc->xmlRootNode, NULL, "version", "1");
  if(!tmpnode) {
    PERR ("can't create new text child");
    xmlFreeDoc(doc);
    return 0x0;
  }

  query_server = xmlNewTextChild(doc->xmlRootNode, NULL, "query-server", NULL);
  if(!query_server) {
    PERR ("couldn't creat query terms");
    xmlFreeDoc(doc);
    return 0x0;
  }

  if(!xml_add_query_restorers(query_server, q)) {
    PERR ("couldn't write query server");
    xmlFreeDoc(doc);
    return 0x0;
  }

  return doc;
}

/* =============================================================== */
/* create a new xml document and poke all account & txn data into it. */

static xmlDocPtr
gncxml_newdoc (AccountGroup *group)
{
  xmlDocPtr doc;
  xmlNodePtr ledger_data;
  xmlNodePtr tmpnode;
  
  doc = xmlNewDoc("1.0");
  doc->xmlRootNode = xmlNewDocNode(doc, NULL, "gnc", NULL);
   
  tmpnode = xmlNewTextChild(doc->xmlRootNode, NULL, "version", "1");
  if(!tmpnode) {
    PERR ("can't create new text child");
    xmlFreeDoc(doc);
    return 0x0;
  }

  ledger_data = xmlNewTextChild(doc->xmlRootNode, NULL, "ledger-data", NULL);
  if(!ledger_data) {
    PERR ("couldn't create xml text child");
    xmlFreeDoc(doc);
    return 0x0;
  }

  if(!xml_add_commodity_restorers(ledger_data)) {
    PERR ("couldn't commodity restore");
    xmlFreeDoc(doc);
    return 0x0;
  }

  if(!xml_add_account_restorers(ledger_data, group)) {
    PERR ("couldn't account restore");
    xmlFreeDoc(doc);
    return 0x0;
  }

  if(!xml_add_txn_and_split_restorers(ledger_data, group)) {
    PERR ("couldn't txn restore");
    xmlFreeDoc(doc);
    return 0x0;
  }

  return doc;
}

/* =============================================================== */

void
gncxml_write_to_buf (AccountGroup *group, char **bufp, int *sz)
{
  xmlDocPtr doc;

  doc = gncxml_newdoc (group);
  if (!doc) return;

  xmlDocDumpMemory (doc, (xmlChar **)bufp, sz);

  PINFO ("wrote %d bytes", *sz);
}

/* =============================================================== */

void
gncxml_write_query_to_buf (Query *q, char **bufp, int *sz)
{
  xmlDocPtr doc;

  doc = gncxml_new_query_doc (q);
  if (!doc) return;

  xmlDocDumpMemory (doc, (xmlChar **)bufp, sz);

  PINFO ("wrote %d bytes", *sz);
}

/* =============================================================== */
/* write the account group to a filename */

gboolean
gncxml_write(AccountGroup *group, const gchar *filename) 
{
  xmlDocPtr doc;
  int status;

  if (!group || !filename) return FALSE;

  doc = gncxml_newdoc (group);
  if (!doc) return FALSE;

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

/* ========================= END OF FILE ============================ */
