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
 * Initial code by Rob L. Browning 4Q 2000
 * Tuneups by James LewisMoss Dec 2000-Feb 2001
 * Generic I/O hack by Linas Vepstas January 2001
 *
 * Copyright (c) 2000,2001 Gnumatic Incorporated
 */

/********************************************************************\
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

#include <config.h>

#define _GNU_SOURCE

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-xml-helper.h"
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

#include "sixtp-writers.h"
#include "io-gncxml.h"

#include "AccountP.h" /* just for kvp_data */
#include "TransactionP.h" /* just for kvp_data */


#ifdef USE_GUILE_FOR_DOUBLE_CONVERSION 
#include <guile/gh.h>
#endif /* USE_GUILE_FOR_DOUBLE_CONVERSION */

static short module = MOD_IO;

/* Pulled from the libxml-1.8.8 header */

static const gchar *gncxml_emacs_trailer =
"<!-- Local variables: -->\n"
"<!-- mode: xml        -->\n"
"<!-- End:             -->\n";

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
gncxml_newdoc (AccountGroup *group, int do_txns)
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

  if (do_txns) {
    if(!xml_add_txn_and_split_restorers(ledger_data, group)) {
      PERR ("couldn't txn restore");
      xmlFreeDoc(doc);
      return 0x0;
    }
  }

  return doc;
}

/* =============================================================== */

void
gncxml_write_to_buf (AccountGroup *group, char **bufp, int *sz)
{
  xmlDocPtr doc;

  doc = gncxml_newdoc (group, 1);
  if (!doc) return;

  xmlDocDumpMemory (doc, (xmlChar **)bufp, sz);

  PINFO ("wrote %d bytes", *sz);
}

/* =============================================================== */

void
gncxml_write_group_to_buf (AccountGroup *group, char **bufp, int *sz)
{
  xmlDocPtr doc;

  doc = gncxml_newdoc (group, 0);
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

  doc = gncxml_newdoc (group, 1);
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
