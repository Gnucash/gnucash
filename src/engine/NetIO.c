/********************************************************************\
 * NetIO.c -- read and write network IO                             *
 * Copyright (C) 2001 Linas Vepstas <linas@linas.org>               *
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
 *                                                                  *
\********************************************************************/

/*
 * Rudimentary implmentation right now; good enough for a demo, 
 * but that's all.
 *
 * HACK ALRT -- this should be moved into its own sbdirectory
 * Mostly so that the engine build doesn't require libghttp
 * as a dependency.  
 */


#include <ghttp.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "BackendP.h"
#include "NetIO.h"
#include "gnc-book.h"
#include "gnc-engine-util.h"
#include "io-gncxml.h"

static short module = MOD_BACKEND;

typedef struct _xmlend XMLBackend;

struct _xmlend {
  Backend be;

  ghttp_request *request;
  char * query_url;
  char * auth_cookie;
};

Backend *xmlendNew (void);


/* ==================================================================== */
/* Perform vaious validty checks on the reply:
 *   -- was the content type text/gnc-xml ?
 *   -- was there a reply body, of positive length?
 *   -- did the body appear to contain gnc xml data?
 * 
 * Also, if the reply contained a set-cookie command, process that.
 */

static int
check_response (XMLBackend *be)
{
  ghttp_request *request;
  const char *bufp;
  int len;

  request = be->request;

  /* get the content type out of the header */
  bufp = ghttp_get_header(request, "Content-Type");
  PINFO ("Content-Type: %s", bufp);

  /* in principle, we should reject content that isn't 
   * labelled as text/gnc-xml.  But for now, we'll be soft ...
   */
  if (strncmp (bufp, "text/gnc-xml", 12))
  {
    PWARN ("content type is incorrectly labelled as %s", bufp);
    be->be.last_err = ERR_NETIO_WRONG_CONTENT_TYPE;
    // return 0;
  }
   
  len = ghttp_get_body_len(request);
  PINFO ("reply length=%d\n", len);

  /* body length must be postive */
  if (0 >= len) 
  {
    const char * errstr = ghttp_get_error (request);
    const char * reason = ghttp_reason_phrase (request);
    PERR ("connection failed: %s %s\n", errstr, reason);

    be->be.last_err = ERR_NETIO_SHORT_READ;
    return len;
  }

  bufp = ghttp_get_body(request);
  g_return_val_if_fail (bufp, 0);
  DEBUG ("%s\n", bufp);

  /* skip paste whitespace */
  bufp += strspn (bufp, " \t\f\n\r\v\b");

  /* see if this really appears to be gnc-xml content ... */
  if (strncmp (bufp, "<?xml version", 13)) 
  {
    PERR ("bogus file content, file was:\n%s", bufp);
    be->be.last_err = ERR_NETIO_NOT_GNCXML;
    return 0;
  }

  /* if we got to here, the response looks good.
   * if there is a cookie in the header, obey it 
   */
  bufp = ghttp_get_header(request, "Set-Cookie");
  if (bufp) 
  {
    if (be->auth_cookie) g_free (be->auth_cookie);
    be->auth_cookie = g_strdup (bufp);
  }
    
  /* must be good */
  return len;
}

/* ==================================================================== */

static void 
setup_request (XMLBackend *be)
{
  ghttp_request *request = be->request;

  /* clean is needed to clear out old request bodies, headers, etc. */
  ghttp_clean (request);
  ghttp_set_header (be->request, http_hdr_Connection, "close");
  ghttp_set_header (be->request, http_hdr_User_Agent, 
       "gnucash/1.5 (Financial Browser for Linux; http://gnucash.org)");
  ghttp_set_sync (be->request, ghttp_sync);

  if (be->auth_cookie) {
    ghttp_set_header (request, "Cookie", be->auth_cookie);
  }
}

/* ==================================================================== */
/* Load a set of accounts and currencies from the indicated URL. */

static AccountGroup *
xmlbeBookLoad (GNCBook *book, const char *url) 
{
  XMLBackend *be;
  AccountGroup *grp;
  ghttp_request *request;
  const char *bufp;
  int len;

  if (!book) return NULL;

  ENTER ("url is %s", url);

  be = (XMLBackend *) xaccGNCBookGetBackend (book); 

  /* hack alert -- we store this first url as some bogus url 
   * for sending queries to 
   * this should be made customizable, I suppose ???? */
  be->query_url = g_strdup (url);

  /* build up a request for the URL */
  setup_request (be);
  request = be->request;
  ghttp_set_uri (request, (char *) url);
  ghttp_set_type (request, ghttp_type_get);
  ghttp_prepare (request);
  ghttp_process (request);

  /* perform various error and validity checking on the response */
  len = check_response (be);
  if (0 >= len) return NULL;

  bufp = ghttp_get_body(request);
  grp = gncxml_read_from_buf (bufp, len);

  LEAVE(" ");
  return grp;
}

/* ==================================================================== */

static void
xmlbeRunQuery (Backend *b, Query *q) 
{
  XMLBackend *be = (XMLBackend *) b;
  AccountGroup *grp, *reply_grp;
  ghttp_request *request;
  char *bufp;
  int len;

  if (!be || !q) return;
  ENTER ("be=%p q=%p", b, q);

  /* set up a new http request, of type POST */
  setup_request (be);
  request = be->request;
  ghttp_set_uri (request, be->query_url);
  ghttp_set_type (request, ghttp_type_post);

  /* convert the query to XML */
  gncxml_write_query_to_buf (q, &bufp, &len);

  /* put the XML into the request body */
  ghttp_set_body (request, bufp, len);

  /* send it off the the webserver, wait for the reply */
  ghttp_prepare (request);
  ghttp_process (request);

  /* free the query xml */
  free (bufp);

  /* perform various error and validity checking on the response */
  len = check_response (be);
  if (0 >= len) return;
 
  /* we get back a list of splits */
  bufp = ghttp_get_body(request);
  reply_grp = gncxml_read_from_buf (bufp, len);

  /* merge the splits into our local cache */
  grp = xaccQueryGetGroup (q);
  xaccGroupConcatGroup (grp, reply_grp);
  xaccGroupMergeAccounts (grp);
  xaccFreeAccountGroup (reply_grp);
     
  LEAVE(" ");
  return;
}

/* ==================================================================== */

static void
xmlbeBookEnd (GNCBook *book) 
{
  XMLBackend *be;
  be = (XMLBackend *) xaccGNCBookGetBackend (book); 

  ghttp_request_destroy (be->request);
  g_free (be->query_url);
}

/* ==================================================================== */

Backend *
xmlendNew (void)
{
  XMLBackend *be;

  be = (XMLBackend *) malloc (sizeof (XMLBackend));

  /* generic backend handlers */
  be->be.book_load = xmlbeBookLoad;
  be->be.book_end = xmlbeBookEnd;

  be->be.account_begin_edit = NULL;
  be->be.account_commit_edit = NULL;
  be->be.trans_begin_edit = NULL;
  be->be.trans_commit_edit = NULL;
  be->be.trans_rollback_edit = NULL;
  be->be.run_query = xmlbeRunQuery;

  be->be.last_err = ERR_BACKEND_NONE;

  be->request = ghttp_request_new();
  be->auth_cookie = NULL;

  be->query_url = NULL;

  return (Backend *) be;
}


/* ============================== END OF FILE ======================== */
