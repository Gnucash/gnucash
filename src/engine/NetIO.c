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
 * ultra super rudimentary right now 

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

static short module = MOD_IO;

typedef struct _xmlend XMLBackend;

struct _xmlend {
  Backend be;

  ghttp_request *request;
  char * query_url;
};

Backend *xmlendNew (void);


/* ==================================================================== */
/* Load a set of accounts and currencies from the indicated URL. */

static AccountGroup *
xmlbeBookLoad (GNCBook *book, const char *url) 
{
  XMLBackend *be;
  AccountGroup *grp;
  ghttp_request *request;
  char *bufp;
  int len;

  if (!book) return NULL;

  ENTER ("url is %s\n", url);

  be = (XMLBackend *) xaccGNCBookGetBackend (book); 

  /* hack alert -- some bogus url for sending queries to */
  /* this should be made customizable, I suppose ???? */
  be->query_url = g_strdup (url);

  request = be->request;
  ghttp_set_uri (request, (char *) url);
  ghttp_set_type (request, ghttp_type_get);
  ghttp_set_header (request, http_hdr_Connection, "close");
  ghttp_set_sync (request, ghttp_sync);
  ghttp_clean (request);
  ghttp_prepare (request);
  ghttp_process (request);

  len = ghttp_get_body_len(request);

  if (0 < len)
  {
     bufp = ghttp_get_body(request);
     PINFO ("reply length=%d\n", len);
     DEBUG ("%s\n", bufp);
     grp = gncxml_read_from_buf (bufp, len);
     return grp;
  }
  else 
  {
     const char * errstr = ghttp_get_error (request);
     const char * reason = ghttp_reason_phrase (request);
     PERR ("connection failed: %s %s\n", errstr, reason);
     return NULL;
  }


  LEAVE("\n");
  return NULL;
}

/* ==================================================================== */

static int
xmlbeRunQuery (Backend *b, Query *q) 
{
  XMLBackend *be = (XMLBackend *) b;
  ghttp_request *request;
  char *bufp;
  int len;

  if (!be || !q) return 999;

  /* set up a new http request, of type POST */
  request = ghttp_request_new();
  ghttp_set_uri (request, be->query_url);
  ghttp_set_type (request, ghttp_type_post);
  ghttp_set_header (request, http_hdr_Connection, "close");
  ghttp_set_sync (request, ghttp_sync);
  ghttp_clean (request);

  /* convert the query to XML */
  gncxml_write_query_to_buf (q, &bufp, &len);

  /* put the XML into the request body */
  ghttp_set_body (request, bufp, len);

  /* send it off the the webserver, wait for the reply */
  ghttp_prepare (request);
  ghttp_process (request);

  /* free the query xml */
  free (bufp);

  len = ghttp_get_body_len(request);

  if (0 < len)
  {
     bufp = ghttp_get_body(request);
     PINFO ("reply length=%d\n", len);
     DEBUG ("%s\n", bufp);

     /* we got back a list of splits, these need to be merged in */
     // grp = gncxml_read_from_buf (bufp, len);
     return 0;
  }
  else 
  {
     const char * errstr = ghttp_get_error (request);
     const char * reason = ghttp_reason_phrase (request);
     PERR ("connection failed: %s %s\n", errstr, reason);

     return 444;
  }


  LEAVE("\n");
  return 0;
}

/* ==================================================================== */
#if 0

xmlbeBookEnd () 
{
  ghttp_request_destroy (be->request);
  g_free (be->query_url);
}
#endif

/* ==================================================================== */

Backend *
xmlendNew (void)
{
  XMLBackend *be;
  be = (XMLBackend *) malloc (sizeof (XMLBackend));

  /* generic backend handlers */
  be->be.book_load = xmlbeBookLoad;
  be->be.book_end = NULL;

  be->be.account_begin_edit = NULL;
  be->be.account_commit_edit = NULL;
  be->be.trans_begin_edit = NULL;
  be->be.trans_commit_edit = NULL;
  be->be.trans_rollback_edit = NULL;
  be->be.run_query = xmlbeRunQuery;

  be->request = ghttp_request_new();
  be->query_url = NULL;

  return (Backend *) be;
}


/* ============================== END OF FILE ======================== */
