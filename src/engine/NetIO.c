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
 * ultra super reudimentary right now 
 * this will be very very different in final verisn
 */


#include <ghttp.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "NetIO.h"
#include "gnc-engine-util.h"
#include "io-gncxml.h"

static short module = MOD_IO;

/* ==================================================================== */

AccountGroup *
xaccRecvAccountGroup (char *url) 
{
  AccountGroup *grp;
  ghttp_request *request;
  char *bufp;
  int len;

  ENTER ("url is %s\n", url);
  request = ghttp_request_new();
  ghttp_set_uri (request, url);
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
     char * errstr = ghttp_get_error (request);
     char * reason = ghttp_reason_phrase (request);
     PERR ("connection failed: %s %s\n", errstr, reason);

     ghttp_request_destroy(request);
     return NULL;
  }

  ghttp_request_destroy(request);

  LEAVE("\n");
  return NULL;
}

/* ============================== END OF FILE ======================== */
