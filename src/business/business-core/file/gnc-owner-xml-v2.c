/********************************************************************\
 * gnc-owner-xml-v2.c -- owner xml i/o implementation           *
 *                                                                  *
 * Copyright (C) 2002 Derek Atkins <warlord@MIT.EDU>                *
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

#include "config.h"

#include <glib.h>
#include <stdlib.h>
#include <string.h>

#include "gnc-xml-helper.h"

#include "sixtp.h"
#include "sixtp-utils.h"
#include "sixtp-parsers.h"
#include "sixtp-utils.h"
#include "sixtp-dom-parsers.h"
#include "sixtp-dom-generators.h"

#include "gnc-xml.h"
#include "io-gncxml-gen.h"
#include "io-gncxml-v2.h"

#include "gnc-owner-xml-v2.h"
#include "gncCustomerP.h"
#include "gncJobP.h"
#include "gncVendorP.h"

#include "gnc-engine-util.h"

static short module = MOD_IO;

const gchar *owner_version_string = "2.0.0";

/* ids */
#define owner_type_string	"owner:type"
#define owner_id_string		"owner:id"

xmlNodePtr
gnc_owner_to_dom_tree (const char *tag, GncOwner *owner)
{
    xmlNodePtr ret;
    const char *type_str;

    switch (gncOwnerGetType (owner)) {
    case GNC_OWNER_CUSTOMER:
      type_str = GNC_CUSTOMER_MODULE_NAME;
      break;
    case GNC_OWNER_JOB:
      type_str = GNC_JOB_MODULE_NAME;
      break;
    case GNC_OWNER_VENDOR:
      type_str = GNC_VENDOR_MODULE_NAME;
      break;
    default:
      PWARN ("Invalid owner type: %d", gncOwnerGetType (owner));
      return NULL;
    }

    ret = xmlNewNode(NULL, tag);
    xmlSetProp(ret, "version", owner_version_string);

    xmlAddChild (ret, text_to_dom_tree (owner_type_string, type_str));
    xmlAddChild (ret, guid_to_dom_tree (owner_id_string,
					gncOwnerGetGUID (owner)));

    return ret;
}

/***********************************************************************/

struct owner_pdata
{
  GncOwner *owner;
  GNCBook *book;
};

static gboolean
owner_type_handler (xmlNodePtr node, gpointer owner_pdata)
{
  struct owner_pdata *pdata = owner_pdata;
  char* txt = dom_tree_to_text(node);
  g_return_val_if_fail(txt, FALSE);

  if (!safe_strcmp (txt, GNC_CUSTOMER_MODULE_NAME))
    gncOwnerInitCustomer (pdata->owner, NULL);
  else if (!safe_strcmp (txt, GNC_JOB_MODULE_NAME))
    gncOwnerInitJob (pdata->owner, NULL);
  else if (!safe_strcmp (txt, GNC_VENDOR_MODULE_NAME))
    gncOwnerInitVendor (pdata->owner, NULL);
  else {
    PWARN ("Unknown owner type: %s", txt);
    g_free(txt);
    return FALSE;
  }

  g_free(txt);
  return TRUE;
}

static gboolean
owner_id_handler (xmlNodePtr node, gpointer owner_pdata)
{
  struct owner_pdata *pdata = owner_pdata;
  GUID *guid;

  guid = dom_tree_to_guid(node);
  g_return_val_if_fail (guid, FALSE);

  switch (gncOwnerGetType (pdata->owner)) {
  case GNC_OWNER_CUSTOMER:
  {
    GncCustomer *cust = gncCustomerLookup (pdata->book, guid);
    if (!cust) {
      cust = gncCustomerCreate (pdata->book);
      gncCustomerSetGUID (cust, guid);
    }
    gncOwnerInitCustomer (pdata->owner, cust);
    break; 
  }
  case GNC_OWNER_JOB:
  {
    GncJob *job = gncJobLookup (pdata->book, guid);
    if (!job) {
      job = gncJobCreate (pdata->book);
      gncJobSetGUID (job, guid);
    }
    gncOwnerInitJob (pdata->owner, job);
    break; 
  }
  case GNC_OWNER_VENDOR:
  {
    GncVendor *vendor = gncVendorLookup (pdata->book, guid);
    if (!vendor) {
      vendor = gncVendorCreate (pdata->book);
      gncVendorSetGUID (vendor, guid);
    }
    gncOwnerInitVendor (pdata->owner, vendor);
    break; 
  }
  default:
    PWARN ("Invalid owner type: %d\n", gncOwnerGetType (pdata->owner));
    g_free (guid);
    return FALSE;
  }

  g_free (guid);
  return TRUE;
}

static struct dom_tree_handler owner_handlers_v2[] = {
    { owner_type_string, owner_type_handler, 1, 0 },
    { owner_id_string, owner_id_handler, 1, 0 },
    { NULL, 0, 0, 0 }
};

gboolean
gnc_dom_tree_to_owner (xmlNodePtr node, GncOwner *owner, GNCBook *book)
{
    struct owner_pdata owner_pdata;
    gboolean successful;

    owner_pdata.owner = owner;
    owner_pdata.book = book;

    successful = dom_tree_generic_parse (node, owner_handlers_v2,
                                         &owner_pdata);

    if (!successful)
    {
        PERR ("failed to parse owner tree");
    }

    return successful;
}
