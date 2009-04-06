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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
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
#include "gncEmployeeP.h"

static QofLogModule log_module = GNC_MOD_IO;

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
      type_str = GNC_ID_CUSTOMER;
      break;
    case GNC_OWNER_JOB:
      type_str = GNC_ID_JOB;
      break;
    case GNC_OWNER_VENDOR:
      type_str = GNC_ID_VENDOR;
      break;
    case GNC_OWNER_EMPLOYEE:
      type_str = GNC_ID_EMPLOYEE;
      break;
    default:
      PWARN ("Invalid owner type: %d", gncOwnerGetType (owner));
      return NULL;
    }

    ret = xmlNewNode(NULL, BAD_CAST tag);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST owner_version_string);

    xmlAddChild (ret, text_to_dom_tree (owner_type_string, type_str));
    xmlAddChild (ret, guid_to_dom_tree (owner_id_string,
					gncOwnerGetGUID (owner)));

    return ret;
}

/***********************************************************************/

struct owner_pdata
{
  GncOwner *owner;
  QofBook *book;
};

static gboolean
owner_type_handler (xmlNodePtr node, gpointer owner_pdata)
{
  struct owner_pdata *pdata = owner_pdata;
  char* txt = dom_tree_to_text(node);
  g_return_val_if_fail(txt, FALSE);

  if (!safe_strcmp (txt, GNC_ID_CUSTOMER))
    gncOwnerInitCustomer (pdata->owner, NULL);
  else if (!safe_strcmp (txt, GNC_ID_JOB))
    gncOwnerInitJob (pdata->owner, NULL);
  else if (!safe_strcmp (txt, GNC_ID_VENDOR))
    gncOwnerInitVendor (pdata->owner, NULL);
  else if (!safe_strcmp (txt, GNC_ID_EMPLOYEE))
    gncOwnerInitEmployee (pdata->owner, NULL);
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
  case GNC_OWNER_EMPLOYEE:
  {
    GncEmployee *employee = gncEmployeeLookup (pdata->book, guid);
    if (!employee) {
      employee = gncEmployeeCreate (pdata->book);
      gncEmployeeSetGUID (employee, guid);
    }
    gncOwnerInitEmployee (pdata->owner, employee);
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
gnc_dom_tree_to_owner (xmlNodePtr node, GncOwner *owner, QofBook *book)
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

static void
owner_ns(FILE *out)
{
  g_return_if_fail(out);
  gnc_xml2_write_namespace_decl(out, "owner");
}

void
gnc_owner_xml_initialize (void)
{
  static GncXmlDataType_t be_data = {
    GNC_FILE_BACKEND_VERS,
    "gnc:Owner",
    NULL,			/* parser_create */
    NULL,			/* add_item */
    NULL,			/* get_count */
    NULL,			/* write */
    NULL,			/* scrub */
    owner_ns,
  };

  qof_object_register_backend ("gnc:Owner",
			    GNC_FILE_BACKEND,
			    &be_data);
}
