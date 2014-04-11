/********************************************************************\
 * gnc-address-xml-v2.c -- address xml i/o implementation           *
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

#include "gnc-address-xml-v2.h"

static QofLogModule log_module = GNC_MOD_IO;

const gchar *address_version_string = "2.0.0";

/* ids */
#define addr_name_string	"addr:name"
#define addr_addr1_string	"addr:addr1"
#define addr_addr2_string	"addr:addr2"
#define addr_addr3_string	"addr:addr3"
#define addr_addr4_string	"addr:addr4"
#define addr_phone_string	"addr:phone"
#define addr_fax_string		"addr:fax"
#define addr_email_string	"addr:email"
#define addr_slots_string	"addr:slots"

static void
maybe_add_string (xmlNodePtr ptr, const char *tag, const char *str)
{
  if (str && strlen(str) > 0)
    xmlAddChild (ptr, text_to_dom_tree (tag, str));
}

xmlNodePtr
gnc_address_to_dom_tree (const char *tag, GncAddress *addr)
{
    xmlNodePtr ret;

    ret = xmlNewNode(NULL, BAD_CAST tag);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST address_version_string);

    maybe_add_string (ret, addr_name_string, gncAddressGetName (addr));
    
    maybe_add_string (ret, addr_addr1_string, gncAddressGetAddr1 (addr));
    maybe_add_string (ret, addr_addr2_string, gncAddressGetAddr2 (addr));
    maybe_add_string (ret, addr_addr3_string, gncAddressGetAddr3 (addr));
    maybe_add_string (ret, addr_addr4_string, gncAddressGetAddr4 (addr));

    maybe_add_string (ret, addr_phone_string, gncAddressGetPhone (addr));
    maybe_add_string (ret, addr_fax_string, gncAddressGetFax (addr));
    maybe_add_string (ret, addr_email_string, gncAddressGetEmail (addr));

    return ret;
}

/***********************************************************************/

struct address_pdata
{
  GncAddress *address;
};

static gboolean
set_string(xmlNodePtr node, GncAddress* addr,
           void (*func)(GncAddress *addr, const char *txt))
{
    gchar* txt = dom_tree_to_text(node);
    g_return_val_if_fail(txt, FALSE);
    
    func(addr, txt);

    g_free(txt);
    
    return TRUE;
}

static gboolean
address_name_handler (xmlNodePtr node, gpointer addr_pdata)
{
    struct address_pdata *pdata = addr_pdata;

    return set_string(node, pdata->address, gncAddressSetName);
}

static gboolean
address_addr1_handler (xmlNodePtr node, gpointer addr_pdata)
{
    struct address_pdata *pdata = addr_pdata;

    return set_string(node, pdata->address, gncAddressSetAddr1);
}

static gboolean
address_addr2_handler (xmlNodePtr node, gpointer addr_pdata)
{
    struct address_pdata *pdata = addr_pdata;

    return set_string(node, pdata->address, gncAddressSetAddr2);
}

static gboolean
address_addr3_handler (xmlNodePtr node, gpointer addr_pdata)
{
    struct address_pdata *pdata = addr_pdata;

    return set_string(node, pdata->address, gncAddressSetAddr3);
}

static gboolean
address_addr4_handler (xmlNodePtr node, gpointer addr_pdata)
{
    struct address_pdata *pdata = addr_pdata;

    return set_string(node, pdata->address, gncAddressSetAddr4);
}

static gboolean
address_phone_handler (xmlNodePtr node, gpointer addr_pdata)
{
    struct address_pdata *pdata = addr_pdata;

    return set_string(node, pdata->address, gncAddressSetPhone);
}

static gboolean
address_fax_handler (xmlNodePtr node, gpointer addr_pdata)
{
    struct address_pdata *pdata = addr_pdata;

    return set_string(node, pdata->address, gncAddressSetFax);
}

static gboolean
address_email_handler (xmlNodePtr node, gpointer addr_pdata)
{
    struct address_pdata *pdata = addr_pdata;

    return set_string(node, pdata->address, gncAddressSetEmail);
}

static gboolean
address_slots_handler (xmlNodePtr node, gpointer addr_pdata)
{
  return TRUE;
}

static struct dom_tree_handler address_handlers_v2[] = {
    { addr_name_string, address_name_handler, 0, 0 },
    { addr_addr1_string, address_addr1_handler, 0, 0 },
    { addr_addr2_string, address_addr2_handler, 0, 0 },
    { addr_addr3_string, address_addr3_handler, 0, 0 },
    { addr_addr4_string, address_addr4_handler, 0, 0 },
    { addr_phone_string, address_phone_handler, 0, 0 },
    { addr_fax_string, address_fax_handler, 0, 0 },
    { addr_email_string, address_email_handler, 0, 0 },
    { addr_slots_string, address_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

gboolean
gnc_dom_tree_to_address (xmlNodePtr node, GncAddress *address)
{
    struct address_pdata addr_pdata;
    gboolean successful;

    addr_pdata.address = address;

    successful = dom_tree_generic_parse (node, address_handlers_v2,
                                         &addr_pdata);

    if (!successful)
    {
        PERR ("failed to parse address tree");
    }

    return successful;
}

static void
address_ns(FILE *out)
{
  g_return_if_fail(out);
  gnc_xml2_write_namespace_decl(out, "addr");
}

void
gnc_address_xml_initialize (void)
{
  static GncXmlDataType_t be_data = {
    GNC_FILE_BACKEND_VERS,
    "gnc:Address",
    NULL,			/* parser_create */
    NULL,			/* add_item */
    NULL,			/* get_count */
    NULL,			/* write */
    NULL,			/* scrub */
    address_ns,
  };

  qof_object_register_backend ("gnc:Address",
			    GNC_FILE_BACKEND,
			    &be_data);
}
