/********************************************************************\
 * gnc-employee-xml-v2.c -- employee xml i/o implementation         *
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

#include "gncEmployeeP.h"
#include "gnc-employee-xml-v2.h"
#include "gnc-address-xml-v2.h"

#define _GNC_MOD_NAME	GNC_ID_EMPLOYEE

static QofLogModule log_module = GNC_MOD_IO;

const gchar *employee_version_string = "2.0.0";

/* ids */
#define gnc_employee_string "gnc:GncEmployee"
#define employee_username_string "employee:username"
#define employee_guid_string "employee:guid"
#define employee_id_string "employee:id"
#define employee_addr_string "employee:addr"
#define employee_language_string "employee:language"
#define employee_acl_string "employee:acl"
#define employee_active_string "employee:active"
#define employee_workday_string "employee:workday"
#define employee_rate_string "employee:rate"
#define employee_currency_string "employee:currency"
#define employee_ccard_string "employee:ccard"
#define employee_slots_string "employee:slots"

static void
maybe_add_string (xmlNodePtr ptr, const char *tag, const char *str)
{
  if (str && strlen(str) > 0)
    xmlAddChild (ptr, text_to_dom_tree (tag, str));
}

static xmlNodePtr
employee_dom_tree_create (GncEmployee *employee)
{
    xmlNodePtr ret, kvpnode;
    gnc_numeric num;
    Account* ccard_acc;

    ret = xmlNewNode(NULL, BAD_CAST gnc_employee_string);
    xmlSetProp(ret, BAD_CAST "version", BAD_CAST employee_version_string);

    xmlAddChild(ret, guid_to_dom_tree(employee_guid_string,
				      qof_instance_get_guid(QOF_INSTANCE (employee))));

    xmlAddChild(ret, text_to_dom_tree(employee_username_string,
                                      gncEmployeeGetUsername (employee)));
    
    xmlAddChild(ret, text_to_dom_tree(employee_id_string,
                                      gncEmployeeGetID (employee)));

    xmlAddChild(ret, gnc_address_to_dom_tree(employee_addr_string,
					     gncEmployeeGetAddr (employee)));
    
    maybe_add_string (ret, employee_language_string,
		      gncEmployeeGetLanguage (employee));
    maybe_add_string (ret, employee_acl_string, gncEmployeeGetAcl (employee));

    xmlAddChild(ret, int_to_dom_tree(employee_active_string,
				     gncEmployeeGetActive (employee)));

    num = gncEmployeeGetWorkday (employee);
    xmlAddChild(ret, gnc_numeric_to_dom_tree (employee_workday_string, &num));

    num = gncEmployeeGetRate (employee);
    xmlAddChild(ret, gnc_numeric_to_dom_tree (employee_rate_string, &num));

    xmlAddChild
      (ret,
       commodity_ref_to_dom_tree(employee_currency_string,
				 gncEmployeeGetCurrency (employee)));

    ccard_acc = gncEmployeeGetCCard (employee);
    if (ccard_acc)
      xmlAddChild(ret, guid_to_dom_tree(employee_ccard_string,
					qof_instance_get_guid(QOF_INSTANCE(ccard_acc))));

    kvpnode = kvp_frame_to_dom_tree (employee_slots_string,
				     qof_instance_get_slots (QOF_INSTANCE(employee)));
    if (kvpnode) xmlAddChild (ret, kvpnode);

    return ret;
}

/***********************************************************************/

struct employee_pdata
{
  GncEmployee *employee;
  QofBook *book;
};

static gboolean
set_string(xmlNodePtr node, GncEmployee* employee,
           void (*func)(GncEmployee *employee, const char *txt))
{
  char* txt = dom_tree_to_text(node);
  g_return_val_if_fail(txt, FALSE);
    
  func(employee, txt);
  
  g_free(txt);
    
  return TRUE;
}

static gboolean
employee_username_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;

    return set_string(node, pdata->employee, gncEmployeeSetUsername);
}

static gboolean
employee_guid_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;
    GUID *guid;
    GncEmployee *employee;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail(guid, FALSE);

    /* See if we've already created this one */
    employee = gncEmployeeLookup (pdata->book, guid);
    if (employee) {
      gncEmployeeDestroy (pdata->employee);
      pdata->employee = employee;
      gncEmployeeBeginEdit (employee);
    } else {
      gncEmployeeSetGUID(pdata->employee, guid);
    }

    g_free(guid);
    
    return TRUE;
}

static gboolean
employee_id_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;

    return set_string(node, pdata->employee, gncEmployeeSetID);
}

static gboolean
employee_language_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;

    return set_string(node, pdata->employee, gncEmployeeSetLanguage);
}

static gboolean
employee_acl_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;

    return set_string(node, pdata->employee, gncEmployeeSetAcl);
}

static gboolean
employee_addr_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;

    return gnc_dom_tree_to_address (node, gncEmployeeGetAddr(pdata->employee));
}

static gboolean
employee_active_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;
    gint64 val;
    gboolean ret;

    ret = dom_tree_to_integer(node, &val);
    if (ret)
      gncEmployeeSetActive(pdata->employee, (gboolean)val);

    return ret;
}

static gboolean
employee_workday_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;
    gnc_numeric *val;

    val = dom_tree_to_gnc_numeric(node);
    g_return_val_if_fail(val, FALSE);
    gncEmployeeSetWorkday(pdata->employee, *val);
    g_free(val);

    return TRUE;
}

static gboolean
employee_rate_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;
    gnc_numeric *val;

    val = dom_tree_to_gnc_numeric(node);
    g_return_val_if_fail(val, FALSE);
    gncEmployeeSetRate(pdata->employee, *val);
    g_free(val);

    return TRUE;
}

static gboolean
employee_currency_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;
    gnc_commodity *com;

    com = dom_tree_to_commodity_ref(node, pdata->book);
    g_return_val_if_fail (com, FALSE);

    gncEmployeeSetCurrency (pdata->employee, com);

    return TRUE;
}

static gboolean
employee_ccard_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;
    GUID *guid;
    Account *ccard_acc;

    guid = dom_tree_to_guid(node);
    g_return_val_if_fail(guid, FALSE);

    ccard_acc = xaccAccountLookup (guid, pdata->book);
    g_free(guid);

    g_return_val_if_fail (ccard_acc, FALSE);
    gncEmployeeSetCCard (pdata->employee, ccard_acc);

    return TRUE;
}

static gboolean
employee_slots_handler (xmlNodePtr node, gpointer employee_pdata)
{
    struct employee_pdata *pdata = employee_pdata;
    return dom_tree_to_kvp_frame_given (
	node, qof_instance_get_slots (QOF_INSTANCE(pdata->employee)));
}

static struct dom_tree_handler employee_handlers_v2[] = {
    { employee_username_string, employee_username_handler, 1, 0 },
    { employee_guid_string, employee_guid_handler, 1, 0 },
    { employee_id_string, employee_id_handler, 1, 0 },
    { employee_addr_string, employee_addr_handler, 1, 0 },
    { employee_language_string, employee_language_handler, 0, 0 },
    { employee_acl_string, employee_acl_handler, 0, 0 },
    { employee_active_string, employee_active_handler, 1, 0 },
    { employee_workday_string, employee_workday_handler, 1, 0 },
    { employee_rate_string, employee_rate_handler, 1, 0 },
    { employee_currency_string, employee_currency_handler, 0, 0 }, /* XXX */
    { "employee:commodity", employee_currency_handler, 0, 0 }, /* XXX */
    { employee_ccard_string, employee_ccard_handler, 0, 0 },
    { employee_slots_string, employee_slots_handler, 0, 0 },
    { NULL, 0, 0, 0 }
};

static GncEmployee*
dom_tree_to_employee (xmlNodePtr node, QofBook *book)
{
    struct employee_pdata employee_pdata;
    gboolean successful;

    employee_pdata.employee = gncEmployeeCreate(book);
    employee_pdata.book = book;
    gncEmployeeBeginEdit (employee_pdata.employee);

    successful = dom_tree_generic_parse (node, employee_handlers_v2,
                                         &employee_pdata);
    if (successful)
      gncEmployeeCommitEdit (employee_pdata.employee);
    else
    {
        PERR ("failed to parse employee tree");
        gncEmployeeDestroy (employee_pdata.employee);
        employee_pdata.employee = NULL;
    }

    return employee_pdata.employee;
}

static gboolean
gnc_employee_end_handler(gpointer data_for_children,
			 GSList* data_from_children, GSList* sibling_data,
			 gpointer parent_data, gpointer global_data,
			 gpointer *result, const gchar *tag)
{
    int successful;
    GncEmployee *employee;
    xmlNodePtr tree = (xmlNodePtr)data_for_children;
    gxpf_data *gdata = (gxpf_data*)global_data;
    QofBook *book = gdata->bookdata;

    successful = TRUE;

    if(parent_data)
    {
        return TRUE;
    }

    /* OK.  For some messed up reason this is getting called again with a
       NULL tag.  So we ignore those cases */
    if(!tag)
    {
        return TRUE;
    }

    g_return_val_if_fail(tree, FALSE);

    employee = dom_tree_to_employee(tree, book);
    if(employee != NULL)
    {
        gdata->cb(tag, gdata->parsedata, employee);
    }

    xmlFreeNode(tree);

    return employee != NULL;
}

static sixtp *
employee_sixtp_parser_create(void)
{
  return sixtp_dom_parser_new(gnc_employee_end_handler, NULL, NULL);
}

static gboolean
employee_should_be_saved (GncEmployee *employee)
{
  const char *id;

  /* make sure this is a valid employee before we save it -- should have an ID */
  id = gncEmployeeGetID (employee);
  if (id == NULL || *id == '\0')
    return FALSE;

  return TRUE;
}

static void
do_count (QofInstance * employee_p, gpointer count_p)
{
  int *count = count_p;
  if (employee_should_be_saved ((GncEmployee *) employee_p))
    (*count)++;
}

static int
employee_get_count (QofBook *book)
{
  int count = 0;
  qof_object_foreach (_GNC_MOD_NAME, book, do_count, (gpointer) &count);
  return count;
}

static void
xml_add_employee (QofInstance * employee_p, gpointer out_p)
{
  xmlNodePtr node;
  GncEmployee *employee = (GncEmployee *) employee_p;
  FILE *out = out_p;

  if (!employee_should_be_saved (employee))
    return;

  node = employee_dom_tree_create (employee);
  xmlElemDump(out, NULL, node);
  fprintf(out, "\n");
  xmlFreeNode (node);
}

static void
employee_write (FILE *out, QofBook *book)
{
  qof_object_foreach (_GNC_MOD_NAME, book, xml_add_employee, (gpointer) out);
}

static void
employee_ns(FILE *out)
{
  g_return_if_fail(out);
  gnc_xml2_write_namespace_decl(out, "employee");
}

void
gnc_employee_xml_initialize (void)
{
  static GncXmlDataType_t be_data = {
    GNC_FILE_BACKEND_VERS,
    gnc_employee_string,
    employee_sixtp_parser_create,
    NULL,			/* add_item */
    employee_get_count,
    employee_write,
    NULL,			/* scrub */
    employee_ns,
  };

  qof_object_register_backend (_GNC_MOD_NAME,
			    GNC_FILE_BACKEND,
			    &be_data);
}
