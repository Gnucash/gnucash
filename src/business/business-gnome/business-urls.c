/*
 * business-urls.c -- Initialize HTML for business code
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2002 Derek Atkins
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
 * Boston, MA  02111-1307,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gnome.h>

#include "gnc-html.h"
#include "gnc-ui-util.h"
#include "gnc-engine-util.h"

#include "gncCustomer.h"
#include "gncVendor.h"
#include "gncEmployee.h"
#include "gncInvoice.h"

#include "business-urls.h"
#include "dialog-customer.h"
#include "dialog-employee.h"
#include "dialog-vendor.h"
#include "dialog-invoice.h"

static gboolean
customerCB (const char *location, const char *label,
	   gboolean new_window, GNCURLResult * result)
{
  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="...:guid=<guid>" */
  if (strncmp ("guid=", location, 5) == 0) {
    GUID guid;
    GNCIdType id_type;
    GncCustomer *customer;

    if (!string_to_guid (location + 5, &guid)) {
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
      return FALSE;
    }

    id_type = xaccGUIDType (&guid, gnc_get_current_book ());
    if (id_type == GNC_ID_NONE || !safe_strcmp (id_type, GNC_ID_NULL))
    {
      result->error_message = g_strdup_printf (_("No such entity: %s"),
					       location);
      return FALSE;
    }
    else if (!safe_strcmp (id_type, GNC_CUSTOMER_MODULE_NAME))
    {
      customer = gncCustomerLookup (gnc_get_current_book (), &guid);
      gnc_ui_customer_edit (customer);
    }
    else
    {
      result->error_message =
	g_strdup_printf (_("Entity type does not match Customer: %s"),
			 location);
      return FALSE;
    }
  }
  else
  {
    result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                             location);
    return FALSE;
  }

  return TRUE;
}

static gboolean
vendorCB (const char *location, const char *label,
	   gboolean new_window, GNCURLResult * result)
{
  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="...:guid=<guid>" */
  if (strncmp ("guid=", location, 5) == 0) {
    GUID guid;
    GNCIdType id_type;
    GncVendor *vendor;

    if (!string_to_guid (location + 5, &guid)) {
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
      return FALSE;
    }

    id_type = xaccGUIDType (&guid, gnc_get_current_book ());
    if (id_type == GNC_ID_NONE || !safe_strcmp (id_type, GNC_ID_NULL))
    {
      result->error_message = g_strdup_printf (_("No such entity: %s"),
					       location);
      return FALSE;
    }
    else if (!safe_strcmp (id_type, GNC_VENDOR_MODULE_NAME))
    {
      vendor = gncVendorLookup (gnc_get_current_book (), &guid);
      gnc_ui_vendor_edit (vendor);
    }
    else
    {
      result->error_message =
	g_strdup_printf (_("Entity type does not match Vendor: %s"),
			 location);
      return FALSE;
    }
  }
  else
  {
    result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                             location);
    return FALSE;
  }

  return TRUE;
}

static gboolean
employeeCB (const char *location, const char *label,
	   gboolean new_window, GNCURLResult * result)
{
  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="...:guid=<guid>" */
  if (strncmp ("guid=", location, 5) == 0) {
    GUID guid;
    GNCIdType id_type;
    GncEmployee *employee;

    if (!string_to_guid (location + 5, &guid)) {
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
      return FALSE;
    }

    id_type = xaccGUIDType (&guid, gnc_get_current_book ());
    if (id_type == GNC_ID_NONE || !safe_strcmp (id_type, GNC_ID_NULL))
    {
      result->error_message = g_strdup_printf (_("No such entity: %s"),
					       location);
      return FALSE;
    }
    else if (!safe_strcmp (id_type, GNC_EMPLOYEE_MODULE_NAME))
    {
      employee = gncEmployeeLookup (gnc_get_current_book (), &guid);
      gnc_ui_employee_edit (employee);
    }
    else
    {
      result->error_message =
	g_strdup_printf (_("Entity type does not match Employee: %s"),
			 location);
      return FALSE;
    }
  }
  else
  {
    result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                             location);
    return FALSE;
  }

  return TRUE;
}

static gboolean
invoiceCB (const char *location, const char *label,
	   gboolean new_window, GNCURLResult * result)
{
  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="...:guid=<guid>" */
  if (strncmp ("guid=", location, 5) == 0) {
    GUID guid;
    GNCIdType id_type;
    GncInvoice *invoice;

    if (!string_to_guid (location + 5, &guid)) {
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
      return FALSE;
    }

    id_type = xaccGUIDType (&guid, gnc_get_current_book ());
    if (id_type == GNC_ID_NONE || !safe_strcmp (id_type, GNC_ID_NULL))
    {
      result->error_message = g_strdup_printf (_("No such entity: %s"),
					       location);
      return FALSE;
    }
    else if (!safe_strcmp (id_type, GNC_INVOICE_MODULE_NAME))
    {
      invoice = gncInvoiceLookup (gnc_get_current_book (), &guid);
      gnc_ui_invoice_edit (invoice);
    }
    else
    {
      result->error_message =
	g_strdup_printf (_("Entity type does not match Invoice: %s"),
			 location);
      return FALSE;
    }
  }
  else
  {
    result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                             location);
    return FALSE;
  }

  return TRUE;
}

static gboolean
ownerreportCB (const char *location, const char *label,
	       gboolean new_window, GNCURLResult * result)
{
  const char *ownerptr;
  const char *acctptr;
  GUID guid;
  GncOwner owner;
  GNCIdType id_type;
  GncOwnerType type;
  char *etype = NULL;
  Account *acc = NULL;

  g_return_val_if_fail (location != NULL, FALSE);
  g_return_val_if_fail (result != NULL, FALSE);

  result->load_to_stream = FALSE;

  /* href="...:owner=<owner-type>:guid=<guid>[&acct=<guid>]" */
  
  acctptr = index (location, '&');
  if (acctptr)
    acctptr++;

  if (strncmp ("owner=", location, 6) != 0) {
    result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                             location);
    return FALSE;
  }

  memset (&owner, 0, sizeof (owner));
    
  ownerptr = location+6;
  switch (*ownerptr) {
  case 'c':
    type = GNC_OWNER_CUSTOMER;
    break;
  case 'v':
    type = GNC_OWNER_VENDOR;
    break;
  case 'e':
    type = GNC_OWNER_EMPLOYEE;
    break;
  default:
    result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
    return FALSE;
  }

  if (!string_to_guid (ownerptr+2, &guid)) {
    result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
    return FALSE;
  }

  id_type = xaccGUIDType (&guid, gnc_get_current_book ());
  if (id_type == GNC_ID_NONE || !safe_strcmp (id_type, GNC_ID_NULL))
  {
    result->error_message = g_strdup_printf (_("No such owner entity: %s"),
					     location);
    return FALSE;
  }

  switch (type) {
  case GNC_OWNER_CUSTOMER:
    if (!safe_strcmp (id_type, GNC_CUSTOMER_MODULE_NAME))
      gncOwnerInitCustomer (&owner,
			    gncCustomerLookup (gnc_get_current_book (),
					       &guid));
    etype = "Customer";
    break;
  case GNC_OWNER_VENDOR:
    if (!safe_strcmp (id_type, GNC_VENDOR_MODULE_NAME))
      gncOwnerInitVendor (&owner,
			  gncVendorLookup (gnc_get_current_book (),
					   &guid));
    etype = "Vendor";
    break;
  case GNC_OWNER_EMPLOYEE:
    if (!safe_strcmp (id_type, GNC_EMPLOYEE_MODULE_NAME))
      gncOwnerInitEmployee (&owner,
			  gncEmployeeLookup (gnc_get_current_book (),
					     &guid));
    etype = "Employee";
    break;
  default:
    etype = "OTHER";
  }

  if (owner.owner.undefined == NULL)
  {
    result->error_message =
      g_strdup_printf (_("Entity type does not match %s: %s"),
		       etype, location);
    return FALSE;
  }

  /* Deal with acctptr, if it exists */
  if (acctptr)
  {
    if (strncmp ("acct=", acctptr, 5) != 0)
    {
      result->error_message = g_strdup_printf (_("Bad URL %s"), location);
      return FALSE;
    }

    if (!string_to_guid (acctptr+5, &guid)) {
      result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
      return FALSE;
    }

    id_type = xaccGUIDType (&guid, gnc_get_current_book ());
    if (id_type == GNC_ID_NONE || !safe_strcmp (id_type, GNC_ID_NULL))
    {
      result->error_message = g_strdup_printf (_("No such Account entity: %s"),
					       location);
      return FALSE;
    }

    if (safe_strcmp (id_type, GNC_ID_ACCOUNT) != 0)
    {
      result->error_message =
	g_strdup_printf (_("Entity is not Account entity: %s"), location);
      return FALSE;
    }

    acc = xaccAccountLookup (&guid, gnc_get_current_book ());
  }

  /* Ok, let's run this report */
  gnc_business_call_owner_report (&owner, acc);

  return TRUE;
}

void
gnc_business_urls_initialize (void)
{
  int i;
  static struct {
    URLType	urltype;
    char *	protocol;
    GncHTMLUrlCB handler;
  } types[] = {
    { GNC_CUSTOMER_MODULE_NAME, GNC_CUSTOMER_MODULE_NAME, customerCB },
    { GNC_VENDOR_MODULE_NAME, GNC_VENDOR_MODULE_NAME, vendorCB },
    { GNC_EMPLOYEE_MODULE_NAME, GNC_EMPLOYEE_MODULE_NAME, employeeCB },
    { GNC_INVOICE_MODULE_NAME, GNC_INVOICE_MODULE_NAME, invoiceCB },
    { URL_TYPE_OWNERREPORT, "gnc-ownerreport", ownerreportCB },
    { NULL, NULL }
  };

  for (i = 0; types[i].urltype; i++)
    gnc_html_register_urltype (types[i].urltype, types[i].protocol);

  for (i = 0; types[i].urltype; i++)
    if (types[i].handler)
      gnc_html_register_url_handler (types[i].urltype, types[i].handler);

}
