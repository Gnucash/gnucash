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
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-html.h"
#include "gnc-ui-util.h"
#include "qof.h"

#include "gncCustomer.h"
#include "gncJob.h"
#include "gncVendor.h"
#include "gncEmployee.h"
#include "gncInvoice.h"

#include "business-urls.h"
#include "dialog-customer.h"
#include "dialog-employee.h"
#include "dialog-vendor.h"
#include "dialog-invoice.h"
#include "dialog-job.h"

/* Disable -Waddress.  GCC 4.2 warns (and fails to compile with -Werror) when
 * passing the address of a guid on the stack to QOF_BOOK_LOOKUP_ENTITY via
 * gncInvoiceLookup and friends.  When the macro gets inlined, the compiler
 * emits a warning that the guid null pointer test is always true.
 */
#if (__GNUC__ >= 4 && __GNUC_MINOR__ >= 2)
#    pragma GCC diagnostic warning "-Waddress"
#endif

#define HANDLE_TYPE(URL_TYPE_STR,OBJ_TYPE) {                                 \
  QofBook *book;                                                             \
  GUID guid;                                                                 \
  QofCollection *coll;                                                       \
                                                                             \
  g_return_val_if_fail (location != NULL, FALSE);                            \
  g_return_val_if_fail (result != NULL, FALSE);                              \
  result->load_to_stream = FALSE;                                            \
                                                                             \
  if (strncmp (URL_TYPE_STR, location, strlen(URL_TYPE_STR)))                \
  {                                                                          \
    result->error_message =                                                  \
                    g_strdup_printf (_("Badly formed URL %s"), location);    \
    return FALSE;                                                            \
  }                                                                          \
  if (!string_to_guid (location + strlen(URL_TYPE_STR), &guid))              \
  {                                                                          \
    result->error_message = g_strdup_printf (_("Bad URL: %s"), location);    \
    return FALSE;                                                            \
  }                                                                          \
                                                                             \
  book = gnc_get_current_book();                                             \
  coll = qof_book_get_collection (book, OBJ_TYPE);                           \
  entity = qof_collection_lookup_entity (coll, &guid);                       \
  if (NULL == entity)                                                        \
  {                                                                          \
    result->error_message = g_strdup_printf (_("No such entity: %s"),        \
                location);                                                   \
    return FALSE;                                                            \
  }                                                                          \
}

static gboolean
customerCB (const char *location, const char *label,
            gboolean new_window, GNCURLResult * result)
{
    QofInstance *entity;
    GncCustomer *customer;

    /* href="...:customer=<guid>" */
    HANDLE_TYPE ("customer=", GNC_ID_CUSTOMER);
    customer = (GncCustomer *) entity;
    gnc_ui_customer_edit (customer);

    return TRUE;
}

static gboolean
vendorCB (const char *location, const char *label,
          gboolean new_window, GNCURLResult * result)
{
    QofInstance *entity;
    GncVendor *vendor;

    /* href="...:vendor=<guid>" */
    HANDLE_TYPE ("vendor=", GNC_ID_VENDOR);
    vendor = (GncVendor *) entity;
    gnc_ui_vendor_edit (vendor);

    return TRUE;
}

static gboolean
employeeCB (const char *location, const char *label,
            gboolean new_window, GNCURLResult * result)
{
    QofInstance *entity;
    GncEmployee *employee;

    /* href="...:employee=<guid>" */
    HANDLE_TYPE ("employee=", GNC_ID_EMPLOYEE);

    employee = (GncEmployee *) entity;
    gnc_ui_employee_edit (employee);

    return TRUE;
}

static gboolean
invoiceCB (const char *location, const char *label,
           gboolean new_window, GNCURLResult * result)
{
    QofInstance *entity;
    GncInvoice *invoice;

    /* href="...:invoice=<guid>" */
    HANDLE_TYPE ("invoice=", GNC_ID_INVOICE);
    invoice = (GncInvoice *) entity;
    gnc_ui_invoice_edit (invoice);

    return TRUE;
}

static gboolean
jobCB (const char *location, const char *label,
       gboolean new_window, GNCURLResult * result)
{
    QofInstance *entity;
    GncJob *job;

    /* href="...:job=<guid>" */
    HANDLE_TYPE ("job=", GNC_ID_JOB);
    job = (GncJob *) entity;
    gnc_ui_job_edit (job);

    return TRUE;
}

/* ================================================================= */

#define RETURN_IF_NULL(inst)                                            \
  if (NULL == inst)                                                     \
  {                                                                     \
    result->error_message =                                             \
      g_strdup_printf (_("No such owner entity: %s"), location);        \
    return FALSE;                                                       \
  }

static gboolean
ownerreportCB (const char *location, const char *label,
               gboolean new_window, GNCURLResult * result)
{
    const char *ownerptr;
    const char *acctptr;
    GUID guid;
    GncOwner owner;
    GncOwnerType type;
    char *etype = NULL;
    Account *acc = NULL;

    g_return_val_if_fail (location != NULL, FALSE);
    g_return_val_if_fail (result != NULL, FALSE);

    result->load_to_stream = FALSE;

    /* href="...:owner=<owner-type>:guid=<guid>[&acct=<guid>]" */

    acctptr = strchr (location, '&');
    if (acctptr)
        acctptr++;

    if (strncmp ("owner=", location, 6) != 0)
    {
        result->error_message = g_strdup_printf (_("Badly formed URL %s"),
                                location);
        return FALSE;
    }

    memset (&owner, 0, sizeof (owner));

    ownerptr = location + 6;
    switch (*ownerptr)
    {
    case 'c':
        type = GNC_OWNER_CUSTOMER;
        break;
    case 'v':
        type = GNC_OWNER_VENDOR;
        break;
    case 'e':
        type = GNC_OWNER_EMPLOYEE;
        break;
    case 'j':
        type = GNC_OWNER_JOB;
        break;
    default:
        result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
        return FALSE;
    }

    if (!string_to_guid (ownerptr + 2, &guid))
    {
        result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
        return FALSE;
    }


    switch (type)
    {
    case GNC_OWNER_CUSTOMER:
    {
        GncCustomer *customer =
            gncCustomerLookup (gnc_get_current_book (), &guid);
        RETURN_IF_NULL (customer);
        gncOwnerInitCustomer (&owner, customer);
        etype = "Customer";
        break;
    }
    case GNC_OWNER_VENDOR:
    {
        GncVendor *vendor =
            gncVendorLookup (gnc_get_current_book (), &guid);
        RETURN_IF_NULL (vendor);
        gncOwnerInitVendor (&owner, vendor);
        etype = "Vendor";
        break;
    }
    case GNC_OWNER_EMPLOYEE:
    {
        GncEmployee *employee =
            gncEmployeeLookup (gnc_get_current_book (), &guid);
        RETURN_IF_NULL(employee);
        gncOwnerInitEmployee (&owner, employee);
        etype = "Employee";
        break;
    }
    case GNC_OWNER_JOB:
    {
        GncJob *job =
            gncJobLookup (gnc_get_current_book (), &guid);
        RETURN_IF_NULL(job);
        gncOwnerInitJob (&owner, job);
        etype = "Job";
        break;
    }
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

        if (!string_to_guid (acctptr + 5, &guid))
        {
            result->error_message = g_strdup_printf (_("Bad URL: %s"), location);
            return FALSE;
        }

        acc = xaccAccountLookup (&guid, gnc_get_current_book ());
        if (NULL == acc)
        {
            result->error_message = g_strdup_printf (_("No such Account entity: %s"),
                                    location);
            return FALSE;
        }
    }

    /* Ok, let's run this report */
    gnc_business_call_owner_report (&owner, acc);

    return TRUE;
}

void
gnc_business_urls_initialize (void)
{
    int i;
    static struct
    {
        URLType urltype;
        char *  protocol;
        GncHTMLUrlCB handler;
    } types[] =
    {
        { GNC_ID_CUSTOMER, GNC_ID_CUSTOMER, customerCB },
        { GNC_ID_VENDOR, GNC_ID_VENDOR, vendorCB },
        { GNC_ID_EMPLOYEE, GNC_ID_EMPLOYEE, employeeCB },
        { GNC_ID_JOB, GNC_ID_JOB, jobCB },
        { GNC_ID_INVOICE, GNC_ID_INVOICE, invoiceCB },
        { URL_TYPE_OWNERREPORT, "gnc-ownerreport", ownerreportCB },
        { NULL, NULL }
    };

    for (i = 0; types[i].urltype; i++)
        gnc_html_register_urltype (types[i].urltype, types[i].protocol);

    for (i = 0; types[i].urltype; i++)
        if (types[i].handler)
            gnc_html_register_url_handler (types[i].urltype, types[i].handler);

}

/* =========================== END OF FILE ========================= */
