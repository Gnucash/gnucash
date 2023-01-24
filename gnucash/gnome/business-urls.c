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

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "gnc-html.h"
#include "gnc-ui-util.h"
#include "qof.h"
#include "stdint.h"

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

#define HANDLE_TYPE(URL_TYPE_STR,OBJ_TYPE) {                                 \
  QofBook *book;                                                             \
  GncGUID guid;                                                                 \
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
    gnc_ui_customer_edit (result->parent, customer);

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
    gnc_ui_vendor_edit (result->parent, vendor);

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
    gnc_ui_employee_edit (result->parent, employee);

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
    gnc_ui_invoice_edit (result->parent, invoice);

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
    gnc_ui_job_edit (result->parent, job);

    return TRUE;
}

/* ================================================================= */

#define DISABLE_REPORT_IF_NULL(inst)                                    \
  if (NULL == inst)                                                     \
  {                                                                     \
    result->error_message =                                             \
      g_strdup_printf (_("No such owner entity: %s"), location);        \
    show_report = FALSE;                                                \
  }

#define DISABLE_REPORT_IF_TRUE(inst)                                    \
  if (inst)                                                             \
  {                                                                     \
    result->error_message =                                             \
      g_strdup_printf (_("Badly formed URL %s"), location);             \
    show_report = FALSE;                                                \
  }

/* parses a string "user=john&pass=smith&age=41" into a string-keyed
   GHashTable. String must not contain non-ASCII chars. Duplicate keys
   will be ignored. */
static GHashTable *parse_parameters (const gchar *parms)
{
    GHashTable *rethash;
    char *query = strdup (parms);
    const char *key_tok = "&\n";
    const char val_tok = '=';

    rethash = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

    for (char *p = strtok (query, key_tok); p; p = strtok (NULL, key_tok))
    {
        gchar * val = strchr (p, val_tok);
        *(val++) = '\0';
        if (val && !g_hash_table_contains (rethash, p))
            g_hash_table_insert (rethash, g_strdup (p), g_strdup (val));
    }

    g_free (query);
    return rethash;
}

static gboolean
ownerreportCB (const char *location, const char *label,
               gboolean new_window, GNCURLResult * result)
{
    gchar *ownerptr, *acctptr, *etype, *datestr = NULL;
    GncGUID guid;
    GncOwner owner;
    Account *acc;
    GHashTable *query_ht;
    time64 enddate = INT64_MAX;
    gboolean show_report = TRUE;

    g_return_val_if_fail (location != NULL, FALSE);
    g_return_val_if_fail (result != NULL, FALSE);

    result->load_to_stream = FALSE;

    /* href="...:owner=<owner-type>:<guid>[&acct=<guid>]" */
    query_ht = parse_parameters (location);

    /* parse the acct guid*/
    acctptr = g_hash_table_lookup (query_ht, "acct");
    DISABLE_REPORT_IF_TRUE (!acctptr || !string_to_guid (acctptr, &guid));
    acc = xaccAccountLookup (&guid, gnc_get_current_book ());

    /* parse the acct guid*/
    datestr = g_hash_table_lookup (query_ht, "enddate");
    if (datestr)
        enddate = g_ascii_strtoull (datestr, NULL, 10);

    /* parse the owner guid */
    ownerptr = g_hash_table_lookup (query_ht, "owner");
    DISABLE_REPORT_IF_TRUE (!ownerptr || !strchr("cvej", ownerptr[0]) ||
                            ownerptr[1] != ':' ||
                            !string_to_guid (ownerptr+2, &guid));
    memset (&owner, 0, sizeof (owner));
    switch (*ownerptr)
    {
    case 'c':
    {
        GncCustomer *customer =
            gncCustomerLookup (gnc_get_current_book (), &guid);
        DISABLE_REPORT_IF_NULL (customer);
        gncOwnerInitCustomer (&owner, customer);
        etype = "Customer";
        break;
    }
    case 'v':
    {
        GncVendor *vendor =
            gncVendorLookup (gnc_get_current_book (), &guid);
        DISABLE_REPORT_IF_NULL (vendor);
        gncOwnerInitVendor (&owner, vendor);
        etype = "Vendor";
        break;
    }
    case 'e':
    {
        GncEmployee *employee =
            gncEmployeeLookup (gnc_get_current_book (), &guid);
        DISABLE_REPORT_IF_NULL(employee);
        gncOwnerInitEmployee (&owner, employee);
        etype = "Employee";
        break;
    }
    case 'j':
    {
        GncJob *job =
            gncJobLookup (gnc_get_current_book (), &guid);
        DISABLE_REPORT_IF_NULL(job);
        gncOwnerInitJob (&owner, job);
        etype = "Job";
        break;
    }
    default:
        etype = "Undefined";
        break;
    }

    if (owner.owner.undefined == NULL)
    {
        result->error_message =
            g_strdup_printf (_("Entity type does not match %s: %s"),
                             etype, location);
        show_report = FALSE;
    }

    /* Ok, let's run this report */
    if (show_report)
    {
        if (enddate != INT64_MAX)
            gnc_business_call_owner_report_with_enddate (result->parent, &owner,
                                                         acc, enddate);
        else
            gnc_business_call_owner_report (result->parent, &owner, acc);
    }

    g_hash_table_destroy (query_ht);
    return show_report;
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
