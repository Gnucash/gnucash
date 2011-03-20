/*
 * gncEntryLedgerModel.c -- Model for GncEntry ledger
 * Copyright (C) 2001, 2002, 2003 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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

#include <glib.h>
#include <glib/gi18n.h>

#include "Account.h"
#include "gnc-ui-util.h"
#include "qof.h"	/* for safe_strcmp */

#include "datecell.h"
#include "checkboxcell.h"

#include "gncEntryLedgerP.h"
#include "gncEntryLedgerModel.h"

static GncEntryLedgerColors reg_colors =
{
    0x96B183,
    0xBFDEB9,
    0xF6FFDA,

    0xFFEF98,
    0xFFEF98,
};

/** Private Interfaces ***********************************************/

/* GET_LABEL */

static const char * get_iacct_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Income Account");
}

static const char * get_bacct_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Expense Account");
}

static const char * get_actn_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Action");
}

static const char * get_date_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Date");
}

static const char * get_desc_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Description");
}

static const char * get_disc_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Discount");
}

static const char * get_distype_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Discount Type");
}

static const char * get_dishow_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Discount How");
}

static const char * get_pric_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Unit Price");
}

static const char * get_qty_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Quantity");
}

static const char * get_taxtable_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Tax Table");
}

static const char * get_taxable_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Taxable?");
}

static const char * get_taxincluded_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Tax Included?");
}

static const char * get_inv_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Invoiced?");
}

static const char * get_value_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Subtotal");
}

static const char * get_taxval_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Tax");
}

static const char * get_billable_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Billable?");
}

static const char * get_payment_label (VirtualLocation virt_loc, gpointer data)
{
    return _("Payment");
}

/* GET_ENTRY */

static const char * get_iacct_entry (VirtualLocation virt_loc,
                                     gboolean translate,
                                     gboolean *conditionally_changed,
                                     gpointer user_data)
{
    static char *name = NULL;

    GncEntryLedger *ledger = user_data;
    GncEntry *entry;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

    g_free (name);
    name = gnc_get_account_name_for_register (gncEntryGetInvAccount (entry));
    return name;
}

static const char * get_bacct_entry (VirtualLocation virt_loc,
                                     gboolean translate,
                                     gboolean *conditionally_changed,
                                     gpointer user_data)
{
    static char *name = NULL;

    GncEntryLedger *ledger = user_data;
    GncEntry *entry;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

    g_free (name);
    name = gnc_get_account_name_for_register (gncEntryGetBillAccount (entry));
    return name;
}

static const char * get_actn_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    return gncEntryGetAction  (entry);
}

static const char * get_date_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    Timespec ts;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

    ts = gncEntryGetDate (entry);
    return gnc_print_date (ts);
}

static const char * get_desc_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    return gncEntryGetDescription (entry);
}

static const char * get_disc_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    gnc_numeric discount;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    discount = gncEntryGetInvDiscount (entry);
    if (gnc_numeric_zero_p (discount))
        return NULL;

    return xaccPrintAmount (discount, gnc_default_print_info (FALSE));
}

static const char * get_distype_entry (VirtualLocation virt_loc,
                                       gboolean translate,
                                       gboolean *conditionally_changed,
                                       gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    char type;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    type = gncEntryGetInvDiscountType (entry);

    if (translate)
    {
        return gnc_entry_ledger_type_string_getter (type + '0');
    }
    else
    {
        static char s[2];
        s[0] = '0' + type;
        s[1] = '\0';
        return s;
    }
}

static const char * get_dishow_entry (VirtualLocation virt_loc,
                                      gboolean translate,
                                      gboolean *conditionally_changed,
                                      gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    char type;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    type = gncEntryGetInvDiscountHow (entry);

    if (translate)
    {
        return gnc_entry_ledger_how_string_getter (type + '0');
    }
    else
    {
        static char s[2];
        s[0] = '0' + type;
        s[1] = '\0';
        return s;
    }
}

static const char * get_pric_entry (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    gnc_numeric price;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    if (ledger->is_invoice)
        price = gncEntryGetInvPrice (entry);
    else
        price = gncEntryGetBillPrice (entry);

    if (gnc_numeric_zero_p (price))
        return NULL;

    return xaccPrintAmount (price, gnc_default_print_info (FALSE));
}

static const char * get_qty_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    gnc_numeric qty;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    qty = gncEntryGetQuantity (entry);

    if (gnc_numeric_zero_p (qty))
        return NULL;

    return xaccPrintAmount (qty, gnc_default_print_info (FALSE));
}

static const char * get_taxable_entry (VirtualLocation virt_loc,
                                       gboolean translate,
                                       gboolean *conditionally_changed,
                                       gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    gboolean taxable;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    if (ledger->is_invoice)
        taxable = gncEntryGetInvTaxable (entry);
    else
        taxable = gncEntryGetBillTaxable (entry);

    return gnc_checkbox_cell_get_string (taxable);
}

static gboolean
gnc_entry_ledger_get_taxable_value (VirtualLocation virt_loc,
                                    gboolean translate,
                                    gboolean *conditionally_changed,
                                    gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    gboolean is_current;

    is_current = virt_cell_loc_equal(ledger->table->current_cursor_loc.vcell_loc,
                                     virt_loc.vcell_loc);
    if (is_current)
        return gnc_entry_ledger_get_checkmark (ledger, ENTRY_TAXABLE_CELL);
    else
    {
        const char *valstr =
            get_taxable_entry (virt_loc, translate, conditionally_changed,
                               user_data);
        if (valstr && *valstr == 'X')
            return TRUE;
    }
    return FALSE;
}

static const char * get_taxtable_entry (VirtualLocation virt_loc,
                                        gboolean translate,
                                        gboolean *conditionally_changed,
                                        gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    GncTaxTable *table;
    gboolean taxable;

    /* load the cell properly; just shadow the value */
    if (!conditionally_changed)
    {
        taxable = gnc_entry_ledger_get_taxable_value (virt_loc, translate,
                  conditionally_changed,
                  user_data);
        if (!taxable)
            return NULL;
    }

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    if (ledger->is_invoice)
        table = gncEntryGetInvTaxTable (entry);
    else
        table = gncEntryGetBillTaxTable (entry);

    return gncTaxTableGetName (table);
}

static const char * get_taxincluded_entry (VirtualLocation virt_loc,
        gboolean translate,
        gboolean *conditionally_changed,
        gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    gboolean taxable, taxincluded;

    /* load the cell properly; just shadow the value */
    if (!conditionally_changed)
    {
        taxable = gnc_entry_ledger_get_taxable_value (virt_loc, translate,
                  conditionally_changed,
                  user_data);
        if (!taxable)
            return NULL;
    }

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    if (ledger->is_invoice)
        taxincluded = gncEntryGetInvTaxIncluded (entry);
    else
        taxincluded = gncEntryGetBillTaxIncluded (entry);

    return gnc_checkbox_cell_get_string (taxincluded);
}

static const char * get_inv_entry (VirtualLocation virt_loc,
                                   gboolean translate,
                                   gboolean *conditionally_changed,
                                   gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

    return gnc_checkbox_cell_get_string (gncEntryGetInvoice (entry) != NULL);

    /* XXX: what if this entry doesn't belong to this invoice?
     * Or, better question, what if this is the blank_entry on
     * an invoice page?  For the latter, don't worry about it;
     * it will be added automatically during the Save operation
     */
}

static const char * get_value_entry (VirtualLocation virt_loc,
                                     gboolean translate,
                                     gboolean *conditionally_changed,
                                     gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    gnc_numeric value;

    /* Check if this is the current cursor */
    if (virt_cell_loc_equal (ledger->table->current_cursor_loc.vcell_loc,
                             virt_loc.vcell_loc))
    {
        gnc_entry_ledger_compute_value (ledger, &value, NULL);
    }
    else
    {
        GncEntry *entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

        if (entry == gnc_entry_ledger_get_blank_entry (ledger))
            return NULL;

        value = gncEntryReturnValue (entry, ledger->is_invoice);
    }
    return xaccPrintAmount (value, gnc_default_print_info (FALSE));
}

static const char * get_taxval_entry (VirtualLocation virt_loc,
                                      gboolean translate,
                                      gboolean *conditionally_changed,
                                      gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    gnc_numeric value;

    /* Check if this is the current cursor */
    if (virt_cell_loc_equal (ledger->table->current_cursor_loc.vcell_loc,
                             virt_loc.vcell_loc))
    {
        gnc_entry_ledger_compute_value (ledger, NULL, &value);
    }
    else
    {
        GncEntry *entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

        if (entry == gnc_entry_ledger_get_blank_entry (ledger))
            return NULL;

        value = gncEntryReturnTaxValue (entry, ledger->is_invoice);
    }

    return xaccPrintAmount (value, gnc_default_print_info (FALSE));
}

static const char * get_billable_entry (VirtualLocation virt_loc,
                                        gboolean translate,
                                        gboolean *conditionally_changed,
                                        gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    return gnc_checkbox_cell_get_string (gncEntryGetBillable (entry));
}

static const char * get_payment_entry (VirtualLocation virt_loc,
                                       gboolean translate,
                                       gboolean *conditionally_changed,
                                       gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    GncEntryPaymentType type;

    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

    if (!entry)
        return "";

    type = gncEntryGetBillPayment (entry);

    switch (type)
    {
    case GNC_PAYMENT_CASH:
        return _("Cash");
    case GNC_PAYMENT_CARD:
        return _("Charge");
    default:
        g_warning ("Invalid payment type: %d", type);
        return "";
    }
}

/* GET_HELP */

static char * get_acct_help (VirtualLocation virt_loc, gpointer user_data)
{
    const char *help;
    GncEntryLedger *ledger = user_data;

    help = gnc_table_get_entry (ledger->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the income/expense account for the Entry, "
                 "or choose one from the list");

    return g_strdup (help);
}

static char * get_actn_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;

    help = gnc_table_get_entry (ledger->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the type of Entry");

    return g_strdup (help);
}

static char * get_date_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    BasicCell *cell;
    char string[1024];
    struct tm *tm;
    Timespec ts;
    time_t tt;

    cell = gnc_table_get_cell (ledger->table, virt_loc);
    if (!cell)
        return NULL;

    if (!cell->value || *cell->value == '\0')
        return NULL;

    gnc_date_cell_get_date ((DateCell *) cell, &ts);
    tt = ts.tv_sec;
    tm = localtime (&tt);
    qof_strftime (string, sizeof(string), "%A %d %B %Y", tm);

    return g_strdup (string);
}

static char * get_desc_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;

    help = gnc_table_get_entry (ledger->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the Entry Description");

    return g_strdup (help);
}

static char * get_disc_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;
    gint type;

    type = gnc_entry_ledger_get_type (ledger, ENTRY_DISTYPE_CELL);

    switch (type)
    {
    case GNC_AMT_TYPE_VALUE:
        help = _("Enter the Discount Amount");
        break;
    case GNC_AMT_TYPE_PERCENT:
        help = _("Enter the Discount Percent");
        break;
    default:
        help = _("Enter the Discount ... unknown type");
        break;
    }

    return g_strdup (help);
}

static char * get_distype_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;
    gint type;

    type = gnc_entry_ledger_get_type (ledger, ENTRY_DISTYPE_CELL);

    switch (type)
    {
    case GNC_AMT_TYPE_VALUE:
        help = _("Discount Type: Monetary Value");
        break;
    case GNC_AMT_TYPE_PERCENT:
        help = _("Discount Type: Percent");
        break;
    default:
        help = _("Select the Discount Type");
        break;
    }
    return g_strdup (help);
}

static char * get_dishow_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;
    gint type;

    type = gnc_entry_ledger_get_type (ledger, ENTRY_DISHOW_CELL);

    switch (type)
    {
    case GNC_DISC_PRETAX:
        help = _("Tax computed after discount is applied");
        break;
    case GNC_DISC_SAMETIME:
        help = _("Discount and tax both applied on pretax value");
        break;
    case GNC_DISC_POSTTAX:
        help = _("Discount computed after tax is applied");
        break;
    default:
        help = _("Select how to compute the Discount and Taxes");
        break;
    }
    return g_strdup (help);
}

static char * get_pric_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;

    help = gnc_table_get_entry (ledger->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the unit-Price for this Entry");

    return g_strdup (help);
}

static char * get_qty_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;

    help = gnc_table_get_entry (ledger->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the Quantity of units for this Entry");

    return g_strdup (help);
}

static char * get_taxtable_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;

    help = gnc_table_get_entry (ledger->table, virt_loc);
    if (!help || *help == '\0')
        help = _("Enter the Tax Table to apply to this entry");

    return g_strdup (help);
}

static char * get_taxable_help (VirtualLocation virt_loc, gpointer user_data)
{
    const char *help;

    help = _("Is this entry taxable?");

    return g_strdup (help);
}

static char * get_taxincluded_help (VirtualLocation virt_loc, gpointer user_data)
{
    const char *help;

    help = _("Is the tax already included in the price of this entry?");

    return g_strdup (help);
}

static char * get_inv_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;

    switch (ledger->type)
    {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_ORDER_VIEWER:
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_BILL_VIEWER:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    case GNCENTRY_EXPVOUCHER_VIEWER:
        help = _("Is this entry Invoiced?");
        break;
    case GNCENTRY_INVOICE_ENTRY:
    case GNCENTRY_INVOICE_VIEWER:
        help = _("Include this entry on this invoice?");
        break;
    default:
        help = _("Unknown EntryLedger Type");
    }

    return g_strdup (help);
}

static char * get_value_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;

    help = gnc_table_get_entry (ledger->table, virt_loc);
    if (!help || *help == '\0')
        help = _("The subtotal value of this entry ");

    return g_strdup (help);
}

static char * get_taxval_help (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    const char *help;

    help = gnc_table_get_entry (ledger->table, virt_loc);
    if (!help || *help == '\0')
        help = _("The total tax of this entry ");

    return g_strdup (help);
}

static char * get_billable_help (VirtualLocation virt_loc, gpointer user_data)
{
    const char *help;

    help = _("Is this entry billable to a customer or job?");

    return g_strdup (help);
}

static char * get_payment_help (VirtualLocation virt_loc, gpointer user_data)
{
    const char *help;

    help = _("How did you pay for this item?");

    return g_strdup (help);
}

/* GET_IO_FLAGS */

static CellIOFlags get_standard_io_flags (VirtualLocation virt_loc,
        gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    switch (ledger->type)
    {
    case GNCENTRY_ORDER_ENTRY:
    case GNCENTRY_BILL_ENTRY:
    case GNCENTRY_EXPVOUCHER_ENTRY:
    {
        GncEntry *entry =
            gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

        /*
         * If the type is an order_entry and the entry was invoiced,
         * make the entry immutable
         */
        if (gncEntryGetInvoice (entry) != NULL)
            return XACC_CELL_ALLOW_SHADOW;
    }
    /* FALLTHROUGH */
    default:
        return XACC_CELL_ALLOW_ALL;
    }
}

static CellIOFlags get_typecell_io_flags (VirtualLocation virt_loc,
        gpointer user_data)
{
    return (get_standard_io_flags (virt_loc, user_data) |
            XACC_CELL_ALLOW_EXACT_ONLY);
}

static CellIOFlags get_inv_io_flags (VirtualLocation virt_loc,
                                     gpointer user_data)
{
    GncEntryLedger *ledger = user_data;

    switch (ledger->type)
    {
    case GNCENTRY_INVOICE_ENTRY:
    {
        /* This cell should be mutably IFF this entry is attached to
         * a bill, order, or something else.
         */
        GncEntry * entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);

        if ((gncEntryGetOrder (entry) != NULL) || (gncEntryGetBill (entry) != NULL))
            return XACC_CELL_ALLOW_ALL | XACC_CELL_ALLOW_EXACT_ONLY;

    }
    /* FALLTHROUGH */
    default:
        return XACC_CELL_ALLOW_SHADOW;
    }
}

static CellIOFlags get_value_io_flags (VirtualLocation virt_loc,
                                       gpointer user_data)
{
    return XACC_CELL_ALLOW_SHADOW;
}

static CellIOFlags get_tax_io_flags (VirtualLocation virt_loc,
                                     gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    gboolean taxable;

    taxable = gnc_entry_ledger_get_checkmark (ledger, ENTRY_TAXABLE_CELL);

    /* Only print the taxtable and taxincluded cells if taxable is true */
    if (taxable)
        return get_standard_io_flags (virt_loc, user_data);

    /* Shadow the value, so the cell is loaded properly */
    return XACC_CELL_ALLOW_SHADOW;
}

static CellIOFlags get_taxincluded_io_flags (VirtualLocation virt_loc,
        gpointer user_data)
{
    CellIOFlags flags = get_tax_io_flags (virt_loc, user_data);
    if (flags == XACC_CELL_ALLOW_SHADOW)
        return flags;
    return flags | XACC_CELL_ALLOW_EXACT_ONLY;
}

static CellIOFlags get_qty_io_flags (VirtualLocation virt_loc, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry;
    CellIOFlags flags = get_standard_io_flags (virt_loc, user_data);

    /* If this isn't an invoice, or the flags are already read-only ... */
    if (!ledger->is_invoice || flags == XACC_CELL_ALLOW_SHADOW)
        return flags;

    /* ok, if this is an invoice ledger AND this entry is attached to a
     * bill (i.e. it's billable), freeze the quantity
     */
    entry = gnc_entry_ledger_get_entry (ledger, virt_loc.vcell_loc);
    if (gncEntryGetBillable (entry))
        return XACC_CELL_ALLOW_SHADOW;

    return flags;
}

/* GET BG_COLORS */

static guint32
gnc_entry_ledger_get_bg_color (VirtualLocation virt_loc,
                               gboolean *hatching, gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    VirtualCell *vcell;
    guint32 bg_color;
    gboolean is_current;

    if (hatching)
        *hatching = FALSE;

    bg_color = 0xffffff; /* white */

    if (!ledger) return bg_color;

    if (gnc_table_virtual_location_in_header (ledger->table, virt_loc))
        return reg_colors.header_bg_color;

    vcell = gnc_table_get_virtual_cell (ledger->table, virt_loc.vcell_loc);
    if (!vcell || !vcell->cellblock)
        return bg_color;

    if ((virt_loc.phys_col_offset < vcell->cellblock->start_col) ||
            (virt_loc.phys_col_offset > vcell->cellblock->stop_col))
        return bg_color;

    is_current = virt_cell_loc_equal
                 (ledger->table->current_cursor_loc.vcell_loc, virt_loc.vcell_loc);

    if (is_current)
        return vcell->start_primary_color ?
               reg_colors.primary_active_bg_color :
               reg_colors.secondary_active_bg_color;

    return vcell->start_primary_color ?
           reg_colors.primary_bg_color : reg_colors.secondary_bg_color;
}

/* SAVE CELLS */

static void gnc_entry_ledger_save_cells (gpointer save_data,
        gpointer user_data)
{
    GncEntryLedger *ledger = user_data;
    GncEntry *entry = save_data;

    g_return_if_fail (entry != NULL);

    /* copy the contents from the cursor to the split */

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_IACCT_CELL, TRUE))
    {
        Account *acc;

        acc = gnc_entry_ledger_get_account (ledger, ENTRY_IACCT_CELL);

        if (acc != NULL)
            gncEntrySetInvAccount (entry, acc);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_BACCT_CELL, TRUE))
    {
        Account *acc;

        acc = gnc_entry_ledger_get_account (ledger, ENTRY_BACCT_CELL);

        if (acc != NULL)
            gncEntrySetBillAccount (entry, acc);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_ACTN_CELL, TRUE))
    {
        const char *value;

        value = gnc_table_layout_get_cell_value (ledger->table->layout,
                ENTRY_ACTN_CELL);
        gncEntrySetAction (entry, value);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_DATE_CELL, TRUE))
    {
        BasicCell *cell;
        Timespec ts;

        cell = gnc_table_layout_get_cell (ledger->table->layout, ENTRY_DATE_CELL);

        /* commit any pending changes */
        gnc_date_cell_commit ((DateCell *) cell);

        gnc_date_cell_get_date ((DateCell *) cell, &ts);
        gncEntrySetDate (entry, ts);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_DESC_CELL, TRUE))
    {
        const char *value;

        value = gnc_table_layout_get_cell_value (ledger->table->layout,
                ENTRY_DESC_CELL);
        gncEntrySetDescription (entry, value);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_DISC_CELL, TRUE))
    {
        gnc_numeric amount;

        if (gnc_entry_ledger_get_numeric (ledger, ENTRY_DISC_CELL, &amount))
            gncEntrySetInvDiscount (entry, amount);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_DISTYPE_CELL, TRUE))
    {
        gint type;

        type = gnc_entry_ledger_get_type (ledger, ENTRY_DISTYPE_CELL);

        if (type != -1)
            gncEntrySetInvDiscountType (entry, type);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_DISHOW_CELL, TRUE))
    {
        gint type;

        type = gnc_entry_ledger_get_type (ledger, ENTRY_DISHOW_CELL);

        if (type != -1)
            gncEntrySetInvDiscountHow (entry, type);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_QTY_CELL, TRUE))
    {
        gnc_numeric amount;

        if (gnc_entry_ledger_get_numeric (ledger, ENTRY_QTY_CELL, &amount))
            gncEntrySetQuantity (entry, amount);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_BILLABLE_CELL, TRUE))
    {
        gboolean billable;

        billable = gnc_entry_ledger_get_checkmark (ledger, ENTRY_BILLABLE_CELL);
        gncEntrySetBillable (entry, billable);
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_PAYMENT_CELL, TRUE))
    {
        const char *value;

        value = gnc_table_layout_get_cell_value (ledger->table->layout,
                ENTRY_PAYMENT_CELL);
        if (!safe_strcmp (value, _("Cash")))
            gncEntrySetBillPayment (entry, GNC_PAYMENT_CASH);
        else if (!safe_strcmp (value, _("Charge")))
            gncEntrySetBillPayment (entry, GNC_PAYMENT_CARD);
        else
            g_warning ("Invalid Payment cell: %s", value ? value : "(null)");
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_PRIC_CELL, TRUE))
    {
        gnc_numeric amount;

        if (gnc_entry_ledger_get_numeric (ledger, ENTRY_PRIC_CELL, &amount))
        {
            if (ledger->is_invoice)
                gncEntrySetInvPrice (entry, amount);
            else
                gncEntrySetBillPrice (entry, amount);
        }
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_TAXABLE_CELL, TRUE))
    {
        gboolean taxable;

        taxable = gnc_entry_ledger_get_checkmark (ledger, ENTRY_TAXABLE_CELL);
        if (ledger->is_invoice)
            gncEntrySetInvTaxable (entry, taxable);
        else
            gncEntrySetBillTaxable (entry, taxable);
    }

    /* XXX: Only (re-set) these if taxable is TRUE? */
    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_TAXTABLE_CELL, TRUE))
    {
        GncTaxTable *table;

        table = gnc_entry_ledger_get_taxtable (ledger, ENTRY_TAXTABLE_CELL);
        if (table)
        {
            if (ledger->is_invoice)
                gncEntrySetInvTaxTable (entry, table);
            else
                gncEntrySetBillTaxTable (entry, table);
        }
    }

    if (gnc_table_layout_get_cell_changed (ledger->table->layout,
                                           ENTRY_TAXINCLUDED_CELL, TRUE))
    {
        gboolean taxincluded;

        taxincluded = gnc_entry_ledger_get_checkmark (ledger,
                      ENTRY_TAXINCLUDED_CELL);
        if (ledger->is_invoice)
            gncEntrySetInvTaxIncluded (entry, taxincluded);
        else
            gncEntrySetBillTaxIncluded (entry, taxincluded);
    }

    if (ledger->type == GNCENTRY_INVOICE_ENTRY)
    {
        gboolean inv_value;

        inv_value = gnc_entry_ledger_get_checkmark (ledger, ENTRY_INV_CELL);

        if (inv_value)
        {
            /* Add this to the invoice (if it's not already attached) */
            if (gncEntryGetInvoice (entry) == NULL)
                gncInvoiceAddEntry (ledger->invoice, entry);

        }
        else
        {
            /* Remove from the invoice iff we're attached to an order or bill */
            if ((gncEntryGetOrder (entry) != NULL) ||
                    (gncEntryGetBill (entry) != NULL))
                gncInvoiceRemoveEntry (ledger->invoice, entry);
        }
    }
}

/* Set Cell Handlers */

static void gnc_entry_ledger_model_new_handlers (TableModel *model,
        GncEntryLedgerType type)
{
    struct model_desc
    {
        const char * cell;
        gpointer entry_handler;
        gpointer label_handler;
        gpointer help_handler;
        gpointer io_flags_handler;
    } models[] =
    {
        { ENTRY_IACCT_CELL, get_iacct_entry, get_iacct_label, get_acct_help, get_standard_io_flags },
        { ENTRY_BACCT_CELL, get_bacct_entry, get_bacct_label, get_acct_help, get_standard_io_flags },
        { ENTRY_ACTN_CELL, get_actn_entry, get_actn_label, get_actn_help, get_standard_io_flags },
        { ENTRY_DATE_CELL, get_date_entry, get_date_label, get_date_help, get_standard_io_flags },
        { ENTRY_DESC_CELL, get_desc_entry, get_desc_label, get_desc_help, get_standard_io_flags },
        { ENTRY_DISC_CELL, get_disc_entry, get_disc_label, get_disc_help, get_standard_io_flags },
        { ENTRY_DISTYPE_CELL, get_distype_entry, get_distype_label, get_distype_help,  get_typecell_io_flags },
        { ENTRY_DISHOW_CELL, get_dishow_entry, get_dishow_label, get_dishow_help, get_typecell_io_flags },
        { ENTRY_PRIC_CELL, get_pric_entry, get_pric_label, get_pric_help, get_standard_io_flags },
        { ENTRY_QTY_CELL, get_qty_entry, get_qty_label, get_qty_help, get_qty_io_flags },
        { ENTRY_TAXABLE_CELL, get_taxable_entry, get_taxable_label, get_taxable_help,  get_typecell_io_flags },
        { ENTRY_TAXTABLE_CELL, get_taxtable_entry, get_taxtable_label, get_taxtable_help, get_tax_io_flags },
        { ENTRY_TAXINCLUDED_CELL, get_taxincluded_entry, get_taxincluded_label, get_taxincluded_help, get_taxincluded_io_flags },
        { ENTRY_INV_CELL, get_inv_entry, get_inv_label, get_inv_help, get_inv_io_flags },
        { ENTRY_VALUE_CELL, get_value_entry, get_value_label, get_value_help, get_value_io_flags },
        { ENTRY_TAXVAL_CELL, get_taxval_entry, get_taxval_label, get_taxval_help, get_value_io_flags },
        { ENTRY_BILLABLE_CELL, get_billable_entry, get_billable_label, get_billable_help, get_typecell_io_flags },
        { ENTRY_PAYMENT_CELL, get_payment_entry, get_payment_label, get_payment_help, get_standard_io_flags },
    };
    unsigned int i;

    gnc_table_model_set_default_bg_color_handler
    (model, gnc_entry_ledger_get_bg_color);


    for (i = 0; i < (sizeof(models) / sizeof(*models)); i++)
    {
        if (models[i].entry_handler)
            gnc_table_model_set_entry_handler (model, models[i].entry_handler,
                                               models[i].cell);
        if (models[i].label_handler)
            gnc_table_model_set_label_handler (model, models[i].label_handler,
                                               models[i].cell);
        if (models[i].help_handler)
            gnc_table_model_set_help_handler (model, models[i].help_handler,
                                              models[i].cell);
        if (models[i].io_flags_handler)
            gnc_table_model_set_io_flags_handler (model, models[i].io_flags_handler,
                                                  models[i].cell);
    } /* for */

    /*
    model->cell_data_allocator = ;
    model->cell_data_deallocator = ;
    model->cell_data_copy = ;
    */

    gnc_table_model_set_post_save_handler (model, gnc_entry_ledger_save_cells);

    switch (type)
    {
    case GNCENTRY_ORDER_VIEWER:
    case GNCENTRY_INVOICE_VIEWER:
    case GNCENTRY_BILL_VIEWER:
    case GNCENTRY_EXPVOUCHER_VIEWER:
        /* make this table read-only */
        gnc_table_model_set_read_only (model, TRUE);
        break;
    default:
        break;
    }
}

/** Public Interface ***********************************************/

TableModel * gnc_entry_ledger_model_new (GncEntryLedgerType type)
{
    TableModel * model;

    model = gnc_table_model_new ();
    gnc_entry_ledger_model_new_handlers (model, type);

    return model;
}
