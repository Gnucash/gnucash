/*
 * sie4-export.cpp -- SIE4 export writer
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 */

#include <config.h>

#include "sie4-export.h"

#include <glib/gi18n.h>

#include <algorithm>
#include <cerrno>
#include <cstdarg>
#include <cstdlib>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "Account.hpp"
#include "Split.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-date.h"
#include "gnc-numeric.h"
#include "gnc-ui-util.h"
#include "gncBusiness.h"
#include "gncInvoice.h"
#include "gncOwner.h"
#include "guid.h"
#include "qofinstance.h"

namespace
{
constexpr auto SIE4_ERROR_DOMAIN{"gnc-sie4-export-error"};

struct AccountInfo
{
    Account *account;
    std::string code;
    long code_num;
    std::string name;
};

struct BusinessObjects
{
    std::map<std::string, std::string> customers;
    std::map<std::string, std::string> vendors;
    std::map<std::string, std::string> invoices;
};

using AccountInfoMap = std::map<Account*, AccountInfo>;
using AmountMap = std::map<std::string, gnc_numeric>;
using InvoiceByTransaction = std::map<Transaction*, GncInvoice*>;
using ObjectRefs = std::vector<std::pair<std::string, std::string>>;

struct SplitLine
{
    Split *split;
    const AccountInfo *account_info;
};

struct VoucherInfo
{
    Transaction *trans;
    std::string number;
    guint64 sort_number;
};

GQuark
sie4_error_quark ()
{
    return g_quark_from_static_string (SIE4_ERROR_DOMAIN);
}

std::string
str_or_empty (const char *value)
{
    return value ? value : "";
}

std::string
format_user_message (const char *message_format, ...)
{
    va_list args;
    va_start (args, message_format);
    auto message = g_strdup_vprintf (message_format, args);
    va_end (args);

    std::string result{message ? message : ""};
    g_free (message);
    return result;
}

std::string
stripped_string (const char *value)
{
    if (!value)
        return {};

    auto copy = g_strdup (value);
    g_strstrip (copy);
    std::string result{copy};
    g_free (copy);
    return result;
}

std::string
full_account_name (Account *account)
{
    auto name = gnc_account_get_full_name (account);
    std::string result{name ? name : ""};
    g_free (name);
    return result;
}

std::string
sie_date (time64 time)
{
    auto text = gnc_print_time64 (time, "%Y%m%d");
    std::string result{text ? text : ""};
    g_free (text);
    return result;
}

std::string
sie_quote (const std::string& value)
{
    std::string result{"\""};

    for (auto ch : value)
    {
        if (ch == '\n' || ch == '\r')
            result += ' ';
        else if (ch == '"' || ch == '\\')
        {
            result += '\\';
            result += ch;
        }
        else
            result += ch;
    }

    result += '"';
    return result;
}

std::string
format_amount (gnc_numeric value)
{
    auto rounded = gnc_numeric_convert (value, 100,
                                        GNC_HOW_DENOM_FIXED |
                                        GNC_HOW_RND_ROUND_HALF_UP);
    if (gnc_numeric_check (rounded) != GNC_ERROR_OK)
        throw std::runtime_error (_("Unable to round an amount to two decimal places."));

    auto num = gnc_numeric_num (rounded);
    auto abs_num = num < 0 ? -num : num;

    std::ostringstream ss;
    if (num < 0)
        ss << '-';
    ss << (abs_num / 100) << '.'
       << std::setw (2) << std::setfill ('0') << (abs_num % 100);
    return ss.str ();
}

bool
is_zero_at_cents (gnc_numeric value)
{
    auto rounded = gnc_numeric_convert (value, 100,
                                        GNC_HOW_DENOM_FIXED |
                                        GNC_HOW_RND_ROUND_HALF_UP);
    return gnc_numeric_check (rounded) == GNC_ERROR_OK && gnc_numeric_zero_p (rounded);
}

bool
parse_account_code (const std::string& code, long& number)
{
    if (code.empty ())
        return false;

    errno = 0;
    char *end = nullptr;
    auto parsed = std::strtol (code.c_str (), &end, 10);
    auto valid = errno == 0 && end && *end == '\0';
    if (valid)
        number = parsed;

    return valid;
}

bool
parse_positive_integer (const std::string& text, guint64& number)
{
    if (text.empty ())
        return false;

    for (auto ch : text)
    {
        if (!g_ascii_isdigit (ch))
            return false;
    }

    errno = 0;
    char *end = nullptr;
    auto parsed = g_ascii_strtoull (text.c_str (), &end, 10);
    auto valid = errno == 0 && end && *end == '\0' && parsed > 0;
    if (valid)
        number = parsed;

    return valid;
}

bool
valid_voucher_series (const std::string& series)
{
    if (series.empty ())
        return false;

    for (auto ch : series)
    {
        if (g_ascii_isspace (ch) || ch == '"')
            return false;
    }

    return true;
}

bool
valid_account_plan (const std::string& account_plan)
{
    if (account_plan.empty ())
        return true;

    return g_ascii_strcasecmp (account_plan.c_str (), "BAS95") == 0 ||
           g_ascii_strcasecmp (account_plan.c_str (), "BAS96") == 0 ||
           g_ascii_strcasecmp (account_plan.c_str (), "EUBAS97") == 0 ||
           g_ascii_strcasecmp (account_plan.c_str (), "NE2007") == 0 ||
           g_ascii_strncasecmp (account_plan.c_str (), "BAS2", 4) == 0;
}

std::string
normalized_currency_code (const char *value)
{
    auto code = stripped_string (value);

    std::transform (code.begin (), code.end (), code.begin (),
                    [](unsigned char ch) {
                        return static_cast<char> (g_ascii_toupper (ch));
                    });

    return code;
}

bool
valid_currency_code (const std::string& currency_code)
{
    if (currency_code.empty ())
        return true;

    return currency_code.size () == 3 &&
           g_ascii_isalpha (currency_code[0]) &&
           g_ascii_isalpha (currency_code[1]) &&
           g_ascii_isalpha (currency_code[2]);
}

void
add_amount (AmountMap& amounts, const std::string& code, gnc_numeric value)
{
    auto current = amounts.count (code) ? amounts[code] : gnc_numeric_zero ();
    auto sum = gnc_numeric_add (current, value, GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_LCD | GNC_HOW_RND_NEVER);
    if (gnc_numeric_check (sum) != GNC_ERROR_OK)
        throw std::runtime_error (_("Unable to accumulate SIE4 amounts exactly."));
    amounts[code] = sum;
}

void
add_total (gnc_numeric& total, gnc_numeric value)
{
    auto sum = gnc_numeric_add (total, value, GNC_DENOM_AUTO,
                                GNC_HOW_DENOM_LCD | GNC_HOW_RND_NEVER);
    if (gnc_numeric_check (sum) != GNC_ERROR_OK)
        throw std::runtime_error (_("Unable to accumulate SIE4 amounts exactly."));
    total = sum;
}

const AccountInfo&
require_account_info (const AccountInfoMap& accounts, Account *account)
{
    if (!account)
        throw std::runtime_error (_("Transaction has a split without an account."));

    auto iter = accounts.find (account);
    if (iter != accounts.end ())
        return iter->second;

    auto name = full_account_name (account);
    throw std::runtime_error (
        format_user_message (_("Transaction uses an account without a numeric account code: %s"),
                             name.c_str ()));
}

bool
is_result_account (Account *account)
{
    auto type = xaccAccountGetType (account);

    return type == ACCT_TYPE_INCOME || type == ACCT_TYPE_EXPENSE;
}

char
account_type_to_ktyp (Account *account)
{
    switch (xaccAccountGetType (account))
    {
    case ACCT_TYPE_INCOME:
        return 'I';
    case ACCT_TYPE_EXPENSE:
        return 'K';
    case ACCT_TYPE_LIABILITY:
    case ACCT_TYPE_PAYABLE:
    case ACCT_TYPE_CREDIT:
    case ACCT_TYPE_CREDITLINE:
    case ACCT_TYPE_EQUITY:
        return 'S';
    default:
        return 'T';
    }
}

bool
account_needs_sie4_code (Account *account,
                         const GncSie4ExportSettings *settings)
{
    auto result_account = is_result_account (account);
    gnc_numeric ib_current = gnc_numeric_zero ();
    gnc_numeric ib_previous = gnc_numeric_zero ();
    gnc_numeric ub_current = gnc_numeric_zero ();
    gnc_numeric ub_previous = gnc_numeric_zero ();
    gnc_numeric res_current = gnc_numeric_zero ();
    gnc_numeric res_previous = gnc_numeric_zero ();

    for (auto split : xaccAccountGetSplits (account))
    {
        auto trans = xaccSplitGetParent (split);
        if (!trans)
            continue;

        auto date = xaccTransGetDate (trans);
        auto value = xaccSplitGetValue (split);
        if (is_zero_at_cents (value))
            continue;

        if (date >= settings->current_start && date <= settings->current_end)
            return true;

        if (result_account)
        {
            if (date >= settings->current_start && date <= settings->current_end)
                add_total (res_current, value);
            if (date >= settings->previous_start && date <= settings->previous_end)
                add_total (res_previous, value);
        }
        else
        {
            if (date < settings->current_start)
                add_total (ib_current, value);
            if (date < settings->previous_start)
                add_total (ib_previous, value);
            if (date <= settings->current_end)
                add_total (ub_current, value);
            if (date <= settings->previous_end)
                add_total (ub_previous, value);
        }
    }

    if (result_account)
        return !is_zero_at_cents (res_current) || !is_zero_at_cents (res_previous);

    return !is_zero_at_cents (ib_current) || !is_zero_at_cents (ib_previous) ||
           !is_zero_at_cents (ub_current) || !is_zero_at_cents (ub_previous);
}

std::vector<AccountInfo>
collect_accounts (QofBook *book,
                  AccountInfoMap& account_map,
                  const GncSie4ExportSettings *settings)
{
    std::vector<AccountInfo> accounts;
    std::vector<std::string> missing_code_accounts;
    std::map<std::string, Account*> used_codes;
    auto root = gnc_book_get_root_account (book);
    auto account_list = gnc_account_get_descendants_sorted (root);

    /* SIE identifies postings by account code, not by GnuCash account GUID.
     * Reject accounts that contribute to the selected export without a numeric
     * code so the export cannot silently omit activity. */
    for (auto node = account_list; node; node = g_list_next (node))
    {
        auto account = GNC_ACCOUNT (node->data);
        auto code = stripped_string (xaccAccountGetCode (account));
        if (code.empty ())
        {
            if (account_needs_sie4_code (account, settings))
            {
                auto name = full_account_name (account);
                missing_code_accounts.push_back (
                    format_user_message (_("%s; set Account Code to the BAS account number"),
                                         name.c_str ()));
            }
            continue;
        }

        long code_num = 0;
        if (!parse_account_code (code, code_num))
        {
            auto name = full_account_name (account);
            g_list_free (account_list);
            throw std::runtime_error (
                format_user_message (_("Account has a non-numeric SIE4 account code: %s (%s)"),
                                     name.c_str (), code.c_str ()));
        }

        auto duplicate = used_codes.find (code);
        if (duplicate != used_codes.end ())
        {
            auto first = full_account_name (duplicate->second);
            auto second = full_account_name (account);
            g_list_free (account_list);
            throw std::runtime_error (
                format_user_message (_("Duplicate account code in SIE4 export: %s (%s, %s)"),
                                     code.c_str (), first.c_str (), second.c_str ()));
        }

        AccountInfo info{account, code, code_num, str_or_empty (xaccAccountGetName (account))};
        used_codes.emplace (code, account);
        account_map.emplace (account, info);
        accounts.push_back (std::move (info));
    }

    g_list_free (account_list);

    if (!missing_code_accounts.empty ())
    {
        std::ostringstream msg;
        msg << _("SIE4 export requires numeric account codes for these accounts:");
        for (const auto& account : missing_code_accounts)
            msg << "\n" << account;
        throw std::runtime_error (msg.str ());
    }

    std::sort (accounts.begin (), accounts.end (),
               [](const auto& left, const auto& right) {
                   if (left.code_num != right.code_num)
                       return left.code_num < right.code_num;
                   return left.code < right.code;
               });

    return accounts;
}

void
validate_export_currency (const std::vector<AccountInfo>& accounts,
                          const GncSie4ExportSettings *settings,
                          const std::string& currency_code)
{
    auto effective_currency = currency_code.empty () ? "SEK" : currency_code;

    for (const auto& account : accounts)
    {
        for (auto split : xaccAccountGetSplits (account.account))
        {
            if (is_zero_at_cents (xaccSplitGetValue (split)))
                continue;

            auto trans = xaccSplitGetParent (split);
            if (!trans || xaccTransGetDate (trans) > settings->current_end)
                continue;

            auto currency = xaccTransGetCurrency (trans);
            auto mnemonic = currency ? gnc_commodity_get_mnemonic (currency) : nullptr;
            auto transaction_currency = normalized_currency_code (mnemonic);

            if (!transaction_currency.empty () && transaction_currency != effective_currency)
            {
                throw std::runtime_error (
                    format_user_message (_("SIE4 export requires a single accounting currency. Transaction currency %s does not match export currency %s."),
                                         transaction_currency.c_str (),
                                         effective_currency.c_str ()));
            }
        }
    }
}

bool
in_range (time64 date, time64 start, time64 end)
{
    return date >= start && date <= end;
}

void
collect_totals (const std::vector<AccountInfo>& accounts,
                const GncSie4ExportSettings *settings,
                AmountMap& ib_current,
                AmountMap& ib_previous,
                AmountMap& ub_current,
                AmountMap& ub_previous,
                AmountMap& res_current,
                AmountMap& res_previous)
{
    for (const auto& account : accounts)
    {
        for (auto split : xaccAccountGetSplits (account.account))
        {
            auto trans = xaccSplitGetParent (split);
            auto date = xaccTransGetDate (trans);
            auto value = xaccSplitGetValue (split);

            /* BAS classes below 3000 are balance accounts: #IB/#UB are
             * cumulative balances. Classes 3000 and above are result accounts:
             * #RES is movement within the fiscal period. */
            if (account.code_num < 3000)
            {
                if (date < settings->current_start)
                    add_amount (ib_current, account.code, value);
                if (date < settings->previous_start)
                    add_amount (ib_previous, account.code, value);
                if (date <= settings->current_end)
                    add_amount (ub_current, account.code, value);
                if (date <= settings->previous_end)
                    add_amount (ub_previous, account.code, value);
            }
            else
            {
                if (in_range (date, settings->current_start, settings->current_end))
                    add_amount (res_current, account.code, value);
                if (in_range (date, settings->previous_start, settings->previous_end))
                    add_amount (res_previous, account.code, value);
            }
        }
    }
}

struct TransactionLess
{
    bool operator() (Transaction *left, Transaction *right) const
    {
        /* Deterministic ordering keeps repeated exports diffable. */
        return xaccTransOrder (left, right) < 0;
    }
};

std::set<Transaction*, TransactionLess>
collect_transactions (const std::vector<AccountInfo>& accounts,
                      const GncSie4ExportSettings *settings)
{
    std::set<Transaction*, TransactionLess> transactions;

    for (const auto& account : accounts)
    {
        for (auto split : xaccAccountGetSplits (account.account))
        {
            if (is_zero_at_cents (xaccSplitGetValue (split)))
                continue;

            auto trans = xaccSplitGetParent (split);
            if (!trans)
                continue;

            auto date = xaccTransGetDate (trans);

            if (in_range (date, settings->current_start, settings->current_end))
                transactions.insert (trans);
        }
    }

    return transactions;
}

std::string
transaction_label (Transaction *trans)
{
    auto description = stripped_string (xaccTransGetDescription (trans));
    std::ostringstream ss;

    ss << sie_date (xaccTransGetDate (trans));
    if (!description.empty ())
        ss << " " << description;

    return ss.str ();
}

std::vector<VoucherInfo>
build_voucher_infos (const std::set<Transaction*, TransactionLess>& transactions,
                     const GncSie4ExportSettings *settings,
                     GncSie4ExportResult *result)
{
    std::vector<VoucherInfo> vouchers;
    std::vector<std::string> invalid_numbers;
    std::vector<std::string> duplicate_numbers;
    std::map<guint64, std::string> used_labels;

    if (result)
        result->generated_voucher_numbers = 0;

    if (settings->use_transaction_numbers)
    {
        for (auto trans : transactions)
        {
            auto number_text = stripped_string (xaccTransGetNum (trans));
            if (number_text.empty ())
                continue;

            guint64 number = 0;
            auto label = transaction_label (trans);
            if (!parse_positive_integer (number_text, number))
            {
                invalid_numbers.push_back (
                    format_user_message (_("%s (transaction number: %s)"),
                                         label.c_str (), number_text.c_str ()));
                continue;
            }

            auto inserted = used_labels.emplace (number, label);
            if (!inserted.second)
            {
                duplicate_numbers.push_back (
                    format_user_message (_("Transaction number %s is used by both %s and %s."),
                                         number_text.c_str (),
                                         inserted.first->second.c_str (),
                                         label.c_str ()));
            }
        }
    }

    if (!invalid_numbers.empty ())
    {
        std::ostringstream msg;
        msg << _("SIE4 export is set to use GnuCash transaction numbers, but these transaction numbers are not positive integers:");
        for (const auto& line : invalid_numbers)
            msg << "\n" << line;
        throw std::runtime_error (msg.str ());
    }

    if (!duplicate_numbers.empty ())
    {
        std::ostringstream msg;
        msg << _("SIE4 export is set to use GnuCash transaction numbers, but these transaction numbers are duplicated:");
        for (const auto& line : duplicate_numbers)
            msg << "\n" << line;
        throw std::runtime_error (msg.str ());
    }

    guint64 next_generated = 1;
    std::set<guint64> used_numbers;
    for (const auto& used : used_labels)
        used_numbers.insert (used.first);
    if (settings->use_transaction_numbers && !used_numbers.empty ())
        next_generated = *used_numbers.rbegin () + 1;

    for (auto trans : transactions)
    {
        auto number_text = settings->use_transaction_numbers ?
            stripped_string (xaccTransGetNum (trans)) : std::string{};
        guint64 number = 0;

        if (!number_text.empty ())
            parse_positive_integer (number_text, number);
        else
        {
            while (used_numbers.count (next_generated) != 0)
                next_generated++;

            number = next_generated;
            number_text = std::to_string (number);
            used_numbers.insert (number);
            next_generated++;

            if (settings->use_transaction_numbers && result)
                result->generated_voucher_numbers++;
        }

        vouchers.push_back ({trans, number_text, number});
    }

    /* SIE verifications in a series must be emitted in increasing voucher
     * number order. TransactionLess is only a deterministic tie-breaker. */
    std::stable_sort (vouchers.begin (), vouchers.end (),
                      [](const auto& left, const auto& right) {
                          if (left.sort_number != right.sort_number)
                              return left.sort_number < right.sort_number;
                          return TransactionLess{} (left.trans, right.trans);
                      });

    return vouchers;
}

InvoiceByTransaction
collect_invoice_map (QofBook *book)
{
    InvoiceByTransaction invoices;
    auto invoice_list = gncBusinessGetList (book, GNC_ID_INVOICE, TRUE);

    for (auto node = invoice_list; node; node = g_list_next (node))
    {
        auto invoice = GNC_INVOICE (node->data);
        auto trans = gncInvoiceGetPostedTxn (invoice);
        if (trans)
            invoices.emplace (trans, invoice);
    }

    g_list_free (invoice_list);
    return invoices;
}

GncInvoice *
invoice_for_split (Split *split,
                   Transaction *trans,
                   const InvoiceByTransaction& invoice_map)
{
    auto lot = xaccSplitGetLot (split);
    if (lot)
    {
        auto invoice = gncInvoiceGetInvoiceFromLot (lot);
        if (invoice)
            return invoice;
    }

    auto iter = invoice_map.find (trans);
    return iter == invoice_map.end () ? nullptr : iter->second;
}

void
add_ref_once (ObjectRefs& refs, const std::string& dimension, const std::string& id)
{
    if (id.empty ())
        return;

    auto ref = std::make_pair (dimension, id);
    if (std::find (refs.begin (), refs.end (), ref) == refs.end ())
        refs.push_back (std::move (ref));
}

void
add_owner_object (BusinessObjects& objects,
                  const GncOwner *owner,
                  ObjectRefs& refs)
{
    if (!owner)
        return;

    auto end_owner = gncOwnerGetEndOwner (owner);
    if (!end_owner)
        return;

    auto id = str_or_empty (gncOwnerGetID (end_owner));
    auto name = str_or_empty (gncOwnerGetName (end_owner));

    switch (gncOwnerGetType (end_owner))
    {
    case GNC_OWNER_CUSTOMER:
        if (!id.empty ())
        {
            objects.customers.emplace (id, "Kund: " + name);
            add_ref_once (refs, "8", id);
        }
        break;
    case GNC_OWNER_VENDOR:
        if (!id.empty ())
        {
            objects.vendors.emplace (id, "Leverantör: " + name);
            add_ref_once (refs, "9", id);
        }
        break;
    default:
        break;
    }
}

ObjectRefs
objects_for_split (Split *split,
                   Transaction *trans,
                   const InvoiceByTransaction& invoice_map,
                   BusinessObjects& objects)
{
    ObjectRefs refs;
    auto invoice = invoice_for_split (split, trans, invoice_map);

    if (invoice)
    {
        auto invoice_id = str_or_empty (gncInvoiceGetID (invoice));
        if (!invoice_id.empty ())
        {
            const auto owner_type = gncInvoiceGetOwnerType (invoice);
            std::string label = owner_type == GNC_OWNER_VENDOR ?
                "Leverantörsfaktura: #" : "Kundfaktura: #";
            objects.invoices.emplace (invoice_id, label + invoice_id);
            add_ref_once (refs, "10", invoice_id);
        }
        add_owner_object (objects, gncInvoiceGetOwner (invoice), refs);
    }
    else
    {
        GncOwner owner;
        if (gncOwnerGetOwnerFromTxn (trans, &owner))
            add_owner_object (objects, &owner, refs);
    }

    return refs;
}

void
collect_business_objects (const std::set<Transaction*, TransactionLess>& transactions,
                          const InvoiceByTransaction& invoice_map,
                          BusinessObjects& objects)
{
    for (auto trans : transactions)
    {
        for (auto node = xaccTransGetSplitList (trans); node; node = g_list_next (node))
        {
            auto split = GNC_SPLIT (node->data);
            if (is_zero_at_cents (xaccSplitGetValue (split)))
                continue;

            objects_for_split (split, trans, invoice_map, objects);
        }
    }
}

std::string
format_object_refs (const ObjectRefs& refs)
{
    std::ostringstream ss;
    bool first = true;

    ss << '{';
    for (const auto& ref : refs)
    {
        if (!first)
            ss << ' ';
        /* Strict SIE parsers expect dimension ids as integers, not quoted
         * strings, even though object ids themselves are strings. */
        ss << ref.first << ' ' << sie_quote (ref.second);
        first = false;
    }
    ss << '}';
    return ss.str ();
}

void
write_header (std::ostream& ss, const GncSie4ExportSettings *settings)
{
    auto account_plan = stripped_string (settings->account_plan);
    auto currency_code = normalized_currency_code (settings->currency_code);

    if (!valid_account_plan (account_plan))
        throw std::runtime_error (_("The SIE account plan must be BAS95, BAS96, EUBAS97, NE2007, or BAS2xxx."));
    if (!valid_currency_code (currency_code))
        throw std::runtime_error (_("The SIE accounting currency must be a three-letter ISO 4217 code."));

    ss << "#FLAGGA 0\n";
    ss << "#FORMAT PC8\n";
    ss << "#SIETYP 4\n";
    ss << "#PROGRAM " << sie_quote ("GnuCash") << ' ' << PROJECT_VERSION << "\n";
    ss << "#GEN " << sie_date (gnc_time (nullptr)) << "\n";
    if (settings->file_id && *settings->file_id)
        ss << "#FNR " << sie_quote (settings->file_id) << "\n";
    ss << "#FNAMN " << sie_quote (str_or_empty (settings->company_name)) << "\n";
    ss << "#ADRESS "
       << sie_quote (str_or_empty (settings->contact)) << ' '
       << sie_quote (str_or_empty (settings->street_address)) << ' '
       << sie_quote (str_or_empty (settings->postal_address)) << ' '
       << sie_quote (str_or_empty (settings->phone)) << "\n";
    ss << "#RAR 0 " << sie_date (settings->current_start)
       << ' ' << sie_date (settings->current_end) << "\n";
    ss << "#RAR -1 " << sie_date (settings->previous_start)
       << ' ' << sie_date (settings->previous_end) << "\n";
    if (settings->organization_number && *settings->organization_number)
        ss << "#ORGNR " << settings->organization_number << "\n";
    ss << "#OMFATTN " << sie_date (settings->current_end) << "\n";
    if (!account_plan.empty ())
        ss << "#KPTYP " << account_plan << "\n";
    if (!currency_code.empty ())
        ss << "#VALUTA " << currency_code << "\n";
}

void
write_accounts (std::ostream& ss, const std::vector<AccountInfo>& accounts)
{
    for (const auto& account : accounts)
    {
        ss << "#KONTO " << account.code << ' ' << sie_quote (account.name) << "\n";
        ss << "#KTYP " << account.code << ' ' << account_type_to_ktyp (account.account) << "\n";
    }
}

void
write_amount_rows (std::ostream& ss,
                   const gchar *record_name,
                   int year_code,
                   const AmountMap& amounts,
                   const std::vector<AccountInfo>& accounts,
                   bool balance_accounts,
                   bool include_zero)
{
    for (const auto& account : accounts)
    {
        if ((account.code_num < 3000) != balance_accounts)
            continue;

        auto iter = amounts.find (account.code);
        auto value = iter == amounts.end () ? gnc_numeric_zero () : iter->second;
        /* SIE always declares the account master data with #KONTO. This switch
         * only controls whether zero-valued #IB/#UB/#RES rows are emitted. */
        if (!include_zero && is_zero_at_cents (value))
            continue;

        ss << record_name << ' ' << year_code << ' ' << account.code
           << ' ' << format_amount (value) << "\n";
    }
}

void
write_dimensions (std::ostream& ss, const BusinessObjects& objects)
{
    /* Optional business dimensions are export-local: 8=customer, 9=vendor,
     * 10=invoice. They are only emitted for the full SIE4 transaction export. */
    ss << "#DIM 8 Kund\n";
    ss << "#DIM 9 Leverantör\n";
    ss << "#DIM 10 Faktura\n";

    for (const auto& object : objects.customers)
        ss << "#OBJEKT 8 " << sie_quote (object.first) << ' '
           << sie_quote (object.second) << "\n";
    for (const auto& object : objects.vendors)
        ss << "#OBJEKT 9 " << sie_quote (object.first) << ' '
           << sie_quote (object.second) << "\n";
    for (const auto& object : objects.invoices)
        ss << "#OBJEKT 10 " << sie_quote (object.first) << ' '
           << sie_quote (object.second) << "\n";
}

std::vector<SplitLine>
sorted_transaction_splits (Transaction *trans, const AccountInfoMap& account_map)
{
    std::vector<SplitLine> splits;

    for (auto node = xaccTransGetSplitList (trans); node; node = g_list_next (node))
    {
        auto split = GNC_SPLIT (node->data);
        if (is_zero_at_cents (xaccSplitGetValue (split)))
            continue;

        auto account = xaccSplitGetAccount (split);
        const auto& account_info = require_account_info (account_map, account);
        splits.push_back ({split, &account_info});
    }

    std::stable_sort (splits.begin (), splits.end (),
                      [](const auto& left, const auto& right) {
                          if (left.account_info->code_num != right.account_info->code_num)
                              return left.account_info->code_num < right.account_info->code_num;
                          return left.account_info->code < right.account_info->code;
                      });

    return splits;
}

void
write_transactions (std::ostream& ss,
                    const std::vector<VoucherInfo>& vouchers,
                    const AccountInfoMap& account_map,
                    const InvoiceByTransaction& invoice_map,
                    BusinessObjects& objects,
                    const std::string& voucher_series,
                    bool include_business_dimensions)
{
    for (const auto& voucher : vouchers)
    {
        auto trans = voucher.trans;
        gnc_numeric balance = gnc_numeric_zero ();
        auto date = xaccTransGetDate (trans);
        auto date_entered = xaccTransGetDateEntered (trans);
        auto splits = sorted_transaction_splits (trans, account_map);

        ss << "#VER " << voucher_series << ' '
           << voucher.number << ' ' << sie_date (date) << ' '
           << sie_quote (str_or_empty (xaccTransGetDescription (trans)));
        if (date_entered != 0)
            ss << ' ' << sie_date (date_entered);
        ss << "\n{\n";

        for (const auto& line : splits)
        {
            auto split = line.split;
            auto value = xaccSplitGetValue (split);
            auto refs = include_business_dimensions ?
                objects_for_split (split, trans, invoice_map, objects) : ObjectRefs{};

            ss << "#TRANS " << line.account_info->code << ' '
               << format_object_refs (refs) << ' '
               << format_amount (value) << ' '
               << sie_date (date) << ' '
               << sie_quote (str_or_empty (xaccSplitGetMemo (split))) << "\n";

            balance = gnc_numeric_add (balance, value, GNC_DENOM_AUTO,
                                       GNC_HOW_DENOM_LCD | GNC_HOW_RND_NEVER);
            if (gnc_numeric_check (balance) != GNC_ERROR_OK)
                throw std::runtime_error (_("Unable to test transaction balance for SIE4 export."));
        }

        ss << "}\n";
        if (!is_zero_at_cents (balance))
        {
            auto date_text = sie_date (date);
            auto description = str_or_empty (xaccTransGetDescription (trans));
            throw std::runtime_error (
                format_user_message (_("Imbalanced transaction in SIE4 export: %s %s"),
                                     date_text.c_str (),
                                     description.c_str ()));
        }
    }
}

std::string
build_export (const GncSie4ExportSettings *settings,
              GncSie4ExportResult *result)
{
    auto book = gnc_get_current_book ();
    AccountInfoMap account_map;
    auto accounts = collect_accounts (book, account_map, settings);

    if (accounts.empty ())
        throw std::runtime_error (_("No accounts with numeric account codes were found."));

    auto currency_code = normalized_currency_code (settings->currency_code);
    if (!valid_currency_code (currency_code))
        throw std::runtime_error (_("The SIE accounting currency must be a three-letter ISO 4217 code."));
    validate_export_currency (accounts, settings, currency_code);

    auto voucher_series = stripped_string (settings->voucher_series);
    if (voucher_series.empty ())
        throw std::runtime_error (_("Voucher series is required for SIE4 export."));
    if (!valid_voucher_series (voucher_series))
        throw std::runtime_error (_("Voucher series must not contain spaces or quote marks for SIE4 export."));

    AmountMap ib_current;
    AmountMap ib_previous;
    AmountMap ub_current;
    AmountMap ub_previous;
    AmountMap res_current;
    AmountMap res_previous;
    collect_totals (accounts, settings, ib_current, ib_previous,
                    ub_current, ub_previous, res_current, res_previous);

    std::set<Transaction*, TransactionLess> transactions;
    InvoiceByTransaction invoice_map;
    BusinessObjects objects;
    transactions = collect_transactions (accounts, settings);
    invoice_map = collect_invoice_map (book);
    if (settings->include_business_dimensions)
        collect_business_objects (transactions, invoice_map, objects);
    auto vouchers = build_voucher_infos (transactions, settings, result);

    std::ostringstream ss;
    write_header (ss, settings);
    ss << "\n";
    write_accounts (ss, accounts);
    ss << "\n";
    if (settings->include_business_dimensions)
    {
        write_dimensions (ss, objects);
        ss << "\n";
    }
    write_amount_rows (ss, "#IB", 0, ib_current, accounts, true, settings->include_zero_balances);
    write_amount_rows (ss, "#IB", -1, ib_previous, accounts, true, settings->include_zero_balances);
    ss << "\n";
    write_amount_rows (ss, "#UB", 0, ub_current, accounts, true, settings->include_zero_balances);
    write_amount_rows (ss, "#UB", -1, ub_previous, accounts, true, settings->include_zero_balances);
    ss << "\n";
    write_amount_rows (ss, "#RES", 0, res_current, accounts, false, settings->include_zero_balances);
    write_amount_rows (ss, "#RES", -1, res_previous, accounts, false, settings->include_zero_balances);
    ss << "\n";
    write_transactions (ss, vouchers, account_map, invoice_map, objects,
                        voucher_series,
                        settings->include_business_dimensions);

    return ss.str ();
}

gboolean
write_cp437_file (const gchar *file_name, const std::string& utf8, GError **error)
{
    gsize bytes_read = 0;
    gsize bytes_written = 0;
    /* SIE declares PC8/Code Page 437. Transliteration keeps ordinary UTF-8
     * punctuation from aborting the whole export. */
    auto converted = g_convert_with_fallback (utf8.c_str (), utf8.size (),
                                              "CP437//TRANSLIT", "UTF-8", "?",
                                              &bytes_read, &bytes_written, error);
    if (!converted)
        return FALSE;

    auto ok = g_file_set_contents (file_name, converted,
                                   static_cast<gssize> (bytes_written), error);
    g_free (converted);
    return ok;
}
}

extern "C" gboolean
gnc_sie4_export (const GncSie4ExportSettings *settings,
                 GncSie4ExportResult *result,
                 GError **error)
{
    g_return_val_if_fail (settings != nullptr, FALSE);
    g_return_val_if_fail (settings->file_name != nullptr, FALSE);

    if (result)
        *result = GncSie4ExportResult{};

    try
    {
        auto content = build_export (settings, result);
        return write_cp437_file (settings->file_name, content, error);
    }
    catch (const std::exception& err)
    {
        g_set_error (error, sie4_error_quark (), 0, "%s", err.what ());
        return FALSE;
    }
}
