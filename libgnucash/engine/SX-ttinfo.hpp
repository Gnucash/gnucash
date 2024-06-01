/********************************************************************\
 * SX-ttinfo.h -- Template Transaction manipulation functions       *
 *               for scheduled transactions                         *
 * Copyright (C) 2001 Robert Merkel <rgmerk@mira.net>               *
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

#ifndef GNC_SX_TTINFO_H
#define GNC_SX_TTINFO_H

#include <glib.h>
#include "qof.h"
#include "SchedXaction.h"
#include "Account.h"
#include "gnc-commodity.h"
#include "gnc-numeric.hpp"

#include <vector>
#include <optional>
#include <string>
#include <algorithm>
#include <memory>

struct OptionalString
{
    const char* operator* () const { return m_str ? m_str->c_str() : nullptr; };
    void operator= (const char* str) { if (str) m_str = str; else reset(); };
    void reset () { m_str = std::nullopt; };
protected:
    std::optional<std::string> m_str;
};

struct OptionalStringFromNumeric : public OptionalString
{
    using OptionalString::operator=;
    void operator= (std::optional<gnc_numeric> num)
    { if (num) m_str = GncNumeric (*num).to_string(); else reset(); }
};

struct TTSplitInfo
{
    OptionalString m_action;
    OptionalString m_memo;
    OptionalStringFromNumeric m_credit_formula, m_debit_formula;
    Account *m_acc = nullptr;

    const char* get_action () const { return *m_action; };
    const char* get_memo () const { return *m_memo; };
    const Account* get_account () const { return m_acc; };
    const char* get_credit_formula () const { return *m_credit_formula; };
    const char* get_debit_formula () const { return *m_debit_formula; };

    void set_action (const char *action) { m_action = action; };
    void set_memo (const char *memo) { m_memo = memo; };
    void set_account (Account *acc) { m_acc = acc; };
    void set_credit_formula (const char *credit_formula) { set_formulas (nullptr, credit_formula); };
    void set_debit_formula (const char *debit_formula) { set_formulas (debit_formula, nullptr); };
    void set_credit_formula_numeric (gnc_numeric num) { set_formulas_numeric ({}, num); };
    void set_debit_formula_numeric (gnc_numeric num) { set_formulas_numeric (num, {}); };

private:
    void set_formulas (const char* debit, const char *credit)
    {
        m_debit_formula = debit;
        m_credit_formula = credit;
    }
    void set_formulas_numeric (std::optional<gnc_numeric> debit, std::optional<gnc_numeric> credit)
    {
        m_debit_formula = debit;
        m_credit_formula = credit;
    }
};

using TTSplitInfoPtr = std::shared_ptr<TTSplitInfo>;
using TTSplitInfoVec = std::vector<TTSplitInfoPtr>;

struct TTInfo
{
    OptionalString m_description;
    OptionalString m_num;
    OptionalString m_notes;
    gnc_commodity *m_common_currency = nullptr;
    TTSplitInfoVec m_splits;

    const char* get_description () const { return *m_description; };
    const char* get_num () const { return *m_num; };
    const char* get_notes () const { return *m_notes; };
    gnc_commodity* get_currency () const { return m_common_currency; };
    const TTSplitInfoVec& get_template_splits () const { return m_splits; };

    void set_description (const char *description) { m_description = description; };
    void set_num (const char *num) { m_num = num; };
    void set_notes (const char *notes) { m_notes = notes; };
    void set_currency (gnc_commodity *currency) { m_common_currency = currency; };
    void clear_template_splits () { m_splits.clear(); };
    void append_template_split (TTSplitInfoPtr& ttsi) { m_splits.push_back (ttsi); };
;
};

using TTInfoPtr = std::shared_ptr<TTInfo>;
using TTInfoVec = std::vector<TTInfoPtr>;

#endif
