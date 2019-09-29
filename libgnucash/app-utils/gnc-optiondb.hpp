/********************************************************************\
 * gnc-optiondb.hpp -- Collection of GncOption objects              *
 * Copyright (C) 2019 John Ralls <jralls@ceridwen.us>               *
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

#ifndef GNC_OPTIONDB_HPP_
#define GNC_OPTIONDB_HPP_

#include "gnc-option.hpp"
#include <functional>
#include <boost/optional.hpp>

class GncOptionDB;

using GncOptionVec = std::vector<GncOption>;
using GncOptionSection = std::pair<std::string, GncOptionVec>;
class GncOptionDB
{
public:
    GncOptionDB();
    GncOptionDB(QofBook* book);
    ~GncOptionDB() = default;

    void save_to_book(QofBook* book, bool do_clear) const;
    int num_sections() const noexcept { return m_sections.size(); }
    bool get_changed() const noexcept { return m_dirty; }
    void register_option(const char* section, GncOption&& option);
    void unregister_option(const char* section, const char* name);
    void set_default_section(const char* section);
    const GncOptionSection* const get_default_section() const noexcept;
    std::string lookup_string_option(const char* section,
                                            const char* name);
    bool set_option(const char* section, const char* name, SCM value);
    void set_selectable(const char* section, const char* name);
    void commit();
private:
    boost::optional<GncOptionSection&> find_section(const char* section);
    boost::optional<GncOption&> find_option(const char* section, const char* name);

    boost::optional<GncOptionSection&> m_default_section;
    std::vector<GncOptionSection> m_sections;
    bool m_dirty = false;

    std::function<void*()> m_get_ui_value;
    std::function<void(void*)> m_set_ui_value;
};

using GncOptionDBPtr = std::unique_ptr<GncOptionDB>;
/**
 * Create an empty option database.
 *
 * It would be nice to use a std::shared_ptr here but Swig doesn't implement
 * that for Guile.
 * @return A newly allocated GncOptionDB. Use delete to destroy it.
 */
GncOptionDBPtr gnc_option_db_new(void);

void gnc_register_string_option(const GncOptionDBPtr& db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, std::string value);

void gnc_register_text_option(const GncOptionDBPtr& db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, std::string value);

void gnc_register_budget_option(const GncOptionDBPtr& db, const char* section,
                                const char* name, const char* key,
                                const char* doc_string, GncBudget* value);

void gnc_register_commodity_option(const GncOptionDBPtr& db, const char* section,
                                   const char* name, const char* key,
                                   const char* doc_string,
                                   gnc_commodity* value);

void gnc_register_currency_option(const GncOptionDBPtr& db, const char* section,
                                  const char* name, const char* key,
                                  const char* doc_string, gnc_commodity* value);



#endif //GNC_OPTIONDB_HPP_
