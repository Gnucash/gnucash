/********************************************************************\
 * gnc-option-ui.hpp -- UI association for GncOption                *
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

#ifndef GNC_OPTION_UI_HPP_
#define GNC_OPTION_UI_HPP_

#include "gnc-option-uitype.hpp"
template <typename UIType>
class GncUIItem
{
public:
    GncUIItem(UIType* widget) : m_widget{widget} {}
    UIType* m_widget;
};

class GncUIType;
using OptionUIItem = GncUIItem<GncUIType>;
using OptionSyncFunc = std::function<void(OptionUIItem&, GncOption&)>;
/**
 * Holds a pointer to the UI item which will control the option and an enum
 * representing the type of the option for dispatch purposes; all of that
 * happens in gnucash/gnome-utils/dialog-options and
 * gnucash/gnome/business-option-gnome.
 *
 * This class takes no ownership responsibility, so calling code is responsible
 * for ensuring that the UI_Item is alive. For convenience the public
 * clear_ui_item function can be used as a weak_ptr's destruction callback to
 * ensure that the ptr will be nulled if the ui_item is destroyed elsewhere.
 */
class GncOptionUIItem
{
public:
    GncOptionUIItem(OptionUIItem&& ui_item, GncOptionUIType type,
                    OptionSyncFunc to_ui, OptionSyncFunc from_ui) :
        m_ui_item{std::move(ui_item)}, m_ui_type{type},
        m_set_ui_item_from_option{to_ui}, m_set_option_from_ui_item{from_ui} {}
    GncOptionUIItem(GncOptionUIType ui_type) :
        m_ui_item{nullptr}, m_ui_type{ui_type} {}
    GncOptionUIItem(const GncOptionUIItem&) = default;
    GncOptionUIItem(GncOptionUIItem&&) = default;
    ~GncOptionUIItem() = default;
    GncOptionUIItem& operator=(const GncOptionUIItem&) = default;
    GncOptionUIItem& operator=(GncOptionUIItem&&) = default;
    GncOptionUIType get_ui_type() const { return m_ui_type; }
    const OptionUIItem& get_ui_item() const {return m_ui_item; }
    void clear_ui_item() { m_ui_item = nullptr; }
    void set_ui_item(OptionUIItem&& ui_item)
    {
        if (m_ui_type == GncOptionUIType::INTERNAL)
        {
            std::string error{"INTERNAL option, setting the UI item forbidden."};
            throw std::logic_error(std::move(error));
        }
        m_ui_item = std::move(ui_item);
    }
    void set_ui_item_from_option(GncOption& option)
    {
        m_set_ui_item_from_option(m_ui_item, option);
    }
    void set_option_from_ui_item(GncOption& option)
    {
        m_set_option_from_ui_item(m_ui_item, option);
    }
private:
    OptionUIItem m_ui_item;
    GncOptionUIType m_ui_type;
    OptionSyncFunc m_set_ui_item_from_option;
    OptionSyncFunc m_set_option_from_ui_item;
};

using GncOptionUIItemPtr = std::unique_ptr<GncOptionUIItem>;

#endif //GNC_OPTION_UI_HPP__
