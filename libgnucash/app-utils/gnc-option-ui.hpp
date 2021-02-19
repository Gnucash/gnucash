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

#include <memory>
#include "gnc-option-uitype.hpp"

class GncOption;
class GncOptionUIItem;
using GncOptionUIItemPtr = std::unique_ptr<GncOptionUIItem>;

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
    GncOptionUIItem(GncOptionUIType type) : m_type{type} {}
    virtual ~GncOptionUIItem() = default;
    GncOptionUIType get_ui_type() const noexcept { return m_type; }
    virtual void set_dirty(bool status) noexcept { m_dirty = status; }
    virtual bool get_dirty() const noexcept { return m_dirty; }
    virtual void set_selectable(bool selectable) const noexcept = 0;
    virtual void clear_ui_item() = 0;
    virtual void set_ui_item_from_option(GncOption& option) noexcept = 0;
    virtual void set_option_from_ui_item(GncOption& option) noexcept = 0;
private:
    GncOptionUIType m_type;
    bool m_dirty = false;
};

#endif //GNC_OPTION_UI_HPP__
