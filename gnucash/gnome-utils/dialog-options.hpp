/********************************************************************\
 * dialog-options.hpp -- GNOME option handling                      *
 * Copyright (C) 1998-2000 Linas Vepstas                            *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>         *
 * Copyright (c) 2011 Robert Fewell                                 *
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
\********************************************************************/

#ifndef GNC_DIALOG_OPTIONS_HPP_
#define GNC_DIALOG_OPTIONS_HPP_

#include <vector>

#include <gnc-option-uitype.hpp>
#include <gnc-option-ui.hpp>
#include "dialog-options.h"

typedef GtkWidget* (*WidgetCreateFunc)(GncOption&, GtkGrid*, GtkLabel*, char*,
                                       GtkWidget**, bool*);
class GncOptionUIFactory
{
public:
    static void set_func(GncOptionUIType type, WidgetCreateFunc func);
    static GtkWidget* create(GncOption&, GtkGrid*, GtkLabel*, char*,
                             GtkWidget**, bool*);
private:
    static std::vector<WidgetCreateFunc> s_registry;
};

class GncOptionGtkUIItem : public GncOptionUIItem
{
public:
    GncOptionGtkUIItem(GtkWidget* widget, GncOptionUIType type);
    GncOptionGtkUIItem(const GncOptionGtkUIItem& item);
    GncOptionGtkUIItem(GncOptionGtkUIItem&&) = default;
    virtual ~GncOptionGtkUIItem() override;
    void clear_ui_item() override;
    void set_widget(GtkWidget* widget);
    virtual GtkWidget* const get_widget() const { return m_widget; }
    static WidgetCreateFunc option_widget_factory(GncOption& option,
                                                  GtkGrid* page,
                                                  GtkLabel* name,
                                                  char* description,
                                                  GtkWidget** enclosing,
                                                  bool* packed);
private:
    GtkWidget* m_widget;
};

template<GncOptionUIType type> GtkWidget*
create_option_widget(GncOption& option, GtkGrid*, GtkLabel*, char*, GtkWidget**,
                     bool*);

template <typename Instance> inline const QofInstance*
qof_instance_cast(Instance inst)
{
    static_assert(std::is_pointer_v<Instance>, "Pointers Only!");
    return reinterpret_cast<const QofInstance*>(inst);
}

#endif // GNC_DIALOG_OPTIONS_HPP_
