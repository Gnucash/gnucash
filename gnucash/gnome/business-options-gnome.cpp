/*
 * business-options.c -- Initialize Business Options
 *
 * Written By: Derek Atkins <warlord@MIT.EDU>
 * Copyright (C) 2002,2006 Derek Atkins
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

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include "business-options-gnome.h"

#include <config.h> // for scanf format string
#include <qof.h>
#include <business-gnome-utils.h>
#include <gnc-ui-util.h> // for gnc_get_current_book
#include <gnc-general-search.h> // for GNC_GENERAL_SEARCH
#include "dialog-utils.h" // for gnc_builder_add_from_file

#include "gnc-report-combo.h"

#include <iostream>
#include <sstream>
#include <exception>

#include "gnc-option-gtk-ui.hpp"
#include "gncOwner.h"
#include <gnc-option.hpp>
#define FUNC_NAME G_STRFUNC


static inline GncOwnerType
ui_type_to_owner_type(GncOptionUIType ui_type)
{
    if (ui_type == GncOptionUIType::CUSTOMER)
        return GNC_OWNER_CUSTOMER;
    if (ui_type == GncOptionUIType::VENDOR)
        return GNC_OWNER_VENDOR;
    if (ui_type == GncOptionUIType::EMPLOYEE)
        return GNC_OWNER_EMPLOYEE;
    if (ui_type == GncOptionUIType::JOB)
        return GNC_OWNER_JOB;

    std::ostringstream oss;
    oss << "UI type " << static_cast<unsigned int>(ui_type) << " could not be converted to owner type\n";
    throw std::invalid_argument(oss.str());
}


class GncGtkOwnerUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkOwnerUIItem(GtkWidget* widget, GncOptionUIType type) :
        GncOptionGtkUIItem(widget, type) {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto owner = option.get_value<const GncOwner*>();
        gnc_owner_set_owner(get_widget(), owner);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        GncOwner owner{};
        gnc_owner_get_owner(get_widget(), &owner);
        if (owner.type == ui_type_to_owner_type(option.get_ui_type()))
            option.set_value<const GncOwner*>(&owner);
    }
};

template<> void
create_option_widget<GncOptionUIType::OWNER>(GncOption& option,
                                                GtkGrid *page_box, int row)
{
    GncOwner owner{};
    auto ui_type{option.get_ui_type()};
    owner.type = ui_type_to_owner_type(ui_type);
    auto enclosing{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    auto widget = gnc_owner_select_create(nullptr, enclosing,
                                          gnc_get_current_book(),
                                          &owner);

    option.set_ui_item(std::make_unique<GncGtkOwnerUIItem>(widget, ui_type));
    option.set_ui_item_from_option();
    g_signal_connect (G_OBJECT (widget), "changed",
                      G_CALLBACK (gnc_option_changed_widget_cb), &option);
    set_name_label(option, page_box, row, false);
    set_tool_tip(option, enclosing);
    grid_attach_widget(page_box, enclosing, row);
}

class GncGtkInvoiceUIItem : public GncOptionGtkUIItem
{
public:
    explicit GncGtkInvoiceUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem(widget, GncOptionUIType::INVOICE) {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto instance{option.get_value<const QofInstance*>()};
        if (instance)
            gnc_general_search_set_selected(GNC_GENERAL_SEARCH(get_widget()),
                                            GNC_INVOICE(instance));
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        option.set_value(qof_instance_cast(gnc_general_search_get_selected(GNC_GENERAL_SEARCH(get_widget()))));
    }
};

template<> void
create_option_widget<GncOptionUIType::INVOICE>(GncOption& option,
                                               GtkGrid *page_box, int row)
{
    auto enclosing{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    auto widget{gnc_invoice_select_create(enclosing, gnc_get_current_book(),
                                          nullptr, nullptr, nullptr)};

    option.set_ui_item(std::make_unique<GncGtkInvoiceUIItem>(widget));
    option.set_ui_item_from_option();
    g_signal_connect(G_OBJECT (widget), "changed",
                     G_CALLBACK (gnc_option_changed_widget_cb), &option);

    set_name_label(option, page_box, row, false);
    set_tool_tip(option, enclosing);
    grid_attach_widget(page_box, enclosing, row);
}

class GncGtkTaxTableUIItem : public GncOptionGtkUIItem
{
public:
    explicit GncGtkTaxTableUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem(widget, GncOptionUIType::TAX_TABLE) {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto taxtable{option.get_value<const QofInstance*>()};
        if (taxtable)
            gnc_simple_combo_set_value(GTK_COMBO_BOX(get_widget()),
                                       GNC_TAXTABLE(taxtable));
        else
            gnc_simple_combo_set_value(GTK_COMBO_BOX(get_widget()),
                                       nullptr);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto taxtable{gnc_simple_combo_get_value(GTK_COMBO_BOX(get_widget()))};
        option.set_value<const QofInstance*>(qof_instance_cast(taxtable));
    }
};

template<> void
create_option_widget<GncOptionUIType::TAX_TABLE>(GncOption& option,
                                                 GtkGrid *page_box, int row)
{
    constexpr const char* glade_file{"business-options-gnome.glade"};
    constexpr const char* glade_store{"taxtable_store"};
    constexpr const char* glade_menu{"taxtable_menu"};
    auto builder{gtk_builder_new()};
    gnc_builder_add_from_file(builder, glade_file, glade_store);
    gnc_builder_add_from_file(builder, glade_file, glade_menu);
    auto widget{GTK_WIDGET(gtk_builder_get_object(builder, glade_menu))};
    gnc_taxtables_combo(GTK_COMBO_BOX(widget), gnc_get_current_book(), TRUE,
                        nullptr);
    option.set_ui_item(std::make_unique<GncGtkTaxTableUIItem>(widget));
    option.set_ui_item_from_option();
    g_object_unref(builder); // Needs to wait until after widget has been reffed.
    g_signal_connect (G_OBJECT (widget), "changed",
                      G_CALLBACK (gnc_option_changed_widget_cb), &option);

    wrap_widget(option, widget, page_box, row);
}

class GncGtkInvReportUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkInvReportUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem(widget, GncOptionUIType::INV_REPORT) {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        std::string guid_string;
        auto str{option.get_value<std::string>()};

        if (str.empty())
        {
            static const std::string default_guid_string(gnc_get_builtin_default_invoice_print_report ());
            guid_string = default_guid_string + "/ ";
        }
        else
            guid_string = str;

        gnc_report_combo_set_active_guid_name (GNC_REPORT_COMBO(get_widget()),
                                               guid_string.c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto report_guid_name = gnc_report_combo_get_active_guid_name (GNC_REPORT_COMBO(get_widget()));
        option.set_value(std::string{report_guid_name});
        g_free (report_guid_name);
    }
};

template<> void
create_option_widget<GncOptionUIType::INV_REPORT>(GncOption& option,
                                                  GtkGrid *page_box,
                                                  int row)
{
    constexpr const char* inv_report{"gnc:custom-report-invoice-template-guids"};
    auto widget = gnc_default_invoice_report_combo (inv_report);
    option.set_ui_item(std::make_unique<GncGtkInvReportUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect (G_OBJECT (widget), "changed",
                      G_CALLBACK (gnc_option_changed_widget_cb), &option);

    wrap_widget (option, widget, page_box, row);
}

void
gnc_business_options_gnome_initialize(void)
{
    GncOptionUIFactory::set_func(GncOptionUIType::OWNER,
                                 create_option_widget<GncOptionUIType::OWNER>);
    GncOptionUIFactory::set_func(GncOptionUIType::CUSTOMER,
                                 create_option_widget<GncOptionUIType::OWNER>);
    GncOptionUIFactory::set_func(GncOptionUIType::VENDOR,
                                 create_option_widget<GncOptionUIType::OWNER>);
    GncOptionUIFactory::set_func(GncOptionUIType::EMPLOYEE,
                                 create_option_widget<GncOptionUIType::OWNER>);
    GncOptionUIFactory::set_func(GncOptionUIType::JOB,
                                 create_option_widget<GncOptionUIType::OWNER>);
    GncOptionUIFactory::set_func(GncOptionUIType::INVOICE,
                                 create_option_widget<GncOptionUIType::INVOICE>);
    GncOptionUIFactory::set_func(GncOptionUIType::TAX_TABLE,
                                 create_option_widget<GncOptionUIType::TAX_TABLE>);
    GncOptionUIFactory::set_func(GncOptionUIType::INV_REPORT,
                                 create_option_widget<GncOptionUIType::INV_REPORT>);
}
