/********************************************************************\
 * gnc-option-gtk-ui.cpp -- Gtk Widgets for manipulating options    *
  * Copyright 2022 John Ralls <jralls@ceridwen.us>                  *
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

#include <gnc-option.hpp>
#include <gnc-option-impl.hpp>
#include "gnc-option-gtk-ui.hpp"
#include <config.h>  // for scanf format string
#include <memory>
#include <qof.h>
#include <gnc-engine.h> // for GNC_MOD_GUI
#include <gnc-commodity.h> // for GNC_COMMODITY
#include "gnc-account-sel.h" // for GNC_ACCOUNT_SEL
#include "gnc-currency-edit.h" //for GNC_CURRENCY_EDIT
#include "gnc-commodity-edit.h" //for gnc_commodity_get_string
#include "gnc-date-edit.h" // for gnc_date_edit
#include "gnc-date-format.h" //for GNC_DATE_FORMAT
#include "gnc-general-select.h" // for GNC_GENERAL_SELECT
#include "gnc-option-uitype.hpp"
#include "gnc-tree-view-account.h" // for GNC_TREE_VIEW_ACCOUNT
#include "gnc-tree-model-budget.h" // for gnc_tree_model_budget
#include "misc-gnome-utils.h" // for xxxgtk_textview_set_text

/*Something somewhere in windows.h defines ABSOLUTE to something and
 *that contaminates using it in RelativeDateType.  Undef it.
 */
#ifdef ABSOLUTE
#undef ABSOLUTE
#endif

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

//Init the class static.
std::vector<WidgetCreateFunc> GncOptionUIFactory::s_registry{static_cast<size_t>(GncOptionUIType::MAX_VALUE)};
bool GncOptionUIFactory::s_initialized{false};
static void gnc_options_ui_factory_initialize (void);

void
GncOptionUIFactory::set_func(GncOptionUIType type, WidgetCreateFunc func)
{
    s_registry[static_cast<size_t>(type)] = func;
}

void
GncOptionUIFactory::create(GncOption& option, GtkGrid* page, int row)
{
    if (!s_initialized)
    {
        gnc_options_ui_factory_initialize();
        s_initialized = true;
    }
    auto type{option.get_ui_type()};
    auto func{s_registry[static_cast<size_t>(type)]};
    if (func)
        func(option, page, row);
    else
        PERR("No function registered for type %d", static_cast<int>(type));
}

GncOptionGtkUIItem::GncOptionGtkUIItem(GtkWidget* widget,
                                       GncOptionUIType type) :
    GncOptionUIItem{type},
    m_widget{static_cast<GtkWidget*>(g_object_ref(widget))} {}

GncOptionGtkUIItem::GncOptionGtkUIItem(const GncOptionGtkUIItem& item) :
    GncOptionUIItem{item.get_ui_type()},
    m_widget{static_cast<GtkWidget*>(g_object_ref(item.get_widget()))} {}

GncOptionGtkUIItem::~GncOptionGtkUIItem()
{
    if (m_widget)
        g_object_unref(m_widget);
}

void
GncOptionGtkUIItem::set_selectable(bool selectable) const noexcept
{
    if (m_widget)
        gtk_widget_set_sensitive (m_widget, selectable);
}

void
GncOptionGtkUIItem::clear_ui_item()
{
    if (m_widget)
        g_object_unref(m_widget);
    m_widget = nullptr;
}

void
GncOptionGtkUIItem::set_widget(GtkWidget* widget)
{
    if (get_ui_type() == GncOptionUIType::INTERNAL)
    {
        std::string error{"INTERNAL option, setting the UI item forbidden."};
        throw std::logic_error(error);
    }

    if (m_widget)
        g_object_unref(m_widget);

    m_widget = static_cast<GtkWidget*>(g_object_ref(widget));
}

SCM
GncOptionGtkUIItem::get_widget_scm_value(const GncOption& option) const
{
    return SCM_BOOL_F;
}
/* ****************************************************************/
/* Option Widgets                                      */
/* ***************************************************************/

static inline GtkWidget* const
option_get_gtk_widget(const GncOption* option)
{
    if (!option) return nullptr;
    auto ui_item{dynamic_cast<const GncOptionGtkUIItem*>(option->get_ui_item())};
    if (ui_item)
        return ui_item->get_widget();

    return nullptr;
}

static inline void
wrap_check_button (const GncOption& option, GtkWidget* widget, GtkGrid* page_box, int row)
{
    auto enclosing{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
    set_tool_tip(option, enclosing);
    gtk_widget_show_all(enclosing);
    /* attach the option widget to the second column of the grid */
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);
}

class GncGtkBooleanUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkBooleanUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::BOOLEAN} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_TOGGLE_BUTTON(get_widget())};
        gtk_toggle_button_set_active(widget, option.get_value<bool>());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_TOGGLE_BUTTON(get_widget())};
        option.set_value(static_cast<bool>(gtk_toggle_button_get_active(widget)));
    }
    SCM get_widget_scm_value(const GncOption& option) const override
    {
        auto widget{GTK_TOGGLE_BUTTON(get_widget())};
        return gtk_toggle_button_get_active(widget) ?
               SCM_BOOL_T : SCM_BOOL_F;
    }
};

template <> void
create_option_widget<GncOptionUIType::BOOLEAN> (GncOption& option,
                                                GtkGrid* page_box, int row)

{
    char *local_name{nullptr};
    auto name{option.get_name().c_str()};
    if (name && *name)
        local_name = _(name);
    auto widget{gtk_check_button_new_with_label (local_name)};

    auto ui_item{std::make_unique<GncGtkBooleanUIItem>(widget)};

    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "toggled",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);

    wrap_check_button(option, widget, page_box, row);
}

class GncGtkStringUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkStringUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::STRING} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_ENTRY(get_widget())};
        gtk_entry_set_text(widget, option.get_value<std::string>().c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_ENTRY(get_widget())};
        option.set_value(std::string{gtk_entry_get_text(widget)});
    }
};

template<> void
create_option_widget<GncOptionUIType::STRING> (GncOption& option,
                                               GtkGrid *page_box, int row)
{
    auto enclosing{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_widget_set_hexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    auto widget = gtk_entry_new();
    if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL)
        gtk_entry_set_alignment (GTK_ENTRY(widget), 1.0);
    auto ui_item{std::make_unique<GncGtkStringUIItem>(widget)};

    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    gtk_box_pack_start(GTK_BOX(enclosing), widget, TRUE, TRUE, 0);
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    gtk_widget_show_all(enclosing);
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);
}

class GncGtkTextUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkTextUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::TEXT} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_TEXT_VIEW(get_widget())};
        xxxgtk_textview_set_text(widget, option.get_value<std::string>().c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_TEXT_VIEW(get_widget())};
        option.set_value(std::string{xxxgtk_textview_get_text(widget)});
    }
};

template<> void
create_option_widget<GncOptionUIType::TEXT> (GncOption& option, GtkGrid *page_box, int row)
{
    auto scroll = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll),
                                   GTK_POLICY_NEVER,
                                   GTK_POLICY_AUTOMATIC);
    gtk_container_set_border_width(GTK_CONTAINER(scroll), 2);

    auto frame = gtk_frame_new(NULL);
    gtk_container_add(GTK_CONTAINER(frame), scroll);

    auto enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_widget_set_vexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    auto widget = gtk_text_view_new();
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(widget), GTK_WRAP_WORD);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(widget), TRUE);
    gtk_text_view_set_accepts_tab (GTK_TEXT_VIEW(widget), FALSE);

    auto ui_item{std::make_unique<GncGtkTextUIItem>(widget)};
    auto text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget));
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(text_buffer), "changed",
                     G_CALLBACK(gnc_option_changed_option_cb), &option);
    gtk_container_add (GTK_CONTAINER (scroll), widget);
    gtk_box_pack_start(GTK_BOX(enclosing), frame, TRUE, TRUE, 0);
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    gtk_widget_show_all(enclosing);
    grid_attach_widget(GTK_GRID(page_box), enclosing, row);
}

class GncGtkCurrencyUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkCurrencyUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::CURRENCY} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_CURRENCY_EDIT(get_widget())};
        auto currency{option.get_value<gnc_commodity*>()};

        if (currency)
            gnc_currency_edit_set_currency(widget, GNC_COMMODITY(currency));
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_CURRENCY_EDIT(get_widget())};
        auto currency = gnc_currency_edit_get_currency(widget);
        option.set_value<gnc_commodity*>(currency);
    }
};

template<> void
create_option_widget<GncOptionUIType::CURRENCY> (GncOption& option, GtkGrid *page_box,
                                                 int row)
{
    auto widget{gnc_currency_edit_new()};
    auto ui_item{std::make_unique<GncGtkCurrencyUIItem>(widget)};
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}

class GncGtkCommodityUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkCommodityUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::COMMODITY} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_GENERAL_SELECT(get_widget())};
        auto commodity{option.get_value<gnc_commodity*>()};

        if (commodity)
            gnc_general_select_set_selected(widget, GNC_COMMODITY(commodity));
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_GENERAL_SELECT(get_widget())};
        auto commodity{gnc_general_select_get_selected(widget)};
        option.set_value<gnc_commodity*>(GNC_COMMODITY(commodity));
    }
};

template<> void
create_option_widget<GncOptionUIType::COMMODITY> (GncOption& option, GtkGrid *page_box,
                                                  int row)
{
    auto widget = gnc_general_select_new(GNC_GENERAL_SELECT_TYPE_SELECT,
                                         gnc_commodity_edit_get_string,
                                         gnc_commodity_edit_new_select,
                                         NULL);

    auto ui_item{std::make_unique<GncGtkCommodityUIItem>(widget)};
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();
    g_signal_connect(G_OBJECT(GNC_GENERAL_SELECT(widget)->entry), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}

static GtkWidget*
create_multichoice_widget(GncOption& option)
{
    auto num_values = option.num_permissible_values();

    g_return_val_if_fail(num_values >= 0, NULL);
    auto renderer = gtk_cell_renderer_text_new();
    auto store = gtk_list_store_new(1, G_TYPE_STRING);
    /* Add values to the list store, entry and tooltip */
    for (decltype(num_values) i = 0; i < num_values; i++)
    {
        GtkTreeIter iter;
        auto itemstring = option.permissible_value_name(i);
        gtk_list_store_append (store, &iter);
        gtk_list_store_set(store, &iter, 0,
                           (itemstring && *itemstring) ? _(itemstring) : "", -1);
    }
    /* Create the new Combo with tooltip and add the store */
    auto widget{GTK_WIDGET(gtk_combo_box_new_with_model(GTK_TREE_MODEL(store)))};
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(widget), renderer, TRUE);
    gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT(widget),
                                   renderer, "text", 0);
    g_object_unref(store);

    return widget;
}

class GncGtkMultichoiceUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkMultichoiceUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::MULTICHOICE} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_COMBO_BOX(get_widget())};
        gtk_combo_box_set_active(widget, option.get_value<uint16_t>());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_COMBO_BOX(get_widget())};
        option.set_value<uint16_t>(static_cast<uint16_t>(gtk_combo_box_get_active(widget)));
    }
    SCM get_widget_scm_value(const GncOption& option) const override
    {
        auto widget{GTK_COMBO_BOX(get_widget())};
        auto id{gtk_combo_box_get_active(widget)};
        auto value{option.permissible_value(id)};
        return scm_from_utf8_symbol(value);
    }
};

template<> void
create_option_widget<GncOptionUIType::MULTICHOICE> (GncOption& option, GtkGrid *page_box,
                                                    int row)
{
    auto widget{create_multichoice_widget(option)};
    auto ui_item{std::make_unique<GncGtkMultichoiceUIItem>(widget)};
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();
    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}


class GncDateEntry
{
public:
    GncDateEntry() = default;
    virtual ~GncDateEntry() = default;
    virtual void set_entry_from_option(GncOption& option) = 0;
    virtual void set_option_from_entry(GncOption& option) = 0;
    // Get the widget that has data
    virtual GtkWidget* get_entry() = 0;
    // Get the widget that gets put on the page
    virtual GtkWidget* get_widget() = 0;
    virtual void toggle_relative(bool) {} //BothDateEntry only
    virtual void block_signals(bool) = 0;
};


using GncDateEntryPtr = std::unique_ptr<GncDateEntry>;

class AbsoluteDateEntry : public GncDateEntry
{
public:
    AbsoluteDateEntry(GncOption& option);
    ~AbsoluteDateEntry() = default;
    void set_entry_from_option(GncOption& option) override;
    void set_option_from_entry(GncOption& option) override;
    GtkWidget* get_entry() override { return GTK_WIDGET(m_entry); }
    GtkWidget* get_widget() override { return GTK_WIDGET(m_entry); }
    void block_signals(bool) override;
private:
    GNCDateEdit* m_entry;
    unsigned long m_handler_id;
};

AbsoluteDateEntry::AbsoluteDateEntry(GncOption& option) :
    m_entry{GNC_DATE_EDIT(gnc_date_edit_new(time(NULL), FALSE, FALSE))}
{
    auto entry = GNC_DATE_EDIT(m_entry)->date_entry;
    m_handler_id = g_signal_connect(G_OBJECT(entry), "changed",
                                    G_CALLBACK(gnc_option_changed_option_cb),
                                    &option);
}

void
AbsoluteDateEntry::block_signals(bool block)
{
    auto entry{G_OBJECT(GNC_DATE_EDIT(m_entry)->date_entry)};
    if (block)
        g_signal_handler_block(entry, m_handler_id);
    else
        g_signal_handler_unblock(entry, m_handler_id);
}

void
AbsoluteDateEntry::set_entry_from_option(GncOption& option)
{
    gnc_date_edit_set_time(m_entry, option.get_value<time64>());
}

void
AbsoluteDateEntry::set_option_from_entry(GncOption& option)
{
    option.set_value<time64>(gnc_date_edit_get_date(m_entry));
}

class RelativeDateEntry : public GncDateEntry
{
public:
    RelativeDateEntry(GncOption& option);
    ~RelativeDateEntry() = default;
    void set_entry_from_option(GncOption& option) override;
    void set_option_from_entry(GncOption& option) override;
    GtkWidget* get_widget() override { return m_entry; }
    GtkWidget* get_entry() override { return m_entry; }
    void block_signals(bool) override;
private:
    GtkWidget* m_entry;
    unsigned long m_handler_id;
};


RelativeDateEntry::RelativeDateEntry(GncOption& option)
{

    auto renderer = gtk_cell_renderer_text_new();
    auto store = gtk_list_store_new(1, G_TYPE_STRING);
    /* Add values to the list store, entry and tooltip */
    auto num = option.num_permissible_values();
    for (decltype(num) index = 0; index < num; ++index)
    {
        GtkTreeIter  iter;
        gtk_list_store_append (store, &iter);
        gtk_list_store_set (store, &iter, 0,
                            option.permissible_value_name(index), -1);
    }

    /* Create the new Combo with tooltip and add the store */
    m_entry = GTK_WIDGET(gtk_combo_box_new_with_model(GTK_TREE_MODEL(store)));
    gtk_combo_box_set_active(GTK_COMBO_BOX(m_entry), 0);
    gtk_cell_layout_pack_start (GTK_CELL_LAYOUT(m_entry), renderer, TRUE);
    gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT(m_entry),
                                   renderer, "text", 0);

    g_object_unref(store);

    m_handler_id = g_signal_connect(G_OBJECT(m_entry), "changed",
                                    G_CALLBACK(gnc_option_changed_widget_cb),
                                    &option);
}

void
RelativeDateEntry::set_entry_from_option(GncOption& option)
{
    gtk_combo_box_set_active(GTK_COMBO_BOX(m_entry), option.get_value<uint16_t>());
}

void
RelativeDateEntry::set_option_from_entry(GncOption& option)
{
    option.set_value<uint16_t>(gtk_combo_box_get_active(GTK_COMBO_BOX(m_entry)));
}

void
RelativeDateEntry::block_signals(bool block)
{
    if (block)
        g_signal_handler_block(m_entry, m_handler_id);
    else
        g_signal_handler_unblock(m_entry, m_handler_id);
}

using AbsoluteDateEntryPtr = std::unique_ptr<AbsoluteDateEntry>;
using RelativeDateEntryPtr = std::unique_ptr<RelativeDateEntry>;

class BothDateEntry : public GncDateEntry
{
public:
    BothDateEntry(GncOption& option);
    ~BothDateEntry() = default; //The GncOptionGtkUIItem owns the widget
    void set_entry_from_option(GncOption& option) override;
    void set_option_from_entry(GncOption& option) override;
    GtkWidget* get_widget() override { return m_widget; }
    GtkWidget* get_entry() override;
    void toggle_relative(bool use_absolute) override;
    void block_signals(bool) override;
private:
    GtkWidget* m_widget;
    GtkWidget* m_abs_button;
    AbsoluteDateEntryPtr m_abs_entry;
    GtkWidget* m_rel_button;
    RelativeDateEntryPtr m_rel_entry;
    bool m_use_absolute = true;
    unsigned long m_abs_hdlr;
    unsigned long m_rel_hdlr;
};

static void date_set_absolute_cb(GtkWidget *widget, gpointer data1);
static void date_set_relative_cb(GtkWidget *widget, gpointer data1);

BothDateEntry::BothDateEntry(GncOption& option) :
    m_widget{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)},
    m_abs_button{gtk_radio_button_new(NULL)},
    m_abs_entry{std::make_unique<AbsoluteDateEntry>(option)},
    m_rel_button{
        gtk_radio_button_new_from_widget(GTK_RADIO_BUTTON(m_abs_button))},
    m_rel_entry{std::make_unique<RelativeDateEntry>(option)}
{
    gtk_box_set_homogeneous (GTK_BOX(m_widget), FALSE);
    m_abs_hdlr = g_signal_connect(G_OBJECT(m_abs_button), "toggled",
                                  G_CALLBACK(date_set_absolute_cb), &option);
    m_rel_hdlr = g_signal_connect(G_OBJECT(m_rel_button), "toggled",
                                  G_CALLBACK(date_set_relative_cb), &option);

    gtk_box_pack_start(GTK_BOX(m_widget),
                       m_abs_button, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget),
                       m_abs_entry->get_entry(), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget),
                       m_rel_button, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget),
                       m_rel_entry->get_entry(), FALSE, FALSE, 0);

}

GtkWidget*
BothDateEntry::get_entry()
{
    return m_use_absolute ? m_abs_entry->get_entry() : m_rel_entry->get_entry();
}

void
BothDateEntry::toggle_relative(bool use_absolute)
{
    m_use_absolute = use_absolute;

    gtk_widget_set_sensitive(GTK_WIDGET(m_abs_entry->get_widget()),
                             m_use_absolute);
    gtk_widget_set_sensitive(GTK_WIDGET(m_rel_entry->get_widget()),
                             !m_use_absolute);
}

void
BothDateEntry::set_entry_from_option(GncOption& option)
{
    m_use_absolute =
        option.get_value<RelativeDatePeriod>() == RelativeDatePeriod::ABSOLUTE;
    if (m_use_absolute)
        m_abs_entry->set_entry_from_option(option);
    else
        m_rel_entry->set_entry_from_option(option);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(m_rel_button),
                                 !m_use_absolute);
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(m_abs_button),
                                 m_use_absolute);

    toggle_relative(m_use_absolute);
}

void
BothDateEntry::set_option_from_entry(GncOption& option)
{
    if (m_use_absolute)
        m_abs_entry->set_option_from_entry(option);
    else
        m_rel_entry->set_option_from_entry(option);
}

void
BothDateEntry::block_signals(bool block)
{
    if (block)
    {
        g_signal_handler_block(m_abs_button, m_abs_hdlr);
        g_signal_handler_block(m_rel_button, m_rel_hdlr);
    }
    else
    {
        g_signal_handler_unblock(m_abs_button, m_abs_hdlr);
        g_signal_handler_unblock(m_rel_button, m_rel_hdlr);
    }
    m_abs_entry->block_signals(block);
    m_rel_entry->block_signals(block);
}

class GncOptionDateUIItem : public GncOptionGtkUIItem
{
public:
    GncOptionDateUIItem(GncDateEntryPtr entry, GncOptionUIType type) :
        GncOptionGtkUIItem{entry->get_widget(), type}, m_entry{std::move(entry)} { }
    ~GncOptionDateUIItem() = default;
    void clear_ui_item() override { m_entry = nullptr; }
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        if (m_entry)
            m_entry->set_entry_from_option(option);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        if (m_entry)
            m_entry->set_option_from_entry(option);
    }
    GtkWidget* get_widget() const override
    {
        return m_entry->get_widget();
    }
    GncDateEntry* get_entry() { return m_entry.get(); }
private:
    GncDateEntryPtr m_entry;
};

static void
date_set_absolute_cb(GtkWidget *widget, gpointer data1)
{
    GncOption* option = static_cast<decltype(option)>(data1);
    auto ui_item = option->get_ui_item();
    if (auto date_ui = dynamic_cast<const GncOptionDateUIItem* const>(ui_item))
    {
        const_cast<GncOptionDateUIItem*>(date_ui)->get_entry()->toggle_relative(true);
        gnc_option_changed_option_cb(widget, option);
    }
}

static void
date_set_relative_cb(GtkWidget *widget, gpointer data1)
{
    GncOption* option = static_cast<decltype(option)>(data1);
    auto ui_item = option->get_ui_item();
    if (auto date_ui = dynamic_cast<const GncOptionDateUIItem* const>(ui_item))
    {
        const_cast<GncOptionDateUIItem*>(date_ui)->get_entry()->toggle_relative(false);
        gnc_option_changed_option_cb(widget, option);
    }
}

static void
create_date_option_widget(GncOption& option, GtkGrid *page_box, int row)
{
    GtkWidget *enclosing{nullptr};
    auto type = option.get_ui_type();
    switch (type)
    {
        case GncOptionUIType::DATE_ABSOLUTE:
            option.set_ui_item(std::make_unique<GncOptionDateUIItem>(std::make_unique<AbsoluteDateEntry>(option), type));
            break;
        case GncOptionUIType::DATE_RELATIVE:
            option.set_ui_item(std::make_unique<GncOptionDateUIItem>(std::make_unique<RelativeDateEntry>(option), type));
            break;
        case GncOptionUIType::DATE_BOTH:
            option.set_ui_item(std::make_unique<GncOptionDateUIItem>(std::make_unique<BothDateEntry>(option), type));
            break;
        default:
            PERR("Attempted to create date option widget with wrong UI type %d",
                 static_cast<int>(type));
            break;
    }

    auto widget{option_get_gtk_widget(&option)};
    if (type == GncOptionUIType::DATE_RELATIVE)
    {
        enclosing = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
        gtk_box_set_homogeneous(GTK_BOX (enclosing), FALSE);

        gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
    }
    else
    {
        enclosing = gtk_frame_new(nullptr);
        g_object_set(G_OBJECT(widget), "margin", 3, NULL);

        gtk_container_add (GTK_CONTAINER(enclosing), widget);
    }

    gtk_widget_set_halign (GTK_WIDGET(enclosing), GTK_ALIGN_START);
    set_name_label(option, page_box, row, false);
    set_tool_tip(option, enclosing);
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);

    auto ui_item{dynamic_cast<GncOptionDateUIItem*>(option.get_ui_item())};
    if (auto date_ui{ui_item ? ui_item->get_entry() : nullptr})
    {
        date_ui->block_signals(true);
        date_ui->set_entry_from_option(option);
        date_ui->block_signals(false);
    }

    gtk_widget_show_all(enclosing);
}

template<> void
create_option_widget<GncOptionUIType::DATE_ABSOLUTE>(GncOption& option,
                                                     GtkGrid *page_box, int row)
{
    create_date_option_widget(option, page_box, row);
}

template<> void
create_option_widget<GncOptionUIType::DATE_RELATIVE>(GncOption& option,
                                                     GtkGrid *page_box, int row)
{
    create_date_option_widget(option, page_box, row);
}

template<> void
create_option_widget<GncOptionUIType::DATE_BOTH>(GncOption& option,
                                                 GtkGrid *page_box, int row)
{
    create_date_option_widget(option, page_box, row);
}

using GncOptionAccountList = std::vector<GncGUID>;

static void
account_select_all_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GncTreeViewAccount *tree_view;
    GtkTreeSelection *selection;

    tree_view = GNC_TREE_VIEW_ACCOUNT(option_get_gtk_widget (option));
    gtk_tree_view_expand_all(GTK_TREE_VIEW(tree_view));
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
    gtk_tree_selection_select_all(selection);
    gnc_option_changed_widget_cb(widget, option);
}

static void
account_clear_all_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GncTreeViewAccount *tree_view;
    GtkTreeSelection *selection;

    tree_view = GNC_TREE_VIEW_ACCOUNT(option_get_gtk_widget (option));
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(tree_view));
    gtk_tree_selection_unselect_all(selection);
    gnc_option_changed_widget_cb(widget, option);
}

static void
account_select_children_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GncTreeViewAccount *tree_view;
    GList *acct_list = NULL, *acct_iter = NULL;

    tree_view = GNC_TREE_VIEW_ACCOUNT(option_get_gtk_widget (option));
    acct_list = gnc_tree_view_account_get_selected_accounts (tree_view);

    for (acct_iter = acct_list; acct_iter; acct_iter = acct_iter->next)
        gnc_tree_view_account_select_subaccounts (tree_view, static_cast<Account*>(acct_iter->data));

    g_list_free (acct_list);
}

static void
account_set_default_cb(GtkWidget* widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    account_clear_all_cb(widget, data);
    option->set_value(option->get_default_value<GncOptionAccountList>());
    option->set_ui_item_from_option();
}

static void
show_hidden_toggled_cb(GtkWidget *widget, GncOption* option)
{
    if (option->get_ui_type() != GncOptionUIType::ACCOUNT_LIST &&
        option->get_ui_type() != GncOptionUIType::ACCOUNT_SEL)
        return;

    auto tree_view = GNC_TREE_VIEW_ACCOUNT(option_get_gtk_widget(option));
    AccountViewInfo avi;
    gnc_tree_view_account_get_view_info (tree_view, &avi);
    avi.show_hidden = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
    gnc_tree_view_account_set_view_info (tree_view, &avi);
    gnc_option_changed_widget_cb(widget, option);
}

class GncGtkAccountListUIItem : public GncOptionGtkUIItem
{
public:
    explicit GncGtkAccountListUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::ACCOUNT_LIST} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_TREE_VIEW_ACCOUNT(get_widget())};
        GList *acc_list = nullptr;
        const GncOptionAccountList& accounts =
            option.get_value<GncOptionAccountList>();
        auto book{gnc_get_current_book()};
        for (auto guid : accounts)
        {
            auto account{xaccAccountLookup(&guid, book)};
            acc_list = g_list_prepend(acc_list, account);
        }
        acc_list = g_list_reverse(acc_list);
        gnc_tree_view_account_set_selected_accounts(widget, acc_list, TRUE);
        g_list_free(acc_list);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_TREE_VIEW_ACCOUNT(get_widget())};
        auto acc_list = gnc_tree_view_account_get_selected_accounts(widget);
        GncOptionAccountList acc_vec;
        acc_vec.reserve(g_list_length(acc_list));
        for (auto node = acc_list; node; node = g_list_next(node))
        {
            auto guid{qof_entity_get_guid(node->data)};
            acc_vec.push_back(*guid);
        }
        g_list_free(acc_list);
        option.set_value(acc_vec);
    }
};

static GtkWidget*
create_account_widget(GncOption& option, char *name)
{
    bool multiple_selection;
    GtkWidget *scroll_win;
    GtkWidget *button;
    GtkWidget *frame;
    GtkWidget *tree;
    GtkWidget *vbox;
    GtkWidget *bbox;
    GList *acct_type_list;
    GtkTreeSelection *selection;

    multiple_selection = option.is_multiselect();
    acct_type_list = option.account_type_list();

    frame = gtk_frame_new(name);

    vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (vbox), FALSE);

    gtk_container_add(GTK_CONTAINER(frame), vbox);

    tree = GTK_WIDGET(gnc_tree_view_account_new (FALSE));
    gtk_tree_view_set_headers_visible (GTK_TREE_VIEW(tree), FALSE);
    selection = gtk_tree_view_get_selection (GTK_TREE_VIEW(tree));
    if (multiple_selection)
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_MULTIPLE);
    else
        gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);

    if (acct_type_list)
    {
        GList *node;
        AccountViewInfo avi;
        int i;

        gnc_tree_view_account_get_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);

        for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
            avi.include_type[i] = FALSE;
        avi.show_hidden = FALSE;

        for (node = acct_type_list; node; node = node->next)
        {
            GNCAccountType type = static_cast<decltype(type)>(GPOINTER_TO_INT (node->data));
            if (type < NUM_ACCOUNT_TYPES)
                avi.include_type[type] = TRUE;
        }

        gnc_tree_view_account_set_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);
        g_list_free (acct_type_list);
    }
    else
    {
        AccountViewInfo avi;
        int i;

        gnc_tree_view_account_get_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);

        for (i = 0; i < NUM_ACCOUNT_TYPES; i++)
            avi.include_type[i] = TRUE;
        avi.show_hidden = FALSE;
        gnc_tree_view_account_set_view_info (GNC_TREE_VIEW_ACCOUNT (tree), &avi);
    }

    scroll_win = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll_win),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);

    gtk_box_pack_start(GTK_BOX(vbox), scroll_win, TRUE, TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER(scroll_win), 5);

    bbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
    gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
    gtk_box_pack_start(GTK_BOX(vbox), bbox, FALSE, FALSE, 10);

    option.set_ui_item(std::make_unique<GncGtkAccountListUIItem>(tree));
    option.set_ui_item_from_option();

    if (multiple_selection)
    {
        button = gtk_button_new_with_label(_("Select All"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Select all accounts."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(account_select_all_cb), &option);

        button = gtk_button_new_with_label(_("Clear All"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Clear the selection and unselect all accounts."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(account_clear_all_cb), &option);

        button = gtk_button_new_with_label(_("Select Children"));
        gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
        gtk_widget_set_tooltip_text(button, _("Select all descendents of selected account."));

        g_signal_connect(G_OBJECT(button), "clicked",
                         G_CALLBACK(account_select_children_cb), &option);
    }

    button = gtk_button_new_with_label(_("Select Default"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select the default account selection."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(account_set_default_cb), &option);

    gtk_widget_set_margin_start (GTK_WIDGET(bbox), 6);
    gtk_widget_set_margin_end (GTK_WIDGET(bbox), 6);

    if (multiple_selection)
    {
        /* Put the "Show hidden" checkbox on a separate line since
           the 4 buttons make the dialog too wide. */
        bbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
        gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_START);
        gtk_box_pack_start(GTK_BOX(vbox), bbox, FALSE, FALSE, 0);
    }

    button = gtk_check_button_new_with_label(_("Show Hidden Accounts"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Show accounts that have been marked hidden."));
    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(button), FALSE);
    g_signal_connect(G_OBJECT(button), "toggled",
                     G_CALLBACK(show_hidden_toggled_cb), &option);

    gtk_container_add(GTK_CONTAINER(scroll_win), tree);
    return frame;
}

static void
option_account_sel_changed_cb(GtkTreeSelection *sel, gpointer data)
{
    auto tree_view{gtk_tree_selection_get_tree_view(sel)};
    gnc_option_changed_widget_cb(GTK_WIDGET(tree_view),
                                 static_cast<GncOption*>(data));
}

template<> void
create_option_widget<GncOptionUIType::ACCOUNT_LIST>(GncOption& option,
                                                    GtkGrid *page_box, int row)
{
    auto enclosing{create_account_widget(option, nullptr)};
    gtk_widget_set_vexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(enclosing), TRUE);
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);

    auto widget{option_get_gtk_widget(&option)};
    auto selection{gtk_tree_view_get_selection(GTK_TREE_VIEW(widget))};
    g_signal_connect(G_OBJECT(selection), "changed",
                     G_CALLBACK(option_account_sel_changed_cb), &option);
    gtk_widget_show_all(enclosing);
}

class GncGtkAccountSelUIItem : public GncOptionGtkUIItem
{
public:
    explicit GncGtkAccountSelUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::ACCOUNT_SEL} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_ACCOUNT_SEL(get_widget())};
        auto instance{option.get_value<const Account*>()};
        if (instance)
            gnc_account_sel_set_account(widget, const_cast<Account*>(instance), FALSE);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_ACCOUNT_SEL(get_widget())};
        option.set_value(qof_instance_cast((gnc_account_sel_get_account(widget))));
    }
};

template<> void
create_option_widget<GncOptionUIType::ACCOUNT_SEL> (GncOption& option,
                                                    GtkGrid *page_box, int row)
{
    auto acct_type_list{option.account_type_list()};
    auto widget{gnc_account_sel_new()};
    gnc_account_sel_set_acct_filters(GNC_ACCOUNT_SEL(widget),
                                     acct_type_list, NULL);
    g_list_free(acct_type_list);

    // gnc_account_sel doesn't emit a changed signal
    option.set_ui_item(std::make_unique<GncGtkAccountSelUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect(widget, "account_sel_changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}

static void
list_changed_cb(GtkTreeSelection *selection, GncOption* option)
{
    GtkTreeView *view = GTK_TREE_VIEW(option_get_gtk_widget (option));
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
list_select_all_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GtkTreeView *view;
    GtkTreeSelection *selection;

    view = GTK_TREE_VIEW(option_get_gtk_widget(option));
    selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_select_all(selection);
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
list_clear_all_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    GtkTreeView *view;
    GtkTreeSelection *selection;

    view = GTK_TREE_VIEW(option_get_gtk_widget(option));
    selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_unselect_all(selection);
    gnc_option_changed_widget_cb(GTK_WIDGET(view), option);
}

static void
list_set_default_cb(GtkWidget *widget, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    list_clear_all_cb(widget, data);
    option->set_value(option->get_default_value<GncMultichoiceOptionIndexVec>());
    option->set_ui_item_from_option();
}

class GncGtkListUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkListUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::LIST} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_TREE_VIEW(get_widget())};
        auto selection{gtk_tree_view_get_selection(widget)};
        g_signal_handlers_block_by_func(selection, (gpointer)list_changed_cb, &option);
        gtk_tree_selection_unselect_all(selection);
        for (auto index : option.get_value<GncMultichoiceOptionIndexVec>())
        {
            auto path{gtk_tree_path_new_from_indices(index, -1)};
            gtk_tree_selection_select_path(selection, path);
            gtk_tree_path_free(path);
        }
        g_signal_handlers_unblock_by_func(selection, (gpointer)list_changed_cb, &option);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_TREE_VIEW(get_widget())};
        auto selection{gtk_tree_view_get_selection(widget)};
        auto rows{option.num_permissible_values()};
        GncMultichoiceOptionIndexVec vec;
        for (size_t row = 0; row < rows; ++row)
        {
            auto path{gtk_tree_path_new_from_indices(row, -1)};
            auto selected{gtk_tree_selection_path_is_selected(selection, path)};
            gtk_tree_path_free(path);
            if (selected)
                vec.push_back(row);
        }
        option.set_value(vec);
    }
};

static GtkWidget *
create_list_widget(GncOption& option, char *name)
{
    auto frame = gtk_frame_new(name);
    auto hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_container_add(GTK_CONTAINER(frame), hbox);

    auto store = gtk_list_store_new(1, G_TYPE_STRING);
    auto view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
    g_object_unref(store);
    auto renderer = gtk_cell_renderer_text_new();
    auto column = gtk_tree_view_column_new_with_attributes("", renderer,
                                                           "text", 0,
                                                           NULL);
    gtk_tree_view_append_column(view, column);
    gtk_tree_view_set_headers_visible(view, FALSE);

    auto num_values = option.num_permissible_values();
    for (decltype(num_values) i = 0; i < num_values; i++)
    {
        GtkTreeIter iter;
        auto raw_string = option.permissible_value_name(i);
        auto string = (raw_string && *raw_string) ? _(raw_string) : "";
        gtk_list_store_append(store, &iter);
        gtk_list_store_set(store, &iter, 0, string ? string : "", -1);
    }

    option.set_ui_item(std::make_unique<GncGtkListUIItem>(GTK_WIDGET(view)));
    option.set_ui_item_from_option();

    gtk_box_pack_start(GTK_BOX(hbox), GTK_WIDGET(view), FALSE, FALSE, 0);

    auto selection = gtk_tree_view_get_selection(view);
    gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
    g_signal_connect(selection, "changed",
                     G_CALLBACK(list_changed_cb), &option);

    auto bbox = gtk_button_box_new (GTK_ORIENTATION_VERTICAL);
    gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_SPREAD);
    gtk_box_pack_end(GTK_BOX(hbox), bbox, FALSE, FALSE, 0);

    auto button = gtk_button_new_with_label(_("Select All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select all entries."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(list_select_all_cb), &option);

    button = gtk_button_new_with_label(_("Clear All"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Clear the selection and unselect all entries."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(list_clear_all_cb), &option);

    button = gtk_button_new_with_label(_("Select Default"));
    gtk_box_pack_start(GTK_BOX(bbox), button, FALSE, FALSE, 0);
    gtk_widget_set_tooltip_text(button, _("Select the default selection."));

    g_signal_connect(G_OBJECT(button), "clicked",
                     G_CALLBACK(list_set_default_cb), &option);

    g_object_set (G_OBJECT(hbox), "margin", 3, NULL);

    return frame;
}

template<> void
create_option_widget<GncOptionUIType::LIST> (GncOption& option,
                                             GtkGrid *page_box, int row)
{

    auto enclosing{create_list_widget(option, nullptr)};
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    grid_attach_widget (GTK_GRID(page_box), enclosing, row);
    gtk_widget_show(enclosing);
}

class GncGtkNumberRangeUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkNumberRangeUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::NUMBER_RANGE} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        if (option.is_alternate())
            gtk_spin_button_set_value(GTK_SPIN_BUTTON(get_widget()),
                                      option.get_value<int>());
        else
            gtk_spin_button_set_value(GTK_SPIN_BUTTON(get_widget()),
                                      option.get_value<double>());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        if (option.is_alternate())
            option.set_value<int>(gtk_spin_button_get_value(GTK_SPIN_BUTTON(get_widget())));
        else
            option.set_value<double>(gtk_spin_button_get_value(GTK_SPIN_BUTTON(get_widget())));
    }
};

/* New spin button configured with the values provided by the passed-in
 * GncOption, which had better contain a GncOptionRangeValue.
 *
 * Also used to set up the pixel spinner in the plot_size control.
 */
static GtkSpinButton*
create_range_spinner(GncOption& option)
{
    gdouble lower_bound = G_MINDOUBLE;
    gdouble upper_bound = G_MAXDOUBLE;
    gdouble step_size = 1.0;

    if (option.is_alternate())
    {
        int tmp_lower_bound = G_MININT;
        int tmp_upper_bound = G_MAXINT;
        int tmp_step_size = 1.0;
        option.get_limits<int>(tmp_upper_bound, tmp_lower_bound, tmp_step_size);
        lower_bound =(double)tmp_lower_bound;
        upper_bound = (double)tmp_upper_bound;
        step_size = (double)tmp_step_size;
    }
    else
        option.get_limits<double>(upper_bound, lower_bound, step_size);

    auto adj = GTK_ADJUSTMENT(gtk_adjustment_new(lower_bound, lower_bound,
                                                 upper_bound, step_size,
                                                 step_size * 5.0,
                                                 0));

    size_t num_decimals = 0;
    for (auto steps = step_size; steps < 1; steps *= 10)
        ++num_decimals;
    auto widget = gtk_spin_button_new(adj, step_size, num_decimals);
    gtk_spin_button_set_numeric(GTK_SPIN_BUTTON(widget), TRUE);

    size_t num_digits = 0;
    for (auto bigger = MAX(ABS(lower_bound), ABS(upper_bound));
         bigger >= 1; bigger /= 10.0)
        ++num_digits;
    num_digits += num_decimals;
    gtk_entry_set_width_chars(GTK_ENTRY(widget), num_digits);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget),
                              (upper_bound / 2)); //default
    return GTK_SPIN_BUTTON(widget);
}

template<> void
create_option_widget<GncOptionUIType::NUMBER_RANGE> (GncOption& option,
                                                     GtkGrid *page_box, int row)
{
    auto widget{create_range_spinner(option)};
    option.set_ui_item(std::make_unique<GncGtkNumberRangeUIItem>(GTK_WIDGET(widget)));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, GTK_WIDGET(widget), page_box, row);
}

class GncGtkColorUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkColorUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::COLOR} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        GdkRGBA color;
        /* gdk_rgba_parse uses pango_color_parse for hex color strings instead
         * of pango_color_parse_with_alpha and that fails if the string length
         * is 8.
        */
        auto value{option.get_value<std::string>().substr(0,6)};
        auto rgba_str{g_strdup_printf("#%s", value.c_str())};
        if (gdk_rgba_parse(&color, rgba_str))
        {
            auto color_button = GTK_COLOR_CHOOSER(get_widget());
            gtk_color_chooser_set_rgba(color_button, &color);
        }
        g_free(rgba_str);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        GdkRGBA color;
        auto color_button = GTK_COLOR_CHOOSER(get_widget());
        gtk_color_chooser_get_rgba(color_button, &color);
        auto rgba_str = g_strdup_printf("%2x%2x%2x%2x",
                                        (uint8_t)(color.red * 255),
                                        (uint8_t)(color.green * 255),
                                        (uint8_t)(color.blue * 255),
                                        (uint8_t)(color.alpha * 255));
        auto rgb_str = g_strdup_printf("%2x%2x%2x",
                                       (uint8_t)(color.red * 255),
                                       (uint8_t)(color.green * 255),
                                       (uint8_t)(color.blue * 255));
// sample-report.scm uses an old HTML4 attribute that doesn't understand alpha.
        option.set_value(std::string{rgb_str});
        g_free(rgba_str);
        g_free(rgb_str);
    }
};

template<> void
create_option_widget<GncOptionUIType::COLOR> (GncOption& option, GtkGrid *page_box, int row)
{
    auto widget = gtk_color_button_new();
    gtk_color_chooser_set_use_alpha(GTK_COLOR_CHOOSER(widget), TRUE);

    option.set_ui_item(std::make_unique<GncGtkColorUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "color-set",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}

class GncGtkFontUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkFontUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::FONT} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        GtkFontChooser *font_chooser = GTK_FONT_CHOOSER(get_widget());
        gtk_font_chooser_set_font(font_chooser,
                                  option.get_value<std::string>().c_str());

    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        GtkFontChooser *font_chooser = GTK_FONT_CHOOSER(get_widget());
        option.set_value(std::string{gtk_font_chooser_get_font(font_chooser)});
    }
};

template<> void
create_option_widget<GncOptionUIType::FONT> (GncOption& option, GtkGrid *page_box, int row)
{
    auto widget{gtk_font_button_new()};
    g_object_set(G_OBJECT(widget),
                 "use-font", TRUE,
                 "show-style", TRUE,
                 "show-size", TRUE,
                 (char *)NULL);

    option.set_ui_item(std::make_unique<GncGtkFontUIItem>(widget));
    option.set_ui_item_from_option();
    g_signal_connect(G_OBJECT(widget), "font-set",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}
/* A pointer to the last selected filename */
#define LAST_SELECTION "last-selection"

static void
update_preview_cb (GtkFileChooser *chooser, void* data)
{
    g_return_if_fail(chooser != NULL);

    ENTER("chooser %p", chooser);
    auto filename = gtk_file_chooser_get_preview_filename(chooser);
    DEBUG("chooser preview name is %s.", filename ? filename : "(null)");
    if (filename == NULL)
    {
        filename = g_strdup(static_cast<const char*>(g_object_get_data(G_OBJECT(chooser), LAST_SELECTION)));
        DEBUG("using last selection of %s", filename ? filename : "(null)");
        if (filename == NULL)
        {
            LEAVE("no usable name");
            return;
        }
    }

    auto image = GTK_IMAGE(gtk_file_chooser_get_preview_widget(chooser));
    auto pixbuf = gdk_pixbuf_new_from_file_at_size(filename, 128, 128, NULL);
    g_free(filename);
    auto have_preview = (pixbuf != NULL);

    gtk_image_set_from_pixbuf(image, pixbuf);
    if (pixbuf)
        g_object_unref(pixbuf);

    gtk_file_chooser_set_preview_widget_active(chooser, have_preview);
    LEAVE("preview visible is %d", have_preview);
}

static void
change_image_cb (GtkFileChooser *chooser, void* data)
{
    auto filename{gtk_file_chooser_get_preview_filename(chooser)};
    if (!filename)
        return;
    g_object_set_data_full(G_OBJECT(chooser), LAST_SELECTION, filename, g_free);
}

class GncGtkPixmapUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkPixmapUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::PIXMAP} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto string{option.get_value<std::string>()};
        if (!string.empty())
        {
            DEBUG("string = %s", string.c_str());
            auto chooser{GTK_FILE_CHOOSER(get_widget())};
            gtk_file_chooser_select_filename(chooser, string.c_str());
            auto filename{gtk_file_chooser_get_filename(chooser)};
            g_object_set_data_full(G_OBJECT(chooser), LAST_SELECTION,
                                   g_strdup(string.c_str()), g_free);
            DEBUG("Set %s, retrieved %s", string.c_str(),
                  filename ? filename : "(null)");
            update_preview_cb(chooser, &option);
        }
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto string = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(get_widget()));
        DEBUG("filename %s", string ? string : "(null)");
        if (string)
        {
            option.set_value(std::string{string});
            g_free(string);
        }
    }
};

template<> void
create_option_widget<GncOptionUIType::PIXMAP> (GncOption& option,
                                               GtkGrid *page_box, int row)
{
    auto enclosing{gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_box_set_homogeneous(GTK_BOX(enclosing), FALSE);
    auto button{gtk_button_new_with_label(_("Clear"))};
    gtk_widget_set_tooltip_text(button, _("Clear any selected image file."));
    auto widget{ gtk_file_chooser_button_new(_("Select image"),
                                             GTK_FILE_CHOOSER_ACTION_OPEN)};
    gtk_widget_set_tooltip_text(widget, _("Select an image file."));
    g_object_set(G_OBJECT(widget),
                 "width-chars", 30,
                 "preview-widget", gtk_image_new(),
                 (char *)NULL);
    option.set_ui_item(std::make_unique<GncGtkPixmapUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT (widget), "selection-changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    g_signal_connect(G_OBJECT (widget), "selection-changed",
                     G_CALLBACK(change_image_cb), &option);
    g_signal_connect(G_OBJECT (widget), "update-preview",
                     G_CALLBACK(update_preview_cb), &option);
    g_signal_connect_swapped(G_OBJECT (button), "clicked",
                             G_CALLBACK(gtk_file_chooser_unselect_all), widget);

    gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(enclosing), button, FALSE, FALSE, 0);

    gtk_widget_show(widget);
    set_name_label(option, page_box, row, false);
    set_tool_tip(option, enclosing);
    gtk_widget_show(enclosing);
    grid_attach_widget(page_box, enclosing, row);
}

static void
radiobutton_set_cb(GtkWidget *w, gpointer data)
{
    GncOption* option = static_cast<decltype(option)>(data);
    gpointer _current, _new_value;
    gint current, new_value;

    auto widget = option_get_gtk_widget(option);

    _current = g_object_get_data(G_OBJECT(widget), "gnc_radiobutton_index");
    current = GPOINTER_TO_INT (_current);

    _new_value = g_object_get_data (G_OBJECT(w), "gnc_radiobutton_index");
    new_value = GPOINTER_TO_INT (_new_value);

    if (current == new_value)
        return;

    g_object_set_data (G_OBJECT(widget), "gnc_radiobutton_index",
                       GINT_TO_POINTER(new_value));
    gnc_option_changed_widget_cb(widget, option);
}

class GncGtkRadioButtonUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkRadioButtonUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::RADIOBUTTON} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto index{option.get_value<uint16_t>()};
        auto list{gtk_container_get_children(GTK_CONTAINER(get_widget()))};
        auto box{GTK_WIDGET(list->data)};
        g_list_free(list);

        list = gtk_container_get_children(GTK_CONTAINER(box));
        auto node{g_list_nth(list, index)};
        GtkButton* button{};
        if (node)
        {
            button = GTK_BUTTON(node->data);
        }
        else
        {
            PERR("Invalid Radio Button Selection %hu", index);
            g_list_free(list);
            return;
        }
        g_list_free(list);
        auto val{g_object_get_data (G_OBJECT (button),
                                    "gnc_radiobutton_index")};
        g_return_if_fail (GPOINTER_TO_UINT (val) == index);

        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button), TRUE);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto index{g_object_get_data(G_OBJECT(get_widget()),
                                     "gnc_radiobutton_index")};
        option.set_value<uint16_t>(GPOINTER_TO_INT(index));
    }
};

static GtkWidget *
create_radiobutton_widget(char *name, GncOption& option)
{
    GtkWidget *frame, *box;
    GtkWidget *widget = NULL;

    auto num_values{option.num_permissible_values()};

    g_return_val_if_fail(num_values >= 0, NULL);

    /* Create our button frame */
    frame = gtk_frame_new (name);

    /* Create the button box */
    box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);
    gtk_container_add (GTK_CONTAINER (frame), box);

    option.set_ui_item(std::make_unique<GncGtkPixmapUIItem>(frame));
    option.set_ui_item_from_option();

    /* Iterate over the options and create a radio button for each one */
    for (decltype(num_values) i = 0; i < num_values; i++)
    {
        auto label = option.permissible_value_name(i);

        widget =
            gtk_radio_button_new_with_label_from_widget (widget ?
                                                         GTK_RADIO_BUTTON (widget) :
                                                         NULL,
                                                         label && *label ? _(label) : "");
        g_object_set_data (G_OBJECT (widget), "gnc_radiobutton_index",
                           GINT_TO_POINTER (i));
        g_signal_connect(G_OBJECT(widget), "toggled",
                         G_CALLBACK(radiobutton_set_cb), &option);
        gtk_box_pack_start (GTK_BOX (box), widget, FALSE, FALSE, 0);
    }

    return frame;
}

template<> void
create_option_widget<GncOptionUIType::RADIOBUTTON> (GncOption& option, GtkGrid *page_box, int row)
 {
     auto enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
     gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
     set_name_label(option, page_box, row, true);
     set_tool_tip(option, enclosing);
     auto widget = create_radiobutton_widget(NULL, option);
     gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
     gtk_widget_show_all(enclosing);
     grid_attach_widget(page_box, enclosing, row);
 }

class GncGtkDateFormatUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkDateFormatUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::DATE_FORMAT} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_DATE_FORMAT(get_widget())};
        gnc_date_format_set_custom(widget,
                                   option.get_value<std::string>().c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_DATE_FORMAT(get_widget())};
        option.set_value(std::string{gnc_date_format_get_custom(widget)});
    }
};


template<> void
create_option_widget<GncOptionUIType::DATE_FORMAT> (GncOption& option,
                                                    GtkGrid *page_box, int row)
{
    auto enclosing = gnc_date_format_new_without_label ();
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
    option.set_ui_item(std::make_unique<GncGtkDateFormatUIItem>(enclosing));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(enclosing), "format_changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    gtk_widget_show_all(enclosing);
    grid_attach_widget(page_box, enclosing, row);
}

class PlotSize;
static void plot_size_set_pixels(GtkWidget*, PlotSize*);
static void plot_size_set_percent(GtkWidget*, PlotSize*);

class PlotSize
{
    GtkWidget *m_widget;
    GtkWidget *m_pixel_button;
    GtkWidget *m_percent_button;
    GtkWidget *m_range_spinner;
    GtkAdjustment *m_adj_pct;
    GtkAdjustment *m_adj_px;
    unsigned long m_percent_handler;
    unsigned long m_pixel_handler;
public:
    PlotSize(GncOption& option);
    ~PlotSize();
    void set_entry_from_option(GncOption& option);
    void set_option_from_entry(GncOption& option);
    GtkWidget* get_widget() { return m_widget; }
    GtkWidget* get_spinner() { return m_range_spinner; }
    void set_pixels() { gtk_spin_button_set_adjustment(GTK_SPIN_BUTTON(m_range_spinner), m_adj_px); }
    void set_percent() { gtk_spin_button_set_adjustment(GTK_SPIN_BUTTON(m_range_spinner), m_adj_pct); }
};

PlotSize::PlotSize(GncOption& option) :
    m_widget{gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 4)}, m_pixel_button{gtk_radio_button_new_with_label(nullptr, _("Pixels"))},
    m_percent_button{gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(m_pixel_button), _("Percent"))},
    m_range_spinner{GTK_WIDGET(create_range_spinner(option))},
    m_adj_pct{GTK_ADJUSTMENT(g_object_ref(gtk_adjustment_new(100.0, 10.0, 100.0, 1.0, 5.0, 0.0)))},
    m_adj_px{GTK_ADJUSTMENT(g_object_ref(gtk_adjustment_new(1000.0, 110.0, 10000.0, 10.0, 250.0, 0.0)))}
{
    gtk_box_set_homogeneous(GTK_BOX(m_widget), FALSE);
    g_object_set (G_OBJECT(m_widget), "margin", 3, NULL);
    set_tool_tip(option, m_widget);
    gtk_box_pack_start(GTK_BOX(m_widget), GTK_WIDGET(m_pixel_button), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget), GTK_WIDGET(m_percent_button), FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(m_widget), GTK_WIDGET(m_range_spinner),
                       FALSE, FALSE, 0);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(m_pixel_button), FALSE);
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(m_percent_button), TRUE);

    m_pixel_handler = g_signal_connect(m_pixel_button, "toggled", G_CALLBACK(plot_size_set_pixels), this);
    m_percent_handler = g_signal_connect(m_percent_button, "toggled", G_CALLBACK(plot_size_set_percent), this);
}

PlotSize::~PlotSize()
{
    g_signal_handler_disconnect(m_pixel_button, m_pixel_handler);
    g_signal_handler_disconnect(m_percent_button, m_percent_handler);
    g_object_unref(m_adj_pct);
    g_object_unref(m_adj_px);
}

void
PlotSize::set_option_from_entry(GncOption& option)
{
    auto value{gtk_spin_button_get_value(GTK_SPIN_BUTTON(m_range_spinner))};
    if (option.is_alternate())
        option.set_value<int>(static_cast<int>(value));
    else
        option.set_value<double>(value);
}

void
PlotSize::set_entry_from_option(GncOption& option)
{
    double value;
    if (option.is_alternate())
    {
        auto int_value{option.get_value<int>()};
        value = static_cast<double>(int_value);
    }
    else
    {
        value = option.get_value<double>();
    }

    if (value > 100.0)
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(m_pixel_button), TRUE);
    else
        gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(m_percent_button), TRUE);

    gtk_spin_button_set_value(GTK_SPIN_BUTTON(m_range_spinner), value);
}

void
plot_size_set_pixels(GtkWidget *widget, PlotSize *ps)
{
    ps->set_pixels();
}

void
plot_size_set_percent(GtkWidget *widget, PlotSize *ps)
{
    ps->set_percent();
}

using PlotSizePtr = std::unique_ptr<PlotSize>;

class GncGtkPlotSizeUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkPlotSizeUIItem(PlotSizePtr&& plot_size) :
        GncOptionGtkUIItem{plot_size->get_widget(), GncOptionUIType::PLOT_SIZE},
        m_plot_size{std::move(plot_size)} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        m_plot_size->set_entry_from_option(option);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        m_plot_size->set_option_from_entry(option);
    }
    PlotSize* get_plot_size() { return m_plot_size.get(); }
private:
PlotSizePtr m_plot_size;
};

template<> void
create_option_widget<GncOptionUIType::PLOT_SIZE> (GncOption& option,
                                                  GtkGrid *page_box, int row)
{

    auto enclosing = gtk_frame_new(NULL);
    gtk_widget_set_halign (GTK_WIDGET(enclosing), GTK_ALIGN_START);
    set_name_label(option, page_box, row, false);

    option.set_ui_item(std::make_unique<GncGtkPlotSizeUIItem>(std::make_unique<PlotSize>(option)));
    option.set_ui_item_from_option();

    auto widget{option_get_gtk_widget(&option)};
    gtk_container_add(GTK_CONTAINER(enclosing), widget);

    gtk_widget_show_all(enclosing);
    grid_attach_widget(page_box, enclosing, row);

    auto ui_item{dynamic_cast<GncGtkPlotSizeUIItem*>(option.get_ui_item())};
    if (ui_item)
        g_signal_connect(G_OBJECT(ui_item->get_plot_size()->get_spinner()), "changed",
                         G_CALLBACK(gnc_option_changed_widget_cb), &option);
}

static GtkWidget *
create_budget_widget(GncOption& option)
{
    GtkTreeModel *tm;
    GtkComboBox *cb;
    GtkCellRenderer *cr;

    tm = gnc_tree_model_budget_new(gnc_get_current_book());
    cb = GTK_COMBO_BOX(gtk_combo_box_new_with_model(tm));
    g_object_unref(tm);
    cr = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(cb), cr, TRUE);

    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(cb), cr, "text",
                                   BUDGET_NAME_COLUMN, NULL);
    return GTK_WIDGET(cb);
}

class GncGtkBudgetUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkBudgetUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::BUDGET} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        GtkTreeIter iter;
        auto widget{GTK_COMBO_BOX(get_widget())};
        auto instance{option.get_value<const QofInstance*>()};
        if (instance)
        {
            auto tree_model{gtk_combo_box_get_model(widget)};
            if (gnc_tree_model_budget_get_iter_for_budget(tree_model, &iter,
                                                          GNC_BUDGET(instance)))
                gtk_combo_box_set_active_iter(widget, &iter);
        }
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        GtkTreeIter iter;
        auto widget{GTK_COMBO_BOX(get_widget())};
        if (gtk_combo_box_get_active_iter(widget, &iter))
        {
            auto tree_model{gtk_combo_box_get_model(widget)};
            auto budget{gnc_tree_model_budget_get_budget(tree_model, &iter)};
            option.set_value(qof_instance_cast(budget));
        }
    }
};

template<> void
create_option_widget<GncOptionUIType::BUDGET> (GncOption& option,
                                               GtkGrid *page_box, int row)
{
    auto widget{create_budget_widget(option)};

    option.set_ui_item(std::make_unique<GncGtkBudgetUIItem>(widget));
    option.set_ui_item_from_option();

    /* Maybe connect destroy handler for tree model here? */
    g_signal_connect(G_OBJECT(widget), "changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);

    wrap_widget(option, widget, page_box, row);
}

static void
gnc_options_ui_factory_initialize(void)
{
    GncOptionUIFactory::set_func(GncOptionUIType::BOOLEAN,
                                 create_option_widget<GncOptionUIType::BOOLEAN>);
    GncOptionUIFactory::set_func(GncOptionUIType::STRING,
                                 create_option_widget<GncOptionUIType::STRING>);
    GncOptionUIFactory::set_func(GncOptionUIType::TEXT,
                                 create_option_widget<GncOptionUIType::TEXT>);
    GncOptionUIFactory::set_func(GncOptionUIType::CURRENCY,
                                 create_option_widget<GncOptionUIType::CURRENCY>);
    GncOptionUIFactory::set_func(GncOptionUIType::COMMODITY,
                                 create_option_widget<GncOptionUIType::COMMODITY>);
    GncOptionUIFactory::set_func(GncOptionUIType::MULTICHOICE,
                                 create_option_widget<GncOptionUIType::MULTICHOICE>);
    GncOptionUIFactory::set_func(GncOptionUIType::DATE_ABSOLUTE,
                                 create_option_widget<GncOptionUIType::DATE_ABSOLUTE>);
    GncOptionUIFactory::set_func(GncOptionUIType::DATE_RELATIVE,
                                 create_option_widget<GncOptionUIType::DATE_RELATIVE>);
    GncOptionUIFactory::set_func(GncOptionUIType::DATE_BOTH,
                                 create_option_widget<GncOptionUIType::DATE_BOTH>);
    GncOptionUIFactory::set_func(GncOptionUIType::ACCOUNT_LIST,
                                 create_option_widget<GncOptionUIType::ACCOUNT_LIST>);
    GncOptionUIFactory::set_func(GncOptionUIType::ACCOUNT_SEL,
                                 create_option_widget<GncOptionUIType::ACCOUNT_SEL>);
    GncOptionUIFactory::set_func(GncOptionUIType::LIST,
                                 create_option_widget<GncOptionUIType::LIST>);
    GncOptionUIFactory::set_func(GncOptionUIType::NUMBER_RANGE,
                                 create_option_widget<GncOptionUIType::NUMBER_RANGE>);
    GncOptionUIFactory::set_func(GncOptionUIType::COLOR,
                                 create_option_widget<GncOptionUIType::COLOR>);
    GncOptionUIFactory::set_func(GncOptionUIType::FONT,
                                 create_option_widget<GncOptionUIType::FONT>);
    GncOptionUIFactory::set_func(GncOptionUIType::PLOT_SIZE,
                                 create_option_widget<GncOptionUIType::PLOT_SIZE>);
    GncOptionUIFactory::set_func(GncOptionUIType::BUDGET,
                                 create_option_widget<GncOptionUIType::BUDGET>);
    GncOptionUIFactory::set_func(GncOptionUIType::PIXMAP,
                                 create_option_widget<GncOptionUIType::PIXMAP>);
    GncOptionUIFactory::set_func(GncOptionUIType::RADIOBUTTON,
                                 create_option_widget<GncOptionUIType::RADIOBUTTON>);
    GncOptionUIFactory::set_func(GncOptionUIType::DATE_FORMAT,
                                 create_option_widget<GncOptionUIType::DATE_FORMAT>);


}
