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
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <algorithm>
#include <cstdint>
#include <memory>
#include <qof.h>
#include <gnc-engine.h> // for GNC_MOD_GUI
#include <gnc-ui-util.h> // for gnc_get_current_book
#include <gnc-commodity.h> // for GNC_COMMODITY
#include "gnc-account-sel.h" // for GNC_ACCOUNT_SEL
#include "gnc-currency-edit.h" //for GNC_CURRENCY_EDIT
#include "gnc-commodity-edit.h" //for gnc_commodity_get_string
#include "gnc-date-edit.h" // for gnc_date_edit
#include "gnc-date-format.h" //for GNC_DATE_FORMAT
#include "gnc-general-select.h" // for GNC_GENERAL_SELECT
#include "gnc-option-uitype.hpp"
#include "account-quickfill.h" // for GncAccountListItem
#include "gnc-tree-model-budget.h" // for GncBudgetListItem
#include "misc-gnome-utils.h" // for xxxgtk_textview_set_text
#include "dialog-utils.h"
#include "gnc-gtk-utils.h"

/* GTK/GDK can include windows.h after the option headers. Its ABSOLUTE macro
 * conflicts with the RelativeDatePeriod enumerator used below.
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
    gtk_box_append (GTK_BOX(enclosing), GTK_WIDGET(widget));
    set_tool_tip(option, enclosing);
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
        auto widget{GTK_CHECK_BUTTON(get_widget())};
        gtk_check_button_set_active(widget, option.get_value<bool>());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_CHECK_BUTTON(get_widget())};
        option.set_value(static_cast<bool>(gtk_check_button_get_active(widget)));
    }
    SCM get_widget_scm_value(const GncOption& option) const override
    {
        auto widget{GTK_CHECK_BUTTON(get_widget())};
        return gtk_check_button_get_active(widget) ?
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
        gnc_entry_set_text(widget, option.get_value<std::string>().c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_ENTRY(get_widget())};
        option.set_value(std::string{gnc_entry_get_text(widget)});
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
    gtk_box_append (GTK_BOX(enclosing), GTK_WIDGET(widget));
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
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
        auto str{xxxgtk_textview_get_text(widget)};
        option.set_value(std::string{str});
        g_free (str);
    }
};

template<> void
create_option_widget<GncOptionUIType::TEXT> (GncOption& option, GtkGrid *page_box, int row)
{
    auto scrolled_window = gtk_scrolled_window_new();
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_NEVER,
                                   GTK_POLICY_AUTOMATIC);
    gnc_widget_set_all_margins (scrolled_window, 2);
    auto frame = gtk_frame_new(NULL);
    gtk_frame_set_child (GTK_FRAME(frame), GTK_WIDGET(scrolled_window));

    auto enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 10);
    gtk_widget_set_vexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(enclosing), TRUE);
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    auto widget = gtk_text_view_new();
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(widget), GTK_WRAP_WORD);
    gtk_widget_set_size_request(widget, 400, -1);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(widget), TRUE);
    gtk_text_view_set_accepts_tab (GTK_TEXT_VIEW(widget), FALSE);

    auto ui_item{std::make_unique<GncGtkTextUIItem>(widget)};
    auto text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget));
    option.set_ui_item(std::move(ui_item));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(text_buffer), "changed",
                     G_CALLBACK(gnc_option_changed_option_cb), &option);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_WIDGET(widget));
    gtk_box_append (GTK_BOX(enclosing), GTK_WIDGET(frame));
    set_name_label(option, page_box, row, true);
    set_tool_tip(option, enclosing);
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

static GtkStringList *
create_permissible_values_model (GncOption& option)
{
    auto values = gtk_string_list_new (nullptr);
    auto count = option.num_permissible_values ();

    g_return_val_if_fail (count >= 0, values);
    for (decltype(count) index = 0; index < count; index++)
    {
        auto value = option.permissible_value_name (index);

        gtk_string_list_append (values, value && *value ? _(value) : "");
    }

    return values;
}

static void
option_dropdown_selection_changed_cb (GObject *object, GParamSpec *pspec,
                                      gpointer user_data)
{
    gnc_option_changed_widget_cb (GTK_WIDGET (object),
                                  static_cast<GncOption *> (user_data));
    (void)pspec;
}

static GtkWidget *
create_multichoice_widget (GncOption& option)
{
    return GTK_WIDGET (gnc_gtk_drop_down_new (G_LIST_MODEL (create_permissible_values_model (option)),
                                               nullptr));
}

class GncGtkMultichoiceUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkMultichoiceUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::MULTICHOICE} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_DROP_DOWN(get_widget())};
        gtk_drop_down_set_selected(widget, option.get_value<uint16_t>());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_DROP_DOWN(get_widget())};
        auto selected{gtk_drop_down_get_selected(widget)};

        if (selected != GTK_INVALID_LIST_POSITION)
            option.set_value<uint16_t>(static_cast<uint16_t>(selected));
    }
    SCM get_widget_scm_value(const GncOption& option) const override
    {
        auto widget{GTK_DROP_DOWN(get_widget())};
        auto selected{gtk_drop_down_get_selected(widget)};

        if (selected == GTK_INVALID_LIST_POSITION)
            return SCM_BOOL_F;

        return scm_from_utf8_symbol(option.permissible_value(selected));
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
    g_signal_connect(G_OBJECT(widget), "notify::selected",
                     G_CALLBACK(option_dropdown_selection_changed_cb), &option);
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
    m_entry = GTK_WIDGET (gnc_gtk_drop_down_new (G_LIST_MODEL (create_permissible_values_model (option)),
                                                  nullptr));
    gtk_drop_down_set_selected (GTK_DROP_DOWN (m_entry), 0);
    m_handler_id = g_signal_connect (m_entry, "notify::selected",
                                     G_CALLBACK (option_dropdown_selection_changed_cb),
                                     &option);
}

void
RelativeDateEntry::set_entry_from_option(GncOption& option)
{
    gtk_drop_down_set_selected (GTK_DROP_DOWN (m_entry), option.get_value<uint16_t>());
}

void
RelativeDateEntry::set_option_from_entry(GncOption& option)
{
    auto selected = gtk_drop_down_get_selected (GTK_DROP_DOWN (m_entry));

    if (selected != GTK_INVALID_LIST_POSITION)
        option.set_value<uint16_t>(static_cast<uint16_t>(selected));
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
    m_abs_button{gtk_check_button_new()},
    m_abs_entry{std::make_unique<AbsoluteDateEntry>(option)},
    m_rel_button{gtk_check_button_new()},
    m_rel_entry{std::make_unique<RelativeDateEntry>(option)}
{
    gtk_check_button_set_group (GTK_CHECK_BUTTON(m_abs_button), GTK_CHECK_BUTTON(m_rel_button));
    gtk_box_set_homogeneous (GTK_BOX(m_widget), FALSE);
    m_abs_hdlr = g_signal_connect(G_OBJECT(m_abs_button), "toggled",
                                  G_CALLBACK(date_set_absolute_cb), &option);
    m_rel_hdlr = g_signal_connect(G_OBJECT(m_rel_button), "toggled",
                                  G_CALLBACK(date_set_relative_cb), &option);

    gtk_box_append (GTK_BOX(m_widget), GTK_WIDGET(m_abs_button));
    gtk_box_append (GTK_BOX(m_widget), GTK_WIDGET(m_abs_entry->get_entry()));
    gtk_box_append (GTK_BOX(m_widget), GTK_WIDGET(m_rel_button));
    gtk_box_append (GTK_BOX(m_widget), GTK_WIDGET(m_rel_entry->get_entry()));
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
    gtk_check_button_set_active(GTK_CHECK_BUTTON(m_rel_button),
                                 !m_use_absolute);
    gtk_check_button_set_active(GTK_CHECK_BUTTON(m_abs_button),
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

        gtk_box_append (GTK_BOX(enclosing), GTK_WIDGET(widget));
    }
    else
    {
        enclosing = gtk_frame_new(nullptr);
        g_object_set(G_OBJECT(widget), "margin", 3, NULL);

        gtk_frame_set_child (GTK_FRAME(enclosing), GTK_WIDGET(widget));
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

static constexpr const char* s_account_list_context_data{
    "gnc-account-list-context"};
static constexpr const char* s_account_list_model_data{
    "gnc-account-list-model"};
static constexpr const char* s_account_list_roots_data{
    "gnc-account-list-roots"};
static constexpr const char* s_account_list_selection_data{
    "gnc-account-list-selection"};
static constexpr const char* s_account_list_source_data{
    "gnc-account-list-source"};
static constexpr const char* s_account_list_expansion_listener_data{
    "gnc-account-list-expansion-listener"};

struct AccountListSelectionContext
{
    GncOption *option;
    gboolean include_type[NUM_ACCOUNT_TYPES];
    gboolean show_hidden;
    gboolean synchronizing;
    gboolean updating_expansion;
    gboolean restore_expansion;
    guint restore_source_id;
    GWeakRef root;
    GncOptionAccountList selected;
    GncOptionAccountList expanded;
    std::string search;
};

static void
account_list_selection_context_free (gpointer data)
{
    auto context = static_cast<AccountListSelectionContext *> (data);

    if (context->restore_source_id)
        g_source_remove (context->restore_source_id);
    g_weak_ref_clear (&context->root);
    delete context;
}

static gboolean
account_list_contains_guid (const GncOptionAccountList& accounts,
                            const GncGUID *guid)
{
    for (const auto& account_guid : accounts)
        if (guid_equal (&account_guid, guid))
            return TRUE;
    return FALSE;
}

static void
account_list_set_guid_selected (GncOptionAccountList& accounts,
                                const GncGUID *guid, gboolean selected)
{
    auto iter = accounts.begin ();

    while (iter != accounts.end () && !guid_equal (&*iter, guid))
        ++iter;
    if (selected && iter == accounts.end ())
        accounts.push_back (*guid);
    else if (!selected && iter != accounts.end ())
        accounts.erase (iter);
}

static AccountListSelectionContext *
account_list_get_context (GtkWidget *root)
{
    return static_cast<AccountListSelectionContext *> (
        g_object_get_data (G_OBJECT (root), s_account_list_context_data));
}

static GtkTreeListModel *
account_list_get_model (GtkWidget *root)
{
    return GTK_TREE_LIST_MODEL (
        g_object_get_data (G_OBJECT (root), s_account_list_model_data));
}

static GListStore *
account_list_get_roots (GtkWidget *root)
{
    return G_LIST_STORE (
        g_object_get_data (G_OBJECT (root), s_account_list_roots_data));
}

static GtkSelectionModel *
account_list_get_selection (GtkWidget *root)
{
    return GTK_SELECTION_MODEL (
        g_object_get_data (G_OBJECT (root), s_account_list_selection_data));
}

static Account *
account_list_get_account (gpointer item)
{
    if (!GTK_IS_TREE_LIST_ROW (item))
        return nullptr;
    auto account = gtk_tree_list_row_get_item (GTK_TREE_LIST_ROW (item));
    auto result = GNC_IS_ACCOUNT (account) ? GNC_ACCOUNT (account) : nullptr;
    g_clear_object (&account);
    return result;
}

static gboolean
account_list_name_matches_search (Account *account, const std::string& search)
{
    if (search.empty ())
        return TRUE;

    auto name = xaccAccountGetName (account);
    auto folded_search = g_utf8_casefold (search.c_str (), -1);
    auto folded_name = g_utf8_casefold (name ? name : "", -1);
    auto matches = g_strstr_len (folded_name, -1, folded_search) != nullptr;

    g_free (folded_name);
    g_free (folded_search);
    return matches;
}

static gboolean
account_list_account_matches_filter (Account *account,
                                     AccountListSelectionContext *context)
{
    auto type = xaccAccountGetType (account);

    return type >= 0 && type < NUM_ACCOUNT_TYPES &&
           context->include_type[type] &&
           (context->show_hidden || !xaccAccountIsHidden (account)) &&
           account_list_name_matches_search (account, context->search);
}

static gboolean
account_list_account_is_visible (Account *account,
                                 AccountListSelectionContext *context)
{
    auto children = gnc_account_get_children (account);
    gboolean visible = account_list_account_matches_filter (account, context);

    for (auto node = children; node && !visible; node = node->next)
        visible = account_list_account_is_visible (
            static_cast<Account *> (node->data), context);
    g_list_free (children);
    return visible;
}

static void
account_list_append_children (GListStore *store, Account *parent,
                              AccountListSelectionContext *context)
{
    auto children = gnc_account_get_children_sorted (parent);

    for (auto node = children; node; node = node->next)
    {
        auto account = static_cast<Account *> (node->data);

        if (account_list_account_is_visible (account, context))
            g_list_store_append (store, account);
    }
    g_list_free (children);
}

static GListModel *
account_list_create_children_cb (gpointer item, gpointer user_data)
{
    auto root = GTK_WIDGET (user_data);
    auto context = account_list_get_context (root);
    auto account = GNC_ACCOUNT (item);
    auto children = g_list_store_new (GNC_TYPE_ACCOUNT);

    account_list_append_children (children, account, context);
    return G_LIST_MODEL (children);
}

static void
account_list_prune_selection (AccountListSelectionContext *context)
{
    auto book = gnc_get_current_book ();
    auto exists = [book] (const GncGUID& guid)
    {
        return xaccAccountLookup (&guid, book) != nullptr;
    };

    context->selected.erase (
        std::remove_if (context->selected.begin (), context->selected.end (),
                        [&exists] (const GncGUID& guid)
                        {
                            return !exists (guid);
                        }),
        context->selected.end ());
    context->expanded.erase (
        std::remove_if (context->expanded.begin (), context->expanded.end (),
                        [&exists] (const GncGUID& guid)
                        {
                            return !exists (guid);
                        }),
        context->expanded.end ());
}

static gboolean
account_list_has_selected_descendant (Account *account,
                                      AccountListSelectionContext *context)
{
    auto book = gnc_get_current_book ();

    for (const auto& guid : context->selected)
    {
        auto selected = xaccAccountLookup (&guid, book);

        if (selected && xaccAccountHasAncestor (selected, account))
            return TRUE;
    }
    return FALSE;
}

static void
account_list_apply_expansion (GtkWidget *root);

static void
account_list_apply_selection (GtkWidget *root)
{
    auto context = account_list_get_context (root);
    auto selection = account_list_get_selection (root);
    auto model = G_LIST_MODEL (account_list_get_model (root));
    auto was_synchronizing = context->synchronizing;

    context->synchronizing = TRUE;
    gtk_selection_model_unselect_all (selection);
    for (guint index = 0; index < g_list_model_get_n_items (model); ++index)
    {
        auto item = g_list_model_get_item (model, index);
        auto account = account_list_get_account (item);

        if (account && account_list_contains_guid (context->selected,
                                                   xaccAccountGetGUID (account)))
            gtk_selection_model_select_item (selection, index, FALSE);
        g_clear_object (&item);
    }
    context->synchronizing = was_synchronizing;
}

static void
account_list_apply_expansion (GtkWidget *root)
{
    auto context = account_list_get_context (root);
    auto model = account_list_get_model (root);

    context->updating_expansion = TRUE;
    for (guint position = 0;
         position < g_list_model_get_n_items (G_LIST_MODEL (model));
         ++position)
    {
        auto row = gtk_tree_list_model_get_row (model, position);
        auto account = account_list_get_account (row);

        if (account && gtk_tree_list_row_is_expandable (row))
        {
            auto expanded = !context->search.empty () ||
                account_list_contains_guid (context->expanded,
                                            xaccAccountGetGUID (account)) ||
                account_list_has_selected_descendant (account, context);

            gtk_tree_list_row_set_expanded (row, expanded);
        }
        g_clear_object (&row);
    }
    context->updating_expansion = FALSE;
}

static gboolean
account_list_restore_selection_cb (gpointer data)
{
    auto context = static_cast<AccountListSelectionContext *> (data);
    auto root = GTK_WIDGET (g_weak_ref_get (&context->root));

    auto restore_expansion = context->restore_expansion;
    context->restore_source_id = 0;
    context->restore_expansion = FALSE;
    if (root)
    {
        if (restore_expansion)
            account_list_apply_expansion (root);
        account_list_apply_selection (root);
        g_object_unref (root);
    }
    context->synchronizing = FALSE;
    return G_SOURCE_REMOVE;
}

static void
account_list_schedule_restore (AccountListSelectionContext *context,
                               gboolean restore_expansion)
{
    context->synchronizing = TRUE;
    context->restore_expansion |= restore_expansion;
    if (!context->restore_source_id)
        context->restore_source_id = g_idle_add (
            account_list_restore_selection_cb, context);
}

static void
account_list_rebuild (GtkWidget *root)
{
    auto context = account_list_get_context (root);
    auto roots = account_list_get_roots (root);
    auto book_root = gnc_book_get_root_account (gnc_get_current_book ());

    account_list_prune_selection (context);
    g_list_store_remove_all (roots);
    account_list_append_children (roots, book_root, context);
    account_list_schedule_restore (context, TRUE);
}

static void
account_list_source_items_changed_cb (GListModel *source, guint position,
                                      guint removed, guint added, GtkWidget *root)
{
    account_list_rebuild (root);
    (void)source;
    (void)position;
    (void)removed;
    (void)added;
}

static void
account_list_model_items_changed_cb (GListModel *model, guint position,
                                     guint removed, guint added,
                                     GtkWidget *root)
{
    auto context = account_list_get_context (root);

    if (!context->synchronizing && !context->updating_expansion)
        account_list_schedule_restore (context, FALSE);
    (void)model;
    (void)position;
    (void)removed;
    (void)added;
}

static void
account_list_selection_changed_cb (GtkSelectionModel *selection, guint position,
                                   guint n_items, GtkWidget *root)
{
    auto context = account_list_get_context (root);
    auto model = G_LIST_MODEL (account_list_get_model (root));

    if (context->synchronizing)
        return;
    for (guint index = position; index < position + n_items; ++index)
    {
        auto item = g_list_model_get_item (model, index);
        auto account = account_list_get_account (item);

        if (account)
            account_list_set_guid_selected (
                context->selected, xaccAccountGetGUID (account),
                gtk_selection_model_is_selected (selection, index));
        g_clear_object (&item);
    }
    gnc_option_changed_widget_cb (root, context->option);
}

static void
account_list_row_expanded_cb (GtkTreeListRow *row, GParamSpec *pspec,
                              GtkWidget *root)
{
    auto context = account_list_get_context (root);
    auto account = account_list_get_account (row);

    if (!context->updating_expansion && account)
        account_list_set_guid_selected (
            context->expanded, xaccAccountGetGUID (account),
            gtk_tree_list_row_get_expanded (row));
    (void)pspec;
}

static void
account_list_item_setup_cb (GtkSignalListItemFactory *factory,
                            GtkListItem *list_item, gpointer user_data)
{
    auto expander = GTK_TREE_EXPANDER (gtk_tree_expander_new ());
    auto label = gtk_label_new (nullptr);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_tree_expander_set_child (expander, label);
    gtk_list_item_set_child (list_item, GTK_WIDGET (expander));
    (void)factory;
    (void)user_data;
}

static void
account_list_item_bind_cb (GtkSignalListItemFactory *factory,
                           GtkListItem *list_item, gpointer user_data)
{
    auto root = GTK_WIDGET (user_data);
    auto row = GTK_TREE_LIST_ROW (gtk_list_item_get_item (list_item));
    auto account = account_list_get_account (row);
    auto expander = GTK_TREE_EXPANDER (gtk_list_item_get_child (list_item));
    auto label = GTK_LABEL (gtk_tree_expander_get_child (expander));

    gtk_label_set_text (label, account ? xaccAccountGetName (account) : "");
    gtk_tree_expander_set_list_row (expander, row);
    if (!g_object_get_data (G_OBJECT (row),
                            s_account_list_expansion_listener_data))
    {
        g_signal_connect_object (row, "notify::expanded",
                                 G_CALLBACK (account_list_row_expanded_cb),
                                 root, G_CONNECT_DEFAULT);
        g_object_set_data (G_OBJECT (row),
                           s_account_list_expansion_listener_data, root);
    }
    (void)factory;
}

static void
account_select_all_cb (GtkWidget *widget, gpointer data)
{
    auto root = GTK_WIDGET (data);
    auto context = account_list_get_context (root);
    auto book_root = gnc_book_get_root_account (gnc_get_current_book ());
    auto accounts = gnc_account_get_descendants (book_root);

    for (auto node = accounts; node; node = node->next)
    {
        auto account = static_cast<Account *> (node->data);

        if (account_list_account_matches_filter (account, context))
            account_list_set_guid_selected (context->selected,
                                            xaccAccountGetGUID (account),
                                            TRUE);
    }
    g_list_free (accounts);
    account_list_apply_selection (root);
    gnc_option_changed_widget_cb (widget, context->option);
}

static void
account_clear_all_cb (GtkWidget *widget, gpointer data)
{
    auto root = GTK_WIDGET (data);
    auto context = account_list_get_context (root);

    context->selected.clear ();
    account_list_apply_selection (root);
    gnc_option_changed_widget_cb (widget, context->option);
}

static void
account_select_children_cb (GtkWidget *widget, gpointer data)
{
    auto root = GTK_WIDGET (data);
    auto context = account_list_get_context (root);
    auto book = gnc_get_current_book ();
    auto parents = context->selected;

    for (const auto& guid : parents)
    {
        auto parent = xaccAccountLookup (&guid, book);
        auto accounts = parent
            ? gnc_account_get_descendants (parent)
            : nullptr;

        for (auto node = accounts; node; node = node->next)
        {
            auto account = static_cast<Account *> (node->data);

            if (account_list_account_matches_filter (account, context))
                account_list_set_guid_selected (
                    context->selected, xaccAccountGetGUID (account), TRUE);
        }
        g_list_free (accounts);
    }
    account_list_apply_expansion (root);
    account_list_apply_selection (root);
    gnc_option_changed_widget_cb (widget, context->option);
}

static void
account_set_default_cb (GtkWidget *widget, gpointer data)
{
    auto root = GTK_WIDGET (data);
    auto context = account_list_get_context (root);

    context->selected = context->option->get_default_value<GncOptionAccountList> ();
    account_list_prune_selection (context);
    account_list_apply_expansion (root);
    account_list_apply_selection (root);
    gnc_option_changed_widget_cb (widget, context->option);
}

static void
show_hidden_toggled_cb (GtkWidget *widget, gpointer data)
{
    auto root = GTK_WIDGET (data);
    auto context = account_list_get_context (root);

    context->show_hidden = gtk_check_button_get_active (GTK_CHECK_BUTTON (widget));
    account_list_rebuild (root);
    gnc_option_changed_widget_cb (widget, context->option);
}

static void
account_list_search_changed_cb (GtkSearchEntry *entry, GtkWidget *root)
{
    auto context = account_list_get_context (root);

    context->search = gtk_editable_get_text (GTK_EDITABLE (entry));
    account_list_rebuild (root);
}

class GncGtkAccountListUIItem : public GncOptionGtkUIItem
{
public:
    explicit GncGtkAccountListUIItem (GtkWidget *widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::ACCOUNT_LIST} {}
    void set_ui_item_from_option (GncOption& option) noexcept override
    {
        auto root = get_widget ();
        auto context = account_list_get_context (root);

        context->selected = option.get_value<GncOptionAccountList> ();
        account_list_prune_selection (context);
        account_list_apply_expansion (root);
        account_list_apply_selection (root);
    }
    void set_option_from_ui_item (GncOption& option) noexcept override
    {
        auto context = account_list_get_context (get_widget ());

        account_list_prune_selection (context);
        option.set_value (context->selected);
    }
};

static GtkWidget *
create_account_widget (GncOption& option, char *name)
{
    auto root = gtk_frame_new (name);
    auto vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10);
    auto search_entry = gtk_search_entry_new ();
    auto scrolled_window = gtk_scrolled_window_new ();
    auto button_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    auto context = new AccountListSelectionContext {};
    auto account_types = option.account_type_list ();
    auto source = gnc_get_shared_account_name_list_model (
        gnc_book_get_root_account (gnc_get_current_book ()),
        "gnc-option-account-list", nullptr, nullptr);
    auto roots = g_list_store_new (GNC_TYPE_ACCOUNT);
    GtkListItemFactory *factory;
    GtkTreeListModel *model;
    GtkSelectionModel *selection;
    GtkWidget *view;
    GtkWidget *button;

    context->option = &option;
    context->show_hidden = TRUE;
    for (auto& include_type : context->include_type)
        include_type = account_types == nullptr;
    for (auto node = account_types; node; node = node->next)
    {
        auto type = static_cast<GNCAccountType> (GPOINTER_TO_INT (node->data));

        if (type >= 0 && type < NUM_ACCOUNT_TYPES)
            context->include_type[type] = TRUE;
    }
    g_list_free (account_types);
    g_weak_ref_init (&context->root, root);
    g_object_set_data_full (G_OBJECT (root), s_account_list_context_data,
                            context, account_list_selection_context_free);
    g_object_set_data_full (G_OBJECT (root), s_account_list_roots_data, roots,
                            g_object_unref);
    g_object_set_data_full (G_OBJECT (root), s_account_list_source_data,
                            g_object_ref (source), g_object_unref);
    account_list_append_children (roots, gnc_book_get_root_account (
                                   gnc_get_current_book ()), context);

    model = gtk_tree_list_model_new (G_LIST_MODEL (roots), FALSE, FALSE,
                                     account_list_create_children_cb, root,
                                     nullptr);
    selection = option.is_multiselect ()
        ? GTK_SELECTION_MODEL (
            gtk_multi_selection_new (G_LIST_MODEL (model)))
        : GTK_SELECTION_MODEL (
            gtk_single_selection_new (G_LIST_MODEL (model)));
    g_object_set_data_full (G_OBJECT (root), s_account_list_model_data, model,
                            g_object_unref);
    g_object_set_data_full (G_OBJECT (root), s_account_list_selection_data,
                            selection, g_object_unref);
    g_signal_connect_object (model, "items-changed",
                             G_CALLBACK (account_list_model_items_changed_cb),
                             root, G_CONNECT_DEFAULT);
    g_signal_connect_object (source, "items-changed",
                             G_CALLBACK (account_list_source_items_changed_cb),
                             root, G_CONNECT_DEFAULT);

    factory = gtk_signal_list_item_factory_new ();
    g_signal_connect (factory, "setup",
                      G_CALLBACK (account_list_item_setup_cb), nullptr);
    g_signal_connect (factory, "bind", G_CALLBACK (account_list_item_bind_cb),
                      root);
    view = gtk_list_view_new (GTK_SELECTION_MODEL (g_object_ref (selection)),
                              GTK_LIST_ITEM_FACTORY (factory));
    gtk_widget_set_vexpand (view, TRUE);
    gtk_widget_set_hexpand (view, TRUE);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                    GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scrolled_window), view);
    gnc_widget_set_all_margins (scrolled_window, 5);

    gtk_frame_set_child (GTK_FRAME (root), vbox);
    gtk_box_append (GTK_BOX (vbox), search_entry);
    gtk_box_append (GTK_BOX (vbox), scrolled_window);
    gtk_box_append (GTK_BOX (vbox), button_box);
    option.set_ui_item (std::make_unique<GncGtkAccountListUIItem> (root));
    option.set_ui_item_from_option ();

    if (option.is_multiselect ())
    {
        button = gtk_button_new_with_label (_("Select All"));
        gtk_widget_set_tooltip_text (button, _("Select all accounts."));
        gtk_box_append (GTK_BOX (button_box), button);
        g_signal_connect (button, "clicked",
                          G_CALLBACK (account_select_all_cb), root);

        button = gtk_button_new_with_label (_("Clear All"));
        gtk_widget_set_tooltip_text (
            button, _("Clear the selection and unselect all accounts."));
        gtk_box_append (GTK_BOX (button_box), button);
        g_signal_connect (button, "clicked", G_CALLBACK (account_clear_all_cb),
                          root);

        button = gtk_button_new_with_label (_("Select Children"));
        gtk_widget_set_tooltip_text (
            button, _("Select all descendents of selected account."));
        gtk_box_append (GTK_BOX (button_box), button);
        g_signal_connect (button, "clicked",
                          G_CALLBACK (account_select_children_cb), root);
    }

    button = gtk_button_new_with_label (_("Select Default"));
    gtk_widget_set_tooltip_text (button, _("Select the default account selection."));
    gtk_box_append (GTK_BOX (button_box), button);
    g_signal_connect (button, "clicked", G_CALLBACK (account_set_default_cb), root);

    if (option.is_multiselect ())
    {
        auto hidden_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);

        gtk_box_append (GTK_BOX (vbox), hidden_box);
        button_box = hidden_box;
    }
    button = gtk_check_button_new_with_label (_("Show Hidden Accounts"));
    gtk_widget_set_tooltip_text (
        button, _("Show accounts that have been marked hidden."));
    gtk_check_button_set_active (GTK_CHECK_BUTTON (button), TRUE);
    gtk_box_append (GTK_BOX (button_box), button);
    g_signal_connect (button, "toggled", G_CALLBACK (show_hidden_toggled_cb), root);

    g_signal_connect (search_entry, "search-changed",
                      G_CALLBACK (account_list_search_changed_cb), root);
    g_signal_connect (selection, "selection-changed",
                      G_CALLBACK (account_list_selection_changed_cb), root);
    return root;
}

template<> void
create_option_widget<GncOptionUIType::ACCOUNT_LIST> (
    GncOption& option, GtkGrid *page_box, int row)
{
    auto enclosing = create_account_widget (option, nullptr);

    gtk_widget_set_vexpand (enclosing, TRUE);
    gtk_widget_set_hexpand (enclosing, TRUE);
    set_name_label (option, page_box, row, true);
    set_tool_tip (option, enclosing);
    grid_attach_widget (page_box, enclosing, row);
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
// Must cast it to const Account* to get the template specialization to recognize it.
        option.set_value(static_cast<const Account*>(gnc_account_sel_get_account(widget)));
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

    // Connect after the initial filter and value setup, so opening the option
    // page does not mark it as user-modified.
    option.set_ui_item(std::make_unique<GncGtkAccountSelUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect(widget, "account_sel_changed",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
    // wrap_widget sets the parent so this comes after.
}

static void
list_item_setup_cb (GtkSignalListItemFactory *factory,
                    GtkListItem *list_item,
                    gpointer user_data)
{
    auto label = gtk_label_new (nullptr);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
}

static void
list_item_bind_cb (GtkSignalListItemFactory *factory,
                   GtkListItem *list_item,
                   gpointer user_data)
{
    auto item = GTK_STRING_OBJECT (gtk_list_item_get_item (list_item));
    auto label = GTK_LABEL (gtk_list_item_get_child (list_item));

    gtk_label_set_text (label, gtk_string_object_get_string (item));
}

static GtkListItemFactory *
list_item_factory_new ()
{
    auto factory = gtk_signal_list_item_factory_new ();

    g_signal_connect (factory, "setup", G_CALLBACK (list_item_setup_cb), nullptr);
    g_signal_connect (factory, "bind", G_CALLBACK (list_item_bind_cb), nullptr);
    return GTK_LIST_ITEM_FACTORY (factory);
}

static GtkMultiSelection *
list_selection_from_option (GncOption *option)
{
    auto view = GTK_LIST_VIEW (option_get_gtk_widget (option));

    return GTK_MULTI_SELECTION (gtk_list_view_get_model (view));
}

static void
list_changed_cb (GtkSelectionModel *selection, guint position, guint n_items,
                 GncOption *option)
{
    gnc_option_changed_widget_cb (GTK_WIDGET (option_get_gtk_widget (option)), option);
}

static void
list_select_all_cb (GtkWidget *widget, gpointer data)
{
    auto option = static_cast<GncOption *>(data);
    auto selection = list_selection_from_option (option);

    gtk_selection_model_select_all (GTK_SELECTION_MODEL (selection));
}

static void
list_clear_all_cb (GtkWidget *widget, gpointer data)
{
    auto option = static_cast<GncOption *>(data);
    auto selection = list_selection_from_option (option);

    gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (selection));
}

static void
list_set_default_cb (GtkWidget *widget, gpointer data)
{
    auto option = static_cast<GncOption *>(data);

    option->set_value (option->get_default_value<GncMultichoiceOptionIndexVec> ());
    option->set_ui_item_from_option ();
    gnc_option_changed_widget_cb (option_get_gtk_widget (option), option);
}

class GncGtkListUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkListUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::LIST} {}

    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto selection = list_selection_from_option (&option);
        auto model = gtk_multi_selection_get_model (selection);
        const auto count = g_list_model_get_n_items (model);

        g_signal_handlers_block_by_func (selection, (gpointer)list_changed_cb, &option);
        gtk_selection_model_unselect_all (GTK_SELECTION_MODEL (selection));
        for (auto index : option.get_value<GncMultichoiceOptionIndexVec> ())
        {
            if (static_cast<guint> (index) < count)
                gtk_selection_model_select_item (GTK_SELECTION_MODEL (selection), index, FALSE);
        }
        g_signal_handlers_unblock_by_func (selection, (gpointer)list_changed_cb, &option);
    }

    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto selection = list_selection_from_option (&option);
        auto selected = gtk_selection_model_get_selection (GTK_SELECTION_MODEL (selection));
        GtkBitsetIter iter;
        guint index;
        GncMultichoiceOptionIndexVec values;

        if (gtk_bitset_iter_init_first (&iter, selected, &index))
        {
            do
            {
                values.push_back (static_cast<decltype(values)::value_type> (index));
            } while (gtk_bitset_iter_next (&iter, &index));
        }
        gtk_bitset_unref (selected);
        option.set_value (values);
    }
};

static GtkWidget *
create_list_widget (GncOption& option, char *name)
{
    auto frame = gtk_frame_new (name);
    auto hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    auto values = create_permissible_values_model (option);
    auto selection = gtk_multi_selection_new (G_LIST_MODEL (values));
    auto view = gtk_list_view_new (GTK_SELECTION_MODEL (selection), list_item_factory_new ());
    auto scrolled = gtk_scrolled_window_new ();

    gtk_box_set_homogeneous (GTK_BOX (hbox), FALSE);
    gtk_frame_set_child (GTK_FRAME (frame), hbox);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scrolled), view);
    gtk_widget_set_hexpand (scrolled, TRUE);
    gtk_widget_set_vexpand (scrolled, TRUE);

    option.set_ui_item (std::make_unique<GncGtkListUIItem> (view));
    option.set_ui_item_from_option ();

    gtk_box_append (GTK_BOX (hbox), scrolled);
    g_signal_connect (selection, "selection-changed", G_CALLBACK (list_changed_cb), &option);

    auto bbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_append (GTK_BOX (hbox), bbox);

    auto button = gtk_button_new_with_label (_("Select All"));
    gtk_box_append (GTK_BOX (bbox), button);
    gtk_widget_set_tooltip_text (button, _("Select all entries."));
    g_signal_connect (button, "clicked", G_CALLBACK (list_select_all_cb), &option);

    button = gtk_button_new_with_label (_("Clear All"));
    gtk_box_append (GTK_BOX (bbox), button);
    gtk_widget_set_tooltip_text (button, _("Clear the selection and unselect all entries."));
    g_signal_connect (button, "clicked", G_CALLBACK (list_clear_all_cb), &option);

    button = gtk_button_new_with_label (_("Select Default"));
    gtk_box_append (GTK_BOX (bbox), button);
    gtk_widget_set_tooltip_text (button, _("Select the default selection."));
    g_signal_connect (button, "clicked", G_CALLBACK (list_set_default_cb), &option);

    g_object_set (hbox, "margin", 3, nullptr);
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
    gtk_widget_set_visible (enclosing, true);
}
class GncGtkNumberRangeUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkNumberRangeUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::NUMBER_RANGE} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        double value;
        if (option.is_alternate())
            value = static_cast<double>(option.get_value<int>());
        else
            value = option.get_value<double>();

        gtk_spin_button_set_value(GTK_SPIN_BUTTON(get_widget()), value);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto value{gtk_spin_button_get_value(GTK_SPIN_BUTTON(get_widget()))};
        if (option.is_alternate())
            option.set_value<int>(static_cast<int>(value));
        else
            option.set_value<double>(value);
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
    gtk_editable_set_max_width_chars (GTK_EDITABLE(widget), num_digits);
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
            auto color_button = GTK_COLOR_DIALOG_BUTTON (get_widget ());
            gtk_color_dialog_button_set_rgba (color_button, &color);
        }
        g_free(rgba_str);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto color_button = GTK_COLOR_DIALOG_BUTTON (get_widget ());
        auto color = gtk_color_dialog_button_get_rgba (color_button);
        if (!color)
            return;
        auto rgba_str = g_strdup_printf("%2x%2x%2x%2x",
                                        (uint8_t)(color->red * 255),
                                        (uint8_t)(color->green * 255),
                                        (uint8_t)(color->blue * 255),
                                        (uint8_t)(color->alpha * 255));
        auto rgb_str = g_strdup_printf("%2x%2x%2x",
                                       (uint8_t)(color->red * 255),
                                       (uint8_t)(color->green * 255),
                                       (uint8_t)(color->blue * 255));
// sample-report.scm uses an old HTML4 attribute that doesn't understand alpha.
        option.set_value(std::string{rgb_str});
        g_free(rgba_str);
        g_free(rgb_str);
    }
};

template<> void
create_option_widget<GncOptionUIType::COLOR> (GncOption& option, GtkGrid *page_box, int row)
{
    auto dialog = gtk_color_dialog_new ();
    gtk_color_dialog_set_with_alpha (dialog, TRUE);
    auto widget = gtk_color_dialog_button_new (dialog);
    g_object_unref (dialog);

    option.set_ui_item(std::make_unique<GncGtkColorUIItem>(widget));
    option.set_ui_item_from_option();

    g_signal_connect(G_OBJECT(widget), "notify::rgba",
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
        auto description = pango_font_description_from_string (
            option.get_value<std::string>().c_str ());
        gtk_font_dialog_button_set_font_desc (
            GTK_FONT_DIALOG_BUTTON (get_widget ()), description);
        pango_font_description_free (description);
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto description = gtk_font_dialog_button_get_font_desc (
            GTK_FONT_DIALOG_BUTTON (get_widget ()));
        if (!description)
            return;
        auto font = pango_font_description_to_string (description);
        option.set_value(std::string{font});
        g_free (font);
    }
};

template<> void
create_option_widget<GncOptionUIType::FONT> (GncOption& option, GtkGrid *page_box, int row)
{
    auto dialog = gtk_font_dialog_new ();
    auto widget = gtk_font_dialog_button_new (dialog);
    g_object_unref (dialog);
    gtk_font_dialog_button_set_level (GTK_FONT_DIALOG_BUTTON (widget),
                                      GTK_FONT_LEVEL_FONT);
    gtk_font_dialog_button_set_use_font (GTK_FONT_DIALOG_BUTTON (widget),
                                         TRUE);
    gtk_font_dialog_button_set_use_size (GTK_FONT_DIALOG_BUTTON (widget),
                                         TRUE);

    option.set_ui_item(std::make_unique<GncGtkFontUIItem>(widget));
    option.set_ui_item_from_option();
    g_signal_connect(G_OBJECT(widget), "notify::font-desc",
                     G_CALLBACK(gnc_option_changed_widget_cb), &option);
    wrap_widget(option, widget, page_box, row);
}
static constexpr const char* s_pixmap_path_data{"gnc-pixmap-path"};
static constexpr const char* s_pixmap_entry_data{"gnc-pixmap-entry"};
static constexpr const char* s_pixmap_picture_data{"gnc-pixmap-image"};
static constexpr const char* s_pixmap_option_data{"gnc-pixmap-option"};
static constexpr int s_pixmap_preview_size{128};

struct PixmapOpenContext
{
    GWeakRef root;
    GtkFileDialog *dialog;
};

static void
pixmap_open_context_free (PixmapOpenContext *context)
{
    g_clear_object (&context->dialog);
    g_weak_ref_clear (&context->root);
    g_free (context);
}

static GncOption *
pixmap_get_option (GtkWidget *root)
{
    return root ? static_cast<GncOption *> (
        g_object_get_data (G_OBJECT (root), s_pixmap_option_data)) : nullptr;
}

static gboolean
pixmap_dialog_error_is_cancelled (const GError *error)
{
    return error && (g_error_matches (error, G_IO_ERROR, G_IO_ERROR_CANCELLED) ||
                     g_error_matches (error, GTK_DIALOG_ERROR,
                                      GTK_DIALOG_ERROR_DISMISSED));
}

static void
pixmap_set_path (GtkWidget *root, const char *path)
{
    auto entry = GTK_ENTRY (
        g_object_get_data (G_OBJECT (root), s_pixmap_entry_data));
    auto picture = GTK_PICTURE (
        g_object_get_data (G_OBJECT (root), s_pixmap_picture_data));

    g_object_set_data_full (G_OBJECT (root), s_pixmap_path_data,
                            g_strdup (path), g_free);
    gtk_editable_set_text (GTK_EDITABLE (entry), path ? path : "");
    gtk_picture_set_paintable (picture, nullptr);

    if (!path || !*path)
        return;

    GError *error = nullptr;
    auto pixbuf = gdk_pixbuf_new_from_file_at_size (path,
                                                  s_pixmap_preview_size,
                                                  s_pixmap_preview_size,
                                                  &error);
    if (!pixbuf)
    {
        PERR ("Unable to load image preview '%s': %s", path,
              error ? error->message : "unknown error");
        g_clear_error (&error);
        return;
    }

    auto texture = gnc_texture_new_from_pixbuf (pixbuf);
    if (texture)
        gtk_picture_set_paintable (picture, GDK_PAINTABLE (texture));
    g_clear_object (&texture);
    g_object_unref (pixbuf);
}

static void
pixmap_dialog_open_cb (GObject *source, GAsyncResult *result, gpointer user_data)
{
    auto context = static_cast<PixmapOpenContext *> (user_data);
    auto root = GTK_WIDGET (g_weak_ref_get (&context->root));
    auto option = pixmap_get_option (root);
    GError *error = nullptr;
    auto file = gtk_file_dialog_open_finish (GTK_FILE_DIALOG (source), result, &error);

    if (file && root && option)
    {
        auto path = g_file_get_path (file);
        if (path)
        {
            pixmap_set_path (root, path);
            gnc_option_changed_widget_cb (root, option);
            g_free (path);
        }
        else
            PERR ("Image selections must be local files.");
    }
    else if (error && !pixmap_dialog_error_is_cancelled (error))
        PERR ("Unable to select an image file: %s", error->message);

    g_clear_error (&error);
    g_clear_object (&file);
    if (root)
        g_object_unref (root);
    pixmap_open_context_free (context);
}

static void
pixmap_choose_clicked_cb (GtkButton *button, gpointer user_data)
{
    auto root = GTK_WIDGET (user_data);
    auto option = pixmap_get_option (root);
    if (!option)
        return;
    auto dialog = gtk_file_dialog_new ();
    auto path = static_cast<const char *> (
        g_object_get_data (G_OBJECT (root), s_pixmap_path_data));
    auto context = g_new0 (PixmapOpenContext, 1);
    auto window_root = gtk_widget_get_root (root);

    gtk_file_dialog_set_title (dialog, _("Select image"));
    if (path && *path)
    {
        auto file = g_file_new_for_path (path);
        gtk_file_dialog_set_initial_file (dialog, file);
        g_object_unref (file);
    }

    context->dialog = GTK_FILE_DIALOG (g_object_ref (dialog));
    g_weak_ref_init (&context->root, root);
    gtk_file_dialog_open (dialog, GTK_IS_WINDOW (window_root) ? GTK_WINDOW (window_root) : nullptr,
                          nullptr, pixmap_dialog_open_cb, context);
    g_object_unref (dialog);
}

static void
pixmap_clear_clicked_cb (GtkButton *button, gpointer user_data)
{
    auto root = GTK_WIDGET (user_data);
    auto option = pixmap_get_option (root);
    if (!option)
        return;

    pixmap_set_path (root, nullptr);
    gnc_option_changed_widget_cb (root, option);
}

class GncGtkPixmapUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkPixmapUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::PIXMAP} {}
    ~GncGtkPixmapUIItem() override
    {
        invalidate_root_option ();
    }
    void clear_ui_item() override
    {
        invalidate_root_option ();
        GncOptionGtkUIItem::clear_ui_item ();
    }
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto value = option.get_value<std::string> ();

        pixmap_set_path (get_widget (), value.empty () ? nullptr : value.c_str ());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto path = static_cast<const char *> (
            g_object_get_data (G_OBJECT (get_widget ()), s_pixmap_path_data));

        option.set_value (std::string {path ? path : ""});
    }
private:
    void invalidate_root_option ()
    {
        auto root = get_widget ();

        if (root)
            g_object_set_data (G_OBJECT (root), s_pixmap_option_data, nullptr);
    }
};

template<> void
create_option_widget<GncOptionUIType::PIXMAP> (GncOption& option,
                                               GtkGrid *page_box, int row)
{
    auto enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);
    auto picture = GTK_PICTURE (gtk_picture_new ());
    auto entry = gtk_entry_new ();
    auto choose = gtk_button_new_with_label (_("Select image"));
    auto clear = gtk_button_new_with_label (_("Clear"));

    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    gtk_picture_set_can_shrink (picture, TRUE);
    gtk_picture_set_content_fit (picture, GTK_CONTENT_FIT_CONTAIN);
    gtk_widget_set_size_request (GTK_WIDGET (picture), s_pixmap_preview_size,
                                 s_pixmap_preview_size);
    gtk_widget_set_halign (GTK_WIDGET (picture), GTK_ALIGN_START);
    gtk_widget_set_valign (GTK_WIDGET (picture), GTK_ALIGN_START);
    gtk_editable_set_editable (GTK_EDITABLE (entry), FALSE);
    gtk_widget_set_hexpand (entry, TRUE);
    gtk_widget_set_tooltip_text (choose, _("Select an image file."));
    gtk_widget_set_tooltip_text (clear, _("Clear any selected image file."));
    g_object_set_data (G_OBJECT (enclosing), s_pixmap_entry_data, entry);
    g_object_set_data (G_OBJECT (enclosing), s_pixmap_picture_data, picture);
    g_object_set_data (G_OBJECT (enclosing), s_pixmap_option_data, &option);

    gtk_box_append (GTK_BOX (enclosing), GTK_WIDGET (picture));
    gtk_box_append (GTK_BOX (enclosing), entry);
    gtk_box_append (GTK_BOX (enclosing), choose);
    gtk_box_append (GTK_BOX (enclosing), clear);
    option.set_ui_item (std::make_unique<GncGtkPixmapUIItem> (enclosing));
    option.set_ui_item_from_option ();

    g_signal_connect (choose, "clicked", G_CALLBACK (pixmap_choose_clicked_cb), enclosing);
    g_signal_connect (clear, "clicked", G_CALLBACK (pixmap_clear_clicked_cb), enclosing);
    set_name_label (option, page_box, row, false);
    set_tool_tip (option, enclosing);
    grid_attach_widget (page_box, enclosing, row);
    gtk_widget_set_visible (enclosing, true);
}
static GtkCheckButton *
radiobutton_get_button (GtkWidget *frame, guint index)
{
    auto buttons = static_cast<GPtrArray *> (
        g_object_get_data (G_OBJECT (frame), "gnc-radiobutton-buttons"));

    if (!buttons || index >= buttons->len)
        return nullptr;

    return GTK_CHECK_BUTTON (g_ptr_array_index (buttons, index));
}

static void
radiobutton_button_array_free (gpointer data)
{
    g_ptr_array_unref (static_cast<GPtrArray *> (data));
}

static void
radiobutton_set_cb (GtkCheckButton *button, gpointer data)
{
    auto option = static_cast<GncOption *>(data);
    auto frame = option_get_gtk_widget (option);
    const auto index = GPOINTER_TO_UINT (
        g_object_get_data (G_OBJECT (button), "gnc_radiobutton_index"));
    const auto current = GPOINTER_TO_UINT (
        g_object_get_data (G_OBJECT (frame), "gnc_radiobutton_index"));

    if (!gtk_check_button_get_active (button) || current == index)
        return;

    g_object_set_data (G_OBJECT (frame), "gnc_radiobutton_index",
                       GUINT_TO_POINTER (index));
    gnc_option_changed_widget_cb (frame, option);
}

class GncGtkCheckButtonGroupUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkCheckButtonGroupUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::RADIOBUTTON} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto index = option.get_value<uint16_t>();
        auto button = radiobutton_get_button (get_widget(), index);

        if (!button)
        {
            PERR("Invalid Radio Button Selection %hu", index);
            return;
        }

        g_signal_handlers_block_by_func (button, (gpointer)radiobutton_set_cb, &option);
        gtk_check_button_set_active (button, TRUE);
        g_signal_handlers_unblock_by_func (button, (gpointer)radiobutton_set_cb, &option);
        g_object_set_data (G_OBJECT (get_widget()), "gnc_radiobutton_index",
                           GUINT_TO_POINTER (index));
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto index = GPOINTER_TO_UINT (g_object_get_data (
            G_OBJECT (get_widget()), "gnc_radiobutton_index"));
        option.set_value<uint16_t>(static_cast<uint16_t>(index));
    }
};

static GtkWidget *
create_radiobutton_widget(char *name, GncOption& option)
{
    auto frame = gtk_frame_new (name);
    auto box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5);
    auto buttons = g_ptr_array_new ();
    GtkCheckButton *first_button = nullptr;
    auto count = option.num_permissible_values ();

    g_return_val_if_fail (count >= 0, nullptr);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);
    gtk_frame_set_child (GTK_FRAME (frame), box);
    g_object_set_data_full (G_OBJECT (frame), "gnc-radiobutton-buttons", buttons,
                            radiobutton_button_array_free);

    for (decltype(count) index = 0; index < count; index++)
    {
        auto label = option.permissible_value_name (index);
        auto button = GTK_CHECK_BUTTON (gtk_check_button_new_with_label (
            label && *label ? _(label) : ""));

        if (first_button)
            gtk_check_button_set_group (button, first_button);
        else
            first_button = button;

        g_object_set_data (G_OBJECT (button), "gnc_radiobutton_index",
                           GUINT_TO_POINTER (index));
        g_signal_connect (button, "toggled", G_CALLBACK (radiobutton_set_cb), &option);
        g_ptr_array_add (buttons, button);
        gtk_box_append (GTK_BOX (box), GTK_WIDGET (button));
    }

    option.set_ui_item (std::make_unique<GncGtkCheckButtonGroupUIItem> (frame));
    option.set_ui_item_from_option ();
    return frame;
}

template<> void
create_option_widget<GncOptionUIType::RADIOBUTTON> (GncOption& option,
                                                     GtkGrid *page_box, int row)
{
    auto enclosing = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5);

    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    set_name_label (option, page_box, row, true);
    set_tool_tip (option, enclosing);
    gtk_box_append (GTK_BOX (enclosing), create_radiobutton_widget (nullptr, option));
    grid_attach_widget (page_box, enclosing, row);
}

class GncGtkDateFormatUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkDateFormatUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::DATE_FORMAT} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GNC_DATE_FORMAT(get_widget())};
        auto [format, months, years, custom] = option.get_value<GncOptionDateFormat>();
        gnc_date_format_set_format(widget, format);
        gnc_date_format_set_months(widget, months);
        gnc_date_format_set_years(widget, years);
        gnc_date_format_set_custom(widget, custom.c_str());
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GNC_DATE_FORMAT(get_widget())};
        GncOptionDateFormat format{
            gnc_date_format_get_format(widget),
            gnc_date_format_get_months(widget),
            gnc_date_format_get_years(widget),
            gnc_date_format_get_custom(widget)};
        option.set_value(format);
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
    m_widget{gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 4)},
    m_pixel_button{gtk_check_button_new_with_label(_("Pixels"))},
    m_percent_button{gtk_check_button_new_with_label(_("Percent"))},
    m_range_spinner{GTK_WIDGET(create_range_spinner(option))},
    m_adj_pct{GTK_ADJUSTMENT(g_object_ref(gtk_adjustment_new(100.0, 10.0, 100.0, 1.0, 5.0, 0.0)))},
    m_adj_px{GTK_ADJUSTMENT(g_object_ref(gtk_adjustment_new(1000.0, 110.0, 10000.0, 10.0, 250.0, 0.0)))}
{
    gtk_check_button_set_group (GTK_CHECK_BUTTON(m_pixel_button), GTK_CHECK_BUTTON(m_percent_button));
    gtk_box_set_homogeneous(GTK_BOX(m_widget), FALSE);
    g_object_set (G_OBJECT(m_widget), "margin", 3, NULL);
    set_tool_tip(option, m_widget);

    gtk_box_append (GTK_BOX(m_widget), GTK_WIDGET(m_pixel_button));
    gtk_box_append (GTK_BOX(m_widget), GTK_WIDGET(m_percent_button));
    gtk_box_append (GTK_BOX(m_widget), GTK_WIDGET(m_range_spinner));

    gtk_check_button_set_active (GTK_CHECK_BUTTON(m_pixel_button), FALSE);
    gtk_check_button_set_active (GTK_CHECK_BUTTON(m_percent_button), TRUE);

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
        gtk_check_button_set_active(GTK_CHECK_BUTTON(m_pixel_button), TRUE);
    else
        gtk_check_button_set_active(GTK_CHECK_BUTTON(m_percent_button), TRUE);

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
    gtk_frame_set_child (GTK_FRAME(enclosing), GTK_WIDGET(widget));

    grid_attach_widget(page_box, enclosing, row);

    auto ui_item{dynamic_cast<GncGtkPlotSizeUIItem*>(option.get_ui_item())};
    if (ui_item)
        g_signal_connect(G_OBJECT(ui_item->get_plot_size()->get_spinner()), "changed",
                         G_CALLBACK(gnc_option_changed_widget_cb), &option);
}

static GtkWidget *
create_budget_widget (GncOption& option)
{
    auto model = gnc_budget_list_model_new (gnc_get_current_book ());
    auto expression = gtk_property_expression_new (GNC_TYPE_BUDGET_LIST_ITEM,
                                                   nullptr, "name");

    return GTK_WIDGET (gnc_gtk_drop_down_new (model, expression));
}

static void
budget_option_selection_changed_cb (GObject *object, GParamSpec *pspec,
                                    gpointer user_data)
{
    gnc_option_changed_widget_cb (GTK_WIDGET (object),
                                  static_cast<GncOption *> (user_data));
    (void)pspec;
}

class GncGtkBudgetUIItem : public GncOptionGtkUIItem
{
public:
    GncGtkBudgetUIItem(GtkWidget* widget) :
        GncOptionGtkUIItem{widget, GncOptionUIType::BUDGET} {}
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto widget{GTK_DROP_DOWN(get_widget())};
        auto instance{option.get_value<const QofInstance*>()};
        if (instance)
        {
            auto position = gnc_budget_list_model_get_position (
                gtk_drop_down_get_model (widget), GNC_BUDGET (instance));
            if (position != G_MAXUINT)
                gtk_drop_down_set_selected (widget, position);
        }
    }
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        auto widget{GTK_DROP_DOWN(get_widget())};
        auto position = gtk_drop_down_get_selected (widget);
        if (position != GTK_INVALID_LIST_POSITION)
        {
            auto item = GNC_BUDGET_LIST_ITEM (g_list_model_get_item (
                gtk_drop_down_get_model (widget), position));
            auto budget = gnc_budget_list_item_get_budget (item);
            g_object_unref (item);
            if (budget)
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

    g_signal_connect(G_OBJECT(widget), "notify::selected",
                     G_CALLBACK(budget_option_selection_changed_cb), &option);

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
