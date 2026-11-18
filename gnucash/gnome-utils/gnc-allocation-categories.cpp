/********************************************************************
 * gnc-allocation-categories.cpp -- native account and holding category editors
 * Copyright (C) 2026 Brent McBride (mcbridebt@hotmail.com)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <https://www.gnu.org/licenses/>.
 ********************************************************************/

#include <config.h>
#include <algorithm>
#include <charconv>
#include <cmath>
#include <memory>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "Account.h"
#include "Transaction.h"
#include "gnc-allocation-categories.hpp"
#include "gnc-commodity.h"
#include "gnc-option-impl.hpp"
#include "gnc-option-gtk-ui.hpp"
#include "gnc-pricedb.h"
#include "gnc-session.h"
#include "gnc-tree-view-account.h"

namespace
{
enum CategoryColumn
{
    CATEGORY_NAME,
    CATEGORY_POSITION,
    CATEGORY_TARGET,
    NUM_CATEGORY_COLUMNS
};

enum HoldingColumn
{
    HOLDING_NAME,
    HOLDING_SYMBOL,
    HOLDING_NAMESPACE,
    HOLDING_ID,
    NUM_HOLDING_COLUMNS
};

// GTK instance structure for the target-percentage cell renderer.
// The offscreen entry/window provide themed drawing when the cell is not being
// edited; the GtkCellRendererText base provides the interactive text editor.
struct GncAllocationTargetRenderer
{
    GtkCellRendererText parent;
    GtkWidget* entry;
    GtkWidget* window;
};

// Shared GTK class structure required by G_DEFINE_TYPE. It adds no class fields;
// class_init overrides inherited rendering, sizing, and finalization methods.
struct GncAllocationTargetRendererClass
{
    GtkCellRendererTextClass parent;
};

G_DEFINE_TYPE(GncAllocationTargetRenderer, gnc_allocation_target_renderer,
              GTK_TYPE_CELL_RENDERER_TEXT)

// Initialize one renderer's offscreen entry for drawing idle target cells.
// Its window owns the entry; the renderer releases the window in finalize.
static void
gnc_allocation_target_renderer_init(GncAllocationTargetRenderer* renderer)
{
    // Drawing an unrealized entry paints nothing. The offscreen parent realizes
    // it without adding a visible window; the renderer owns both widgets.
    renderer->window = gtk_offscreen_window_new();
    g_object_ref_sink(renderer->window);
    renderer->entry = gtk_entry_new();
    gtk_container_add(GTK_CONTAINER(renderer->window), renderer->entry);
    gtk_entry_set_width_chars(GTK_ENTRY(renderer->entry), 6);
    gtk_entry_set_alignment(GTK_ENTRY(renderer->entry), 1.0f);
    gtk_entry_set_placeholder_text(GTK_ENTRY(renderer->entry), _("Not set"));
    gtk_widget_show_all(renderer->window);
}

// Install entry-based drawing and size requests, plus widget cleanup.
// Text editing remains provided by the GtkCellRendererText parent class.
static void
gnc_allocation_target_renderer_class_init(GncAllocationTargetRendererClass* klass)
{
    G_OBJECT_CLASS(klass)->finalize = +[](GObject* object) {
        auto renderer = reinterpret_cast<GncAllocationTargetRenderer*>(object);
        gtk_widget_destroy(renderer->window);
        g_object_unref(renderer->window);
        G_OBJECT_CLASS(gnc_allocation_target_renderer_parent_class)->finalize(object);
    };
    auto cell_class = GTK_CELL_RENDERER_CLASS(klass);
    cell_class->get_preferred_width = +[](GtkCellRenderer* cell, GtkWidget*,
                                        gint* minimum, gint* natural) {
        auto renderer = reinterpret_cast<GncAllocationTargetRenderer*>(cell);
        gtk_widget_get_preferred_width(renderer->entry, minimum, natural);
        if (minimum)
            *minimum += 4;
        if (natural)
            *natural += 4;
    };
    cell_class->get_preferred_height = +[](GtkCellRenderer* cell, GtkWidget*,
                                         gint* minimum, gint* natural) {
        auto renderer = reinterpret_cast<GncAllocationTargetRenderer*>(cell);
        gtk_widget_get_preferred_height(renderer->entry, minimum, natural);
        if (minimum)
            *minimum += 4;
        if (natural)
            *natural += 4;
    };
    // Override both height requests: inherited text sizing would clip the
    // entry frame even when the ordinary preferred-height request is correct.
    cell_class->get_preferred_height_for_width = +[](GtkCellRenderer* cell, GtkWidget* widget,
                                                   gint, gint* minimum, gint* natural) {
        GTK_CELL_RENDERER_GET_CLASS(cell)->get_preferred_height(cell, widget, minimum, natural);
    };
    cell_class->render = +[](GtkCellRenderer* cell, cairo_t* context, GtkWidget* widget,
                            const GdkRectangle*, const GdkRectangle* area,
                            GtkCellRendererState) {
        auto renderer = reinterpret_cast<GncAllocationTargetRenderer*>(cell);
        gchar* text;
        g_object_get(cell, "text", &text, nullptr);
        gtk_entry_set_text(GTK_ENTRY(renderer->entry), text ? text : "");
        g_free(text);
        gtk_widget_set_sensitive(renderer->entry, gtk_widget_is_sensitive(widget));
        gtk_style_context_set_parent(gtk_widget_get_style_context(renderer->entry),
                                     gtk_widget_get_style_context(widget));
        GtkAllocation allocation{0, 0, std::max(1, area->width - 4),
                                      std::max(1, area->height - 4)};
        gtk_widget_size_allocate(renderer->entry, &allocation);
        cairo_save(context);
        cairo_rectangle(context, area->x, area->y, area->width, area->height);
        cairo_clip(context);
        cairo_translate(context, area->x + 2, area->y + 2);
        gtk_widget_draw(renderer->entry, context);
        cairo_restore(context);
    };
}

// Show the full label from model column 0 for the hovered or keyboard-focused
// row, so truncated account and holding names remain readable.
static void
set_allocation_tree_tooltips(GtkTreeView* view)
{
    gtk_widget_set_has_tooltip(GTK_WIDGET(view), TRUE);
    g_signal_connect(view, "query-tooltip",
        G_CALLBACK(+[](GtkWidget* widget, gint x, gint y, gboolean keyboard,
                       GtkTooltip* tooltip, gpointer) -> gboolean {
            auto view = GTK_TREE_VIEW(widget);
            GtkTreeModel* model;
            GtkTreePath* path;
            GtkTreeIter iter;
            if (!gtk_tree_view_get_tooltip_context(view, &x, &y, keyboard, &model, &path, &iter))
                return FALSE;
            gchar* text;
            gtk_tree_model_get(model, &iter, 0, &text, -1);
            gtk_tooltip_set_text(tooltip, text);
            gtk_tree_view_set_tooltip_row(view, tooltip, path);
            g_free(text);
            gtk_tree_path_free(path);
            return TRUE;
        }), nullptr);
}

// Append a holding beneath parent, with a readable name/symbol label and its
// namespace and unique name. The model copies the strings; commodity is borrowed.
static void
append_allocation_holding(GtkTreeStore* store, GtkTreeIter* parent, gnc_commodity* commodity)
{
    GtkTreeIter iter;
    gtk_tree_store_append(store, &iter, parent);
    auto name = gnc_commodity_get_fullname(commodity);
    auto symbol = gnc_commodity_get_mnemonic(commodity);
    auto label = name && *name && g_strcmp0(name, symbol) != 0
        ? g_strdup_printf("%s (%s)", name, symbol) : g_strdup(symbol);
    gtk_tree_store_set(store, &iter, HOLDING_NAME, label, HOLDING_SYMBOL, symbol,
        HOLDING_NAMESPACE, gnc_commodity_get_namespace(commodity),
        HOLDING_ID, gnc_commodity_get_unique_name(commodity), -1);
    g_free(label);
}

struct CategoryPane
{
    GtkWidget* m_box{};
    GtkListStore* m_store{};
    GtkTreeView* m_view{};
    GtkCellRenderer* m_name_renderer{};
    GtkCellRenderer* m_target_renderer{};
    GtkWidget* m_add{};
    GtkWidget* m_rename{};
    GtkWidget* m_remove{};
};

struct MemberPane
{
    GtkWidget* m_box{};
    GncTreeViewAccount* m_accounts{};
    GtkTreeView* m_securities{};
    GtkWidget* m_children{};
};

// Both editors share category editing and explicit membership storage.
// Account mode assigns account GUIDs; holding mode assigns commodity unique
// names across accounts. Both support optional category targets.
// m_values holds pending edits; the GTK models present those edits, and the
// option value is written separately by set_option_from_ui_item().
class GncGtkAccountCategoriesUIItem : public GncOptionGtkUIItem
{
    struct Category
    {
        std::string m_name;
        // Account GUIDs or security unique names, depending on the picker mode.
        std::vector<std::string> m_members;
        bool m_automatic_name{false};
        // Empty is unspecified, not a zero-percent target.
        std::string m_target{};
    };
    GncOption& m_option;
    GtkListStore* m_store;
    GtkTreeView* m_categories;
    GncTreeViewAccount* m_accounts;
    GtkTreeView* m_securities;
    GtkWidget* m_error;
    std::vector<Category> m_values;
    std::set<std::string> m_unavailable_members;
    int m_selected{-1};
    // Programmatic selection changes emit the same GTK signals as user edits.
    bool m_loading{false};
    bool m_valid{false};

    static std::string account_id(Account* account)
    {
        auto text = guid_to_string(xaccAccountGetGUID(account));
        std::string result{text};
        g_free(text);
        return result;
    }

    static Account* account_from_id(const std::string& id)
    {
        GncGUID guid;
        return string_to_guid(id.c_str(), &guid)
            ? xaccAccountLookup(&guid, gnc_get_current_book()) : nullptr;
    }

    GtkTreeView* member_view() const
    {
        return m_securities ? m_securities : GTK_TREE_VIEW(m_accounts);
    }

    // Return a holding row's stable commodity name; group rows and invalid
    // paths yield an empty string rather than a membership identifier.
    std::string security_id(GtkTreePath* path) const
    {
        auto model = gtk_tree_view_get_model(m_securities);
        GtkTreeIter iter;
        if (!gtk_tree_model_get_iter(model, &iter, path))
            return {};
        gchar* id;
        gtk_tree_model_get(model, &iter, HOLDING_ID, &id, -1);
        std::string result{id ? id : ""};
        g_free(id);
        return result;
    }

    // The picker normally omits unused currencies. Keep a saved assignment
    // visible even if that currency is no longer otherwise used by the book.
    void show_saved_currency(const std::string& id)
    {
        auto commodity = gnc_commodity_table_lookup_unique(
            gnc_commodity_table_get_table(gnc_get_current_book()), id.c_str());
        if (!commodity || !gnc_commodity_is_currency(commodity))
            return;
        auto model = gtk_tree_view_get_model(m_securities);
        GtkTreeIter group;
        for (auto valid = gtk_tree_model_get_iter_first(model, &group); valid;
             valid = gtk_tree_model_iter_next(model, &group))
        {
            gchar* name;
            gtk_tree_model_get(model, &group, HOLDING_NAME, &name, -1);
            bool currencies = g_strcmp0(name, _("Currencies")) == 0;
            g_free(name);
            if (!currencies)
                continue;
            GtkTreeIter member;
            for (auto child = gtk_tree_model_iter_children(model, &member, &group); child;
                 child = gtk_tree_model_iter_next(model, &member))
            {
                gchar* existing;
                gtk_tree_model_get(model, &member, HOLDING_ID, &existing, -1);
                bool found = id == existing;
                g_free(existing);
                if (found)
                    return;
            }
            append_allocation_holding(GTK_TREE_STORE(model), &group, commodity);
            return;
        }
    }

    // Exclude other categories' assignments, but keep the current category's
    // members selectable so users can remove or retain them.
    void update_unavailable_members()
    {
        m_unavailable_members.clear();
        for (size_t index = 0; index < m_values.size(); ++index)
        {
            if (index == static_cast<size_t>(m_selected))
                continue;
            const auto& members = m_values[index].m_members;
            m_unavailable_members.insert(members.begin(), members.end());
        }
    }

    bool member_available(const std::string& id) const
    {
        // Cell rendering calls this for every column; don't rescan all categories.
        return m_loading || !m_unavailable_members.contains(id);
    }

    bool account_available(Account* account) const
    {
        return account && member_available(account_id(account));
    }

    // Accept an unspecified target or a complete finite number in [0, 100].
    // Empty input succeeds without modifying value; invalid input may modify it.
    static bool target_value(const std::string& text, double& value)
    {
        if (text.empty())
            return true;
        char* end = nullptr;
        value = g_strtod(text.c_str(), &end);
        return end != text.c_str() && *end == '\0' &&
            std::isfinite(value) && value >= 0 && value <= 100;
    }

    // Return the first error that prevents committing the definitions. Target
    // completeness and the combined target total are checked separately below.
    std::string validation_error() const
    {
        std::set<std::string> names;
        std::set<std::string> members;
        // No security assignments means all selected holdings are Uncategorized.
        if (m_values.empty() && !m_securities)
            return _("Create at least one category.");
        for (const auto& category : m_values)
        {
            double target = 0;
            if (!target_value(category.m_target, target))
                return _("Enter a target percentage from 0 to 100, or leave it blank.");
            if (std::all_of(category.m_name.begin(), category.m_name.end(),
                            [](unsigned char character) { return g_ascii_isspace(character); }) ||
                !names.insert(category.m_name).second)
                return _("Category names must be nonempty and unique.");
            for (const auto& id : category.m_members)
            {
                auto security = m_securities ? gnc_commodity_table_lookup_unique(
                    gnc_commodity_table_get_table(gnc_get_current_book()), id.c_str()) : nullptr;
                if (m_securities ? !security : !account_from_id(id))
                    return m_securities ? _("A selected holding no longer exists.")
                                        : _("A selected account no longer exists.");
                if (!members.insert(id).second)
                    return m_securities
                        ? _("Each holding may belong to only one category and may be selected only once.")
                        : _("Each account may belong to only one category and may be selected only once.");
            }
        }
        return {};
    }

    // Update commit validity and the displayed error or advisory warning.
    // Missing targets and totals other than 100% do not invalidate categories.
    void validate()
    {
        auto error = validation_error();
        // Set validity before adding advisory text: an incomplete target plan
        // must not prevent saving otherwise usable allocation categories.
        m_valid = error.empty();
        if (m_valid)
        {
            double total = 0;
            size_t specified = 0;
            for (const auto& category : m_values)
            {
                if (category.m_target.empty())
                    continue;
                double target = 0;
                target_value(category.m_target, target);
                total += target;
                ++specified;
            }
            if (specified && specified != m_values.size())
                error = _("Some categories have no target weight. "
                          "Enter a target for each category; use 0% where intended. "
                          "Target weights are only used when measuring drift from actual allocation.");
            else if (specified && std::abs(total - 100) > 0.000001)
            {
                auto warning = g_strdup_printf(
                    _("Targets total %.*f%%. Adjust target weights to total 100%%. "
                      "Target weights are only used when measuring drift from actual allocation."),
                    std::abs(total - 100) < 0.005 ? 6 : 2, total);
                error = warning;
                g_free(warning);
            }
        }
        gtk_label_set_text(GTK_LABEL(m_error), error.c_str());
        gtk_widget_set_visible(m_error, !error.empty());
    }

    // Keep the member column's title, visible label, and tooltip aligned with
    // the selected category, or with the new-category prompt if none is selected.
    void update_members_heading()
    {
        auto heading = m_selected >= 0
            ? g_strdup_printf(m_securities ? _("Holdings in \"%s\"") : _("Accounts in \"%s\""),
                              m_values.at(m_selected).m_name.c_str())
            : g_strdup(m_securities ? _("Holdings for a new category") : _("Accounts for a new category"));
        auto column = m_securities ? gtk_tree_view_get_column(m_securities, 0)
            : gnc_tree_view_find_column_by_name(GNC_TREE_VIEW(m_accounts), "name");
        gtk_tree_view_column_set_title(column, heading);
        auto label = gtk_tree_view_column_get_widget(column);
        gtk_label_set_text(GTK_LABEL(label), heading);
        gtk_widget_set_tooltip_text(label, heading);
        g_free(heading);
    }

    // Refresh membership restrictions and validation after a pending edit,
    // then notify the options dialog without committing the option value.
    void changed()
    {
        update_unavailable_members();
        validate();
        update_members_heading();
        gnc_option_changed_widget_cb(get_widget(), &m_option);
    }

    // Project the chosen category's stored members into the right-hand picker.
    // Suppress selection callbacks while doing so: changing the selected
    // category must not overwrite memberships or mark the option as edited.
    void select_category()
    {
        if (m_loading)
            return;
        GtkTreeIter iter;
        GtkTreeModel* model;
        auto selection = gtk_tree_view_get_selection(m_categories);
        m_selected = -1;
        if (gtk_tree_selection_get_selected(selection, &model, &iter))
        {
            auto path = gtk_tree_model_get_path(model, &iter);
            m_selected = gtk_tree_path_get_indices(path)[0];
            gtk_tree_path_free(path);
        }
        update_unavailable_members();
        m_loading = true;
        if (m_securities)
        {
            auto selected = gtk_tree_view_get_selection(m_securities);
            gtk_tree_selection_unselect_all(selected);
            auto model = gtk_tree_view_get_model(m_securities);
            gtk_tree_model_foreach(model,
                +[](GtkTreeModel*, GtkTreePath* path, GtkTreeIter* iter, gpointer data) -> gboolean {
                    auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                    if (self->m_selected < 0)
                        return FALSE;
                    auto id = self->security_id(path);
                    const auto& members = self->m_values.at(self->m_selected).m_members;
                    if (!id.empty() && std::find(members.begin(), members.end(), id) != members.end())
                    {
                        gtk_tree_view_expand_to_path(self->m_securities, path);
                        gtk_tree_selection_select_iter(gtk_tree_view_get_selection(self->m_securities), iter);
                    }
                    return FALSE;
                }, this);
        }
        else
        {
            GList* accounts = nullptr;
            if (m_selected >= 0)
                for (const auto& id : m_values.at(m_selected).m_members)
                    if (auto account = account_from_id(id))
                        accounts = g_list_prepend(accounts, account);
            gnc_tree_view_account_set_selected_accounts(m_accounts, accounts, TRUE);
            g_list_free(accounts);
        }
        m_loading = false;
        update_members_heading();
        gtk_widget_queue_draw(GTK_WIDGET(member_view()));
    }

    // Copy user selections back into the pending category definitions. A first
    // selection can create a category; an automatically named empty category
    // adopts its first member's name only until explicitly named by the user.
    void select_members()
    {
        if (m_loading)
            return;
        std::vector<std::string> selected;
        std::string name;
        if (m_securities)
        {
            bool grouping_selected = false;
            auto model = gtk_tree_view_get_model(m_securities);
            auto paths = gtk_tree_selection_get_selected_rows(gtk_tree_view_get_selection(m_securities), nullptr);
            for (auto node = paths; node; node = node->next)
            {
                auto path = static_cast<GtkTreePath*>(node->data);
                auto id = security_id(path);
                if (id.empty())
                    grouping_selected = true;
                else
                    selected.push_back(std::move(id));
                if (name.empty() && !selected.empty())
                {
                    GtkTreeIter iter;
                    gtk_tree_model_get_iter(model, &iter, path);
                    gchar* text;
                    gtk_tree_model_get(model, &iter, HOLDING_NAME, &text, -1);
                    name = text;
                    g_free(text);
                }
                gtk_tree_path_free(path);
            }
            g_list_free(paths);
            // Namespace rows organize the picker; selecting only a heading
            // must not clear the current category's actual holding assignments.
            if (selected.empty() && grouping_selected)
                return;
        }
        else
        {
            auto accounts = gnc_tree_view_account_get_selected_accounts(m_accounts);
            if (accounts)
                name = xaccAccountGetName(static_cast<Account*>(accounts->data));
            for (auto node = accounts; node; node = node->next)
                selected.push_back(account_id(static_cast<Account*>(node->data)));
            g_list_free(accounts);
        }
        if (m_selected < 0)
        {
            if (!selected.empty())
                add_category(name.empty() ? _("New Category") : name, std::move(selected));
            return;
        }
        auto& category = m_values.at(m_selected);
        if (category.m_automatic_name && !selected.empty())
        {
            category.m_name = unique_name(name.empty() ? _("New Category") : name, &category);
            category.m_automatic_name = false;
            GtkTreeIter iter;
            if (gtk_tree_model_iter_nth_child(GTK_TREE_MODEL(m_store), &iter, nullptr, m_selected))
                gtk_list_store_set(m_store, &iter, CATEGORY_NAME, category.m_name.c_str(), -1);
        }
        category.m_members = std::move(selected);
        gtk_widget_queue_draw(GTK_WIDGET(member_view()));
        changed();
    }

    // Expand the current selection explicitly, rather than saving a rule that
    // would silently include future descendants. Batch holding selections and
    // publish one membership update after all eligible descendants are selected.
    void select_children()
    {
        if (m_loading)
            return;
        if (m_securities)
        {
            auto selection = gtk_tree_view_get_selection(m_securities);
            auto paths = gtk_tree_selection_get_selected_rows(selection, nullptr);
            struct Selection { GncGtkAccountCategoriesUIItem* m_self; GList* m_paths; } data{this, paths};
            m_loading = true;
            gtk_tree_model_foreach(gtk_tree_view_get_model(m_securities),
                +[](GtkTreeModel*, GtkTreePath* path, GtkTreeIter* iter, gpointer context) -> gboolean {
                    auto data = static_cast<Selection*>(context);
                    auto self = data->m_self;
                    auto selection = gtk_tree_view_get_selection(self->m_securities);
                    auto id = self->security_id(path);
                    if (id.empty())
                    {
                        gtk_tree_selection_unselect_iter(selection, iter);
                        return FALSE;
                    }
                    if (self->m_unavailable_members.contains(id))
                        return FALSE;
                    bool include = false;
                    if (self->m_selected >= 0)
                    {
                        const auto& members = self->m_values.at(self->m_selected).m_members;
                        include = std::find(members.begin(), members.end(), id) != members.end();
                    }
                    for (auto node = data->m_paths; node && !include; node = node->next)
                        include = gtk_tree_path_is_descendant(path, static_cast<GtkTreePath*>(node->data));
                    if (include)
                    {
                        gtk_tree_view_expand_to_path(self->m_securities, path);
                        gtk_tree_selection_select_iter(selection, iter);
                    }
                    return FALSE;
                }, &data);
            g_list_free_full(paths, reinterpret_cast<GDestroyNotify>(gtk_tree_path_free));
            m_loading = false;
            select_members();
            return;
        }
        auto accounts = gnc_tree_view_account_get_selected_accounts(m_accounts);
        for (auto node = accounts; node; node = node->next)
            gnc_tree_view_account_select_subaccounts(m_accounts, static_cast<Account*>(node->data));
        g_list_free(accounts);
    }

    // Add a numeric suffix only when base conflicts with another category.
    // excluded lets an existing category retain its own name during renaming.
    std::string unique_name(const std::string& base, const Category* excluded = nullptr) const
    {
        auto name = base;
        unsigned int suffix = 2;
        while (std::any_of(m_values.begin(), m_values.end(),
                          [&name, excluded](const auto& category) {
                              return &category != excluded && category.m_name == name;
                          }))
            name = base + " " + std::to_string(suffix++);
        return name;
    }

    // FALSE commits rather than cancels the active cell editor. Commit before
    // changing rows so pending text reaches m_values and name uniqueness checks.
    void commit_category_edit()
    {
        for (guint index = 0; index < gtk_tree_view_get_n_columns(m_categories); ++index)
            gtk_cell_area_stop_editing(gtk_cell_layout_get_area(GTK_CELL_LAYOUT(
                gtk_tree_view_get_column(m_categories, index))), FALSE);
    }

    // Commit active text, append and select a uniquely named category, and
    // notify the dialog. An empty category may adopt its first member's name.
    void add_category(std::string name = _("New Category"), std::vector<std::string> members = {})
    {
        commit_category_edit();
        name = unique_name(name);
        auto automatic_name = members.empty();
        m_values.push_back({name, std::move(members), automatic_name});
        GtkTreeIter iter;
        gtk_list_store_append(m_store, &iter);
        gtk_list_store_set(m_store, &iter, CATEGORY_NAME, name.c_str(),
                   CATEGORY_POSITION, static_cast<int>(m_values.size() - 1), -1);
        gtk_tree_selection_select_iter(gtk_tree_view_get_selection(m_categories), &iter);
        changed();
    }

    // Remove the selected definition, not its accounts or commodities, and
    // refresh selection and validation. No selection leaves the data unchanged.
    void remove_category()
    {
        if (m_selected < 0)
            return;
        GtkTreeIter iter;
        auto selection = gtk_tree_view_get_selection(m_categories);
        if (!gtk_tree_selection_get_selected(selection, nullptr, &iter))
            return;
        m_loading = true;
        m_values.erase(m_values.begin() + m_selected);
        gtk_list_store_remove(m_store, &iter);
        update_category_positions();
        m_selected = -1;
        m_loading = false;
        if (gtk_tree_model_get_iter_first(GTK_TREE_MODEL(m_store), &iter))
            gtk_tree_selection_select_iter(selection, &iter);
        select_category();
        changed();
    }

    // Reset each row's hidden identity column to its current m_values index
    // after deletion or a completed reorder.
    void update_category_positions()
    {
        GtkTreeIter iter;
        auto model = GTK_TREE_MODEL(m_store);
        int position = 0;
        for (auto valid = gtk_tree_model_get_iter_first(model, &iter); valid;
             valid = gtk_tree_model_iter_next(model, &iter))
            gtk_list_store_set(m_store, &iter, CATEGORY_POSITION, position++, -1);
    }

    // Reconcile m_values with the completed GTK row order, preserving the
    // selected category and notifying the dialog only when an item moved.
    void reorder_categories()
    {
        // GTK DnD temporarily copies/inserts/deletes rows. Rebuild only after
        // it finishes, using the hidden original-position column as identity.
        m_loading = true;
        std::vector<Category> reordered;
        reordered.reserve(m_values.size());
        auto selected = m_selected;
        bool moved = false;
        GtkTreeIter iter;
        auto model = GTK_TREE_MODEL(m_store);
        for (auto valid = gtk_tree_model_get_iter_first(model, &iter); valid;
             valid = gtk_tree_model_iter_next(model, &iter))
        {
            int previous;
            gtk_tree_model_get(model, &iter, CATEGORY_POSITION, &previous, -1);
            auto position = static_cast<int>(reordered.size());
            moved = moved || previous != position;
            if (previous == selected)
                m_selected = position;
            reordered.push_back(std::move(m_values.at(previous)));
        }
        m_values = std::move(reordered);
        update_category_positions();
        if (m_selected >= 0)
        {
            auto path = gtk_tree_path_new_from_indices(m_selected, -1);
            gtk_tree_view_set_cursor(m_categories, path, nullptr, FALSE);
            gtk_tree_view_scroll_to_cell(m_categories, path, nullptr, FALSE, 0, 0);
            gtk_tree_path_free(path);
        }
        m_loading = false;
        select_category();
        if (moved)
            changed();
    }

    // Move the selected category by the signed row offset and reconcile the
    // stored order. Ignore moves outside the list or during programmatic loading.
    void move_category(int direction)
    {
        if (m_loading || m_selected < 0)
            return;
        auto target = m_selected + direction;
        if (target < 0 || static_cast<size_t>(target) >= m_values.size())
            return;
        GtkTreeIter current, destination;
        auto model = GTK_TREE_MODEL(m_store);
        gtk_tree_model_iter_nth_child(model, &current, nullptr, m_selected);
        gtk_tree_model_iter_nth_child(model, &destination, nullptr, target);
        m_loading = true;
        gtk_list_store_swap(m_store, &current, &destination);
        reorder_categories();
    }

    // Enforce one-category-per-member both interactively and visually. Loaded
    // invalid selections remain removable instead of trapping the user.
    void configure_member_selection(GncTreeViewAccount* accounts, GtkTreeView* securities)
    {
        gtk_tree_selection_set_select_function(
            gtk_tree_view_get_selection(member_view()),
            +[](GtkTreeSelection*, GtkTreeModel*, GtkTreePath* path,
                gboolean selected, gpointer data) -> gboolean {
                auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                // Selected rows must remain deselectable, even for invalid data.
                return selected || (self->m_securities ? self->member_available(self->security_id(path))
                    : self->account_available(gnc_tree_view_account_get_account_from_path(self->m_accounts, path)));
            }, this, nullptr);
        if (accounts)
            gnc_tree_view_account_set_sensitive_func(accounts,
            +[](Account* account, gpointer data) -> gboolean {
                return static_cast<GncGtkAccountCategoriesUIItem*>(data)->account_available(account);
            }, this);
        if (securities)
        {
            auto columns = gtk_tree_view_get_columns(securities);
            for (auto node = columns; node; node = node->next)
            {
                auto column = GTK_TREE_VIEW_COLUMN(node->data);
                auto cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(column));
                for (auto cell = cells; cell; cell = cell->next)
                    gtk_tree_view_column_set_cell_data_func(column, GTK_CELL_RENDERER(cell->data),
                        +[](GtkTreeViewColumn*, GtkCellRenderer* renderer, GtkTreeModel* model,
                            GtkTreeIter* iter, gpointer data) {
                            auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                            gchar* id;
                            gtk_tree_model_get(model, iter, HOLDING_ID, &id, -1);
                            g_object_set(renderer, "sensitive", self->member_available(id ? id : ""), nullptr);
                            g_free(id);
                        }, this, nullptr);
                g_list_free(cells);
            }
            g_list_free(columns);
        }
    }

    // Connect category/member selection, drag ordering, and keyboard handlers.
    // Drag operations defer membership updates until the final row order exists.
    void connect_category_signals(GtkTreeView* categories)
    {
        g_signal_connect(categories, "button-press-event",
            G_CALLBACK(+[](GtkWidget* widget, GdkEventButton* event, gpointer) -> gboolean {
                auto view = GTK_TREE_VIEW(widget);
                if (event->type != GDK_BUTTON_PRESS || event->button != GDK_BUTTON_PRIMARY ||
                    event->window != gtk_tree_view_get_bin_window(view))
                    return FALSE;
                GtkTreePath* path = nullptr;
                GtkTreeViewColumn* column = nullptr;
                if (!gtk_tree_view_get_path_at_pos(view, event->x, event->y, &path, &column, nullptr, nullptr))
                    return FALSE;
                auto target = column == gtk_tree_view_get_column(view, 1);
                if (target)
                {
                    gtk_widget_grab_focus(widget);
                    gtk_tree_view_set_cursor(view, path, column, TRUE);
                }
                gtk_tree_path_free(path);
                return target;
            }), nullptr);
        // Tab arriving from outside starts editing at the appropriate edge.
        // Leave focus moves within the tree and non-Tab navigation to GTK.
        g_signal_connect(categories, "focus",
            G_CALLBACK(+[](GtkWidget* widget, GtkDirectionType direction, gpointer data) -> gboolean {
                if (direction != GTK_DIR_TAB_FORWARD && direction != GTK_DIR_TAB_BACKWARD)
                    return FALSE;
                auto window = gtk_widget_get_toplevel(widget);
                if (!GTK_IS_WINDOW(window))
                    return FALSE;
                auto focus = gtk_window_get_focus(GTK_WINDOW(window));
                if (!focus || focus == widget || gtk_widget_is_ancestor(focus, widget))
                    return FALSE;
                auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                auto rows = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(self->m_store), nullptr);
                if (!rows)
                    return FALSE;
                auto backwards = direction == GTK_DIR_TAB_BACKWARD;
                auto path = gtk_tree_path_new_from_indices(backwards ? rows - 1 : 0, -1);
                auto view = GTK_TREE_VIEW(widget);
                auto column = backwards ? gtk_tree_view_get_n_columns(view) - 1 : 0;
                gtk_widget_grab_focus(widget);
                gtk_tree_view_set_cursor(view, path, gtk_tree_view_get_column(view, column), TRUE);
                gtk_tree_path_free(path);
                g_signal_stop_emission_by_name(widget, "focus");
                return TRUE;
            }), this);
        g_signal_connect(gtk_tree_view_get_selection(categories), "changed",
            G_CALLBACK(+[](GtkTreeSelection*, gpointer data) {
                static_cast<GncGtkAccountCategoriesUIItem*>(data)->select_category();
            }), this);
        g_signal_connect(categories, "drag-begin",
            G_CALLBACK(+[](GtkWidget*, GdkDragContext*, gpointer data) {
                static_cast<GncGtkAccountCategoriesUIItem*>(data)->m_loading = true;
            }), this);
        g_signal_connect(categories, "drag-end",
            G_CALLBACK(+[](GtkWidget*, GdkDragContext*, gpointer data) {
                static_cast<GncGtkAccountCategoriesUIItem*>(data)->reorder_categories();
            }), this);
        g_signal_connect(categories, "key-press-event",
            G_CALLBACK(+[](GtkWidget*, GdkEventKey* event, gpointer data) -> gboolean {
                if ((event->state & gtk_accelerator_get_default_mod_mask()) != GDK_MOD1_MASK)
                    return FALSE;
                auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                if (event->keyval == GDK_KEY_Up || event->keyval == GDK_KEY_KP_Up)
                    self->move_category(-1);
                else if (event->keyval == GDK_KEY_Down || event->keyval == GDK_KEY_KP_Down)
                    self->move_category(1);
                else
                    return FALSE;
                return TRUE;
            }), this);
        g_signal_connect(gtk_tree_view_get_selection(member_view()), "changed",
            G_CALLBACK(+[](GtkTreeSelection*, gpointer data) {
                static_cast<GncGtkAccountCategoriesUIItem*>(data)->select_members();
            }), this);
    }

    // Finish the active editor before focus transfer cancels its pending text.
    // Keep the editor alive until GTK has finished updating its focus pointer.
    static void finish_editor_on_focus_change(GtkWindow* window, GtkWidget* focus, GtkCellEditable* editor)
    {
        if (gtk_window_get_focus(window) != GTK_WIDGET(editor) || focus == GTK_WIDGET(editor))
            return;
        g_signal_handlers_disconnect_by_func(window,
            reinterpret_cast<gpointer>(finish_editor_on_focus_change), editor);
        g_idle_add_full(G_PRIORITY_DEFAULT_IDLE,
            +[](gpointer) -> gboolean { return G_SOURCE_REMOVE; },
            g_object_ref(editor), g_object_unref);
        gtk_cell_editable_editing_done(editor);
        gtk_cell_editable_remove_widget(editor);
    }

    // Let GTK find an external control without this tree reclaiming focus.
    void focus_outside_categories(bool backwards)
    {
        auto skip_tree = g_signal_connect(m_categories, "focus",
            G_CALLBACK(+[](GtkWidget* widget, GtkDirectionType, gpointer) -> gboolean {
                g_signal_stop_emission_by_name(widget, "focus");
                return FALSE;
            }), nullptr);
        gtk_widget_child_focus(gtk_widget_get_toplevel(GTK_WIDGET(m_categories)),
            backwards ? GTK_DIR_TAB_BACKWARD : GTK_DIR_TAB_FORWARD);
        g_signal_handler_disconnect(m_categories, skip_tree);
    }

    // Commit plain Tab/Shift+Tab and traverse cells in row-major order,
    // returning focus to dialog controls at the first and last cells.
    bool navigate_editor(GdkEventKey* event)
    {
        if (event->keyval != GDK_KEY_Tab && event->keyval != GDK_KEY_KP_Tab &&
            event->keyval != GDK_KEY_ISO_Left_Tab)
            return false;
        auto modifiers = event->state & gtk_accelerator_get_default_mod_mask();
        if (modifiers & ~GDK_SHIFT_MASK)
            return false;
        GtkTreePath* path = nullptr;
        GtkTreeViewColumn* column = nullptr;
        gtk_tree_view_get_cursor(m_categories, &path, &column);
        if (!path || !column)
        {
            if (path)
                gtk_tree_path_free(path);
            return false;
        }
        auto columns = gtk_tree_view_get_n_columns(m_categories);
        int column_index = 0;
        while (gtk_tree_view_get_column(m_categories, column_index) != column)
            ++column_index;
        auto backwards = (modifiers & GDK_SHIFT_MASK) || event->keyval == GDK_KEY_ISO_Left_Tab;
        auto next = gtk_tree_path_get_indices(path)[0] * columns + column_index + (backwards ? -1 : 1);
        gtk_tree_path_free(path);
        gtk_cell_area_stop_editing(gtk_cell_layout_get_area(GTK_CELL_LAYOUT(column)), FALSE);
        gtk_widget_grab_focus(GTK_WIDGET(m_categories));
        auto rows = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(m_store), nullptr);
        if (next < 0 || next >= rows * columns)
            focus_outside_categories(backwards);
        else
        {
            path = gtk_tree_path_new_from_indices(next / columns, -1);
            gtk_tree_view_set_cursor(m_categories, path,
                gtk_tree_view_get_column(m_categories, next % columns), TRUE);
            gtk_tree_path_free(path);
        }
        return true;
    }

    // Connect each live cell editor to focus preservation and Tab navigation.
    void connect_editor_navigation(GtkCellRenderer* renderer)
    {
        g_signal_connect(renderer, "editing-started",
            G_CALLBACK(+[](GtkCellRenderer*, GtkCellEditable* editable, gchar*, gpointer data) {
                auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                auto window = gtk_widget_get_toplevel(self->get_widget());
                if (GTK_IS_WINDOW(window))
                    g_signal_connect_object(window, "set-focus",
                        G_CALLBACK(finish_editor_on_focus_change), editable, G_CONNECT_DEFAULT);
                g_signal_connect(editable, "key-press-event",
                    G_CALLBACK(+[](GtkWidget*, GdkEventKey* event, gpointer data) -> gboolean {
                        return static_cast<GncGtkAccountCategoriesUIItem*>(data)->navigate_editor(event);
                    }), data);
            }), this);
    }

    // Connect cell navigation and name edits. Trim surrounding whitespace,
    // disable automatic naming, and leave duplicate-name reporting to validation.
    void connect_name_editor(GtkCellRenderer* renderer)
    {
        connect_editor_navigation(renderer);
        g_signal_connect(renderer, "edited",
            G_CALLBACK(+[](GtkCellRendererText*, gchar* path_string, gchar* text, gpointer data) {
                auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                if (self->m_loading)
                    return;
                auto path = gtk_tree_path_new_from_string(path_string);
                auto index = gtk_tree_path_get_indices(path)[0];
                auto name = g_strdup(text);
                g_strstrip(name);
                self->m_values.at(index).m_name = name;
                self->m_values.at(index).m_automatic_name = false;
                GtkTreeIter iter;
                gtk_tree_model_get_iter(GTK_TREE_MODEL(self->m_store), &iter, path);
                gtk_list_store_set(self->m_store, &iter, CATEGORY_NAME, name, -1);
                gtk_tree_path_free(path);
                g_free(name);
                self->changed();
            }), this);
    }

    // Format valid targets with a percent suffix without changing the model;
    // keep unspecified and malformed values visible as entered.
    void configure_target_display(GtkCellRenderer* target_renderer)
    {
        gtk_tree_view_column_set_cell_data_func(
            gtk_tree_view_get_column(m_categories, 1), target_renderer,
            +[](GtkTreeViewColumn*, GtkCellRenderer* renderer, GtkTreeModel* model,
                GtkTreeIter* iter, gpointer) {
                gchar* text;
                gtk_tree_model_get(model, iter, CATEGORY_TARGET, &text, -1);
                std::string display{text ? text : ""};
                double value = 0;
                if (!display.empty() && target_value(display, value))
                    display += '%';
                g_object_set(renderer, "text", display.c_str(), nullptr);
                g_free(text);
            }, nullptr, nullptr);
    }

    // Initialize each live editor from the raw target rather than its formatted
    // display, and apply numeric-entry hints and the unspecified placeholder.
    void connect_target_entry(GtkCellRenderer* target_renderer)
    {
        g_signal_connect(target_renderer, "editing-started",
            G_CALLBACK(+[](GtkCellRenderer*, GtkCellEditable* editable, gchar* path,
                           gpointer data) {
                if (GTK_IS_ENTRY(editable))
                {
                    auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                    auto model = GTK_TREE_MODEL(self->m_store);
                    GtkTreeIter iter;
                    if (gtk_tree_model_get_iter_from_string(model, &iter, path))
                    {
                        gchar* text;
                        gtk_tree_model_get(model, &iter, CATEGORY_TARGET, &text, -1);
                        gtk_entry_set_text(GTK_ENTRY(editable), text ? text : "");
                        g_free(text);
                    }
                    gtk_entry_set_input_purpose(GTK_ENTRY(editable), GTK_INPUT_PURPOSE_NUMBER);
                    gtk_entry_set_has_frame(GTK_ENTRY(editable), TRUE);
                    gtk_entry_set_alignment(GTK_ENTRY(editable), 1.0f);
                    gtk_entry_set_placeholder_text(GTK_ENTRY(editable), _("Not set"));
                }
            }), this);
    }

    // Connect target presentation, navigation, and pending-value updates.
    // A null renderer is expected in account mode and requires no connections.
    void connect_target_editor(GtkCellRenderer* target_renderer)
    {
        if (target_renderer)
        {
            connect_editor_navigation(target_renderer);
            configure_target_display(target_renderer);
            connect_target_entry(target_renderer);
            g_signal_connect(target_renderer, "edited",
                G_CALLBACK(+[](GtkCellRendererText*, gchar* path_string, gchar* text, gpointer data) {
                    auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
                    if (self->m_loading)
                        return;
                    auto path = gtk_tree_path_new_from_string(path_string);
                    auto index = gtk_tree_path_get_indices(path)[0];
                    auto target = g_strdup(text);
                    g_strstrip(target);
                    self->m_values.at(index).m_target = target;
                    GtkTreeIter iter;
                    gtk_tree_model_get_iter(GTK_TREE_MODEL(self->m_store), &iter, path);
                    gtk_list_store_set(self->m_store, &iter, CATEGORY_TARGET, target, -1);
                    gtk_tree_path_free(path);
                    g_free(target);
                    self->changed();
                }), this);
        }
    }

    // Connect category commands and the optional descendant-selection button.
    // Preserve active cell text when Add takes focus before its click handler.
    void connect_category_buttons(GtkWidget* add, GtkWidget* rename, GtkWidget* remove, GtkWidget* children)
    {
        // A mouse click transfers focus before emitting "clicked". Commit here
        // before GTK can cancel the editor; committing in add_category is too late
        // for that focus-transfer path, but still covers other activation paths.
        g_signal_connect(add, "grab-focus", G_CALLBACK(+[](GtkWidget*, gpointer data) {
            static_cast<GncGtkAccountCategoriesUIItem*>(data)->commit_category_edit();
        }), this);
        g_signal_connect(add, "clicked", G_CALLBACK(+[](GtkButton*, gpointer data) {
            static_cast<GncGtkAccountCategoriesUIItem*>(data)->add_category();
        }), this);
        if (children)
            g_signal_connect(children, "clicked", G_CALLBACK(+[](GtkButton*, gpointer data) {
                static_cast<GncGtkAccountCategoriesUIItem*>(data)->select_children();
            }), this);
        g_signal_connect(remove, "clicked", G_CALLBACK(+[](GtkButton*, gpointer data) {
            static_cast<GncGtkAccountCategoriesUIItem*>(data)->remove_category();
        }), this);
        g_signal_connect(rename, "clicked", G_CALLBACK(+[](GtkButton*, gpointer data) {
            auto self = static_cast<GncGtkAccountCategoriesUIItem*>(data);
            if (self->m_selected < 0)
                return;
            auto path = gtk_tree_path_new_from_indices(self->m_selected, -1);
            gtk_tree_view_set_cursor(self->m_categories, path,
                                     gtk_tree_view_get_column(self->m_categories, 0), TRUE);
            gtk_tree_path_free(path);
        }), this);
    }

    // Consume the target field only for explicit-targets payloads. Scheme #f
    // means unspecified; preserve malformed values as text for validation.
    SCM load_category_target(Category& category, SCM members, bool targets)
    {
        if (!targets || !scm_is_pair(members))
            return members;
        auto target = scm_car(members);
        if (!scm_is_false(target))
        {
            if (scm_is_real(target))
            {
                char buffer[G_ASCII_DTOSTR_BUF_SIZE];
                auto value = scm_to_double(target);
                // Shortest round-trip text avoids exposing binary-double noise
                // such as 56.299999999999997 without rounding the stored value.
                auto result = std::to_chars(buffer, buffer + sizeof buffer, value);
                category.m_target = result.ec == std::errc{}
                    ? std::string(buffer, result.ptr)
                    : g_ascii_dtostr(buffer, sizeof buffer, value);
            }
            else
            {
                auto text = scm_to_utf8_string(scm_object_to_string(target, SCM_UNDEFINED));
                category.m_target = text;
                free(text);
            }
        }
        return scm_cdr(members);
    }

    // Legacy account groups included descendants implicitly. Expand those once
    // on load; explicit payloads must retain exactly their saved membership.
    void load_category_members(Category& category, SCM members, bool explicit_members)
    {
        auto append_member = [&category](const std::string& id) {
            if (std::find(category.m_members.begin(), category.m_members.end(), id) == category.m_members.end())
                category.m_members.push_back(id);
        };
        for (auto accounts = members; scm_is_pair(accounts); accounts = scm_cdr(accounts))
        {
            if (!scm_is_string(scm_car(accounts)))
                continue;
            auto guid_string = scm_to_utf8_string(scm_car(accounts));
            GncGUID guid;
            if (m_securities)
            {
                append_member(guid_string);
                show_saved_currency(guid_string);
            }
            else if (string_to_guid(guid_string, &guid))
            {
                append_member(guid_string);
                if (!explicit_members)
                    if (auto account = xaccAccountLookup(&guid, gnc_get_current_book()))
                    {
                        auto descendants = gnc_account_get_descendants_sorted(account);
                        for (auto node = descendants; node; node = node->next)
                            append_member(account_id(static_cast<Account*>(node->data)));
                        g_list_free(descendants);
                    }
            }
            free(guid_string);
        }
    }

public:
    bool is_valid() const noexcept override { return m_valid; }

    // Clear pending definitions and both panes' selections, then revalidate and
    // mark the option edited. The saved option is unchanged until commit.
    void clear_values()
    {
        m_loading = true;
        m_selected = -1;
        m_values.clear();
        gtk_list_store_clear(m_store);
        m_loading = false;
        select_category();
        changed();
    }

    // Suppress selection updates during teardown, disconnect tree callbacks,
    // and destroy the widget hierarchy while this UI item still exists.
    ~GncGtkAccountCategoriesUIItem() override
    {
        m_loading = true;
        g_signal_handlers_disconnect_by_data(m_categories, this);
        if (get_widget())
            gtk_widget_destroy(get_widget());
    }

    // Bind the assembled panes to option and install handlers before loading
    // its value. Widget pointers remain owned by the GTK hierarchy.
    GncGtkAccountCategoriesUIItem(GncOption& option, GtkWidget* box,
                                 const CategoryPane& categories,
                                 const MemberPane& members, GtkWidget* error) :
        GncOptionGtkUIItem{box, option.get_ui_type()},
        m_option{option}, m_store{categories.m_store}, m_categories{categories.m_view},
        m_accounts{members.m_accounts}, m_securities{members.m_securities}, m_error{error}
    {
        configure_member_selection(m_accounts, m_securities);
        connect_category_signals(m_categories);
        connect_name_editor(categories.m_name_renderer);
        connect_target_editor(categories.m_target_renderer);
        connect_category_buttons(categories.m_add, categories.m_rename,
                                 categories.m_remove, members.m_children);
    }

    // Replace pending definitions from the serialized option, migrate legacy
    // memberships, and rebuild selection/validation without marking an edit.
    void set_ui_item_from_option(GncOption& option) noexcept override
    {
        auto text = option.get_value<std::string>();
        // Read a data payload, never evaluate Scheme from saved report settings.
        auto values = scm_c_catch(SCM_BOOL_T,
            +[](void* data) -> SCM {
                return scm_c_read_string(static_cast<std::string*>(data)->c_str());
            }, &text,
            +[](void*, SCM, SCM) -> SCM { return SCM_EOL; }, nullptr, nullptr, nullptr);
        // Payload entries are (name member...) or (name target member...).
        // The leading marker distinguishes these and disables legacy expansion.
        auto targets = scm_is_pair(values) &&
            scm_is_eq(scm_car(values), scm_from_utf8_symbol("explicit-targets"));
        auto explicit_members = targets || (scm_is_pair(values) &&
            scm_is_eq(scm_car(values), scm_from_utf8_symbol("explicit")));
        if (explicit_members)
            values = scm_cdr(values);
        m_loading = true;
        m_selected = -1;
        m_values.clear();
        gtk_list_store_clear(m_store);
        for (; scm_is_pair(values); values = scm_cdr(values))
        {
            auto entry = scm_car(values);
            if (!scm_is_pair(entry) || !scm_is_string(scm_car(entry)))
                continue;
            auto name = scm_to_utf8_string(scm_car(entry));
            Category category{name, {}};
            free(name);
            auto members = load_category_target(category, scm_cdr(entry), targets);
            load_category_members(category, members, explicit_members);
            m_values.push_back(std::move(category));
            GtkTreeIter iter;
            gtk_list_store_append(m_store, &iter);
            gtk_list_store_set(m_store, &iter, CATEGORY_NAME, m_values.back().m_name.c_str(),
                               CATEGORY_POSITION, static_cast<int>(m_values.size() - 1),
                               CATEGORY_TARGET, m_values.back().m_target.c_str(), -1);
        }
        m_loading = false;
        GtkTreeIter iter;
        if (gtk_tree_model_get_iter_first(GTK_TREE_MODEL(m_store), &iter))
            gtk_tree_selection_select_iter(gtk_tree_view_get_selection(m_categories), &iter);
        select_category();
        validate();
    }

    // Serialize pending definitions in display order with explicit membership.
    // Preserve unspecified versus zero targets; an invalid target aborts writing.
    void set_option_from_ui_item(GncOption& option) noexcept override
    {
        // If any target is specified, give every category a target field and
        // use #f for missing targets. Otherwise retain the target-free format.
        auto targets = std::any_of(m_values.begin(), m_values.end(),
            [](const Category& category) { return !category.m_target.empty(); });
        auto values = SCM_EOL;
        // Build Scheme lists backwards with cons to preserve the displayed order.
        for (auto category = m_values.rbegin(); category != m_values.rend(); ++category)
        {
            auto members = SCM_EOL;
            for (auto member = category->m_members.rbegin(); member != category->m_members.rend(); ++member)
                members = scm_cons(scm_from_utf8_string(member->c_str()), members);
            if (targets)
            {
                double target = 0;
                if (!target_value(category->m_target, target))
                    return;
                members = scm_cons(category->m_target.empty() ? SCM_BOOL_F : scm_from_double(target), members);
            }
            values = scm_cons(scm_cons(scm_from_utf8_string(category->m_name.c_str()), members), values);
        }
        // Keep the marker and ordering stable for saved-report compatibility.
        values = scm_cons(scm_from_utf8_symbol(targets ? "explicit-targets" : "explicit"), values);
        auto text = scm_to_utf8_string(scm_object_to_string(values, SCM_UNDEFINED));
        option.set_value(std::string{text});
        free(text);
    }
};

// Create the homogeneous, consistently spaced grid used by both button groups.
GtkWidget*
button_grid()
{
    auto grid = gtk_grid_new();
    gtk_grid_set_row_spacing(GTK_GRID(grid), 6);
    gtk_grid_set_column_spacing(GTK_GRID(grid), 6);
    gtk_grid_set_column_homogeneous(GTK_GRID(grid), TRUE);
    return grid;
}

// Pack category commands into pane and retain buttons for later signal wiring.
// Clear All resolves option's current UI item when clicked; option must outlive it.
void
create_category_buttons(GncOption& option, CategoryPane& pane)
{
    auto buttons = button_grid();
    pane.m_add = gtk_button_new_with_label(_("Add"));
    pane.m_rename = gtk_button_new_with_label(_("Rename"));
    pane.m_remove = gtk_button_new_with_label(_("Remove"));
    auto clear = gtk_button_new_with_label(_("Clear All"));
    gtk_button_set_image(GTK_BUTTON(pane.m_add),
                        gtk_image_new_from_icon_name("list-add", GTK_ICON_SIZE_BUTTON));
    gtk_button_set_image(GTK_BUTTON(pane.m_remove),
                        gtk_image_new_from_icon_name("list-remove", GTK_ICON_SIZE_BUTTON));
    gtk_widget_set_tooltip_text(pane.m_add, _("Add a category."));
    gtk_widget_set_tooltip_text(pane.m_rename, _("Rename the selected category."));
    gtk_widget_set_tooltip_text(pane.m_remove, _("Remove the selected category without changing any accounts."));
    gtk_widget_set_tooltip_text(clear, _("Remove all categories and account assignments."));
    gtk_grid_attach(GTK_GRID(buttons), pane.m_add, 0, 0, 1, 1);
    gtk_grid_attach(GTK_GRID(buttons), pane.m_rename, 1, 0, 1, 1);
    gtk_grid_attach(GTK_GRID(buttons), pane.m_remove, 0, 1, 1, 1);
    gtk_grid_attach(GTK_GRID(buttons), clear, 1, 1, 1, 1);
    g_signal_connect(clear, "clicked", G_CALLBACK(+[](GtkButton*, gpointer data) {
        auto item = dynamic_cast<GncGtkAccountCategoriesUIItem*>(static_cast<GncOption*>(data)->get_ui_item());
        if (item)
            item->clear_values();
    }), &option);
    gtk_box_pack_start(GTK_BOX(pane.m_box), buttons, FALSE, FALSE, 0);
}

// Append the editable target column after the category-name column, binding raw
// targets from the model. Return the renderer owned by the new column.
GtkCellRenderer*
create_target_column(GtkTreeView* view)
{
    auto renderer = GTK_CELL_RENDERER(g_object_new(gnc_allocation_target_renderer_get_type(), nullptr));
    g_object_set(renderer, "editable", TRUE, "xalign", 1.0f, "width-chars", 6, nullptr);
    gtk_tree_view_insert_column_with_attributes(view, -1, _("Target %"), renderer, "text", CATEGORY_TARGET, nullptr);
    auto column = gtk_tree_view_get_column(view, 1);
    gtk_tree_view_column_set_resizable(column, TRUE);
    auto heading = gtk_label_new(_("Target %"));
    gtk_widget_set_tooltip_text(heading,
        _("Optional allocation target from 0 to 100. Leave blank for unspecified; zero is an explicit target. "
          "Targets are retained independently of the chart type."));
    gtk_tree_view_column_set_widget(column, heading);
    gtk_widget_show(heading);
    return renderer;
}

// Assemble the editable, reorderable category list and its command buttons.
// Selection wiring comes later.
CategoryPane
create_category_pane(GncOption& option)
{
    CategoryPane pane;
    pane.m_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
    pane.m_store = gtk_list_store_new(NUM_CATEGORY_COLUMNS, G_TYPE_STRING, G_TYPE_INT, G_TYPE_STRING);
    pane.m_view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(pane.m_store)));
    g_object_unref(pane.m_store);
    gtk_tree_view_set_reorderable(pane.m_view, TRUE);
    pane.m_name_renderer = gtk_cell_renderer_text_new();
    g_object_set(pane.m_name_renderer, "editable", TRUE, "ellipsize", PANGO_ELLIPSIZE_END,
                 "width-chars", 16, "max-width-chars", 18, nullptr);
    set_allocation_tree_tooltips(pane.m_view);
    gtk_tree_view_insert_column_with_attributes(pane.m_view, -1, _("Category"),
                                                pane.m_name_renderer, "text", CATEGORY_NAME, nullptr);
    gtk_tree_view_column_set_resizable(gtk_tree_view_get_column(pane.m_view, 0), TRUE);
    gtk_tree_view_column_set_expand(gtk_tree_view_get_column(pane.m_view, 0), TRUE);
    pane.m_target_renderer = create_target_column(pane.m_view);
    auto scroll = gtk_scrolled_window_new(nullptr, nullptr);
    gtk_widget_set_size_request(scroll, pane.m_target_renderer ? 240 : 160, 180);
    gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(pane.m_view));
    gtk_box_pack_start(GTK_BOX(pane.m_box), scroll, TRUE, TRUE, 0);
    create_category_buttons(option, pane);
    return pane;
}

// Return unique commodity names for currencies referenced by the current
// book's accounts, transactions, or prices, excluding globally unused entries.
std::set<std::string>
used_currencies()
{
    std::set<std::string> result;
    auto accounts = gnc_account_get_descendants(gnc_get_current_root_account());
    for (auto node = accounts; node; node = node->next)
    {
        auto commodity = xaccAccountGetCommodity(static_cast<Account*>(node->data));
        if (commodity && gnc_commodity_is_currency(commodity))
            result.insert(gnc_commodity_get_unique_name(commodity));
    }
    g_list_free(accounts);
    qof_collection_foreach(qof_book_get_collection(gnc_get_current_book(), GNC_ID_TRANS),
        +[](QofInstance* instance, gpointer data) {
            auto currency = xaccTransGetCurrency(reinterpret_cast<Transaction*>(instance));
            if (currency && gnc_commodity_is_currency(currency))
                static_cast<std::set<std::string>*>(data)->insert(gnc_commodity_get_unique_name(currency));
        }, &result);
    gnc_pricedb_foreach_price(gnc_pricedb_get_db(gnc_get_current_book()),
        +[](GNCPrice* price, gpointer data) -> gboolean {
            for (auto commodity : {gnc_price_get_commodity(price), gnc_price_get_currency(price)})
                if (commodity && gnc_commodity_is_currency(commodity))
                    static_cast<std::set<std::string>*>(data)->insert(gnc_commodity_get_unique_name(commodity));
            return TRUE;
        }, &result, FALSE);
    return result;
}

struct HoldingsTree
{
    GtkTreeStore* m_store;
    GtkTreeIter m_currencies;
    GtkTreeIter m_securities;
    std::set<std::string> m_used_currencies;
    std::map<std::string, GtkTreeIter> m_namespaces;
};

// Commodity-table visitor using the HoldingsTree supplied in data. Skip unused
// currencies and templates, group securities by namespace, and keep iterating.
gboolean
append_holding(gnc_commodity* commodity, gpointer data)
{
    auto tree = static_cast<HoldingsTree*>(data);
    auto currency = gnc_commodity_is_currency(commodity);
    auto name = gnc_commodity_get_namespace(commodity);
    if ((currency && !tree->m_used_currencies.contains(gnc_commodity_get_unique_name(commodity))) ||
        g_strcmp0(name, GNC_COMMODITY_NS_TEMPLATE) == 0)
        return TRUE;
    auto parent = tree->m_currencies;
    if (!currency)
    {
        auto found = tree->m_namespaces.find(name);
        if (found != tree->m_namespaces.end())
            parent = found->second;
        else
        {
            gtk_tree_store_append(tree->m_store, &parent, &tree->m_securities);
            gtk_tree_store_set(tree->m_store, &parent, HOLDING_NAME, name, HOLDING_ID, "", -1);
            tree->m_namespaces.emplace(name, parent);
        }
    }
    append_allocation_holding(tree->m_store, &parent, commodity);
    return TRUE;
}

// Build a sorted holding picker grouped into currencies and security namespaces.
// The returned view owns its model; membership restrictions are installed later.
GtkTreeView*
create_holdings_view()
{
    auto store = gtk_tree_store_new(NUM_HOLDING_COLUMNS,
                                   G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
    GtkTreeIter currencies, securities;
    gtk_tree_store_append(store, &currencies, nullptr);
    gtk_tree_store_set(store, &currencies, HOLDING_NAME, _("Currencies"), HOLDING_ID, "", -1);
    gtk_tree_store_append(store, &securities, nullptr);
    gtk_tree_store_set(store, &securities, HOLDING_NAME, _("Securities"), HOLDING_ID, "", -1);
    HoldingsTree tree{store, currencies, securities, used_currencies(), {}};
    gnc_commodity_table_foreach_commodity(gnc_commodity_table_get_table(gnc_get_current_book()),
                                        append_holding, &tree);
    gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(store), HOLDING_NAME, GTK_SORT_ASCENDING);
    auto view = GTK_TREE_VIEW(gtk_tree_view_new_with_model(GTK_TREE_MODEL(store)));
    g_object_unref(store);
    auto renderer = gtk_cell_renderer_text_new();
    g_object_set(renderer, "ellipsize", PANGO_ELLIPSIZE_END, nullptr);
    gtk_tree_view_insert_column_with_attributes(view, -1, _("Holdings"), renderer, "text", HOLDING_NAME, nullptr);
    set_allocation_tree_tooltips(view);
    auto column = gtk_tree_view_get_column(view, 0);
    gtk_tree_view_column_set_resizable(column, TRUE);
    gtk_tree_view_column_set_expand(column, TRUE);
    gtk_tree_view_column_set_sizing(column, GTK_TREE_VIEW_COLUMN_FIXED);
    gtk_tree_view_column_set_fixed_width(column, 220);
    gtk_tree_view_expand_all(view);
    return view;
}

// Build an account picker limited to asset-related types, including hidden
// accounts. Category-specific selection restrictions are installed later.
GncTreeViewAccount*
create_accounts_view()
{
    auto view = GNC_TREE_VIEW_ACCOUNT(gnc_tree_view_account_new(FALSE));
    AccountViewInfo info;
    gnc_tree_view_account_get_view_info(view, &info);
    for (auto& include : info.include_type)
        include = FALSE;
    for (auto type : {ACCT_TYPE_ASSET, ACCT_TYPE_BANK, ACCT_TYPE_CASH, ACCT_TYPE_STOCK,
                     ACCT_TYPE_MUTUAL, ACCT_TYPE_RECEIVABLE, ACCT_TYPE_CURRENCY})
        info.include_type[type] = TRUE;
    info.show_hidden = TRUE;
    gnc_tree_view_account_set_view_info(view, &info);
    return view;
}

// Pack bulk-selection controls and connect Select All/Clear All to view.
// Retain Select Children for the UI item's mode-specific descendant handler.
void
create_member_buttons(MemberPane& pane, GtkTreeView* view)
{
    auto buttons = button_grid();
    auto select = gtk_button_new_with_label(_("Select All"));
    auto clear = gtk_button_new_with_label(_("Clear All"));
    pane.m_children = gtk_button_new_with_label(_("Select Children"));
    gtk_widget_set_tooltip_text(select, pane.m_securities
        ? _("Select all holdings not assigned to another category.")
        : _("Select all accounts not assigned to another category."));
    gtk_widget_set_tooltip_text(clear, pane.m_securities
        ? _("Clear the selected category's holding selection without removing the category.")
        : _("Clear the selected category's account selection without removing the category."));
    gtk_widget_set_tooltip_text(pane.m_children, pane.m_securities
        ? _("Select holdings beneath the selected groups, except holdings assigned to another category.")
        : _("Select descendants of the selected accounts, except accounts assigned to another category."));
    gtk_grid_attach(GTK_GRID(buttons), select, 0, 0, 1, 1);
    gtk_grid_attach(GTK_GRID(buttons), clear, 1, 0, 1, 1);
    gtk_grid_attach(GTK_GRID(buttons), pane.m_children, 0, 1, 2, 1);
    g_signal_connect(select, "clicked", G_CALLBACK(+[](GtkButton*, gpointer data) {
        auto view = GTK_TREE_VIEW(data);
        gtk_tree_view_expand_all(view);
        gtk_tree_selection_select_all(gtk_tree_view_get_selection(view));
    }), view);
    g_signal_connect(clear, "clicked", G_CALLBACK(+[](GtkButton*, gpointer data) {
        gtk_tree_selection_unselect_all(gtk_tree_view_get_selection(GTK_TREE_VIEW(data)));
    }), view);
    gtk_box_pack_start(GTK_BOX(pane.m_box), buttons, FALSE, FALSE, 0);
}

// Assemble the account or holding picker selected by option's UI type, with
// multiple selection, a category-specific heading, and bulk-selection controls.
MemberPane
create_member_pane(GncOption& option)
{
    MemberPane pane;
    pane.m_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
    if (option.get_ui_type() == GncOptionUIType::SECURITY_CATEGORIES)
        pane.m_securities = create_holdings_view();
    else
        pane.m_accounts = create_accounts_view();
    auto view = pane.m_securities ? pane.m_securities : GTK_TREE_VIEW(pane.m_accounts);
    auto column = pane.m_securities ? gtk_tree_view_get_column(view, 0)
        : gnc_tree_view_find_column_by_name(GNC_TREE_VIEW(view), "name");
    auto heading = gtk_label_new(pane.m_securities ? _("Holdings for a new category")
                                                 : _("Accounts for a new category"));
    gtk_label_set_xalign(GTK_LABEL(heading), 0);
    gtk_label_set_ellipsize(GTK_LABEL(heading), PANGO_ELLIPSIZE_END);
    gtk_label_set_max_width_chars(GTK_LABEL(heading), pane.m_securities ? -1 : 24);
    gtk_tree_view_column_set_widget(column, heading);
    gtk_widget_show(heading);
    gtk_tree_view_column_set_sort_column_id(column, -1);
    gtk_tree_view_column_set_sort_indicator(column, FALSE);
    gtk_tree_view_column_set_clickable(column, FALSE);
    gtk_tree_view_set_headers_visible(view, TRUE);
    gtk_tree_selection_set_mode(gtk_tree_view_get_selection(view), GTK_SELECTION_MULTIPLE);
    auto scroll = gtk_scrolled_window_new(nullptr, nullptr);
    gtk_widget_set_size_request(scroll, 220, 180);
    gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(view));
    gtk_widget_set_tooltip_text(GTK_WIDGET(view), pane.m_securities
        ? _("Select the securities and currencies belonging to this category.")
        : _("Select category accounts. Only selected accounts are included."));
    gtk_box_pack_start(GTK_BOX(pane.m_box), scroll, TRUE, TRUE, 0);
    create_member_buttons(pane, view);
    return pane;
}
}

void
gnc_create_allocation_categories_widget(GncOption& option, GtkGrid* grid, int row)
{
    auto box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6);
    auto panes = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL);
    auto categories = create_category_pane(option);
    auto members = create_member_pane(option);
    gtk_widget_set_margin_end(categories.m_box, 12);
    gtk_widget_set_margin_start(members.m_box, 12);
    gtk_paned_pack1(GTK_PANED(panes), categories.m_box, FALSE, FALSE);
    gtk_paned_pack2(GTK_PANED(panes), members.m_box, TRUE, FALSE);
    gtk_box_pack_start(GTK_BOX(box), panes, TRUE, TRUE, 0);
    auto error = gtk_label_new("");
    gtk_widget_set_no_show_all(error, TRUE);
    gtk_label_set_line_wrap(GTK_LABEL(error), TRUE);
    gtk_label_set_xalign(GTK_LABEL(error), 0);
    gtk_box_pack_start(GTK_BOX(box), error, FALSE, FALSE, 0);
    gtk_widget_set_hexpand(box, TRUE);
    gtk_widget_set_vexpand(box, TRUE);
    // The option owns the UI item, which connects callbacks before loading the
    // saved value. Loading suppresses edit signals while rebuilding both panes.
    option.set_ui_item(std::make_unique<GncGtkAccountCategoriesUIItem>(
        option, box, categories, members, error));
    option.set_ui_item_from_option();
    set_tool_tip(option, box);
    gtk_widget_set_halign(GTK_WIDGET(grid), GTK_ALIGN_FILL);
    grid_attach_widget(grid, box, row);
    gtk_widget_show_all(box);
}

