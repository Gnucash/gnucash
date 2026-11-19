/********************************************************************
 * test-option-ui.hpp -- shared GTK option test fixtures
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

#pragma once

#include "gnc-option-impl.hpp"
#include "gnc-optiondb-impl.hpp"
#include "gnc-optiondb.hpp"
#include <config.h>
#include <gtest/gtest.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <stdexcept>
#include <string>
#include <vector>

#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-engine.h"
#include "gnc-gsettings.h"
#include "gnc-option-gtk-ui.hpp"
#include "dialog-options.hpp"
#include "gnc-pricedb.h"
#include "gnc-session.h"
extern "C"
{
#include "gnc-tree-view-account.h"
}

namespace
{
struct Book
{
    QofBook* book{qof_session_get_book(gnc_get_current_session())};
    Account* root{gnc_account_create_root(book)};
    gnc_commodity* currency;
    Account* parent;
    Account* child;

    Book()
    {
        auto table = gnc_commodity_table_get_table(book);
        currency = gnc_commodity_table_lookup(table, "CURRENCY", "USD");
        if (!currency)
            currency = gnc_commodity_table_insert(table,
                gnc_commodity_new(book, "US Dollar", "CURRENCY", "USD", "", 100));
        parent = account(root, "Bank");
        child = account(parent, "Savings");
    }

    Account* account(Account* owner, const char* name)
    {
        auto result = xaccMallocAccount(book);
        xaccAccountBeginEdit(result);
        xaccAccountSetName(result, name);
        xaccAccountSetType(result, ACCT_TYPE_BANK);
        xaccAccountSetCommodity(result, currency);
        gnc_account_append_child(owner, result);
        xaccAccountCommitEdit(result);
        return result;
    }

    ~Book() { gnc_clear_current_session(); }

    static std::string id(Account* account)
    {
        auto text = guid_to_string(xaccAccountGetGUID(account));
        std::string result{text};
        g_free(text);
        return result;
    }
};

struct Editor
{
    GncOption option;
    GtkWidget* window{gtk_window_new(GTK_WINDOW_TOPLEVEL)};
    GtkWidget* grid{gtk_grid_new()};
    std::vector<GtkTreeView*> trees;
    GncOptionGtkUIItem* item;

    Editor(GncOptionUIType type, const std::string& payload) :
        Editor{GncOption{"Categories", "Categories", "a", "", payload, type}}
    {
    }

    explicit Editor(GncOption value) : option{std::move(value)}
    {
        gtk_container_add(GTK_CONTAINER(window), grid);
        GncOptionUIFactory::create(option, GTK_GRID(grid), 0);
        item = dynamic_cast<GncOptionGtkUIItem*>(option.get_ui_item());
        if (!item)
            throw std::runtime_error{"Option has no GTK UI item"};
        visit(grid, +[](GtkWidget* widget, void* data) {
            if (GTK_IS_TREE_VIEW(widget))
                static_cast<Editor*>(data)->trees.push_back(GTK_TREE_VIEW(widget));
        }, this);
        auto categories = option.get_ui_type() == GncOptionUIType::ACCOUNT_CATEGORIES ||
            option.get_ui_type() == GncOptionUIType::SECURITY_CATEGORIES;
        if (trees.size() != (categories ? 2u : 1u))
            throw std::runtime_error{"Option editor has unexpected tree views"};
        gtk_widget_show_all(window);
        while (gtk_events_pending())
            gtk_main_iteration();
    }

    static void visit(GtkWidget* widget, void (*callback)(GtkWidget*, void*), void* data)
    {
        callback(widget, data);
        if (!GTK_IS_CONTAINER(widget))
            return;
        auto children = gtk_container_get_children(GTK_CONTAINER(widget));
        for (auto node = children; node; node = node->next)
            visit(GTK_WIDGET(node->data), callback, data);
        g_list_free(children);
    }

    void load(const std::string& payload)
    {
        option.set_value(payload);
        option.set_ui_item_from_option();
    }

    std::string save()
    {
        option.set_option_from_ui_item();
        return option.get_value<std::string>();
    }

    void edit(int column, const char* path, const char* text)
    {
        auto cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(
            gtk_tree_view_get_column(trees[0], column)));
        ASSERT_NE(cells, nullptr);
        g_signal_emit_by_name(cells->data, "edited", path, text);
        g_list_free(cells);
    }

    void click(const char* label, unsigned int occurrence = 0)
    {
        struct Matches { const char* label; std::vector<GtkButton*> buttons; } matches{label, {}};
        visit(grid, +[](GtkWidget* widget, void* data) {
            auto matches = static_cast<Matches*>(data);
            if (GTK_IS_BUTTON(widget) &&
                g_strcmp0(gtk_button_get_label(GTK_BUTTON(widget)), matches->label) == 0)
                matches->buttons.push_back(GTK_BUTTON(widget));
        }, &matches);
        ASSERT_LT(occurrence, matches.buttons.size());
        gtk_button_clicked(matches.buttons.at(occurrence));
    }

    void select(int tree, int index)
    {
        auto path = gtk_tree_path_new_from_indices(index, -1);
        gtk_tree_selection_select_path(gtk_tree_view_get_selection(trees.at(tree)), path);
        gtk_tree_path_free(path);
    }

    bool key(guint keyval, GdkModifierType modifiers = GDK_MOD1_MASK)
    {
        GdkEventKey event{};
        event.type = GDK_KEY_PRESS;
        event.keyval = keyval;
        event.state = modifiers;
        gboolean handled = FALSE;
        g_signal_emit_by_name(trees[0], "key-press-event", &event, &handled);
        return handled;
    }

    std::string warning() const
    {
        auto children = gtk_container_get_children(GTK_CONTAINER(item->get_widget()));
        auto label = GTK_LABEL(g_list_last(children)->data);
        std::string text{gtk_label_get_text(label)};
        g_list_free(children);
        return text;
    }

    bool member_sensitive(const std::string& id, GtkTreeViewColumn* column = nullptr)
    {
        struct Search
        {
            Editor* editor;
            const std::string& id;
            GtkTreeViewColumn* column;
            bool found;
            bool sensitive;
        } search{this, id, column, false, false};
        gtk_tree_model_foreach(gtk_tree_view_get_model(trees[1]),
            +[](GtkTreeModel* model, GtkTreePath*, GtkTreeIter* iter, void* data) -> gboolean {
                auto search = static_cast<Search*>(data);
                auto view = search->editor->trees[1];
                std::string id;
                if (GNC_IS_TREE_VIEW_ACCOUNT(view))
                {
                    auto account = gnc_tree_view_account_get_account_from_iter(model, iter);
                    if (account)
                        id = Book::id(account);
                }
                else
                {
                    gchar* text;
                    gtk_tree_model_get(model, iter, 3, &text, -1);
                    id = text;
                    g_free(text);
                }
                if (id != search->id)
                    return FALSE;
                auto column = search->column ? search->column : gtk_tree_view_get_column(view, 0);
                gtk_tree_view_column_cell_set_cell_data(column, model, iter, FALSE, FALSE);
                auto cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(column));
                if (!cells)
                {
                    ADD_FAILURE() << "Account column has no cell renderer";
                    return TRUE;
                }
                gboolean sensitive = FALSE;
                g_object_get(cells->data, "sensitive", &sensitive, nullptr);
                g_list_free(cells);
                search->found = true;
                search->sensitive = sensitive;
                return TRUE;
            }, &search);
        EXPECT_TRUE(search.found) << id;
        return search.sensitive;
    }

    ~Editor()
    {
        option.set_ui_item({});
        gtk_widget_destroy(window);
    }
};

}
