#include "test-option-ui.hpp"

namespace
{
// Apply account sensitivity consistently without enabling independently disabled cells.
TEST(AccountTree, account_sensitivity)
{
    Book book;
    auto parent = Book::id(book.parent);
    auto child = Book::id(book.child);
    Editor editor{GncOptionUIType::ACCOUNT_CATEGORIES, "(explicit)"};
    auto view = GNC_TREE_VIEW_ACCOUNT(editor.trees[1]);
    gnc_tree_view_account_set_sensitive_func(view, nullptr, nullptr);
    auto standard = gtk_tree_view_get_column(editor.trees[1], 0);
    auto source = +[](Account*, GtkTreeViewColumn*, GtkCellRenderer*) { return g_strdup("Custom"); };
    auto custom = gnc_tree_view_account_add_custom_column(view, "Custom", source, nullptr);
    auto property = gnc_tree_view_account_add_property_column(view, "Name", "name");
    auto renderer = gtk_cell_renderer_text_new();
    g_object_set(renderer, "sensitive", FALSE, nullptr);
    auto disabled = gnc_tree_view_account_add_custom_column_renderer(view, "Disabled", source, nullptr, renderer);
    auto dynamic = gnc_tree_view_account_add_custom_column(view, "Conditional",
        +[](Account* account, GtkTreeViewColumn*, GtkCellRenderer* cell) {
            g_object_set(cell, "sensitive", g_strcmp0(xaccAccountGetName(account), "Savings") != 0, nullptr);
            return g_strdup("Conditional");
        }, nullptr);
    auto check = [&](const std::string& id, bool expected) {
        for (auto column : {standard, custom, property})
            EXPECT_EQ(editor.member_sensitive(id, column), expected);
        EXPECT_FALSE(editor.member_sensitive(id, disabled));
    };
    check(parent, true);
    check(child, true);
    EXPECT_TRUE(editor.member_sensitive(parent, dynamic));
    EXPECT_FALSE(editor.member_sensitive(child, dynamic));

    auto sensitive = +[](Account* account, gpointer data) -> gboolean { return account != data; };
    gnc_tree_view_account_set_sensitive_func(view, sensitive, book.parent);
    check(parent, false);
    check(child, true);
    check(parent, false);
    EXPECT_FALSE(editor.member_sensitive(parent, dynamic));
    EXPECT_FALSE(editor.member_sensitive(child, dynamic));

    gnc_tree_view_account_set_sensitive_func(view, sensitive, book.child);
    check(parent, true);
    check(child, false);
    EXPECT_TRUE(editor.member_sensitive(parent, dynamic));
    EXPECT_FALSE(editor.member_sensitive(child, dynamic));
    gnc_tree_view_account_set_sensitive_func(view, nullptr, nullptr);
    check(parent, true);
    check(child, true);
    EXPECT_TRUE(editor.member_sensitive(parent, dynamic));
    EXPECT_FALSE(editor.member_sensitive(child, dynamic));

    gnc_tree_view_account_set_sensitive_func(view, sensitive, book.parent);
    check(parent, false);
    EXPECT_FALSE(editor.member_sensitive(parent, dynamic));
    gnc_tree_view_account_set_sensitive_func(view, nullptr, nullptr);
    check(parent, true);
    check(child, true);
    EXPECT_TRUE(editor.member_sensitive(parent, dynamic));
    EXPECT_FALSE(editor.member_sensitive(child, dynamic));
}

// Replacing or disabling an account editor must not retain old signal connections.
TEST(AccountTree, account_editing_callbacks)
{
    Book book;
    Editor editor{GncOptionUIType::ACCOUNT_CATEGORIES, "(explicit)"};
    auto view = GNC_TREE_VIEW_ACCOUNT(editor.trees[1]);
    auto column = gtk_tree_view_get_column(editor.trees[1], 0);
    auto cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(column));
    GtkCellRenderer* renderer = nullptr;
    for (auto node = cells; node; node = node->next)
        if (GTK_IS_CELL_RENDERER_TEXT(node->data))
            renderer = GTK_CELL_RENDERER(node->data);
    g_list_free(cells);
    ASSERT_NE(renderer, nullptr);
    struct Edits { unsigned int first{}; unsigned int second{}; } edits;
    g_object_set_data(G_OBJECT(column), "test-edit-counts", &edits);
    auto callback = +[](Account* account, GtkTreeViewColumn* column, const gchar* text) {
        ASSERT_NE(account, nullptr);
        EXPECT_STREQ(text, "Edited");
        ++static_cast<Edits*>(g_object_get_data(G_OBJECT(column), "test-edit-counts"))->first;
    };
    auto replacement = +[](Account*, GtkTreeViewColumn* column, const gchar*) {
        ++static_cast<Edits*>(g_object_get_data(G_OBJECT(column), "test-edit-counts"))->second;
    };
    auto emit_edit = [&] { g_signal_emit_by_name(renderer, "edited", "0", "Edited"); };

    gnc_tree_view_account_set_name_edited(view, callback);
    emit_edit();
    EXPECT_EQ(edits.first, 1u);
    gnc_tree_view_account_set_name_edited(view, callback);
    emit_edit();
    EXPECT_EQ(edits.first, 2u);
    gnc_tree_view_account_set_name_edited(view, replacement);
    emit_edit();
    EXPECT_EQ(edits.first, 2u);
    EXPECT_EQ(edits.second, 1u);

    gnc_tree_view_account_set_name_edited(view, nullptr);
    gboolean editable = TRUE;
    g_object_get(renderer, "editable", &editable, nullptr);
    EXPECT_FALSE(editable);
    emit_edit();
    EXPECT_EQ(edits.second, 1u);
    gnc_tree_view_account_set_name_edited(view, nullptr);
    gnc_tree_view_account_set_name_edited(view, callback);
    g_object_get(renderer, "editable", &editable, nullptr);
    EXPECT_TRUE(editable);
    emit_edit();
    EXPECT_EQ(edits.first, 3u);
    EXPECT_EQ(edits.second, 1u);
    gnc_tree_view_account_set_name_edited(view, nullptr);
    g_object_set_data(G_OBJECT(column), "test-edit-counts", nullptr);
}
}

int main(int argc, char** argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    if (!gtk_init_check(&argc, &argv))
    {
        if (g_strcmp0(g_getenv("GNC_REQUIRE_GUI_TESTS"), "1") == 0)
        {
            g_printerr("GTK allocation tests require a display; run under xvfb-run.\n");
            return 1;
        }
        return 77;
    }
    scm_init_guile();
    gnc_engine_init(0, nullptr);
    gnc_gsettings_load_backend();
    return RUN_ALL_TESTS();
}