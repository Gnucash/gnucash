#include "test-option-ui.hpp"

namespace
{
TEST(DialogOptions, ValidatesCategoriesAndSelectsVisiblePages)
{
    Book book;
    GncOptionDB options;
    options.register_option("Hidden", GncOption{"Hidden", "Legacy", "a", "", std::string{"retained"}});
    options.register_option("General", GncOption{"General", "Table", "a", "", true, GncOptionUIType::BOOLEAN});
    options.register_option("Accounts", GncOption{"Accounts", "Include", "a", "", true, GncOptionUIType::BOOLEAN});
    options.register_option("Categories", GncOption{"Categories", "Categories", "a", "",
        std::string{"(explicit)"}, GncOptionUIType::ACCOUNT_CATEGORIES});
    auto dialog = std::make_unique<GncOptionsDialog>("Allocation test", nullptr);
    dialog->build_contents(&options, false);
    EXPECT_EQ(gtk_notebook_get_n_pages(GTK_NOTEBOOK(dialog->get_notebook())), 3);
    auto notebook = GTK_NOTEBOOK(dialog->get_notebook());
    for (auto section : {"General", "Accounts", "Categories", "Accounts"})
    {
        ASSERT_TRUE(dialog->select_page(section));
        auto index = gtk_notebook_get_current_page(notebook);
        auto page = gtk_notebook_get_nth_page(notebook, index);
        EXPECT_STREQ(gtk_notebook_get_tab_label_text(notebook, page), section);
        auto view = GTK_TREE_VIEW(dialog->get_page_list_view());
        GtkTreeModel* model;
        GtkTreeIter iter;
        ASSERT_TRUE(gtk_tree_selection_get_selected(gtk_tree_view_get_selection(view), &model, &iter));
        gint selected;
        gtk_tree_model_get(model, &iter, 0, &selected, -1);
        EXPECT_EQ(selected, index);
    }
    auto selected = gtk_notebook_get_current_page(notebook);
    EXPECT_FALSE(dialog->select_page("Missing"));
    EXPECT_FALSE(dialog->select_page("Hidden"));
    EXPECT_FALSE(dialog->select_page(nullptr));
    EXPECT_EQ(gtk_notebook_get_current_page(notebook), selected);
    struct Buttons { GtkWidget* apply{}; GtkWidget* ok{}; } buttons;
    Editor::visit(dialog->get_widget(), +[](GtkWidget* widget, void* data) {
        auto buttons = static_cast<Buttons*>(data);
        auto name = gtk_buildable_get_name(GTK_BUILDABLE(widget));
        if (g_strcmp0(name, "applybutton") == 0)
            buttons->apply = widget;
        if (g_strcmp0(name, "okbutton") == 0)
            buttons->ok = widget;
    }, &buttons);
    ASSERT_NE(buttons.apply, nullptr);
    ASSERT_NE(buttons.ok, nullptr);
    dialog->set_sensitive(true);
    EXPECT_FALSE(gtk_widget_get_sensitive(buttons.apply));
    EXPECT_FALSE(gtk_widget_get_sensitive(buttons.ok));
    auto category = options.find_option("Categories", "Categories");
    ASSERT_NE(category, nullptr);
    category->set_value("(explicit (\"Bank\" \"" + Book::id(book.parent) + "\"))");
    category->set_ui_item_from_option();
    dialog->changed();
    EXPECT_TRUE(gtk_widget_get_sensitive(buttons.apply));
    EXPECT_TRUE(gtk_widget_get_sensitive(buttons.ok));
    dialog->set_sensitive(false);
    EXPECT_FALSE(gtk_widget_get_sensitive(buttons.apply));
    category->set_value(std::string{"(explicit)"});
    category->set_ui_item_from_option();
    dialog->changed();
    EXPECT_FALSE(gtk_widget_get_sensitive(buttons.ok));
    category->set_ui_item({});
    category->make_internal();
    dialog->set_sensitive(true);
    EXPECT_TRUE(gtk_widget_get_sensitive(buttons.apply));
    auto window = dialog->get_widget();
    g_object_ref(window);
    dialog.reset();
    gtk_widget_destroy(window);
    g_object_unref(window);
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