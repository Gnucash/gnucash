#include "test-option-ui.hpp"

namespace
{
TEST(DialogOptions, ValidatesCategories)
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