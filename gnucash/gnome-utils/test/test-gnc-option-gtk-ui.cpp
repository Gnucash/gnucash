#include "test-option-ui.hpp"

namespace
{
TEST(OptionGtkUI, account_type_filter)
{
    Book book;
    auto expense = book.account(book.root, "Expenses");
    xaccAccountSetType(expense, ACCT_TYPE_EXPENSE);
    auto hidden = book.account(book.root, "Hidden cash");
    xaccAccountSetHidden(hidden, TRUE);
    auto table = gnc_commodity_table_get_table(book.book);
    auto commodity = gnc_commodity_table_insert(table,
        gnc_commodity_new(book.book, "Test security", "NASDAQ", "TEST", "", 100));
    auto stock = book.account(book.root, "Stock");
    xaccAccountSetType(stock, ACCT_TYPE_STOCK);
    xaccAccountSetCommodity(stock, commodity);
    auto mutual = book.account(book.root, "Mutual");
    xaccAccountSetType(mutual, ACCT_TYPE_MUTUAL);
    xaccAccountSetCommodity(mutual, commodity);
    auto invalid = book.account(book.root, "Non-holding");
    xaccAccountSetCommodity(invalid, commodity);
    auto group = xaccMallocAccount(book.book);
    xaccAccountBeginEdit(group);
    xaccAccountSetName(group, "Group");
    xaccAccountSetType(group, ACCT_TYPE_ASSET);
    gnc_account_append_child(book.root, group);
    xaccAccountCommitEdit(group);
    auto cash = book.account(group, "Cash");
    xaccAccountSetType(cash, ACCT_TYPE_CASH);
    auto currency = book.account(book.root, "Currency");
    xaccAccountSetType(currency, ACCT_TYPE_CURRENCY);
    auto asset = book.account(book.root, "Asset");
    xaccAccountSetType(asset, ACCT_TYPE_ASSET);
    GncOptionAccountList defaults{*xaccAccountGetGUID(stock)};
    GncOptionAccountListValue value{"Accounts", "Accounts", "a", "",
        GncOptionUIType::ACCOUNT_LIST, defaults,
        GncOptionAccountTypeList{ACCT_TYPE_ASSET, ACCT_TYPE_BANK, ACCT_TYPE_CASH,
                                ACCT_TYPE_CURRENCY, ACCT_TYPE_STOCK, ACCT_TYPE_MUTUAL}};
    Editor editor{GncOption{std::move(value)}};
    auto visible = [&editor](Account* account) {
        struct Search { Account* account; bool found; } search{account, false};
        gtk_tree_model_foreach(gtk_tree_view_get_model(editor.trees[0]),
            +[](GtkTreeModel* model, GtkTreePath*, GtkTreeIter* iter, void* data) -> gboolean {
                auto search = static_cast<Search*>(data);
                search->found = gnc_tree_view_account_get_account_from_iter(model, iter) == search->account;
                return search->found;
            }, &search);
        return search.found;
    };
    for (auto account : {stock, mutual, book.parent, book.child, hidden, group, cash, currency, asset})
        EXPECT_TRUE(visible(account));
    EXPECT_FALSE(visible(expense));
    EXPECT_TRUE(visible(invalid));
    editor.click("Clear All");
    editor.option.set_option_from_ui_item();
    EXPECT_TRUE(editor.option.get_value<GncOptionAccountList>() == defaults);
    editor.click("Select Default");
    editor.option.set_option_from_ui_item();
    EXPECT_TRUE(editor.option.get_value<GncOptionAccountList>() == defaults);
    editor.click("Show Hidden Accounts");
    EXPECT_FALSE(visible(hidden));
    EXPECT_FALSE(visible(expense));
    editor.click("Show Hidden Accounts");
    EXPECT_TRUE(visible(hidden));
    EXPECT_TRUE(visible(invalid));
    GncOptionAccountListValue restricted{"Accounts", "Accounts", "a", "",
        GncOptionUIType::ACCOUNT_LIST, GncOptionAccountTypeList{ACCT_TYPE_STOCK}};
    Editor restricted_editor{GncOption{std::move(restricted)}};
    restricted_editor.click("Show Hidden Accounts");
    restricted_editor.click("Show Hidden Accounts");
    restricted_editor.click("Select All");
    restricted_editor.option.set_option_from_ui_item();
    EXPECT_TRUE(restricted_editor.option.get_value<GncOptionAccountList>() == defaults);
}

TEST(OptionGtkUI, chart_choices)
{
    using KeyType = GncOptionMultichoiceKeyType;
    GncOptionDB options;
    options.register_option("Chart", GncOption{GncOptionMultichoiceValue{
        "Chart", "Snapshot Chart", "b", "", "doughnut",
        {{"bars", "Bars", KeyType::SYMBOL},
         {"doughnut", "Doughnut", KeyType::SYMBOL},
         {"pie", "Pie", KeyType::SYMBOL},
         {"stacked-bar", "Stacked Bar", KeyType::SYMBOL}}}});
    auto dialog = std::make_unique<GncOptionsDialog>("Chart choices test", nullptr);
    dialog->build_contents(&options, false);
    auto option = options.find_option("Chart", "Snapshot Chart");
    auto item = dynamic_cast<GncOptionGtkUIItem*>(option->get_ui_item());
    auto combo = GTK_COMBO_BOX(item->get_widget());
    EXPECT_EQ(gtk_combo_box_get_active(combo), 1);
    EXPECT_EQ(item->set_choices(*option, {0, 3}, 0), 0);
    EXPECT_EQ(gtk_tree_model_iter_n_children(gtk_combo_box_get_model(combo), nullptr), 2);
    EXPECT_EQ(gtk_combo_box_get_active(combo), 0);
    EXPECT_EQ(option->get_value<uint16_t>(), 1);
    gtk_combo_box_set_active(combo, 1);
    EXPECT_EQ(item->get_choice().value(), 3);
    EXPECT_TRUE(scm_is_eq(item->get_widget_scm_value(*option), scm_from_utf8_symbol("stacked-bar")));
    option->set_option_from_ui_item();
    EXPECT_EQ(option->get_value<uint16_t>(), 3);
    EXPECT_EQ(item->set_choices(*option, {0, 1, 2, 3}, 0), 3);
    EXPECT_EQ(gtk_combo_box_get_active(combo), 3);
    option->set_ui_item_from_option();
    EXPECT_EQ(gtk_combo_box_get_active(combo), 3);
    gtk_combo_box_set_active(combo, 1);
    item->set_choices(*option, {0, 3}, 0);
    item->set_choices(*option, {0, 1, 2, 3}, 0);
    EXPECT_EQ(gtk_combo_box_get_active(combo), 0);
    option->set_ui_item({});
}

TEST(OptionGtkUI, report_chart_controls)
{
    Book book;
    auto values = scm_c_eval_string(
        "(use-modules (gnucash engine) (gnucash app-utils) (gnucash reports standard asset-allocation))"
        "(define allocation-chart-test-options (allocation-options-generator #:targets? #t))"
        "(map (lambda (name) (gnc-lookup-option allocation-chart-test-options \"Chart\" name))"
        " '(\"Time Frame\" \"Measure\" \"Chart Type\" \"Units\"))");
    scm_gc_protect_object(values);
    auto window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    auto grid = gtk_grid_new();
    gtk_container_add(GTK_CONTAINER(window), grid);
    std::vector<GncOption*> options;
    for (auto remaining = values; scm_is_pair(remaining); remaining = scm_cdr(remaining))
    {
        auto option = static_cast<GncOption*>(scm_to_pointer(scm_car(remaining)));
        GncOptionUIFactory::create(*option, GTK_GRID(grid), options.size());
        options.push_back(option);
    }
    for (auto index : {1, 0, 2, 3})
        gnc_option_changed_option_cb(nullptr, options[index]);
    auto combo = [&options](size_t index) {
        auto item = dynamic_cast<GncOptionGtkUIItem*>(options[index]->get_ui_item());
        return GTK_COMBO_BOX(item->get_widget());
    };
    EXPECT_EQ(gtk_tree_model_iter_n_children(gtk_combo_box_get_model(combo(2)), nullptr), 4);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 1);
    EXPECT_TRUE(gtk_widget_get_sensitive(GTK_WIDGET(combo(2))));
    EXPECT_FALSE(gtk_widget_get_sensitive(GTK_WIDGET(combo(3))));
    auto unit_model = gtk_combo_box_get_model(combo(3));
    GtkTreeIter unit_iter;
    ASSERT_TRUE(gtk_tree_model_iter_nth_child(unit_model, &unit_iter, nullptr, 1));
    gchar* unit_label;
    gtk_tree_model_get(unit_model, &unit_iter, 0, &unit_label, -1);
    EXPECT_STREQ(unit_label, "Currency");
    g_free(unit_label);
    gtk_combo_box_set_active(combo(1), 1);
    EXPECT_EQ(gtk_tree_model_iter_n_children(gtk_combo_box_get_model(combo(2)), nullptr), 2);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 0);
    EXPECT_TRUE(gtk_widget_get_sensitive(GTK_WIDGET(combo(3))));
    for (auto option : options)
        option->set_ui_item({});
    gtk_widget_destroy(window);
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    grid = gtk_grid_new();
    gtk_container_add(GTK_CONTAINER(window), grid);
    for (size_t index = 0; index < options.size(); ++index)
        GncOptionUIFactory::create(*options[index], GTK_GRID(grid), index);
    for (auto index : {1, 0, 2, 3})
        gnc_option_changed_option_cb(nullptr, options[index]);
    EXPECT_EQ(gtk_combo_box_get_active(combo(1)), 0);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 1);
    gtk_combo_box_set_active(combo(2), 2);
    gtk_combo_box_set_active(combo(0), 1);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 2);
    auto chart_item = dynamic_cast<GncOptionGtkUIItem*>(options[2]->get_ui_item());
    EXPECT_TRUE(scm_is_eq(chart_item->get_widget_scm_value(*options[2]), scm_from_utf8_symbol("line")));
    gtk_combo_box_set_active(combo(0), 0);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 1);
    gtk_combo_box_set_active(combo(2), 0);
    gtk_combo_box_set_active(combo(0), 1);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 0);
    gtk_combo_box_set_active(combo(2), 1);
    gtk_combo_box_set_active(combo(0), 0);
    gtk_combo_box_set_active(combo(2), 3);
    gtk_combo_box_set_active(combo(1), 1);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 1);
    gtk_combo_box_set_active(combo(0), 1);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 1);
    gtk_combo_box_set_active(combo(2), 3);
    gtk_combo_box_set_active(combo(0), 0);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 0);
    gtk_combo_box_set_active(combo(1), 0);
    EXPECT_EQ(gtk_combo_box_get_active(combo(2)), 0);
    gtk_combo_box_set_active(combo(1), 1);
    gtk_combo_box_set_active(combo(2), 1);
    for (auto option : options)
        option->set_option_from_ui_item();
    EXPECT_TRUE(options[1]->get_value<std::string>() == "drift");
    EXPECT_TRUE(options[2]->get_value<std::string>() == "stacked-bar");
    for (auto option : options)
        option->set_ui_item({});
    gtk_widget_destroy(window);
    scm_gc_unprotect_object(values);
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