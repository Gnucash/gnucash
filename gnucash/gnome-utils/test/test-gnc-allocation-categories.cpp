#include "test-option-ui.hpp"

namespace
{
TEST(AllocationCategories, account_payloads)
{
    Book book;
    auto parent = Book::id(book.parent);
    auto child = Book::id(book.child);
    auto explicit_payload = "(explicit (\"Bank\" \"" + parent + "\"))";
    Editor editor{GncOptionUIType::ACCOUNT_CATEGORIES, explicit_payload};
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_EQ(gtk_tree_view_get_n_columns(editor.trees[0]), 2u);
    EXPECT_TRUE(editor.save() == explicit_payload);
    editor.load("((\"Bank\" \"" + parent + "\"))");
    auto migrated = editor.save();
    EXPECT_TRUE(migrated.find(parent) != std::string::npos);
    EXPECT_TRUE(migrated.find(child) != std::string::npos);
    EXPECT_TRUE(migrated.starts_with("(explicit "));
    editor.edit(0, "0", "  Cash  ");
    EXPECT_TRUE(editor.save().find("\"Cash\"") != std::string::npos);
    for (const auto& payload : {
        std::string{"(explicit (\"Empty\"))"},
        "(explicit-targets (\"Bank\" 80 \"" + parent + "\") (\"Empty\" 20))"})
    {
        editor.load(payload);
        EXPECT_TRUE(editor.item->is_valid());
        EXPECT_TRUE(editor.warning().empty());
        auto saved = editor.save();
        editor.load(saved);
        EXPECT_TRUE(editor.save() == saved);
    }
    for (const auto& payload : {
        std::string{"(explicit)"},
        "(explicit (\" \" \"" + parent + "\"))",
        "(explicit (\"Cash\" \"" + parent + "\") (\"Cash\" \"" + child + "\"))",
        "(explicit (\"One\" \"" + parent + "\") (\"Two\" \"" + parent + "\"))",
        std::string{"(explicit (\"Deleted\" \"0123456789abcdef0123456789abcdef\"))"},
        std::string{"("}, std::string{"#.(error \"never evaluate\")"}})
    {
        editor.load(payload);
        EXPECT_FALSE(editor.item->is_valid());
    }
    editor.load(explicit_payload);
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().empty());
    editor.load("(explicit-targets (\"Bank\" 0 \"" + parent + "\") (\"Savings\" #f \"" + child + "\"))");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().find("Some categories have no target weight.") != std::string::npos);
    auto targets = editor.save();
    EXPECT_TRUE(targets.find("0.0") != std::string::npos);
    EXPECT_TRUE(targets.find("#f") != std::string::npos);
    editor.edit(1, "1", "100");
    EXPECT_TRUE(editor.warning().empty());
    targets = editor.save();
    editor.load(targets);
    EXPECT_TRUE(editor.save() == targets);
    for (const auto* target : {"-1", "101", "nan", "inf", "no", "50%"})
    {
        editor.edit(1, "0", target);
        EXPECT_FALSE(editor.item->is_valid());
        EXPECT_TRUE(editor.save() == targets);
    }
    editor.edit(1, "0", "60");
    editor.edit(1, "1", "30");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().find("Targets total") != std::string::npos);
    editor.edit(1, "1", "40");
    EXPECT_TRUE(editor.warning().empty());
    editor.edit(1, "0", "");
    editor.edit(1, "1", "");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().empty());
    EXPECT_TRUE(editor.save().starts_with("(explicit "));
}

TEST(AllocationCategories, security_targets)
{
    Book book;
    Editor editor{GncOptionUIType::SECURITY_CATEGORIES,
        "(explicit-targets (\"Cash\" 0 \"CURRENCY::USD\") (\"Other\" #f))"};
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_EQ(gtk_tree_view_get_n_columns(editor.trees[0]), 2u);
    auto saved = editor.save();
    EXPECT_TRUE(saved.find("#f") != std::string::npos);
    EXPECT_TRUE(saved.find("0.0") != std::string::npos);
    EXPECT_TRUE(editor.warning().find("Some categories have no target weight.") != std::string::npos);
    auto column = gtk_tree_view_get_column(editor.trees[0], 1);
    auto model = gtk_tree_view_get_model(editor.trees[0]);
    GtkTreeIter iter;
    ASSERT_TRUE(gtk_tree_model_get_iter_first(model, &iter));
    gtk_tree_view_column_cell_set_cell_data(column, model, &iter, FALSE, FALSE);
    auto cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(column));
    gchar* display = nullptr;
    g_object_get(cells->data, "text", &display, nullptr);
    EXPECT_STREQ(display, "0%");
    g_free(display);
    auto entry = gtk_entry_new();
    g_object_ref_sink(entry);
    g_signal_emit_by_name(cells->data, "editing-started", entry, "0");
    EXPECT_STREQ(gtk_entry_get_text(GTK_ENTRY(entry)), "0");
    g_signal_emit_by_name(cells->data, "editing-started", entry, "1");
    EXPECT_STREQ(gtk_entry_get_text(GTK_ENTRY(entry)), "");
    EXPECT_TRUE(gtk_tree_model_iter_next(model, &iter));
    gtk_tree_view_column_cell_set_cell_data(column, model, &iter, FALSE, FALSE);
    g_object_get(cells->data, "text", &display, nullptr);
    EXPECT_STREQ(display, "");
    g_free(display);
    EXPECT_TRUE(editor.save().find("Not set") == std::string::npos);
    gtk_widget_destroy(entry);
    g_object_unref(entry);
    g_list_free(cells);
    for (const auto* target : {"-1", "101", "nan", "inf", "no", "50%"})
    {
        editor.edit(1, "0", target);
        EXPECT_FALSE(editor.item->is_valid());
        auto previous = editor.option.get_value<std::string>();
        EXPECT_TRUE(editor.save() == previous);
    }
    for (const auto* target : {"", "0", "100", "33.333333333333336"})
    {
        editor.edit(1, "0", target);
        EXPECT_TRUE(editor.item->is_valid());
        saved = editor.save();
        editor.load(saved);
        EXPECT_TRUE(editor.item->is_valid());
        EXPECT_TRUE(editor.save() == saved);
    }
    editor.load("(explicit)");
    EXPECT_TRUE(editor.item->is_valid());
    editor.load("(explicit-targets (\"Malformed\" invalid \"CURRENCY::USD\"))");
    EXPECT_FALSE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().find("Enter a target percentage") != std::string::npos);
    editor.load("(explicit-targets (\"Cash\" 100 \"CURRENCY::USD\") (\"Other\" #f))");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().find("Some categories have no target weight.") != std::string::npos);
    EXPECT_TRUE(editor.warning().find("use 0% where intended") != std::string::npos);
    EXPECT_TRUE(editor.warning().find("Target weights are only used when measuring drift from actual allocation.") != std::string::npos);
    EXPECT_TRUE(editor.warning().find("Targets total") == std::string::npos);
    for (const auto* payload : {
        "(explicit-targets (\"Cash\" 60 \"CURRENCY::USD\") (\"Other\" 30))",
        "(explicit-targets (\"Cash\" 80 \"CURRENCY::USD\") (\"Other\" 30))",
        "(explicit-targets (\"Cash\" 0 \"CURRENCY::USD\") (\"Other\" 0))"})
    {
        editor.load(payload);
        EXPECT_TRUE(editor.item->is_valid());
        EXPECT_TRUE(editor.warning().find("Targets total") != std::string::npos);
        EXPECT_TRUE(editor.warning().find("Adjust target weights to total 100%") != std::string::npos);
        EXPECT_TRUE(editor.warning().find("Target weights are only used when measuring drift from actual allocation.") != std::string::npos);
        EXPECT_TRUE(editor.warning().find("Some categories") == std::string::npos);
    }
    editor.load("(explicit-targets (\"Cash\" 50.00001 \"CURRENCY::USD\") (\"Other\" 50))");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().find("Targets total 100.000010%") != std::string::npos);
    editor.load("(explicit-targets (\"Cash\" 49.99999 \"CURRENCY::USD\") (\"Other\" 50))");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().find("Targets total 99.999990%") != std::string::npos);
    editor.load("(explicit-targets (\"Cash\" 50.0000001 \"CURRENCY::USD\") (\"Other\" 50))");
    EXPECT_TRUE(editor.warning().empty());
    editor.load("(explicit-targets (\"Cash\" #f \"CURRENCY::USD\") (\"Other\" #f))");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().empty());
    editor.load("(explicit-targets (\"Cash\" 100 \"CURRENCY::USD\") (\"Other\" 0))");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().empty());
    editor.load("(explicit (\"Gone\" \"NASDAQ::MISSING\"))");
    EXPECT_FALSE(editor.item->is_valid());
    editor.load("(explicit (\"One\" \"CURRENCY::USD\") (\"Two\" \"CURRENCY::USD\"))");
    EXPECT_FALSE(editor.item->is_valid());
}

TEST(AllocationCategories, account_controls)
{
    Book book;
    auto parent = Book::id(book.parent);
    auto child = Book::id(book.child);
    Editor editor{GncOptionUIType::ACCOUNT_CATEGORIES, "(explicit)"};
    EXPECT_EQ(gtk_tree_view_get_n_columns(editor.trees[0]), 2u);
    editor.click("Remove");
    editor.click("Rename");
    EXPECT_TRUE(editor.key(GDK_KEY_Up));
    editor.click("Add");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().empty());
    auto accounts = g_list_prepend(nullptr, book.parent);
    gnc_tree_view_account_set_selected_accounts(GNC_TREE_VIEW_ACCOUNT(editor.trees[1]), accounts, TRUE);
    g_list_free(accounts);
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.save().find("\"Bank\"") != std::string::npos);
    EXPECT_TRUE(editor.save().find(child) == std::string::npos);
    editor.click("Select Children");
    EXPECT_TRUE(editor.save().find(child) != std::string::npos);
    editor.edit(0, "0", "Custom");
    editor.click("Clear All", 1);
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.warning().empty());
    editor.click("Select All");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.save().find("\"Custom\"") != std::string::npos);
    editor.click("Add");
    EXPECT_FALSE(editor.member_sensitive(parent));
    EXPECT_FALSE(editor.member_sensitive(child));
    editor.click("Select All");
    auto selected = gnc_tree_view_account_get_selected_accounts(GNC_TREE_VIEW_ACCOUNT(editor.trees[1]));
    EXPECT_EQ(selected, nullptr);
    editor.click("Add");
    EXPECT_TRUE(editor.save().find("\"New Category 2\"") != std::string::npos);
    editor.click("Remove");
    editor.select(0, 1);
    editor.click("Remove");
    EXPECT_TRUE(editor.item->is_valid());
    editor.click("Clear All");
    EXPECT_TRUE(editor.save() == "(explicit)");
    EXPECT_FALSE(editor.item->is_valid());
    editor.click("Select All");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.save().find(parent) != std::string::npos);
    editor.load("(explicit-targets (\"Legacy\" 100 \"" + parent + "\"))");
    EXPECT_EQ(gtk_tree_view_get_n_columns(editor.trees[0]), 2u);
    EXPECT_TRUE(editor.save().find("explicit-targets") != std::string::npos);
    EXPECT_TRUE(editor.save().find("100.0") != std::string::npos);
    auto view = GNC_TREE_VIEW_ACCOUNT(editor.trees[1]);
    auto sensitive = +[](Account* account, gpointer data) -> gboolean { return account != data; };
    gnc_tree_view_account_set_sensitive_func(view, sensitive, book.parent);
    EXPECT_TRUE(editor.member_sensitive(child));
    EXPECT_FALSE(editor.member_sensitive(parent));
    gnc_tree_view_account_set_sensitive_func(view, nullptr, nullptr);
    EXPECT_TRUE(editor.member_sensitive(parent));
    EXPECT_TRUE(editor.member_sensitive(child));
    gnc_tree_view_account_set_sensitive_func(view, sensitive, book.parent);
    EXPECT_FALSE(editor.member_sensitive(parent));
    gnc_tree_view_account_set_sensitive_func(view, nullptr, nullptr);
    EXPECT_TRUE(editor.member_sensitive(parent));
}

TEST(AllocationCategories, category_keyboard)
{
    Book book;
    Editor editor{GncOptionUIType::SECURITY_CATEGORIES,
        "(explicit-targets (\"First\" 60) (\"Second\" 40))"};
    auto before = gtk_button_new_with_label("Before categories");
    auto after = gtk_button_new_with_label("After categories");
    gtk_grid_attach(GTK_GRID(editor.grid), before, 0, -1, 1, 1);
    gtk_grid_attach(GTK_GRID(editor.grid), after, 0, 1, 1, 1);
    gtk_widget_show_all(editor.window);
    gtk_container_check_resize(GTK_CONTAINER(editor.window));
    auto activation = gdk_event_new(GDK_FOCUS_CHANGE);
    activation->focus_change.window = GDK_WINDOW(g_object_ref(gtk_widget_get_window(editor.window)));
    activation->focus_change.in = TRUE;
    gtk_widget_send_focus_change(editor.window, activation);
    gdk_event_free(activation);
    auto start_edit = [&](int row, int column) {
        auto path = gtk_tree_path_new_from_indices(row, -1);
        gtk_widget_grab_focus(GTK_WIDGET(editor.trees[0]));
        gtk_tree_view_set_cursor(editor.trees[0], path,
                                 gtk_tree_view_get_column(editor.trees[0], column), TRUE);
        gtk_tree_path_free(path);
    };
    auto press_key = [&](guint keyval, GdkModifierType modifiers = static_cast<GdkModifierType>(0)) {
        auto event = gdk_event_new(GDK_KEY_PRESS);
        event->key.window = GDK_WINDOW(g_object_ref(gtk_widget_get_window(editor.window)));
        event->key.send_event = TRUE;
        event->key.keyval = keyval;
        event->key.state = modifiers;
        GdkKeymapKey* keys = nullptr;
        gint count = 0;
        ASSERT_TRUE(gdk_keymap_get_entries_for_keyval(
            gdk_keymap_get_for_display(gtk_widget_get_display(editor.window)), keyval, &keys, &count));
        ASSERT_GT(count, 0);
        event->key.hardware_keycode = keys[0].keycode;
        event->key.group = keys[0].group;
        g_free(keys);
        gdk_event_set_device(event, gdk_seat_get_keyboard(
            gdk_display_get_default_seat(gtk_widget_get_display(editor.window))));
        gtk_widget_event(editor.window, event);
        gdk_event_free(event);
    };
    auto active_entry = [&](int row, int column, const char* text) {
        auto focus = gtk_window_get_focus(GTK_WINDOW(editor.window));
        if (!GTK_IS_ENTRY(focus))
            throw std::runtime_error{"Category editor has no active entry"};
        EXPECT_TRUE(gtk_widget_is_ancestor(focus, GTK_WIDGET(editor.trees[0])));
        EXPECT_STREQ(gtk_entry_get_text(GTK_ENTRY(focus)), text);
        GtkTreePath* path = nullptr;
        GtkTreeViewColumn* current = nullptr;
        gtk_tree_view_get_cursor(editor.trees[0], &path, &current);
        if (!path)
            throw std::runtime_error{"Category editor has no active row"};
        EXPECT_EQ(gtk_tree_path_get_indices(path)[0], row);
        EXPECT_TRUE(current == gtk_tree_view_get_column(editor.trees[0], column));
        gtk_tree_path_free(path);
        return GTK_ENTRY(focus);
    };
    start_edit(0, 0);
    active_entry(0, 0, "First");
    press_key(GDK_KEY_Tab);
    gtk_entry_set_text(active_entry(0, 1, "60"), "55");
    press_key(GDK_KEY_Tab);
    gtk_entry_set_text(active_entry(1, 0, "Second"), "Updated Second");
    press_key(GDK_KEY_Tab);
    active_entry(1, 1, "40");
    press_key(GDK_KEY_ISO_Left_Tab, GDK_SHIFT_MASK);
    active_entry(1, 0, "Updated Second");
    press_key(GDK_KEY_Tab, GDK_SHIFT_MASK);
    active_entry(0, 1, "55");
    press_key(GDK_KEY_ISO_Left_Tab, GDK_SHIFT_MASK);
    active_entry(0, 0, "First");
    press_key(GDK_KEY_ISO_Left_Tab, GDK_SHIFT_MASK);
    for (int step = 0; step < 3; ++step)
    {
        auto focus = gtk_window_get_focus(GTK_WINDOW(editor.window));
        ASSERT_NE(focus, nullptr);
        if (focus != GTK_WIDGET(editor.trees[0]) &&
            !gtk_widget_is_ancestor(focus, GTK_WIDGET(editor.trees[0])))
            break;
        EXPECT_FALSE(GTK_IS_ENTRY(focus));
        press_key(GDK_KEY_ISO_Left_Tab, GDK_SHIFT_MASK);
    }
    EXPECT_TRUE(gtk_window_get_focus(GTK_WINDOW(editor.window)) == before);
    press_key(GDK_KEY_Tab);
    active_entry(0, 0, "First");
    press_key(GDK_KEY_ISO_Left_Tab, GDK_SHIFT_MASK);
    EXPECT_TRUE(gtk_window_get_focus(GTK_WINDOW(editor.window)) == before);
    start_edit(1, 1);
    gtk_entry_set_text(active_entry(1, 1, "40"), "45");
    press_key(GDK_KEY_Tab);
    for (int step = 0; step < 3; ++step)
    {
        auto focus = gtk_window_get_focus(GTK_WINDOW(editor.window));
        ASSERT_NE(focus, nullptr);
        if (focus != GTK_WIDGET(editor.trees[0]) &&
            !gtk_widget_is_ancestor(focus, GTK_WIDGET(editor.trees[0])))
            break;
        EXPECT_FALSE(GTK_IS_ENTRY(focus));
        press_key(GDK_KEY_Tab);
    }
    auto focus = gtk_window_get_focus(GTK_WINDOW(editor.window));
    ASSERT_NE(focus, nullptr);
    EXPECT_FALSE(focus == GTK_WIDGET(editor.trees[0]));
    EXPECT_FALSE(gtk_widget_is_ancestor(focus, GTK_WIDGET(editor.trees[0])));
    ASSERT_TRUE(GTK_IS_BUTTON(focus));
    EXPECT_STREQ(gtk_button_get_label(GTK_BUTTON(focus)), "Add");
    EXPECT_TRUE(gtk_widget_has_focus(focus));
    press_key(GDK_KEY_ISO_Left_Tab, GDK_SHIFT_MASK);
    active_entry(1, 1, "45");
    press_key(GDK_KEY_Tab);
    EXPECT_TRUE(gtk_window_get_focus(GTK_WINDOW(editor.window)) == focus);
    auto saved = editor.save();
    EXPECT_TRUE(saved.find("First\" 55.0") != std::string::npos);
    EXPECT_TRUE(saved.find("Updated Second\" 45.0") != std::string::npos);
    start_edit(0, 1);
    gtk_entry_set_text(active_entry(0, 1, "55"), "99");
    press_key(GDK_KEY_Escape);
    EXPECT_TRUE(editor.save() == saved);
    start_edit(0, 1);
    gtk_entry_set_text(active_entry(0, 1, "55"), "70");
    gtk_widget_grab_focus(after);
    EXPECT_TRUE(editor.save().find("First\" 70.0") != std::string::npos);
    start_edit(0, 0);
    gtk_entry_set_text(active_entry(0, 0, "First"), "Renamed before Apply");
    gtk_widget_grab_focus(after);
    EXPECT_TRUE(editor.save().find("Renamed before Apply\" 70.0") != std::string::npos);
    auto applied = editor.option.get_value<std::string>();
    start_edit(0, 1);
    gtk_entry_set_text(active_entry(0, 1, "70"), "80");
    gtk_widget_grab_focus(after);
    EXPECT_TRUE(editor.option.get_value<std::string>() == applied);
    editor.load(applied);
    start_edit(1, 0);
    gtk_entry_set_text(active_entry(1, 0, "Updated Second"), "Final Second");
    press_key(GDK_KEY_Return);
    EXPECT_TRUE(editor.save().find("Final Second\" 45.0") != std::string::npos);
    editor.click("Add");
    start_edit(2, 0);
    gtk_entry_set_text(active_entry(2, 0, "New Category"), "Precious Metals");
    gtk_widget_grab_focus(focus);
    editor.click("Add");
    EXPECT_TRUE(editor.save().find("Precious Metals") != std::string::npos);
    EXPECT_EQ(gtk_tree_model_iter_n_children(gtk_tree_view_get_model(editor.trees[0]), nullptr), 4);
    start_edit(3, 1);
    gtk_entry_set_text(active_entry(3, 1, ""), "0");
    gtk_widget_grab_focus(focus);
    editor.click("Add");
    EXPECT_TRUE(editor.save().find("New Category\" 0.0") != std::string::npos);
    EXPECT_EQ(gtk_tree_model_iter_n_children(gtk_tree_view_get_model(editor.trees[0]), nullptr), 5);
}

TEST(AllocationCategories, target_first_key)
{
    Book book;
    for (auto type : {GncOptionUIType::ACCOUNT_CATEGORIES, GncOptionUIType::SECURITY_CATEGORIES})
    {
        auto member = type == GncOptionUIType::ACCOUNT_CATEGORIES
            ? Book::id(book.parent) : "CURRENCY::USD";
        auto other = type == GncOptionUIType::ACCOUNT_CATEGORIES
            ? " \"" + Book::id(book.child) + "\"" : std::string{};
        Editor editor{type, "(explicit (\"Cash\" \"" + member + "\") (\"Other\"" + other + "))"};
        gtk_container_check_resize(GTK_CONTAINER(editor.window));
        auto tree = editor.trees[0];
        auto activation = gdk_event_new(GDK_FOCUS_CHANGE);
        activation->focus_change.window = GDK_WINDOW(g_object_ref(gtk_widget_get_window(editor.window)));
        activation->focus_change.in = TRUE;
        gtk_widget_send_focus_change(editor.window, activation);
        gdk_event_free(activation);
        auto path = gtk_tree_path_new_from_indices(1, -1);
        GdkRectangle area;
        gtk_tree_view_get_cell_area(tree, path, gtk_tree_view_get_column(tree, 1), &area);
        gtk_tree_path_free(path);
        for (auto event_type : {GDK_BUTTON_PRESS, GDK_BUTTON_RELEASE})
        {
            auto event = gdk_event_new(event_type);
            event->button.window = GDK_WINDOW(g_object_ref(gtk_tree_view_get_bin_window(tree)));
            event->button.button = 1;
            event->button.x = area.x + area.width / 2;
            event->button.y = area.y + area.height / 2;
            gdk_event_set_device(event, gdk_seat_get_pointer(
                gdk_display_get_default_seat(gtk_widget_get_display(editor.window))));
            gtk_widget_event(GTK_WIDGET(tree), event);
            gdk_event_free(event);
        }
        auto entry = gtk_window_get_focus(GTK_WINDOW(editor.window));
        ASSERT_TRUE(GTK_IS_ENTRY(entry));
        EXPECT_TRUE(gtk_widget_is_ancestor(entry, GTK_WIDGET(tree)));
        g_object_set(entry, "im-module", "gtk-im-context-simple", nullptr);
        auto key = gdk_event_new(GDK_KEY_PRESS);
        key->key.window = GDK_WINDOW(g_object_ref(gtk_widget_get_window(editor.window)));
        key->key.keyval = GDK_KEY_3;
        GdkKeymapKey* keys = nullptr;
        gint count = 0;
        ASSERT_TRUE(gdk_keymap_get_entries_for_keyval(
            gdk_keymap_get_for_display(gtk_widget_get_display(editor.window)), GDK_KEY_3, &keys, &count));
        ASSERT_GT(count, 0);
        key->key.hardware_keycode = keys[0].keycode;
        key->key.group = keys[0].group;
        g_free(keys);
        gdk_event_set_device(key, gdk_seat_get_keyboard(
            gdk_display_get_default_seat(gtk_widget_get_display(editor.window))));
        gtk_widget_event(editor.window, key);
        gdk_event_free(key);
        entry = gtk_window_get_focus(GTK_WINDOW(editor.window));
        ASSERT_TRUE(GTK_IS_ENTRY(entry));
        EXPECT_TRUE(gtk_widget_is_ancestor(entry, GTK_WIDGET(tree)));
        EXPECT_STREQ(gtk_entry_get_text(GTK_ENTRY(entry)), "3");
        gtk_widget_grab_focus(GTK_WIDGET(tree));
        EXPECT_TRUE(editor.save().find("Other\" 3.0") != std::string::npos);
    }
}

TEST(AllocationCategories, category_order)
{
    Book book;
    Editor editor{GncOptionUIType::SECURITY_CATEGORIES,
        "(explicit-targets (\"First\" 60) (\"Second\" 40))"};
    auto original = editor.save();
    EXPECT_TRUE(editor.key(GDK_KEY_Up));
    EXPECT_TRUE(editor.save() == original);
    EXPECT_TRUE(editor.key(GDK_KEY_Down));
    auto reversed = editor.save();
    EXPECT_TRUE(reversed.find("Second") < reversed.find("First"));
    EXPECT_TRUE(reversed.find("Second\" 40.0") != std::string::npos);
    EXPECT_TRUE(editor.key(GDK_KEY_Down));
    EXPECT_TRUE(editor.save() == reversed);
    EXPECT_TRUE(editor.key(GDK_KEY_KP_Up));
    EXPECT_TRUE(editor.save() == original);
    EXPECT_TRUE(editor.key(GDK_KEY_KP_Down));
    EXPECT_TRUE(editor.save() == reversed);
    g_signal_connect(editor.trees[0], "drag-begin",
        G_CALLBACK(+[](GtkWidget* widget, GdkDragContext*, void*) {
            g_signal_stop_emission_by_name(widget, "drag-begin");
        }), nullptr);
    g_signal_connect(editor.trees[0], "drag-end",
        G_CALLBACK(+[](GtkWidget* widget, GdkDragContext*, void*) {
            g_signal_stop_emission_by_name(widget, "drag-end");
        }), nullptr);
    g_signal_emit_by_name(editor.trees[0], "drag-begin", nullptr);
    gint order[]{1, 0};
    gtk_list_store_reorder(GTK_LIST_STORE(gtk_tree_view_get_model(editor.trees[0])), order);
    g_signal_emit_by_name(editor.trees[0], "drag-end", nullptr);
    EXPECT_TRUE(editor.save() == original);
    editor.select(0, 0);
    editor.click("Rename");
    auto entry = gtk_window_get_focus(GTK_WINDOW(editor.window));
    ASSERT_TRUE(GTK_IS_ENTRY(entry));
    gpointer pending_editor = entry;
    g_object_add_weak_pointer(G_OBJECT(entry), &pending_editor);
    gtk_entry_set_text(GTK_ENTRY(entry), " Renamed ");
    gtk_widget_grab_focus(GTK_WIDGET(editor.trees[1]));
    EXPECT_TRUE(editor.save().find("\"Renamed\"") != std::string::npos);
    while (gtk_events_pending())
        gtk_main_iteration();
    EXPECT_EQ(pending_editor, nullptr);
    EXPECT_TRUE(gtk_window_get_focus(GTK_WINDOW(editor.window)) == GTK_WIDGET(editor.trees[1]));
    editor.click("Clear All");
    EXPECT_TRUE(editor.item->is_valid());
    editor.click("Add");
    editor.click("Remove");
    EXPECT_TRUE(editor.save() == "(explicit)");
}

TEST(AllocationCategories, security_controls)
{
    Book book;
    auto table = gnc_commodity_table_get_table(book.book);
    for (const auto* symbol : {"FIRST", "SECOND"})
        gnc_commodity_table_insert(table,
            gnc_commodity_new(book.book, symbol, "NASDAQ", symbol, "", 100));
    Editor editor{GncOptionUIType::SECURITY_CATEGORIES, "(explicit)"};
    editor.select(1, 1);
    EXPECT_TRUE(editor.save() == "(explicit)");
    editor.click("Select Children");
    auto saved = editor.save();
    EXPECT_TRUE(saved.find("NASDAQ::FIRST") != std::string::npos);
    EXPECT_TRUE(saved.find("NASDAQ::SECOND") != std::string::npos);
    EXPECT_TRUE(saved.find("CURRENCY::USD") == std::string::npos);
    editor.click("Add");
    EXPECT_FALSE(editor.member_sensitive("NASDAQ::FIRST"));
    EXPECT_TRUE(editor.member_sensitive("CURRENCY::USD"));
    editor.click("Select All");
    saved = editor.save();
    EXPECT_TRUE(saved.find("CURRENCY::USD") != std::string::npos);
    auto first = saved.find("NASDAQ::FIRST");
    EXPECT_TRUE(saved.find("NASDAQ::FIRST", first + 1) == std::string::npos);
    editor.click("Select Children");
    EXPECT_TRUE(editor.save() == saved);
    editor.click("Clear All", 1);
    EXPECT_TRUE(editor.item->is_valid());
    editor.click("Select All");
    EXPECT_TRUE(editor.save() == saved);
}

TEST(AllocationCategories, currency_sources)
{
    Book book;
    auto table = gnc_commodity_table_get_table(book.book);
    auto currency = [&](const char* symbol) {
        auto result = gnc_commodity_table_lookup(table, "CURRENCY", symbol);
        if (!result)
            result = gnc_commodity_table_insert(table,
                gnc_commodity_new(book.book, symbol, "CURRENCY", symbol, "", 100));
        return result;
    };
    auto euro = currency("EUR");
    auto pound = currency("GBP");
    currency("JPY");
    auto transaction = xaccMallocTransaction(book.book);
    xaccTransBeginEdit(transaction);
    xaccTransSetCurrency(transaction, euro);
    int amount = 1;
    for (auto account : {book.parent, book.child})
    {
        auto split = xaccMallocSplit(book.book);
        xaccSplitSetParent(split, transaction);
        xaccSplitSetAccount(split, account);
        xaccSplitSetAmount(split, gnc_numeric_create(amount, 1));
        xaccSplitSetValue(split, gnc_numeric_create(amount, 1));
        amount = -amount;
    }
    xaccTransCommitEdit(transaction);
    auto price = gnc_price_create(book.book);
    gnc_price_begin_edit(price);
    gnc_price_set_commodity(price, book.currency);
    gnc_price_set_currency(price, pound);
    gnc_price_set_time64(price, 1);
    gnc_price_set_value(price, gnc_numeric_create(1, 1));
    gnc_price_commit_edit(price);
    EXPECT_TRUE(gnc_pricedb_add_price(gnc_pricedb_get_db(book.book), price));
    gnc_price_unref(price);
    Editor editor{GncOptionUIType::SECURITY_CATEGORIES, "(explicit)"};
    auto listed = [&editor](const char* id) {
        struct Search { const char* id; bool found; } search{id, false};
        gtk_tree_model_foreach(gtk_tree_view_get_model(editor.trees[1]),
            +[](GtkTreeModel* model, GtkTreePath*, GtkTreeIter* iter, void* data) -> gboolean {
                auto search = static_cast<Search*>(data);
                gchar* id;
                gtk_tree_model_get(model, iter, 3, &id, -1);
                search->found = g_strcmp0(id, search->id) == 0;
                g_free(id);
                return search->found;
            }, &search);
        return search.found;
    };
    EXPECT_TRUE(listed("CURRENCY::USD"));
    EXPECT_TRUE(listed("CURRENCY::EUR"));
    EXPECT_TRUE(listed("CURRENCY::GBP"));
    EXPECT_FALSE(listed("CURRENCY::JPY"));
    editor.load("(explicit (\"Saved currency\" \"CURRENCY::JPY\"))");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(listed("CURRENCY::JPY"));
    auto saved = editor.save();
    editor.load(saved);
    EXPECT_TRUE(editor.save() == saved);
    editor.load("(explicit 7 (#f) (\"Cash\" 7 \"CURRENCY::USD\" \"CURRENCY::USD\"))");
    EXPECT_TRUE(editor.item->is_valid());
    EXPECT_TRUE(editor.save() == "(explicit (\"Cash\" \"CURRENCY::USD\"))");
}

TEST(AllocationCategories, category_tooltips)
{
    Book book;
    Editor editor{GncOptionUIType::SECURITY_CATEGORIES,
        "(explicit (\"A long category name\" \"CURRENCY::USD\"))"};
    auto tooltip = GTK_TOOLTIP(g_object_new(GTK_TYPE_TOOLTIP, nullptr));
    auto path = gtk_tree_path_new_from_indices(0, -1);
    gtk_tree_view_set_cursor(editor.trees[0], path, nullptr, FALSE);
    gtk_tree_path_free(path);
    gboolean handled = FALSE;
    g_signal_emit_by_name(editor.trees[0], "query-tooltip", 0, 0, TRUE, tooltip, &handled);
    EXPECT_TRUE(handled);
    editor.click("Clear All");
    handled = TRUE;
    g_signal_emit_by_name(editor.trees[0], "query-tooltip", 0, 0, TRUE, tooltip, &handled);
    EXPECT_FALSE(handled);
    g_object_unref(tooltip);
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