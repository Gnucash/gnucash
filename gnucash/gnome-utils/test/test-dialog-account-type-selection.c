/*
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 */

/*
 * test-dialog-account-type-selection.c -- Account type selection ownership tests
 */

#include <config.h>

#include <gtk/gtk.h>

#include "Account.h"
#include "dialog-account.h"
#include "dialog-utils.h"
#include "gnc-component-manager.h"
#include "gnc-engine.h"
#include "gnc-prefs-utils.h"
#include "gnc-session.h"
#include "gnc-tree-model-account-types.h"
#include "gnc-tree-view-account.h"
#include "qof.h"

static void
object_finalized (gpointer data, GObject *object)
{
    gboolean *finalized = data;

    *finalized = TRUE;
    (void)object;
}

static void
drain_main_context (void)
{
    while (g_main_context_pending (NULL))
        g_main_context_iteration (NULL, FALSE);
}

static GtkDropDown *
find_account_type_dropdown (GtkWidget *widget)
{
    if (GTK_IS_DROP_DOWN (widget))
    {
        GListModel *model = gtk_drop_down_get_model (GTK_DROP_DOWN (widget));
        GObject *item = model ? g_list_model_get_item (model, 0) : NULL;
        gboolean is_account_type_dropdown = GNC_IS_ACCOUNT_TYPE_ITEM (item);

        g_clear_object (&item);
        if (is_account_type_dropdown)
            return GTK_DROP_DOWN (widget);
    }

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GtkDropDown *dropdown = find_account_type_dropdown (child);

        if (dropdown)
            return dropdown;
    }
    return NULL;
}

static GncTreeViewAccount *
find_account_parent_view (GtkWidget *widget, Account *selected_account)
{
    if (GNC_IS_TREE_VIEW_ACCOUNT (widget) &&
        gnc_tree_view_account_get_selected_account (
            GNC_TREE_VIEW_ACCOUNT (widget)) == selected_account)
        return GNC_TREE_VIEW_ACCOUNT (widget);

    for (GtkWidget *child = gtk_widget_get_first_child (widget); child;
         child = gtk_widget_get_next_sibling (child))
    {
        GncTreeViewAccount *view = find_account_parent_view (child,
                                                              selected_account);

        if (view)
            return view;
    }
    return NULL;
}

static guint
find_account_type_position (GListModel *model, GNCAccountType type)
{
    for (guint position = 0; position < g_list_model_get_n_items (model);
         position++)
    {
        GncAccountTypeItem *item = GNC_ACCOUNT_TYPE_ITEM (
            g_list_model_get_item (model, position));
        gboolean found = gnc_account_type_item_get_account_type (item) == type;

        g_object_unref (item);
        if (found)
            return position;
    }
    return GTK_INVALID_LIST_POSITION;
}

static GtkWindow *
find_account_window (void)
{
    GListModel *windows = gtk_window_get_toplevels ();

    for (guint index = 0; index < g_list_model_get_n_items (windows); index++)
    {
        GtkWindow *window = g_list_model_get_item (windows, index);

        if (find_account_type_dropdown (GTK_WIDGET (window)))
            return window;
        g_object_unref (window);
    }
    return NULL;
}

static void
test_account_type_selection_owns_model_items (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    GList *valid_types = NULL;
    GtkWindow *window;
    GtkDropDown *dropdown;
    GListStore *model;
    gboolean dialog_finalized = FALSE;
    gboolean item_finalized[2] = { FALSE, FALSE };

    gnc_set_current_session (session);
    gnc_account_create_root (book);
    valid_types = g_list_append (valid_types, GINT_TO_POINTER (ACCT_TYPE_BANK));
    valid_types = g_list_append (valid_types, GINT_TO_POINTER (ACCT_TYPE_CASH));
    gnc_ui_new_account_with_types_and_commodity (NULL, book, valid_types,
                                                  NULL);
    g_list_free (valid_types);

    window = find_account_window ();
    g_assert_nonnull (window);
    dropdown = find_account_type_dropdown (GTK_WIDGET (window));
    g_assert_nonnull (dropdown);
    model = G_LIST_STORE (gtk_drop_down_get_model (dropdown));
    g_assert_nonnull (model);
    g_assert_cmpuint (g_list_model_get_n_items (G_LIST_MODEL (model)), ==,
                      G_N_ELEMENTS (item_finalized));

    for (guint index = 0; index < G_N_ELEMENTS (item_finalized); index++)
    {
        GObject *item = g_list_model_get_item (G_LIST_MODEL (model), index);

        g_assert_true (GNC_IS_ACCOUNT_TYPE_ITEM (item));
        g_object_weak_ref (item, object_finalized, &item_finalized[index]);
        g_object_unref (item);
    }

    for (guint round = 0; round < 4; round++)
        for (guint index = 0; index < G_N_ELEMENTS (item_finalized); index++)
        {
            GObject *item;

            gtk_drop_down_set_selected (dropdown, index);
            item = gtk_drop_down_get_selected_item (dropdown);
            g_assert_true (GNC_IS_ACCOUNT_TYPE_ITEM (item));
            g_assert_false (item_finalized[index]);
        }

    g_list_store_remove_all (model);
    g_assert_cmpuint (g_list_model_get_n_items (G_LIST_MODEL (model)), ==, 0);
    g_assert_null (gtk_drop_down_get_selected_item (dropdown));

    g_object_weak_ref (G_OBJECT (window), object_finalized, &dialog_finalized);
    gtk_window_destroy (window);
    g_object_unref (window);
    drain_main_context ();
    g_assert_true (dialog_finalized);
    g_assert_true (item_finalized[0]);
    g_assert_true (item_finalized[1]);

    gnc_clear_current_session ();
}

static void
test_account_type_parent_change (gboolean choose_income)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root;
    Account *income;
    GList *valid_types = NULL;
    GtkWindow *window;
    GtkDropDown *dropdown;
    GncTreeViewAccount *parent_view;
    GListModel *model;
    GNCAccountType expected = choose_income ? ACCT_TYPE_INCOME : ACCT_TYPE_BANK;
    guint position;
    GncAccountTypeItem *item;

    gnc_set_current_session (session);
    gnc_account_create_root (book);
    root = gnc_book_get_root_account (book);
    income = xaccMallocAccount (book);
    xaccAccountSetName (income, "Income parent");
    xaccAccountSetType (income, ACCT_TYPE_INCOME);
    gnc_account_append_child (root, income);
    valid_types = g_list_append (valid_types, GINT_TO_POINTER (ACCT_TYPE_BANK));
    valid_types = g_list_append (valid_types, GINT_TO_POINTER (ACCT_TYPE_INCOME));
    gnc_ui_new_account_with_types_and_commodity (NULL, book, valid_types, NULL);
    g_list_free (valid_types);

    /* The account tree applies the explicitly requested parent in its restore idle. */
    drain_main_context ();

    window = find_account_window ();
    g_assert_nonnull (window);
    dropdown = find_account_type_dropdown (GTK_WIDGET (window));
    parent_view = find_account_parent_view (GTK_WIDGET (window), root);
    g_assert_nonnull (dropdown);
    g_assert_nonnull (parent_view);
    model = gtk_drop_down_get_model (dropdown);
    position = find_account_type_position (model, ACCT_TYPE_BANK);
    g_assert_cmpuint (position, !=, GTK_INVALID_LIST_POSITION);
    gtk_drop_down_set_selected (dropdown, position);
    gnc_tree_view_account_set_selected_account (parent_view, income);
    drain_main_context ();

    model = gtk_drop_down_get_model (dropdown);
    position = find_account_type_position (model, ACCT_TYPE_NONE);
    g_assert_cmpuint (position, ==, 0);
    item = GNC_ACCOUNT_TYPE_ITEM (gtk_drop_down_get_selected_item (dropdown));
    g_assert_nonnull (item);
    g_assert_cmpint (gnc_account_type_item_get_account_type (item), ==,
                     ACCT_TYPE_NONE);

    if (choose_income)
    {
        position = find_account_type_position (model, ACCT_TYPE_INCOME);
        g_assert_cmpuint (position, !=, GTK_INVALID_LIST_POSITION);
        gtk_drop_down_set_selected (dropdown, position);
    }

    gnc_tree_view_account_set_selected_account (parent_view, root);
    drain_main_context ();
    item = GNC_ACCOUNT_TYPE_ITEM (gtk_drop_down_get_selected_item (dropdown));
    g_assert_nonnull (item);
    g_assert_cmpint (gnc_account_type_item_get_account_type (item), ==, expected);

    gtk_window_destroy (window);
    g_object_unref (window);
    drain_main_context ();
    gnc_clear_current_session ();
}

static void
test_account_type_parent_change_restores_preferred (void)
{
    test_account_type_parent_change (FALSE);
}

static void
test_account_type_parent_change_keeps_user_choice (void)
{
    test_account_type_parent_change (TRUE);
}

static void
test_account_builder_roots_include_color_dialogs (void)
{
    static const struct
    {
        const gchar *root;
        const gchar *button;
    } roots[] = {
        { "account_dialog", "color_entry_button" },
        { "account_cascade_dialog", "color_button" },
    };

    for (guint index = 0; index < G_N_ELEMENTS (roots); index++)
    {
        GtkBuilder *builder = gtk_builder_new ();
        GtkWindow *window;
        GtkColorDialogButton *button;

        g_assert_true (gnc_builder_add_from_file (builder, "dialog-account.glade",
                                                   roots[index].root));
        window = GTK_WINDOW (gtk_builder_get_object (builder, roots[index].root));
        button = GTK_COLOR_DIALOG_BUTTON (gtk_builder_get_object
                                          (builder, roots[index].button));
        g_assert_nonnull (window);
        g_assert_nonnull (button);
        g_assert_nonnull (gtk_color_dialog_button_get_dialog (button));
        gtk_window_destroy (window);
        g_object_unref (builder);
    }
}

static void
test_account_builder_container_types (void)
{
    GtkBuilder *builder = gtk_builder_new ();
    static const gchar *boxes[] = {
        "commodity_hbox",
        "higher_balance_limit_hbox",
        "lower_balance_limit_hbox",
        "opening_balance_box",
        "opening_balance_date_box",
    };

    g_assert_true (gnc_builder_add_from_file (builder, "dialog-account.glade",
                                               "account_dialog"));
    for (guint index = 0; index < G_N_ELEMENTS (boxes); index++)
        g_assert_true (GTK_IS_BOX (gtk_builder_get_object (builder, boxes[index])));
    g_assert_true (GTK_IS_SCROLLED_WINDOW (gtk_builder_get_object
                                            (builder, "parent_scroll")));
    g_assert_true (GTK_IS_SCROLLED_WINDOW (gtk_builder_get_object
                                            (builder, "transfer_account_scroll")));
    gtk_window_destroy (GTK_WINDOW (gtk_builder_get_object (builder,
                                                             "account_dialog")));
    g_object_unref (builder);
}

int
main (int argc, char **argv)
{
    int status;

    g_setenv ("GSETTINGS_BACKEND", "memory", TRUE);
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    qof_log_init_filename_special ("stderr");
    qof_log_set_level ("gnc", (QofLogLevel)G_LOG_LEVEL_DEBUG);
    gnc_engine_init_static (argc, argv);
    gnc_prefs_init ();
    gnc_component_manager_init ();

    g_test_add_func ("/gnome-utils/dialog-account/type-selection-ownership",
                     test_account_type_selection_owns_model_items);
    g_test_add_func ("/gnome-utils/dialog-account/type-selection/parent-change/restores-preferred",
                     test_account_type_parent_change_restores_preferred);
    g_test_add_func ("/gnome-utils/dialog-account/type-selection/parent-change/keeps-user-choice",
                     test_account_type_parent_change_keeps_user_choice);
    g_test_add_func ("/gnome-utils/dialog-account/builder-color-dialog-roots",
                     test_account_builder_roots_include_color_dialogs);
    g_test_add_func ("/gnome-utils/dialog-account/builder-container-types",
                     test_account_builder_container_types);
    status = g_test_run ();

    gnc_component_manager_shutdown ();
    gnc_prefs_remove_registered ();
    gnc_engine_shutdown ();
    return status;
}
