/* Copyright (C) 2026 GnuCash contributors
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 */

#include <config.h>
#include <gtk/gtk.h>
#include "test/gnome-response-test-fixture.h"

#include "cashobjects.h"
#include "gnc-component-manager.h"
#include "gnc-gsettings.h"
#include "gnc-session.h"
#include "gncBillTerm.h"
#include "qofevent.h"
#include "qofinstance.h"
#include "qof.h"
extern "C"
{
#include "dialog-billterms.h"
}

namespace
{
struct ResponseState
{
    GtkWidget *parent_to_destroy{};
    GtkWidget *dialog_to_reenter{};
    GncGUID watched_term_guid{};
    gboolean parent_destroyed_by_modify{};
    gboolean response_reentered{};
    gulong event_handler{};
};

GtkWidget *
find_buildable (GtkWidget *root, const char *name)
{
    if (GTK_IS_BUILDABLE (root) &&
        g_strcmp0 (gtk_buildable_get_name (GTK_BUILDABLE (root)), name) == 0)
        return root;
    if (!GTK_IS_CONTAINER (root))
        return nullptr;
    auto children = gtk_container_get_children (GTK_CONTAINER (root));
    GtkWidget *result = nullptr;
    for (auto node = children; node && !result; node = node->next)
        result = find_buildable (GTK_WIDGET (node->data), name);
    g_list_free (children);
    return result;
}

GtkWidget *
find_named_toplevel (const char *name, GtkWindow *parent = nullptr)
{
    auto windows = gtk_window_list_toplevels ();
    GtkWidget *result = nullptr;
    for (auto node = windows; node; node = node->next)
    {
        auto widget = GTK_WIDGET (node->data);
        if (g_strcmp0 (gtk_widget_get_name (widget), name) == 0 &&
            (!parent || gtk_window_get_transient_for (GTK_WINDOW(widget)) == parent))
        {
            EXPECT_EQ (result, nullptr);
            if (result)
            {
                g_list_free (windows);
                return nullptr;
            }
            result = widget;
        }
    }
    g_list_free (windows);
    return result;
}

void
destroy_parent_on_modify (QofInstance *entity, QofEventId event_type,
                          gpointer user_data, gpointer)
{
    auto state = static_cast<ResponseState *> (user_data);
    if (!(event_type & QOF_EVENT_MODIFY) ||
        !guid_equal (qof_instance_get_guid (entity), &state->watched_term_guid) ||
        !state->parent_to_destroy)
        return;
    auto parent = state->parent_to_destroy;
    state->parent_to_destroy = nullptr;
    state->parent_destroyed_by_modify = TRUE;
    if (state->dialog_to_reenter)
    {
        auto dialog = state->dialog_to_reenter;
        state->dialog_to_reenter = nullptr;
        state->response_reentered = TRUE;
        gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    }
    gtk_widget_destroy (parent);
}

class BillTermsResponseTest : public GnomeResponseTest
{
protected:
    void SetUp () override
    {
        GnomeResponseTest::SetUp ();
        book = qof_book_new ();
        session = qof_session_new (book);
        gnc_set_current_session (session);
        owner = gtk_window_new (GTK_WINDOW_TOPLEVEL);
        g_object_ref_sink (owner);
        gtk_widget_realize (owner);
        open_manager ();
        ASSERT_NE (manager, nullptr);
    }

    void TearDown () override
    {
        if (state.event_handler)
            qof_event_unregister_handler (state.event_handler);
        state = {};
        for (auto window : managers)
        {
            gtk_widget_destroy (window);
            g_object_unref (window);
        }
        if (owner)
        {
            gtk_widget_destroy (owner);
            g_object_unref (owner);
        }
        GnomeResponseTest::TearDown ();
        for (auto widget : retained_widgets)
            g_object_unref (widget);
        retained_widgets.clear ();
        gnc_clear_current_session ();
        session = nullptr;
        owner = nullptr;
    }

    QofBook *book{};
    QofSession *session{};
    GtkWidget *owner{};
    GtkWidget *manager{};
    std::vector<GtkWidget *> managers;
    std::vector<GtkWidget *> retained_widgets;
    ResponseState state;

    void open_manager ()
    {
        ASSERT_NE (gnc_ui_billterms_window_new (GTK_WINDOW (owner), book), nullptr);
        manager = find_named_toplevel ("gnc-id-bill-terms");
        if (manager)
        {
            g_object_ref (manager);
            managers.push_back (manager);
        }
    }

    GncBillTerm *create_term (const char *name)
    {
        /* A CREATE event can refresh the manager immediately. Keep the GUI
         * from observing the object before its type and name are initialized. */
        gnc_suspend_gui_refresh ();
        auto term = gncBillTermCreate (book);
        gncBillTermSetType (term, GNC_TERM_TYPE_DAYS);
        gncBillTermSetName (term, name);
        gnc_resume_gui_refresh ();
        return term;
    }

    void retain_widget (GtkWidget *widget)
    {
        ASSERT_NE (widget, nullptr);
        g_object_ref (widget);
        retained_widgets.push_back (widget);
    }

    GtkWidget *open_edit_dialog_for_first_term ()
    {
        auto parent = find_named_toplevel ("gnc-id-bill-terms");
        EXPECT_NE (parent, nullptr);
        if (!parent)
            return nullptr;
        auto view = find_buildable (parent, "terms_view");
        EXPECT_TRUE (GTK_IS_TREE_VIEW (view));
        if (!GTK_IS_TREE_VIEW (view))
            return nullptr;
        auto model = gtk_tree_view_get_model (GTK_TREE_VIEW (view));
        GtkTreeIter iter;
        auto has_first = gtk_tree_model_get_iter_first (model, &iter);
        EXPECT_TRUE (has_first);
        if (!has_first)
            return nullptr;
        auto path = gtk_tree_model_get_path (model, &iter);
        gtk_tree_selection_select_path (
            gtk_tree_view_get_selection (GTK_TREE_VIEW (view)), path);
        gtk_tree_path_free (path);
        auto edit_button = find_buildable (parent, "edit_term_button");
        EXPECT_TRUE (GTK_IS_BUTTON (edit_button));
        if (!GTK_IS_BUTTON (edit_button))
            return nullptr;
        gtk_button_clicked (GTK_BUTTON (edit_button));
        auto dialog = find_named_toplevel ("gnc-id-new-bill-terms",
                                          GTK_WINDOW (parent));
        EXPECT_TRUE (GTK_IS_DIALOG (dialog));
        return GTK_IS_DIALOG (dialog) ? dialog : nullptr;
    }

    GtkWidget *find_delete_question ()
    {
        auto parent = find_named_toplevel ("gnc-id-bill-terms");
        EXPECT_NE (parent, nullptr);
        if (!parent)
            return nullptr;
        auto button = find_buildable (parent, "delete_term_button");
        EXPECT_TRUE (GTK_IS_BUTTON (button));
        if (!GTK_IS_BUTTON (button))
            return nullptr;
        gtk_button_clicked (GTK_BUTTON (button));
        auto windows = gtk_window_list_toplevels ();
        GtkWidget *question = nullptr;
        for (auto node = windows; node; node = node->next)
            if (GTK_IS_MESSAGE_DIALOG (node->data) &&
                gtk_window_get_transient_for (GTK_WINDOW (node->data)) ==
                    GTK_WINDOW (parent))
                question = GTK_WIDGET (node->data);
        g_list_free (windows);
        EXPECT_NE (question, nullptr);
        return question;
    }
};

TEST_F (BillTermsResponseTest, DecliningDeleteKeepsTerm)
{
    auto term = create_term ("Confirmed term");
    auto guid = *gncBillTermGetGUID (term);
    auto question = find_delete_question ();
    ASSERT_NE (question, nullptr);
    EXPECT_NE (gncBillTermLookup (book, &guid), nullptr);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_NO);
    EXPECT_NE (gncBillTermLookup (book, &guid), nullptr);
}

TEST_F (BillTermsResponseTest, AcceptingDeleteRemovesUnreferencedTerm)
{
    auto term = create_term ("Confirmed term");
    auto guid = *gncBillTermGetGUID (term);
    auto question = find_delete_question ();
    ASSERT_NE (question, nullptr);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_YES);
    EXPECT_EQ (gncBillTermLookup (book, &guid), nullptr);
}

TEST_F (BillTermsResponseTest, ReferencedTermSurvivesDeleteConfirmation)
{
    auto term = create_term ("Confirmed term");
    auto guid = *gncBillTermGetGUID (term);
    gncBillTermIncRef (term);
    auto question = find_delete_question ();
    ASSERT_NE (question, nullptr);
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_YES);
    term = gncBillTermLookup (book, &guid);
    ASSERT_NE (term, nullptr);
    gncBillTermDecRef (term);
}

TEST_F (BillTermsResponseTest, LateDeleteResponseAfterManagerDestructionDoesNotDeleteTerm)
{
    auto term = create_term ("Confirmed term");
    auto guid = *gncBillTermGetGUID (term);
    auto question = find_delete_question ();
    ASSERT_NE (question, nullptr);
    retain_widget (question);
    gtk_widget_destroy (find_named_toplevel ("gnc-id-bill-terms"));
    gtk_dialog_response (GTK_DIALOG (question), GTK_RESPONSE_YES);
    EXPECT_NE (gncBillTermLookup (book, &guid), nullptr);
}

TEST_F (BillTermsResponseTest, NewAcceptedCreatesTerm)
{
    auto parent = find_named_toplevel ("gnc-id-bill-terms");
    ASSERT_NE (parent, nullptr);

    auto new_button = find_buildable (parent, "new_term_button");
    ASSERT_TRUE (GTK_IS_BUTTON (new_button));
    gtk_button_clicked (GTK_BUTTON (new_button));
    auto dialog = find_named_toplevel ("gnc-id-new-bill-terms",
                                      GTK_WINDOW (parent));
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    ASSERT_TRUE (gtk_window_get_modal (GTK_WINDOW (dialog)));
    ASSERT_TRUE (gtk_window_get_destroy_with_parent (GTK_WINDOW (dialog)));
    auto name = find_buildable (dialog, "name_entry");
    ASSERT_TRUE (GTK_IS_ENTRY (name));
    gtk_entry_set_text (GTK_ENTRY (name), "Response-created term");
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

    auto term = gncBillTermLookupByName (book, "Response-created term");
    ASSERT_NE (term, nullptr);
    EXPECT_EQ (find_named_toplevel ("gnc-id-new-bill-terms",
                                       GTK_WINDOW (parent)), nullptr);

}

TEST_F (BillTermsResponseTest, ParentDestroyedRejectsPendingEdit)
{
    auto term = create_term ("Pending edit term");
    auto term_guid = *gncBillTermGetGUID (term);
    auto parent = find_named_toplevel ("gnc-id-bill-terms");
    ASSERT_NE (parent, nullptr);
    auto dialog = open_edit_dialog_for_first_term ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    auto description = find_buildable (dialog, "entry_desc");
    ASSERT_TRUE (GTK_IS_ENTRY (description));
    gtk_entry_set_text (GTK_ENTRY (description), "Must not survive parent close");

    /* Retain the destroyed widget so a late response is safe to deliver. */
    retain_widget (dialog);
    gtk_widget_destroy (parent);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    auto unchanged = gncBillTermLookup (book, &term_guid);
    ASSERT_NE (unchanged, nullptr);
    EXPECT_STREQ (gncBillTermGetDescription (unchanged), "");
    EXPECT_EQ (find_named_toplevel ("gnc-id-new-bill-terms"), nullptr);
}

TEST_F (BillTermsResponseTest, ModifyEventMayDestroyAndReenterOwner)
{
    auto term = create_term ("Reentrant edit term");
    auto term_guid = *gncBillTermGetGUID (term);
    auto parent = find_named_toplevel ("gnc-id-bill-terms");
    ASSERT_NE (parent, nullptr);
    auto dialog = open_edit_dialog_for_first_term ();
    ASSERT_TRUE (GTK_IS_DIALOG (dialog));
    retain_widget (dialog);
    auto description = find_buildable (dialog, "entry_desc");
    ASSERT_TRUE (GTK_IS_ENTRY (description));
    gtk_entry_set_text (GTK_ENTRY (description), "Committed before parent close");
    auto due_days = find_buildable (dialog, "days:due_days");
    auto discount_days = find_buildable (dialog, "days:discount_days");
    ASSERT_TRUE (GTK_IS_SPIN_BUTTON (due_days));
    ASSERT_TRUE (GTK_IS_SPIN_BUTTON (discount_days));
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (due_days), 20);
    gtk_spin_button_set_value (GTK_SPIN_BUTTON (discount_days), 3);
    state.watched_term_guid = term_guid;
    state.parent_to_destroy = parent;
    state.dialog_to_reenter = dialog;
    state.parent_destroyed_by_modify = FALSE;
    state.response_reentered = FALSE;
    state.event_handler = qof_event_register_handler (
        destroy_parent_on_modify, &state);
    gtk_dialog_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);
    qof_event_unregister_handler (state.event_handler);
    state.event_handler = 0;
    EXPECT_TRUE (state.parent_destroyed_by_modify);
    EXPECT_TRUE (state.response_reentered);
    EXPECT_EQ (state.dialog_to_reenter, nullptr);
    EXPECT_EQ (find_named_toplevel ("gnc-id-bill-terms"), nullptr);
    term = gncBillTermLookup (book, &term_guid);
    ASSERT_NE (term, nullptr);
    EXPECT_STREQ (gncBillTermGetDescription (term), "Committed before parent close");
    EXPECT_EQ (gncBillTermGetDueDays (term), 20);
    EXPECT_EQ (gncBillTermGetDiscountDays (term), 3);
    EXPECT_EQ (qof_instance_get_editlevel (term), 0);

}
}

int
main (int argc, char **argv)
{
    ::testing::InitGoogleTest (&argc, argv);
    if (!gtk_init_check (&argc, &argv))
    {
        g_printerr ("GTK display initialization failed; GUI tests require a display.\n");
        return 1;
    }
    qof_init ();
    g_assert_true (cashobjects_register ());
    gnc_component_manager_init ();
    gnc_gsettings_load_backend ();
    g_log_set_always_fatal (static_cast<GLogLevelFlags> (
        G_LOG_FATAL_MASK | G_LOG_LEVEL_WARNING | G_LOG_LEVEL_CRITICAL));
    auto result = RUN_ALL_TESTS ();
    gnc_gsettings_shutdown ();
    gnc_component_manager_shutdown ();
    gnc_clear_current_session ();
    qof_close ();
    return result;
}
