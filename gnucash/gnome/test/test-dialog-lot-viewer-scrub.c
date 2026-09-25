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

#include <config.h>

#include <gtk/gtk.h>

#include "dialog-lot-viewer.h"
#include "gnc-engine.h"
#include "qofbook.h"
#include "qofsession.h"

static Account *
add_account (QofBook *book, Account *root, GNCAccountType type,
             const char *name)
{
    Account *account = xaccMallocAccount (book);
    xaccAccountBeginEdit (account);
    xaccAccountSetName (account, name);
    xaccAccountSetType (account, type);
    gnc_account_append_child (root, account);
    xaccAccountCommitEdit (account);
    return account;
}

static void
test_non_apar_routes_to_async_runner (void)
{
    QofSession *session = qof_session_new (qof_book_new ());
    QofBook *book = qof_session_get_book (session);
    Account *root = gnc_account_create_root (book);
    Account *stock = add_account (book, root, ACCT_TYPE_STOCK, "Stock");
    Account *receivable = add_account (book, root, ACCT_TYPE_RECEIVABLE,
                                       "Receivable");
    Account *payable = add_account (book, root, ACCT_TYPE_PAYABLE, "Payable");

    g_assert_true (gnc_lot_viewer_account_uses_async_scrub (stock));
    g_assert_false (gnc_lot_viewer_account_uses_async_scrub (receivable));
    g_assert_false (gnc_lot_viewer_account_uses_async_scrub (payable));
    g_assert_false (gnc_lot_viewer_account_uses_async_scrub (NULL));
    qof_session_destroy (session);
}

static void
test_scrub_controls_lock_and_unlock_together (void)
{
    GtkWidget *lot_button = gtk_button_new ();
    GtkWidget *account_button = gtk_button_new ();
    g_object_ref_sink (lot_button);
    g_object_ref_sink (account_button);

    gnc_lot_viewer_set_scrub_controls_sensitive (
        lot_button, account_button, FALSE);
    g_assert_false (gtk_widget_get_sensitive (lot_button));
    g_assert_false (gtk_widget_get_sensitive (account_button));

    gnc_lot_viewer_set_scrub_controls_sensitive (
        lot_button, account_button, TRUE);
    g_assert_true (gtk_widget_get_sensitive (lot_button));
    g_assert_true (gtk_widget_get_sensitive (account_button));
    g_object_unref (lot_button);
    g_object_unref (account_button);
}

int
main (int argc, char **argv)
{
    int status;
    g_test_init (&argc, &argv, NULL);
    gtk_init ();
    gnc_engine_init (argc, argv);
    g_test_add_func ("/gnome/lot-viewer/non-apar-async-route",
                     test_non_apar_routes_to_async_runner);
    g_test_add_func ("/gnome/lot-viewer/scrub-control-lock",
                     test_scrub_controls_lock_and_unlock_together);
    status = g_test_run ();
    gnc_engine_shutdown ();
    return status;
}
