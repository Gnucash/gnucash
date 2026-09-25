/********************************************************************\
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/
/** @addtogroup Import_Export
    @{ */
/** @internal
@file import-match-picker.cpp
   @brief The transaction match picker dialog implementation
*/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include <string>

#include "import-match-picker.h"
#include "import-selection-model.h"
#include "qof.h"
#include "gnc-ui-util.h"
#include "gnc-gtk-utils.h"
#include "dialog-utils.h"
#include "gnc-prefs.h"

#define GNC_PREFS_GROUP "dialogs.import.generic.match-picker"
#define GNC_PREF_DISPLAY_RECONCILED "display-reconciled"

static constexpr auto DOWNLOADED_ROW_DATA = "import-match-picker-downloaded-row";
static constexpr auto MATCH_ROW_DATA = "import-match-picker-match-row";

struct DownloadedRow
{
    std::string account;
    std::string date;
    std::string amount;
    std::string description;
    std::string memo;
    std::string balanced;
    GNCImportTransInfo *transaction_info;
};

struct MatchRow
{
    std::string confidence;
    GdkPixbuf *confidence_pixbuf;
    std::string date;
    std::string amount;
    std::string description;
    std::string memo;
    std::string reconciled;
    std::string pending;
    GNCImportMatchInfo *match_info;
};

struct _transpickerdialog
{
    /* GTK owns the window. Signal-source child widgets are weakly observed. */
    GtkWidget *transaction_matcher;
    GtkColumnView *downloaded_view;
    GtkColumnView *match_view;
    GtkCheckButton *reconciled_chk;
    GtkButton *cancel_button;
    GtkButton *ok_button;
    GListStore *downloaded_store;
    GListStore *match_store;
    GtkSingleSelection *downloaded_selection;
    GtkSingleSelection *match_selection;
    GNCImportSettings *user_settings;
    GNCImportTransInfo *selected_trans_info;
    GNCImportMatchInfo *selected_match_info;
    GNCImportPendingMatches *pending_matches;
    GNCImportTransInfo *transaction_info;
    GNCImportMatchInfo *old_match_info;
    gboolean old_selected_manually;
    GNCImportMatchPickerDoneCB done_cb;
    gpointer user_data;
    gboolean finished;
    gboolean destroyed;
    gboolean signals_disconnected;
};

static void
match_picker_disconnect_signals (GNCImportMatchPicker *matcher)
{
    if (!matcher || matcher->signals_disconnected)
        return;
    matcher->signals_disconnected = TRUE;

    if (matcher->transaction_matcher)
        g_signal_handlers_disconnect_by_data (matcher->transaction_matcher, matcher);
    if (matcher->downloaded_selection)
        g_signal_handlers_disconnect_by_data (matcher->downloaded_selection, matcher);
    if (matcher->match_selection)
        g_signal_handlers_disconnect_by_data (matcher->match_selection, matcher);
    /* During an external window dispose, finalized children have already
     * cleared these weak pointers; externally retained children remain safe
     * to disconnect. */
    if (matcher->match_view)
    {
        g_signal_handlers_disconnect_by_data (matcher->match_view, matcher);
        g_object_remove_weak_pointer (G_OBJECT (matcher->match_view),
                                      reinterpret_cast<gpointer *> (&matcher->match_view));
        matcher->match_view = nullptr;
    }
    if (matcher->reconciled_chk)
    {
        g_signal_handlers_disconnect_by_data (matcher->reconciled_chk, matcher);
        g_object_remove_weak_pointer (G_OBJECT (matcher->reconciled_chk),
                                      reinterpret_cast<gpointer *> (&matcher->reconciled_chk));
        matcher->reconciled_chk = nullptr;
    }
    if (matcher->cancel_button)
    {
        g_signal_handlers_disconnect_by_data (matcher->cancel_button, matcher);
        g_object_remove_weak_pointer (G_OBJECT (matcher->cancel_button),
                                      reinterpret_cast<gpointer *> (&matcher->cancel_button));
        matcher->cancel_button = nullptr;
    }
    if (matcher->ok_button)
    {
        g_signal_handlers_disconnect_by_data (matcher->ok_button, matcher);
        g_object_remove_weak_pointer (G_OBJECT (matcher->ok_button),
                                      reinterpret_cast<gpointer *> (&matcher->ok_button));
        matcher->ok_button = nullptr;
    }
}

static GObject*
downloaded_row_new (std::string account, std::string date, std::string amount,
                    std::string description, std::string memo, std::string balanced,
                    GNCImportTransInfo *transaction_info)
{
    auto row = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    auto values = new DownloadedRow { std::move (account), std::move (date), std::move (amount),
                                      std::move (description), std::move (memo), std::move (balanced),
                                      transaction_info };
    g_object_set_data_full (row, DOWNLOADED_ROW_DATA, values,
                            [] (gpointer data) { delete static_cast<DownloadedRow*> (data); });
    return row;
}

static DownloadedRow*
downloaded_row_get (GObject *row)
{
    return static_cast<DownloadedRow*> (g_object_get_data (row, DOWNLOADED_ROW_DATA));
}

static GObject*
match_row_new (std::string confidence, GdkPixbuf *confidence_pixbuf, std::string date,
               std::string amount, std::string description, std::string memo,
               std::string reconciled, std::string pending, GNCImportMatchInfo *match_info)
{
    auto row = G_OBJECT (g_object_new (G_TYPE_OBJECT, nullptr));
    auto values = new MatchRow { std::move (confidence), confidence_pixbuf, std::move (date),
                                 std::move (amount), std::move (description), std::move (memo),
                                 std::move (reconciled), std::move (pending), match_info };
    g_object_set_data_full (row, MATCH_ROW_DATA, values, [] (gpointer data)
    {
        auto values = static_cast<MatchRow*> (data);
        g_clear_object (&values->confidence_pixbuf);
        delete values;
    });
    return row;
}

static MatchRow*
match_row_get (GObject *row)
{
    return static_cast<MatchRow*> (g_object_get_data (row, MATCH_ROW_DATA));
}

static const gchar*
downloaded_text (const DownloadedRow *row, guint column)
{
    switch (column)
    {
    case 0: return row->account.c_str ();
    case 1: return row->date.c_str ();
    case 2: return row->amount.c_str ();
    case 3: return row->description.c_str ();
    case 4: return row->memo.c_str ();
    case 5: return row->balanced.c_str ();
    default: return "";
    }
}

static const gchar*
match_text (const MatchRow *row, guint column)
{
    switch (column)
    {
    case 0: return row->date.c_str ();
    case 1: return row->amount.c_str ();
    case 2: return row->description.c_str ();
    case 3: return row->memo.c_str ();
    case 4: return row->reconciled.c_str ();
    case 5: return row->pending.c_str ();
    default: return "";
    }
}

static void
text_item_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto label = gtk_label_new (nullptr);
    (void)factory;
    (void)user_data;
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
}

static void
downloaded_item_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto row = downloaded_row_get (G_OBJECT (gtk_list_item_get_item (item)));
    (void)factory;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        downloaded_text (row, GPOINTER_TO_UINT (user_data)));
}

static void
match_item_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto row = match_row_get (G_OBJECT (gtk_list_item_get_item (item)));
    (void)factory;
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)),
                        match_text (row, GPOINTER_TO_UINT (user_data)));
}

static void
confidence_item_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 4);
    auto picture = gnc_import_match_score_picture_new ();
    auto label = gtk_label_new (nullptr);
    (void)factory;
    (void)user_data;
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_box_append (GTK_BOX (box), GTK_WIDGET (picture));
    gtk_box_append (GTK_BOX (box), label);
    gtk_list_item_set_child (item, box);
}

static void
confidence_item_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer user_data)
{
    auto row = match_row_get (G_OBJECT (gtk_list_item_get_item (item)));
    auto box = gtk_list_item_get_child (item);
    auto picture = GTK_PICTURE (gtk_widget_get_first_child (box));
    auto label = GTK_LABEL (gtk_widget_get_next_sibling (GTK_WIDGET (picture)));
    (void)factory;
    (void)user_data;

    gtk_label_set_text (label, row->confidence.c_str ());
    if (!row->confidence_pixbuf)
    {
        gtk_picture_set_paintable (picture, nullptr);
        return;
    }

    auto texture = gnc_texture_new_from_pixbuf (row->confidence_pixbuf);
    if (!texture)
    {
        gtk_picture_set_paintable (picture, nullptr);
        return;
    }
    gtk_picture_set_paintable (picture, GDK_PAINTABLE (texture));
    g_object_unref (texture);
}

static void
add_text_column (GtkColumnView *view, const gchar *title, guint column,
                 GCallback bind_callback, gboolean expand)
{
    auto factory = gtk_signal_list_item_factory_new ();
    auto view_column = gtk_column_view_column_new (title, factory);
    g_signal_connect (factory, "setup", G_CALLBACK (text_item_setup), nullptr);
    g_signal_connect (factory, "bind", bind_callback, GUINT_TO_POINTER (column));
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_append_column (view, view_column);
    g_object_unref (view_column);
}

static void
match_picker_finish (GNCImportMatchPicker *matcher, gint response)
{
    if (!matcher || matcher->finished)
        return;
    matcher->finished = TRUE;
    match_picker_disconnect_signals (matcher);

    if (response == GTK_RESPONSE_OK && matcher->selected_match_info != matcher->old_match_info)
    {
        gnc_import_TransInfo_set_selected_match_info (matcher->transaction_info,
                                                       matcher->selected_match_info, TRUE);
        if (matcher->old_match_info)
            gnc_import_PendingMatches_remove_match (matcher->pending_matches,
                                                    matcher->old_match_info,
                                                    matcher->old_selected_manually);
        if (matcher->selected_match_info)
            gnc_import_PendingMatches_add_match (matcher->pending_matches,
                                                 matcher->selected_match_info,
                                                 TRUE);
    }

    if (!matcher->destroyed)
    {
        gnc_save_window_size (GNC_PREFS_GROUP, GTK_WINDOW (matcher->transaction_matcher));
        gtk_window_destroy (GTK_WINDOW (matcher->transaction_matcher));
    }

    auto done_cb = matcher->done_cb;
    auto transaction_info = matcher->transaction_info;
    auto user_data = matcher->user_data;
    g_clear_object (&matcher->downloaded_selection);
    g_clear_object (&matcher->match_selection);
    g_clear_object (&matcher->downloaded_store);
    g_clear_object (&matcher->match_store);
    gnc_import_Settings_delete (matcher->user_settings);
    g_free (matcher);

    if (done_cb)
        done_cb (transaction_info, user_data);
}

static void
match_picker_button_clicked_cb (GtkButton *button, GNCImportMatchPicker *matcher)
{
    auto response = GPOINTER_TO_INT (g_object_get_data (G_OBJECT (button), "response"));
    match_picker_finish (matcher, response);
}

static gboolean
match_picker_close_request_cb (GtkWindow *window, GNCImportMatchPicker *matcher)
{
    (void)window;
    match_picker_finish (matcher, GTK_RESPONSE_CANCEL);
    return TRUE;
}

static void
match_picker_destroyed_cb (GtkWidget *window, GNCImportMatchPicker *matcher)
{
    if (!matcher || matcher->finished)
        return;
    matcher->destroyed = TRUE;
    match_picker_finish (matcher, GTK_RESPONSE_CANCEL);
    (void)window;
}

static void
downloaded_selection_changed_cb (GtkSelectionModel *selection, guint position, guint n_items,
                                 GNCImportMatchPicker *matcher)
{
    auto selected = gtk_single_selection_get_selected (GTK_SINGLE_SELECTION (selection));
    (void)position;
    (void)n_items;
    if (selected == GTK_INVALID_LIST_POSITION)
    {
        matcher->selected_trans_info = nullptr;
        return;
    }

    auto row = G_OBJECT (g_list_model_get_item (G_LIST_MODEL (matcher->downloaded_store), selected));
    matcher->selected_trans_info = downloaded_row_get (row)->transaction_info;
    g_object_unref (row);
}

static void
match_selection_changed_cb (GtkSelectionModel *selection, guint position, guint n_items,
                            GNCImportMatchPicker *matcher)
{
    auto selected = gtk_single_selection_get_selected (GTK_SINGLE_SELECTION (selection));
    (void)position;
    (void)n_items;
    if (selected == GTK_INVALID_LIST_POSITION)
    {
        matcher->selected_match_info = nullptr;
        return;
    }

    auto row = G_OBJECT (g_list_model_get_item (G_LIST_MODEL (matcher->match_store), selected));
    matcher->selected_match_info = match_row_get (row)->match_info;
    g_object_unref (row);
}

static void
match_update_match_model (GNCImportMatchPicker *matcher)
{
    g_return_if_fail (matcher);

    g_list_store_remove_all (matcher->match_store);
    matcher->selected_match_info = nullptr;
    if (!matcher->selected_trans_info)
        return;

    auto show_reconciled = gtk_check_button_get_active (GTK_CHECK_BUTTON (matcher->reconciled_chk));
    for (auto n = gnc_import_TransInfo_get_match_list (matcher->selected_trans_info); n; n = g_list_next (n))
    {
        auto match_info = static_cast<GNCImportMatchInfo*> (n->data);
        auto split = gnc_import_MatchInfo_get_split (match_info);
        auto reconciled = xaccSplitGetReconcile (split);
        if (!show_reconciled && reconciled != NREC)
            continue;

        auto probability = gnc_import_MatchInfo_get_probability (match_info);
        auto trans = xaccSplitGetParent (split);
        auto match_type = gnc_import_PendingMatches_get_match_type (matcher->pending_matches, match_info);
        auto date = qof_print_date (xaccTransGetDate (trans));
        auto amount = xaccPrintAmount (xaccSplitGetAmount (split), gnc_split_amount_print_info (split, true));
        auto pending = (match_type == GNCImportPending_MANUAL || match_type == GNCImportPending_AUTO)
            ? g_strdup_printf ("%s (%s)", gnc_get_reconcile_str (CREC),
                               gnc_import_PendingMatches_get_type_str (match_type)) : nullptr;
        auto pixbuf = probability ? gen_probability_pixbuf (probability, matcher->user_settings,
                                                              GTK_WIDGET (matcher->match_view)) : nullptr;
        auto confidence = g_strdup_printf ("%d", probability);
        auto row = match_row_new (confidence, pixbuf,
                                  date ? date : "", amount ? amount : "",
                                  xaccTransGetDescription (trans) ? xaccTransGetDescription (trans) : "",
                                  xaccSplitGetMemo (split) ? xaccSplitGetMemo (split) : "",
                                  gnc_get_reconcile_str (reconciled), pending ? pending : "", match_info);
        g_list_store_append (matcher->match_store, row);
        auto position = g_list_model_get_n_items (G_LIST_MODEL (matcher->match_store)) - 1;
        if (match_info == gnc_import_TransInfo_get_selected_match (matcher->selected_trans_info))
            gtk_single_selection_set_selected (matcher->match_selection, position);
        g_object_unref (row);
        g_free (confidence);
        g_free (date);
        g_free (pending);
    }
}

static void
match_show_reconciled_changed_cb (GtkCheckButton *checkbox, GNCImportMatchPicker *matcher)
{
    (void)checkbox;
    match_update_match_model (matcher);
}

static void
match_transaction_activated_cb (GtkColumnView *view, guint position, GNCImportMatchPicker *matcher)
{
    (void)view;
    (void)position;
    match_picker_finish (matcher, GTK_RESPONSE_OK);
}

static void
downloaded_transaction_append (GNCImportMatchPicker *matcher, GNCImportTransInfo *transaction_info)
{
    g_return_if_fail (matcher);
    g_return_if_fail (transaction_info);

    guint position = GTK_INVALID_LIST_POSITION;
    for (guint index = 0; index < g_list_model_get_n_items (G_LIST_MODEL (matcher->downloaded_store)); ++index)
    {
        auto row = G_OBJECT (g_list_model_get_item (G_LIST_MODEL (matcher->downloaded_store), index));
        if (downloaded_row_get (row)->transaction_info == transaction_info)
            position = index;
        g_object_unref (row);
        if (position != GTK_INVALID_LIST_POSITION)
            break;
    }

    if (position == GTK_INVALID_LIST_POSITION)
    {
        auto split = gnc_import_TransInfo_get_fsplit (transaction_info);
        auto trans = gnc_import_TransInfo_get_trans (transaction_info);
        auto date = qof_print_date (xaccTransGetDate (trans));
        auto amount = g_strdup (xaccPrintAmount (xaccSplitGetAmount (split),
                                                  gnc_split_amount_print_info (split, TRUE)));
        auto imbalance = g_strdup (xaccPrintAmount (xaccTransGetImbalanceValue (trans),
                                                     gnc_commodity_print_info (xaccTransGetCurrency (trans), TRUE)));
        auto row = downloaded_row_new (xaccAccountGetName (xaccSplitGetAccount (split)), date ? date : "",
                                       amount ? amount : "", xaccTransGetDescription (trans) ? xaccTransGetDescription (trans) : "",
                                       xaccSplitGetMemo (split) ? xaccSplitGetMemo (split) : "", imbalance ? imbalance : "",
                                       transaction_info);
        g_list_store_append (matcher->downloaded_store, row);
        position = g_list_model_get_n_items (G_LIST_MODEL (matcher->downloaded_store)) - 1;
        g_object_unref (row);
        g_free (date);
        g_free (amount);
        g_free (imbalance);
    }

    gtk_single_selection_set_selected (matcher->downloaded_selection, position);
    matcher->selected_trans_info = transaction_info;
    match_update_match_model (matcher);
}

static void
init_match_picker_gui (GtkWidget *parent, GNCImportMatchPicker *matcher)
{
    auto builder = gtk_builder_new ();
    matcher->user_settings = gnc_import_Settings_new ();
    gnc_builder_add_from_file (builder, "dialog-import.glade", "match_picker_dialog");

    matcher->transaction_matcher = GTK_WIDGET (gtk_builder_get_object (builder, "match_picker_dialog"));
    auto downloaded_scroller = GTK_SCROLLED_WINDOW (gtk_builder_get_object (builder, "download_view"));
    auto match_scroller = GTK_SCROLLED_WINDOW (gtk_builder_get_object (builder, "matched_view"));
    matcher->reconciled_chk = GTK_CHECK_BUTTON (gtk_builder_get_object (builder, "hide_reconciled_check1"));
    matcher->cancel_button = GTK_BUTTON (gtk_builder_get_object (builder, "cancel_button1"));
    matcher->ok_button = GTK_BUTTON (gtk_builder_get_object (builder, "ok_button1"));
    g_return_if_fail (matcher->transaction_matcher && downloaded_scroller && match_scroller &&
                      matcher->reconciled_chk && matcher->cancel_button && matcher->ok_button);

    g_object_add_weak_pointer (G_OBJECT (matcher->reconciled_chk),
                               reinterpret_cast<gpointer *> (&matcher->reconciled_chk));
    g_object_add_weak_pointer (G_OBJECT (matcher->cancel_button),
                               reinterpret_cast<gpointer *> (&matcher->cancel_button));
    g_object_add_weak_pointer (G_OBJECT (matcher->ok_button),
                               reinterpret_cast<gpointer *> (&matcher->ok_button));

    matcher->downloaded_store = g_list_store_new (G_TYPE_OBJECT);
    matcher->match_store = g_list_store_new (G_TYPE_OBJECT);
    matcher->downloaded_selection = gnc_import_single_selection_new (G_LIST_MODEL (matcher->downloaded_store));
    matcher->match_selection = gnc_import_single_selection_new (G_LIST_MODEL (matcher->match_store));
    matcher->downloaded_view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (g_object_ref (matcher->downloaded_selection))));
    matcher->match_view = GTK_COLUMN_VIEW (gtk_column_view_new (GTK_SELECTION_MODEL (g_object_ref (matcher->match_selection))));
    g_object_add_weak_pointer (G_OBJECT (matcher->match_view),
                               reinterpret_cast<gpointer *> (&matcher->match_view));

    gtk_column_view_set_reorderable (matcher->downloaded_view, TRUE);
    gtk_column_view_set_reorderable (matcher->match_view, TRUE);
    gtk_scrolled_window_set_child (downloaded_scroller, GTK_WIDGET (matcher->downloaded_view));
    gtk_scrolled_window_set_child (match_scroller, GTK_WIDGET (matcher->match_view));

    add_text_column (matcher->downloaded_view, _("Account"), 0, G_CALLBACK (downloaded_item_bind), FALSE);
    add_text_column (matcher->downloaded_view, _("Date"), 1, G_CALLBACK (downloaded_item_bind), FALSE);
    add_text_column (matcher->downloaded_view, _("Amount"), 2, G_CALLBACK (downloaded_item_bind), FALSE);
    add_text_column (matcher->downloaded_view, _("Description"), 3, G_CALLBACK (downloaded_item_bind), TRUE);
    add_text_column (matcher->downloaded_view, _("Memo"), 4, G_CALLBACK (downloaded_item_bind), TRUE);
    add_text_column (matcher->downloaded_view, _("Balanced"), 5, G_CALLBACK (downloaded_item_bind), FALSE);

    auto confidence_factory = gtk_signal_list_item_factory_new ();
    auto confidence_column = gtk_column_view_column_new (_("Confidence"), confidence_factory);
    g_signal_connect (confidence_factory, "setup", G_CALLBACK (confidence_item_setup), nullptr);
    g_signal_connect (confidence_factory, "bind", G_CALLBACK (confidence_item_bind), nullptr);
    gtk_column_view_column_set_resizable (confidence_column, TRUE);
    gtk_column_view_append_column (matcher->match_view, confidence_column);
    g_object_unref (confidence_column);
    add_text_column (matcher->match_view, _("Date"), 0, G_CALLBACK (match_item_bind), FALSE);
    add_text_column (matcher->match_view, _("Amount"), 1, G_CALLBACK (match_item_bind), FALSE);
    add_text_column (matcher->match_view, _("Description"), 2, G_CALLBACK (match_item_bind), TRUE);
    add_text_column (matcher->match_view, _("Memo"), 3, G_CALLBACK (match_item_bind), TRUE);
    add_text_column (matcher->match_view, _("Reconciled"), 4, G_CALLBACK (match_item_bind), FALSE);
    add_text_column (matcher->match_view, _("Pending Action"), 5, G_CALLBACK (match_item_bind), TRUE);

    gtk_widget_set_name (matcher->transaction_matcher, "gnc-id-import-matcher-picker");
    gnc_widget_style_context_add_class (matcher->transaction_matcher, "gnc-class-imports");
    if (parent)
        gtk_window_set_transient_for (GTK_WINDOW (matcher->transaction_matcher), GTK_WINDOW (parent));
    gtk_window_set_modal (GTK_WINDOW (matcher->transaction_matcher), TRUE);
    gnc_prefs_bind (GNC_PREFS_GROUP, GNC_PREF_DISPLAY_RECONCILED, nullptr, matcher->reconciled_chk, "active");
    g_signal_connect (matcher->downloaded_selection, "selection-changed",
                      G_CALLBACK (downloaded_selection_changed_cb), matcher);
    g_signal_connect (matcher->match_selection, "selection-changed",
                      G_CALLBACK (match_selection_changed_cb), matcher);
    g_signal_connect (matcher->match_view, "activate", G_CALLBACK (match_transaction_activated_cb), matcher);
    g_signal_connect (matcher->reconciled_chk, "toggled", G_CALLBACK (match_show_reconciled_changed_cb), matcher);
    g_object_set_data (G_OBJECT (matcher->cancel_button), "response",
                       GINT_TO_POINTER (GTK_RESPONSE_CANCEL));
    g_object_set_data (G_OBJECT (matcher->ok_button), "response",
                       GINT_TO_POINTER (GTK_RESPONSE_OK));
    g_signal_connect (matcher->cancel_button, "clicked",
                      G_CALLBACK (match_picker_button_clicked_cb), matcher);
    g_signal_connect (matcher->ok_button, "clicked",
                      G_CALLBACK (match_picker_button_clicked_cb), matcher);
    g_signal_connect (matcher->transaction_matcher, "close-request",
                      G_CALLBACK (match_picker_close_request_cb), matcher);
    g_signal_connect (matcher->transaction_matcher, "destroy",
                      G_CALLBACK (match_picker_destroyed_cb), matcher);
    gtk_window_set_default_widget (GTK_WINDOW (matcher->transaction_matcher),
                                   GTK_WIDGET (matcher->ok_button));
    gnc_restore_window_size (GNC_PREFS_GROUP, GTK_WINDOW (matcher->transaction_matcher),
                             parent ? GTK_WINDOW (parent) : nullptr);
    g_object_unref (builder);
}

GNCImportMatchPicker *
gnc_import_match_picker_run (GtkWidget *parent, GNCImportTransInfo *transaction_info,
                             GNCImportPendingMatches *pending_matches,
                             GNCImportMatchPickerDoneCB done_cb, gpointer user_data)
{
    g_return_val_if_fail (transaction_info, nullptr);
    auto matcher = g_new0 (GNCImportMatchPicker, 1);
    matcher->pending_matches = pending_matches;
    matcher->transaction_info = transaction_info;
    matcher->old_match_info = gnc_import_TransInfo_get_selected_match (transaction_info);
    matcher->old_selected_manually = gnc_import_TransInfo_get_match_selected_manually (transaction_info);
    matcher->done_cb = done_cb;
    matcher->user_data = user_data;
    init_match_picker_gui (parent, matcher);
    downloaded_transaction_append (matcher, transaction_info);
    gtk_widget_set_visible (matcher->transaction_matcher, TRUE);
    return matcher;
}

void
gnc_import_match_picker_cancel (GNCImportMatchPicker *matcher)
{
    match_picker_finish (matcher, GTK_RESPONSE_CANCEL);
}

/** @} */
