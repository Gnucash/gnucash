/********************************************************************\
 * dialog-account-picker.c -- window for picking a GnuCash account  *
 * from the QIF importer.                                           *
 *                                                                  *
 * Copyright (C) 2000-2001 Bill Gribble <grib@billgribble.com>      *
 * Copyright (c) 2006 David Hampton <hampton@employees.org>        *
 * Copyright (c) 2026 GnuCash Contributors                          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libguile.h>

#include "dialog-account-picker.h"
#include "assistant-qif-import.h"
#include "dialog-utils.h"
#include "gnc-prefs.h"
#include "gnc-gtk-utils.h"
#include "gnc-ui-util.h"
#include "guile-mappings.h"
#include "gnc-guile-utils.h"

#define GNC_PREFS_GROUP "dialogs.import.qif.account-picker"

typedef struct
{
    GObject parent_instance;
    gchar *name;
    gchar *full_name;
    gboolean placeholder;
    gboolean is_new;
} QIFAccountPickerRow;

typedef struct
{
    GObjectClass parent_class;
} QIFAccountPickerRowClass;

GType qif_account_picker_row_get_type (void);

G_DEFINE_TYPE (QIFAccountPickerRow, qif_account_picker_row, G_TYPE_OBJECT)

struct _accountpickerdialog
{
    GtkWindow *window;
    GtkScrolledWindow *scroller;
    GtkColumnView *view;
    GtkWidget *warning_box;
    GtkLabel *warning;
    GtkButton *ok_button;
    QIFImportWindow *qif_wind;
    GListStore *rows;
    GtkSingleSelection *selection;
    SCM map_entry;
    SCM original_name;
    gchar *selected_name;
    QIFAccountPickerCallback callback;
    gpointer callback_data;
    gboolean finished;
};

typedef struct
{
    QIFAccountPickerDialog *picker;
    GtkWindow *window;
    GtkEntry *entry;
} NewAccountDialog;

static void build_acct_tree (QIFAccountPickerDialog *picker,
                             QIFImportWindow *import);

static void
qif_account_picker_row_finalize (GObject *object)
{
    QIFAccountPickerRow *row = (QIFAccountPickerRow *)object;

    g_free (row->name);
    g_free (row->full_name);
    G_OBJECT_CLASS (qif_account_picker_row_parent_class)->finalize (object);
}

static void
qif_account_picker_row_class_init (QIFAccountPickerRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = qif_account_picker_row_finalize;
}

static void
qif_account_picker_row_init (QIFAccountPickerRow *row)
{
    (void)row;
}

static QIFAccountPickerRow *
qif_account_picker_row_new (const gchar *name, const gchar *full_name,
                            gboolean placeholder, gboolean is_new)
{
    QIFAccountPickerRow *row =
        (QIFAccountPickerRow *)g_object_new (qif_account_picker_row_get_type (), NULL);

    row->name = g_strdup (name);
    row->full_name = g_strdup (full_name);
    row->placeholder = placeholder;
    row->is_new = is_new;
    return row;
}

static void
account_cell_setup (GtkSignalListItemFactory *factory, GtkListItem *list_item,
                    gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (list_item, label);
    (void)factory;
    (void)user_data;
}

static void
account_cell_bind (GtkSignalListItemFactory *factory, GtkListItem *list_item,
                   gpointer user_data)
{
    QIFAccountPickerRow *row =
        (QIFAccountPickerRow *)gtk_list_item_get_item (list_item);
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (list_item));
    guint column = GPOINTER_TO_UINT (user_data);

    if (!row)
        return;

    switch (column)
    {
    case 0:
        gtk_label_set_text (label, row->full_name);
        gtk_widget_set_tooltip_text (GTK_WIDGET (label), row->name);
        break;
    case 1:
        gtk_label_set_text (label, row->placeholder ? "✓" : "");
        gtk_label_set_xalign (label, 0.5);
        break;
    case 2:
        gtk_label_set_text (label, row->is_new ? "✓" : "");
        gtk_label_set_xalign (label, 0.5);
        break;
    default:
        g_assert_not_reached ();
    }
    (void)factory;
}

static void
picker_add_column (QIFAccountPickerDialog *picker, const gchar *title,
                   guint column, gboolean expand)
{
    GtkListItemFactory *factory = GTK_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    GtkColumnViewColumn *view_column;

    g_signal_connect (factory, "setup", G_CALLBACK (account_cell_setup), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (account_cell_bind),
                      GUINT_TO_POINTER (column));
    view_column = gtk_column_view_column_new (title, factory);
    gtk_column_view_column_set_resizable (view_column, TRUE);
    gtk_column_view_column_set_expand (view_column, expand);
    gtk_column_view_append_column (picker->view, view_column);
    g_object_unref (view_column);
}

static void
acct_rows_add_accts (SCM accts, GListStore *store, const gchar *base_name,
                     const gchar *search_name, guint *selected_position)
{
    while (!scm_is_null (accts))
    {
        SCM current = SCM_CAR (accts);
        gchar *component_name;
        gchar *account_name;
        gboolean leaf_node;
        gboolean placeholder = FALSE;
        gboolean is_new;
        Account *account;
        QIFAccountPickerRow *row;

        if (scm_is_null (current))
        {
            g_critical ("QIF import: empty account entry in account picker");
            accts = SCM_CDR (accts);
            continue;
        }

        component_name = scm_is_string (SCM_CAR (current))
            ? gnc_scm_to_utf8_string (SCM_CAR (current)) : g_strdup ("");
        leaf_node = scm_is_null (SCM_CADDR (current));
        account_name = base_name && *base_name
            ? g_strjoin (gnc_get_account_separator_string (), base_name,
                         component_name, NULL)
            : g_strdup (component_name);
        is_new = SCM_CADR (current) == SCM_BOOL_T;

        account = gnc_account_lookup_by_full_name (gnc_get_current_root_account (),
                                                   account_name);
        if (account)
            placeholder = xaccAccountGetPlaceholder (account);

        row = qif_account_picker_row_new (component_name, account_name,
                                          placeholder, is_new);
        if (search_name &&
            g_utf8_collate (search_name, account_name) == 0)
            *selected_position = g_list_model_get_n_items (G_LIST_MODEL (store));
        g_list_store_append (store, row);
        g_object_unref (row);

        if (!leaf_node)
            acct_rows_add_accts (SCM_CADDR (current), store, account_name,
                                 search_name, selected_position);

        g_free (account_name);
        g_free (component_name);
        accts = SCM_CDR (accts);
    }
}

static void
build_acct_tree (QIFAccountPickerDialog *picker, QIFImportWindow *import)
{
    SCM get_accounts = scm_c_eval_string ("qif-import:get-all-accts");
    SCM account_tree = scm_call_1 (get_accounts,
                                   gnc_ui_qif_import_assistant_get_mappings (import));
    guint selected_position = GTK_INVALID_LIST_POSITION;
    gchar *name_to_select = g_strdup (picker->selected_name);

    g_list_store_remove_all (picker->rows);
    acct_rows_add_accts (account_tree, picker->rows, NULL, name_to_select,
                         &selected_position);
    g_free (name_to_select);
    gtk_single_selection_set_selected (picker->selection, selected_position);
}

static void
picker_selection_changed (GtkSelectionModel *selection, guint position,
                          guint n_items, gpointer user_data)
{
    QIFAccountPickerDialog *picker = user_data;
    QIFAccountPickerRow *row =
        (QIFAccountPickerRow *)gtk_single_selection_get_selected_item (picker->selection);
    SCM name_setter;

    g_free (picker->selected_name);
    picker->selected_name = NULL;
    gtk_widget_set_visible (picker->warning_box, FALSE);
    gtk_widget_set_sensitive (GTK_WIDGET (picker->ok_button), FALSE);

    if (!row)
        return;

    picker->selected_name = g_strdup (row->full_name);
    name_setter = scm_c_eval_string ("qif-map-entry:set-gnc-name!");
    scm_call_2 (name_setter, picker->map_entry,
                scm_from_utf8_string (picker->selected_name));

    if (row->placeholder)
    {
        gchar *text = g_strdup_printf (
            _("The account %s is a placeholder account and does not allow "
              "transactions. Please choose a different account."),
            picker->selected_name);

        gtk_label_set_text (picker->warning, text);
        gtk_widget_set_visible (picker->warning_box, TRUE);
        g_free (text);
    }
    else
        gtk_widget_set_sensitive (GTK_WIDGET (picker->ok_button), TRUE);

    g_object_unref (row);
    (void)selection;
    (void)position;
    (void)n_items;
}

static void
picker_finish (QIFAccountPickerDialog *picker, gboolean accepted)
{
    SCM name_setter;

    if (picker->finished)
        return;
    picker->finished = TRUE;

    if (!accepted)
    {
        name_setter = scm_c_eval_string ("qif-map-entry:set-gnc-name!");
        scm_call_2 (name_setter, picker->map_entry, picker->original_name);
    }

    gnc_save_window_size (GNC_PREFS_GROUP, picker->window);
    if (picker->callback)
        picker->callback (accepted, picker->callback_data);

    gtk_window_destroy (picker->window);
    g_clear_object (&picker->selection);
    g_clear_object (&picker->rows);
    g_clear_object (&picker->window);
    scm_gc_unprotect_object (picker->original_name);
    scm_gc_unprotect_object (picker->map_entry);
    g_free (picker->selected_name);
    g_free (picker);
}

static gboolean
picker_close_request (GtkWindow *window, gpointer user_data)
{
    picker_finish ((QIFAccountPickerDialog *)user_data, FALSE);
    (void)window;
    return TRUE;
}

static void
picker_accept_clicked (GtkButton *button, gpointer user_data)
{
    picker_finish ((QIFAccountPickerDialog *)user_data, TRUE);
    (void)button;
}

static void
picker_cancel_clicked (GtkButton *button, gpointer user_data)
{
    picker_finish ((QIFAccountPickerDialog *)user_data, FALSE);
    (void)button;
}

static void
picker_activated (GtkColumnView *view, guint position, gpointer user_data)
{
    QIFAccountPickerDialog *picker = user_data;

    gtk_single_selection_set_selected (picker->selection, position);
    if (picker->selected_name &&
        gtk_widget_get_sensitive (GTK_WIDGET (picker->ok_button)))
        picker_finish (picker, TRUE);
    (void)view;
}

static void
new_account_dialog_destroyed (GtkWidget *widget, gpointer user_data)
{
    g_free (user_data);
    (void)widget;
}

static void
new_account_accept_clicked (GtkButton *button, gpointer user_data)
{
    NewAccountDialog *dialog = user_data;
    QIFAccountPickerDialog *picker = dialog->picker;
    const gchar *name = gtk_editable_get_text (GTK_EDITABLE (dialog->entry));
    gchar *full_name;
    SCM name_setter;

    if (picker->finished || !name || !*name)
        return;

    full_name = picker->selected_name && *picker->selected_name
        ? g_strjoin (gnc_get_account_separator_string (), picker->selected_name,
                     name, NULL)
        : g_strdup (name);
    g_free (picker->selected_name);
    picker->selected_name = full_name;
    name_setter = scm_c_eval_string ("qif-map-entry:set-gnc-name!");
    scm_call_2 (name_setter, picker->map_entry,
                scm_from_utf8_string (picker->selected_name));
    build_acct_tree (picker, picker->qif_wind);
    gtk_widget_grab_focus (GTK_WIDGET (picker->view));
    gtk_window_destroy (dialog->window);
    (void)button;
}

static void
picker_new_clicked (GtkButton *button, gpointer user_data)
{
    QIFAccountPickerDialog *picker = user_data;
    NewAccountDialog *dialog = g_new0 (NewAccountDialog, 1);
    GtkWidget *content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    GtkWidget *action_box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    GtkWidget *accept = gtk_button_new_with_mnemonic (_("_Add"));
    GtkWidget *cancel = gtk_button_new_with_mnemonic (_("_Cancel"));

    dialog->picker = picker;
    dialog->window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (dialog->window);
    dialog->entry = GTK_ENTRY (gtk_entry_new ());
    gtk_window_set_title (dialog->window, _("New Account"));
    gtk_window_set_transient_for (dialog->window, picker->window);
    gtk_window_set_destroy_with_parent (dialog->window, TRUE);
    gtk_window_set_modal (dialog->window, TRUE);
    gtk_widget_set_margin_start (content, 12);
    gtk_widget_set_margin_end (content, 12);
    gtk_widget_set_margin_top (content, 12);
    gtk_widget_set_margin_bottom (content, 12);
    gtk_box_append (GTK_BOX (content),
                    gtk_label_new (_("Enter a name for the account")));
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (dialog->entry));
    gtk_widget_set_hexpand (GTK_WIDGET (dialog->entry), TRUE);
    gtk_widget_set_halign (action_box, GTK_ALIGN_END);
    gtk_box_append (GTK_BOX (action_box), cancel);
    gtk_box_append (GTK_BOX (action_box), accept);
    gtk_box_append (GTK_BOX (content), action_box);
    gtk_window_set_child (dialog->window, content);
    gtk_window_set_default_widget (dialog->window, accept);
    g_signal_connect (accept, "clicked", G_CALLBACK (new_account_accept_clicked), dialog);
    g_signal_connect_swapped (cancel, "clicked", G_CALLBACK (gtk_window_destroy), dialog->window);
    g_signal_connect (dialog->window, "destroy", G_CALLBACK (new_account_dialog_destroyed), dialog);
    gtk_window_present (dialog->window);
    gtk_widget_grab_focus (GTK_WIDGET (dialog->entry));
    (void)button;
}

void
qif_account_picker_dialog_async (GtkWindow *parent, QIFImportWindow *qif_wind,
                                 SCM map_entry,
                                 QIFAccountPickerCallback callback,
                                 gpointer user_data)
{
    QIFAccountPickerDialog *picker = g_new0 (QIFAccountPickerDialog, 1);
    SCM name_getter = scm_c_eval_string ("qif-map-entry:gnc-name");
    GtkBuilder *builder = gtk_builder_new ();
    GtkButton *new_button;
    GtkButton *cancel_button;

    picker->map_entry = map_entry;
    picker->original_name = scm_call_1 (name_getter, map_entry);
    scm_gc_protect_object (picker->map_entry);
    scm_gc_protect_object (picker->original_name);
    picker->callback = callback;
    picker->callback_data = user_data;
    picker->qif_wind = qif_wind;
    if (scm_is_string (picker->original_name))
        picker->selected_name = gnc_scm_to_utf8_string (picker->original_name);

    gnc_builder_add_from_file (builder, "dialog-account-picker.glade",
                               "qif_import_account_picker_dialog");
    picker->window = GTK_WINDOW (gtk_builder_get_object (
        builder, "qif_import_account_picker_dialog"));
    picker->scroller = GTK_SCROLLED_WINDOW (gtk_builder_get_object (
        builder, "account_tree_scroller"));
    picker->warning_box = GTK_WIDGET (gtk_builder_get_object (
        builder, "placeholder_warning_hbox"));
    picker->warning = GTK_LABEL (gtk_builder_get_object (
        builder, "placeholder_warning_label"));
    picker->ok_button = GTK_BUTTON (gtk_builder_get_object (builder, "okbutton"));
    new_button = GTK_BUTTON (gtk_builder_get_object (builder, "newbutton"));
    cancel_button = GTK_BUTTON (gtk_builder_get_object (builder, "cancelbutton"));
    g_return_if_fail (picker->window && picker->scroller && picker->warning_box &&
                      picker->warning && picker->ok_button && new_button && cancel_button);
    g_object_ref (picker->window);
    g_object_unref (builder);

    gtk_window_set_transient_for (picker->window, parent);
    gtk_window_set_modal (picker->window, TRUE);
    gnc_restore_window_size (GNC_PREFS_GROUP, picker->window, parent);

    picker->rows = g_list_store_new (qif_account_picker_row_get_type ());
    picker->selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (picker->rows)));
    picker->view = GTK_COLUMN_VIEW (gtk_column_view_new (
        GTK_SELECTION_MODEL (g_object_ref (picker->selection))));
    picker_add_column (picker, _("Account"), 0, TRUE);
    picker_add_column (picker, _("Placeholder?"), 1, FALSE);
    picker_add_column (picker, _("New?"), 2, FALSE);
    gtk_scrolled_window_set_child (picker->scroller, GTK_WIDGET (picker->view));
    g_signal_connect (picker->selection, "selection-changed",
                      G_CALLBACK (picker_selection_changed), picker);
    g_signal_connect (picker->view, "activate", G_CALLBACK (picker_activated), picker);
    g_signal_connect (picker->window, "close-request", G_CALLBACK (picker_close_request), picker);
    g_signal_connect (picker->ok_button, "clicked", G_CALLBACK (picker_accept_clicked), picker);
    g_signal_connect (cancel_button, "clicked", G_CALLBACK (picker_cancel_clicked), picker);
    g_signal_connect (new_button, "clicked", G_CALLBACK (picker_new_clicked), picker);
    gtk_window_set_default_widget (picker->window, GTK_WIDGET (picker->ok_button));

    build_acct_tree (picker, qif_wind);
    gtk_window_present (picker->window);
}
