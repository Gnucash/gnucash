/**************************************************************************\
 * dialog-custom-report.c -- dialog for managing custom reports           *
 *                                                                        *
 * Copyright (C) 2009 Andrew Sackville-West (andrew@swclan.homelinux.org) *
 *                                                                        *
 * This program is free software; you can redistribute it and/or          *
 * modify it under the terms of the GNU General Public License as         *
 * published by the Free Software Foundation; either version 2 of the     *
 * License, or (at your option) any later version.                        *
\*************************************************************************/

#include <config.h>

#include <glib/gi18n.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include "swig-runtime.h"

#include "business-gnome-utils.h"
#include "dialog-custom-report.h"
#include "dialog-utils.h"
#include "gnc-main-window.h"
#include "gnc-gtk-utils.h"
#include "window-report.h"
#include "guile-mappings.h"
#include "gnc-guile-utils.h"
#include "gnc-ui.h"
#include "gnc-ui-util.h"
#include "gnc-report.h"
#include "gnc-plugin-page-report.h"

#define GNC_PREFS_GROUP_REPORT_SAVED_CONFIGS "dialogs.report-saved-configs"
#define CUSTOM_REPORT_DIALOG_DATA "gnc-custom-report-dialog"
#define CUSTOM_REPORT_ACTION_DATA "gnc-custom-report-action"
#define CUSTOM_REPORT_NAME_DATA "gnc-custom-report-name-request"
#define CUSTOM_REPORT_DELETE_DATA "gnc-custom-report-delete-request"

typedef struct _CustomReportRow CustomReportRow;
typedef struct _CustomReportRowClass CustomReportRowClass;
typedef struct _CustomReportDialog CustomReportDialog;
typedef struct _CustomReportNameRequest CustomReportNameRequest;
typedef struct _CustomReportDeleteRequest CustomReportDeleteRequest;

typedef enum
{
    CUSTOM_REPORT_RUN,
    CUSTOM_REPORT_EDIT,
    CUSTOM_REPORT_DELETE
} CustomReportAction;

struct _CustomReportRow
{
    GObject parent_instance;
    GncGUID guid;
    gchar *name;
};

struct _CustomReportRowClass
{
    GObjectClass parent_class;
};

struct _CustomReportDialog
{
    GtkWindow *window;
    GtkColumnView *view;
    GtkWidget *scroller;
    GtkWidget *empty_label;
    GtkLabel *status;
    GListStore *rows;
    GtkSortListModel *sorted_rows;
    GtkSingleSelection *selection;
    GWeakRef parent;
    gulong parent_destroy_id;
    gboolean closing;
};

struct _CustomReportNameRequest
{
    GtkWindow *window;
    GtkEntry *entry;
    GtkLabel *feedback;
    GWeakRef parent;
    GncGUID guid;
    gulong parent_destroy_id;
    gboolean completed;
};

struct _CustomReportDeleteRequest
{
    GtkWindow *window;
    GWeakRef parent;
    GncGUID guid;
    gulong parent_destroy_id;
    gboolean completed;
};

typedef struct
{
    CustomReportDialog *crd;
    CustomReportAction action;
} CustomReportActionFactory;

typedef struct
{
    CustomReportDialog *crd;
    GncGUID guid;
    guint position;
    CustomReportAction action;
} CustomReportActionBinding;

GType custom_report_row_get_type (void);

G_DEFINE_FINAL_TYPE (CustomReportRow, custom_report_row, G_TYPE_OBJECT)

static void custom_report_refresh (CustomReportDialog *crd,
                                   const GncGUID *selected_guid);
static void custom_report_close (CustomReportDialog *crd);
static void custom_report_run_guid (CustomReportDialog *crd, const GncGUID *guid);
static void custom_report_edit_guid (CustomReportDialog *crd, const GncGUID *guid);
static void custom_report_delete_guid (CustomReportDialog *crd, const GncGUID *guid);

static void
custom_report_row_finalize (GObject *object)
{
    CustomReportRow *row = (CustomReportRow *)object;
    g_free (row->name);
    G_OBJECT_CLASS (custom_report_row_parent_class)->finalize (object);
}

static void
custom_report_row_class_init (CustomReportRowClass *klass)
{
    G_OBJECT_CLASS (klass)->finalize = custom_report_row_finalize;
}

static void
custom_report_row_init (CustomReportRow *row)
{
    (void)row;
}

static CustomReportRow *
custom_report_row_new (const GncGUID *guid, const gchar *name)
{
    CustomReportRow *row = g_object_new (custom_report_row_get_type (), NULL);
    row->guid = *guid;
    row->name = g_strdup (name ? name : "");
    return row;
}

static GtkOrdering
custom_report_row_compare (gconstpointer left, gconstpointer right, gpointer data)
{
    const CustomReportRow *first = left;
    const CustomReportRow *second = right;
    gint result = g_utf8_collate (first->name, second->name);
    (void)data;
    return result < 0 ? GTK_ORDERING_SMALLER :
           result > 0 ? GTK_ORDERING_LARGER : GTK_ORDERING_EQUAL;
}

static gboolean
custom_report_guid_to_scm (const GncGUID *guid, SCM *scm_guid)
{
    gchar *guid_text;
    if (!guid || !scm_guid)
        return FALSE;
    guid_text = guid_to_string (guid);
    if (!guid_text)
        return FALSE;
    *scm_guid = scm_from_utf8_string (guid_text);
    g_free (guid_text);
    return TRUE;
}

static void
custom_report_set_status (CustomReportDialog *crd, const gchar *text)
{
    gtk_label_set_text (crd->status, text ? text : "");
    gtk_widget_set_visible (GTK_WIDGET (crd->status), text && *text);
}

static gboolean
custom_report_selected_guid (CustomReportDialog *crd, GncGUID *guid)
{
    CustomReportRow *row = (CustomReportRow *)gtk_single_selection_get_selected_item (crd->selection);
    if (!row)
        return FALSE;
    *guid = row->guid;
    return TRUE;
}

static guint
custom_report_find_guid (CustomReportDialog *crd, const GncGUID *guid)
{
    guint index, count = g_list_model_get_n_items (G_LIST_MODEL (crd->selection));
    for (index = 0; index < count; index++)
    {
        CustomReportRow *row = (CustomReportRow *)g_list_model_get_item (
            G_LIST_MODEL (crd->selection), index);
        gboolean match = row && guid_equal (&row->guid, guid);
        g_clear_object (&row);
        if (match)
            return index;
    }
    return GTK_INVALID_LIST_POSITION;
}

static void
custom_report_select_guid (CustomReportDialog *crd, const GncGUID *guid)
{
    guint position = custom_report_find_guid (crd, guid);
    if (position == GTK_INVALID_LIST_POSITION)
        return;
    gtk_single_selection_set_selected (crd->selection, position);
    gtk_column_view_scroll_to (crd->view, position, NULL, GTK_LIST_SCROLL_FOCUS, NULL);
}

static CustomReportDialog *
custom_report_from_window (GtkWindow *window)
{
    return window ? g_object_get_data (G_OBJECT (window), CUSTOM_REPORT_DIALOG_DATA) : NULL;
}

static void
custom_report_update_empty_state (CustomReportDialog *crd)
{
    gboolean populated = g_list_model_get_n_items (G_LIST_MODEL (crd->rows)) != 0;
    gtk_widget_set_visible (crd->scroller, populated);
    gtk_widget_set_visible (crd->empty_label, !populated);
}

static void
custom_report_refresh (CustomReportDialog *crd, const GncGUID *selected_guid)
{
    SCM get_guids = scm_c_eval_string ("gnc:custom-report-template-guids");
    SCM get_name = scm_c_eval_string ("gnc:report-template-menu-name/report-guid");
    SCM guid_list = scm_call_0 (get_guids);
    GncGUID previous_guid;
    const GncGUID *to_select = selected_guid;

    if (!to_select && custom_report_selected_guid (crd, &previous_guid))
        to_select = &previous_guid;

    gtk_single_selection_set_selected (crd->selection, GTK_INVALID_LIST_POSITION);
    g_list_store_remove_all (crd->rows);
    while (scm_is_pair (guid_list))
    {
        GncGUID guid;
        gchar *guid_text = scm_to_utf8_string (SCM_CAR (guid_list));
        gchar *name = gnc_scm_to_utf8_string (scm_call_2 (get_name, SCM_CAR (guid_list), SCM_BOOL_F));
        if (string_to_guid (guid_text, &guid))
        {
            CustomReportRow *row = custom_report_row_new (&guid, name);
            g_list_store_append (crd->rows, row);
            g_object_unref (row);
        }
        g_free (name);
        g_free (guid_text);
        guid_list = SCM_CDR (guid_list);
    }
    custom_report_update_empty_state (crd);
    if (to_select)
        custom_report_select_guid (crd, to_select);
}

static void
custom_report_free (gpointer data)
{
    CustomReportDialog *crd = data;
    GtkWindow *parent = g_weak_ref_get (&crd->parent);
    if (parent && crd->parent_destroy_id)
        g_signal_handler_disconnect (parent, crd->parent_destroy_id);
    g_clear_object (&parent);
    g_clear_object (&crd->selection);
    g_clear_object (&crd->sorted_rows);
    g_clear_object (&crd->rows);
    g_weak_ref_clear (&crd->parent);
    g_free (crd);
}

static void
custom_report_parent_destroyed (GtkWidget *parent, gpointer data)
{
    CustomReportDialog *crd = data;
    (void)parent;
    crd->parent_destroy_id = 0;
    custom_report_close (crd);
}

static void
custom_report_close (CustomReportDialog *crd)
{
    if (!crd || crd->closing)
        return;
    crd->closing = TRUE;
    gnc_save_window_size (GNC_PREFS_GROUP_REPORT_SAVED_CONFIGS, crd->window);
    gtk_window_destroy (crd->window);
}

static gboolean
custom_report_close_request (GtkWindow *window, gpointer data)
{
    (void)window;
    custom_report_close (data);
    return TRUE;
}

static void
custom_report_help (GtkButton *button, gpointer data)
{
    CustomReportDialog *crd = data;
    (void)button;
    gnc_gnome_help (crd->window, DF_MANUAL, DL_USAGE_CUSTOMREP);
}

static void
custom_report_close_clicked (GtkButton *button, gpointer data)
{
    (void)button;
    custom_report_close (data);
}

static void
custom_report_run_guid (CustomReportDialog *crd, const GncGUID *guid)
{
    SCM scm_guid, make_report;
    GtkWindow *parent;
    gint report_id;
    if (!custom_report_guid_to_scm (guid, &scm_guid))
        return;
    make_report = scm_c_eval_string ("gnc:make-report");
    report_id = scm_to_int (scm_call_1 (make_report, scm_guid));
    parent = g_weak_ref_get (&crd->parent);
    custom_report_close (crd);
    if (parent)
    {
        gnc_main_window_open_report (report_id, GNC_MAIN_WINDOW (parent));
        g_object_unref (parent);
    }
}

static void
custom_report_run_selected (CustomReportDialog *crd)
{
    GncGUID guid;
    if (!custom_report_selected_guid (crd, &guid))
    {
        custom_report_set_status (crd, _("You must select a report configuration to load."));
        return;
    }
    custom_report_set_status (crd, NULL);
    custom_report_run_guid (crd, &guid);
}

static void
custom_report_update_default_name (SCM guid, const gchar *new_name)
{
    gchar *default_guid = gnc_get_default_invoice_print_report ();
    gchar *guid_text = scm_to_utf8_string (guid);
    if (g_strcmp0 (default_guid, guid_text) == 0)
    {
        QofBook *book = gnc_get_current_book ();
        gchar *default_name = book ? qof_book_get_default_invoice_report_name (book) : NULL;
        if (book && g_strcmp0 (default_name, new_name) != 0)
            qof_book_set_default_invoice_report (book, default_guid, new_name);
        g_free (default_name);
    }
    g_free (guid_text);
    g_free (default_guid);
}

static void
custom_report_name_request_free (gpointer data)
{
    CustomReportNameRequest *request = data;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    if (parent && request->parent_destroy_id)
        g_signal_handler_disconnect (parent, request->parent_destroy_id);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
custom_report_name_finish (CustomReportNameRequest *request)
{
    if (request->completed)
        return;
    request->completed = TRUE;
    gtk_window_destroy (request->window);
}

static void
custom_report_name_parent_destroyed (GtkWidget *parent, gpointer data)
{
    CustomReportNameRequest *request = data;
    (void)parent;
    request->parent_destroy_id = 0;
    custom_report_name_finish (request);
}

static void
custom_report_name_save (GtkButton *button, gpointer data)
{
    CustomReportNameRequest *request = data;
    GtkWindow *parent;
    CustomReportDialog *crd;
    SCM guid, unique, new_name_scm;
    const gchar *new_name;
    (void)button;
    if (request->completed)
        return;
    new_name = gtk_editable_get_text (GTK_EDITABLE (request->entry));
    if (!new_name || !*new_name)
    {
        gtk_label_set_text (request->feedback, _("The report name cannot be empty."));
        gtk_widget_set_visible (GTK_WIDGET (request->feedback), TRUE);
        return;
    }
    if (!custom_report_guid_to_scm (&request->guid, &guid))
    {
        custom_report_name_finish (request);
        return;
    }
    parent = g_weak_ref_get (&request->parent);
    crd = custom_report_from_window (parent);
    if (!crd || crd->closing)
    {
        g_clear_object (&parent);
        custom_report_name_finish (request);
        return;
    }
    unique = scm_c_eval_string ("gnc:report-template-has-unique-name?");
    new_name_scm = scm_from_utf8_string (new_name);
    if (!scm_is_true (scm_call_2 (unique, guid, new_name_scm)))
    {
        gtk_label_set_text (request->feedback,
                            _("A saved report configuration with this name already exists, please choose another name."));
        gtk_widget_set_visible (GTK_WIDGET (request->feedback), TRUE);
        g_object_unref (parent);
        return;
    }
    scm_call_2 (scm_c_eval_string ("gnc:rename-report"), guid, new_name_scm);
    custom_report_update_default_name (guid, new_name);
    custom_report_refresh (crd, &request->guid);
    g_object_unref (parent);
    custom_report_name_finish (request);
}

static void
custom_report_name_cancel (GtkButton *button, gpointer data)
{
    (void)button;
    custom_report_name_finish (data);
}

static gboolean
custom_report_name_close_request (GtkWindow *window, gpointer data)
{
    (void)window;
    custom_report_name_finish (data);
    return TRUE;
}

static void
custom_report_edit_guid (CustomReportDialog *crd, const GncGUID *guid)
{
    GtkWindow *owner;
    guint position;
    CustomReportRow *row;
    CustomReportNameRequest *request;
    GtkWidget *content, *label, *actions, *cancel, *save;
    if (crd->closing)
        return;
    owner = GTK_WINDOW (g_object_ref (crd->window));
    position = custom_report_find_guid (crd, guid);
    row = position == GTK_INVALID_LIST_POSITION ? NULL :
          (CustomReportRow *)g_list_model_get_item (G_LIST_MODEL (crd->selection), position);
    if (!row)
    {
        g_object_unref (owner);
        return;
    }
    request = g_new0 (CustomReportNameRequest, 1);
    request->window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (request->window);
    request->guid = *guid;
    g_weak_ref_init (&request->parent, owner);
    gtk_window_set_title (request->window, _("Edit Report Configuration Name"));
    gtk_window_set_modal (request->window, TRUE);
    gtk_window_set_transient_for (request->window, owner);
    gtk_window_set_resizable (request->window, FALSE);
    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    label = gtk_label_new_with_mnemonic (_("_Name:"));
    request->entry = GTK_ENTRY (gtk_entry_new ());
    request->feedback = GTK_LABEL (gtk_label_new (NULL));
    actions = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    cancel = gtk_button_new_with_mnemonic (_("_Cancel"));
    save = gtk_button_new_with_mnemonic (_("_Save"));
    gtk_widget_set_margin_start (content, 18);
    gtk_widget_set_margin_end (content, 18);
    gtk_widget_set_margin_top (content, 18);
    gtk_widget_set_margin_bottom (content, 18);
    gtk_label_set_mnemonic_widget (GTK_LABEL (label), GTK_WIDGET (request->entry));
    gtk_editable_set_text (GTK_EDITABLE (request->entry), row->name);
    gtk_widget_set_hexpand (GTK_WIDGET (request->entry), TRUE);
    gtk_widget_set_visible (GTK_WIDGET (request->feedback), FALSE);
    gtk_label_set_wrap (request->feedback, TRUE);
    gtk_widget_set_halign (actions, GTK_ALIGN_END);
    gtk_box_append (GTK_BOX (actions), cancel);
    gtk_box_append (GTK_BOX (actions), save);
    gtk_box_append (GTK_BOX (content), label);
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (request->entry));
    gtk_box_append (GTK_BOX (content), GTK_WIDGET (request->feedback));
    gtk_box_append (GTK_BOX (content), actions);
    gtk_window_set_child (request->window, content);
    gtk_window_set_default_widget (request->window, save);
    g_object_set_data_full (G_OBJECT (request->window), CUSTOM_REPORT_NAME_DATA,
                            request, custom_report_name_request_free);
    request->parent_destroy_id = g_signal_connect (
        owner, "destroy", G_CALLBACK (custom_report_name_parent_destroyed), request);
    g_signal_connect (cancel, "clicked", G_CALLBACK (custom_report_name_cancel), request);
    g_signal_connect (save, "clicked", G_CALLBACK (custom_report_name_save), request);
    g_signal_connect (request->window, "close-request",
                      G_CALLBACK (custom_report_name_close_request), request);
    gtk_window_present (request->window);
    gtk_widget_grab_focus (GTK_WIDGET (request->entry));
    g_object_unref (row);
    g_object_unref (owner);
}

static void
custom_report_edit_selected (CustomReportDialog *crd)
{
    GncGUID guid;
    if (!custom_report_selected_guid (crd, &guid))
    {
        custom_report_set_status (crd, _("You must select a report configuration to rename."));
        return;
    }
    custom_report_set_status (crd, NULL);
    custom_report_edit_guid (crd, &guid);
}

static void
custom_report_delete_request_free (gpointer data)
{
    CustomReportDeleteRequest *request = data;
    GtkWindow *parent = g_weak_ref_get (&request->parent);
    if (parent && request->parent_destroy_id)
        g_signal_handler_disconnect (parent, request->parent_destroy_id);
    g_clear_object (&parent);
    g_weak_ref_clear (&request->parent);
    g_free (request);
}

static void
custom_report_delete_finish (CustomReportDeleteRequest *request, gboolean confirmed)
{
    GtkWindow *parent;
    CustomReportDialog *crd;
    SCM guid;
    if (request->completed)
        return;
    request->completed = TRUE;
    parent = g_weak_ref_get (&request->parent);
    crd = custom_report_from_window (parent);
    if (confirmed && crd && !crd->closing && custom_report_guid_to_scm (&request->guid, &guid))
    {
        scm_call_1 (scm_c_eval_string ("gnc:delete-report"), guid);
        custom_report_refresh (crd, NULL);
    }
    g_clear_object (&parent);
    gtk_window_destroy (request->window);
}

static void
custom_report_delete_parent_destroyed (GtkWidget *parent, gpointer data)
{
    CustomReportDeleteRequest *request = data;
    (void)parent;
    request->parent_destroy_id = 0;
    custom_report_delete_finish (request, FALSE);
}

static void
custom_report_delete_cancel (GtkButton *button, gpointer data)
{
    (void)button;
    custom_report_delete_finish (data, FALSE);
}

static void
custom_report_delete_confirm (GtkButton *button, gpointer data)
{
    (void)button;
    custom_report_delete_finish (data, TRUE);
}

static gboolean
custom_report_delete_close_request (GtkWindow *window, gpointer data)
{
    (void)window;
    custom_report_delete_finish (data, FALSE);
    return TRUE;
}

static void
custom_report_delete_guid (CustomReportDialog *crd, const GncGUID *guid)
{
    GtkWindow *owner;
    CustomReportDeleteRequest *request;
    SCM scm_guid, get_name;
    gchar *name, *message;
    GtkWidget *content, *detail, *actions, *cancel, *confirm;
    if (crd->closing || !custom_report_guid_to_scm (guid, &scm_guid))
        return;
    owner = GTK_WINDOW (g_object_ref (crd->window));
    get_name = scm_c_eval_string ("gnc:report-template-menu-name/report-guid");
    name = gnc_scm_to_utf8_string (scm_call_2 (get_name, scm_guid, SCM_BOOL_F));
    message = g_strdup_printf (_("Are you sure you want to delete %s?"), name);
    request = g_new0 (CustomReportDeleteRequest, 1);
    request->window = GTK_WINDOW (gtk_window_new ());
    gnc_window_bind_to_application (request->window);
    request->guid = *guid;
    g_weak_ref_init (&request->parent, owner);
    gtk_window_set_title (request->window, _("Delete Report Configuration?"));
    gtk_window_set_modal (request->window, TRUE);
    gtk_window_set_transient_for (request->window, owner);
    gtk_window_set_resizable (request->window, FALSE);
    content = gtk_box_new (GTK_ORIENTATION_VERTICAL, 12);
    detail = gtk_label_new (message);
    actions = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 6);
    cancel = gtk_button_new_with_mnemonic (_("_Cancel"));
    confirm = gtk_button_new_with_mnemonic (_("_Delete"));
    gtk_widget_set_margin_start (content, 18);
    gtk_widget_set_margin_end (content, 18);
    gtk_widget_set_margin_top (content, 18);
    gtk_widget_set_margin_bottom (content, 18);
    gtk_label_set_wrap (GTK_LABEL (detail), TRUE);
    gtk_label_set_xalign (GTK_LABEL (detail), 0.0);
    gtk_widget_set_halign (actions, GTK_ALIGN_END);
    gtk_box_append (GTK_BOX (actions), cancel);
    gtk_box_append (GTK_BOX (actions), confirm);
    gtk_box_append (GTK_BOX (content), detail);
    gtk_box_append (GTK_BOX (content), actions);
    gtk_window_set_child (request->window, content);
    gtk_window_set_default_widget (request->window, confirm);
    g_object_set_data_full (G_OBJECT (request->window), CUSTOM_REPORT_DELETE_DATA,
                            request, custom_report_delete_request_free);
    request->parent_destroy_id = g_signal_connect (
        owner, "destroy", G_CALLBACK (custom_report_delete_parent_destroyed), request);
    g_signal_connect (cancel, "clicked", G_CALLBACK (custom_report_delete_cancel), request);
    g_signal_connect (confirm, "clicked", G_CALLBACK (custom_report_delete_confirm), request);
    g_signal_connect (request->window, "close-request",
                      G_CALLBACK (custom_report_delete_close_request), request);
    gtk_window_present (request->window);
    g_free (message);
    g_free (name);
    g_object_unref (owner);
}

static void
custom_report_delete_selected (CustomReportDialog *crd)
{
    GncGUID guid;
    if (!custom_report_selected_guid (crd, &guid))
    {
        custom_report_set_status (crd, _("You must select a report configuration to delete."));
        return;
    }
    custom_report_set_status (crd, NULL);
    custom_report_delete_guid (crd, &guid);
}

static void
custom_report_action_binding_free (gpointer data)
{
    g_free (data);
}

static void
custom_report_name_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    GtkWidget *label = gtk_label_new (NULL);
    gtk_label_set_xalign (GTK_LABEL (label), 0.0);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
    (void)factory;
    (void)data;
}

static void
custom_report_name_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    CustomReportRow *row = (CustomReportRow *)gtk_list_item_get_item (item);
    gtk_label_set_text (GTK_LABEL (gtk_list_item_get_child (item)), row ? row->name : "");
    (void)factory;
    (void)data;
}

static void
custom_report_action_clicked (GtkButton *button, gpointer data)
{
    CustomReportActionBinding *binding = g_object_get_data (G_OBJECT (button), CUSTOM_REPORT_ACTION_DATA);
    (void)data;
    if (!binding || !binding->crd || binding->crd->closing)
        return;
    gtk_single_selection_set_selected (binding->crd->selection, binding->position);
    switch (binding->action)
    {
    case CUSTOM_REPORT_RUN: custom_report_run_guid (binding->crd, &binding->guid); break;
    case CUSTOM_REPORT_EDIT: custom_report_edit_guid (binding->crd, &binding->guid); break;
    case CUSTOM_REPORT_DELETE: custom_report_delete_guid (binding->crd, &binding->guid); break;
    }
}

static void
custom_report_action_setup (GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    CustomReportActionFactory *factory_data = data;
    GtkWidget *button = gtk_button_new ();
    const gchar *icon = "edit-delete";
    const gchar *tooltip = _("Delete report configuration");
    if (factory_data->action == CUSTOM_REPORT_RUN)
    {
        icon = "system-run";
        tooltip = _("Load report configuration");
    }
    else if (factory_data->action == CUSTOM_REPORT_EDIT)
    {
        icon = "accessories-text-editor";
        tooltip = _("Edit report configuration name");
    }
    gtk_button_set_icon_name (GTK_BUTTON (button), icon);
    gtk_widget_set_tooltip_text (button, tooltip);
    gtk_list_item_set_child (item, button);
    g_signal_connect (button, "clicked", G_CALLBACK (custom_report_action_clicked), NULL);
    (void)factory;
}

static void
custom_report_action_bind (GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    CustomReportActionFactory *factory_data = data;
    CustomReportRow *row = (CustomReportRow *)gtk_list_item_get_item (item);
    GtkWidget *button = gtk_list_item_get_child (item);
    CustomReportActionBinding *binding = g_new0 (CustomReportActionBinding, 1);
    binding->crd = factory_data->crd;
    binding->action = factory_data->action;
    binding->position = gtk_list_item_get_position (item);
    if (row)
        binding->guid = row->guid;
    g_object_set_data_full (G_OBJECT (button), CUSTOM_REPORT_ACTION_DATA,
                            binding, custom_report_action_binding_free);
    gtk_widget_set_sensitive (button, row != NULL);
    (void)factory;
}

static void
custom_report_action_unbind (GtkListItemFactory *factory, GtkListItem *item, gpointer data)
{
    g_object_set_data (G_OBJECT (gtk_list_item_get_child (item)), CUSTOM_REPORT_ACTION_DATA, NULL);
    (void)factory;
    (void)data;
}

static void
custom_report_add_name_column (CustomReportDialog *crd)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *column;
    g_signal_connect (factory, "setup", G_CALLBACK (custom_report_name_setup), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (custom_report_name_bind), NULL);
    column = gtk_column_view_column_new (_("Report Name"), factory);
    gtk_column_view_column_set_expand (column, TRUE);
    gtk_column_view_append_column (crd->view, column);
    g_object_unref (column);
}

static void
custom_report_add_action_column (CustomReportDialog *crd, const gchar *title,
                                 CustomReportAction action)
{
    GtkListItemFactory *factory = gtk_signal_list_item_factory_new ();
    GtkColumnViewColumn *column;
    CustomReportActionFactory *factory_data = g_new0 (CustomReportActionFactory, 1);
    factory_data->crd = crd;
    factory_data->action = action;
    g_object_set_data_full (G_OBJECT (factory), "custom-report-action-factory",
                            factory_data, g_free);
    g_signal_connect (factory, "setup", G_CALLBACK (custom_report_action_setup), factory_data);
    g_signal_connect (factory, "bind", G_CALLBACK (custom_report_action_bind), factory_data);
    g_signal_connect (factory, "unbind", G_CALLBACK (custom_report_action_unbind), factory_data);
    column = gtk_column_view_column_new (title, factory);
    gtk_column_view_append_column (crd->view, column);
    g_object_unref (column);
}

static void
custom_report_activate (GtkColumnView *view, guint position, gpointer data)
{
    CustomReportDialog *crd = data;
    CustomReportRow *row = (CustomReportRow *)g_list_model_get_item (G_LIST_MODEL (crd->selection), position);
    (void)view;
    if (!row)
        return;
    gtk_single_selection_set_selected (crd->selection, position);
    custom_report_run_guid (crd, &row->guid);
    g_object_unref (row);
}

static void
custom_report_selection_changed (GtkSelectionModel *model, guint position, guint count, gpointer data)
{
    (void)model;
    (void)position;
    (void)count;
    custom_report_set_status (data, NULL);
}

static gboolean
custom_report_key_pressed (GtkEventControllerKey *controller, guint keyval,
                           guint keycode, GdkModifierType state, gpointer data)
{
    CustomReportDialog *crd = data;
    GtkWidget *focus = gtk_window_get_focus (crd->window);
    (void)controller;
    (void)keycode;
    (void)state;
    if (keyval == GDK_KEY_Escape)
    {
        custom_report_close (crd);
        return TRUE;
    }
    if (GTK_IS_EDITABLE (focus))
        return FALSE;
    if (keyval == GDK_KEY_Return || keyval == GDK_KEY_KP_Enter)
    {
        custom_report_run_selected (crd);
        return TRUE;
    }
    if (keyval == GDK_KEY_F2)
    {
        custom_report_edit_selected (crd);
        return TRUE;
    }
    if (keyval == GDK_KEY_Delete)
    {
        custom_report_delete_selected (crd);
        return TRUE;
    }
    return FALSE;
}

static CustomReportDialog *
gnc_ui_custom_report_internal (GncMainWindow *window)
{
    GtkBuilder *builder;
    CustomReportDialog *crd;
    GtkWidget *help, *close;
    GtkSorter *sorter;
    GtkEventController *keys;
    if (!window)
        return NULL;
    crd = g_new0 (CustomReportDialog, 1);
    builder = gtk_builder_new ();
    gnc_builder_add_from_file (builder, "dialog-custom-report.glade", "custom_report_window");
    crd->window = GTK_WINDOW (gtk_builder_get_object (builder, "custom_report_window"));
    crd->view = GTK_COLUMN_VIEW (gtk_builder_get_object (builder, "custom_report_list_view"));
    crd->scroller = GTK_WIDGET (gtk_builder_get_object (builder, "custom_report_sw"));
    crd->empty_label = GTK_WIDGET (gtk_builder_get_object (builder, "no_report_notification"));
    crd->status = GTK_LABEL (gtk_builder_get_object (builder, "custom_report_status"));
    help = GTK_WIDGET (gtk_builder_get_object (builder, "help_button"));
    close = GTK_WIDGET (gtk_builder_get_object (builder, "close_report_button"));
    crd->rows = g_list_store_new (custom_report_row_get_type ());
    sorter = GTK_SORTER (gtk_custom_sorter_new (custom_report_row_compare, NULL, NULL));
    crd->sorted_rows = gtk_sort_list_model_new (G_LIST_MODEL (g_object_ref (crd->rows)), sorter);
    crd->selection = gtk_single_selection_new (G_LIST_MODEL (g_object_ref (crd->sorted_rows)));
    gtk_single_selection_set_autoselect (crd->selection, FALSE);
    gtk_column_view_set_model (crd->view, GTK_SELECTION_MODEL (crd->selection));
    custom_report_add_name_column (crd);
    custom_report_add_action_column (crd, _("Run"), CUSTOM_REPORT_RUN);
    custom_report_add_action_column (crd, _("Edit"), CUSTOM_REPORT_EDIT);
    custom_report_add_action_column (crd, _("Delete"), CUSTOM_REPORT_DELETE);
    g_weak_ref_init (&crd->parent, G_OBJECT (window));
    gtk_window_set_transient_for (crd->window, GTK_WINDOW (window));
    gtk_widget_set_name (GTK_WIDGET (crd->window), "gnc-id-custom-report");
    g_object_set_data_full (G_OBJECT (crd->window), CUSTOM_REPORT_DIALOG_DATA, crd, custom_report_free);
    crd->parent_destroy_id = g_signal_connect (window, "destroy",
                                                G_CALLBACK (custom_report_parent_destroyed), crd);
    g_signal_connect (crd->window, "close-request", G_CALLBACK (custom_report_close_request), crd);
    g_signal_connect (help, "clicked", G_CALLBACK (custom_report_help), crd);
    g_signal_connect (close, "clicked", G_CALLBACK (custom_report_close_clicked), crd);
    g_signal_connect (crd->view, "activate", G_CALLBACK (custom_report_activate), crd);
    g_signal_connect (crd->selection, "selection-changed", G_CALLBACK (custom_report_selection_changed), crd);
    keys = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET (crd->window), keys);
    g_signal_connect (keys, "key-pressed", G_CALLBACK (custom_report_key_pressed), crd);
    gnc_restore_window_size (GNC_PREFS_GROUP_REPORT_SAVED_CONFIGS, crd->window, GTK_WINDOW (window));
    custom_report_refresh (crd, NULL);
    gtk_window_present (crd->window);
    g_object_unref (builder);
    return crd;
}

void
gnc_ui_custom_report (GncMainWindow *window)
{
    gnc_ui_custom_report_internal (window);
}

void
gnc_ui_custom_report_edit_name (GncMainWindow *window, SCM scm_guid)
{
    SCM is_custom = scm_c_eval_string ("gnc:report-template-is-custom/template-guid?");
    gchar *guid_text;
    GncGUID guid;
    CustomReportDialog *crd;
    if (scm_is_false (scm_call_1 (is_custom, scm_guid)))
        return;
    guid_text = scm_to_utf8_string (scm_guid);
    if (!string_to_guid (guid_text, &guid))
    {
        g_free (guid_text);
        return;
    }
    g_free (guid_text);
    crd = gnc_ui_custom_report_internal (window);
    if (!crd || custom_report_find_guid (crd, &guid) == GTK_INVALID_LIST_POSITION)
        return;
    custom_report_select_guid (crd, &guid);
    custom_report_edit_guid (crd, &guid);
}
