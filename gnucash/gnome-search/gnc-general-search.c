/*
 * gnc-general-search.c -- widget to pop up a search dialog and display
 * a selected object.
 * Copyright (C) 2001 Free Software Foundation
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include <gtk/gtk.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

#include "gnc-component-manager.h"
#include "qof.h"
#include "gnc-general-search.h"
#include "gnc-ui.h"
#include "dialog-utils.h"

#define GNCGENERALSEARCH_CLASS "gnc-general-search-widget"

enum { SELECTION_CHANGED, LAST_SIGNAL };

typedef struct _GNCGeneralSearchPrivate GNCGeneralSearchPrivate;
struct _GNCGeneralSearchPrivate
{
    GncGUID guid;
    QofIdTypeConst type;
    GNCSearchCB search_cb;
    gpointer user_data;
    GNCSearchWindow *sw;
    const QofParam *get_guid;
    gint component_id;

    GtkPopover *completion_popover;
    GtkListView *completion_view;
    GListStore *completion_store;
    GtkFilterListModel *completion_filtered;
    GtkSingleSelection *completion_selection;
    GtkCustomFilter *completion_filter;
    gboolean updating_entry;
};

G_DEFINE_TYPE_WITH_PRIVATE (GNCGeneralSearch, gnc_general_search, GTK_TYPE_BOX)
#define _PRIVATE(o) ((GNCGeneralSearchPrivate *)gnc_general_search_get_instance_private (GNC_GENERAL_SEARCH (o)))

static guint general_search_signals[LAST_SIGNAL];
static GQuark completion_object_quark;

static void gnc_general_search_dispose (GObject *object);
static void reset_selection_text (GNCGeneralSearch *gsl);
static void completion_update_matches (GNCGeneralSearch *gsl, gboolean present);

static void
completion_row_setup_cb (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
                         GtkListItem *item, G_GNUC_UNUSED gpointer user_data)
{
    GtkWidget *label = gtk_label_new (NULL);

    gtk_label_set_xalign (GTK_LABEL (label), 0.0f);
    gtk_label_set_ellipsize (GTK_LABEL (label), PANGO_ELLIPSIZE_END);
    gtk_list_item_set_child (item, label);
}

static void
completion_row_bind_cb (G_GNUC_UNUSED GtkSignalListItemFactory *factory,
                        GtkListItem *item, G_GNUC_UNUSED gpointer user_data)
{
    GtkStringObject *row = GTK_STRING_OBJECT (gtk_list_item_get_item (item));
    GtkLabel *label = GTK_LABEL (gtk_list_item_get_child (item));

    gtk_label_set_text (label, row ? gtk_string_object_get_string (row) : "");
}

static gboolean
completion_filter_cb (gpointer item, gpointer user_data)
{
    GNCGeneralSearch *gsl = GNC_GENERAL_SEARCH (user_data);
    const gchar *entry_text = gtk_editable_get_text (GTK_EDITABLE (gsl->entry));
    const gchar *name = gtk_string_object_get_string (GTK_STRING_OBJECT (item));
    gchar *folded_entry;
    gchar *folded_name;
    gboolean matches;

    if (!entry_text || !*entry_text)
        return FALSE;

    folded_entry = g_utf8_casefold (entry_text, -1);
    folded_name = g_utf8_casefold (name, -1);
    matches = folded_entry && folded_name && g_strstr_len (folded_name, -1, folded_entry);
    g_free (folded_entry);
    g_free (folded_name);
    return matches;
}

static QofObject *
completion_object_at (GNCGeneralSearch *gsl, guint position)
{
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    GObject *row;
    QofObject *qofobject = NULL;

    if (position == GTK_INVALID_LIST_POSITION)
        return NULL;

    row = g_list_model_get_item (G_LIST_MODEL (priv->completion_filtered), position);
    if (row)
    {
        qofobject = g_object_get_qdata (row, completion_object_quark);
        g_object_unref (row);
    }
    return qofobject;
}

static gboolean
completion_select_position (GNCGeneralSearch *gsl, guint position)
{
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    QofObject *qofobject = completion_object_at (gsl, position);

    if (!qofobject)
        return FALSE;

    gtk_single_selection_set_selected (priv->completion_selection, position);
    gnc_general_search_set_selected (gsl, qofobject);
    gtk_popover_popdown (priv->completion_popover);
    return TRUE;
}

static void
completion_view_activate_cb (G_GNUC_UNUSED GtkListView *view, guint position,
                             gpointer user_data)
{
    completion_select_position (GNC_GENERAL_SEARCH (user_data), position);
}

static void
completion_update_matches (GNCGeneralSearch *gsl, gboolean present)
{
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    const gchar *entry_text = gtk_editable_get_text (GTK_EDITABLE (gsl->entry));

    if (!entry_text || !*entry_text)
    {
        gtk_single_selection_set_selected (priv->completion_selection,
                                           GTK_INVALID_LIST_POSITION);
        gtk_popover_popdown (priv->completion_popover);
        return;
    }

    gtk_filter_changed (GTK_FILTER (priv->completion_filter), GTK_FILTER_CHANGE_DIFFERENT);
    gtk_single_selection_set_selected (priv->completion_selection,
                                       GTK_INVALID_LIST_POSITION);
    if (present && g_list_model_get_n_items (G_LIST_MODEL (priv->completion_filtered)) > 0)
        gtk_popover_popup (priv->completion_popover);
    else if (g_list_model_get_n_items (G_LIST_MODEL (priv->completion_filtered)) == 0)
        gtk_popover_popdown (priv->completion_popover);
}

static void
entry_changed_cb (G_GNUC_UNUSED GtkEditable *editable, gpointer user_data)
{
    GNCGeneralSearch *gsl = GNC_GENERAL_SEARCH (user_data);

    if (!_PRIVATE (gsl)->updating_entry)
        completion_update_matches (gsl, TRUE);
}

static gboolean
entry_key_pressed_cb (G_GNUC_UNUSED GtkEventControllerKey *controller,
                      guint keyval, G_GNUC_UNUSED guint keycode,
                      G_GNUC_UNUSED GdkModifierType state, gpointer user_data)
{
    GNCGeneralSearch *gsl = GNC_GENERAL_SEARCH (user_data);
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    guint count = g_list_model_get_n_items (G_LIST_MODEL (priv->completion_filtered));
    guint selected = gtk_single_selection_get_selected (priv->completion_selection);

    if (keyval == GDK_KEY_Escape)
    {
        gtk_popover_popdown (priv->completion_popover);
        return TRUE;
    }
    if (keyval == GDK_KEY_Down || keyval == GDK_KEY_Up)
    {
        if (!count)
            return FALSE;
        if (selected == GTK_INVALID_LIST_POSITION)
            selected = keyval == GDK_KEY_Down ? 0 : count - 1;
        else if (keyval == GDK_KEY_Down)
            selected = (selected + 1) % count;
        else
            selected = selected ? selected - 1 : count - 1;
        gtk_single_selection_set_selected (priv->completion_selection, selected);
        gtk_popover_popup (priv->completion_popover);
        return TRUE;
    }
    if ((keyval == GDK_KEY_Return || keyval == GDK_KEY_KP_Enter) && count)
    {
        if (selected == GTK_INVALID_LIST_POSITION)
            selected = 0;
        return completion_select_position (gsl, selected);
    }
    return FALSE;
}

static void
entry_focus_leave_cb (G_GNUC_UNUSED GtkEventControllerFocus *controller,
                      gpointer user_data)
{
    GNCGeneralSearch *gsl = GNC_GENERAL_SEARCH (user_data);
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    GtkRoot *root = gtk_widget_get_root (gsl->entry);
    GtkWidget *focus = root ? gtk_root_get_focus (root) : NULL;
    const gchar *text = gnc_entry_get_text (GTK_ENTRY (gsl->entry));
    gchar *folded_text = g_utf8_casefold (text, -1);
    gpointer selected_item = NULL;

    if (focus && gtk_widget_is_ancestor (focus, GTK_WIDGET (priv->completion_popover)))
    {
        g_free (folded_text);
        return;
    }

    guint count = g_list_model_get_n_items (G_LIST_MODEL (priv->completion_store));

    if (gsl->selected_item)
    {
        gchar *selected_text = g_strdup (qof_object_printable (priv->type, gsl->selected_item));
        gchar *folded_selected = g_utf8_casefold (selected_text, -1);
        if (g_utf8_collate (folded_text, folded_selected) == 0)
            selected_item = gsl->selected_item;
        g_free (selected_text);
        g_free (folded_selected);
    }

    for (guint position = 0; !selected_item && position < count; position++)
    {
        GObject *row = g_list_model_get_item (G_LIST_MODEL (priv->completion_store), position);
        gchar *folded_name = g_utf8_casefold (gtk_string_object_get_string (GTK_STRING_OBJECT (row)), -1);
        if (g_utf8_collate (folded_text, folded_name) == 0)
            selected_item = g_object_get_qdata (row, completion_object_quark);
        g_free (folded_name);
        g_object_unref (row);
    }
    g_free (folded_text);
    gtk_popover_popdown (priv->completion_popover);
    gnc_general_search_set_selected (gsl, selected_item);
}
static void
reset_selection_text (GNCGeneralSearch *gsl)
{
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    const char *text = gsl->selected_item ? qof_object_printable (priv->type, gsl->selected_item) : "";

    priv->updating_entry = TRUE;
    gnc_entry_set_text (GTK_ENTRY (gsl->entry), text);
    priv->updating_entry = FALSE;
}

static void
refresh_handler (GHashTable *changes, gpointer data)
{
    GNCGeneralSearch *gsl = GNC_GENERAL_SEARCH (data);
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    const EventInfo *info;

    if (!changes)
        return;
    info = gnc_gui_get_entity_events (changes, &priv->guid);
    if (!info)
        return;
    if (info->event_mask & QOF_EVENT_DESTROY)
        gsl->selected_item = NULL;
    reset_selection_text (gsl);
}

static void
new_item_selected_cb (G_GNUC_UNUSED GtkWindow *dialog, gpointer item,
                      gpointer user_data)
{
    gnc_general_search_set_selected (GNC_GENERAL_SEARCH (user_data), item);
}

static void
on_close_cb (G_GNUC_UNUSED GtkWindow *dialog, gpointer user_data)
{
    _PRIVATE (GNC_GENERAL_SEARCH (user_data))->sw = NULL;
}

static void
search_cb (GtkButton *button, gpointer user_data)
{
    GNCGeneralSearch *gsl = GNC_GENERAL_SEARCH (user_data);
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    GNCSearchWindow *sw;

    if (priv->sw)
    {
        gnc_search_dialog_raise (priv->sw);
        return;
    }

    sw = priv->search_cb (gnc_ui_get_gtk_window (GTK_WIDGET (button)),
                          gsl->selected_item, priv->user_data);
    if (!sw)
        return;

    priv->sw = sw;
    gnc_search_dialog_connect_on_close (sw, G_CALLBACK (on_close_cb), gsl);
    gnc_search_dialog_set_select_cb (sw, new_item_selected_cb, gsl, gsl->allow_clear);
}

static void
create_completion (GNCGeneralSearch *gsl, QofIdTypeConst type, QofBook *book)
{
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);
    GtkListItemFactory *factory;
    GtkWidget *scroller;
    QofQuery *query;
    GList *list;

    priv->completion_store = g_list_store_new (GTK_TYPE_STRING_OBJECT);
    query = qof_query_create_for (type);
    qof_query_add_boolean_match (query, g_slist_prepend (NULL, QOF_PARAM_ACTIVE),
                                 TRUE, QOF_QUERY_AND);
    qof_query_set_book (query, book);
    list = qof_query_run (query);
    for (GList *node = list; node; node = node->next)
    {
        gchar *name = g_strdup (qof_object_printable (type, node->data));
        if (name)
        {
            GtkStringObject *row = gtk_string_object_new (name);
            g_object_set_qdata_full (G_OBJECT (row), completion_object_quark,
                                     g_object_ref (node->data), g_object_unref);
            g_list_store_append (priv->completion_store, row);
            g_object_unref (row);
            g_free (name);
        }
    }
    qof_query_destroy (query);

    priv->completion_filter = gtk_custom_filter_new (completion_filter_cb, gsl, NULL);
    priv->completion_filtered = gtk_filter_list_model_new (
        G_LIST_MODEL (g_object_ref (priv->completion_store)),
        GTK_FILTER (g_object_ref (priv->completion_filter)));
    priv->completion_selection = gtk_single_selection_new (
        G_LIST_MODEL (g_object_ref (priv->completion_filtered)));
    gtk_single_selection_set_autoselect (priv->completion_selection, FALSE);
    gtk_single_selection_set_can_unselect (priv->completion_selection, TRUE);

    factory = GTK_LIST_ITEM_FACTORY (gtk_signal_list_item_factory_new ());
    g_signal_connect (factory, "setup", G_CALLBACK (completion_row_setup_cb), NULL);
    g_signal_connect (factory, "bind", G_CALLBACK (completion_row_bind_cb), NULL);
    priv->completion_view = GTK_LIST_VIEW (gtk_list_view_new (
        GTK_SELECTION_MODEL (g_object_ref (priv->completion_selection)), factory));
    g_signal_connect (priv->completion_view, "activate",
                      G_CALLBACK (completion_view_activate_cb), gsl);

    scroller = gtk_scrolled_window_new ();
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scroller),
                                    GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (scroller),
                                   GTK_WIDGET (priv->completion_view));
    gtk_widget_set_size_request (scroller, 320, 180);

    priv->completion_popover = GTK_POPOVER (gtk_popover_new ());
    gtk_popover_set_autohide (priv->completion_popover, TRUE);
    gtk_popover_set_has_arrow (priv->completion_popover, FALSE);
    gtk_popover_set_position (priv->completion_popover, GTK_POS_BOTTOM);
    gtk_popover_set_child (priv->completion_popover, scroller);
    gtk_widget_set_parent (GTK_WIDGET (priv->completion_popover), gsl->entry);
}

static void
create_children (GNCGeneralSearch *gsl, const char *label, gboolean text_editable,
                 QofIdTypeConst type, QofBook *book)
{
    GtkEventController *focus_controller;
    GtkEventController *key_controller;

    gsl->entry = gtk_entry_new ();
    if (!text_editable)
        gtk_editable_set_editable (GTK_EDITABLE (gsl->entry), FALSE);
    gtk_widget_set_hexpand (gsl->entry, TRUE);
    gtk_box_append (GTK_BOX (gsl), gsl->entry);

    create_completion (gsl, type, book);
    g_signal_connect (gsl->entry, "changed", G_CALLBACK (entry_changed_cb), gsl);
    focus_controller = gtk_event_controller_focus_new ();
    gtk_widget_add_controller (gsl->entry, focus_controller);
    g_signal_connect (focus_controller, "leave", G_CALLBACK (entry_focus_leave_cb), gsl);
    key_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (gsl->entry, key_controller);
    g_signal_connect (key_controller, "key-pressed", G_CALLBACK (entry_key_pressed_cb), gsl);

    gsl->button = gtk_button_new_with_label (label);
    gtk_box_append (GTK_BOX (gsl), gsl->button);
    g_signal_connect (gsl->button, "clicked", G_CALLBACK (search_cb), gsl);
}

static void
completion_dispose (GNCGeneralSearchPrivate *priv)
{
    if (priv->completion_popover)
    {
        gtk_popover_popdown (priv->completion_popover);
        gtk_widget_unparent (GTK_WIDGET (priv->completion_popover));
        priv->completion_popover = NULL;
        priv->completion_view = NULL;
    }
    g_clear_object (&priv->completion_selection);
    g_clear_object (&priv->completion_filtered);
    g_clear_object (&priv->completion_filter);
    g_clear_object (&priv->completion_store);
}

static void
gnc_general_search_dispose (GObject *object)
{
    GNCGeneralSearch *gsl = GNC_GENERAL_SEARCH (object);
    GNCGeneralSearchPrivate *priv = _PRIVATE (gsl);

    if (priv->sw)
    {
        gnc_search_dialog_set_select_cb (priv->sw, NULL, NULL, FALSE);
        gnc_search_dialog_disconnect (priv->sw, gsl);
        priv->sw = NULL;
    }
    if (priv->component_id)
    {
        gnc_unregister_gui_component (priv->component_id);
        priv->component_id = 0;
    }
    completion_dispose (priv);
    gsl->entry = NULL;
    gsl->button = NULL;
    G_OBJECT_CLASS (gnc_general_search_parent_class)->dispose (object);
}

static void
gnc_general_search_class_init (GNCGeneralSearchClass *klass)
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);

    object_class->dispose = gnc_general_search_dispose;
    completion_object_quark = g_quark_from_static_string ("gnc-general-search-qof-object");
    general_search_signals[SELECTION_CHANGED] =
        g_signal_new ("changed", G_TYPE_FROM_CLASS (klass), G_SIGNAL_RUN_FIRST,
                      G_STRUCT_OFFSET (GNCGeneralSearchClass, changed), NULL, NULL,
                      g_cclosure_marshal_VOID__VOID, G_TYPE_NONE, 0);
    klass->changed = NULL;
}

static void
gnc_general_search_init (GNCGeneralSearch *gsl)
{
    gtk_orientable_set_orientation (GTK_ORIENTABLE (gsl), GTK_ORIENTATION_HORIZONTAL);
    gsl->selected_item = NULL;
}

GtkWidget *
gnc_general_search_new (QofIdTypeConst type, const char *label,
                        gboolean text_editable, GNCSearchCB search_cb,
                        gpointer user_data, QofBook *book)
{
    GNCGeneralSearch *gsl;
    GNCGeneralSearchPrivate *priv;
    const QofParam *get_guid;

    g_return_val_if_fail (type && label && search_cb, NULL);
    get_guid = qof_class_get_parameter (type, QOF_PARAM_GUID);
    g_return_val_if_fail (get_guid, NULL);

    gsl = g_object_new (GNC_TYPE_GENERAL_SEARCH, NULL);
    priv = _PRIVATE (gsl);
    priv->type = type;
    priv->search_cb = search_cb;
    priv->user_data = user_data;
    priv->get_guid = get_guid;
    create_children (gsl, label, text_editable, type, book);
    priv->component_id = gnc_register_gui_component (GNCGENERALSEARCH_CLASS,
                                                       refresh_handler, NULL, gsl);
    return GTK_WIDGET (gsl);
}

void
 gnc_general_search_grab_focus (GNCGeneralSearch *gsl)
{
    g_return_if_fail (GNC_IS_GENERAL_SEARCH (gsl));
    gtk_widget_grab_focus (gsl->entry);
}

void
gnc_general_search_set_selected (GNCGeneralSearch *gsl, gpointer selection)
{
    GNCGeneralSearchPrivate *priv;

    g_return_if_fail (GNC_IS_GENERAL_SEARCH (gsl));
    priv = _PRIVATE (gsl);
    if (selection != gsl->selected_item)
    {
        gsl->selected_item = selection;
        g_signal_emit (gsl, general_search_signals[SELECTION_CHANGED], 0);
    }
    reset_selection_text (gsl);
    gnc_gui_component_clear_watches (priv->component_id);
    if (selection && priv->get_guid)
    {
        GncGUID *guid = (GncGUID *)priv->get_guid->param_getfcn (
            gsl->selected_item, priv->get_guid);
        priv->guid = guid ? *guid : *guid_null ();
        gnc_gui_component_watch_entity (priv->component_id, &priv->guid,
                                        QOF_EVENT_MODIFY | QOF_EVENT_DESTROY);
    }
    else
        priv->guid = *guid_null ();
}

gpointer
gnc_general_search_get_selected (GNCGeneralSearch *gsl)
{
    g_return_val_if_fail (GNC_IS_GENERAL_SEARCH (gsl), NULL);
    return gsl->selected_item;
}

void
gnc_general_search_allow_clear (GNCGeneralSearch *gsl, gboolean allow_clear)
{
    g_return_if_fail (GNC_IS_GENERAL_SEARCH (gsl));
    gsl->allow_clear = allow_clear;
}