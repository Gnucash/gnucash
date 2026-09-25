/*
 * gnc-import-assistant.c -- GtkWindow/GtkStack controller for import wizards
 *
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 2 of the License, or (at your
 * option) any later version.
 */

#include <config.h>

#include "gnc-import-assistant.h"

typedef struct
{
    GtkStack *stack;
    GtkWidget *title;
    GtkBox *actions;
    GtkWidget *back;
    GtkWidget *next;
    GtkWidget *apply;
    GtkWidget *cancel;
    GtkWidget *close;
    GPtrArray *pages;
    GArray *complete;
    GArray *actions_for_page;
    GArray *history;
    int current_page;
    gboolean committed;
    gboolean closing;
    GncImportAssistantPrepareFunc prepare;
    GncImportAssistantActionFunc apply_cb;
    GncImportAssistantActionFunc cancel_cb;
    GncImportAssistantActionFunc close_cb;
    GncImportAssistantForwardFunc forward;
    gpointer callback_data;
    gpointer forward_data;
    GDestroyNotify forward_destroy;
} GncImportAssistantState;

#define GNC_IMPORT_ASSISTANT_STATE_KEY "gnc-import-assistant-state"

static GncImportAssistantState *
assistant_state (GncImportAssistant *assistant)
{
    return assistant ? g_object_get_data (G_OBJECT (assistant),
                                          GNC_IMPORT_ASSISTANT_STATE_KEY) : NULL;
}

static int
page_index (GncImportAssistantState *state, GtkWidget *page)
{
    guint index;

    if (!state || !page)
        return -1;
    for (index = 0; index < state->pages->len; ++index)
        if (g_ptr_array_index (state->pages, index) == page)
            return (int)index;
    return -1;
}

static void
update_navigation (GncImportAssistant *assistant)
{
    GncImportAssistantState *state = assistant_state (assistant);
    GncImportAssistantPageAction action;
    gboolean complete;
    GtkWidget *default_widget = NULL;

    if (!state || state->current_page < 0 ||
        state->current_page >= (int)state->pages->len)
        return;

    action = g_array_index (state->actions_for_page,
                            GncImportAssistantPageAction, state->current_page);
    complete = g_array_index (state->complete, gboolean, state->current_page);
    gtk_widget_set_visible (state->back, !state->committed &&
                            state->history->len > 0 &&
                            action != GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gtk_widget_set_sensitive (state->back, !state->committed &&
                              state->history->len > 0 &&
                              action != GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gtk_widget_set_visible (state->next, action == GNC_IMPORT_ASSISTANT_PAGE_NEXT);
    gtk_widget_set_sensitive (state->next, complete &&
                              action == GNC_IMPORT_ASSISTANT_PAGE_NEXT);
    gtk_widget_set_visible (state->apply, action == GNC_IMPORT_ASSISTANT_PAGE_APPLY);
    gtk_widget_set_sensitive (state->apply, complete &&
                               action == GNC_IMPORT_ASSISTANT_PAGE_APPLY);
    gtk_widget_set_visible (state->cancel, action != GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gtk_widget_set_visible (state->close, action == GNC_IMPORT_ASSISTANT_PAGE_CLOSE);
    gtk_widget_set_sensitive (state->close, action == GNC_IMPORT_ASSISTANT_PAGE_CLOSE);

    if (action == GNC_IMPORT_ASSISTANT_PAGE_CLOSE)
        default_widget = state->close;
    else if (action == GNC_IMPORT_ASSISTANT_PAGE_APPLY && complete)
        default_widget = state->apply;
    else if (action == GNC_IMPORT_ASSISTANT_PAGE_NEXT && complete)
        default_widget = state->next;
    gtk_window_set_default_widget (GTK_WINDOW (assistant), default_widget);
}

static void
show_page (GncImportAssistant *assistant, int page, gboolean add_history)
{
    GncImportAssistantState *state = assistant_state (assistant);
    GtkWidget *child;
    GtkStackPage *stack_page;
    const char *title;

    if (!state || page < 0 || page >= (int)state->pages->len)
        return;
    if (add_history && state->current_page >= 0 &&
        state->current_page != page)
        g_array_append_val (state->history, state->current_page);
    state->current_page = page;
    child = g_ptr_array_index (state->pages, page);
    stack_page = gtk_stack_get_page (state->stack, child);
    title = stack_page ? gtk_stack_page_get_title (stack_page) : NULL;
    if (state->title)
        gtk_label_set_text (GTK_LABEL (state->title), title ? title : "");
    gtk_stack_set_visible_child (state->stack, child);
    if (state->prepare)
        state->prepare (assistant, child, state->callback_data);
    update_navigation (assistant);
}

static void
back_clicked (GtkButton *button, gpointer user_data)
{
    gnc_import_assistant_previous_page (GNC_IMPORT_ASSISTANT (user_data));
    (void)button;
}

static void
next_clicked (GtkButton *button, gpointer user_data)
{
    gnc_import_assistant_next_page (GNC_IMPORT_ASSISTANT (user_data));
    (void)button;
}

static void
apply_clicked (GtkButton *button, gpointer user_data)
{
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT (user_data);
    GncImportAssistantState *state = assistant_state (assistant);

    if (state && state->apply_cb)
        state->apply_cb (assistant, state->callback_data);
    (void)button;
}

static void
cancel_clicked (GtkButton *button, gpointer user_data)
{
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT (user_data);
    GncImportAssistantState *state = assistant_state (assistant);

    if (state && state->cancel_cb)
        state->cancel_cb (assistant, state->callback_data);
    (void)button;
}

static void
close_clicked (GtkButton *button, gpointer user_data)
{
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT (user_data);
    GncImportAssistantState *state = assistant_state (assistant);

    if (state && state->close_cb)
        state->close_cb (assistant, state->callback_data);
    (void)button;
}

static gboolean
close_requested (GtkWindow *window, gpointer user_data)
{
    GncImportAssistant *assistant = GNC_IMPORT_ASSISTANT (user_data);
    GncImportAssistantState *state = assistant_state (assistant);

    if (state && !state->closing && state->cancel_cb)
    {
        state->closing = TRUE;
        g_object_ref (assistant);
        state->cancel_cb (assistant, state->callback_data);
        state = assistant_state (assistant);
        if (state)
            state->closing = FALSE;
        g_object_unref (assistant);
    }
    (void)window;
    return TRUE;
}

static gboolean
escape_pressed (GtkWidget *widget, GVariant *args, gpointer user_data)
{
    cancel_clicked (NULL, user_data);
    (void)widget;
    (void)args;
    return TRUE;
}

static void
state_destroyed (gpointer user_data)
{
    GncImportAssistantState *state = user_data;

    if (!state)
        return;
    if (state->forward_destroy)
        state->forward_destroy (state->forward_data);
    g_clear_pointer (&state->pages, g_ptr_array_unref);
    g_clear_pointer (&state->complete, g_array_unref);
    g_clear_pointer (&state->actions_for_page, g_array_unref);
    g_clear_pointer (&state->history, g_array_unref);
    g_free (state);
}

GncImportAssistant *
gnc_import_assistant_new (GtkWindow *window, GtkStack *stack, GtkWidget *title,
                          GtkBox *actions, GtkWidget *back, GtkWidget *next,
                          GtkWidget *apply, GtkWidget *cancel, GtkWidget *close)
{
    GncImportAssistantState *state;
    GtkWidget *child;
    GtkShortcutController *shortcuts;

    g_return_val_if_fail (GTK_IS_WINDOW (window), NULL);
    g_return_val_if_fail (GTK_IS_STACK (stack), NULL);

    state = g_new0 (GncImportAssistantState, 1);
    state->stack = stack;
    state->title = title;
    state->actions = actions;
    state->pages = g_ptr_array_new ();
    state->complete = g_array_new (FALSE, FALSE, sizeof (gboolean));
    state->actions_for_page = g_array_new (FALSE, FALSE,
                                           sizeof (GncImportAssistantPageAction));
    state->history = g_array_new (FALSE, FALSE, sizeof (int));
    state->current_page = -1;

    child = gtk_widget_get_first_child (GTK_WIDGET (stack));
    while (child)
    {
        gboolean complete = FALSE;
        GncImportAssistantPageAction action = GNC_IMPORT_ASSISTANT_PAGE_NEXT;

        g_ptr_array_add (state->pages, child);
        g_array_append_val (state->complete, complete);
        g_array_append_val (state->actions_for_page, action);
        child = gtk_widget_get_next_sibling (child);
    }

    state->back = back;
    state->next = next;
    state->apply = apply;
    state->cancel = cancel;
    state->close = close;
    if (!state->back || !state->next || !state->apply || !state->cancel ||
        !state->close)
    {
        state_destroyed (state);
        return NULL;
    }
    g_object_set_data_full (G_OBJECT (window), GNC_IMPORT_ASSISTANT_STATE_KEY,
                            state, state_destroyed);
    g_signal_connect (state->back, "clicked", G_CALLBACK (back_clicked), window);
    g_signal_connect (state->next, "clicked", G_CALLBACK (next_clicked), window);
    g_signal_connect (state->apply, "clicked", G_CALLBACK (apply_clicked), window);
    g_signal_connect (state->cancel, "clicked", G_CALLBACK (cancel_clicked), window);
    g_signal_connect (state->close, "clicked", G_CALLBACK (close_clicked), window);
    g_signal_connect (window, "close-request", G_CALLBACK (close_requested), window);
    shortcuts = GTK_SHORTCUT_CONTROLLER (gtk_shortcut_controller_new ());
    gtk_shortcut_controller_add_shortcut (
        shortcuts, gtk_shortcut_new (gtk_keyval_trigger_new (GDK_KEY_Escape, 0),
                                     gtk_callback_action_new (escape_pressed, window, NULL)));
    gtk_widget_add_controller (GTK_WIDGET (window), GTK_EVENT_CONTROLLER (shortcuts));
    return GNC_IMPORT_ASSISTANT (window);
}

void
gnc_import_assistant_set_callbacks (GncImportAssistant *assistant,
                                    GncImportAssistantPrepareFunc prepare,
                                    GncImportAssistantActionFunc apply,
                                    GncImportAssistantActionFunc cancel,
                                    GncImportAssistantActionFunc close,
                                    gpointer user_data)
{
    GncImportAssistantState *state = assistant_state (assistant);

    if (!state)
        return;
    state->prepare = prepare;
    state->apply_cb = apply;
    state->cancel_cb = cancel;
    state->close_cb = close;
    state->callback_data = user_data;
    if (state->current_page < 0 && state->pages->len)
        show_page (assistant, 0, FALSE);
}

void
gnc_import_assistant_set_page_action (GncImportAssistant *assistant, int page,
                                      GncImportAssistantPageAction action)
{
    GncImportAssistantState *state = assistant_state (assistant);

    if (!state || page < 0 || page >= (int)state->actions_for_page->len)
        return;
    g_array_index (state->actions_for_page, GncImportAssistantPageAction, page) = action;
    update_navigation (assistant);
}

void
gnc_import_assistant_set_forward_page_func (GncImportAssistant *assistant,
                                             GncImportAssistantForwardFunc func,
                                             gpointer data,
                                             GDestroyNotify destroy)
{
    GncImportAssistantState *state = assistant_state (assistant);

    if (!state)
        return;
    if (state->forward_destroy)
        state->forward_destroy (state->forward_data);
    state->forward = func;
    state->forward_data = data;
    state->forward_destroy = destroy;
}

int
gnc_import_assistant_get_current_page (GncImportAssistant *assistant)
{
    GncImportAssistantState *state = assistant_state (assistant);
    return state ? state->current_page : -1;
}

int
gnc_import_assistant_get_n_pages (GncImportAssistant *assistant)
{
    GncImportAssistantState *state = assistant_state (assistant);
    return state ? (int)state->pages->len : 0;
}

GtkWidget *
gnc_import_assistant_get_nth_page (GncImportAssistant *assistant, int page)
{
    GncImportAssistantState *state = assistant_state (assistant);
    return state && page >= 0 && page < (int)state->pages->len ?
        g_ptr_array_index (state->pages, page) : NULL;
}

void
gnc_import_assistant_set_current_page (GncImportAssistant *assistant, int page)
{
    show_page (assistant, page, FALSE);
}

void
gnc_import_assistant_next_page (GncImportAssistant *assistant)
{
    GncImportAssistantState *state = assistant_state (assistant);
    int page;

    if (!state || state->current_page < 0 ||
        !g_array_index (state->complete, gboolean, state->current_page))
        return;
    page = state->forward ? state->forward (state->current_page,
                                            state->forward_data) :
                            state->current_page + 1;
    show_page (assistant, page, TRUE);
}

void
gnc_import_assistant_previous_page (GncImportAssistant *assistant)
{
    GncImportAssistantState *state = assistant_state (assistant);
    int page;

    if (!state || !state->history->len || state->committed)
        return;
    page = g_array_index (state->history, int, state->history->len - 1);
    g_array_remove_index (state->history, state->history->len - 1);
    show_page (assistant, page, FALSE);
}

void
gnc_import_assistant_set_page_complete (GncImportAssistant *assistant,
                                        GtkWidget *page, gboolean complete)
{
    GncImportAssistantState *state = assistant_state (assistant);
    int index = page_index (state, page);

    if (index < 0)
        return;
    g_array_index (state->complete, gboolean, index) = complete;
    update_navigation (assistant);
}

void
gnc_import_assistant_set_page_title (GncImportAssistant *assistant,
                                     GtkWidget *page, const char *title)
{
    GncImportAssistantState *state = assistant_state (assistant);
    GtkStackPage *stack_page;

    if (!state || !page)
        return;
    stack_page = gtk_stack_get_page (state->stack, page);
    if (!stack_page)
        return;
    gtk_stack_page_set_title (stack_page, title);
    if (page == gnc_import_assistant_get_nth_page (assistant,
                                                    state->current_page) &&
        state->title)
        gtk_label_set_text (GTK_LABEL (state->title), title ? title : "");
}

void
gnc_import_assistant_commit (GncImportAssistant *assistant)
{
    GncImportAssistantState *state = assistant_state (assistant);
    if (state)
    {
        state->committed = TRUE;
        update_navigation (assistant);
    }
}

void
gnc_import_assistant_add_action_widget (GncImportAssistant *assistant,
                                         GtkWidget *widget)
{
    GncImportAssistantState *state = assistant_state (assistant);
    GtkWidget *spacer;

    if (!state || !state->actions || !widget)
        return;

    gtk_widget_set_halign (GTK_WIDGET (state->actions), GTK_ALIGN_FILL);
    gtk_widget_set_hexpand (GTK_WIDGET (state->actions), TRUE);
    spacer = g_object_get_data (G_OBJECT (state->actions),
                                "gnc-import-assistant-action-spacer");
    if (!spacer)
    {
        spacer = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
        gtk_widget_set_hexpand (spacer, TRUE);
        gtk_box_prepend (state->actions, spacer);
        g_object_set_data (G_OBJECT (state->actions),
                           "gnc-import-assistant-action-spacer", spacer);
    }

    gtk_box_prepend (state->actions, widget);
}

void
gnc_import_assistant_remove_action_widget (GncImportAssistant *assistant,
                                            GtkWidget *widget)
{
    GncImportAssistantState *state = assistant_state (assistant);
    if (state && state->actions && widget &&
        gtk_widget_get_parent (widget) == GTK_WIDGET (state->actions))
        gtk_box_remove (state->actions, widget);
}
