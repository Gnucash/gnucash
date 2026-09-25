/*
 * gnc-import-assistant.h -- GtkWindow/GtkStack controller for import wizards
 *
 * Copyright (C) 2026
 *
 * This program is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation, either version 2 of the License, or (at your
 * option) any later version.
 */

#ifndef GNC_IMPORT_ASSISTANT_H
#define GNC_IMPORT_ASSISTANT_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

/* A GncImportAssistant is a GtkWindow with an associated GtkStack state
 * controller. Keeping it typedef-compatible with GtkWindow makes the
 * transition explicit without recreating the removed legacy widget type. */
typedef GtkWindow GncImportAssistant;

#define GNC_IMPORT_ASSISTANT(obj) (GTK_WINDOW (obj))

typedef enum
{
    GNC_IMPORT_ASSISTANT_PAGE_NEXT,
    GNC_IMPORT_ASSISTANT_PAGE_APPLY,
    GNC_IMPORT_ASSISTANT_PAGE_CLOSE,
} GncImportAssistantPageAction;

typedef void (*GncImportAssistantPrepareFunc) (GncImportAssistant *assistant,
                                                GtkWidget *page,
                                                gpointer user_data);
typedef void (*GncImportAssistantActionFunc) (GncImportAssistant *assistant,
                                               gpointer user_data);
typedef int (*GncImportAssistantForwardFunc) (int current_page,
                                               gpointer user_data);

GncImportAssistant *gnc_import_assistant_new (GtkWindow *window,
                                               GtkStack *stack,
                                               GtkWidget *title,
                                               GtkBox *actions,
                                               GtkWidget *back,
                                               GtkWidget *next,
                                               GtkWidget *apply,
                                               GtkWidget *cancel,
                                               GtkWidget *close);
void gnc_import_assistant_set_callbacks (GncImportAssistant *assistant,
                                         GncImportAssistantPrepareFunc prepare,
                                         GncImportAssistantActionFunc apply,
                                         GncImportAssistantActionFunc cancel,
                                         GncImportAssistantActionFunc close,
                                         gpointer user_data);
void gnc_import_assistant_set_page_action (GncImportAssistant *assistant,
                                           int page,
                                           GncImportAssistantPageAction action);
void gnc_import_assistant_set_forward_page_func (GncImportAssistant *assistant,
                                                  GncImportAssistantForwardFunc func,
                                                  gpointer data,
                                                  GDestroyNotify destroy);
int gnc_import_assistant_get_current_page (GncImportAssistant *assistant);
int gnc_import_assistant_get_n_pages (GncImportAssistant *assistant);
GtkWidget *gnc_import_assistant_get_nth_page (GncImportAssistant *assistant,
                                              int page);
void gnc_import_assistant_set_current_page (GncImportAssistant *assistant,
                                            int page);
void gnc_import_assistant_next_page (GncImportAssistant *assistant);
void gnc_import_assistant_previous_page (GncImportAssistant *assistant);
void gnc_import_assistant_set_page_complete (GncImportAssistant *assistant,
                                             GtkWidget *page,
                                             gboolean complete);
void gnc_import_assistant_set_page_title (GncImportAssistant *assistant,
                                          GtkWidget *page,
                                          const char *title);
void gnc_import_assistant_commit (GncImportAssistant *assistant);
void gnc_import_assistant_add_action_widget (GncImportAssistant *assistant,
                                              GtkWidget *widget);
void gnc_import_assistant_remove_action_widget (GncImportAssistant *assistant,
                                                 GtkWidget *widget);

G_END_DECLS

#endif /* GNC_IMPORT_ASSISTANT_H */
