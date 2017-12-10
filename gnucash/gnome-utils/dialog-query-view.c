/*
 * dialog-query-view.c -- a simple dialog to display a query view and
 *                        allow users to select items (or close the view)
 *
 * Created By:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
 * Copyright (c) 2012 Robert Fewell
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#include <config.h>
#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "qof.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"

#include "dialog-query-view.h"
#include "gnc-query-view.h"

struct _DialogQueryView
{
    GtkWidget            * dialog;
    GtkWidget            * label;
    GtkWidget            * qview;
    GtkWidget            * button_box;
    GNCDisplayViewButton * buttons;
    gpointer               user_data;
    GList                * books;
    gint                   component_id;
};

static void
dqv_clear_booklist (DialogQueryView *dqv)
{
    GList *node;

    g_return_if_fail (dqv);

    for (node = dqv->books; node; node = node->next)
        guid_free ((GncGUID*)node->data);
    g_list_free (dqv->books);
    dqv->books = NULL;
}

static void
dqv_build_booklist (DialogQueryView *dqv, Query *q)
{
    GList *node;

    g_return_if_fail (dqv);

    for (node = qof_query_get_books(q); node; node = node->next)
    {
        QofBook *book = node->data;
        GncGUID *guid = guid_malloc();
        *guid = *(qof_book_get_guid(book));
        dqv->books = g_list_prepend(dqv->books, guid);
    }
}

static void
gnc_dialog_query_run_callback (GNCDisplayViewButton *cb, gpointer item,
                               DialogQueryView *dqv)
{
    if (!cb)
        return;

    if (cb->cb_fcn)
        (cb->cb_fcn)(GTK_WINDOW (dqv->dialog), item, dqv->user_data);
}

static void
gnc_dialog_query_view_button_clicked (GtkButton *button, DialogQueryView *dqv)
{
    GNCDisplayViewButton *cb;
    gpointer entry;

    g_return_if_fail (dqv);
    entry = gnc_query_view_get_selected_entry (GNC_QUERY_VIEW (dqv->qview));
    if (!entry)
        return;

    cb = g_object_get_data (G_OBJECT (button), "data");
    g_return_if_fail (cb);

    gnc_dialog_query_run_callback (cb, entry, dqv);
}

static void
gnc_dialog_query_view_double_click_entry (GNCQueryView *qview, gpointer item,
                                          gpointer user_data)
{
    DialogQueryView *dqv = user_data;

    g_return_if_fail (dqv);
    g_return_if_fail (item);

    if (!dqv->buttons)
        return;

    gnc_dialog_query_run_callback (dqv->buttons, item, dqv);
}

static int
gnc_dialog_query_view_delete_cb (GtkDialog *dialog, GdkEvent  *event, DialogQueryView *dqv)
{
    g_return_val_if_fail (dqv, TRUE);

    gnc_unregister_gui_component (dqv->component_id);

    /* destroy the book list */
    dqv_clear_booklist (dqv);

    /* Destroy and exit */
    gtk_widget_destroy(dqv->dialog);
    g_free (dqv);
    return FALSE;
}

static void
close_handler (gpointer data)
{
    DialogQueryView *dqv = data;

    g_return_if_fail (dqv);
    gnc_dialog_query_view_delete_cb (GTK_DIALOG(dqv->dialog), NULL, dqv);
}

static void
gnc_dialog_query_view_refresh_handler (GHashTable *changes, gpointer user_data)
{
    DialogQueryView *dqv = (DialogQueryView *)user_data;
    const EventInfo *info;
    GList *node;

    if (changes)
    {
        for (node = dqv->books; node; node = node->next)
        {
            info = gnc_gui_get_entity_events (changes, (const GncGUID*)(node->data));
            if (info && (info->event_mask & QOF_EVENT_DESTROY))
            {
                gnc_close_gui_component (dqv->component_id);
                return;
            }
        }
    }
}

static void
gnc_dialog_query_view_close (GtkButton *button, DialogQueryView *dqv)
{
    /* Don't select anything */
    gnc_dialog_query_view_destroy (dqv);
}

/*****************************************************************/
/* PUBLIC INTERFACES */

DialogQueryView *
gnc_dialog_query_view_new (GtkWindow *parent, GList *param_list, Query *q)
{
    GtkBuilder  *builder;
    DialogQueryView *dqv;
    GtkWidget *result_hbox, *close, *scrollWin, *frame;
    GList *node;

    dqv = g_new0 (DialogQueryView, 1);
    builder = gtk_builder_new();
    gnc_builder_add_from_file (builder, "dialog-query-view.glade", "query_view_dialog");

    /* Grab the dialog, save the dialog info */
    dqv->dialog = GTK_WIDGET(gtk_builder_get_object (builder, "query_view_dialog"));
    g_object_set_data (G_OBJECT (dqv->dialog), "dialog-info", dqv);
    gtk_window_set_transient_for(GTK_WINDOW(dqv->dialog), parent);

    // Set the style context for this dialog so it can be easily manipulated with css
    gnc_widget_set_style_context (GTK_WIDGET(dqv->dialog), "GncQueryViewDialog");

    /* grab the widgets */
    dqv->label = GTK_WIDGET(gtk_builder_get_object (builder, "dialog_label"));
    result_hbox = GTK_WIDGET(gtk_builder_get_object (builder, "result_hbox"));
    close = GTK_WIDGET(gtk_builder_get_object (builder, "close_button"));

    /* build the query list */
    dqv->qview = gnc_query_view_new (param_list, q);

    frame = gtk_frame_new(NULL);

    scrollWin = gtk_scrolled_window_new (NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (scrollWin),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_container_set_border_width(GTK_CONTAINER(scrollWin), 5);

    gtk_container_add(GTK_CONTAINER(scrollWin), dqv->qview);
    gtk_container_add(GTK_CONTAINER(frame), scrollWin);

    gtk_box_pack_start (GTK_BOX (result_hbox), frame, TRUE, TRUE, 3);

    /* Create the button_box */
    dqv->button_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 2);
    gtk_box_set_homogeneous (GTK_BOX (dqv->button_box), FALSE);

    gtk_box_pack_start (GTK_BOX (result_hbox), dqv->button_box, FALSE, FALSE, 3);

    /* connect the double-click signal of the qview */
    g_signal_connect (G_OBJECT (dqv->qview), "double_click_entry",
                      G_CALLBACK(gnc_dialog_query_view_double_click_entry), dqv);

    /* connect to the close button */
    g_signal_connect (G_OBJECT (close), "clicked",
                      G_CALLBACK (gnc_dialog_query_view_close), dqv);

    /* connect to the cleanup */
    g_signal_connect (G_OBJECT (dqv->dialog), "delete_event",
                      G_CALLBACK (gnc_dialog_query_view_delete_cb), dqv);

    /* register ourselves */
    dqv->component_id = gnc_register_gui_component ("GNC Dialog Query View",
                        gnc_dialog_query_view_refresh_handler,
                        close_handler, dqv);

    /* Build the book list */
    dqv_build_booklist (dqv, q);

    /* and register the books */
    for (node = dqv->books; node; node = node->next)
        gnc_gui_component_watch_entity (dqv->component_id, (GncGUID*)node->data,
                                        QOF_EVENT_DESTROY);

    g_object_unref(G_OBJECT(builder));

    return dqv;
}

void gnc_dialog_query_view_set_title (DialogQueryView *dqv, const char *title)
{
    if (!dqv || !title) return;
    gtk_window_set_title (GTK_WINDOW (dqv->dialog), title);
}

void gnc_dialog_query_view_set_label (DialogQueryView *dqv, const char *label)
{
    if (!dqv || !label) return;
    gtk_label_set_text (GTK_LABEL(dqv->label), label);
}

void gnc_dialog_query_view_set_buttons (DialogQueryView *dqv,
                                        GNCDisplayViewButton *buttons,
                                        gpointer user_data)
{
    GtkWidget *button;
    int i;

    if (!dqv || !buttons) return;
    g_return_if_fail (dqv->buttons == NULL);

    dqv->buttons = buttons;
    dqv->user_data = user_data;

    /* build up the buttons */
    for (i = 0; buttons[i].label; i++)
    {
        /* Note: The "label" member of the GNCDisplayListButton still
         * isn't translated. Hence, we must translate it here. */
        button = gtk_button_new_with_label (_(buttons[i].label));
        g_object_set_data (G_OBJECT (button), "data", &(dqv->buttons[i]));
        g_signal_connect (G_OBJECT (button), "clicked",
                          G_CALLBACK(gnc_dialog_query_view_button_clicked), dqv);
        gtk_box_pack_start (GTK_BOX (dqv->button_box), button, FALSE, FALSE, 3);
    }
}

void gnc_dialog_query_view_set_numerics (DialogQueryView *dqv, gboolean abs,
        gboolean inv_sort)
{
    if (!dqv) return;

    gnc_query_view_set_numerics (GNC_QUERY_VIEW(dqv->qview), abs, inv_sort);
}

void gnc_dialog_query_view_refresh (DialogQueryView *dqv)
{
    if (!dqv) return;

    gnc_query_view_refresh (GNC_QUERY_VIEW(dqv->qview));
    gtk_widget_show_all (dqv->dialog);
}

void gnc_dialog_query_view_destroy (DialogQueryView *dqv)
{
    if (!dqv) return;
    gnc_close_gui_component (dqv->component_id);
}

DialogQueryView *
gnc_dialog_query_view_create (GtkWindow *parent, GList *param_list, Query *q,
                              const char *title, const char *label,
                              gboolean abs, gboolean inv_sort,
                              gint sort_column, GtkSortType order,
                              GNCDisplayViewButton *buttons, gpointer user_data)
{
    DialogQueryView *dqv;

    if (!param_list || !q)
        return NULL;

    dqv = gnc_dialog_query_view_new (parent, param_list, q);
    if (!dqv)
        return NULL;

    if (title)
        gnc_dialog_query_view_set_title (dqv, title);

    if (label)
        gnc_dialog_query_view_set_label (dqv, label);

    gnc_dialog_query_view_set_numerics (dqv, abs, inv_sort);

    if (buttons)
        gnc_dialog_query_view_set_buttons (dqv, buttons, user_data);

    gnc_dialog_query_view_refresh (dqv);

    /* Set the sort order */
    gnc_query_sort_order (GNC_QUERY_VIEW (dqv->qview), sort_column, order);

    /* Unselect all rows */
    gnc_query_view_unselect_all (GNC_QUERY_VIEW (dqv->qview));

    return dqv;
}
