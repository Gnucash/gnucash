/*
 * dialog-query-list.c -- a simple dialog to display a querylist and
 *                        allow users to select items (or close the list)
 *
 * Created By:	Derek Atkins <derek@ihtfp.com>
 * Copyright (c) 2003 Derek Atkins <warlord@MIT.EDU>
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

#include "config.h"
#include <gtk/gtk.h>

#include "gnc-book.h"
#include "QueryNew.h"

#include "dialog-utils.h"
#include "gnc-component-manager.h"

#include "dialog-query-list.h"
#include "gnc-query-list.h"

struct _DialogQueryList
{
    GtkWidget *	dialog;
    GtkWidget *	label;
    GtkWidget *	qlist;
    GtkWidget *	button_box;

    GNCDisplayListButton *	buttons;
    gpointer		user_data;

    GList *	books;

    gint		component_id;
};

static void
dql_clear_booklist (DialogQueryList *dql)
{
    GList *node;

    g_return_if_fail (dql);

    for (node = dql->books; node; node = node->next)
        xaccGUIDFree ((GUID*)node->data);
    g_list_free (dql->books);
    dql->books = NULL;
}

static void
dql_build_booklist (DialogQueryList *dql, Query *q)
{
    GList *node;

    g_return_if_fail (dql);

    for (node = gncQueryGetBooks(q); node; node = node->next)
    {
        GNCBook *book = node->data;
        GUID *guid = xaccGUIDMalloc();
        *guid = *(gnc_book_get_guid(book));
        dql->books = g_list_prepend(dql->books, guid);
    }
}

static void
gnc_dialog_query_run_callback (GNCDisplayListButton *cb, gpointer item,
                               DialogQueryList *dql)
{
    if (!cb)
        return;

    if (cb->cb_fcn)
        (cb->cb_fcn)(item, dql->user_data);
}

static void
gnc_dialog_query_list_button_clicked (GtkButton *button, DialogQueryList *dql)
{
    GNCDisplayListButton *cb;
    gpointer current;

    g_return_if_fail (dql);
    current = gnc_query_list_get_current_entry (GNC_QUERY_LIST (dql->qlist));
    if (!current)
        return;

    cb = g_object_get_data (G_OBJECT (button), "data");
    g_return_if_fail (cb);

    gnc_dialog_query_run_callback (cb, current, dql);
}

static void
gnc_dialog_query_list_double_click_entry (GNCQueryList *list, gpointer item,
        gpointer user_data)
{
    DialogQueryList *dql = user_data;

    g_return_if_fail (dql);
    g_return_if_fail (item);

    if (!dql->buttons)
        return;

    gnc_dialog_query_run_callback (dql->buttons, item, dql);
}

static int
gnc_dialog_query_list_delete_cb (GtkDialog *dialog, GdkEvent  *event, DialogQueryList *dql)
{
    g_return_val_if_fail (dql, TRUE);

    gnc_unregister_gui_component (dql->component_id);

    /* XXX: Clear/destroy the param_list? */

    /* destroy the book list */
    dql_clear_booklist (dql);

    /* Destroy and exit */
    gtk_widget_destroy(dql->dialog);
    g_free (dql);
    return FALSE;
}

static void
close_handler (gpointer data)
{
    DialogQueryList * dql = data;

    g_return_if_fail (dql);
    gnc_dialog_query_list_delete_cb (GTK_DIALOG(dql->dialog), NULL, dql);
}

static void
gnc_dialog_query_list_refresh_handler (GHashTable *changes, gpointer user_data)
{
    DialogQueryList *dql = (DialogQueryList *)user_data;
    const EventInfo *info;
    GList *node;

    if (changes)
    {
        for (node = dql->books; node; node = node->next)
        {
            info = gnc_gui_get_entity_events (changes, (const GUID*)(node->data));
            if (info && (info->event_mask & QOF_EVENT_DESTROY))
            {
                gnc_close_gui_component (dql->component_id);
                return;
            }
        }
    }
}

static void
gnc_dialog_query_list_close (GtkButton *button, DialogQueryList *dql)
{
    /* Don't select anything */
    gnc_dialog_query_list_destroy (dql);
}

/*****************************************************************/
/* PUBLIC INTERFACES */

DialogQueryList *
gnc_dialog_query_list_new (GList *param_list, Query *q)
{
    GladeXML *xml;
    DialogQueryList *dql;
    GtkWidget *scroller, *close;
    GList *node;

    dql = g_new0 (DialogQueryList, 1);
    xml = gnc_glade_xml_new ("dialog-query-list.glade", "Query List Dialog");

    /* Grab the dialog, save the dialog info */
    dql->dialog = glade_xml_get_widget (xml, "Query List Dialog");
    g_object_set_data (G_OBJECT (dql->dialog), "dialog-info", dql);

    /* grab the widgets */
    dql->label = glade_xml_get_widget (xml, "dialog_label");
    dql->button_box = glade_xml_get_widget (xml, "button_vbox");
    scroller = glade_xml_get_widget (xml, "result_scroller");
    close = glade_xml_get_widget (xml, "close_button");

    /* build the query list */
    dql->qlist = gnc_query_list_new (param_list, q);
    gtk_container_add (GTK_CONTAINER (scroller), dql->qlist);

    /* connect the double-click signal of the qlist */
    g_signal_connect (G_OBJECT (dql->qlist), "double_click_entry",
                      G_CALLBACK(gnc_dialog_query_list_double_click_entry), dql);


    /* connect to the close button */
    g_signal_connect (G_OBJECT (close), "clicked",
                      G_CALLBACK (gnc_dialog_query_list_close), dql);

    /* connect to the cleanup */
    g_signal_connect (G_OBJECT (dql->dialog), "delete_event",
                      G_CALLBACK (gnc_dialog_query_list_delete_cb), dql);


    /* register ourselves */
    dql->component_id =
        gnc_register_gui_component ("GNC Dialog Query List",
                                    gnc_dialog_query_list_refresh_handler,
                                    close_handler, dql);

    /* Build the book list */
    dql_build_booklist (dql, q);

    /* and register the books */
    for (node = dql->books; node; node = node->next)
        gnc_gui_component_watch_entity (dql->component_id, (GUID*)node->data,
                                        QOF_EVENT_DESTROY);

    return dql;
}

void gnc_dialog_query_list_set_title (DialogQueryList *dql, const char *title)
{
    if (!dql || !title) return;
    gtk_window_set_title (GTK_WINDOW (dql->dialog), title);
}

void gnc_dialog_query_list_set_label (DialogQueryList *dql, const char *label)
{
    if (!dql || !label) return;
    gtk_label_set_text (GTK_LABEL(dql->label), label);
}

void gnc_dialog_query_list_set_buttons (DialogQueryList *dql,
                                        GNCDisplayListButton *buttons,
                                        gpointer user_data)
{
    GtkWidget *button;
    int i;

    if (!dql || !buttons) return;
    g_return_if_fail (dql->buttons == NULL);

    dql->buttons = buttons;
    dql->user_data = user_data;

    /* build up the buttons */
    for (i = 0; buttons[i].label; i++)
    {
        button = gtk_button_new_with_label (buttons[i].label);
        g_object_set_data (G_OBJECT (button), "data", &(dql->buttons[i]));
        g_signal_connect (G_OBJECT (button), "clicked",
                          G_CALLBACK(gnc_dialog_query_list_button_clicked), dql);
        gtk_box_pack_start (GTK_BOX (dql->button_box), button, FALSE, FALSE, 3);
    }
}

void gnc_dialog_query_list_set_numerics (DialogQueryList *dql, gboolean abs,
        gboolean inv_sort)
{
    if (!dql) return;

    gnc_query_list_set_numerics (GNC_QUERY_LIST(dql->qlist), abs, inv_sort);
}

void gnc_dialog_query_list_refresh (DialogQueryList *dql)
{
    if (!dql) return;

    gnc_query_list_refresh (GNC_QUERY_LIST(dql->qlist));
    gtk_widget_show_all (dql->dialog);
}

void gnc_dialog_query_list_destroy (DialogQueryList *dql)
{
    if (!dql) return;
    gnc_close_gui_component (dql->component_id);
}

DialogQueryList *
gnc_dialog_query_list_create (GList *param_list, Query *q,
                              const char *title, const char *label,
                              gboolean abs, gboolean inv_sort,
                              GNCDisplayListButton *buttons, gpointer user_data)
{
    DialogQueryList *dql;

    if (!param_list || !q)
        return NULL;

    dql = gnc_dialog_query_list_new (param_list, q);
    if (!dql)
        return NULL;

    if (title)
        gnc_dialog_query_list_set_title (dql, title);

    if (label)
        gnc_dialog_query_list_set_label (dql, label);

    gnc_dialog_query_list_set_numerics (dql, abs, inv_sort);

    if (buttons)
        gnc_dialog_query_list_set_buttons (dql, buttons, user_data);

    gnc_dialog_query_list_refresh (dql);

    return dql;
}
