/*************************************************************************
 * The following code implements a text view in a custom GtkCellRenderer.
 *
 * Copyright (C) 2020 Robert Fewell
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *************************************************************************/
#include <config.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>

#include "gnc-cell-renderer-text-view.h"
#include "gnc-cell-view.h"

static GtkCellEditable *gcrtv_start_editing (GtkCellRenderer          *cell,
                                             GdkEvent                 *event,
                                             GtkWidget                *widget,
                                             const gchar              *path,
                                             const GdkRectangle       *background_area,
                                             const GdkRectangle       *cell_area,
                                             GtkCellRendererState      flags);

#define GNC_CELL_RENDERER_TEXT_VIEW_PATH "gnc-cell-renderer-text-view-path"

struct _GncCellRendererTextView
{
    GtkCellRendererText  parent;

    /* The editable entry. */
    GtkWidget *editable;
};

G_DEFINE_TYPE (GncCellRendererTextView, gnc_cell_renderer_text_view, GTK_TYPE_CELL_RENDERER_TEXT)

static void
gnc_cell_renderer_text_view_init (GncCellRendererTextView *self)
{
}

static void
gnc_cell_renderer_text_view_finalize (GObject *object)
{
    G_OBJECT_CLASS (gnc_cell_renderer_text_view_parent_class)->finalize (object);
}

static void
gnc_cell_renderer_text_view_class_init (GncCellRendererTextViewClass *klass)
{
    GObjectClass         *gobject_class = G_OBJECT_CLASS(klass);
    GtkCellRendererClass *cell_class = GTK_CELL_RENDERER_CLASS(klass);

    gobject_class->finalize = gnc_cell_renderer_text_view_finalize;

    cell_class->start_editing = gcrtv_start_editing;
}

static void
gcrtv_editing_done (GtkCellEditable         *editable,
                    GncCellRendererTextView *cell_tv)
{
    gchar       *path;
    gchar       *new_text;

    if (GNC_CELL_VIEW(editable)->focus_out_id > 0)
    {
        g_signal_handler_disconnect (GNC_CELL_VIEW(editable)->text_view,
                                     GNC_CELL_VIEW(editable)->focus_out_id);
        GNC_CELL_VIEW(editable)->focus_out_id = 0;
    }

    if (GNC_CELL_VIEW(editable)->populate_popup_id > 0)
    {
        g_signal_handler_disconnect (GNC_CELL_VIEW(editable)->text_view,
                                     GNC_CELL_VIEW(editable)->populate_popup_id);
        GNC_CELL_VIEW(editable)->populate_popup_id = 0;
    }

    if (GNC_CELL_VIEW(editable)->editing_canceled)
    {
        gtk_cell_renderer_stop_editing (GTK_CELL_RENDERER(cell_tv), TRUE);
        return;
    }

    path = g_object_get_data (G_OBJECT(editable), 
                              GNC_CELL_RENDERER_TEXT_VIEW_PATH);

    new_text = gnc_cell_view_get_text (GNC_CELL_VIEW(editable));

    gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE(editable));

    g_signal_emit_by_name (cell_tv, "edited", path, new_text);

    g_free (new_text);
}

static gboolean
gcrtv_button_press_event (GtkWidget      *widget,
                          GdkEventButton *event,
                          gpointer        user_data)
{
    // allows mouse clicks in text view
    return TRUE;
}

static GtkCellEditable *
gcrtv_start_editing (GtkCellRenderer      *cell,
                     GdkEvent             *event,
                     GtkWidget            *widget,
                     const gchar          *path,
                     const GdkRectangle   *background_area,
                     const GdkRectangle   *cell_area,
                     GtkCellRendererState  flags)
{
    GncCellRendererTextView *cell_tv = GNC_CELL_RENDERER_TEXT_VIEW(cell);
    GtkWidget                *editable;
    gchar                    *text = NULL;
    gboolean                  iseditable;

    g_object_get (G_OBJECT(cell_tv), "editable", &iseditable, NULL);

    /* If the cell isn't editable we return NULL. */
    if (iseditable == FALSE)
        return NULL;

    editable = g_object_new (GNC_TYPE_CELL_VIEW, NULL);

    g_signal_connect (editable, "button-press-event",
                      G_CALLBACK(gcrtv_button_press_event),
                      NULL);

    g_object_get (G_OBJECT(cell), "text", &text, NULL);

    gnc_cell_view_set_text (GNC_CELL_VIEW(editable), text);

    g_free (text);

    gtk_widget_grab_focus (GTK_WIDGET(editable));

    g_object_set_data_full (G_OBJECT(editable),
                            GNC_CELL_RENDERER_TEXT_VIEW_PATH,
                            g_strdup (path),
                            g_free);

    gtk_widget_show (editable);

    g_signal_connect (editable, "editing-done", G_CALLBACK(gcrtv_editing_done), cell_tv);

    cell_tv->editable = editable;

    g_object_add_weak_pointer (G_OBJECT(cell_tv->editable),
                               (gpointer) &cell_tv->editable);

    return GTK_CELL_EDITABLE(editable);
}

GtkCellRenderer *
gnc_cell_renderer_text_view_new (void)
{
    return g_object_new (GNC_TYPE_CELL_RENDERER_TEXT_VIEW, NULL);
}
