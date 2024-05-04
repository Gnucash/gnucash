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
#include <string.h>

#include "gnc-cell-view.h"

static void  gnc_cell_view_editable_init (GtkCellEditableIface *iface);

static void  gnc_cell_view_set_property (GObject      *object,
                                         guint         param_id,
                                         const GValue *value,
                                         GParamSpec   *pspec);

static void  gnc_cell_view_get_property (GObject    *object,
                                         guint       param_id,
                                         GValue     *value,
                                         GParamSpec *pspec);

enum {
    PROP_0,
    PROP_EDITING_CANCELED,
};

G_DEFINE_TYPE_WITH_CODE (GncCellView, gnc_cell_view, GTK_TYPE_BOX,
                         G_IMPLEMENT_INTERFACE (GTK_TYPE_CELL_EDITABLE,
                         gnc_cell_view_editable_init))

static void
gnc_cell_view_dispose (GObject *gobject)
{
    if (GNC_CELL_VIEW(gobject)->tooltip_id > 0)
    {
        g_source_remove (GNC_CELL_VIEW(gobject)->tooltip_id);
        GNC_CELL_VIEW(gobject)->tooltip_id = 0;
    }
    G_OBJECT_CLASS(gnc_cell_view_parent_class)->dispose (gobject);
}

static void
gnc_cell_view_init (GncCellView *cv)
{
    cv->editing_canceled = FALSE;

    cv->text_view = g_object_new (GTK_TYPE_TEXT_VIEW, "accepts-tab", FALSE, NULL);
    cv->buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW(cv->text_view));

    gtk_text_view_set_left_margin (GTK_TEXT_VIEW(cv->text_view), 2);
    gtk_text_view_set_right_margin (GTK_TEXT_VIEW(cv->text_view), 2);

    gtk_widget_set_tooltip_text (GTK_WIDGET(cv->text_view),
            _("Use Shift combined with Return or Keypad Enter to finish editing"));

    gtk_box_append (GTK_BOX(cv), GTK_WIDGET(cv->text_view));
    gtk_widget_set_visible (GTK_WIDGET(cv->text_view), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(cv), TRUE);
    gtk_widget_set_hexpand (GTK_WIDGET(cv->text_view), TRUE);

    cv->tooltip_id = 0;

    gtk_widget_set_can_focus (GTK_WIDGET(cv->text_view), TRUE);
}

static void
gnc_cell_view_class_init (GncCellViewClass *klass)
{
    GObjectClass  *gobject_class = G_OBJECT_CLASS(klass);

    gobject_class->dispose = gnc_cell_view_dispose;

    gobject_class->set_property = gnc_cell_view_set_property;
    gobject_class->get_property = gnc_cell_view_get_property;

    g_object_class_override_property (gobject_class,
                                      PROP_EDITING_CANCELED,
                                      "editing-canceled");
}

static void
gnc_cell_view_set_property (GObject      *object,
                            guint         param_id,
                            const GValue *value,
                            GParamSpec   *pspec)
{
    GncCellView *cv = GNC_CELL_VIEW(object);

    switch (param_id)
    {
        case PROP_EDITING_CANCELED:
            cv->editing_canceled = g_value_get_boolean (value);
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
            break;
    }
}

static void
gnc_cell_view_get_property (GObject    *object,
                            guint       param_id,
                            GValue     *value,
                            GParamSpec *pspec)
{
    GncCellView *cv = GNC_CELL_VIEW(object);

    switch (param_id)
    {
        case PROP_EDITING_CANCELED:
            g_value_set_boolean (value, cv->editing_canceled);
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
            break;
    }
}

static gboolean
gtk_cell_editable_key_press_event (GtkEventControllerKey *key, guint keyval,
                                   guint keycode, GdkModifierType state,
                                   gpointer user_data)
{
    GncCellView *cv = user_data;

    if (keyval == GDK_KEY_Escape)
    {
        cv->editing_canceled = TRUE;

        gtk_cell_editable_editing_done (GTK_CELL_EDITABLE(cv));
        gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE(cv));
        return TRUE;
    }

    if ((keyval == GDK_KEY_Return || keyval == GDK_KEY_KP_Enter)
         && (state & GDK_SHIFT_MASK))
    {
        gtk_cell_editable_editing_done (GTK_CELL_EDITABLE(cv));
        return TRUE;
    }
    return FALSE;
}

static gboolean
gcv_remove_tooltip (GncCellView *cv)
{
    if (cv->tooltip_id > 0)
    {
        gtk_widget_set_tooltip_text (GTK_WIDGET(cv->text_view), NULL);
        cv->tooltip_id = 0;
    }
    return FALSE;
}

static void
gcv_start_editing (GtkCellEditable *cell_editable,
                   GdkEvent        *event)
{
    GncCellView *cv = GNC_CELL_VIEW(cell_editable);
    GtkTextIter siter, eiter;

    // Remove the text_view tooltip after 5secs to stop it recurring
    cv->tooltip_id = g_timeout_add (5000, (GSourceFunc) gcv_remove_tooltip, cv);

    gtk_text_buffer_get_bounds (cv->buffer, &siter, &eiter);
    gtk_text_buffer_select_range (cv->buffer, &eiter, &siter);

    gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW(cv->text_view), TRUE);

    gtk_widget_grab_focus (GTK_WIDGET(cv->text_view));

    GtkEventController *event_controller = gtk_event_controller_key_new ();
    gtk_widget_add_controller (GTK_WIDGET(cv->text_view), event_controller);
    g_signal_connect (G_OBJECT(event_controller),
                      "key-pressed",
                      G_CALLBACK(gtk_cell_editable_key_press_event), cv);
}

static void
gnc_cell_view_editable_init (GtkCellEditableIface *iface)
{
    iface->start_editing = gcv_start_editing;
}

void
gnc_cell_view_set_text (GncCellView *cv, const gchar *text)
{
    g_return_if_fail (GNC_IS_CELL_VIEW(cv));

    gtk_text_buffer_set_text (cv->buffer, text ? text : "", -1);
}

gchar *
gnc_cell_view_get_text (GncCellView *cv)
{
    GtkTextIter siter, eiter;

    g_return_val_if_fail (GNC_IS_CELL_VIEW(cv), NULL);

    gtk_text_buffer_get_bounds (cv->buffer, &siter, &eiter);

    return gtk_text_buffer_get_text (cv->buffer, &siter, &eiter, TRUE);
}
