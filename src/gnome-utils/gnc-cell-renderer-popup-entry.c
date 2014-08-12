/*************************************************************************
 * The following code has come from Planner. This code implements a
 * GtkCalendar in a custom GtkCellEditable popup from GtkCellRenderer.
 *
 * These files have been renamed and changed to remove code not required
 * and to remove a dependency on libplanner.
 *
 * Copyright (C) 2012 Robert Fewell
 *
 * Copyright (C) 2001-2002 CodeFactory AB
 * Copyright (C) 2001-2002 Richard Hult <richard@imendio.com>
 * Copyright (C) 2001-2002 Mikael Hallendal <micke@imendio.com>
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
#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <gdk/gdkkeysyms.h>
#include <string.h>

#include "gnc-cell-renderer-popup-entry.h"
#include "dialog-utils.h"
#include "gnc-date.h"

static void     gnc_popup_entry_init       (GncPopupEntry        *entry);
static void     gnc_popup_entry_class_init (GncPopupEntryClass   *klass);
static void     gpw_cell_editable_init     (GtkCellEditableIface *iface);
static gboolean gpw_key_press_event        (GtkWidget            *box,
        GdkEventKey          *key_event);

enum
{
    ARROW_CLICKED,
    LAST_SIGNAL
};

enum
{
    ARG_0,
    ARG_EDITING_CANCELED
};

static GtkEventBoxClass *parent_class;
static guint signals[LAST_SIGNAL];

GtkType
gnc_popup_entry_get_type (void)
{
    static GtkType widget_type = 0;

    if (!widget_type)
    {
        static const GTypeInfo widget_info =
        {
            sizeof (GncPopupEntryClass),
            NULL,		/* base_init */
            NULL,		/* base_finalize */
            (GClassInitFunc) gnc_popup_entry_class_init,
            NULL,		/* class_finalize */
            NULL,		/* class_data */
            sizeof (GncPopupEntry),
            0,              /* n_preallocs */
            (GInstanceInitFunc) gnc_popup_entry_init,
        };

        static const GInterfaceInfo cell_editable_info =
        {
            (GInterfaceInitFunc) gpw_cell_editable_init,    /* interface_init */
            NULL,                                           /* interface_finalize */
            NULL                                            /* interface_data */
        };

        widget_type = g_type_register_static (GTK_TYPE_EVENT_BOX,
                                              "GncPopupEntry",
                                              &widget_info,
                                              0);

        g_type_add_interface_static (widget_type,
                                     GTK_TYPE_CELL_EDITABLE,
                                     &cell_editable_info);
    }

    return widget_type;
}

static void
gnc_popup_entry_init (GncPopupEntry *widget)
{
    GtkWidget *arrow;

    widget->hbox = gtk_hbox_new (FALSE, 0);
    gtk_widget_show (widget->hbox);

    widget->entry = g_object_new (GTK_TYPE_ENTRY, "has_frame", FALSE, NULL);
    GTK_ENTRY (widget->entry)->is_cell_renderer = TRUE;
    gtk_widget_show (widget->entry);

    widget->button = gtk_button_new ();
    gtk_widget_show (widget->button);

    arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_OUT);
    gtk_widget_show (arrow);

    gtk_container_add (GTK_CONTAINER (widget->button), arrow);

    gtk_box_pack_start (GTK_BOX (widget->hbox), widget->entry, TRUE, TRUE, 0);
    gtk_box_pack_start (GTK_BOX (widget->hbox), widget->button, FALSE, TRUE, 0);

    gtk_container_add (GTK_CONTAINER (widget), widget->hbox);

    GTK_WIDGET_SET_FLAGS (widget, GTK_CAN_FOCUS);
    gtk_widget_add_events (GTK_WIDGET (widget), GDK_KEY_PRESS_MASK);
    gtk_widget_add_events (GTK_WIDGET (widget), GDK_KEY_RELEASE_MASK);
}

static void
gnc_popup_entry_class_init (GncPopupEntryClass *klass)
{
    GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

    widget_class->key_press_event = gpw_key_press_event;

    gtk_object_add_arg_type ("GncPopupEntry::editing-canceled", GTK_TYPE_BOOL, GTK_ARG_READWRITE, ARG_EDITING_CANCELED);

    parent_class = GTK_EVENT_BOX_CLASS (g_type_class_peek_parent (klass));

    signals[ARROW_CLICKED] = g_signal_new
                             ("arrow-clicked",
                              G_TYPE_FROM_CLASS (klass),
                              G_SIGNAL_RUN_LAST,
                              0,
                              NULL, NULL,
                              g_cclosure_marshal_VOID__VOID,
                              G_TYPE_NONE, 0);

}

static void
gpw_arrow_clicked (GtkWidget *button, GncPopupEntry *widget)
{
    g_signal_emit (widget, signals[ARROW_CLICKED], 0);
}

/* GtkCellEditable method implementations
 */
static void
gtk_cell_editable_entry_activated (GtkEntry *entry, GncPopupEntry *widget)
{
    gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (widget));
    gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (widget));
}

static gboolean
gtk_cell_editable_key_press_event (GtkEntry      *entry,
                                   GdkEventKey   *key_event,
                                   GncPopupEntry *widget)
{
    const char *date_string;
    gint year = 0, month = 0, day = 0;
    struct tm when;

    if (key_event->keyval == GDK_Escape)
    {
        widget->editing_canceled = TRUE;

        gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (widget));
        gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (widget));

        return TRUE;
    }

    date_string = gtk_entry_get_text (entry);

    memset (&when, 0, sizeof (when));

    if (qof_scan_date (date_string, &day, &month, &year))
    {
        when.tm_year = year - 1900;
        when.tm_mon = month - 1 ;
        when.tm_mday = day;

        if (!gnc_handle_date_accelerator (key_event, &when, date_string))
            return FALSE;

        gtk_entry_set_text (entry, qof_print_date (gnc_mktime (&when)));
        gtk_widget_grab_focus (GTK_WIDGET (entry));
        return TRUE;
    }
    return FALSE;
}

static gboolean
gpw_key_press_event (GtkWidget   *box,
                     GdkEventKey *key_event)
{
    GncPopupEntry *widget = GNC_POPUP_ENTRY (box);
    GdkEvent       tmp_event;

    gtk_widget_grab_focus (widget->entry);

    if (key_event->keyval == GDK_Escape)
    {
        widget->editing_canceled = TRUE;

        gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (widget));
        gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (widget));

        return TRUE;
    }

    if (key_event->keyval == GDK_Left)
    {
        gtk_editable_set_position (GTK_EDITABLE (widget->entry), 0);
        return TRUE;
    }

    if (key_event->keyval == GDK_Right)
    {
        gtk_editable_set_position (GTK_EDITABLE (widget->entry), -1);
        return TRUE;
    }

    /* Hackish :/ Synthesize a key press event for the entry. */
    memcpy (&tmp_event, key_event, sizeof (GdkEventKey));

    tmp_event.key.window = widget->entry->window;
    tmp_event.key.send_event = TRUE;

    gtk_widget_event (widget->entry, &tmp_event);

    return GTK_WIDGET_CLASS (parent_class)->key_press_event (GTK_WIDGET (widget),
            key_event);
}

static void
gpw_start_editing (GtkCellEditable *cell_editable,
                   GdkEvent        *event)
{
    GncPopupEntry *widget = GNC_POPUP_ENTRY (cell_editable);

    gtk_editable_select_region (GTK_EDITABLE (widget->entry), 0, -1);

    g_signal_connect (G_OBJECT (widget->entry),
                      "activate",
                      G_CALLBACK (gtk_cell_editable_entry_activated),
                      widget);
    g_signal_connect (G_OBJECT (widget->entry),
                      "key_press_event",
                      G_CALLBACK (gtk_cell_editable_key_press_event),
                      widget);
    g_signal_connect (G_OBJECT (widget->button),
                      "clicked",
                      (GCallback) gpw_arrow_clicked,
                      widget);
}

static void
gpw_cell_editable_init (GtkCellEditableIface *iface)
{
    iface->start_editing = gpw_start_editing;
}

void
gnc_popup_entry_set_text (GncPopupEntry *popup, const gchar *text)
{
    g_return_if_fail (GNC_IS_POPUP_ENTRY (popup));

    gtk_entry_set_text (GTK_ENTRY (popup->entry), text ? text : "");
}

const gchar *
gnc_popup_entry_get_text (GncPopupEntry *popup)
{
    g_return_val_if_fail (GNC_IS_POPUP_ENTRY (popup), NULL);

    return gtk_entry_get_text (GTK_ENTRY (popup->entry));
}

gint
gnc_popup_get_button_width (void)
{
    GtkWidget *window, *button, *arrow;
    gint       width;

    GtkRequisition req;

    window = gtk_window_new (GTK_WINDOW_POPUP);

    button = gtk_button_new ();
    gtk_widget_show (button);
    gtk_container_add (GTK_CONTAINER (window), button);

    arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_OUT);
    gtk_widget_show (arrow);
    gtk_container_add (GTK_CONTAINER (button), arrow);

    gtk_window_move (GTK_WINDOW (window), -500, -500);
    gtk_widget_show (window);

    gtk_widget_size_request (window, &req);

    width = req.width;

    gtk_widget_destroy (window);

    return width;
}

