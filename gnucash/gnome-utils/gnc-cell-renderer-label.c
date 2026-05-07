/********************************************************************
 * gnc-cell-renderer-label.c -- A GtkCellRendererText subclass that
 * shows a selectable (but not editable) GtkEntry when activated,
 * allowing the user to select and copy cell text via Ctrl+C.
 * The entry has no frame and zero minimum width so it fits exactly
 * within the cell area without overflowing into adjacent columns.
 *
 * Copyright (C) 2026 GnuCash contributors
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *******************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "gnc-cell-renderer-label.h"
#include "gnc-ui-util.h"

/* ================================================================
 * Selectable entry helpers
 *
 * GtkEntry already implements GtkCellEditable.  We use a plain
 * non-editable GtkEntry (no wrapper widget) so its size exactly
 * matches the cell area allocated by the tree view.  Callbacks
 * always set editing-canceled so nothing is written back to the
 * model.
 * ================================================================ */

static void
gsl_dismiss (GtkEntry *entry)
{
    g_object_set (entry, "editing-canceled", TRUE, NULL);
    gtk_cell_editable_editing_done (GTK_CELL_EDITABLE (entry));
    gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (entry));
}

static gboolean
gsl_key_press_cb (GtkWidget *widget,
                  GdkEventKey *event,
                  gpointer   user_data)
{
    if (event->keyval == GDK_KEY_Escape)
    {
        gsl_dismiss (GTK_ENTRY (widget));
        return TRUE;
    }
    /* GtkEntry handles Ctrl+C natively (copies to GDK_SELECTION_CLIPBOARD). */
    return FALSE;
}

static gboolean
gsl_focus_in_cb (GtkWidget     *widget,
                 GdkEventFocus *event,
                 gpointer       user_data)
{
    gtk_editable_select_region (GTK_EDITABLE (widget), 0, -1);
    return FALSE;
}

static gboolean
gsl_focus_out_cb (GtkWidget   *widget,
                  GdkEventFocus *event,
                  gpointer     user_data)
{
    gsl_dismiss (GTK_ENTRY (widget));
    return FALSE;
}

static GtkWidget *
gnc_selectable_entry_new (const gchar *text, gfloat xalign)
{
    GtkEntry *entry = GTK_ENTRY (gtk_entry_new ());

    /* Non-editable so the user can select but not modify. */
    gtk_editable_set_editable (GTK_EDITABLE (entry), FALSE);
    /* No frame: matches the cell visual and avoids extra padding. */
    gtk_entry_set_has_frame (entry, FALSE);
    /* Allow the entry to shrink to the cell width. */
    gtk_entry_set_width_chars (entry, 0);
    gtk_entry_set_alignment (entry, xalign);

    gchar *clean = gnc_filter_text_for_bidi_marks (text ? text : "");
    gtk_entry_set_text (entry, clean ? clean : "");
    g_free (clean);

    g_signal_connect (entry, "focus-in-event",
                      G_CALLBACK (gsl_focus_in_cb), NULL);
    g_signal_connect (entry, "key-press-event",
                      G_CALLBACK (gsl_key_press_cb), NULL);
    g_signal_connect (entry, "focus-out-event",
                      G_CALLBACK (gsl_focus_out_cb), NULL);

    gtk_widget_show (GTK_WIDGET (entry));
    return GTK_WIDGET (entry);
}

/* ================================================================
 * GncCellRendererLabel
 *
 * A GtkCellRendererText subclass whose start_editing returns a
 * non-editable GtkEntry (no frame, zero minimum width) so clicking
 * a cell shows the value in a selectable widget that fits exactly
 * within the cell area.
 * ================================================================ */

struct _GncCellRendererLabel
{
    GtkCellRendererText parent;
};

G_DEFINE_TYPE (GncCellRendererLabel, gnc_cell_renderer_label, GTK_TYPE_CELL_RENDERER_TEXT)

static GtkCellEditable *
gnc_cell_renderer_label_start_editing (GtkCellRenderer      *cell,
                                       GdkEvent             *event,
                                       GtkWidget            *widget,
                                       const gchar          *path,
                                       const GdkRectangle   *background_area,
                                       const GdkRectangle   *cell_area,
                                       GtkCellRendererState  flags)
{
    gchar  *text = NULL;
    gfloat  xalign = 0.0;
    GtkWidget *editable;

    g_object_get (cell, "text", &text, "xalign", &xalign, NULL);

    editable = gnc_selectable_entry_new (text, xalign);
    g_free (text);

    return GTK_CELL_EDITABLE (editable);
}

static void
gnc_cell_renderer_label_class_init (GncCellRendererLabelClass *klass)
{
    GtkCellRendererClass *cell_class = GTK_CELL_RENDERER_CLASS (klass);

    cell_class->start_editing = gnc_cell_renderer_label_start_editing;
}

static void
gnc_cell_renderer_label_init (GncCellRendererLabel *self)
{
    /* GTK_CELL_RENDERER_MODE_EDITABLE causes start_editing() to be
     * called when the user activates the cell. */
    g_object_set (self, "mode", GTK_CELL_RENDERER_MODE_EDITABLE, NULL);
}

GtkCellRenderer *
gnc_cell_renderer_label_new (void)
{
    return g_object_new (GNC_TYPE_CELL_RENDERER_LABEL, NULL);
}
