/*
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
#include "gnc-cell-view.h"
struct _GncCellView
{
    GtkBox parent_instance;
    GtkTextView *text_view;
};
G_DEFINE_TYPE (GncCellView, gnc_cell_view, GTK_TYPE_BOX)

static void
gnc_cell_view_init (GncCellView *view)
{
    GtkTextBuffer *buffer;
    gtk_orientable_set_orientation (GTK_ORIENTABLE (view), GTK_ORIENTATION_VERTICAL);
    view->text_view = GTK_TEXT_VIEW (gtk_text_view_new ());
    gtk_text_view_set_wrap_mode (view->text_view, GTK_WRAP_WORD_CHAR);
    gtk_text_view_set_top_margin (view->text_view, 2);
    gtk_text_view_set_bottom_margin (view->text_view, 2);
    gtk_box_append (GTK_BOX (view), GTK_WIDGET (view->text_view));
    buffer = gtk_text_view_get_buffer (view->text_view);
    gtk_text_buffer_set_text (buffer, "", -1);
}
static void
gnc_cell_view_class_init (GncCellViewClass *klass)
{
    (void)klass;
}
GtkWidget *
gnc_cell_view_new (void)
{
    return GTK_WIDGET (g_object_new (GNC_TYPE_CELL_VIEW, NULL));
}
void
gnc_cell_view_set_text (GncCellView *view, const gchar *text)
{
    g_return_if_fail (GNC_IS_CELL_VIEW (view));
    gtk_text_buffer_set_text (gtk_text_view_get_buffer (view->text_view), text? text: "", -1);
}
gchar *
gnc_cell_view_get_text (GncCellView *view)
{
    GtkTextIter start, end;
    GtkTextBuffer *buffer;
    g_return_val_if_fail (GNC_IS_CELL_VIEW (view), NULL);
    buffer = gtk_text_view_get_buffer (view->text_view);
    gtk_text_buffer_get_bounds (buffer, &start, &end);
    return gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
}
