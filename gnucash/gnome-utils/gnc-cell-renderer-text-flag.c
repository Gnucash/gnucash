/**
 * gnc-cell-renderer-text-flag.c -- text cell renderer with flag.
 *
 * Copyright (C) 2019 Adrian Panella <ianchi74@outlook.com>
 * All rights reserved.
 **/

/* GnuCash is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Gnucash is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
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

#include "gnc-cell-renderer-text-flag.h"

/**
 * @Title: GncCellRendererTextFlag
 *
 * A #GncCellRendererTextFlag extends the GtkCellRendererText
 * adding the avility to show a color triangle on the top right corner
 * to flag the cell.
 */


static void gnc_cell_renderer_text_flag_get_property(GObject *object,
                                                     guint param_id,
                                                     GValue *value,
                                                     GParamSpec *pspec);
static void gnc_cell_renderer_text_flag_set_property(GObject *object,
                                                     guint param_id,
                                                     const GValue *value,
                                                     GParamSpec *pspec);

static void gnc_cell_renderer_text_flag_render(
    GtkCellRenderer *cell, cairo_t *cr, GtkWidget *widget,
    const GdkRectangle *background_area, const GdkRectangle *cell_area,
    GtkCellRendererState flags);

enum
{
    PROP_0,

    PROP_FLAG_SIZE,
    PROP_FLAG_COLOR,
    PROP_FLAG_COLOR_RGBA,
    PROP_FLAGGED,
    PROP_FLAG_COLOR_SELECTED,
    PROP_FLAG_COLOR_RGBA_SELECTED,

    LAST_PROP
};

struct _GncCellRendererTextFlag
{
    GtkCellRendererText parent;

    gint size;
    GdkRGBA color;
    GdkRGBA color_selected;
    gboolean flagged;
};

G_DEFINE_TYPE(GncCellRendererTextFlag,
              gnc_cell_renderer_text_flag,
              GTK_TYPE_CELL_RENDERER_TEXT)

static void
gnc_cell_renderer_text_flag_init(GncCellRendererTextFlag *celltext)
{
    celltext->size = 8;
    gdk_rgba_parse(&celltext->color, "red");
    celltext->flagged = FALSE;
}

static void
gnc_cell_renderer_text_flag_class_init(GncCellRendererTextFlagClass *class)
{
    GObjectClass *object_class       = G_OBJECT_CLASS(class);
    GtkCellRendererClass *cell_class = GTK_CELL_RENDERER_CLASS(class);

    object_class->get_property = gnc_cell_renderer_text_flag_get_property;
    object_class->set_property = gnc_cell_renderer_text_flag_set_property;

    cell_class->render = gnc_cell_renderer_text_flag_render;

    g_object_class_install_property(
        object_class, PROP_FLAG_SIZE,
        g_param_spec_int("flag-size", "Flag size", "Flag size", 0, 50,
                         8, G_PARAM_READWRITE));

    g_object_class_install_property(
        object_class, PROP_FLAG_COLOR,
        g_param_spec_string("flag-color", "Flag color name",
                            "Flag color as a string", "red",
                            G_PARAM_WRITABLE));

    g_object_class_install_property(
        object_class, PROP_FLAG_COLOR_RGBA,
        g_param_spec_boxed("flag-color-rgba", "Flag color as RGBA",
                           "Flag color as a GdkRGBA", GDK_TYPE_RGBA,
                           G_PARAM_READWRITE));

    g_object_class_install_property(
        object_class, PROP_FLAG_COLOR_SELECTED,
        g_param_spec_string("flag-color-selected", "Flag color name for selected rows",
                            "Flag color as a string, to use in selected rows", "white",
                            G_PARAM_WRITABLE));

    g_object_class_install_property(
        object_class, PROP_FLAG_COLOR_RGBA_SELECTED,
        g_param_spec_boxed("flag-color-rgba-selected", "Flag color as RGBA for selected rows",
                           "Flag color as a GdkRGBA, to use in selected rows", GDK_TYPE_RGBA,
                           G_PARAM_READWRITE));                           

    g_object_class_install_property(
        object_class, PROP_FLAGGED,
        g_param_spec_boolean("flagged", "Flag set",
                             "Flag indicator is set", FALSE,
                             G_PARAM_READWRITE));
}

static void
gnc_cell_renderer_text_flag_get_property(GObject *object, guint param_id,
                                         GValue *value, GParamSpec *pspec)
{
    GncCellRendererTextFlag *celltext    = GNC_CELL_RENDERER_TEXT_FLAG(object);

    switch (param_id)
    {
    case PROP_FLAGGED:
        g_value_set_boolean(value, celltext->flagged);
        break;

    case PROP_FLAG_SIZE:
        g_value_set_int(value, celltext->size);
        break;

    case PROP_FLAG_COLOR_RGBA:
      g_value_set_boxed (value, &celltext->color);
      break;

    case PROP_FLAG_COLOR_RGBA_SELECTED:
      g_value_set_boxed (value, &celltext->color_selected);
      break;

    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
        break;
    }
}

static void
gnc_cell_renderer_text_flag_set_property(GObject *object, guint param_id,
                                         const GValue *value,
                                         GParamSpec *pspec)
{
    GncCellRendererTextFlag *celltext    = GNC_CELL_RENDERER_TEXT_FLAG(object);
    switch (param_id)
    {
    case PROP_FLAG_COLOR:
    case PROP_FLAG_COLOR_SELECTED:
    {
        GdkRGBA rgba;

        if (!g_value_get_string(value))
            break;
        else if (gdk_rgba_parse(&rgba, g_value_get_string(value))) 
        {
            if (param_id == PROP_FLAG_COLOR_SELECTED)
                celltext->color = rgba;
            else
                celltext->color_selected = rgba;
        }
        else
            g_warning("Don't know color '%s'", g_value_get_string(value));
    }
    break;

    case PROP_FLAG_COLOR_RGBA:
    {
        GdkRGBA *rgba;

        rgba = g_value_get_boxed(value);
        if (rgba)
            celltext->color = *rgba;
    }
    break;

    case PROP_FLAG_COLOR_RGBA_SELECTED:
    {
        GdkRGBA *rgba;

        rgba = g_value_get_boxed(value);
        if (rgba)
            celltext->color_selected = *rgba;
    }
    break;

    case PROP_FLAGGED:
        celltext->flagged = g_value_get_boolean(value);
        break;

    case PROP_FLAG_SIZE:
        celltext->size = g_value_get_int(value);
        break;

    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, param_id, pspec);
        break;
    }
}

/**
 * gnc_cell_renderer_text_flag_new:
 *
 * Creates a new #GtkCellRendererTextFlag. 
 * It is a standard GtkCellRendererText extended to optionally show a
 * coloured triangle as a flag in the top right corner
 *
 * Returns: the new cell renderer
 **/
GtkCellRenderer *
gnc_cell_renderer_text_flag_new(void)
{
    return g_object_new(GNC_TYPE_CELL_RENDERER_TEXT_FLAG, NULL);
}

static void
gnc_cell_renderer_text_flag_render(GtkCellRenderer *cell, cairo_t *cr,
                                   GtkWidget *widget,
                                   const GdkRectangle *background_area,
                                   const GdkRectangle *cell_area,
                                   GtkCellRendererState flags)

{
    GncCellRendererTextFlag *celltext    = GNC_CELL_RENDERER_TEXT_FLAG(cell);

    // call the parent renderer to do the standard drawing
    GTK_CELL_RENDERER_CLASS(gnc_cell_renderer_text_flag_parent_class)
        ->render(cell, cr, widget, background_area, cell_area, flags);

    // add the flag (triangle in the top right corner)
    if (celltext->flagged)
    {
        guint size = MIN(MIN(background_area->height, celltext->size),
                         background_area->width);
        double x   = background_area->x + background_area->width - size;
        double y   = background_area->y;

        cairo_move_to(cr, x, y);
        cairo_rel_line_to(cr, size, 0);
        cairo_rel_line_to(cr, 0, size);
        cairo_close_path(cr);
        gdk_cairo_set_source_rgba(cr, (flags & GTK_CELL_RENDERER_SELECTED)
                                          ? &celltext->color_selected
                                          : &celltext->color);
        cairo_fill(cr);
    }
}
