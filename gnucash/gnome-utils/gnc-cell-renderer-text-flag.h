/**
 * gnc-cell-renderer-text-flag.h -- text cell renderer with flag.
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

#ifndef __GNC_CELL_RENDERER_TEXT_FLAG_H__
#define __GNC_CELL_RENDERER_TEXT_FLAG_H__

#include <gtk/gtkcellrenderertext.h>


#define GNC_TYPE_CELL_RENDERER_TEXT_FLAG		(gnc_cell_renderer_text_flag_get_type ())
#define GNC_CELL_RENDERER_TEXT_FLAG(obj)		(G_TYPE_CHECK_INSTANCE_CAST ((obj), GNC_TYPE_CELL_RENDERER_TEXT_FLAG, GncCellRendererTextFlag))
#define GNC_CELL_RENDERER_TEXT_FLAG_CLASS(klass)	(G_TYPE_CHECK_CLASS_CAST ((klass), GNC_TYPE_CELL_RENDERER_TEXT_FLAG, GncCellRendererTextFlagClass))
#define GNC_IS_CELL_RENDERER_TEXT_FLAG(obj)		(G_TYPE_CHECK_INSTANCE_TYPE ((obj), GNC_TYPE_CELL_RENDERER_TEXT_FLAG))
#define GNC_IS_CELL_RENDERER_TEXT_FLAG_CLASS(klass)	(G_TYPE_CHECK_CLASS_TYPE ((klass), GNC_TYPE_CELL_RENDERER_TEXT_FLAG))
#define GNC_CELL_RENDERER_TEXT_FLAG_GET_CLASS(obj)   (G_TYPE_INSTANCE_GET_CLASS ((obj), GNC_TYPE_CELL_RENDERER_TEXT_FLAG, GncCellRendererTextFlagClass))

typedef struct _GncCellRendererTextFlag              GncCellRendererTextFlag;
typedef struct _GncCellRendererTextFlagClass         GncCellRendererTextFlagClass;
typedef struct _GtkCellRendererTextPrivate
{
    guint size;
    GdkRGBA color;
    GdkRGBA color_selected;
    gboolean flagged;
} GncCellRendererTextFlagPrivate;

struct _GncCellRendererTextFlag
{
  GtkCellRendererText parent;

  /*< private >*/
  GncCellRendererTextFlagPrivate *priv;
};

struct _GncCellRendererTextFlagClass
{
  GtkCellRendererTextClass parent_class;
};

GType gnc_cell_renderer_text_flag_get_type(void) G_GNUC_CONST;
GtkCellRenderer *gnc_cell_renderer_text_flag_new(void);

#endif /* __GNC_CELL_RENDERER_TEXT_FLAG_H__ */
