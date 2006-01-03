/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-rotation-sel.h - Select a text orientation
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License, version 2, as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */
#ifndef _GO_ROTATION_SEL_H_
#define _GO_ROTATION_SEL_H_

#include <gtk/gtkwindow.h>
#include <goffice/utils/goffice-utils.h>

G_BEGIN_DECLS

#define GO_ROTATION_SEL_TYPE	(go_rotation_sel_get_type ())
#define GO_ROTATION_SEL(obj)	(G_TYPE_CHECK_INSTANCE_CAST((obj), GO_ROTATION_SEL_TYPE, GORotationSel))
#define IS_GO_ROTATION_SEL(obj)	(G_TYPE_CHECK_INSTANCE_TYPE((obj), GO_ROTATION_SEL_TYPE))

typedef struct _GORotationSel GORotationSel;

GType      go_rotation_sel_get_type (void);
GtkWidget *go_rotation_sel_new      (void);
void	   go_rotation_sel_set_rotation (GORotationSel *rs, int degrees);
int	   go_rotation_sel_get_rotation (GORotationSel const *rs);

G_END_DECLS

#endif /* _GO_ROTATION_SEL_H_ */
