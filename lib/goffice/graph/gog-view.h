/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-view.h : A sized render engine for an item.
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 */
#ifndef GOG_VIEW_H
#define GOG_VIEW_H

#include <goffice/graph/goffice-graph.h>
#include <glib-object.h>

G_BEGIN_DECLS

struct _GogView {
	GObject	 base;

	GogObject *model;

	GogRenderer *renderer;  /* not NULL */
	GogView	    *parent;	/* potentially NULL */
	GSList	    *children;

	GogViewAllocation  allocation;	/* in renderer units */
	GogViewAllocation  residual;	/* left over after compass children are placed */
	unsigned allocation_valid : 1;  /* adjust our layout when child changes size */
	unsigned child_allocations_valid : 1;  /* some children need to adjust their layout */
	unsigned being_updated : 1;
};

typedef struct {
	GObjectClass	base;

	gboolean	clip;

	/* Virtuals */
	void	 (*state_init)    (GogView *);
	void	 (*size_request)  (GogView *, GogViewRequisition *r);
	void	 (*size_allocate) (GogView *, GogViewAllocation const *a);
	void	 (*render)        (GogView *, GogViewAllocation const *bbox);
	gboolean (*info_at_point) (GogView *, double x, double y,
				   GogObject const *cur_selection,
				   GogObject **obj, char **name);
} GogViewClass;

#define GOG_VIEW_TYPE		(gog_view_get_type ())
#define GOG_VIEW(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_VIEW_TYPE, GogView))
#define IS_GOG_VIEW(o)		(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_VIEW_TYPE))
#define GOG_VIEW_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GOG_VIEW_TYPE, GogViewClass))
#define IS_GOG_VIEW_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GOG_VIEW_TYPE))
#define GOG_VIEW_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GOG_VIEW_TYPE, GogViewClass))

GType gog_view_get_type (void);

GogObject *gog_view_get_model	     (GogView const *view);
void	   gog_view_render	     (GogView *v, GogViewAllocation const *bbox);
void       gog_view_queue_redraw     (GogView *v);
void       gog_view_queue_resize     (GogView *v);
void       gog_view_size_request     (GogView *v, GogViewRequisition *req);
void       gog_view_size_allocate    (GogView *v, GogViewAllocation const *a);
gboolean   gog_view_update_sizes     (GogView *v);
gboolean   gog_view_info_at_point    (GogView *container, double x, double y,
				      GogObject const *cur_selection,
				      GogObject **obj, char **name);
GogView   *gog_view_find_child_view  (GogView const *container,
				      GogObject const *target_model);

/* protected */
void gog_view_size_child_request (GogView *v,
				  GogViewRequisition const *avail,
				  GogViewRequisition *req);

G_END_DECLS

#endif /* GOG_VIEW_H */
