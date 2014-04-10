/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-view.c :
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

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-renderer.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>

/* this should be per model */
#define PAD_HACK	4	/* pts */

enum {
	GOG_VIEW_PROP_0,
	GOG_VIEW_PROP_PARENT,
	GOG_VIEW_PROP_MODEL
};

static GObjectClass *parent_klass;

static void
cb_child_added (GogObject *parent, GogObject *child,
		GogView *view)
{
	g_return_if_fail (view->model == parent);

	gog_object_new_view (child, view);
	gog_view_queue_resize (view);
}

static void
cb_remove_child (GogObject *parent, GogObject *child,
		 GogView *view)
{
	GSList *ptr = view->children;
	GogObjectClass const *klass;
	GogView *tmp;

	g_return_if_fail (view->model == parent);

	gog_view_queue_resize (view);
	for (; ptr != NULL ; ptr = ptr->next) {
		tmp = GOG_VIEW (ptr->data);

		g_return_if_fail (tmp != NULL);

		if (tmp->model == child) {
			g_object_unref (tmp);
			return;
		}
	}

	/* The object may not create a view */
	klass = GOG_OBJECT_GET_CLASS (child);
	if (klass->view_type != 0)
		g_warning ("%s (%p) saw %s(%p) being removed from %s(%p) for which I didn't have a child",
			   G_OBJECT_TYPE_NAME (view), view,
			   G_OBJECT_TYPE_NAME (child), child,
			   G_OBJECT_TYPE_NAME (parent), parent);
}

static void
cb_model_changed (GogObject *model, gboolean resized, GogView *view)
{
	gog_debug (0, g_warning ("model %s(%p) for view %s(%p) changed %d",
		   G_OBJECT_TYPE_NAME (model), model,
		   G_OBJECT_TYPE_NAME (view), view, resized););
	if (resized)
		gog_view_queue_resize (view);
	else
		gog_view_queue_redraw (view);
}

/* make the list of view children match the models order */
static void
cb_model_reordered (GogView *view)
{
	GSList *tmp, *new_order = NULL;
	GSList *ptr = view->model->children;

	for (; ptr != NULL ; ptr = ptr->next) {
		tmp = view->children;
		/* not all the views may be created yet check for NULL */
		while (tmp != NULL && GOG_VIEW (tmp->data)->model != ptr->data)
			tmp = tmp->next;
		if (tmp != NULL)
			new_order = g_slist_prepend (new_order, tmp->data);
	}
	g_slist_free (view->children);
	view->children = g_slist_reverse (new_order);
}

static void
gog_view_set_property (GObject *gobject, guint param_id,
		       GValue const *value, GParamSpec *pspec)
{
	GogView *view = GOG_VIEW (gobject);
	gboolean init_state = (view->renderer == NULL || view->model == NULL);

	switch (param_id) {
	case GOG_VIEW_PROP_PARENT:
		g_return_if_fail (view->parent == NULL);

		view->parent = GOG_VIEW (g_value_get_object (value));
		if (view->parent != NULL) {
			view->renderer = view->parent->renderer;
			view->parent->children = g_slist_prepend (view->parent->children, view);
			cb_model_reordered (view->parent);
		}
		break;

	case GOG_VIEW_PROP_MODEL:
		g_return_if_fail (view->model == NULL);

		view->model = GOG_OBJECT (g_value_get_object (value));
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (gobject, param_id, pspec);
		return; /* NOTE : RETURN */
	}

	/* renderer set via parent or manually */
	if (init_state && view->renderer != NULL && view->model != NULL) {
		GogViewClass *klass = GOG_VIEW_GET_CLASS (view);
		GSList *ptr = view->model->children;

		for ( ;ptr != NULL ; ptr = ptr->next)
			gog_object_new_view (ptr->data, view);

		g_signal_connect_object (G_OBJECT (view->model),
			"child_added",
			G_CALLBACK (cb_child_added), view, 0);
		g_signal_connect_object (G_OBJECT (view->model),
			"child_removed",
			G_CALLBACK (cb_remove_child), view, 0);
		g_signal_connect_object (G_OBJECT (view->model),
			"changed",
			G_CALLBACK (cb_model_changed), view, 0);
		g_signal_connect_object (G_OBJECT (view->model),
			"children-reordered",
			G_CALLBACK (cb_model_reordered), view, G_CONNECT_SWAPPED);

		if (klass->state_init != NULL)
			(klass->state_init) (view);
	}
}

static void
gog_view_finalize (GObject *obj)
{
	GogView *tmp, *view = GOG_VIEW (obj);
	GSList *ptr;

	if (view->parent != NULL)
		view->parent->children = g_slist_remove (view->parent->children, view);

	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		tmp = GOG_VIEW (ptr->data);
		/* not really necessary, but helpful during initial deployment
		 * when not everything has a view yet */
		if (tmp != NULL) {
			tmp->parent = NULL; /* short circuit */
			g_object_unref (tmp);
		}
	}
	g_slist_free (view->children);
	view->children = NULL;

	(*parent_klass->finalize) (obj);
}

static void
gog_view_size_request_real (GogView *view, GogViewRequisition *req)
{
	req->w = req->h = 1.;
}

static void
gog_view_size_allocate_real (GogView *view, GogViewAllocation const *allocation)
{
	GSList *ptr;
	GogView *child;
	GogObjectPosition pos;
	GogViewRequisition req;
	GogViewAllocation tmp, available = *allocation, res = *allocation;
	double const pad_h = gog_renderer_pt2r_y (view->renderer, PAD_HACK);
	double const pad_w = gog_renderer_pt2r_x (view->renderer, PAD_HACK);

	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;

		pos = child->model->position;
		if (pos & GOG_POSITION_MANUAL) {
			/* position relative to the entire region */
			tmp = available;
			/* add some flags to control interpretation of manual
			 * eg abs/percentage from start/end */
			g_warning ("manual is not supported yet");
		} else if (pos & GOG_POSITION_COMPASS) {
			gboolean vertical = TRUE;

			/* Dead simple */
			gog_view_size_request (child, &req);
			if (req.h > res.h)
				req.h = res.h;
			if (req.w > res.w)
				req.w = res.w;
			tmp = res;

			if (pos & GOG_POSITION_N) {
				if (req.h > 0) {
					res.y += req.h + pad_h;
					res.h -= req.h + pad_h;
				} else
					req.h = 0;
				tmp.h  = req.h;
				vertical = FALSE;
			} else if (pos & GOG_POSITION_S) {
				if (req.h > 0) {
					res.h -= req.h + pad_h;
					tmp.y  = res.y + res.h + pad_h;
				} else
					req.h = 0;
				tmp.h  = req.h;
				vertical = FALSE;
			} 

				if (pos & GOG_POSITION_E) {
					if (req.w > 0) {
						res.w -= req.w + pad_w;
						tmp.x  = res.x + res.w + pad_w;
					} else
						req.w = 0;
					tmp.w  = req.w;
					/* For NE & NW only alignment fill makes sense */
					if (pos & (GOG_POSITION_N|GOG_POSITION_S))
						pos = GOG_POSITION_ALIGN_FILL;
				} else if (pos & GOG_POSITION_W) {
					if (req.w > 0) {
						res.x += req.w + pad_w;
						res.w -= req.w + pad_w;
					} else
						req.w = 0;
					tmp.w  = req.w;
					/* For NE & NW only alignment fill makes sense */
					if (pos & (GOG_POSITION_N|GOG_POSITION_S))
						pos = GOG_POSITION_ALIGN_FILL;
 				}

				pos &= GOG_POSITION_ALIGNMENT;
				if (GOG_POSITION_ALIGN_FILL != pos) {
					if (vertical) {
						if (GOG_POSITION_ALIGN_END == pos) {
							if (tmp.h >= req.h)
								tmp.y += tmp.h - req.h;
						} else if (GOG_POSITION_ALIGN_CENTER == pos) {
							if (tmp.h >= req.h)
								tmp.y += (tmp.h - req.h) / 2.;
						}
						tmp.h = req.h;
					} else {
						if (GOG_POSITION_ALIGN_END == pos) {
							if (tmp.w >= req.w)
								tmp.x += tmp.w - req.w;
						} else if (GOG_POSITION_ALIGN_CENTER == pos) {
							if (tmp.w >= req.w)
								tmp.x += (tmp.w - req.w) / 2.;
						}
						tmp.w = req.w;
					}
				}

			gog_view_size_allocate (child, &tmp);
		} else if (pos != GOG_POSITION_SPECIAL)
			g_warning ("unexpected position %x for child %p of %p",
				   pos, child, view);
	}
	view->residual = res;
}

/* A simple default implementation */
static void
gog_view_render_real (GogView *view, GogViewAllocation const *bbox)
{
	GSList *ptr;
	for (ptr = view->children ; ptr != NULL ; ptr = ptr->next)
		gog_view_render	(ptr->data, bbox);
}

static void
gog_view_class_init (GogViewClass *view_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *) view_klass;

	parent_klass = g_type_class_peek_parent (view_klass);
	gobject_klass->set_property = gog_view_set_property;
	gobject_klass->finalize	    = gog_view_finalize;
	view_klass->size_request    = gog_view_size_request_real;
	view_klass->size_allocate   = gog_view_size_allocate_real;
	view_klass->render	    = gog_view_render_real;
	view_klass->clip	    = FALSE;

	g_object_class_install_property (gobject_klass, GOG_VIEW_PROP_PARENT,
		g_param_spec_object ("parent", "parent",
			"the GogView parent",
			GOG_VIEW_TYPE, G_PARAM_WRITABLE));
	g_object_class_install_property (gobject_klass, GOG_VIEW_PROP_MODEL,
		g_param_spec_object ("model", "model",
			"the GogObject this view displays",
			GOG_OBJECT_TYPE, G_PARAM_WRITABLE));
}

static void
gog_view_init (GogView *view)
{
	view->allocation_valid  = FALSE;
	view->child_allocations_valid = FALSE;
	view->being_updated = FALSE;
	view->model	   = NULL;
	view->parent	   = NULL;
	view->children	   = NULL;
}

GSF_CLASS_ABSTRACT (GogView, gog_view,
		    gog_view_class_init, gog_view_init,
		    G_TYPE_OBJECT)

GogObject *
gog_view_get_model (GogView const *view)
{
	return view->model;
}

/**
 * gog_view_queue_redraw :
 * @view : a #GogView
 *
 * Requests a redraw for the entire graph.
 **/
void
gog_view_queue_redraw (GogView *view)
{
	g_return_if_fail (GOG_VIEW (view) != NULL);
	g_return_if_fail (view->renderer != NULL);

	gog_renderer_request_update (view->renderer);
}

/**
 * gog_view_queue_resize :
 * @view : a #GogView
 *
 * Flags a view to have its size renegotiated; should
 * be called when a model for some reason has a new size request.
 * For example, when you change the size of a legend.
 **/
void
gog_view_queue_resize (GogView *view)
{
	g_return_if_fail (GOG_VIEW (view) != NULL);
	g_return_if_fail (view->renderer != NULL);

	gog_renderer_request_update (view->renderer);

#if 0 /* optimization that breaks when child contributes to size of parent */
	view->allocation_valid = FALSE; /* in case there is no parent */
	if (NULL == (view = view->parent))
		return;
	view->allocation_valid = FALSE;
	while (NULL != (view = view->parent) && view->child_allocations_valid)
		view->child_allocations_valid = FALSE;
#else
	do
		view->allocation_valid = FALSE; /* in case there is no parent */
	while (NULL != (view = view->parent) && view->allocation_valid);
#endif
}

/**
 * gog_view_size_request :
 * @view : a #GogView
 * @requisition : a #GogViewRequisition.
 *
 * When called @requisition holds the available space and is populated with the
 * desired size based on that input and other elements of the view or its model's
 * state (eg the position).
 *
 * Remember that the size request is not necessarily the size a view will
 * actually be allocated.
 **/
void
gog_view_size_request (GogView *view, GogViewRequisition *requisition)
{
	GogViewClass *klass = GOG_VIEW_GET_CLASS (view);

	g_return_if_fail (klass != NULL);
	g_return_if_fail (requisition != NULL);
	if (klass->size_request)
		(klass->size_request) (view, requisition);
	else
		requisition->w = requisition->h = 1.;
}

/**
 * gog_view_size_allocate :
 * @view : a #GogView
 * @allocation: position and size to be allocated to @view
 *
 * Assign a size and position to a GogView.  Primarilly used by containers.
 **/
void
gog_view_size_allocate (GogView *view, GogViewAllocation const *allocation)
{
	GogViewClass *klass = GOG_VIEW_GET_CLASS (view);

	g_return_if_fail (allocation != NULL);
	g_return_if_fail (klass != NULL);
	g_return_if_fail (klass->size_allocate != NULL);
	g_return_if_fail (!view->being_updated);

	gog_debug (0, g_warning ("size_allocate %s %p : x = %g, y = %g w = %g, h = %g",
		   G_OBJECT_TYPE_NAME (view), view,
		   allocation->x, allocation->y, allocation->w, allocation->h););

	view->being_updated = TRUE;
	(klass->size_allocate) (view, allocation);
	view->being_updated = FALSE;

	if (&view->allocation != allocation)
		view->allocation = *allocation;
	view->allocation_valid = view->child_allocations_valid = TRUE;
}

gboolean
gog_view_update_sizes (GogView *view)
{
	g_return_val_if_fail (GOG_VIEW (view) != NULL, TRUE);
	g_return_val_if_fail (!view->being_updated, TRUE);

	if (!view->allocation_valid)
		gog_view_size_allocate (view, &view->allocation);
	else if (!view->child_allocations_valid) {
		GSList *ptr;

		view->being_updated = TRUE;
		for (ptr = view->children ; ptr != NULL ; ptr = ptr->next)
			gog_view_update_sizes (ptr->data);
		view->being_updated = FALSE;

		view->child_allocations_valid = TRUE;
	} else
		return FALSE;
	return TRUE;
}

void
gog_view_render	(GogView *view, GogViewAllocation const *bbox)
{
	GogViewClass *klass = GOG_VIEW_GET_CLASS (view);

	g_return_if_fail (view->renderer != NULL);

	if (view->residual.w < 0 || view->residual.h < 0)
		return;
	
	if (klass->clip) {
		gog_renderer_clip_push (view->renderer, &view->allocation);
		klass->render (view, bbox);
		gog_renderer_clip_pop (view->renderer);
	}
	else
		klass->render (view, bbox);
}

/**
 * gog_view_info_at_point :
 * @view : a #GogView
 * @x : 
 * @y :
 * @cur_selection : If @cur_selection is the object @x,@y, and it could create
 *	 a child there, create it
 * @obj : If non-NULL store the object @x,@y
 * @name : store the name of the most derived object even if it does not yet exist
 *	caller is responsible for freeing the string (ignored in NULL)
 *
 * Returns TRUE if an object is found
 **/
gboolean
gog_view_info_at_point (GogView *view, double x, double y,
			GogObject const *cur_selection,
			GogObject **obj, char **name)
{
	GSList *ptr;
	GogViewClass *klass = GOG_VIEW_GET_CLASS (view);

	g_return_val_if_fail (klass != NULL, FALSE);
	g_return_val_if_fail (view->allocation_valid, FALSE);
	g_return_val_if_fail (view->child_allocations_valid, FALSE);

	if (x < view->allocation.x ||
	    x >= (view->allocation.x + view->allocation.w) ||
	    y < view->allocation.y ||
	    y >= (view->allocation.y + view->allocation.h))
		return FALSE;

	for (ptr = view->children; ptr != NULL ; ptr = ptr->next)
		if (gog_view_info_at_point (ptr->data, x, y, cur_selection, obj, name))
			return TRUE;

	if (klass->info_at_point != NULL)
		return (klass->info_at_point) (view, x, y, cur_selection, obj, name);
	
	if (obj != NULL)
		*obj = view->model;
	if (name != NULL)
		*name = g_strdup (gog_object_get_name (view->model));

	return TRUE;
}

/**
 * gog_view_size_child_request :
 * @view : #GogView
 * @avail : the amount of space available in total
 * @req : holds the amount of space for the parent, and is expanded with the
 * 	needs of the children.
 *
 * Takes the space requested in @req and expands it to hold all @view->model's
 * children.
 * Returns the necessary size in @req.
 **/
void
gog_view_size_child_request (GogView *view,
			     GogViewRequisition const *avail,
			     GogViewRequisition *res)
{
	GSList *ptr, *list;
	GogView *child;
	GogObjectPosition pos;
	GogViewRequisition req;
	double const pad_h = gog_renderer_pt2r_y (view->renderer, PAD_HACK);
	double const pad_w = gog_renderer_pt2r_x (view->renderer, PAD_HACK);

	/* walk the list in reverse */
	list = g_slist_reverse (g_slist_copy (view->children));
	for (ptr = list; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;

		pos = child->model->position;
		if (pos & GOG_POSITION_MANUAL) {
			g_warning ("manual is not supported yet");
		} else if (pos & GOG_POSITION_COMPASS) {
			/* Dead simple */
			gog_view_size_request (child, &req);

			if (pos & (GOG_POSITION_N|GOG_POSITION_S)) {
				if (req.h > 0)
					res->h += req.h + pad_h;
			} else if (res->h < req.h)
				res->h = req.h;

			if (pos & (GOG_POSITION_E|GOG_POSITION_W)) {
				if (req.w > 0)
					res->w += req.w + pad_w;
			} else if (res->w < req.w)
				res->w = req.w;

		} else if (pos != GOG_POSITION_SPECIAL)
			g_warning ("unexpected position %x for child %p of %p",
				   pos, child, view);
	}
	g_slist_free (list);
}

/**
 * gog_view_find_child_view :
 * @container : #GogView
 * @target_model : #GogObject
 *
 * Find the GogView contained in @container that icorresponds to @model.
 * Returns NULL on error
 **/
GogView *
gog_view_find_child_view  (GogView const *container, GogObject const *target_model)
{
	GogObject const *obj, *old_target;
	GSList *ptr;

	g_return_val_if_fail (IS_GOG_VIEW (container), NULL);
	g_return_val_if_fail (IS_GOG_OBJECT (target_model), NULL);

	/* @container is a view for @target_models parent */
	obj = target_model;
	while (obj != NULL && container->model != obj)
		obj = obj->parent;

	g_return_val_if_fail (obj != NULL, NULL);

	for ( ; obj != target_model ; container = ptr->data) {
		/* find the parent of @target_object that should be a child of this view */
		old_target = obj;
		obj = target_model;
		while (obj != NULL && obj->parent != old_target)
			obj = obj->parent;

		g_return_val_if_fail (obj != NULL, NULL);

		for (ptr = container->children ; ptr != NULL ; ptr = ptr->next)
			if (GOG_VIEW (ptr->data)->model == obj)
				break;

		g_return_val_if_fail (ptr != NULL, NULL);
	}

	return (GogView *)container;
}
