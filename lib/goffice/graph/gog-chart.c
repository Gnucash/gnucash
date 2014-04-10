/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-chart.c :
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
#include <goffice/graph/gog-chart-impl.h>
#include <goffice/graph/gog-plot-impl.h>
#include <goffice/graph/gog-graph-impl.h>
#include <goffice/graph/gog-style.h>
#include <goffice/graph/gog-view.h>
#include <goffice/graph/gog-axis.h>
#include <goffice/graph/gog-grid.h>
#include <goffice/graph/gog-grid-line.h>
#include <goffice/graph/gog-renderer.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <string.h>
#include <math.h>

enum {
	CHART_PROP_0,
	CHART_PROP_CARDINALITY_VALID
};

static GType gog_chart_view_get_type (void);
static GObjectClass *chart_parent_klass;

static void
gog_chart_update (GogObject *obj)
{
	GogChart *chart = GOG_CHART (obj);
	unsigned full = chart->full_cardinality;
	unsigned visible = chart->visible_cardinality;

	gog_chart_get_cardinality (chart, NULL, NULL);

	if (full != chart->full_cardinality ||
	    visible != chart->visible_cardinality)
		g_object_notify (G_OBJECT (chart), "cardinality-valid");
}

static void
gog_chart_finalize (GObject *obj)
{
	GogChart *chart = GOG_CHART (obj);

	/* on exit the role remove routines are not called */
	g_slist_free (chart->plots);
	g_slist_free (chart->axes);

	(chart_parent_klass->finalize) (obj);
}

static void
gog_chart_get_property (GObject *obj, guint param_id,
			GValue *value, GParamSpec *pspec)
{
	GogChart *chart = GOG_CHART (obj);
	switch (param_id) {
	case CHART_PROP_CARDINALITY_VALID:
		g_value_set_boolean (value, chart->cardinality_valid);
		break;

	default: G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, param_id, pspec);
		 break;
	}
}

static void
gog_chart_children_reordered (GogObject *obj)
{
	GSList *ptr, *accum = NULL;
	GogChart *chart = GOG_CHART (obj);

	for (ptr = obj->children; ptr != NULL ; ptr = ptr->next)
		if (IS_GOG_PLOT (ptr->data))
			accum = g_slist_prepend (accum, ptr->data);
	g_slist_free (chart->plots);
	chart->plots = g_slist_reverse (accum);

	gog_chart_request_cardinality_update (chart);
}

static void
role_plot_post_add (GogObject *parent, GogObject *plot)
{
	GogChart *chart = GOG_CHART (parent);
	gboolean ok = TRUE;

	/* APPEND to keep order, there won't be that many */
	chart->plots = g_slist_append (chart->plots, plot);
	gog_chart_request_cardinality_update (chart);

	if (chart->plots->next == NULL)
		ok = gog_chart_axis_set_assign (chart,
			gog_plot_axis_set_pref (GOG_PLOT (plot)));
	ok |= gog_plot_axis_set_assign (GOG_PLOT (plot),
		chart->axis_set);

	/* a quick post condition to keep us on our toes */
	g_return_if_fail (ok);
}

static void
role_plot_pre_remove (GogObject *parent, GogObject *plot)
{
	GogChart *chart = GOG_CHART (parent);
	gog_plot_axis_clear (GOG_PLOT (plot), GOG_AXIS_SET_ALL);
	chart->plots = g_slist_remove (chart->plots, plot);
	gog_chart_request_cardinality_update (chart);
}

static gboolean
role_grid_can_add (GogObject const *parent)
{
	GogChart const *chart = GOG_CHART (parent);
	return chart->grid == NULL && chart->axis_set == GOG_AXIS_SET_XY;
}
static void
role_grid_post_add (GogObject *parent, GogObject *child)
{
	GogChart *chart = GOG_CHART (parent);
	g_return_if_fail (chart->grid == NULL);
	chart->grid = child;
}

static void
role_grid_pre_remove (GogObject *parent, GogObject *grid)
{
	GogChart *chart = GOG_CHART (parent);
	g_return_if_fail (chart->grid == grid);
	chart->grid = NULL;
}

static gboolean
axis_can_add (GogObject const *parent, GogAxisType t)
{
	GogChart *chart = GOG_CHART (parent);
	if (chart->axis_set == GOG_AXIS_SET_UNKNOWN)
		return FALSE;
	return (chart->axis_set & (1 << t)) != 0;
}
static gboolean
axis_can_remove (GogObject const *child)
{
	return NULL == gog_axis_contributors (GOG_AXIS (child));
}

static void
axis_post_add (GogObject *axis, GogAxisType t)
{
	GogChart *chart = GOG_CHART (axis->parent);
	g_object_set (G_OBJECT (axis), "type", (int)t, NULL);
	chart->axes = g_slist_prepend (chart->axes, axis);
}

static void
axis_pre_remove (GogObject *parent, GogObject *axis)
{
	GogChart *chart = GOG_CHART (parent);
	gog_axis_clear_contributors (GOG_AXIS (axis));
	chart->axes = g_slist_remove (chart->axes, axis);
}

static gboolean x_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_X); }
static void x_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_X); }
static gboolean y_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_Y); }
static void y_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_Y); }
static gboolean z_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_Z); }
static void z_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_Z); }
static gboolean circular_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_CIRCULAR); }
static void circular_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_CIRCULAR); }
static gboolean radial_axis_can_add (GogObject const *parent) { return axis_can_add (parent, GOG_AXIS_RADIAL); }
static void radial_axis_post_add    (GogObject *parent, GogObject *child)  { axis_post_add   (child, GOG_AXIS_RADIAL); }

static GogObjectRole const roles[] = {
	{ N_("Legend"), "GogLegend",	0,
	  GOG_POSITION_COMPASS, GOG_POSITION_E|GOG_POSITION_ALIGN_CENTER, GOG_OBJECT_NAME_BY_ROLE,
	  NULL, NULL, NULL, NULL, NULL, NULL, { -1 } },
	{ N_("Title"), "GogLabel",	1,
	  GOG_POSITION_COMPASS, GOG_POSITION_N|GOG_POSITION_ALIGN_CENTER, GOG_OBJECT_NAME_BY_ROLE,
	  NULL, NULL, NULL, NULL, NULL, NULL, { -1 } },
	{ N_("Grid"), "GogGrid",	0,
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
	  role_grid_can_add, NULL, NULL, role_grid_post_add, role_grid_pre_remove, NULL, { -1 } },
	{ N_("X-Axis"), "GogAxis",	1,
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
	  x_axis_can_add, axis_can_remove, NULL, x_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_X } },
	{ N_("Y-Axis"), "GogAxis",	2,
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
	  y_axis_can_add, axis_can_remove, NULL, y_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_Y } },
	{ N_("Z-Axis"), "GogAxis",	3,
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
	  z_axis_can_add, axis_can_remove, NULL, z_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_Z } },
	{ N_("Circular-Axis"), "GogAxis", 1,
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
	  circular_axis_can_add, axis_can_remove, NULL, circular_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_CIRCULAR } },
	{ N_("Radial-Axis"), "GogAxis",	2,
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_ROLE,
	  radial_axis_can_add, axis_can_remove, NULL, radial_axis_post_add, axis_pre_remove, NULL,
	  { GOG_AXIS_RADIAL } },
	{ N_("Plot"), "GogPlot",	4,	/* keep the axis before the plots */
	  GOG_POSITION_SPECIAL, GOG_POSITION_SPECIAL, GOG_OBJECT_NAME_BY_TYPE,
	  NULL, NULL, NULL, role_plot_post_add, role_plot_pre_remove, NULL, { -1 } }
};

static void
gog_chart_class_init (GogObjectClass *gog_klass)
{
	GObjectClass *gobject_klass = (GObjectClass *)gog_klass;

	chart_parent_klass = g_type_class_peek_parent (gog_klass);
	gobject_klass->finalize = gog_chart_finalize;
	gobject_klass->get_property = gog_chart_get_property;

	g_object_class_install_property (gobject_klass, CHART_PROP_CARDINALITY_VALID,
		g_param_spec_boolean ("cardinality-valid", "cardinality-valid",
			"Is the charts cardinality currently vaid",
			FALSE, G_PARAM_READABLE));

	gog_klass->view_type = gog_chart_view_get_type ();
	gog_klass->update    = gog_chart_update;
	gog_klass->children_reordered = gog_chart_children_reordered;
	gog_object_register_roles (gog_klass, roles, G_N_ELEMENTS (roles));
}

static void
gog_chart_init (GogChart *chart)
{
	chart->x     = 0;
	chart->y     = 0;
	chart->cols  = 0;
	chart->rows  = 0;

	/* start as true so that we can queue an update when it changes */
	chart->cardinality_valid = TRUE;
	chart->axis_set = GOG_AXIS_SET_UNKNOWN;
}

GSF_CLASS (GogChart, gog_chart,
	   gog_chart_class_init, gog_chart_init,
	   GOG_OUTLINED_OBJECT_TYPE)

/**
 * gog_chart_get_position :
 * @chart : const #GogChart
 * @x :
 * @y :
 * @cols :
 * @rows :
 *
 * Returns TRUE if the chart has been positioned.
 **/
gboolean
gog_chart_get_position (GogChart const *chart,
			unsigned *x, unsigned *y, unsigned *cols, unsigned *rows)
{
	g_return_val_if_fail (GOG_CHART (chart), FALSE);

	if (chart->cols <= 0 || chart->rows <= 0)
		return FALSE;

	if (x != NULL)	  *x	= chart->x;
	if (y != NULL)	  *y	= chart->y;
	if (cols != NULL) *cols	= chart->cols;
	if (rows != NULL) *rows	= chart->rows;

	return TRUE;
}

/**
 * gog_chart_set_position :
 * @chart : #GogChart
 * @x :
 * @y :
 * @cols :
 * @rows :
 *
 **/
void
gog_chart_set_position (GogChart *chart,
			unsigned x, unsigned y, unsigned cols, unsigned rows)
{
	g_return_if_fail (GOG_CHART (chart) != NULL);

	if (chart->x == x && chart->y == y &&
	    chart->cols == cols && chart->rows == rows)
		return;

	chart->x = x;
	chart->y = y;
	chart->cols = cols;
	chart->rows = rows;

	gog_graph_validate_chart_layout (GOG_GRAPH (GOG_OBJECT (chart)->parent));
	gog_object_emit_changed (GOG_OBJECT (chart), TRUE);
}

void
gog_chart_get_cardinality (GogChart *chart, unsigned *full, unsigned *visible)
{
	GSList *ptr;
	unsigned tmp_full, tmp_visible;

	g_return_if_fail (GOG_CHART (chart) != NULL);

	if (!chart->cardinality_valid) {
		chart->cardinality_valid = TRUE;
		chart->full_cardinality = chart->visible_cardinality = 0;
		for (ptr = chart->plots ; ptr != NULL ; ptr = ptr->next) {
			gog_plot_get_cardinality (ptr->data, &tmp_full, &tmp_visible);
			chart->full_cardinality += tmp_full;
			chart->visible_cardinality += tmp_visible;
		}
	}

	if (full != NULL)
		*full = chart->full_cardinality;
	if (visible != NULL)
		*visible = chart->visible_cardinality;
}

void
gog_chart_request_cardinality_update (GogChart *chart)
{
	g_return_if_fail (GOG_CHART (chart) != NULL);
	
	if (chart->cardinality_valid) {
		chart->cardinality_valid = FALSE;
		gog_object_request_update (GOG_OBJECT (chart));
	}
}

void
gog_chart_foreach_elem (GogChart *chart, gboolean only_visible,
			GogEnumFunc handler, gpointer data)
{
	GSList *ptr;

	g_return_if_fail (GOG_CHART (chart) != NULL);
	g_return_if_fail (chart->cardinality_valid);

	for (ptr = chart->plots ; ptr != NULL ; ptr = ptr->next)
		gog_plot_foreach_elem (ptr->data, only_visible, handler, data);
}

GSList *
gog_chart_get_plots (GogChart const *chart)
{
	g_return_val_if_fail (GOG_CHART (chart) != NULL, NULL);
	return chart->plots;
}

GogAxisSet
gog_chart_axis_set (GogChart const *chart)
{
	g_return_val_if_fail (GOG_CHART (chart) != NULL, GOG_AXIS_SET_UNKNOWN);
	return chart->axis_set;
}

gboolean
gog_chart_axis_set_is_valid (GogChart const *chart, GogAxisSet type)
{
	GSList *ptr;

	g_return_val_if_fail (GOG_CHART (chart) != NULL, FALSE);

	for (ptr = chart->plots ; ptr != NULL ; ptr = ptr->next)
		if (!gog_plot_axis_set_is_valid (ptr->data, type))
			return FALSE;
	return TRUE;
}

static void
gog_chart_add_axis (GogChart *chart, GogAxisType type)
{
	unsigned i = G_N_ELEMENTS (roles);
	while (i-- > 0)
		if (roles[i].user.i == (int)type) {
			gog_object_add_by_role (GOG_OBJECT (chart), roles + i, NULL);
			return;
		}
	g_warning ("unknown axis type %d", type);
}

gboolean
gog_chart_axis_set_assign (GogChart *chart, GogAxisSet axis_set)
{
	GogAxis *axis;
	GSList  *ptr;
	GogAxisType type;

	g_return_val_if_fail (GOG_CHART (chart) != NULL, FALSE);

	if (chart->axis_set == axis_set)
		return TRUE;
	chart->axis_set = axis_set;

	if (chart->grid != NULL && axis_set != GOG_AXIS_SET_XY) {
		GogObject *grid = chart->grid; /* clear_parent clears ::grid */
		gog_object_clear_parent (GOG_OBJECT (grid));
		g_object_unref (grid);
	} else if (chart->grid == NULL && axis_set == GOG_AXIS_SET_XY)
		gog_object_add_by_name (GOG_OBJECT (chart), "Grid", NULL);

	/* Add at least 1 instance of any required axis */
	for (type = 0 ; type < GOG_AXIS_TYPES ; type++)
		if ((axis_set & (1 << type))) {
			GSList *tmp = gog_chart_get_axis (chart, type);
			if (tmp == NULL)
				gog_chart_add_axis (chart, type);
			else
				g_slist_free (tmp);
		}

	/* link the plots */
	for (ptr = chart->plots ; ptr != NULL ; ptr = ptr->next)
		if (!gog_plot_axis_set_assign (ptr->data, axis_set))
			return FALSE;

	/* remove any existing axis that do not fit this scheme */
	for (ptr = GOG_OBJECT (chart)->children ; ptr != NULL ; ) {
		axis = ptr->data;
		ptr = ptr->next; /* list may change under us */
		if (IS_GOG_AXIS (axis)) {
			type = -1;
			g_object_get (G_OBJECT (axis), "type", &type, NULL);
			if (type < 0 || type >= GOG_AXIS_TYPES) {
				g_warning ("Invalid axis");
				continue;
			}

			if (0 == (axis_set & (1 << type))) {
				gog_object_clear_parent (GOG_OBJECT (axis));
				g_object_unref (axis);
			}
		}
	}

	return TRUE;
}

/**
 * gog_chart_get_axis :
 * @chart : #GogChart
 * @target  : #GogAxisType
 *
 * Return a list which the caller must free of all axis of type @target
 * associated with @chart.
 **/
GSList *
gog_chart_get_axis (GogChart const *chart, GogAxisType target)
{
	GSList *ptr, *res = NULL;
	GogAxis *axis;
	int type;

	g_return_val_if_fail (GOG_CHART (chart) != NULL, NULL);

	for (ptr = GOG_OBJECT (chart)->children ; ptr != NULL ; ptr = ptr->next) {
		axis = ptr->data;
		if (IS_GOG_AXIS (axis)) {
			type = -1;
			g_object_get (G_OBJECT (axis), "type", &type, NULL);
			if (type < 0 || type >= GOG_AXIS_TYPES) {
				g_warning ("Invalid axis");
				continue;
			}
			if (type == target)
				res = g_slist_prepend (res, axis);
		}
	}

	return res;
}

/**
 * gog_chart_get_grid :
 * @chart : #GogChart
 *
 * Returns the grid associated with @chart if one exists
 * otherwise NULL.
 **/
GogGrid  *
gog_chart_get_grid (GogChart const *chart)
{
	g_return_val_if_fail (GOG_CHART (chart) != NULL, NULL);
	return GOG_GRID (chart->grid);
}

/*********************************************************************/

typedef struct {
	GogOutlinedView base;

	GogViewAllocation	plot_area;
} GogChartView;
typedef GogOutlinedViewClass	GogChartViewClass;

#define GOG_CHART_VIEW_TYPE	(gog_chart_view_get_type ())
#define GOG_CHART_VIEW(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_CHART_VIEW_TYPE, GogChartView))
#define IS_GOG_CHART_VIEW(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_CHART_VIEW_TYPE))

static GogViewClass *cview_parent_klass;

GogViewAllocation const *
gog_chart_view_get_plot_area (GogView const *view)
{
	g_return_val_if_fail ((GOG_CHART_VIEW (view) != NULL), NULL);

	return & (GOG_CHART_VIEW(view)->plot_area);
}

static void
child_request (GogView *view, GogViewAllocation *res, 
	       GogViewAllocation const *plot_area,
	       gboolean allocate)
{
	GSList *ptr;
	GogView *child;
	GogAxis const *axis;
	GogViewRequisition req;
	GogViewAllocation allocation;

	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;
		if (child->model->position != GOG_POSITION_SPECIAL ||
		    !IS_GOG_AXIS (child->model))
			continue;

		axis = GOG_AXIS (child->model);
		req.w = req.h = 0.;
		gog_view_size_request (child, &req);
		allocation = *plot_area;
		switch (gog_axis_get_atype (axis)) {
			case GOG_AXIS_X:
				if (req.h > 0) {
					res->h -= req.h;
					allocation.h = req.h;
					if (gog_axis_get_pos (axis) == GOG_AXIS_AT_HIGH) {
						allocation.y = res->y;
						res->y += req.h;
					} else
						allocation.y = res->y + res->h;	
				}
				
				
				break;
			case GOG_AXIS_Y:
				if (req.w > 0) {
					res->w -= req.w;
					allocation.w = req.w;
					if (gog_axis_get_pos (axis) == GOG_AXIS_AT_LOW) {
						allocation.x = res->x;
						res->x += req.w;
					} else
						allocation.x = res->x + res->w;
				}
				break;
			default:
				break;
		}
		if (allocate)
			gog_view_size_allocate (child, &allocation);
	}
}

static void
gog_chart_view_size_allocate (GogView *view, GogViewAllocation const *allocation)
{
	GSList *ptr;
	GogView *child;
	GogChart *chart = GOG_CHART (view->model);
	GogViewAllocation res = *allocation;
	GogViewAllocation tmp, axis_alloc;

	(cview_parent_klass->size_allocate) (view, &res);

	res = view->residual; 
	switch (chart->axis_set) {

		case GOG_AXIS_SET_XY:
		{
			GogViewPadding axis_padding, padding = {0., 0., 0., 0.};

			tmp = res;
			child_request (view, &res, &res, FALSE);
			
			/* FIXME: we need to iterate until convergence */ 
			for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
				child = ptr->data;
				if (child->model->position != GOG_POSITION_SPECIAL ||
				    !IS_GOG_AXIS (child->model))
					continue;
				
				gog_axis_view_padding_request (child, &axis_padding, &res);
				padding.wr = MAX (padding.wr, axis_padding.wr);
				padding.wl = MAX (padding.wl, axis_padding.wl);
				padding.hb = MAX (padding.hb, axis_padding.hb);
				padding.ht = MAX (padding.ht, axis_padding.ht);
			}
			res.x += padding.wl;
			res.w -= padding.wl + padding.wr;
			res.y += padding.ht;
			res.h -= padding.ht + padding.hb;

			for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
				child = ptr->data;
				if (child->model->position != GOG_POSITION_SPECIAL ||
				    !IS_GOG_AXIS (child->model))
					continue;

				switch (gog_axis_get_atype (GOG_AXIS (child->model))) 
				{
					case GOG_AXIS_X:
						axis_alloc = tmp;
						axis_alloc.x = res.x;
						axis_alloc.w = res.w;
						gog_view_size_allocate (child, &axis_alloc);
						break;

					case GOG_AXIS_Y:
						axis_alloc = tmp;
						axis_alloc.y = res.y;
						axis_alloc.h = res.h;
						gog_view_size_allocate (child, &axis_alloc);
						break;

					default:
						break;
				}	
			}
		}		      
		break;

	case GOG_AXIS_SET_RADAR:
		/* Give the axes the whole residual area. */
		for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
			child = ptr->data;
			if (IS_GOG_AXIS (child->model))
				gog_view_size_allocate (child, &res);
		}
		break;
	case GOG_AXIS_SET_NONE:
		break;

	case GOG_AXIS_SET_UNKNOWN:
		return;
	default:
		g_warning ("only have layout engine for xy, radar, and none currently");
		return;
	}

	/* overlay all the plots in the residual */
	for (ptr = view->children; ptr != NULL ; ptr = ptr->next) {
		child = ptr->data;
		if (child->model->position == GOG_POSITION_SPECIAL &&
		    (IS_GOG_PLOT (child->model) || child->model == chart->grid))
			gog_view_size_allocate (child, &res);
	}
	
	GOG_CHART_VIEW(view)->plot_area = res;
}

static void
gog_chart_view_init (GogChartView *cview)
{
}

static void
grid_line_render (GSList *start_ptr, GogViewAllocation const *bbox) 
{
	GSList *ptr, *child_ptr;
	GogView *child_view, *axis_child_view;

	/* Render minor lines first */
	for (ptr = start_ptr; ptr != NULL; ptr = ptr->next) {
		child_view = ptr->data;
		if (IS_GOG_AXIS (child_view->model)) {
			for (child_ptr = child_view->children; child_ptr != NULL; child_ptr = child_ptr->next) {
				axis_child_view = child_ptr->data;
				if (IS_GOG_GRID_LINE (axis_child_view->model) &&
				    gog_grid_line_is_minor (GOG_GRID_LINE (axis_child_view->model)))
					gog_view_render (axis_child_view, bbox);
			}
		}
	}	    
	/* then render major lines */
	for (ptr = start_ptr; ptr != NULL; ptr = ptr->next) {
		child_view = ptr->data;
		if (IS_GOG_AXIS (child_view->model)) {
			for (child_ptr = child_view->children; child_ptr != NULL; child_ptr = child_ptr->next) {
				axis_child_view = child_ptr->data;
				if (IS_GOG_GRID_LINE (axis_child_view->model) &&
				    !gog_grid_line_is_minor (GOG_GRID_LINE (axis_child_view->model)))
					gog_view_render (axis_child_view, bbox);
			}
		}
	}	    
}

static void
gog_chart_view_render (GogView *view, GogViewAllocation const *bbox)
{
	GSList *ptr;
	GogView *child_view;
	gboolean grid_line_rendered = FALSE;

	cview_parent_klass->render (view, bbox);

	/* KLUDGE: render grid lines before axis */
	for (ptr = view->children ; ptr != NULL ; ptr = ptr->next) {
		child_view = ptr->data;
		if (!grid_line_rendered && IS_GOG_AXIS (child_view->model)) {
			grid_line_render (ptr, bbox);
			grid_line_rendered = TRUE;
		}
		gog_view_render	(ptr->data, bbox);
	}
}

static void
gog_chart_view_class_init (GogChartViewClass *gview_klass)
{
	GogViewClass *view_klass = (GogViewClass *) gview_klass;
	GogOutlinedViewClass *oview_klass = (GogOutlinedViewClass *) gview_klass;

	cview_parent_klass = g_type_class_peek_parent (gview_klass);
	view_klass->size_allocate   = gog_chart_view_size_allocate;
	view_klass->clip = TRUE;
	view_klass->render = gog_chart_view_render;
	oview_klass->call_parent_render = FALSE;
}

static GSF_CLASS (GogChartView, gog_chart_view,
		  gog_chart_view_class_init, gog_chart_view_init,
		  GOG_OUTLINED_VIEW_TYPE)
