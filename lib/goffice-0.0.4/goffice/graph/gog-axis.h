/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-axis.h : 
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#ifndef GOG_AXIS_H
#define GOG_AXIS_H

#include <goffice/graph/gog-axis-line.h>
#include <goffice/graph/goffice-graph.h>
#include <goffice/data/go-data.h>
#include <glib-object.h>

G_BEGIN_DECLS

typedef enum {
	GOG_AXIS_ELEM_MIN = 0,
	GOG_AXIS_ELEM_MAX,
	GOG_AXIS_ELEM_MAJOR_TICK,
	GOG_AXIS_ELEM_MINOR_TICK,
	GOG_AXIS_ELEM_CROSS_POINT,
	GOG_AXIS_ELEM_MAX_ENTRY
} GogAxisElemType;

typedef struct {
	double		 position;
	GogAxisTickTypes	 type;
	char 		*label;
} GogAxisTick;

typedef struct _GogAxisMapDesc GogAxisMapDesc;

typedef struct {
	GogAxis		*axis;
	GogAxisMapDesc	const *desc;
	gpointer	 data;
	gboolean	 is_valid;	/* Default to FALSE if desc::init == NULL */
} GogAxisMap;

struct _GogAxisMapDesc {
	double 		(*map) 		 (GogAxisMap *map, double value);
	double 		(*map_to_view)   (GogAxisMap *map, double value);
	double 		(*map_from_view) (GogAxisMap *map, double value);
	gboolean	(*map_finite)    (double value);
	double		(*map_baseline)  (GogAxisMap *map);
	void		(*map_bounds)	 (GogAxisMap *map, double *minimum, double *maximum);
	gboolean 	(*init) 	 (GogAxisMap *map, double offset, double length);
	void		(*destroy) 	 (GogAxisMap *map);
	void		(*auto_bound) 	 (GogAxis *axis, 
					  double minimum, double maximum,
					  double *bound);
	void		(*calc_ticks) 	 (GogAxis *axis);
	char const	*name;
	char const	*description;
};

GogAxisMap*   gog_axis_map_new	 	  (GogAxis *axis, double offset, double length);
double	      gog_axis_map 		  (GogAxisMap *map, double x);
double	      gog_axis_map_to_view	  (GogAxisMap *map, double x);
double	      gog_axis_map_from_view	  (GogAxisMap *map, double x);
gboolean      gog_axis_map_finite	  (GogAxisMap *map, double value);
double	      gog_axis_map_get_baseline	  (GogAxisMap *map);
void 	      gog_axis_map_get_extents 	  (GogAxisMap *map, double *start, double *stop);
void	      gog_axis_map_get_bounds 	  (GogAxisMap *map, double *minimum, double *maximum);
void 	      gog_axis_map_free		  (GogAxisMap *map);
gboolean      gog_axis_map_is_valid 	  (GogAxisMap *map);

/*****************************************************************************/

#define GOG_AXIS_TYPE	(gog_axis_get_type ())
#define GOG_AXIS(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_AXIS_TYPE, GogAxis))
#define IS_GOG_AXIS(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_AXIS_TYPE))

GType gog_axis_get_type (void);

GogAxisType   gog_axis_get_atype 	  (GogAxis const *axis);
gboolean      gog_axis_is_center_on_ticks (GogAxis const *axis);
gboolean      gog_axis_is_discrete        (GogAxis const *axis);
gboolean      gog_axis_is_inverted	  (GogAxis const *axis);
gboolean      gog_axis_get_bounds 	  (GogAxis const *axis,
					   double *minima, double *maxima);
unsigned      gog_axis_get_ticks 	  (GogAxis *axis, GogAxisTick **ticks);
GOData	     *gog_axis_get_labels	  (GogAxis const *axis,
					   GogPlot **plot_that_labeled_axis);

double 	      gog_axis_get_entry 	  (GogAxis const *axis, GogAxisElemType i, 
					   gboolean *user_defined);

void 	      gog_axis_add_contributor	  (GogAxis *axis, GogObject *contrib);
void 	      gog_axis_del_contributor	  (GogAxis *axis, GogObject *contrib);
GSList const *gog_axis_contributors	  (GogAxis *axis);
void	      gog_axis_clear_contributors (GogAxis *axis);
void	      gog_axis_bound_changed	  (GogAxis *axis, GogObject *contrib);

GogGridLine  *gog_axis_get_grid_line 	  (GogAxis *axis, gboolean major);

G_END_DECLS

#endif /* GOG_AXIS_H */
