/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-data-set.c : A Utility interface for managing GOData as attributes
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

#include <goffice/goffice-config.h>
#include <goffice/graph/gog-data-set.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/data/go-data.h>
#include <goffice/data/go-data-simple.h>
#include <goffice/utils/go-math.h>

GType
gog_dataset_get_type (void)
{
	static GType gog_dataset_type = 0;

	if (!gog_dataset_type) {
		static GTypeInfo const gog_dataset_info = {
			sizeof (GogDatasetClass),	/* class_size */
			NULL,		/* base_init */
			NULL,		/* base_finalize */
		};

		gog_dataset_type = g_type_register_static (G_TYPE_INTERFACE,
			"GogDataset", &gog_dataset_info, 0);
	}

	return gog_dataset_type;
}

/**
 * gog_dataset_dims :
 * @set : #GogDataset
 * @first : inclusive
 * @last : _inclusive_
 *
 * Returns the first and last valid indicises to get/set dim.
 **/
void
gog_dataset_dims (GogDataset const *set, int *first, int *last)
{
	GogDatasetClass *klass = GOG_DATASET_GET_CLASS (set);
	g_return_if_fail (klass != NULL);
	g_return_if_fail (first != NULL);
	g_return_if_fail (last != NULL);
	return (klass->dims) (set, first, last);
}

/**
 * gog_dataset_get_dim :
 * @set : #GogDataset
 * @dim_i :
 *
 * Returns the GOData associated with dimension @dim_i.  Does NOT add a
 * reference.
 **/
GOData *
gog_dataset_get_dim (GogDataset const *set, int dim_i)
{
	GogDatasetElement *elem = gog_dataset_get_elem (set, dim_i);
	g_return_val_if_fail (elem != NULL, NULL);
	return elem->data;
}

/**
 * gog_dataset_set_dim :
 * @series : #GogSeries
 * @dim_i :  < 0 gets the name
 * @val : #GOData
 * @err : #GError
 *
 * Absorbs a ref to @val if it is non NULL
 **/
void
gog_dataset_set_dim (GogDataset *set, int dim_i, GOData *val, GError **err)
{
	GogDatasetClass *klass;

	g_return_if_fail (val == NULL || GO_DATA (val) != NULL);

	if (set == NULL || !IS_GOG_DATASET (set)) {
		g_warning ("gog_dataset_set_dim called with invalid GogDataset");
		goto done;
	}

	klass = GOG_DATASET_GET_CLASS (set);

	/* short circuit */
	if (val != gog_dataset_get_dim (set, dim_i)) {
		gog_dataset_set_dim_internal (set, dim_i, val,
			gog_object_get_graph (GOG_OBJECT (set)));

		if (klass->set_dim)
			(klass->set_dim) (set, dim_i, val, err);
		if (klass->dim_changed)
			(klass->dim_changed) (set, dim_i);
	}

done :
	/* absorb ref to orig, simplifies life cycle easier for new GODatas */
	if (val != NULL)
		g_object_unref (val);
}

GogDatasetElement *
gog_dataset_get_elem (GogDataset const *set, int dim_i)
{
	GogDatasetClass *klass = GOG_DATASET_GET_CLASS (set);
	g_return_val_if_fail (klass != NULL, NULL);
	return (klass->get_elem) (set, dim_i);
}

static void
cb_dataset_dim_changed (GOData *data, GogDatasetElement *elem)
{
	GogDatasetClass *klass = GOG_DATASET_GET_CLASS (elem->set);

	g_return_if_fail (klass != NULL);
	if (klass->dim_changed)
		(klass->dim_changed) (elem->set, elem->dim_i);
}

/**
 * gog_dataset_set_dim_internal :
 * 
 * and internal routine to handle signal setup and teardown
 **/
void
gog_dataset_set_dim_internal (GogDataset *set, int dim_i,
			      GOData *val, GogGraph *graph)
{
	GogDatasetElement *elem = gog_dataset_get_elem (set, dim_i);

	g_return_if_fail (elem != NULL);

	if (graph != NULL) {
		if (val == elem->data)
			return;
		if (val != NULL)
			val = gog_graph_ref_data (graph, val);
		if (elem->handler != 0) {
			g_signal_handler_disconnect (G_OBJECT (elem->data),
				elem->handler);
			elem->handler = 0;
			gog_graph_unref_data (graph, elem->data);
		}
		if (val != NULL)
			elem->handler = g_signal_connect (
				G_OBJECT (val), "changed",
				G_CALLBACK (cb_dataset_dim_changed), elem);
	} else {
		if (val != NULL)
			g_object_ref (val);
		if (elem->data != NULL)
			g_object_unref (elem->data);
	}
	elem->data  = val;
	elem->set   = set;
	elem->dim_i = dim_i;
	gog_object_request_update (GOG_OBJECT (set));
}

void
gog_dataset_finalize (GogDataset *set)
{
	GogGraph *graph = gog_object_get_graph (GOG_OBJECT (set));
	int first, last;

	gog_dataset_dims (set, &first, &last);
	while (first <= last)
		gog_dataset_set_dim_internal (set, first++, NULL, graph);
}

void
gog_dataset_parent_changed (GogDataset *set, gboolean was_set)
{
	GogGraph *graph = gog_object_get_graph (GOG_OBJECT (set));
	GogDatasetElement *elem;
	GOData *dat;
	int i, last;

	for (gog_dataset_dims (set, &i, &last); i <= last ; i++) {
		elem = gog_dataset_get_elem (set, i);
		if (elem == NULL || elem->data == NULL)
			continue;
		dat = elem->data;
		if (!was_set) {
			g_object_ref (dat);
			gog_dataset_set_dim_internal (set, i, NULL, graph);
			elem->data = dat;
		} else if (elem->handler == 0) {
			elem->data = NULL; /* disable the short circuit */
			gog_dataset_set_dim_internal (set, i, dat, graph);
			g_object_unref (dat);
		}
	}
	if (was_set)
		gog_object_request_update (GOG_OBJECT (set));
}

void
gog_dataset_dup_to_simple (GogDataset const *src, GogDataset *dst)
{
	gint	     n, last;
	GOData *src_dat, *dst_dat;
	gog_dataset_dims (src, &n, &last);
	for ( ; n <= last ; n++) {
		src_dat = gog_dataset_get_dim (src, n);
		if (src_dat == NULL)
			continue;
		dst_dat = NULL;
		/* for scalar and vector data, try to transform to values first
		if we find non finite, use strings */
		if (IS_GO_DATA_SCALAR (src_dat)) {
			char const *str = go_data_scalar_get_str (GO_DATA_SCALAR (src_dat));
			char *end;
			double d =  g_strtod (str, &end);
			dst_dat =(*end == 0)? go_data_scalar_val_new (d):
						go_data_scalar_str_new (g_strdup (str), TRUE);
		} else if (IS_GO_DATA_VECTOR (src_dat)) {
			gboolean as_values = TRUE;
			GODataVector *vec = GO_DATA_VECTOR (src_dat);
			double *d = go_data_vector_get_values (vec);
			int i, n = go_data_vector_get_len (vec);
			for (i = 0; i < n; i++)
				if (go_finite (d[i])) {
					as_values = FALSE;
					break;
				}
			if (as_values)
				/* we don't need to duplicate, since this is used only for
				short lived objects */
				dst_dat = go_data_vector_val_new (d, n, NULL);
			else {
				char **str = g_new (char*, n + 1);
				str[n] = NULL;
				for (i = 0; i < n; i++)
					str[i] = go_data_vector_get_str (vec, i);
				dst_dat = go_data_vector_str_new ((char const* const*) str, n, g_free);
			}
		} else if (IS_GO_DATA_MATRIX (src_dat)) {
			/* only values are supported so don't care */
			GODataMatrix *mat = GO_DATA_MATRIX (src_dat);
			GODataMatrixSize size = go_data_matrix_get_size (mat);
			dst_dat = go_data_matrix_val_new (go_data_matrix_get_values (mat),
									size.rows, size.columns, NULL);
		}
		gog_dataset_set_dim (dst, n, dst_dat, NULL);
	}
}
