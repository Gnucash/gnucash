/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-data.c :
 *
 * Copyright (C) 2003-2005 Jody Goldberg (jody@gnome.org)
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
#include "go-data.h"
#include "go-data-impl.h"
#include <goffice/utils/go-math.h>

#include <gsf/gsf-impl-utils.h>
#include <glib/gi18n.h>
#include <string.h>

#define GO_DATA_CLASS(k)	(G_TYPE_CHECK_CLASS_CAST ((k), GO_DATA_TYPE, GODataClass))
#define IS_GO_DATA_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GO_DATA_TYPE))
#define GO_DATA_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GO_DATA_TYPE, GODataClass))

enum {
	CHANGED,
	LAST_SIGNAL
};

static gulong go_data_signals [LAST_SIGNAL] = { 0, };

/* trivial fall back */
static GOData *
go_data_dup_real (GOData const *src)
{
	char   *str = go_data_as_str (src);
	GOData *dst = g_object_new (G_OBJECT_TYPE (src), NULL);
	if (dst != NULL)
		go_data_from_str (dst, str);
	g_free (str);

	return dst;
}

static void
go_data_init (GOData *data)
{
	data->flags = 0;
}

#if 0
static GObjectClass *parent_klass;
static void
go_data_finalize (GOData *obj)
{
	g_warning ("finalize");
	(parent_klass->finalize) (obj);
}
#endif

static void
go_data_class_init (GODataClass *klass)
{
	go_data_signals [CHANGED] = g_signal_new ("changed",
		G_TYPE_FROM_CLASS (klass),
		G_SIGNAL_RUN_LAST,
		G_STRUCT_OFFSET (GODataClass, changed),
		NULL, NULL,
		g_cclosure_marshal_VOID__VOID,
		G_TYPE_NONE, 0);
	klass->dup = go_data_dup_real;
#if 0
	{
		GObjectClass *gobj_klass = (GObjectClass *)klass;
		gobj_klass->finalize = go_data_finalize;
		parent_klass = g_type_class_peek_parent (klass);
	}
#endif
}

GSF_CLASS_ABSTRACT (GOData, go_data,
		    go_data_class_init, go_data_init,
		    G_TYPE_OBJECT)

/**
 * go_data_dup : 
 * @src : #GOData
 *
 * A deep copy of @src.
 **/
GOData *
go_data_dup (GOData const *src)
{
	if (src != NULL) {
		GODataClass const *klass = GO_DATA_GET_CLASS (src);
		g_return_val_if_fail (klass != NULL, NULL);
		return (*klass->dup) (src);
	}
	return NULL;
}

/**
 * go_data_eq :
 * @a : #GOData
 * @b : #GOData
 *
 * Returns TRUE if @a and @b are the same
 **/
gboolean
go_data_eq (GOData const *a, GOData const *b)
{
	if (a == b)
		return TRUE;
	else {
		GODataClass *a_klass = GO_DATA_GET_CLASS (a);
		GODataClass *b_klass = GO_DATA_GET_CLASS (b);

		g_return_val_if_fail (a_klass != NULL, FALSE);
		g_return_val_if_fail (a_klass->eq != NULL, FALSE);

		if (a_klass != b_klass)
			return FALSE;

		return (*a_klass->eq) (a, b);
	}
}

/**
 * go_data_prefered_fmt :
 * @dat : #GOData
 *
 * Caller is responsible for unrefing the result.
 * Returns the fmt preferred by the the data
 **/
GOFormat *
go_data_preferred_fmt (GOData const *dat)
{
	GODataClass const *klass = GO_DATA_GET_CLASS (dat);
	g_return_val_if_fail (klass != NULL, NULL);
	if (klass->preferred_fmt)
		return (*klass->preferred_fmt) (dat);
	return NULL;
}

/**
 * go_data_as_str :
 * @dat : #GOData
 *
 * Return a string representation of the data source that the caller is
 * responsible for freeing
 *
 * NOTE : This is the _source_ not the content.
 **/
char *
go_data_as_str (GOData const *dat)
{
	GODataClass const *klass = GO_DATA_GET_CLASS (dat);
	g_return_val_if_fail (klass != NULL, NULL);
	return (*klass->as_str) (dat);
}

/**
 * go_data_from_str :
 * @dat : #GOData
 * @str :
 *
 * De-serializes the source information returned from go_data_as_str.
 * Returns FALSE on error.
 **/
gboolean
go_data_from_str (GOData *dat, char const *str)
{
	GODataClass const *klass = GO_DATA_GET_CLASS (dat);
	g_return_val_if_fail (klass != NULL, FALSE);
	return (*klass->from_str) (dat, str);
}

/**
 * go_data_emit_changed :
 * @dat : #GOData
 *
 * protected utility to emit a 'changed' signal
 **/
void
go_data_emit_changed (GOData *dat)
{
	GODataClass const *klass = GO_DATA_GET_CLASS (dat);

	g_return_if_fail (klass != NULL);

	if (klass->emit_changed)
		(*klass->emit_changed) (dat);

	g_signal_emit (G_OBJECT (dat), go_data_signals [CHANGED], 0);
}

/*************************************************************************/

#define GO_DATA_SCALAR_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST ((k), GO_DATA_SCALAR_TYPE, GODataScalarClass))
#define IS_GO_DATA_SCALAR_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GO_DATA_SCALAR_TYPE))
#define GO_DATA_SCALAR_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GO_DATA_SCALAR_TYPE, GODataScalarClass))

GSF_CLASS_ABSTRACT (GODataScalar, go_data_scalar,
		    NULL, NULL,
		    GO_DATA_TYPE)

double
go_data_scalar_get_value (GODataScalar *scalar)
{
	GODataScalarClass const *klass = GO_DATA_SCALAR_GET_CLASS (scalar);
	g_return_val_if_fail (klass != NULL, 0.); /* TODO : make this a nan */
	return (*klass->get_value) (scalar);
}

char const *
go_data_scalar_get_str (GODataScalar *scalar)
{
	GODataScalarClass const *klass = GO_DATA_SCALAR_GET_CLASS (scalar);
	g_return_val_if_fail (klass != NULL, NULL);
	return (*klass->get_str) (scalar);
}

/*************************************************************************/

#define GO_DATA_VECTOR_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST ((k), GO_DATA_VECTOR_TYPE, GODataVectorClass))
#define IS_GO_DATA_VECTOR_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GO_DATA_VECTOR_TYPE))
#define GO_DATA_VECTOR_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GO_DATA_VECTOR_TYPE, GODataVectorClass))

static void
go_data_vector_emit_changed (GOData *data)
{
	data->flags &= ~(GO_DATA_CACHE_IS_VALID | GO_DATA_VECTOR_LEN_CACHED);
}
static void
go_data_vector_class_init (GODataClass *klass)
{
	klass->emit_changed = go_data_vector_emit_changed;
}

GSF_CLASS_ABSTRACT (GODataVector, go_data_vector,
		    go_data_vector_class_init, NULL,
		    GO_DATA_TYPE)

int
go_data_vector_get_len (GODataVector *vec)
{
	if (! (vec->base.flags & GO_DATA_VECTOR_LEN_CACHED)) {
		GODataVectorClass const *klass = GO_DATA_VECTOR_GET_CLASS (vec);

		g_return_val_if_fail (klass != NULL, 0);

		(*klass->load_len) (vec);

		g_return_val_if_fail (vec->base.flags & GO_DATA_VECTOR_LEN_CACHED, 0);
	}

	return vec->len;
}

double *
go_data_vector_get_values (GODataVector *vec)
{
	if (! (vec->base.flags & GO_DATA_CACHE_IS_VALID)) {
		GODataVectorClass const *klass = GO_DATA_VECTOR_GET_CLASS (vec);

		g_return_val_if_fail (klass != NULL, NULL);

		(*klass->load_values) (vec);

		g_return_val_if_fail (vec->base.flags & GO_DATA_CACHE_IS_VALID, NULL);
	}

	return vec->values;
}

double
go_data_vector_get_value (GODataVector *vec, unsigned i)
{
	if (! (vec->base.flags & GO_DATA_CACHE_IS_VALID)) {
		GODataVectorClass const *klass = GO_DATA_VECTOR_GET_CLASS (vec);
		g_return_val_if_fail (klass != NULL, go_nan);
		return (*klass->get_value) (vec, i);
	}

	g_return_val_if_fail ((int)i < vec->len, go_nan);
	return vec->values [i];
}

char *
go_data_vector_get_str (GODataVector *vec, unsigned i)
{
	GODataVectorClass const *klass = GO_DATA_VECTOR_GET_CLASS (vec);
	char *res;

	g_return_val_if_fail (klass != NULL, g_strdup (""));
	g_return_val_if_fail ((int)i < vec->len, g_strdup (""));

	res = (*klass->get_str) (vec, i);
	if (res == NULL)
		return g_strdup ("");
	return res;
}

void
go_data_vector_get_minmax (GODataVector *vec, double *min, double *max)
{
	if (! (vec->base.flags & GO_DATA_CACHE_IS_VALID)) {
		GODataVectorClass const *klass = GO_DATA_VECTOR_GET_CLASS (vec);

		g_return_if_fail (klass != NULL);

		(*klass->load_values) (vec);

		g_return_if_fail (vec->base.flags & GO_DATA_CACHE_IS_VALID);
	}

	if (min != NULL)
		*min = vec->minimum;
	if (max != NULL)
		*max = vec->maximum;
}

/*************************************************************************/

#define GO_DATA_MATRIX_CLASS(k)		(G_TYPE_CHECK_CLASS_CAST ((k), GO_DATA_MATRIX_TYPE, GODataMatrixClass))
#define IS_GO_DATA_MATRIX_CLASS(k)	(G_TYPE_CHECK_CLASS_TYPE ((k), GO_DATA_MATRIX_TYPE))
#define GO_DATA_MATRIX_GET_CLASS(o)	(G_TYPE_INSTANCE_GET_CLASS ((o), GO_DATA_MATRIX_TYPE, GODataMatrixClass))

static void
go_data_matrix_emit_changed (GOData *data)
{
	data->flags &= ~(GO_DATA_CACHE_IS_VALID | GO_DATA_MATRIX_SIZE_CACHED);
}

static void
go_data_matrix_class_init (GODataClass *klass)
{
	klass->emit_changed = go_data_matrix_emit_changed;
}

GSF_CLASS_ABSTRACT (GODataMatrix, go_data_matrix,
		    go_data_matrix_class_init, NULL,
		    GO_DATA_TYPE)

GODataMatrixSize
go_data_matrix_get_size (GODataMatrix *mat)
{
	if (! (mat->base.flags & GO_DATA_MATRIX_SIZE_CACHED)) {
		GODataMatrixClass const *klass = GO_DATA_MATRIX_GET_CLASS (mat);
		static GODataMatrixSize null_size = {0, 0};

		g_return_val_if_fail (klass != NULL, null_size);

		(*klass->load_size) (mat);

		g_return_val_if_fail (mat->base.flags & GO_DATA_MATRIX_SIZE_CACHED, null_size);
	}

	return mat->size;
}

double *
go_data_matrix_get_values (GODataMatrix *mat)
{
	if (! (mat->base.flags & GO_DATA_CACHE_IS_VALID)) {
		GODataMatrixClass const *klass = GO_DATA_MATRIX_GET_CLASS (mat);

		g_return_val_if_fail (klass != NULL, NULL);

		(*klass->load_values) (mat);

		g_return_val_if_fail (mat->base.flags & GO_DATA_CACHE_IS_VALID, NULL);
	}

	return mat->values;
}

double
go_data_matrix_get_value (GODataMatrix *mat, unsigned i, unsigned j)
{
	g_return_val_if_fail (((int)i < mat->size.rows) && ((int)j < mat->size.columns), go_nan);
	if (! (mat->base.flags & GO_DATA_CACHE_IS_VALID)) {
		GODataMatrixClass const *klass = GO_DATA_MATRIX_GET_CLASS (mat);
		g_return_val_if_fail (klass != NULL, go_nan);
		return (*klass->get_value) (mat, i, j);
	}

	return mat->values[i * mat->size.columns + j];
}

char *
go_data_matrix_get_str (GODataMatrix *mat, unsigned i, unsigned j)
{
	GODataMatrixClass const *klass = GO_DATA_MATRIX_GET_CLASS (mat);
	char *res;

	g_return_val_if_fail (klass != NULL, NULL);
	g_return_val_if_fail (((int)i < mat->size.rows) && ((int)j < mat->size.columns), g_strdup (""));

	res = (*klass->get_str) (mat, i, j);
	if (res == NULL)
		return g_strdup ("");
	return res;
}

void
go_data_matrix_get_minmax (GODataMatrix *mat, double *min, double *max)
{
	if (! (mat->base.flags & GO_DATA_CACHE_IS_VALID)) {
		GODataMatrixClass const *klass = GO_DATA_MATRIX_GET_CLASS (mat);

		g_return_if_fail (klass != NULL);

		(*klass->load_values) (mat);

		g_return_if_fail (mat->base.flags & GO_DATA_CACHE_IS_VALID);
	}

	if (min != NULL)
		*min = mat->minimum;
	if (max != NULL)
		*max = mat->maximum;
}
