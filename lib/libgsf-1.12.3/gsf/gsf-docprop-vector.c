/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf-docprop-vector.c: A type implementing OLE Document Property vectors using a GValueArray
 *
 * Copyright (C) 2004-2005 Frank Chiulli (fc-linux@cox.net)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2.1 of the GNU Lesser General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA
 */

#include <gsf-config.h>
#include <gsf/gsf-docprop-vector.h>
#include <gsf/gsf-impl-utils.h>
#include <stdio.h>

struct _GsfDocPropVector {
	GObject      parent;

	GValueArray *gva;
};
typedef GObjectClass  GsfDocPropVectorClass;

#define GSF_DOCPROP_VECTOR_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), GSF_DOCPROP_VECTOR_TYPE, GsfDocPropVectorClass))
#define GSF_DOCPROP_VECTOR_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), GSF_DOCPROP_VECTOR_TYPE, GsfDocPropVectorClass))
#define IS_GSF_DOCPROP_VECTOR_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), GSF_DOCPROP_VECTOR_TYPE))

static GObjectClass *parent_class;

GValueArray *
gsf_value_get_docprop_varray (GValue const *value)
{
	GsfDocPropVector *v = gsf_value_get_docprop_vector (value);
	return v ? v->gva : NULL;
}

/**
 * gsf_docprop_value_get_vector
 * @value: A GValue of type #GsfDocPropVector.
 *
 * This function returns a pointer to the GsfDocPropVector structure in @value.
 * No additional references are created.
 *
 * Returns: A pointer to the #GsfDocPropVector structure in @value
 **/
GsfDocPropVector *
gsf_value_get_docprop_vector (GValue const *value)
{
	g_return_val_if_fail (VAL_IS_GSF_DOCPROP_VECTOR (value), NULL);
	
	return (GsfDocPropVector *) g_value_get_object (value);
}

/**
 * gsf_docprop_vector_append
 * @vector: The vector to which the GValue will be added
 * @value:  The GValue to add to @vector
 *
 * Insert a copy of @value as the last element of @vector.
 **/
void
gsf_docprop_vector_append (GsfDocPropVector *vector, GValue *value)
{
	g_return_if_fail (vector != NULL);
	g_return_if_fail (value != NULL);

	if (G_IS_VALUE (value))
		vector->gva = g_value_array_append (vector->gva, value);
}

/**
 * gsf_docprop_vector_as_string
 * @vector: The #GsfDocPropVector from which GValues will be extracted.
 *
 * This function returns a string which represents all the GValues in @vector.
 * The caller is responsible for freeing the result.
 *
 * Returns: a string of comma-separated values
 **/
gchar*
gsf_docprop_vector_as_string (GsfDocPropVector *vector)
{
	gchar		*rstring;

	guint		 i;
	guint		 num_values;

	g_return_val_if_fail (vector != NULL, NULL);
	g_return_val_if_fail (vector->gva != NULL, NULL);

	rstring    = g_new0 (gchar, 1);
	num_values = vector->gva->n_values;
	
	for (i = 0; i < num_values; i++) {
		char    *str;
		GValue	*v;
		
		v = g_value_array_get_nth (vector->gva, i);
		str = g_strdup_value_contents (v);
		rstring = g_strconcat (rstring, str, ",", NULL);
		g_free (str);
		g_value_unset (v);
	}

	return rstring;
}

static void
gsf_docprop_vector_finalize (GObject *obj)
{
	GsfDocPropVector *vector = (GsfDocPropVector *) obj;
	if (vector->gva != NULL) {
		g_value_array_free (vector->gva);
		vector->gva = NULL;
	}
	parent_class->finalize (obj);
}

static void
gsf_docprop_vector_class_init (GObjectClass *gobject_class)
{
	parent_class = g_type_class_peek (G_TYPE_OBJECT);
	gobject_class->finalize = gsf_docprop_vector_finalize;
}

static void
gsf_docprop_vector_init (GsfDocPropVector *vector)
{
	vector->gva = g_value_array_new (0);
}

GSF_CLASS (GsfDocPropVector, gsf_docprop_vector,
	   gsf_docprop_vector_class_init, gsf_docprop_vector_init,
	   G_TYPE_OBJECT)

/**
 * gsf_docprop_vector_new
 *
 * This function creates a new gsf_docprop_vector object.
 *
 * Returns: GsfDocPropVector*
 **/
GsfDocPropVector*
gsf_docprop_vector_new (void)
{
	return g_object_new (GSF_DOCPROP_VECTOR_TYPE, NULL);
}

