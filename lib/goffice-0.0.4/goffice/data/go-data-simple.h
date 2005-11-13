/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-data-simple.h : 
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
#ifndef GO_DATA_SIMPLE_H
#define GO_DATA_SIMPLE_H

#include <goffice/data/goffice-data.h>
#include <goffice/data/go-data.h>

G_BEGIN_DECLS

#define GO_DATA_SCALAR_VAL_TYPE  (go_data_scalar_val_get_type ())
#define GO_DATA_SCALAR_VAL(o)	 (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_SCALAR_VAL_TYPE, GODataScalarVal))
#define IS_GO_DATA_SCALAR_VAL(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_SCALAR_VAL_TYPE))

typedef struct _GODataScalarVal GODataScalarVal;
GType	 go_data_scalar_val_get_type (void);
GOData	*go_data_scalar_val_new      (double val);

#define GO_DATA_SCALAR_STR_TYPE  (go_data_scalar_str_get_type ())
#define GO_DATA_SCALAR_STR(o)	 (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_SCALAR_STR_TYPE, GODataScalarStr))
#define IS_GO_DATA_SCALAR_STR(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_SCALAR_STR_TYPE))

typedef struct _GODataScalarStr GODataScalarStr;
GType	 go_data_scalar_str_get_type (void);
GOData	*go_data_scalar_str_new      (char const *text, gboolean needs_free);
void     go_data_scalar_str_set_str  (GODataScalarStr *str,
				      char const *text, gboolean needs_free);

#define GO_DATA_VECTOR_VAL_TYPE  (go_data_vector_val_get_type ())
#define GO_DATA_VECTOR_VAL(o)	 (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_VECTOR_VAL_TYPE, GODataVectorVal))
#define IS_GO_DATA_VECTOR_VAL(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_VECTOR_VAL_TYPE))

typedef struct _GODataVectorVal GODataVectorVal;
GType	 go_data_vector_val_get_type (void);
GOData	*go_data_vector_val_new      (double *val, unsigned n, GDestroyNotify   notify);

#define GO_DATA_VECTOR_STR_TYPE  (go_data_vector_str_get_type ())
#define GO_DATA_VECTOR_STR(o)	 (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_VECTOR_STR_TYPE, GODataVectorStr))
#define IS_GO_DATA_VECTOR_STR(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_VECTOR_STR_TYPE))

typedef struct _GODataVectorStr GODataVectorStr;
GType	go_data_vector_str_get_type	      (void);
GOData *go_data_vector_str_new		      (char const * const *str, unsigned n, GDestroyNotify notify);
void    go_data_vector_str_set_translate_func (GODataVectorStr *vector,
					       GOTranslateFunc  func,
					       gpointer         data,
					       GDestroyNotify   notify);
void go_data_vector_str_set_translation_domain (GODataVectorStr *vec,
						char const      *domain);

#define GO_DATA_MATRIX_VAL_TYPE  (go_data_matrix_val_get_type ())
#define GO_DATA_MATRIX_VAL(o)	 (G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_MATRIX_VAL_TYPE, GODataMatrixVal))
#define IS_GO_DATA_MATRIX_VAL(o) (G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_MATRIX_VAL_TYPE))

typedef struct _GODataMatrixVal GODataMatrixVal;
GType	 go_data_matrix_val_get_type (void);
GOData	*go_data_matrix_val_new      (double *val, unsigned rows, unsigned columns, GDestroyNotify   notify);

G_END_DECLS

#endif /* GO_DATA_SIMPLE_H */
