/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-data.h : 
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
#ifndef GO_DATA_H
#define GO_DATA_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/utils/goffice-utils.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GO_DATA_TYPE	(go_data_get_type ())
#define GO_DATA(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_TYPE, GOData))
#define IS_GO_DATA(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_TYPE))

GType go_data_get_type (void);
gboolean  go_data_needs_recalc	(GOData const *dat);
GOData	 *go_data_dup		(GOData const *src);
gboolean  go_data_eq		(GOData const *a, GOData const *b);
GOFormat *go_data_preferred_fmt (GOData const *dat);
char     *go_data_as_str	(GOData const *dat);
gboolean  go_data_from_str	(GOData *dat, char const *str);
void	  go_data_emit_changed  (GOData *dat);

/*************************************************************************/

#define GO_DATA_SCALAR_TYPE	(go_data_scalar_get_type ())
#define GO_DATA_SCALAR(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_SCALAR_TYPE, GODataScalar))
#define IS_GO_DATA_SCALAR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_SCALAR_TYPE))

GType go_data_scalar_get_type (void);

double      go_data_scalar_get_value  (GODataScalar *val);
char const *go_data_scalar_get_str    (GODataScalar *val);

/*************************************************************************/

#define GO_DATA_VECTOR_TYPE	(go_data_vector_get_type ())
#define GO_DATA_VECTOR(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_VECTOR_TYPE, GODataVector))
#define IS_GO_DATA_VECTOR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_VECTOR_TYPE))

GType go_data_vector_get_type (void);

int	 go_data_vector_get_len    (GODataVector *vec);
double	*go_data_vector_get_values (GODataVector *vec);
double	 go_data_vector_get_value  (GODataVector *vec, unsigned i);
char	*go_data_vector_get_str    (GODataVector *vec, unsigned i);
void	 go_data_vector_get_minmax (GODataVector *vec, double *min, double *max);

/*************************************************************************/

#define GO_DATA_MATRIX_TYPE	(go_data_matrix_get_type ())
#define GO_DATA_MATRIX(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GO_DATA_MATRIX_TYPE, GODataMatrix))
#define IS_GO_DATA_MATRIX(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GO_DATA_MATRIX_TYPE))

GType go_data_matrix_get_type (void);

GOMatrixSize	 go_data_matrix_get_size    (GODataMatrix *mat);
double	*go_data_matrix_get_values (GODataMatrix *mat);
double	 go_data_matrix_get_value  (GODataMatrix *mat, unsigned i, unsigned j);
char	*go_data_matrix_get_str    (GODataMatrix *mat, unsigned i, unsigned j);
void	 go_data_matrix_get_minmax (GODataMatrix *mat, double *min, double *max);

G_END_DECLS

#endif /* GO_DATA_H */
