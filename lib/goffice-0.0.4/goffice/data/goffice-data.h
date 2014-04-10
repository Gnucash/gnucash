/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * goffice-data.h: 
 *
 * Copyright (C) 2005 Jody Goldberg (jody@gnome.org)
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

#ifndef GOFFICE_DATA_H
#define GOFFICE_DATA_H

#include <glib.h>

G_BEGIN_DECLS

/* Data */
typedef struct _GOData		 GOData;
typedef struct _GODataScalar	 GODataScalar;
typedef struct _GODataVector	 GODataVector;
typedef struct _GODataMatrix	 GODataMatrix;
typedef struct {
	int rows;	/* negative if dirty, includes missing values */
	int columns;	/* negative if dirty, includes missing values */
} GODataMatrixSize;

G_END_DECLS

#endif /* GOFFICE_DATA_H */
