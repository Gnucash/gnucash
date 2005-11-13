/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-data-allocator.c :
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
#include <goffice/graph/gog-data-allocator.h>
#include <goffice/graph/gog-object.h>
#include <goffice/graph/gog-graph.h>
#include <goffice/data/go-data.h>

GType
gog_data_allocator_get_type (void)
{
	static GType gog_data_allocator_type = 0;

	if (!gog_data_allocator_type) {
		static GTypeInfo const gog_data_allocator_info = {
			sizeof (GogDataAllocatorClass),	/* class_size */
			NULL,		/* base_init */
			NULL,		/* base_finalize */
		};

		gog_data_allocator_type = g_type_register_static (G_TYPE_INTERFACE,
			"GogDataAllocator", &gog_data_allocator_info, 0);
	}

	return gog_data_allocator_type;
}

void
gog_data_allocator_allocate (GogDataAllocator *dalloc, GogPlot *plot)
{
	g_return_if_fail (IS_GOG_DATA_ALLOCATOR (dalloc));
	GOG_DATA_ALLOCATOR_GET_CLASS (dalloc)->allocate (dalloc, plot);
}

gpointer
gog_data_allocator_editor (GogDataAllocator *dalloc, GogDataset *set,
			   int dim_i, GogDataType data_type)
{
	g_return_val_if_fail (IS_GOG_DATA_ALLOCATOR (dalloc), NULL);
	return GOG_DATA_ALLOCATOR_GET_CLASS (dalloc)->editor (dalloc, set,
							      dim_i, data_type);
}
