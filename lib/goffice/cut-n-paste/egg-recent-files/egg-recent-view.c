/* File import from libegg to gnumeric by import-egg.  Do not edit.  */

#include <goffice/goffice-config.h>
/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/**
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 *
 * Authors:
 *   James Willcox <jwillcox@cs.indiana.edu>
 */


#ifdef HAVE_CONFIG_H
/* #include <config.h> */
#endif

#include <string.h>
#include <gtk/gtk.h>
#include "egg-recent-view.h"


GtkType
egg_recent_view_get_type (void)
{
	static GtkType view_type = 0;

	if (!view_type)
	{
		static const GTypeInfo view_info =
		{
			sizeof (EggRecentViewClass),  /* class_size */
			NULL,			    /* base_init */
			NULL,			    /* base_finalize */
		};

		view_type = g_type_register_static (G_TYPE_INTERFACE,
						    "EggRecentView",
						    &view_info, 0);
	}

	return view_type;
}

EggRecentModel *
egg_recent_view_get_model (EggRecentView *view)
{
	g_return_val_if_fail (view, NULL);

	return EGG_RECENT_VIEW_GET_CLASS (view)->do_get_model (view);
}

void
egg_recent_view_set_model (EggRecentView *view, EggRecentModel *model)
{
	g_return_if_fail (view);
	g_return_if_fail (model);

	EGG_RECENT_VIEW_GET_CLASS (view)->do_set_model (view, model);
}
