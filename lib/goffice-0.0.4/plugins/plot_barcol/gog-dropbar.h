/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-dropbar.h
 *
 * Copyright (C) 2005
 *	Jean Brefort (jean.brefort@normalesup.org)
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

#ifndef GOG_DROPBAR_H
#define GOG_DROPBAR_H

#include "gog-barcol.h"

G_BEGIN_DECLS


typedef GogBarColPlot GogDropBarPlot;
typedef GogPlot1_5dClass GogDropBarPlotClass;

#define GOG_DROPBAR_PLOT_TYPE	(gog_dropbar_plot_get_type ())
#define GOG_DROPBAR_PLOT(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_DROPBAR_PLOT_TYPE, GogDropBarPlot))
#define GOG_IS_PLOT_DROPBAR(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_DROPBAR_PLOT_TYPE))

GType gog_dropbar_plot_get_type (void);
void  gog_dropbar_plot_register_type (GTypeModule *module);
void  gog_dropbar_view_register_type (GTypeModule *module);

G_END_DECLS

#endif	/* GOG_DROPBAR_H */
