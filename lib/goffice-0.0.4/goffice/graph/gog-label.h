/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-label.h : 
 *
 * Copyright (C) 2003-2004 Jody Goldberg (jody@gnome.org)
 * Copyright (C) 2005      Jean Brefort (jean.brefort@normalesup.org)
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
#ifndef GOG_LABEL_H
#define GOG_LABEL_H

#include <goffice/graph/goffice-graph.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define GOG_LABEL_TYPE	(gog_label_get_type ())
#define GOG_LABEL(o)	(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_LABEL_TYPE, GogLabel))
#define IS_GOG_LABEL(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_LABEL_TYPE))

GType gog_label_get_type (void);

#define GOG_REG_EQN_TYPE		(gog_reg_eqn_get_type ())
#define GOG_REG_EQN(o)		(G_TYPE_CHECK_INSTANCE_CAST ((o), GOG_REG_EQN_TYPE, GogRegEqn))
#define IS_GOG_REG_EQN(o)	(G_TYPE_CHECK_INSTANCE_TYPE ((o), GOG_REG_EQN_TYPE))

GType gog_reg_eqn_get_type (void);

G_END_DECLS

#endif /* GOG_LABEL_H */
