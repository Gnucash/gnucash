/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gsf.h: 
 *
 * Copyright (C) 2002-2004 Jody Goldberg (jody@gnome.org)
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

#ifndef GSF_H
#define GSF_H

#include <glib.h>

G_BEGIN_DECLS

typedef struct _GsfInput	GsfInput;
typedef struct _GsfInfile 	GsfInfile;

typedef struct _GsfOutput	GsfOutput;
typedef struct _GsfOutfile 	GsfOutfile;

typedef struct _GsfDocProp	GsfDocProp;
typedef struct _GsfDocMetaData	GsfDocMetaData;
typedef struct _GsfTimestamp	GsfTimestamp;

/* FIXME:
 * gsf_off_t is really supposed to be the widest type off_t can be configured
 * to on the platform
 */ 
typedef gint64 gsf_off_t;
#define GSF_OFF_T_FORMAT	G_GINT64_FORMAT

G_END_DECLS

#endif /* GSF_H */
