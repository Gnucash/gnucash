/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * goffice-app.h: 
 *
 * Copyright (C) 2004 Jody Goldberg (jody@gnome.org)
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

#ifndef GOFFICE_APP_H
#define GOFFICE_APP_H

#include <glib/gmacros.h>

G_BEGIN_DECLS

typedef struct _GOApp		GOApp;
typedef struct _GODoc		GODoc;
typedef struct _GODocControl	GODocControl;
typedef struct _GOCmdContext	GOCmdContext;

typedef struct _GOPlugin	GOPlugin;
typedef struct _GOPluginService	GOPluginService;
typedef struct _GOPluginLoader	GOPluginLoader;

/* temporary */
typedef struct _ErrorInfo		ErrorInfo;
typedef struct _GOFileSaver 		GOFileSaver;
typedef struct _GOFileOpener		GOFileOpener;
typedef struct _IOContext		IOContext;

/*
 * File probe level tells file opener (its probe method to be exact), how
 * hard it should try to recognize the type of the file. File openers may
 * ignore this or support only some probe levels, but if specifies
 * "reccomened" behaviour.
 * Before opening any file we detect its type by calling probe for
 * every registered file opener (in order of priority) and passing
 * FILE_PROBE_FILE_NAME as probe level. If none of them recogizes the file,
 * we increase probe level and try again...
 */
typedef enum {
	FILE_PROBE_FILE_NAME,	/* Test only file name, don't read file contents */
	FILE_PROBE_CONTENT,	/* Read the whole file if it's necessary */
	FILE_PROBE_LAST
} FileProbeLevel;

G_END_DECLS

#endif /* GOFFICE_GRAPH_H */
