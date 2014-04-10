/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * gog-guru.h : 
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
#ifndef GOG_GURU_H
#define GOG_GURU_H

#include <goffice/graph/goffice-graph.h>
#include <goffice/app/goffice-app.h>
#include <gtk/gtkwindow.h>

G_BEGIN_DECLS

/* This interface _will_ change */
/* The callback in the closure should match the following prototype: */
/* typedef void (*GogGuruRegister) (GogGraph *graph, gpointer user); */

GtkWidget *gog_guru (GogGraph *graph, GogDataAllocator *dalloc,
		     GOCmdContext *cc, GtkWindow *toplevel,
		     GClosure *closure);

G_END_DECLS

#endif /* GOG_GURU_H */
