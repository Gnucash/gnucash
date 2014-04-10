/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-font.h : 
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
#ifndef GO_FONT_H
#define GO_FONT_H

#include <glib.h>
#include <goffice/utils/goffice-utils.h>
#include <pango/pango-font.h>
#include <pango/pangofc-fontmap.h>

G_BEGIN_DECLS

struct _GOFont {
	PangoFontDescription *desc;
	int	 ref_count;
	int	 font_index; /* each renderer keeps an array for lookup */
};

GOFont const *go_font_new_by_desc  (PangoFontDescription *desc);
GOFont const *go_font_new_by_name  (char const *str);
GOFont const *go_font_new_by_index (unsigned i);
char   	     *go_font_as_str       (GOFont const *font);
GOFont const *go_font_ref	   (GOFont const *font);
void	      go_font_unref	   (GOFont const *font);
gboolean      go_font_eq	   (GOFont const *a, GOFont const *b);

/* cache notification */
void go_font_cache_register   (GClosure *callback);
void go_font_cache_unregister (GClosure *callback);

/* private */
void go_font_init     (void);
void go_font_shutdown (void);

/* See http://bugzilla.gnome.org/show_bug.cgi?id=143542 */
void go_pango_fc_font_map_cache_clear (PangoFcFontMap *font_map);

G_END_DECLS

#endif /* GO_FONT_H */
