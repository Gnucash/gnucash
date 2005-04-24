/* vim: set sw=8: -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*
 * go-format.h : 
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
#ifndef GO_FORMAT_H
#define GO_FORMAT_H

#include <goffice/utils/goffice-utils.h>
#include <glib-object.h>

G_BEGIN_DECLS

GOFormat   *go_format_new_from_XL	 (char const *descriptor_string, gboolean delocalize);
char   	   *go_format_as_XL		 (GOFormat const *fmt, gboolean localized);
GOFormat   *go_format_ref		 (GOFormat *fmt);
void        go_format_unref		 (GOFormat *fmt);
char	   *go_format_value   		 (GOFormat const *fmt, double val);
gboolean    go_format_eq		 (GOFormat const *a, GOFormat const *b);
GOFormat   *go_format_general		 (void);
GOFormat   *go_format_default_date	 (void);
GOFormat   *go_format_default_time	 (void);
GOFormat   *go_format_default_percentage (void);
GOFormat   *go_format_default_money	 (void);

G_END_DECLS

#endif /* GO_FORMAT_H */
