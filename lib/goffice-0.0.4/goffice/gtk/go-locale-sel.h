/*
 *  Copyright (C) 2003 Andreas J. Guelzow
 *
 *  based on code by:
 *  Copyright (C) 2000 Marco Pesenti Gritti
 *  from the galeon code base
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef _GO_LOCALE_SEL_H_
#define _GO_LOCALE_SEL_H_

#include <gtk/gtkwidget.h>

G_BEGIN_DECLS

#define GO_LOCALE_SEL_TYPE        (go_locale_sel_get_type ())
#define GO_LOCALE_SEL(obj)        (G_TYPE_CHECK_INSTANCE_CAST((obj), GO_LOCALE_SEL_TYPE, GOLocaleSel))
#define IS_GO_LOCALE_SEL(obj)     (G_TYPE_CHECK_INSTANCE_TYPE((obj), GO_LOCALE_SEL_TYPE))

typedef struct _GOLocaleSel GOLocaleSel;

GType        go_locale_sel_get_type (void);
GtkWidget *  go_locale_sel_new (void);

gchar       *go_locale_sel_get_locale (GOLocaleSel *cs);
gboolean     go_locale_sel_set_locale (GOLocaleSel *cs, const char *loc);

void         go_locale_sel_set_sensitive (GOLocaleSel *cs, gboolean sensitive);

const char  *go_locale_sel_get_locale_name (GOLocaleSel *cs, const char *loc);

G_END_DECLS

#endif /* _GO_LOCALE_SEL_H_ */
