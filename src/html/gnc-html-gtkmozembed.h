/********************************************************************
 * gnc-html-gtkmozembed.h -- display html with gnc special tags     *
 * Copyright (C) 2009 Phil Longstaff <plongstaff@rogers.com>        *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#ifndef GNC_HTML_GTKMOZEMBED_H
#define GNC_HTML_GTKMOZEMBED_H

#include <glib-object.h>
#include "gnc-html.h"

G_BEGIN_DECLS

#define GNC_TYPE_HTML_GTKMOZEMBED       (gnc_html_gtkmozembed_get_type())
#define GNC_HTML_GTKMOZEMBED(o)         (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_HTML_GTKMOZEMBED, GncHtmlGtkmozembed))
#define GNC_HTML_GTKMOZEMBED_CLASS(k)   (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_HTML_GTKMOZEMBED, GncHtmlGtkmozembedClass))
#define GNC_IS_HTML_GTKMOZEMBED(o)      (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_HTML_GTKMOZEMBED))
#define GNC_IS_HTML_GTKMOZEMBED_CLASS(k)   (G_TYPE_CHECK_CLASS_TYPE((k), GNC_TYPE_HTML_GTKMOZEMBED))
#define GNC_HTML_GTKMOZEMBED_GET_CLASS(o)  (G_TYPE_INSTANCE_GET_CLASS((o), GNC_TYPE_HTML_GTKMOZEMBED, GncHtmlGtkmozembedClass))

typedef struct _GncHtmlGtkmozembed GncHtmlGtkmozembed;
typedef struct _GncHtmlGtkmozembedClass GncHtmlGtkmozembedClass;
typedef struct _GncHtmlGtkmozembedPrivate GncHtmlGtkmozembedPrivate;

struct _GncHtmlGtkmozembed {
	GncHtml parent_instance;

	/*< private >*/
	GncHtmlGtkmozembedPrivate* priv;
};

struct _GncHtmlGtkmozembedClass {
	GncHtmlClass parent_class;
};

GType gnc_html_gtkmozembed_get_type( void );

GncHtml* gnc_html_gtkmozembed_new( void );

G_END_DECLS

#endif
