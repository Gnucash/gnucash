/********************************************************************
 * gnc-html-litehtml.h -- display html with gnc special tags        *
 * Copyright (C) 2024 Bob Fewell                                    *
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

#ifndef GNC_HTML_LITEHTML_H
#define GNC_HTML_LITEHTML_H

#include <glib-object.h>
#include "gnc-html.h"

G_BEGIN_DECLS

#define GNC_TYPE_HTML_LITEHTML       (gnc_html_litehtml_get_type())
#define GNC_HTML_LITEHTML(o)         (G_TYPE_CHECK_INSTANCE_CAST ((o), GNC_TYPE_HTML_LITEHTML, GncHtmlLitehtml))
#define GNC_HTML_LITEHTML_CLASS(k)   (G_TYPE_CHECK_CLASS_CAST((k), GNC_TYPE_HTML_LITEHTML, GncHtmlLitehtmlClass))
#define GNC_IS_HTML_LITEHTML(o)      (G_TYPE_CHECK_INSTANCE_TYPE((o), GNC_TYPE_HTML_LITEHTML))
#define GNC_IS_HTML_LITEHTML_CLASS(k)   (G_TYPE_CHECK_CLASS_TYPE((k), GNC_TYPE_HTML_LITEHTML))
#define GNC_HTML_LITEHTML_GET_CLASS(o)  (G_TYPE_INSTANCE_GET_CLASS((o), GNC_TYPE_HTML_LITEHTML, GncHtmlLitehtmlClass))

typedef struct _GncHtmlLitehtml GncHtmlLitehtml;
typedef struct _GncHtmlLitehtmlClass GncHtmlLitehtmlClass;
typedef struct _GncHtmlLitehtmlPrivate GncHtmlLitehtmlPrivate;

/** Key for saving the PDF-export directory in the print settings */
#define GNC_GTK_PRINT_SETTINGS_EXPORT_DIR "gnc-pdf-export-directory"

struct _GncHtmlLitehtml
{
    GncHtml parent_instance;

    /*< private >*/
    GncHtmlLitehtmlPrivate* priv;
};

struct _GncHtmlLitehtmlClass
{
    GncHtmlClass parent_class;
};

GType gnc_html_litehtml_get_type (void);

GncHtml* gnc_html_litehtml_new (void);

G_END_DECLS

#endif
