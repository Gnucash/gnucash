/********************************************************************
 * gnc-html-litehtml-p.h -- display html with gnc special tags      *
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

#ifndef GNC_HTML_LITEHTML_P_H
#define GNC_HTML_LITEHTML_P_H

#include "gnc-html-p.h"
#include "html-widget-wrapped.h"

struct _GncHtmlLitehtmlPrivate
{
    struct _GncHtmlPrivate base;

    GtkWidget           *web_view;
    html_widget_wrapped *html_wrapped_widget;
    gchar               *html_string;         /* html string being displayed */

};

#endif
