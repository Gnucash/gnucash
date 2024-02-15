/*******************************************************************\
 * html-widget-wrapped.h -- litehtml widget                         *
 *                                                                  *
 * Copyright (C) 2024 Robert Fewell                                 *
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
/** @file html-widget-wrapped.h
    @brief Litehtml Widget Wrapped
    @author Copyright (c) 2024 Robert Fewell
*/
#ifndef LH_HTML_WIDGET_WRAPPED_HPP
#define LH_HTML_WIDGET_WRAPPED_HPP

#include <config.h>

#ifdef __cplusplus
extern "C" {
#endif

#include "gnc-html.h"

typedef struct html_widget html_widget_wrapped;

/** Create a new html_widget for displaying reports
 *
 *  @return a pointer to the html_widget
 */
html_widget_wrapped * gnc_html_litehtml_widget_new(void);

/** Return the anchor string
 *
 *  @param html_widget_ptr pointer to html_widget
 *  @param event the event button structure
 *
 *  @return the anchor string or NULL
 */
const gchar * gnc_html_litehtml_get_anchor(html_widget_wrapped *html_widget_ptr,
                                           GdkEventButton *event);

/** Return the drawing area used
 *
 *  @param html_widget_ptr pointer to html_widget
 *
 *  @return the drawing area used for displaying report
 */
GtkWidget * gnc_html_litehtml_get_drawing_area(html_widget_wrapped *html_widget_ptr);

/** Open report file
 *
 *  @param html_widget_ptr pointer to html_widget
 *  @param report filename to be displayed
 */
void gnc_html_litehtml_load_file(html_widget_wrapped *html_widget_ptr,
                                 const gchar *filename);

/** Destroy the html_widget
 *
 *  @param html_widget_ptr pointer to html_widget
 */
void gnc_html_litehtml_delete(html_widget_wrapped *html_widget_ptr);

#ifdef __cplusplus
}
#endif

#endif
