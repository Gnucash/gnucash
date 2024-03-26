/*******************************************************************\
 * html-widget.hpp -- litehtml widget                               *
 *                                                                  *
 * Copyright (C) 2024 Robert Fewell                                 *
 *                                                                  *
 * This is a modified version of source file from litehtml example  *
 * browser.                                                         *
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
/** @file html-widget.hpp
    @brief Litehtml Widget
    @author Copyright (c) 2024 Robert Fewell
*/
#ifndef LH_HTML_WIDGET_HPP
#define LH_HTML_WIDGET_HPP

#include <config.h>
#include <gtk/gtk.h>
#include <cairo.h>

#include "container-linux.hpp"

class html_widget :     public container_linux
{
    litehtml::string            m_url;
    litehtml::string            m_base_url;
//    litehtml::document::ptr     m_html;
//    int                         m_rendered_width;
    litehtml::string            m_cursor;
//    litehtml::string            m_clicked_url;

public:
    html_widget();
    virtual ~html_widget();

    GtkWidget * get_drawing_area() {return m_drawing_area;}
    const char * get_anchor(GdkEventButton *event);

    litehtml::document::ptr     m_html;
    litehtml::string            m_clicked_url;
    int                         m_rendered_width;

    void open_page(const litehtml::string& url);
    void show_hash(const litehtml::string& hash);
    void dump(const litehtml::string& file_name);
    void update_cursor();
    void on_parent_size_allocate(GtkAllocation* allocation);

protected:
    void scroll_to(int x, int y);

    void get_client_rect(litehtml::position& client) const override;
    void on_anchor_click(const char* url, const litehtml::element::ptr& el) override;
    void set_cursor(const char* cursor) override;
    void import_css(litehtml::string& text, const litehtml::string& url, litehtml::string& baseurl) override;
    void set_caption(const char* caption) override;
    void set_base_url(const char* base_url) override;
    GdkPixbuf* get_image(const char* url, bool redraw_on_ready) override;
    void make_url(const char* url, const char* basepath, litehtml::string& out) override;

    GtkWidget * m_drawing_area;
    gulong      m_size_alloc;

private:
    void load_text_file(const litehtml::string& url, litehtml::string& out);
    GtkAllocation get_parent_allocation();
};

#endif
