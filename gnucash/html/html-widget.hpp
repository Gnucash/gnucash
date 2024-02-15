
#ifndef LH_HTML_WIDGET_HPP
#define LH_HTML_WIDGET_HPP

#include <config.h>
#include <gtk/gtk.h>
#include <cairo.h>

#include "container-linux.hpp"

class html_widget :     public GtkDrawingArea,
                        public container_linux
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
