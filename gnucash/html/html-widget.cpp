#include <config.h>

#include "html-widget.hpp"

#include <litehtml/url_path.h>
#include <litehtml/url.h>

#include <fstream>
#include <iostream>
#include <ostream>
#include <stack>
#include <string>

#include <gtk/gtk.h>
#include <cairo.h>


#define BUFF_SIZE    10 * 1024

class html_dumper : public litehtml::dumper
{
    std::ofstream m_cout;
    int indent;
    std::list<std::tuple<int, std::string>> m_node_text;
private:
    void print_indent(int size)
    {
        m_cout << litehtml::string(size, '\t');
    }

public:
    html_dumper(const litehtml::string& file_name) : m_cout(file_name), indent(0)
    {

    }

    void begin_node(const litehtml::string &descr) override
    {
        m_node_text.emplace_back(std::make_tuple(indent, "#" + descr));
        indent++;
    }

    void end_node() override
    {
        indent--;
    }

    void begin_attrs_group(const litehtml::string &descr) override
    {
    }

    void end_attrs_group() override
    {
    }

    void add_attr(const litehtml::string &name, const litehtml::string &value) override
    {
        if(name == "display" || name == "float")
        {
            std::get<1>(m_node_text.back()) += " " + name + "[" + value + "]";
        }
    }

    void print()
    {
        for(const auto& data : m_node_text)
        {
            print_indent(std::get<0>(data));
            m_cout << std::get<1>(data) << std::endl;
        }
    }
};

static gboolean
draw_callback (GtkWidget *widget, cairo_t *cr, gpointer user_data)
{
printf("%s called, %p\n",__FUNCTION__, widget);

    html_widget *hw = (html_widget*)user_data;

    litehtml::position pos;

    GdkRectangle rect;
    gdk_cairo_get_clip_rectangle(cr, &rect);

    pos.width   = rect.width;
    pos.height  = rect.height;
    pos.x       = rect.x;
    pos.y       = rect.y;

    GtkAllocation allocation;
    gtk_widget_get_allocation (widget, &allocation);

    cairo_move_to(cr, 0, 0);
    cairo_rel_line_to(cr, allocation.width, 0);
    cairo_rel_line_to(cr, 0, allocation.height);
    cairo_rel_line_to(cr, -allocation.width, 0);
    cairo_close_path(cr);

    cairo_set_source_rgb(cr, 1, 1, 1);
    cairo_fill(cr);

    if(hw->m_html)
    {
        hw->m_html->draw((litehtml::uint_ptr) cr, 0, 0, &pos);
    }

    return true;
}

static gboolean
button_press_event(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    html_widget *hw = (html_widget*)user_data;
printf("%s called, ###\n",__FUNCTION__);
    if(hw->m_html)
    {
        litehtml::position::vector redraw_boxes;
        if(hw->m_html->on_lbutton_down((int) event->x, (int) event->y, (int) event->x, (int) event->y, redraw_boxes))
        {
            for(auto& pos : redraw_boxes)
            {
                gtk_widget_queue_draw_area(hw->get_drawing_area(), pos.x, pos.y, pos.width, pos.height);
            }
        }
    }
    return true;
}

static gboolean
motion_notify_event(GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
    html_widget *hw = (html_widget*)user_data;
//printf("%s called\n",__FUNCTION__);
    if(hw->m_html)
    {
        litehtml::position::vector redraw_boxes;
        if(hw->m_html->on_mouse_over((int) event->x, (int) event->y, (int) event->x, (int) event->y, redraw_boxes))
        {
            for(auto& pos : redraw_boxes)
            {
                gtk_widget_queue_draw_area(hw->get_drawing_area(), pos.x, pos.y, pos.width, pos.height);
            }
        }
    }
    return true;
}

static void
size_allocate(GtkWidget *widget, GtkAllocation *allocation, gpointer user_data)
{
    html_widget *hw = (html_widget*)user_data;
printf("%s called\n",__FUNCTION__);

    if(hw->m_html && hw->m_rendered_width != allocation->width)
    {
        hw->m_rendered_width = allocation->width;
        hw->m_html->media_changed();
        hw->m_html->render(hw->m_rendered_width);
        gtk_widget_set_size_request(widget, hw->m_html->width(), hw->m_html->height());
        gtk_widget_queue_draw(widget);
    }
}

html_widget::html_widget()
{
printf("%s called\n",__FUNCTION__);
    m_rendered_width    = 0;
    m_html              = nullptr;
    m_size_alloc        = 0;

    m_drawing_area = gtk_drawing_area_new();

    g_signal_connect(G_OBJECT(m_drawing_area), "draw",
                     G_CALLBACK(draw_callback), (gpointer)this);

    gtk_widget_add_events(m_drawing_area, GDK_POINTER_MOTION_MASK |
                                          GDK_BUTTON_PRESS_MASK |
                                          GDK_BUTTON_RELEASE_MASK);

    g_signal_connect(G_OBJECT(m_drawing_area), "button-press-event",
                     G_CALLBACK(button_press_event), (gpointer)this);

    g_signal_connect(G_OBJECT(m_drawing_area), "motion-notify-event",
                     G_CALLBACK(motion_notify_event), (gpointer)this);
}

html_widget::~html_widget()
{
printf("%s called\n",__FUNCTION__);
}

const char * html_widget::get_anchor(GdkEventButton *event)
{
printf("%s called\n",__FUNCTION__);

    if(m_html)
    {
        litehtml::position::vector redraw_boxes;
        m_clicked_url.clear();
        if(m_html->on_lbutton_up((int) event->x, (int) event->y, (int) event->x, (int) event->y, redraw_boxes))
        {
            for(auto& pos : redraw_boxes)
            {
                gtk_widget_queue_draw_area(m_drawing_area, pos.x, pos.y, pos.width, pos.height);
            }
        }
        if(!m_clicked_url.empty())
        {
           return m_clicked_url.c_str();
        }
    }
    return nullptr;
}

void html_widget::get_client_rect(litehtml::position& client) const
{
printf("%s called\n",__FUNCTION__);
    GtkAllocation allocation;
    gtk_widget_get_allocation(m_drawing_area, &allocation);

    client.width = allocation.width;
    client.height = allocation.height;
    client.x = 0;
    client.y = 0;
}


void html_widget::on_anchor_click(const char* url, const litehtml::element::ptr& el)
{
printf("%s called\n",__FUNCTION__);
    if(url)
    {
        make_url(url, m_base_url.c_str(), m_clicked_url);
    }
}

void html_widget::set_cursor(const char* cursor)
{
//printf("%s called\n",__FUNCTION__);
    if(cursor)
    {
        if(m_cursor != cursor)
        {
            m_cursor = cursor;
            update_cursor();
        }
    }
}

void html_widget::import_css(litehtml::string& text, const litehtml::string& url, litehtml::string& baseurl)
{
printf("%s called\n",__FUNCTION__);
    std::string css_url;
    make_url(url.c_str(), baseurl.c_str(), css_url);
    load_text_file(css_url, text);
    if(!text.empty())
    {
        baseurl = css_url;
    }
}

void html_widget::set_caption(const char* caption)
{
printf("%s called\n",__FUNCTION__);
}

void html_widget::set_base_url(const char* base_url)
{
printf("%s called\n",__FUNCTION__);
    if(base_url)
    {
        m_base_url = litehtml::resolve(litehtml::url(m_url), litehtml::url(base_url)).str();
    } else
    {
        m_base_url = m_url;
    }
}

GdkPixbuf* html_widget::get_image(const char* url, bool redraw_on_ready)
{
printf("#### %s called, url '%s', '%s'\n",__FUNCTION__, url, m_base_url.c_str());
    GdkPixbuf *ptr = nullptr;
    std::string file_path(url);
//    file_path.erase (0, 8); //FIXME for linux "file:///"

//    GFile *file = g_file_new_for_path(file_path.c_str());

    GFile *file = g_file_new_for_uri(file_path.c_str());

    GInputStream *stream = (GInputStream *)g_file_read(file, nullptr, nullptr);

    if(stream)
    {
        ptr = gdk_pixbuf_new_from_stream(stream, nullptr, nullptr);
        g_object_unref(stream);
    }
    g_object_unref(file);
    return ptr;
}

GtkAllocation html_widget::get_parent_allocation()
{
printf("%s called\n",__FUNCTION__);
    GtkAllocation alloc;
    GtkWidget *parent = gtk_widget_get_parent(m_drawing_area);
    gtk_widget_get_allocation(parent, &alloc);
    return alloc;
}

void html_widget::open_page(const litehtml::string& url)
{
printf("%s called\n",__FUNCTION__);
    m_url       = url;
    m_base_url  = url;

    std::string html;
    load_text_file(url, html);

    if (m_size_alloc == 0)
        m_size_alloc = g_signal_connect(G_OBJECT(gtk_widget_get_parent(m_drawing_area)), "size-allocate",
                                        G_CALLBACK(size_allocate), (gpointer)this);

    m_html = litehtml::document::createFromString(html.c_str(), this);

    if(m_html)
    {
        GtkAllocation allocation;
        gtk_widget_get_allocation(m_drawing_area, &allocation);

        m_rendered_width = allocation.width;
        m_html->render(m_rendered_width);
        gtk_widget_set_size_request(m_drawing_area, m_html->width(), m_html->height());
    }
    gtk_widget_queue_draw(m_drawing_area);
}

void html_widget::scroll_to(int x, int y)
{
printf("%s called\n",__FUNCTION__);
//    auto vadj = m_browser->get_scrolled()->get_vadjustment();
//    auto hadj = m_browser->get_scrolled()->get_hadjustment();
//    vadj->set_value(vadj->get_lower() + y);
//    hadj->set_value(hadj->get_lower() + x);
}

void html_widget::make_url(const char* url, const char* basepath, litehtml::string& out)
{
printf("%s called, urk '%s', basepath '%s'\n",__FUNCTION__, url, basepath);
    if(!basepath || !basepath[0])
    {
        if(!m_base_url.empty())
        {
            out = litehtml::resolve(litehtml::url(m_base_url), litehtml::url(url)).str();
        } else
        {
            out = url;
        }
    } else
    {
        out = litehtml::resolve(litehtml::url(basepath), litehtml::url(url)).str();
    }
printf(" out is '%s'\n", out.c_str());
}

void html_widget::update_cursor()
{
printf("%s called\n",__FUNCTION__);
    GdkDisplay *display = gdk_display_get_default();
    GdkWindow *window = gtk_widget_get_window(m_drawing_area);
    GdkCursor *cursor;
    GdkCursorType cursType = GDK_ARROW;

    if(m_cursor == "pointer")
        cursType = GDK_HAND2;

    if(cursType == GDK_ARROW)
    {
        gdk_window_set_cursor(window, nullptr);

    } else
    {
        cursor = gdk_cursor_new_for_display(display, cursType);
        gdk_window_set_cursor(window, cursor);
    }
}

void html_widget::load_text_file(const litehtml::string& url, litehtml::string& out)
{
printf("%s called\n",__FUNCTION__);
    out.clear();

//    GFile *file = g_file_new_for_path(url.c_str());

    GFile *file = g_file_new_for_uri(url.c_str());

    GInputStream *stream = (GInputStream *)g_file_read(file, nullptr, nullptr);

    if(stream)
    {
        gssize sz;
        char buff[BUFF_SIZE + 1];
        while( (sz = g_input_stream_read(stream, buff, BUFF_SIZE, nullptr, nullptr)) > 0 )
        {
            buff[sz] = 0;
            out += buff;
        }
        g_object_unref(stream);
    }
    g_object_unref(file);
}

void html_widget::dump(const litehtml::string& file_name)
{
printf("%s called\n",__FUNCTION__);
    if(m_html)
    {
        html_dumper dumper(file_name);
        m_html->dump(dumper);
        dumper.print();
    }
}

