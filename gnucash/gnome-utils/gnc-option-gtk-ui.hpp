/********************************************************************\
 * gnc-option-gtk-ui.hpp -- Gtk Widgets for manipulating options    *
  * Copyright 2022 John Ralls <jralls@ceridwen.us>                  *
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

#ifndef GNC_OPTION_GTK_UI_HPP
#define GNC_OPTION_GTK_UI_HPP

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <vector>
#include <gnc-option.hpp>
#include <gnc-option-uitype.hpp>
#include <gnc-option-ui.hpp>
#include <libguile.h>

/** @fn WidgetCreateFunc
*  Function pointer for per-option-type GtkWidget constructors.
*  @param option The option to create an element for.
*  @param page_box The option dialog page's layout grid
*  @param name_label A GtkLabel to attach to the widget
*  @param documentation The string to use for the tooltip.
*  @param enclosing The parent widget
*  @param packed Whether the widget will be packed into an eventbox.
*  @return pointer to the widget.
*/

typedef void (*WidgetCreateFunc)(GncOption&, GtkGrid*, int);

/** @class GncOptionUIFactory
 *  Factory class that keeps track of which GncOptionValueType needs which
 *  WidgetCreateFunc and calls the appropriate one when required.
 */
class GncOptionUIFactory
{
public:
/** Register a WidgetCreateFunc
 *  @param type The UI type
 *  @param func The function to register
 */
    static void set_func(GncOptionUIType type, WidgetCreateFunc func);
/** Create a widget
 *  @param option The option for which to create the widget
 *  @param page The Option dialog page in which to insert the widget
 *  @param row The row in which to insert the widget
 *  @return pointer to the created widget.
 */
    static void create(GncOption& option, GtkGrid* page, int row);
private:
    static std::vector<WidgetCreateFunc> s_registry;
    static bool s_initialized;
};

/** class GncOptionGtkUIItem
 *  Gtk-specific Interface class for Option Widget
 */
class GncOptionGtkUIItem : public GncOptionUIItem
{
public:
    GncOptionGtkUIItem(GtkWidget* widget, GncOptionUIType type);
    GncOptionGtkUIItem(const GncOptionGtkUIItem& item);
    GncOptionGtkUIItem(GncOptionGtkUIItem&&) = default;
    virtual ~GncOptionGtkUIItem() override;
/** Control wether the widget is sensitive */
    virtual void set_selectable(bool) const noexcept override;
/** Clear the data from the widget. */
    void clear_ui_item() override;
    void set_widget(GtkWidget* widget);
    virtual GtkWidget* get_widget() const { return m_widget; }
    virtual SCM get_widget_scm_value(const GncOption&) const;
private:
    GtkWidget* m_widget;
};

void gnc_option_changed_widget_cb (GtkWidget *widget, GncOption *option);
void gnc_option_changed_option_cb (GtkWidget *dummy, GncOption *option);

template<GncOptionUIType type> void
create_option_widget(GncOption& option, GtkGrid*, int row);

/** Templated cast to convert various QofInstance subtype ptrs into QofInstance*
 * to placate the C++ type system. QofInstance is a GObject hierarchy so the
 * usual C++ type substitution doesn't work.
 */
template <typename Instance> inline const QofInstance*
qof_instance_cast(Instance inst)
{
    static_assert(std::is_pointer_v<Instance>, "Pointers Only!");
    return reinterpret_cast<const QofInstance*>(inst);
}

inline void
align_label (GtkLabel *name_label)
{
    /* some option widgets have a large vertical footprint so align
       the label name to the top and add a small top margin */
    gtk_widget_set_valign (GTK_WIDGET(name_label), GTK_ALIGN_START);
    gtk_widget_set_margin_top (GTK_WIDGET(name_label), 6);
}

inline void
set_name_label(const GncOption& option, GtkGrid* page_box, int row, bool align)
{
    auto name{option.get_name().c_str()};
    if (name && *name)
    {
        auto label{gtk_label_new(_(name))};
        if (align)
            align_label(GTK_LABEL(label));
        gtk_widget_set_halign (GTK_WIDGET(label), GTK_ALIGN_END);
        gtk_grid_attach(GTK_GRID(page_box), label, 0, row, 1, 1);
    }
}

inline void
set_tool_tip(const GncOption& option, GtkWidget* box)
{
    auto documentation{option.get_docstring().c_str()};
    if (documentation && *documentation)
        gtk_widget_set_tooltip_text(box, _(documentation));
}

inline void
grid_attach_widget(GtkGrid* grid, GtkWidget* widget, int row)
{
    /* attach the option widget to the second column of the grid */
    gtk_grid_attach (grid, widget, 1, row, 1, 1);

}

inline void
wrap_widget (const GncOption& option, GtkWidget* widget, GtkGrid* page_box, int row)
{
    auto enclosing{gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 5)};
    gtk_box_set_homogeneous (GTK_BOX (enclosing), FALSE);
    gtk_box_pack_start(GTK_BOX(enclosing), widget, FALSE, FALSE, 0);
    set_name_label(option, page_box, row, false);
    set_tool_tip(option, enclosing);
    gtk_widget_show_all(enclosing);
    grid_attach_widget(page_box, enclosing, row);
}
#endif //GNC_OPTION_GTK_UI_HPP
