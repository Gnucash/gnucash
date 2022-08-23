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

#include <libguile.h>
#include <gtk/gtk.h>
#include <vector>
#include <gnc-option.hpp>
#include <gnc-option-uitype.hpp>
#include <gnc-option-ui.hpp>

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

typedef GtkWidget* (*WidgetCreateFunc)(GncOption&, GtkGrid*, GtkLabel*, char*,
                                       GtkWidget**, bool*);
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
 *  @param name The label to attach to the widget
 *  @param description The text for the widget's tooltip
 *  @param enclosing The widget's parent
 *  @param packed Whether the widget will be packed into an eventbox.
 *  @return pointer to the created widget.
 */
    static GtkWidget* create(GncOption&, GtkGrid*, GtkLabel*, char*,
                             GtkWidget**, bool*);
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

template<GncOptionUIType type> GtkWidget*
create_option_widget(GncOption& option, GtkGrid*, GtkLabel*, char*, GtkWidget**,
                     bool*);

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

#endif //GNC_OPTION_GTK_UI_HPP
