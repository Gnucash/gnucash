/*
 * go-optionmenu.c
 *
 * Copyright (C) 2002 Andreas J. Guelzow <aguelzow@taliesin.ca>
 * Copyright (C) 2006 Morten Welinder (terra@gnome.org)
 *
 * based extensively on:
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * Modified by the GTK+ Team and others 1997-2000.  See the GTK AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 *
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
 * USA.
 */

#include <config.h>
#include "go-optionmenu.h"

#include <gdk/gdkkeysyms.h>
#include <glib/gi18n-lib.h>

enum
{
    CHANGED, LAST_SIGNAL
};

enum
{
    PROP_0,
    PROP_MENU
};

static GtkButtonClass *parent_class = NULL;
static guint signals[LAST_SIGNAL] = { 0 };

GtkWidget*
go_option_menu_new(void)
{
    return g_object_new(GO_TYPE_OPTION_MENU, NULL);
}

static void go_option_menu_detacher(GtkWidget *widget, GtkMenu *menu)
{
#if 0
    GOOptionMenu *option_menu = GO_OPTION_MENU (widget);
    /* What?  */
#endif
}

static void go_option_menu_update_contents(GOOptionMenu *option_menu)
{
    const char *text;
    GtkWidget *w;
    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));

    w = gtk_bin_get_child(GTK_BIN(option_menu->selected));
    text = g_object_get_data(G_OBJECT(w), "option-menu-text");

    if (!text && GTK_IS_LABEL(w))
        text = gtk_label_get_text(GTK_LABEL(w));

    if (!text)
        text = "";

#if 0
    g_print ("text = \"%s\"\n", text);
#endif

    gtk_label_set_text(option_menu->button_label, text);
}

static void go_option_menu_select_item(GOOptionMenu *option_menu,
        GtkMenuItem *item)
{
    if (item == option_menu->selected)
        return;

    if (GTK_IS_CHECK_MENU_ITEM(option_menu->selected))
        gtk_check_menu_item_set_active(
                GTK_CHECK_MENU_ITEM(option_menu->selected), FALSE);

    option_menu->selected = item;

    if (GTK_IS_CHECK_MENU_ITEM(item))
        gtk_check_menu_item_set_active(
                GTK_CHECK_MENU_ITEM(option_menu->selected), TRUE);

    go_option_menu_update_contents(option_menu);
}

#if !GTK_CHECK_VERSION(3,22,0)
static void go_option_menu_position(GtkMenu *menu, gint *x, gint *y,
        gboolean *push_in, gpointer user_data)
{
    GOOptionMenu *option_menu = user_data;
    GtkWidget *widget;
    GtkRequisition requisition;
    GList *children;
    gint screen_width;
    gint menu_xpos;
    gint menu_ypos;
    gint menu_width;
    GtkAllocation allocation;

    widget = GTK_WIDGET(option_menu);

    gtk_widget_get_preferred_size(GTK_WIDGET(menu), &requisition, NULL);
    menu_width = requisition.width;

    gdk_window_get_origin(gtk_widget_get_window(widget), &menu_xpos,
            &menu_ypos);

    gtk_widget_get_allocation(widget, &allocation);
    menu_xpos += allocation.x;
    menu_ypos += allocation.y + allocation.height / 2 - 2;

    children = gtk_container_get_children(GTK_CONTAINER(option_menu->menu));
    while (children)
    {
        GtkWidget *child = children->data;

        if (GTK_IS_CHECK_MENU_ITEM(child)
                && gtk_check_menu_item_get_active(GTK_CHECK_MENU_ITEM(child)))
        {
            gtk_widget_get_preferred_size(child, &requisition, NULL);
            menu_ypos -= requisition.height / 2;
            break;
        }

        if (gtk_widget_get_visible(child))
        {
            gtk_widget_get_preferred_size(child, &requisition, NULL);
            menu_ypos -= requisition.height;
        }

        children = children->next;
    }

    screen_width = gdk_screen_get_width(gtk_widget_get_screen(widget));

    if (menu_xpos + menu_width > screen_width)
        menu_xpos -= (menu_xpos + menu_width) - screen_width;
    if (menu_xpos < 0)
        menu_xpos = 0;

    *x = menu_xpos;
    *y = menu_ypos;
    *push_in = TRUE;
}
#endif

static gint go_option_menu_button_press(GtkWidget *widget,
        GdkEventButton *event)
{
    GOOptionMenu *option_menu;

    g_return_val_if_fail(GO_IS_OPTION_MENU(widget), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    option_menu = GO_OPTION_MENU(widget);

    if (event->type == GDK_BUTTON_PRESS && event->button == 1)
    {
#if GTK_CHECK_VERSION(3,22,0)
        gtk_menu_popup_at_widget (GTK_MENU(option_menu->menu),
                                  widget,
                                  GDK_GRAVITY_SOUTH_WEST,
                                  GDK_GRAVITY_NORTH_WEST,
                                  (GdkEvent *) event);
#else
        gtk_menu_popup(GTK_MENU(option_menu->menu), NULL, NULL,
                go_option_menu_position, option_menu, event->button,
                event->time);

#endif
        return TRUE;
    }

    return FALSE;
}

static gint go_option_menu_key_press(GtkWidget *widget, GdkEventKey *event)
{
    GOOptionMenu *option_menu = GO_OPTION_MENU(widget);

    switch (event->keyval)
    {
    case GDK_KEY_KP_Space:
    case GDK_KEY_space:
#if GTK_CHECK_VERSION(3,22,0)
        gtk_menu_popup_at_widget (GTK_MENU(option_menu->menu),
                                  widget,
                                  GDK_GRAVITY_SOUTH_WEST,
                                  GDK_GRAVITY_NORTH_WEST,
                                  (GdkEvent *) event);
#else
        gtk_menu_popup(GTK_MENU(option_menu->menu), NULL, NULL,
                go_option_menu_position, option_menu, 0, event->time);
#endif
        return TRUE;
    }

    return FALSE;
}

static void cb_select(GtkMenuItem *item, GOOptionMenu *option_menu)
{
    go_option_menu_select_item(option_menu, item);
    g_signal_emit(option_menu, signals[CHANGED], 0);
}

static void handle_menu_signals(GOOptionMenu *option_menu, gboolean connect)
{
    GList *children = gtk_container_get_children(
            GTK_CONTAINER(option_menu->menu));

    while (children)
    {
        GtkWidget *child = children->data;
        children = g_list_remove(children, child);

        if (GTK_IS_MENU_ITEM(child))
        {
            GtkWidget *sub = gtk_menu_item_get_submenu(GTK_MENU_ITEM(child));

            if (sub)
                children = g_list_concat(children,
                        gtk_container_get_children(GTK_CONTAINER(sub)));
            else if (connect)
                g_signal_connect(child, "activate", G_CALLBACK(cb_select),
                        option_menu);

            else
                g_signal_handlers_disconnect_by_func(child,
                        G_CALLBACK(cb_select), option_menu);

        }
    }
}

void go_option_menu_set_menu(GOOptionMenu *option_menu, GtkWidget *menu)
{
    GtkMenuShell *shell;

    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));
    g_return_if_fail(GTK_IS_MENU_SHELL(menu));

    shell = (GtkMenuShell *) menu;
    if (option_menu->menu == shell)
        return;

    if (option_menu->menu)
    {
        gtk_menu_shell_cancel(option_menu->menu);

        handle_menu_signals(option_menu, FALSE);

        gtk_menu_detach(GTK_MENU(option_menu->menu));
        g_object_unref(option_menu->menu);
    }

    option_menu->menu = shell;

    if (shell)
    {
        g_object_ref(shell);

        gtk_menu_attach_to_widget(GTK_MENU(shell), GTK_WIDGET(option_menu),
                go_option_menu_detacher);

        handle_menu_signals(option_menu, TRUE);

        go_option_menu_select_item(option_menu,
                GTK_MENU_ITEM(gtk_menu_get_active(GTK_MENU(shell))));
    }

    g_object_notify(G_OBJECT(option_menu), "menu");
}

void go_option_menu_set_history(GOOptionMenu *option_menu, GSList *selection)
{
    g_return_if_fail(selection != NULL);
    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));

    if (option_menu->menu)
    {
        GtkMenuShell *menu = option_menu->menu;

        while (1)
        {
            int n = GPOINTER_TO_INT(selection->data);
            GtkMenuItem *item = g_list_nth_data(
                    gtk_container_get_children(GTK_CONTAINER(menu)), n);
            selection = selection->next;
            if (selection)
                menu = GTK_MENU_SHELL(gtk_menu_item_get_submenu(item));
            else
            {
                go_option_menu_select_item(option_menu, item);
                break;
            }
        }
    }
}

/**
 * go_option_menu_get_history:
 * @option_menu: a #GOOptionMenu
 *
 * Retrieves the currently selected menu item.
 *
 * Return value: the selected menu_item
 **/

GtkWidget *
go_option_menu_get_history(GOOptionMenu *option_menu)
{
    return GTK_WIDGET(option_menu->selected);
}

static void go_option_menu_set_property(GObject *object, guint prop_id,
        const GValue *value, GParamSpec *pspec)
{
    GOOptionMenu *option_menu = GO_OPTION_MENU(object);

    switch (prop_id)
    {
    case PROP_MENU:
        go_option_menu_set_menu(option_menu, g_value_get_object(value));
        break;

    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void go_option_menu_get_property(GObject *object, guint prop_id,
        GValue *value, GParamSpec *pspec)
{
    GOOptionMenu *option_menu = GO_OPTION_MENU(object);

    switch (prop_id)
    {
    case PROP_MENU:
        g_value_set_object(value, option_menu->menu);
        break;

    default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID(object, prop_id, pspec);
        break;
    }
}

static void go_option_menu_destroy(GtkWidget *widget)
{
    GOOptionMenu *option_menu;

    g_return_if_fail(GO_IS_OPTION_MENU(widget));

    option_menu = GO_OPTION_MENU(widget);

    if (option_menu->menu)
    {
        gtk_widget_destroy(GTK_WIDGET(option_menu->menu));
        g_object_unref(option_menu->menu);
        option_menu->menu = NULL;
    }
    option_menu->selected = NULL;

    GTK_WIDGET_CLASS(parent_class)->destroy(widget);
}

static void go_option_menu_class_init(GOOptionMenuClass *class)
{
    GObjectClass *gobject_class = (GObjectClass*) class;
    GtkWidgetClass *widget_class = (GtkWidgetClass*) class;

    parent_class = g_type_class_peek_parent(class);

    signals[CHANGED] = g_signal_new("changed", G_OBJECT_CLASS_TYPE(class),
            G_SIGNAL_RUN_LAST, G_STRUCT_OFFSET(GOOptionMenuClass, changed),
            NULL, NULL, g_cclosure_marshal_VOID__VOID, G_TYPE_NONE, 0);

    gobject_class->set_property = go_option_menu_set_property;
    gobject_class->get_property = go_option_menu_get_property;
    widget_class->destroy = go_option_menu_destroy;
    widget_class->button_press_event = go_option_menu_button_press;
    widget_class->key_press_event = go_option_menu_key_press;

    g_object_class_install_property(gobject_class, PROP_MENU,
            g_param_spec_object("menu", _("Menu"), _("The menu of options"),
                    GTK_TYPE_MENU, G_PARAM_READABLE | G_PARAM_WRITABLE));
}

static void go_option_menu_init(GOOptionMenu *option_menu)
{
    GtkWidget *box;
    GtkWidget *arrow, *sep;

    gtk_widget_set_can_focus(GTK_WIDGET(option_menu), TRUE);
    gtk_widget_set_can_default(GTK_WIDGET(option_menu), FALSE);
    gtk_widget_set_receives_default(GTK_WIDGET(option_menu), FALSE);

    box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_set_homogeneous (GTK_BOX (box), FALSE);

    option_menu->menu = NULL;
    option_menu->selected = NULL;

    option_menu->button_label = GTK_LABEL(gtk_label_new(""));
    gtk_box_pack_start(GTK_BOX(box), GTK_WIDGET(option_menu->button_label), FALSE, TRUE, 0);
#if GTK_CHECK_VERSION(3,14,0)
    arrow = gtk_image_new_from_icon_name ("go-down", GTK_ICON_SIZE_BUTTON);
#else
    arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_NONE);
#endif
#if GTK_CHECK_VERSION(3,12,0)
    gtk_widget_set_margin_start (GTK_WIDGET(arrow), 5);
#else
    gtk_widget_set_margin_left (GTK_WIDGET(arrow), 5);
#endif

    gtk_box_pack_end(GTK_BOX(box), arrow, FALSE, FALSE, 0);

    sep = gtk_separator_new (GTK_ORIENTATION_VERTICAL);
    gtk_box_pack_end(GTK_BOX(box), sep, FALSE, FALSE, 0);

    gtk_container_add(GTK_CONTAINER(option_menu), GTK_WIDGET(box));
}

GType go_option_menu_get_type(void)
{
    static GType option_menu_type = 0;

    if (!option_menu_type)
    {
        static const GTypeInfo option_menu_info =
        { sizeof(GOOptionMenuClass), NULL, /* base_init */
        NULL, /* base_finalize */
        (GClassInitFunc) go_option_menu_class_init, NULL, /* class_finalize */
        NULL, /* class_data */
        sizeof(GOOptionMenu), 0, /* n_preallocs */
        (GInstanceInitFunc) go_option_menu_init, };

        option_menu_type = g_type_register_static(GTK_TYPE_BUTTON,
                "GOOptionMenu", &option_menu_info, 0);
    }

    return option_menu_type;
}
