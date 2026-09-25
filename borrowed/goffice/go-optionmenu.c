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

#include <gdk/gdk.h>
#include <glib/gi18n-lib.h>

#define OPTION_MENU_TEXT_KEY "option-menu-text"
#define OPTION_MENU_INDICATOR_KEY "go-option-menu-selected-indicator"

enum
{
    CHANGED, LAST_SIGNAL
};

enum
{
    PROP_0,
    PROP_MENU
};

static guint signals[LAST_SIGNAL] = { 0 };

struct _GOOptionMenu
{
    GtkButton button;

    GtkPopover *menu;
    GtkWidget *selected;
    GtkLabel *button_label;
    GHashTable *history;
};

G_DEFINE_TYPE (GOOptionMenu, go_option_menu, GTK_TYPE_BUTTON)

GtkWidget *
go_option_menu_new(void)
{
    return g_object_new(GO_TYPE_OPTION_MENU, NULL);
}
static char *
go_option_menu_history_key(const GSList *selection)
{
    GString *key = g_string_new(NULL);

    for (; selection; selection = selection->next)
        g_string_append_printf(key, "%d/", GPOINTER_TO_INT(selection->data));

    return g_string_free(key, FALSE);
}

static void
go_option_menu_update_contents(GOOptionMenu *option_menu)
{
    const char *text = NULL;
    GtkWidget *child = NULL;

    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));

    if (option_menu->selected)
    {
        text = g_object_get_data(G_OBJECT(option_menu->selected),
                                 OPTION_MENU_TEXT_KEY);
        if (!text && GTK_IS_BUTTON(option_menu->selected))
            child = gtk_button_get_child(GTK_BUTTON(option_menu->selected));
        if (!text && child && GTK_IS_LABEL(child))
            text = gtk_label_get_text(GTK_LABEL(child));
    }

    gtk_label_set_text(option_menu->button_label, text ? text : "");
}

static void
go_option_menu_set_indicator(GtkWidget *item, gboolean selected)
{
    GtkWidget *indicator;

    if (!item)
        return;

    indicator = g_object_get_data(G_OBJECT(item), OPTION_MENU_INDICATOR_KEY);
    if (indicator)
        gtk_widget_set_visible(indicator, selected);
}

void
go_option_menu_set_active_item(GOOptionMenu *option_menu, GtkWidget *item)
{
    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));
    g_return_if_fail(GTK_IS_WIDGET(item));

    if (item == option_menu->selected)
        return;

    go_option_menu_set_indicator(option_menu->selected, FALSE);
    option_menu->selected = item;
    go_option_menu_set_indicator(option_menu->selected, TRUE);
    go_option_menu_update_contents(option_menu);
}

void
go_option_menu_activate_item(GOOptionMenu *option_menu, GtkWidget *item)
{
    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));
    g_return_if_fail(GTK_IS_WIDGET(item));

    go_option_menu_set_active_item(option_menu, item);
    g_signal_emit(option_menu, signals[CHANGED], 0);

    if (option_menu->menu)
        gtk_popover_popdown(option_menu->menu);
}

void
go_option_menu_register_item(GOOptionMenu *option_menu,
                             const GSList *selection, GtkWidget *item)
{
    char *key;

    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));
    g_return_if_fail(selection != NULL);
    g_return_if_fail(GTK_IS_WIDGET(item));

    key = go_option_menu_history_key(selection);
    g_hash_table_replace(option_menu->history, key, item);
}

static gboolean
go_option_menu_popup(GOOptionMenu *option_menu)
{
    if (!option_menu->menu)
        return FALSE;

    gtk_popover_set_position(option_menu->menu, GTK_POS_BOTTOM);
    gtk_popover_popup(option_menu->menu);
    return TRUE;
}

static void
go_option_menu_click_pressed(GtkGestureClick *gesture,
                             G_GNUC_UNUSED gint n_press,
                             G_GNUC_UNUSED gdouble x,
                             G_GNUC_UNUSED gdouble y,
                             GOOptionMenu *option_menu)
{
    if (go_option_menu_popup(option_menu))
        gtk_gesture_set_state(GTK_GESTURE(gesture), GTK_EVENT_SEQUENCE_CLAIMED);
}

static gboolean
go_option_menu_key_pressed(GtkEventControllerKey *controller,
                           guint keyval,
                           G_GNUC_UNUSED guint keycode,
                           G_GNUC_UNUSED GdkModifierType state,
                           GOOptionMenu *option_menu)
{
    switch (keyval)
    {
    case GDK_KEY_KP_Space:
    case GDK_KEY_space:
        if (go_option_menu_popup(option_menu))
        {
            gtk_event_controller_reset(GTK_EVENT_CONTROLLER(controller));
            return TRUE;
        }
        break;
    }

    return FALSE;
}

void
go_option_menu_set_menu(GOOptionMenu *option_menu, GtkWidget *menu)
{
    GtkWidget *old_menu;

    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));
    g_return_if_fail(menu == NULL || GTK_IS_POPOVER(menu));

    if ((GtkWidget *) option_menu->menu == menu)
        return;

    old_menu = GTK_WIDGET(option_menu->menu);
    if (old_menu)
    {
        go_option_menu_set_indicator(option_menu->selected, FALSE);
        option_menu->selected = NULL;
        gtk_popover_popdown(option_menu->menu);
        if (gtk_widget_get_parent(old_menu) == GTK_WIDGET(option_menu))
            gtk_widget_unparent(old_menu);
        g_clear_object(&option_menu->menu);
    }

    if (option_menu->history)
        g_hash_table_remove_all(option_menu->history);
    go_option_menu_update_contents(option_menu);

    if (menu)
    {
        option_menu->menu = g_object_ref(GTK_POPOVER(menu));
        gtk_widget_set_parent(menu, GTK_WIDGET(option_menu));
    }

    g_object_notify(G_OBJECT(option_menu), "menu");
}

void
go_option_menu_set_history(GOOptionMenu *option_menu, GSList *selection)
{
    GtkWidget *item;
    char *key;

    g_return_if_fail(GO_IS_OPTION_MENU(option_menu));
    g_return_if_fail(selection != NULL);

    key = go_option_menu_history_key(selection);
    item = g_hash_table_lookup(option_menu->history, key);
    g_free(key);

    if (item)
        go_option_menu_set_active_item(option_menu, item);
}

/**
 * go_option_menu_get_history:
 * @option_menu: a #GOOptionMenu
 *
 * Retrieves the currently selected option widget.
 *
 * Return value: (transfer none): the selected option widget
 **/
GtkWidget *
go_option_menu_get_history(GOOptionMenu *option_menu)
{
    g_return_val_if_fail(GO_IS_OPTION_MENU(option_menu), NULL);
    return option_menu->selected;
}

static void
go_option_menu_set_property(GObject *object, guint prop_id,
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

static void
go_option_menu_get_property(GObject *object, guint prop_id,
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

static void
go_option_menu_dispose(GObject *object)
{
    GOOptionMenu *option_menu = GO_OPTION_MENU(object);

    go_option_menu_set_menu(option_menu, NULL);
    g_clear_pointer(&option_menu->history, g_hash_table_unref);

    G_OBJECT_CLASS(go_option_menu_parent_class)->dispose(object);
}

static void
go_option_menu_class_init(GOOptionMenuClass *class)
{
    GObjectClass *gobject_class = G_OBJECT_CLASS(class);

    signals[CHANGED] = g_signal_new("changed", G_OBJECT_CLASS_TYPE(class),
            G_SIGNAL_RUN_LAST, 0,
            NULL, NULL, g_cclosure_marshal_VOID__VOID, G_TYPE_NONE, 0);

    gobject_class->set_property = go_option_menu_set_property;
    gobject_class->get_property = go_option_menu_get_property;
    gobject_class->dispose = go_option_menu_dispose;

    g_object_class_install_property(gobject_class, PROP_MENU,
            g_param_spec_object("menu", _("Menu"), _("The menu of options"),
                    GTK_TYPE_POPOVER, G_PARAM_READABLE | G_PARAM_WRITABLE));
}

static void
go_option_menu_init(GOOptionMenu *option_menu)
{
    GtkEventController *key_controller;
    GtkGesture *click_gesture;
    GtkWidget *box;
    GtkWidget *arrow;
    GtkWidget *separator;

    gtk_widget_set_focusable(GTK_WIDGET(option_menu), TRUE);

    option_menu->history = g_hash_table_new_full(g_str_hash, g_str_equal,
                                                 g_free, NULL);

    box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    option_menu->button_label = GTK_LABEL(gtk_label_new(""));
    gtk_label_set_xalign(option_menu->button_label, 0.0f);
    gtk_widget_set_hexpand(GTK_WIDGET(option_menu->button_label), TRUE);
    gtk_box_append(GTK_BOX(box), GTK_WIDGET(option_menu->button_label));

    separator = gtk_separator_new(GTK_ORIENTATION_VERTICAL);
    gtk_widget_set_margin_start(separator, 5);
    gtk_widget_set_margin_end(separator, 5);
    gtk_box_append(GTK_BOX(box), separator);

    arrow = gtk_image_new_from_icon_name("pan-down-symbolic");
    gtk_box_append(GTK_BOX(box), arrow);

    gtk_button_set_child(GTK_BUTTON(option_menu), box);

    click_gesture = gtk_gesture_click_new();
    gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(click_gesture),
                                  GDK_BUTTON_PRIMARY);
    gtk_event_controller_set_propagation_phase(GTK_EVENT_CONTROLLER(click_gesture),
                                               GTK_PHASE_CAPTURE);
    g_signal_connect(click_gesture, "pressed",
                     G_CALLBACK(go_option_menu_click_pressed), option_menu);
    gtk_widget_add_controller(GTK_WIDGET(option_menu),
                              GTK_EVENT_CONTROLLER(click_gesture));

    key_controller = gtk_event_controller_key_new();
    gtk_event_controller_set_propagation_phase(key_controller, GTK_PHASE_CAPTURE);
    g_signal_connect(key_controller, "key-pressed",
                     G_CALLBACK(go_option_menu_key_pressed), option_menu);
    gtk_widget_add_controller(GTK_WIDGET(option_menu), key_controller);
}
