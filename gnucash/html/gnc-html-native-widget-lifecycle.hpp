/********************************************************************
 * gnc-html-native-widget-lifecycle.hpp -- native view GTK cleanup *
 *                                                                  *
 * Copyright (C) 2026 The GnuCash Project                           *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 ********************************************************************/

#ifndef GNC_HTML_NATIVE_WIDGET_LIFECYCLE_HPP
#define GNC_HTML_NATIVE_WIDGET_LIFECYCLE_HPP

#include <gtk/gtk.h>

#include <vector>

class GncHtmlNativeWidgetLifecycle
{
public:
    explicit GncHtmlNativeWidgetLifecycle (GtkWidget *view = nullptr) : view_ (view) {}

    void set_view (GtkWidget *view) { view_ = view; }

    void add_signal (GObject *object, gulong signal)
    {
        if (object && signal)
            signals_.push_back ({object, signal});
    }

    void set_tick_callback (guint callback) { tick_callback_ = callback; }

    void add_controller (GtkEventController *controller)
    {
        if (controller)
            controllers_.push_back (controller);
    }

    void clear ()
    {
        if (!view_)
            return;

        for (const auto &signal : signals_)
            if (g_signal_handler_is_connected (signal.object, signal.id))
                g_signal_handler_disconnect (signal.object, signal.id);
        signals_.clear ();

        if (tick_callback_)
        {
            gtk_widget_remove_tick_callback (view_, tick_callback_);
            tick_callback_ = 0;
        }

        for (const auto controller : controllers_)
            if (gtk_event_controller_get_widget (controller) == view_)
                gtk_widget_remove_controller (view_, controller);
        controllers_.clear ();
        view_ = nullptr;
    }

    guint tick_callback () const { return tick_callback_; }

private:
    struct Signal
    {
        GObject *object;
        gulong id;
    };

    GtkWidget *view_ = nullptr;
    guint tick_callback_ = 0;
    std::vector<Signal> signals_;
    std::vector<GtkEventController *> controllers_;
};

#endif /* GNC_HTML_NATIVE_WIDGET_LIFECYCLE_HPP */
