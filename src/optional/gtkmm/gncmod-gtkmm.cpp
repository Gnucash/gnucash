/*
 * gncmod-gtkmm.cpp --
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

/**
 * @internal
 * @file gncmod-gtkmm.cpp
 * @brief Module definition/initialization for gtkmm support
 * @author Copyright (C) 2011 Christian Stimming <stimming@tuhh.de>
 */

#include "config.h"
/*#include <glib.h>*/
#include <glib/gi18n.h>

extern "C" {
#include "gnc-module/gnc-module.h"
#include "gnc-module/gnc-module-api.h"

    GNC_MODULE_API_DECL(libgncmod_gtkmm)

    /* version of the gnc module system interface we require */
    gint libgncmod_gtkmm_gnc_module_system_interface = 0;

    /* module versioning uses libtool semantics. */
    gint libgncmod_gtkmm_gnc_module_current  = 0;
    gint libgncmod_gtkmm_gnc_module_revision = 0;
    gint libgncmod_gtkmm_gnc_module_age      = 0;
} // END extern "C"

// c++ includes
#include <gtkmm.h>

// And our own plugin
#include "gnc-plugin-gtkmm.hpp"

extern "C" {

    gchar *
    libgncmod_gtkmm_gnc_module_path(void)
    {
        return g_strdup("gnucash/gtkmm");
    }

    gchar *
    libgncmod_gtkmm_gnc_module_description(void)
    {
        return g_strdup("Support for gtkmm gui");
    }

    gint
    libgncmod_gtkmm_gnc_module_init(gint refcount)
    {
        // Load modules we depend on
        if (!gnc_module_load("gnucash/engine", 0)
                || !gnc_module_load("gnucash/app-utils", 0)
                || !gnc_module_load("gnucash/gnome-utils", 0))
        {
            return FALSE;
        }

        // Initialize the gtkmm framework. Calling this static method
        // is sufficient; we don't actually need a Gtk::Main object.
        Gtk::Main::init_gtkmm_internals();

        // Register our plugin, adding menu items with callbacks
        gncmm::gnc_plugin_gtkmm_create_plugin();

        return 1;
    }

    gint
    libgncmod_gtkmm_gnc_module_end(gint refcount)
    {
        return 1;
    }

} // END extern "C"
