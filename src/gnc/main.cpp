/*
 * main.cpp -- The program entry point for cutecash
 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>
 * Copyright (C) 2010 Christian Stimming
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
#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libguile.h>
#include <glib/gi18n.h>
#include <glib.h>
extern "C"
{
#include "gnc-module/gnc-module.h"
#include "core-utils/gnc-path.h"
#include "core-utils/binreloc.h"
    /* #include "gnc-version.h" */
#include "engine/gnc-engine.h"
#include "core-utils/gnc-filepath-utils.h"
#include "engine/gnc-hooks.h"
#include "engine/gnc-commodity.h"
#include "core-utils/gnc-main.h"
#include "engine/gnc-session.h"
#include "engine/engine-helpers.h"
#include "engine/gnc-engine.h"
#include "swig-runtime.h"

#include "backend/xml/gnc-backend-xml.h"
#include "business/business-core/gncBusiness.h"
#include "business/business-core/xml/gncmod-business-backend-xml.h"
} // END extern C

#ifdef HAVE_GETTEXT
#  include <libintl.h>
#  include <locale.h>
#endif

#include <QApplication>
#include "mainwindow.hpp"

namespace gnc
{

#define APP_GNUCASH "/apps/gnucash"

/* GNUCASH_SVN is defined whenever we're building from an SVN tree */
#ifdef GNUCASH_SVN
static int is_development_version = TRUE;
#else
static int is_development_version = FALSE;
#endif

static gchar **log_flags = NULL;
static gchar *log_to_filename = NULL;

static void
gnc_log_init()
{
    if (log_to_filename != NULL)
    {
        qof_log_init_filename_special(log_to_filename);
    }
    else
    {
        /* initialize logging to our file. */
        gchar *tracefilename;
        tracefilename = g_build_filename(g_get_tmp_dir(), "gnucash.trace",
                                         (gchar *)NULL);
        qof_log_init_filename(tracefilename);
        g_free(tracefilename);
    }

    // set a reasonable default.
    qof_log_set_default(QOF_LOG_WARNING);

    gnc_log_default();

    if (gnc_is_debugging())
    {
        qof_log_set_level("", QOF_LOG_INFO);
        qof_log_set_level("qof", QOF_LOG_INFO);
        qof_log_set_level("gnc", QOF_LOG_INFO);
    }

    {
        gchar *log_config_filename;
        log_config_filename = gnc_build_dotgnucash_path("log.conf");
        if (g_file_test(log_config_filename, G_FILE_TEST_EXISTS))
            qof_log_parse_log_config(log_config_filename);
        g_free(log_config_filename);
    }

    if (log_flags != NULL)
    {
        int i = 0;
        for (; log_flags[i] != NULL; i++)
        {
            QofLogLevel level;
            gchar **parts = NULL;

            gchar *log_opt = log_flags[i];
            parts = g_strsplit(log_opt, "=", 2);
            if (parts == NULL || parts[0] == NULL || parts[1] == NULL)
            {
                g_warning("string [%s] not parseable", log_opt);
                continue;
            }

            level = qof_log_level_from_string(parts[1]);
            qof_log_set_level(parts[0], level);
            g_strfreev(parts);
        }
    }
}

} // END namespace gnc

int
main(int argc, char ** argv)
{
#if !defined(G_THREADS_ENABLED) || defined(G_THREADS_IMPL_NONE)
#    error "No GLib thread implementation available!"
#endif
    g_thread_init(NULL);

#ifdef ENABLE_BINRELOC
    {
        GError *binreloc_error = NULL;
        if (!gbr_init(&binreloc_error))
        {
            g_print("main: Error on gbr_init: %s\n", binreloc_error->message);
            g_error_free(binreloc_error);
        }
    }
#else
    //g_message("main: binreloc relocation support was disabled at configure time.\n");
#endif

#ifdef HAVE_GETTEXT
    {
        gchar *localedir = gnc_path_get_localedir();
        /* setlocale(LC_ALL, ""); is already called by gtk_set_locale()
           via gtk_init(). */
        bindtextdomain(GETTEXT_PACKAGE, localedir);
        textdomain(GETTEXT_PACKAGE);
        bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
        g_free(localedir);
    }
#endif

    qof_log_init();
    qof_log_set_default(QOF_LOG_INFO);

    gnc::gnc_log_init();

    qof_init();
    gnc_module_system_init();
    gnc_engine_init_static(argc, argv);

    // Call the statically-linked versions of the backend init
    // functions
    gnc_module_init_backend_xml();
    //gnc_module_init_backend_dbi();
    gnc_module_init_business_core_init();
    gnc_module_init_business_core_xml_init();

    // From here on the new C++ code
    QApplication app(argc, argv);
    gnc::MainWindow mainWin;
    mainWin.show();

    // Go into the main qt event loop
    int r = app.exec();

    // Shutdown
    //gnc_module_finalize_backend_dbi();
    qof_close();
    return r;

}
