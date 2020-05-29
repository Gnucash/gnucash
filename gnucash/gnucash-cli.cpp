/*
 * gnucash-cli.cpp -- The command line entry point for GnuCash
 *
 * Copyright (C) 2020 Geert Janssens <geert@kobaltwit.be>
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
#include <config.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libguile.h>
#include <glib/gi18n.h>
#include <glib.h>
#include <binreloc.h>
#include <gnc-locale-utils.h>
#include <gnc-engine.h>
#include <gnc-ui-util.h>
#include <gnc-commodity.h>
#include <swig-runtime.h>
#include <guile-mappings.h>
#ifdef __MINGW32__
#include <Windows.h>
#include <fcntl.h>
#endif

#include "gnucash-base.hpp"

extern "C" {
#include <gfec.h>
#include <gnc-engine-guile.h>
#include <gnc-environment.h>
#include <gnc-filepath-utils.h>
#include <gnc-hooks.h>
#include <gnc-path.h>
#include <gnc-prefs.h>
#include <gnc-prefs-utils.h>
#include <gnc-gnome-utils.h>
#include <gnc-gsettings.h>
#include <gnc-report.h>
#include <gnc-session.h>
#include <gnc-splash.h>
#include <gnc-version.h>
}

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

/* Change the following to have a console window attached to GnuCash
 * for displaying stdout and stderr on Windows.
 */
#define __MSWIN_CONSOLE__ 0

#include <libintl.h>
#include <locale.h>

#ifdef MAC_INTEGRATION
#  include <Foundation/Foundation.h>
#endif

/* GNC_VCS is defined whenever we're building from an svn/svk/git/bzr tree */
#ifdef GNC_VCS
static int is_development_version = TRUE;
#else
static int is_development_version = FALSE;
#define GNC_VCS ""
#endif

static gchar *userdata_migration_msg = NULL;

static void
gnc_print_unstable_message(void)
{
    if (!is_development_version) return;

    g_print("\n\n%s\n%s\n%s %s\n%s %s\n",
            _("This is a development version. It may or may not work."),
            _("Report bugs and other problems to gnucash-devel@gnucash.org"),
	    /* Translators: An URLs follows*/
            _("You can also lookup and file bug reports at"), PACKAGE_BUGREPORT,
	    /* Translators: An URLs follows*/
           _("To find the last stable version, please refer to"), PACKAGE_URL);
}

static gboolean
try_load_config_array(const gchar *fns[])
{
    gchar *filename;
    int i;

    for (i = 0; fns[i]; i++)
    {
        filename = gnc_build_userdata_path(fns[i]);
        if (gfec_try_load(filename))
        {
            g_free(filename);
            return TRUE;
        }
        g_free(filename);
    }
    return FALSE;
}

static void
update_message(const gchar *msg)
{
    gnc_update_splash_screen(msg, GNC_SPLASH_PERCENTAGE_UNKNOWN);
    g_message("%s", msg);
}

static void
load_system_config(void)
{
    static int is_system_config_loaded = FALSE;
    gchar *system_config_dir;
    gchar *system_config;

    if (is_system_config_loaded) return;

    update_message("loading system configuration");
    system_config_dir = gnc_path_get_pkgsysconfdir();
    system_config = g_build_filename(system_config_dir, "config", NULL);
    is_system_config_loaded = gfec_try_load(system_config);
    g_free(system_config_dir);
    g_free(system_config);
}

static void
load_user_config(void)
{
    /* Don't continue adding to this list. When 3.0 rolls around bump
       the 2.4 files off the list. */
    static const gchar *saved_report_files[] =
    {
        SAVED_REPORTS_FILE, SAVED_REPORTS_FILE_OLD_REV, NULL
    };
    static const gchar *stylesheet_files[] = { "stylesheets-2.0", NULL};
    static int is_user_config_loaded = FALSE;

    if (is_user_config_loaded)
        return;
    else is_user_config_loaded = TRUE;

    update_message("loading user configuration");
    {
        gchar *config_filename;
        config_filename = g_build_filename (gnc_userconfig_dir (),
                                                "config-user.scm", (char *)NULL);
        gfec_try_load(config_filename);
        g_free(config_filename);
    }

    update_message("loading saved reports");
    try_load_config_array(saved_report_files);
    update_message("loading stylesheets");
    try_load_config_array(stylesheet_files);
}

static void
inner_main_add_price_quotes(void *data, int argc, char **argv)
{
    const char* add_quotes_file = static_cast<const char*>(data);
    SCM mod, add_quotes, scm_book, scm_result = SCM_BOOL_F;
    QofSession *session = NULL;

    scm_c_eval_string("(debug-set! stack 200000)");

    mod = scm_c_resolve_module("gnucash price-quotes");
    scm_set_current_module(mod);

    gnc_prefs_init ();
    qof_event_suspend();
    scm_c_eval_string("(gnc:price-quotes-install-sources)");

    if (!gnc_quote_source_fq_installed())
    {
        g_print("%s", _("No quotes retrieved. Finance::Quote isn't "
                        "installed properly.\n"));
        goto fail;
    }

    add_quotes = scm_c_eval_string("gnc:book-add-quotes");
    session = gnc_get_current_session();
    if (!session) goto fail;

    qof_session_begin(session, add_quotes_file, FALSE, FALSE, FALSE);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR) goto fail;

    qof_session_load(session, NULL);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR) goto fail;

    scm_book = gnc_book_to_scm(qof_session_get_book(session));
    scm_result = scm_call_2(add_quotes, SCM_BOOL_F, scm_book);

    qof_session_save(session, NULL);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR) goto fail;

    qof_session_destroy(session);
    if (!scm_is_true(scm_result))
    {
        g_warning("Failed to add quotes to %s.", add_quotes_file);
        goto fail;
    }

    qof_event_resume();
    gnc_shutdown(0);
    return;
fail:
    if (session)
    {
        if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR)
            g_warning("Session Error: %s",
                      qof_session_get_error_message(session));
        qof_session_destroy(session);
    }
    qof_event_resume();
    gnc_shutdown(1);
}

int
main(int argc, char ** argv)
{
    Gnucash::Base application;

    application.parse_command_line (&argc, &argv);
    application.start ();

    auto quotes_file = application.get_quotes_file ();
    if (quotes_file)
        scm_boot_guile (argc, argv, inner_main_add_price_quotes, (void *)quotes_file);

    exit(0);  /* never reached */
}
