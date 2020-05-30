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

#include <libguile.h>
#include <guile-mappings.h>
#ifdef __MINGW32__
#include <Windows.h>
#include <fcntl.h>
#endif

#include "gnucash-commands.hpp"

extern "C" {
#include <gnc-engine-guile.h>
#include <gnc-prefs.h>
#include <gnc-prefs-utils.h>
#include <gnc-gnome-utils.h>
#include <gnc-session.h>
}

#include <boost/locale.hpp>
#include <iostream>

namespace bl = boost::locale;

struct run_report_args {
    const std::string& file_to_load;
    const std::string& run_report;
    const std::string& export_type;
    const std::string& output_file;
};

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void
scm_add_quotes(void *data, [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
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
        std::cerr << bl::translate ("No quotes retrieved. Finance::Quote isn't "
                                    "installed properly.") << "\n";
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

static void
report_session_percentage (const char *message, double percent)
{
    static double previous = 0.0;
    if ((percent - previous) < 5.0)
        return;
    fprintf (stderr, "\r%3.0f%% complete...", percent);
    previous = percent;
    return;
}

static void
scm_run_report (void *data,
                [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    auto args = static_cast<run_report_args*>(data);
    QofSession *session = NULL;
    SCM cmdline, report, type, file;
    const gchar *datafile;

    scm_c_eval_string("(debug-set! stack 200000)");
    scm_c_use_module ("gnucash utilities");
    scm_c_use_module ("gnucash app-utils");
    scm_c_use_module ("gnucash report");
    scm_c_use_module ("gnucash reports");

    // gnc_report_init ();
    // load_system_config();
    // load_user_config();
    gnc_prefs_init ();
    qof_event_suspend ();
    datafile = args->file_to_load.c_str();

    cmdline = scm_c_eval_string ("gnc:cmdline-run-report");
    report = scm_from_utf8_string (args->run_report.c_str());

    type = !args->export_type.empty() ? scm_from_utf8_string (args->export_type.c_str()) : SCM_BOOL_F;
    file = !args->output_file.empty() ? scm_from_utf8_string (args->output_file.c_str()) : SCM_BOOL_F;

    /* dry-run? is #t: try report, check validity of options */
    if (scm_is_false (scm_call_4 (cmdline, report, type, file, SCM_BOOL_T)))
        goto fail;

    fprintf (stderr, "Loading datafile %s...\n", datafile);

    session = gnc_get_current_session ();
    if (!session) goto fail;

    qof_session_begin (session, datafile, TRUE, FALSE, FALSE);
    if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR) goto fail;

    qof_session_load (session, report_session_percentage);
    if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR) goto fail;

    fprintf (stderr, "\n");

    /* dry-run? is #f: run the report */
    scm_call_4 (cmdline, report, type, file, SCM_BOOL_F);

    qof_session_end (session);
    if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR) goto fail;

    qof_session_destroy (session);

    qof_event_resume ();
    gnc_shutdown (0);
    return;
fail:
    if (session)
    {
        if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR)
            g_warning ("Session Error: %d %s",
                       qof_session_get_error (session),
                       qof_session_get_error_message (session));
        qof_session_destroy (session);
    }
    qof_event_resume ();
    gnc_shutdown (1);
}

int
Gnucash::add_quotes (std::string &uri)
{
    if (not uri.empty())
        scm_boot_guile (0, nullptr, scm_add_quotes, (void *)uri.c_str());

    return 0;
}

int
Gnucash::run_report (const std::string& file_to_load,
                     const std::string& run_report,
                     const std::string& export_type,
                     const std::string& output_file)
{
    auto args = run_report_args { file_to_load, run_report,
                                  export_type, output_file };
    if (not run_report.empty())
        scm_boot_guile (0, nullptr, scm_run_report, &args);

    return 0;
}
