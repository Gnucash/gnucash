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
#include <gnc-report.h>
#include <gnc-session.h>
#include <qoflog.h>
}

#include <boost/locale.hpp>
#include <iostream>

namespace bl = boost::locale;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void
scm_cleanup_and_exit_with_failure (QofSession *session)
{
    if (session)
    {
        auto error{qof_session_get_error (session)};
        if (error != ERR_BACKEND_NO_ERR)
        {
            if (error == ERR_BACKEND_LOCKED)
                PERR ("File is locked, won't open.");
            else
                PERR ("Session Error: %s\n",
                      qof_session_get_error_message (session));
        }
        qof_session_destroy (session);
    }
    qof_event_resume();
    gnc_shutdown (1);
}

static void
scm_add_quotes(void *data, [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    auto add_quotes_file = static_cast<const std::string*>(data);

    scm_c_eval_string("(debug-set! stack 200000)");

    auto mod = scm_c_resolve_module("gnucash price-quotes");
    scm_set_current_module(mod);

    gnc_prefs_init ();
    qof_event_suspend();
    scm_c_eval_string("(gnc:price-quotes-install-sources)");

    if (!gnc_quote_source_fq_installed())
    {
        std::cerr << bl::translate ("No quotes retrieved. Finance::Quote isn't "
                                    "installed properly.") << "\n";
        scm_cleanup_and_exit_with_failure (nullptr);
    }

    auto add_quotes = scm_c_eval_string("gnc:book-add-quotes");
    auto session = gnc_get_current_session();
    if (!session)
        scm_cleanup_and_exit_with_failure (session);

    qof_session_begin(session, add_quotes_file->c_str(), SESSION_NORMAL_OPEN);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR)
        scm_cleanup_and_exit_with_failure (session);

    qof_session_load(session, NULL);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR)
        scm_cleanup_and_exit_with_failure (session);

    auto scm_book = gnc_book_to_scm(qof_session_get_book(session));
    auto scm_result = scm_call_2(add_quotes, SCM_BOOL_F, scm_book);

    qof_session_save(session, NULL);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR)
        scm_cleanup_and_exit_with_failure (session);

    qof_session_destroy(session);
    if (!scm_is_true(scm_result))
    {
        PERR ("Failed to add quotes to %s.", add_quotes_file->c_str());
        scm_cleanup_and_exit_with_failure (session);
    }

    qof_event_resume();
    gnc_shutdown(0);
    return;
}

static void
report_session_percentage (const char *message, double percent)
{
    static double previous = 0.0;
    if ((percent - previous) < 5.0)
        return;
    PINFO ("\r%3.0f%% complete...", percent);
    previous = percent;
    return;
}

struct run_report_args {
    const std::string& file_to_load;
    const std::string& run_report;
    const std::string& export_type;
    const std::string& output_file;
};

static void
scm_run_report (void *data,
                [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    auto args = static_cast<run_report_args*>(data);

    scm_c_eval_string("(debug-set! stack 200000)");
    scm_c_use_module ("gnucash utilities");
    scm_c_use_module ("gnucash app-utils");
    scm_c_use_module ("gnucash reports");

    gnc_report_init ();
    // load_system_config();
    // load_user_config();
    gnc_prefs_init ();
    qof_event_suspend ();

    auto datafile = args->file_to_load.c_str();
    auto cmdline = scm_c_eval_string ("gnc:cmdline-run-report");
    auto report = scm_from_utf8_string (args->run_report.c_str());
    auto type = !args->export_type.empty() ?
                scm_from_utf8_string (args->export_type.c_str()) : SCM_BOOL_F;
    auto file = !args->output_file.empty() ?
                scm_from_utf8_string (args->output_file.c_str()) : SCM_BOOL_F;

    /* dry-run? is #t: try report, check validity of options */
    if (scm_is_false (scm_call_4 (cmdline, report, type, file, SCM_BOOL_T)))
        scm_cleanup_and_exit_with_failure (nullptr);

    PINFO ("Loading datafile %s...\n", datafile);

    auto session = gnc_get_current_session ();
    if (!session)
        scm_cleanup_and_exit_with_failure (session);

    qof_session_begin (session, datafile, SESSION_READ_ONLY);
    if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR)
        scm_cleanup_and_exit_with_failure (session);

    qof_session_load (session, report_session_percentage);
    if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR)
        scm_cleanup_and_exit_with_failure (session);

    /* dry-run? is #f: run the report */
    scm_call_4 (cmdline, report, type, file, SCM_BOOL_F);

    qof_session_end (session);
    if (qof_session_get_error (session) != ERR_BACKEND_NO_ERR)
        scm_cleanup_and_exit_with_failure (session);

    qof_session_destroy (session);

    qof_event_resume ();
    gnc_shutdown (0);
    return;
}

int
Gnucash::add_quotes (const bo_str& uri)
{
    if (uri && !uri->empty())
        scm_boot_guile (0, nullptr, scm_add_quotes, (void *)&(*uri));

    return 0;
}

int
Gnucash::run_report (const bo_str& file_to_load,
                     const bo_str& run_report,
                     const bo_str& export_type,
                     const bo_str& output_file)
{
    auto args = run_report_args { file_to_load ? *file_to_load : std::string(),
                                  run_report ? *run_report : std::string(),
                                  export_type ? *export_type : std::string(),
                                  output_file ? *output_file : std::string() };
    if (run_report && !run_report->empty())
        scm_boot_guile (0, nullptr, scm_run_report, &args);

    return 0;
}
