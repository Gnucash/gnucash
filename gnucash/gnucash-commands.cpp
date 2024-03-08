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
#include "gnucash-core-app.hpp"
#include "gnc-ui-util.h"
#include "gnc-path.h"

#include <gnc-filepath-utils.h>
#include <gnc-engine-guile.h>
#include <gnc-prefs.h>
#include <gnc-prefs-utils.h>
#include <gnc-session.h>
#include <qoflog.h>

#include <boost/locale.hpp>
#include <boost/filesystem.hpp>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <gnc-report.h>
#include <gnc-quotes.hpp>

#ifdef HAVE_PYTHON_H
#include <Python.h>
#endif

namespace bl = boost::locale;

static std::string empty_string{};

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

#ifdef _WIN32
struct ConsoleStruct
{
public:
    bool has_ansi () { return m_has_ansi; };
private:
    HANDLE m_stdoutHandle = INVALID_HANDLE_VALUE;
    DWORD m_outModeInit = 0;
    bool m_has_ansi = false;
    ConsoleStruct () : m_stdoutHandle {GetStdHandle(STD_OUTPUT_HANDLE)}
    {
        if (m_stdoutHandle != INVALID_HANDLE_VALUE && GetConsoleMode(m_stdoutHandle, &m_outModeInit))
        {
            SetConsoleMode (m_stdoutHandle, m_outModeInit | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
            m_has_ansi = true;
        }
    }
    ~ConsoleStruct ()
    {
        if (m_stdoutHandle == INVALID_HANDLE_VALUE)
            return;
        printf ("\x1b[0m");
        SetConsoleMode (m_stdoutHandle, m_outModeInit);
    }
} console_ansi;
#else
struct ConsoleStruct
{
    bool has_ansi () { return true; };
} console_ansi;
#endif

static int
cleanup_and_exit_with_failure (QofSession *session)
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
    return 1;
}

static void gnc_shutdown_cli (int exit_status)
{
    gnc_hook_run (HOOK_SHUTDOWN, NULL);
    gnc_engine_shutdown ();
    exit (exit_status);
}

/* scm_boot_guile doesn't expect to return, so call shutdown ourselves here */
static void
scm_cleanup_and_exit_with_failure (QofSession *session)
{
    cleanup_and_exit_with_failure (session);
    gnc_shutdown_cli (1);
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

static std::string
get_line (const char* prompt)
{
    std::string rv;
    std::cout << (console_ansi.has_ansi() ? "\x1b[1;33m" : "") << prompt
              << (console_ansi.has_ansi() ? "\x1b[m" : "");
    if (!std::getline (std::cin, rv))
        gnc_shutdown_cli (1);
    return rv;
}

static std::string
get_choice (const char* prompt, const std::vector<std::string> choices)
{
    std::string response;
    do
    {
        response = get_line (prompt);
    }
    while (std::none_of (choices.begin(), choices.end(),
                         [&response](auto& choice) { return choice == response; } ));
    return response;
}

struct scripting_args
{
    const boost::optional<std::string>& script;
    bool interactive;
};

// when guile or python script/interactive session succeed
static void
cleanup_and_exit_with_save ()
{
    if (gnc_current_session_exist())
    {
        auto session = gnc_get_current_session();
        auto book = gnc_get_current_book();
        auto is_yes = [] (const char* prompt)
        {
            auto s = get_choice (prompt, {"Y","y","N","n"});
            return s == "Y" || s == "y";
        };

        std::cerr << _("Warning: session was not cleared.") << std::endl;

        if (!qof_book_session_not_saved (book))
            std::cerr << _("Please don't forget to clear session before shutdown.") << std::endl;
        else if (qof_book_is_readonly (book))
            std::cerr << _("Book is readonly. Unsaved changes will be lost.") << std::endl;
        else if (is_yes (_("There are unsaved changes. Save before clearing [YN]?")))
            qof_session_save (session, report_session_percentage);

        gnc_clear_current_session ();
    }
    gnc_shutdown_cli (0);
}

/* Don't try to use std::string& for the members of the following struct, it
 * results in the values getting corrupted as it passes through initializing
 * Scheme when compiled with Clang.
 */
struct run_report_args {
    const std::string& file_to_load;
    const std::string& run_report;
    const std::string& export_type;
    const std::string& output_file;
};

static inline void
write_report_file (const char *html, const char* file)
{
    if (!file || !html || !*html) return;
    auto ofs{gnc_open_filestream(file)};
    if (!ofs)
    {
        std::cerr << "Failed to open file " << file << " for writing\n";
        return;
    }
    ofs << html << std::endl;
    // ofs destructor will close the file
}

static QofSession*
load_file (const std::string& file_to_load, bool open_readwrite)
{
    PINFO ("Loading %s %s", file_to_load.c_str(), open_readwrite ? "(r/w)" : "(readonly)");
    auto session = gnc_get_current_session();
    if (!session)
        gnc_shutdown_cli (1);

    auto mode = open_readwrite ? SESSION_NORMAL_OPEN : SESSION_READ_ONLY;
    while (true)
    {
        qof_session_begin (session, file_to_load.c_str(), mode);
        auto io_err = qof_session_get_error (session);
        switch (io_err)
        {
        case ERR_BACKEND_NO_ERR:
            qof_session_load (session, report_session_percentage);
            return session;
        case ERR_BACKEND_LOCKED:
        {
            // Translators: [R] [U] and [A] are responses and must not be translated.
            auto response = get_choice (_("File Locked. Open [R]eadonly, [U]nlock or [A]bort?"),
                                        {"R","r","U","u","A","a"});
            if (response == "R" || response == "r")
                mode = SESSION_READ_ONLY;
            else if (response == "U" || response == "u")
                mode = SESSION_BREAK_LOCK;
            else if (response == "A" || response == "a")
                gnc_shutdown_cli (1);
            break;
        }
        case ERR_BACKEND_READONLY:
            std::cerr << _("File is readonly. Cannot open read-write") << std::endl;
            mode = SESSION_READ_ONLY;
            break;
        default:
            std::cerr << _("Unknown error. Abort.") << std::endl;
            scm_cleanup_and_exit_with_failure (session);
        }
    }
}

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
    Gnucash::gnc_load_scm_config ([](const gchar *msg){ PINFO ("%s", msg); });
    gnc_prefs_init ();
    qof_event_suspend ();

    auto datafile = args->file_to_load.c_str();
    auto check_report_cmd = scm_c_eval_string ("gnc:cmdline-check-report");
    auto get_report_cmd = scm_c_eval_string ("gnc:cmdline-get-report-id");
    auto run_export_cmd = scm_c_eval_string ("gnc:cmdline-template-export");
    /* We generally insist on using scm_from_utf8_string() throughout GnuCash
     * because all GUI-sourced strings and all file-sourced strings are encoded
     * that way. In this case, though, the input is coming from a shell window
     * and Microsoft Windows shells are generally not capable of entering UTF8
     * so it's necessary here to allow guile to read the locale and interpret
     * the input in that encoding.
     */
    auto report = scm_from_locale_string (args->run_report.c_str());
    auto type = !args->export_type.empty() ?
                scm_from_locale_string (args->export_type.c_str()) : SCM_BOOL_F;

    if (scm_is_false (scm_call_2 (check_report_cmd, report, type)))
        scm_cleanup_and_exit_with_failure (nullptr);

    PINFO ("Loading datafile %s...\n", datafile);

    auto session = load_file (args->file_to_load, false);

    if (!args->export_type.empty())
    {
        SCM retval = scm_call_2 (run_export_cmd, report, type);
        SCM query_result = scm_c_eval_string ("gnc:html-document?");
        SCM get_export_string = scm_c_eval_string ("gnc:html-document-export-string");
        SCM get_export_error = scm_c_eval_string ("gnc:html-document-export-error");

        if (scm_is_false (scm_call_1 (query_result, retval)))
        {
            std::cerr << _("This report must be upgraded to \
return a document object with export-string or export-error.") << std::endl;
            scm_cleanup_and_exit_with_failure (nullptr);
        }

        SCM export_string = scm_call_1 (get_export_string, retval);
        SCM export_error = scm_call_1 (get_export_error, retval);

        if (scm_is_string (export_string))
        {
            auto output = scm_to_utf8_string (export_string);
            if (!args->output_file.empty())
            {
                write_report_file(output, args->output_file.c_str());
            }
            else
            {
                std::cout << output << std::endl;
            }
            g_free (output);
        }
        else if (scm_is_string (export_error))
        {
            auto err = scm_to_utf8_string (export_error);
            std::cerr << err << std::endl;
            g_free (err);
            scm_cleanup_and_exit_with_failure (nullptr);
        }
        else
        {
            std::cerr << _("This report must be upgraded to \
return a document object with export-string or export-error.") << std::endl;
            scm_cleanup_and_exit_with_failure (nullptr);
        }
    }
    else
    {
        SCM id = scm_call_1(get_report_cmd, report);

        if (scm_is_false (id))
            scm_cleanup_and_exit_with_failure (nullptr);
        char *html, *errmsg;

        if (gnc_run_report_with_error_handling (scm_to_int(id), &html, &errmsg))
        {
            if (!args->output_file.empty())
            {
                write_report_file(html, args->output_file.c_str());
            }
            else
            {
                std::cout << html << std::endl;
            }
            g_free (html);
        }
        else
        {
            std::cerr << errmsg << std::endl;
            g_free (errmsg);
        }
    }

    qof_session_destroy (session);

    qof_event_resume ();
    gnc_shutdown_cli (0);
    return;
}


struct show_report_args {
    const std::string& file_to_load;
    const std::string& show_report;
};

static void
scm_report_show (void *data,
                [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    auto args = static_cast<show_report_args*>(data);

    scm_c_eval_string("(debug-set! stack 200000)");
    scm_c_use_module ("gnucash utilities");
    scm_c_use_module ("gnucash app-utils");
    scm_c_use_module ("gnucash reports");
    gnc_report_init ();
    Gnucash::gnc_load_scm_config ([](const gchar *msg){ PINFO ("%s", msg); });

    if (!args->file_to_load.empty())
    {
        auto datafile = args->file_to_load.c_str();
        PINFO ("Loading datafile %s...\n", datafile);
        [[maybe_unused]] auto session = load_file (args->file_to_load, false);
    }

    scm_call_2 (scm_c_eval_string ("gnc:cmdline-report-show"),
                scm_from_locale_string (args->show_report.c_str ()),
                scm_current_output_port ());
    gnc_shutdown_cli (0);
    return;
}


static void
scm_report_list ([[maybe_unused]] void *data,
                 [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    scm_c_eval_string("(debug-set! stack 200000)");
    scm_c_use_module ("gnucash app-utils");
    scm_c_use_module ("gnucash reports");
    gnc_report_init ();
    Gnucash::gnc_load_scm_config ([](const gchar *msg){ PINFO ("%s", msg); });

    scm_call_1 (scm_c_eval_string ("gnc:cmdline-report-list"),
                scm_current_output_port ());
    gnc_shutdown_cli (0);
    return;
}

int
Gnucash::check_finance_quote (void)
{
    gnc_prefs_init ();
    try
    {
        GncQuotes quotes;
        std::cout << bl::format (bl::translate ("Found Finance::Quote version {1}.")) % quotes.version() << "\n";
        std::cout << bl::translate ("Finance::Quote sources:\n");
        int count{0};
        const auto width{12};
        for (auto source : quotes.sources())
        {
            auto mul{source.length() / width + 1};
            count += mul;
            if (count > 6)
            {
                count = mul;
                std::cout << "\n";
            }
            std::cout << std::setw(mul * (width + 1)) << std::left << source;
        }
        std::cout << std::endl;
        return 0;
    }
    catch (const GncQuoteException& err)
    {
        std::cout << err.what() << std::endl;
        return 1;
    }
}

int
Gnucash::add_quotes (const bo_str& uri)
{
    gnc_prefs_init ();
    qof_event_suspend();

    auto session = load_file (*uri, true);

    try
    {
        GncQuotes quotes;
        std::cout << bl::format (bl::translate ("Found Finance::Quote version {1}.")) % quotes.version() << std::endl;
        auto quote_sources = quotes.sources();
        gnc_quote_source_set_fq_installed (quotes.version().c_str(), quote_sources);
        quotes.fetch(qof_session_get_book(session));
        if (quotes.had_failures())
            std::cerr << quotes.report_failures() << std::endl;
    }
    catch (const GncQuoteException& err)
    {
        std::cerr << bl::translate("Price retrieval failed: ") << err.what() << std::endl;
    }
    qof_session_save(session, NULL);
    if (qof_session_get_error(session) != ERR_BACKEND_NO_ERR)
        return cleanup_and_exit_with_failure (session);

    qof_session_destroy(session);
    qof_event_resume();
    return 0;
}

int
Gnucash::report_quotes (const char* source, const StrVec& commodities, bool verbose)
{
    gnc_prefs_init();
    try
    {
        GncQuotes quotes;
        quotes.report(source, commodities, verbose);
        if (quotes.had_failures())
            std::cerr << quotes.report_failures() << std::endl;
    }
    catch (const GncQuoteException& err)
    {
        std::cerr << bl::translate("Price retrieval failed: ") << err.what() << std::endl;
        return -1;
   }
    return 0;
}

int
Gnucash::run_report (const bo_str& file_to_load,
                     const bo_str& run_report,
                     const bo_str& export_type,
                     const bo_str& output_file)
{
    auto args = run_report_args { file_to_load ? *file_to_load : empty_string,
                                  run_report ? *run_report : empty_string,
                                  export_type ? *export_type : empty_string,
                                  output_file ? *output_file : empty_string };
    if (run_report && !run_report->empty())
        scm_boot_guile (0, nullptr, scm_run_report, &args);

    return 0;
}

int
Gnucash::report_show (const bo_str& file_to_load,
                      const bo_str& show_report)
{
    auto args = show_report_args { file_to_load ? *file_to_load : empty_string,
                                   show_report ? *show_report : empty_string };
    if (show_report && !show_report->empty())
        scm_boot_guile (0, nullptr, scm_report_show, &args);

    return 0;
}

int
Gnucash::report_list (void)
{
    scm_boot_guile (0, nullptr, scm_report_list, NULL);
    return 0;
}

// scripting code follows:

static void
run_guile_cli (void *data, [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
{
    auto args = static_cast<scripting_args*>(data);
    if (args->script)
    {
        PINFO ("Running script from %s... ", args->script->c_str());
        scm_c_primitive_load (args->script->c_str());
    }
    if (args->interactive)
    {
        std::cout << _("Welcome to Gnucash Interactive Guile Session") << std::endl;
        std::vector<const char*> modules =
            { "gnucash core-utils", "gnucash engine", "gnucash app-utils", "gnucash report",
              "system repl repl", "ice-9 readline" };
        auto show_and_load = [](const auto& mod)
        {
            std::cout << bl::format ("(use-modules ({1}))") % mod << std::endl;
            scm_c_use_module (mod);
        };
        std::for_each (modules.begin(), modules.end(), show_and_load);
        scm_c_eval_string ("(activate-readline)");
        scm_c_eval_string ("(start-repl)");
    }
    cleanup_and_exit_with_save ();
}


#ifdef HAVE_PYTHON_H
static void
python_cleanup_and_exit (PyConfig& config, PyStatus& status)
{
    if (qof_book_session_not_saved (gnc_get_current_book()))
        std::cerr << _("Book is readonly. Unsaved changes will be lost.") << std::endl;
    gnc_clear_current_session ();

    PyConfig_Clear(&config);
    if (status.err_msg && *status.err_msg)
        std::cerr << bl::format (_("Python Config failed with error {1}")) % status.err_msg
                  << std::endl;
    gnc_shutdown_cli (status.exitcode);
}

static void
run_python_cli (int argc, char **argv, scripting_args* args)
{
    PyConfig config;
    PyConfig_InitPythonConfig(&config);

    PyStatus status = PyConfig_SetBytesArgv(&config, argc, argv);
    if (PyStatus_Exception(status))
        python_cleanup_and_exit (config, status);

    status = Py_InitializeFromConfig(&config);
    if (PyStatus_Exception(status))
        python_cleanup_and_exit (config, status);

    PyConfig_Clear(&config);

    if (args->script)
    {
        auto script_filename = args->script->c_str();
        PINFO ("Running python script %s...", script_filename);
        auto fp = fopen (script_filename, "rb");
        if (!fp)
        {
            std::cerr << bl::format (_("Unable to load Python script {1}")) % script_filename
                      << std::endl;
            python_cleanup_and_exit (config, status);
        }
        else if (PyRun_SimpleFileEx (fp, script_filename, 1) != 0)
        {
            std::cerr << bl::format (_("Python script {1} execution failed.")) % script_filename
                      << std::endl;
            python_cleanup_and_exit (config, status);
        }
    }
    if (args->interactive)
    {
        std::cout << _("Welcome to Gnucash Interactive Python Session") << std::endl;
        PyRun_InteractiveLoop (stdin, "foo");
    }
    Py_Finalize();
    cleanup_and_exit_with_save ();
}
#endif

static const std::vector<const char*> valid_schemes = { "file", "mysql", "postgres" };

static bool
is_valid_uri (const bo_str& uri)
{
    if (!uri)
        return false;

    if (boost::filesystem::is_regular_file (*uri))
        return true;

    auto scheme = g_uri_parse_scheme (uri->c_str());

    if (!scheme)
        return false;

    auto rv = std::any_of (valid_schemes.begin(), valid_schemes.end(),
                           [&scheme](const char* str) { return !g_strcmp0(str, scheme); });

    g_free (scheme);
    return rv;
}

int
Gnucash::run_scripting (std::vector<const char*> newArgv,
                        const bo_str& file_to_load,
                        std::string& language,
                        const bo_str& script,
                        bool open_readwrite,
                        bool interactive)
{
    std::vector<std::string> errors;
    static const std::vector<std::string> languages = { "guile", "python" };

    if (open_readwrite && !file_to_load)
        errors.push_back (_ ("--readwrite: missing datafile!"));

    if (script && (!boost::filesystem::is_regular_file (*script)))
        errors.push_back ((bl::format (_("--script: {1} is not a file")) % *script).str());

    if (std::none_of (languages.begin(), languages.end(),
                      [&language](auto& lang){ return language == lang; }))
        errors.push_back (_ ("--language: must be 'python' or 'guile'"));
#ifndef HAVE_PYTHON_H
    else if (language == "python")
        errors.push_back (_("--language: python wasn't compiled in this build"));
#endif

    if (!errors.empty())
    {
        std::cerr << _("Errors parsing arguments:") << std::endl;
        auto to_console = [](const auto& str){ std::cerr << str << std::endl; };
        std::for_each (errors.begin(), errors.end(), to_console);
        gnc_shutdown_cli (1);
    }

    gnc_prefs_init ();
    gnc_ui_util_init();
    if (is_valid_uri (file_to_load))
        [[maybe_unused]] auto session = load_file (*file_to_load, open_readwrite);

    scripting_args args { script, interactive };
    if (language == "guile")
    {
        scm_boot_guile (newArgv.size(), (char**)newArgv.data(), run_guile_cli, &args);
        return 0;               // never reached...
    }
#ifdef HAVE_PYTHON_H
    else if (language == "python")
    {
        run_python_cli (newArgv.size(), (char**)newArgv.data(), &args);
        return 0;               // never reached...
    }
#endif

    return 0;                   // never reached
}
