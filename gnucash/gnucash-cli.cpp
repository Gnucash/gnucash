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

#include "gnucash-base.hpp"

extern "C" {
#include <gnc-engine-guile.h>
#include <gnc-prefs-utils.h>
#include <gnc-gnome-utils.h>
#include <gnc-session.h>
}

#include <boost/locale.hpp>
#include <iostream>

namespace bl = boost::locale;

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = GNC_MOD_GUI;

static void
inner_main_add_price_quotes(void *data, [[maybe_unused]] int argc, [[maybe_unused]] char **argv)
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
