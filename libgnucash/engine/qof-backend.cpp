/********************************************************************\
 * qofbackend.c -- utility routines for dealing with the data backend  *
 * Copyright (C) 2000 Linas Vepstas <linas@linas.org>               *
 * Copyright (C) 2004-5 Neil Williams <linux@codehelp.co.uk>        *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

extern "C"
{

#include <config.h>
#include "qof.h"
#include <gnc-path.h>
#include "gncla-dir.h"
}

#include <string>
#include <algorithm>
#include <vector>

#include "qof-backend.hpp"

G_GNUC_UNUSED static QofLogModule log_module = QOF_MOD_BACKEND;

#define QOF_CONFIG_DESC    "desc"
#define QOF_CONFIG_TIP     "tip"

/* *******************************************************************\
 * error handling                                                   *
\********************************************************************/

GModuleVec QofBackend::c_be_registry{};

void
QofBackend::set_error(QofBackendError err)
{
    /* use stack-push semantics. Only the earliest error counts */
    if (m_last_err != ERR_BACKEND_NO_ERR) return;
    m_last_err = err;
}

QofBackendError
QofBackend::get_error()
{
    /* use 'stack-pop' semantics */
    auto err = m_last_err;
    m_last_err = ERR_BACKEND_NO_ERR;
    return err;
}

bool
QofBackend::check_error()
{
    return m_last_err != ERR_BACKEND_NO_ERR;
}

void
QofBackend::set_message (std::string&& msg)
{
    m_error_msg = msg;
}

const std::string&&
QofBackend::get_message ()
{
    return std::move(m_error_msg);
}

bool
QofBackend::register_backend(const char* directory, const char* module_name)
{
    if (!g_module_supported ())
    {
        PWARN("Modules not supported.");
        return false;
    }

    auto absdir = directory;
    if (!absdir || !g_path_is_absolute(absdir))
        absdir = gnc_path_get_pkglibdir ();
    auto fullpath = g_module_build_path (absdir, module_name);
/* Darwin modules can have either .so or .dylib for a suffix */
    if (!g_file_test (fullpath, G_FILE_TEST_EXISTS) &&
        g_strcmp0 (G_MODULE_SUFFIX, "so") == 0)
    {
        auto modname = g_strdup_printf ("lib%s.dylib", module_name);
        g_free (fullpath);
        fullpath = g_build_filename (absdir, modname, NULL);
        g_free (modname);
    }
    auto backend = g_module_open (fullpath, G_MODULE_BIND_LAZY);
    g_free (fullpath);
    if (!backend)
    {
        PINFO ("%s: %s\n", PROJECT_NAME, g_module_error ());
        return false;
    }
    void (*module_init_func)(void);
    if (g_module_symbol (backend, "qof_backend_module_init",
                         reinterpret_cast<void**>(&module_init_func)))
        module_init_func ();

    g_module_make_resident (backend);
    c_be_registry.push_back(backend);
    return TRUE;
}

void
QofBackend::release_backends()
{
    for (auto backend : c_be_registry)
    {
        void (*module_finalize_func)(void);
        if (g_module_symbol(backend, "qof_backend_module_finalize",
                            reinterpret_cast<void**>(&module_finalize_func)))
            module_finalize_func();
    }
}
/***********************************************************************/
QofBackendError
qof_backend_get_error (QofBackend* qof_be)
{
    if (qof_be == nullptr) return ERR_BACKEND_NO_ERR;
    return ((QofBackend*)qof_be)->get_error();
}

void
qof_backend_set_error (QofBackend* qof_be, QofBackendError err)
{
    if (qof_be == nullptr) return;
    ((QofBackend*)qof_be)->set_error(err);
}

gboolean
qof_backend_can_rollback (QofBackend* qof_be)
{
    if (qof_be == nullptr) return FALSE;
    return true;
}

void
qof_backend_rollback_instance (QofBackend* qof_be, QofInstance* inst)
{
    if (qof_be == nullptr) return;
    ((QofBackend*)qof_be)->rollback(inst);
}

gboolean
qof_load_backend_library (const char *directory, const char* module_name)
{
    return QofBackend::register_backend(directory, module_name);
}

void
qof_finalize_backend_libraries(void)
{
    QofBackend::release_backends();
}

/************************* END OF FILE ********************************/
