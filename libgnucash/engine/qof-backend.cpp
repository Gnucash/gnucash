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


#include <config.h>
#include "qof.h"
#include <gnc-path.h>
#include "gncla-dir.h"

#include <string>
#include <algorithm>
#include <vector>
#include <unordered_map>

#include "qof-backend.hpp"

G_GNUC_UNUSED static QofLogModule log_module = QOF_MOD_BACKEND;

#define QOF_CONFIG_DESC    "desc"
#define QOF_CONFIG_TIP     "tip"

/* *******************************************************************\
 * error handling                                                   *
\********************************************************************/

GModuleVec QofBackend::c_be_registry{};

void
QofBackend::commit(QofInstance* instance)
{
    if (qof_instance_is_dirty(instance))
        qof_instance_mark_clean(instance);
}

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
    auto pkgdir = gnc_path_get_pkglibdir ();
    if (!absdir || !g_path_is_absolute(absdir))
        absdir = pkgdir;
    auto fullpath = g_module_build_path (absdir, module_name);
/* Darwin modules can have either .so or .dylib for a suffix */
    if (!g_file_test (fullpath, G_FILE_TEST_EXISTS) &&
        g_strcmp0 (G_MODULE_SUFFIX, "so") == 0)
    {
        auto modname = g_strdup_printf ("lib%s.dylib", module_name);
        g_free (fullpath);
        fullpath = g_build_filename (absdir, modname, nullptr);
        g_free (modname);
    }
    auto backend = g_module_open (fullpath, G_MODULE_BIND_LAZY);
    g_free (fullpath);
    g_free (pkgdir);
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

static const std::unordered_map<QofBackendError,const char*> qof_backend_error_map =
{
    { ERR_BACKEND_NO_ERR,      nullptr },
    { ERR_BACKEND_NO_HANDLER,  "no backend handler found for this access method (ENOSYS)" },
    { ERR_BACKEND_NO_BACKEND,  "Backend * pointer was unexpectedly null" },
    { ERR_BACKEND_BAD_URL,     "Can't parse url" },
    { ERR_BACKEND_NO_SUCH_DB,  "the named database doesn't exist" },
    { ERR_BACKEND_CANT_CONNECT,"bad dbname/login/passwd or network failure" },
    { ERR_BACKEND_CONN_LOST,   "Lost connection to server" },
    { ERR_BACKEND_LOCKED,      "in use by another user (ETXTBSY)" },
    { ERR_BACKEND_STORE_EXISTS,"File exists, data would be destroyed" },
    { ERR_BACKEND_READONLY,	   "cannot write to file/directory" },
    { ERR_BACKEND_TOO_NEW,     "file/db version newer than what we can read" },
    { ERR_BACKEND_DATA_CORRUPT,"data in db is corrupt" },
    { ERR_BACKEND_SERVER_ERR,  "error in response from server" },
    { ERR_BACKEND_ALLOC,       "internal memory allocation failure" },
    { ERR_BACKEND_PERM,        "user login successful, but no permissions to access the desired object" },
    { ERR_BACKEND_MODIFIED,    "commit of object update failed because another user has modified the object" },
    { ERR_BACKEND_MOD_DESTROY, "commit of object update failed because another user has deleted the object" },
    { ERR_BACKEND_MISC,        "undetermined error" },
    { ERR_QOF_OVERFLOW,		   "EOVERFLOW - generated by strtol or strtoll. When converting XML strings into numbers, an overflow has been detected. The XML file contains invalid data in a field that is meant to hold a signed long integer or signed long long integer." },

    /* fileio errors */
    { ERR_FILEIO_FILE_BAD_READ, "read failed or file prematurely truncated" },
    { ERR_FILEIO_FILE_EMPTY,    "file exists, is readable, but is empty" },
    { ERR_FILEIO_FILE_LOCKERR,  "mangled locks (unspecified error)" },
    { ERR_FILEIO_FILE_NOT_FOUND,"not found / no such file" },
    { ERR_FILEIO_FILE_TOO_OLD,  "file version so old we can't read it" },
    { ERR_FILEIO_UNKNOWN_FILE_TYPE,"didn't recognize the file type" },
    { ERR_FILEIO_PARSE_ERROR,   "couldn't parse the data in the file" },
    { ERR_FILEIO_BACKUP_ERROR,  "couldn't make a backup of the file" },
    { ERR_FILEIO_WRITE_ERROR,   "couldn't write to the file" },
    { ERR_FILEIO_READ_ERROR,    "Could not open the file for reading." },
    { ERR_FILEIO_NO_ENCODING,   "file does not specify encoding" },
    { ERR_FILEIO_FILE_EACCES,   "No read access permission for the given file" },
    { ERR_FILEIO_RESERVED_WRITE,"User attempt to write to a directory reserved for internal use by GnuCash" },
    { ERR_FILEIO_FILE_UPGRADE,  "file will be upgraded and not be able to be read by prior versions - warn user" },

    /* network errors */
    { ERR_NETIO_SHORT_READ,     "not enough bytes received" },
    { ERR_NETIO_WRONG_CONTENT_TYPE,"wrong kind of server, wrong data served" },
    { ERR_NETIO_NOT_GNCXML,     "whatever it is, we can't parse it." },

    /* database errors */
    { ERR_SQL_MISSING_DATA,     "database doesn't contain expected data" },
    { ERR_SQL_DB_TOO_OLD,       "database is old and needs upgrading" },
    { ERR_SQL_DB_TOO_NEW,       "database is newer, we can't write to it" },
    { ERR_SQL_DB_BUSY,          "database is busy, cannot upgrade version" },
    { ERR_SQL_BAD_DBI,          "LibDBI has numeric errors" },
    { ERR_SQL_DBI_UNTESTABLE,   "could not complete test for LibDBI bug" },

    /* RPC errors */
    { ERR_RPC_HOST_UNK,         "Host unknown" },
    { ERR_RPC_CANT_BIND,        "can't bind to address" },
    { ERR_RPC_CANT_ACCEPT,      "can't accept connection" },
    { ERR_RPC_NO_CONNECTION,    "no connection to server" },
    { ERR_RPC_BAD_VERSION,      "RPC Version Mismatch" },
    { ERR_RPC_FAILED,           "Operation failed" },
    { ERR_RPC_NOT_ADDED,        "object not added" }
};

QofBackendError
qof_backend_get_error (QofBackend* qof_be)
{
    if (qof_be == nullptr) return ERR_BACKEND_NO_ERR;
    return ((QofBackend*)qof_be)->get_error();
}

const char*
qof_backend_get_error_string (QofBackendError backend_error)
{
    auto str_iter = qof_backend_error_map.find (backend_error);
    if (str_iter == qof_backend_error_map.end())
    {
        PERR ("cannot error string for error %d", backend_error);
        return nullptr;
    }
    return str_iter->second;
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
