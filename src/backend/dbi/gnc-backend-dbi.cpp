/********************************************************************
 * gnc-backend-dbi.c: load and save data to SQL via libdbi          *
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
\********************************************************************/
/** @file gnc-backend-dbi.c
 *  @brief load and save data to SQL
 *  @author Copyright (c) 2006-2008 Phil Longstaff <plongstaff@rogers.com>
 *
 * This file implements the top-level QofBackend API for saving/
 * restoring data to/from an SQL db using libdbi
 */
extern "C"
{
#include "config.h"

#include <platform.h>
#ifdef __STRICT_ANSI__
#undef __STRICT_ANSI__
#define __STRICT_ANSI_UNSET__ 1
#endif
#ifdef _NO_OLDNAMES
#undef _NO_OLDNAMES
#endif
#ifdef _UWIN
#undef _UWIN
#endif
#if PLATFORM(WINDOWS)
#include <windows.h>
#define __STDC_FORMAT_MACROS 1
#endif

#include <inttypes.h>
#include <errno.h>
#include <glib.h>
#include <glib/gstdio.h>

#include "qof.h"
#include "qofquery-p.h"
#include "qofquerycore-p.h"
#include "Account.h"
#include "TransLog.h"
#include "gnc-engine.h"
#include "SX-book.h"
#include "Recurrence.h"

#include "gnc-uri-utils.h"
#include "gnc-filepath-utils.h"
#include <gnc-path.h>
#include "gnc-locale-utils.h"

#include "gnc-prefs.h"

#ifdef S_SPLINT_S
#include "splint-defs.h"
#endif

}
#include <boost/regex.hpp>
#include <string>
#include <iomanip>

#include <gnc-backend-prov.hpp>
#include "gnc-backend-dbi.h"
#include "gnc-backend-dbi.hpp"

#include <gnc-sql-object-backend.hpp>
#include "gnc-dbisqlresult.hpp"
#include "gnc-dbisqlconnection.hpp"

#if PLATFORM(WINDOWS)
#ifdef __STRICT_ANSI_UNSET__
#undef __STRICT_ANSI_UNSET__
#define __STRICT_ANSI__ 1
#endif
#endif

#if LIBDBI_VERSION >= 900
#define HAVE_LIBDBI_R 1
#define HAVE_LIBDBI_TO_LONGLONG 1
static dbi_inst dbi_instance = nullptr;
#else
#define HAVE_LIBDBI_R 0
#define HAVE_LIBDBI_TO_LONGLONG 0
#endif

#define TRANSACTION_NAME "trans"

static QofLogModule log_module = G_LOG_DOMAIN;

#define FILE_URI_TYPE "file"
#define FILE_URI_PREFIX (FILE_URI_TYPE "://")
#define SQLITE3_URI_TYPE "sqlite3"
#define SQLITE3_URI_PREFIX (SQLITE3_URI_TYPE "://")
#define PGSQL_DEFAULT_PORT 5432

static void adjust_sql_options (dbi_conn connection);
static bool save_may_clobber_data (dbi_conn conn, const std::string& dbname);

template <DbType Type>
class QofDbiBackendProvider : public QofBackendProvider
{
public:
    QofDbiBackendProvider (const char* name, const char* type) :
        QofBackendProvider {name, type} {}
    QofDbiBackendProvider(QofDbiBackendProvider&) = delete;
    QofDbiBackendProvider operator=(QofDbiBackendProvider&) = delete;
    QofDbiBackendProvider(QofDbiBackendProvider&&) = delete;
    QofDbiBackendProvider operator=(QofDbiBackendProvider&&) = delete;
    ~QofDbiBackendProvider () = default;
    QofBackend* create_backend(void)
    {
        return new GncDbiBackend<Type>(nullptr, nullptr);
    }
    bool type_check(const char* type) { return true; }
};

/* ================================================================= */
/* ================================================================= */
struct UriStrings
{
    UriStrings() = default;
    UriStrings(const std::string& uri);
    ~UriStrings() = default;
    std::string basename() const noexcept;
    const char* dbname() const noexcept;
    std::string quote_dbname(DbType t) const noexcept;
    std::string m_protocol;
    std::string m_host;
    std::string m_dbname;
    std::string m_username;
    std::string m_password;
    std::string m_basename;
    int m_portnum;
};

UriStrings::UriStrings(const std::string& uri)
{
    gchar *protocol, *host, *username, *password, *dbname;
    int portnum;
    gnc_uri_get_components(uri.c_str(), &protocol, &host, &portnum, &username,
                           &password, &dbname);
    m_protocol = std::string{protocol};
    m_host = std::string{host};
    m_dbname = std::string{dbname};
    if (username)
	m_username = std::string{username};
    if (password)
	m_password = std::string{password};
    m_portnum = portnum;
    g_free(protocol);
    g_free(host);
    g_free(username);
    g_free(password);
    g_free(dbname);
    
}

std::string
UriStrings::basename() const noexcept
{
    return m_protocol + "_" + m_host + "_" + m_username + "_" + m_dbname;
}

const char*
UriStrings::dbname() const noexcept
{
    return m_dbname.c_str();
}

std::string
UriStrings::quote_dbname(DbType t) const noexcept
{
    if (m_dbname.empty())
        return "";
    const char quote = (t == DbType::DBI_MYSQL ? '`' : '"');
    std::string retval(1, quote);
    retval += m_dbname + quote;
    return retval;
}

static void
set_options(dbi_conn conn, const PairVec& options)
{
    for (auto option : options)
    {
        auto opt = option.first.c_str();
        auto val = option.second.c_str();
        auto result = dbi_conn_set_option(conn, opt, val);
        if (result < 0)
        {
            const char *msg = nullptr;
            int err = dbi_conn_error(conn, &msg);
            PERR("Error setting %s option to %s: %s", opt, val, msg);
            throw std::runtime_error(msg);
        }
    }
}

/**
 * Sets standard db options in a dbi_conn.
 *
 * @param conn dbi_conn connection
 * @param uri UriStrings containing the needed paramters.
 * @return TRUE if successful, FALSE if error
 */
template <DbType Type> bool
GncDbiBackend<Type>::set_standard_connection_options (dbi_conn conn,
                                                const UriStrings& uri)

{
    gint result;
    PairVec options;
    options.push_back(std::make_pair("host", uri.m_host));
    options.push_back(std::make_pair("dbname", uri.m_dbname));
    options.push_back(std::make_pair("username", uri.m_username));
    options.push_back(std::make_pair("password", uri.m_password));
    options.push_back(std::make_pair("encoding", "UTF-8"));
    try
    {
        set_options(conn, options);
        auto result = dbi_conn_set_option_numeric(conn, "port", uri.m_portnum);
        if (result < 0)
        {
            const char *msg = nullptr;
            auto err = dbi_conn_error(conn, &msg);
            PERR("Error setting port option to %d: %s", uri.m_portnum, msg);
            throw std::runtime_error(msg);
        }
    }
    catch (std::runtime_error& err)
    {
        set_error (ERR_BACKEND_SERVER_ERR);
        return false;
    }

    return true;
}

template <DbType Type> void error_handler(void* conn, void* data);
void error_handler(void* conn, void* data);

template <DbType Type> dbi_conn
GncDbiBackend<Type>::conn_setup (PairVec& options, UriStrings& uri)
{
    const char* dbstr = (Type == DbType::DBI_SQLITE ? "sqlite3" :
                         Type == DbType::DBI_MYSQL ? "mysql" : "pgsql");
#if HAVE_LIBDBI_R
    dbi_conn conn = nullptr;
    if (dbi_instance)
        conn = dbi_conn_new_r (dbstr, dbi_instance);
    else
        PERR ("Attempt to connect with an uninitialized dbi_instance");
#else
    auto conn = dbi_conn_new (dbstr);
#endif

    if (conn == nullptr)
    {
        PERR ("Unable to create %s dbi connection", dbstr);
        set_error (ERR_BACKEND_BAD_URL);
	return nullptr;
    }

    dbi_conn_error_handler (conn, error_handler<Type>, this);
    if (!uri.m_dbname.empty() &&
        !set_standard_connection_options(conn, uri))
    {
        dbi_conn_close(conn);
        return nullptr;
    }
    if(!options.empty())
    {
        try {
            set_options(conn, options);
        }
        catch (std::runtime_error& err)
        {
            dbi_conn_close(conn);
            set_error (ERR_BACKEND_SERVER_ERR);
            return nullptr;
        }
    }

    return conn;
}

template <DbType Type>bool
GncDbiBackend<Type>::create_database(dbi_conn conn, const char* db)
{
    const char *dbname;
    const char *dbcreate;
    if (Type == DbType::DBI_MYSQL)
    {
        dbname = "mysql";
        dbcreate = "CREATE DATABASE %s CHARACTER SET utf8";
    }
    else
    {
        dbname = "postgres";
        dbcreate = "CREATE DATABASE %s WITH TEMPLATE template0 ENCODING 'UTF8'";
    }
    PairVec options;
    options.push_back(std::make_pair("dbname", dbname));
    try
    {
        set_options(conn, options);
    }
    catch (std::runtime_error& err)
    {
        set_error (ERR_BACKEND_SERVER_ERR);
        return false;
    }

    auto result = dbi_conn_connect (conn);
    if (result < 0)
    {
        PERR ("Unable to connect to %s database", dbname);
        set_error(ERR_BACKEND_SERVER_ERR);
        return false;
    }
    if (Type == DbType::DBI_MYSQL)
        adjust_sql_options(conn);
    auto dresult = dbi_conn_queryf (conn, dbcreate, db);
    if (dresult == nullptr)
    {
        PERR ("Unable to create database '%s'\n", db);
        set_error (ERR_BACKEND_SERVER_ERR);
        return false;
    }
    if (Type == DbType::DBI_PGSQL)
    {
        const char *alterdb = "ALTER DATABASE %s SET "
            "standard_conforming_strings TO on";
        dbi_conn_queryf (conn, alterdb, db);
    }
    dbi_conn_close(conn);
    conn = nullptr;
    return true;
}

template <> void
error_handler<DbType::DBI_SQLITE> (dbi_conn conn, void* user_data)
{
    const char* msg;
    GncDbiBackend<DbType::DBI_SQLITE> *dbi_be =
        static_cast<decltype(dbi_be)>(user_data);
    int errnum = dbi_conn_error (conn, &msg);
    PERR ("DBI error: %s\n", msg);
    if (dbi_be->connected())
        dbi_be->set_dbi_error (ERR_BACKEND_MISC, 0, false);
}

template <> void
GncDbiBackend<DbType::DBI_SQLITE>::session_begin(QofSession* session,
                                                 const char* book_id,
                                                 bool ignore_lock,
                                                 bool create, bool force)
{
    gboolean file_exists;
    PairVec options;

    g_return_if_fail (session != nullptr);
    g_return_if_fail (book_id != nullptr);

    ENTER (" ");

    /* Remove uri type if present */
    auto path = gnc_uri_get_path (book_id);
    std::string filepath{path};
    g_free(path);
    GFileTest ftest = static_cast<decltype (ftest)> (
        G_FILE_TEST_IS_REGULAR | G_FILE_TEST_EXISTS) ;
    file_exists = g_file_test (filepath.c_str(), ftest);
    if (!create && !file_exists)
    {
        set_error (ERR_FILEIO_FILE_NOT_FOUND);
        std::string msg{"Sqlite3 file "};
        set_message (msg + filepath + " not found");
        PWARN ("Sqlite3 file %s not found", filepath.c_str());
        LEAVE("Error");
	return;
    }

    if (create && !force && file_exists)
    {
        set_error (ERR_BACKEND_STORE_EXISTS);
        auto msg = "Might clobber, no force";
        PWARN ("%s", msg);
        LEAVE("Error");
	return;
    }

    connect(nullptr);
    /* dbi-sqlite3 documentation says that sqlite3 doesn't take a "host" option */
    options.push_back(std::make_pair("host", "localhost"));
    auto dirname = g_path_get_dirname (filepath.c_str());
    auto basename = g_path_get_basename (filepath.c_str());
    options.push_back(std::make_pair("dbname", basename));
    options.push_back(std::make_pair("sqlite3_dbdir", dirname));
    if (basename != nullptr) g_free (basename);
    if (dirname != nullptr) g_free (dirname);
    UriStrings uri;
    auto conn = conn_setup(options, uri);
    if (conn == nullptr)
    {
        LEAVE("Error");
        return;
    }

    auto result = dbi_conn_connect (conn);

    if (result < 0)
    {
        dbi_conn_close(conn);
        PERR ("Unable to connect to %s: %d\n", book_id, result);
        set_error (ERR_BACKEND_BAD_URL);
        LEAVE("Error");
	return;
    }

    if (!conn_test_dbi_library(conn))
    {
        if (create && !file_exists)
        {
         /* File didn't exist before, but it does now, and we don't want to
          * leave it lying around.
          */
            dbi_conn_close (conn);
            conn = nullptr;
            g_unlink (filepath.c_str());
        }
        dbi_conn_close(conn);
        LEAVE("Bad DBI Library");
        return;
    }

    try
    {
        connect(new GncDbiSqlConnection(DbType::DBI_SQLITE,
                                            this, conn, ignore_lock));
    }
    catch (std::runtime_error& err)
    {
        return;
    }

    /* We should now have a proper session set up.
     * Let's start logging */
    xaccLogSetBaseName (filepath.c_str());
    PINFO ("logpath=%s", filepath.c_str() ? filepath.c_str() : "(null)");

    LEAVE ("");
}


template <> void
error_handler<DbType::DBI_MYSQL> (dbi_conn conn, void* user_data)
{
    GncDbiBackend<DbType::DBI_MYSQL>* dbi_be =
        static_cast<decltype(dbi_be)>(user_data);
    const char* msg;

    auto err_num = dbi_conn_error (conn, &msg);

    /* Note: the sql connection may not have been initialized yet
     *       so let's be careful with using it
     */

    /* Database doesn't exist. When this error is triggered the
     * GncDbiSqlConnection may not exist yet either, so don't use it here
     */
    if (err_num == 1049)            // Database doesn't exist
    {
        PINFO ("DBI error: %s\n", msg);
        dbi_be->set_exists(false);
        return;
    }

    /* All the other error handling code assumes the GncDbiSqlConnection
     *  has been initialized. So let's assert it exits here, otherwise
     * simply return.
     */
    if (!dbi_be->connected())
    {
        PINFO ("DBI error: %s\n", msg);
        PINFO ("Note: GbcDbiSqlConnection not yet initialized. Skipping further error processing.");
        return;
    }

    /* Test for other errors */
    if (err_num == 2006)       // Server has gone away
    {
        PINFO ("DBI error: %s - Reconnecting...\n", msg);
        dbi_be->set_dbi_error (ERR_BACKEND_CONN_LOST, 1, true);
        dbi_be->retry_connection(msg);
    }
    else if (err_num == 2003)       // Unable to connect
    {
        dbi_be->set_dbi_error (ERR_BACKEND_CANT_CONNECT, 1, true);
        dbi_be->retry_connection (msg);
    }
    else                            // Any other error
    {
        PERR ("DBI error: %s\n", msg);
        dbi_be->set_dbi_error (ERR_BACKEND_MISC, 0, FALSE);
    }
}

#define SQL_OPTION_TO_REMOVE "NO_ZERO_DATE"

/* Given an sql_options string returns a copy of the string adjusted as
 * necessary.  In particular if string the contains SQL_OPTION_TO_REMOVE it is
 * removed along with comma separator.
 */
std::string
adjust_sql_options_string(const std::string& str)
{
/* Regex that finds the SQL_OPTION_TO_REMOVE as the first, last, or middle of a
 * comma-delimited list.
 */
    boost::regex reg{"(?:," SQL_OPTION_TO_REMOVE "$|\\b"
            SQL_OPTION_TO_REMOVE "\\b,?)"};
    return regex_replace(str, reg, std::string{""});
}

/* checks mysql sql_options and adjusts if necessary */
static void
adjust_sql_options (dbi_conn connection)
{
    dbi_result result = dbi_conn_query( connection, "SELECT @@sql_mode");
    if (result == nullptr)
    {
        const char* errmsg;
        int err = dbi_conn_error(connection, &errmsg);
        PERR("Unable to read sql_mode %d : %s", err, errmsg);
        return;
    }
    dbi_result_first_row(result);
    std::string str{dbi_result_get_string_idx(result, 1)};
    dbi_result_free(result);
    if (str.empty())
    {
        const char* errmsg;
        int err = dbi_conn_error(connection, &errmsg);
	if (err)
	    PERR("Unable to get sql_mode %d : %s", err, errmsg);
	else
	    PINFO("Sql_mode isn't set.");
        return;
    }
    PINFO("Initial sql_mode: %s", str.c_str());
    if(str.find(SQL_OPTION_TO_REMOVE) == std::string::npos)
        return;

    std::string adjusted_str{adjust_sql_options_string(str)};
    PINFO("Setting sql_mode to %s", adjusted_str.c_str());
    std::string set_str{"SET sql_mode=" + std::move(adjusted_str)};
    dbi_result set_result = dbi_conn_query(connection,
                                           set_str.c_str());
    if (set_result)
    {
        dbi_result_free(set_result);
    }
    else
    {
        const char* errmsg;
        int err = dbi_conn_error(connection, &errmsg);
        PERR("Unable to set sql_mode %d : %s", err, errmsg);
    }
}


template <DbType Type> void
GncDbiBackend<Type>::session_begin (QofSession* session, const char* book_id,
                                    bool ignore_lock, bool create, bool force)
{
    GncDbiTestResult dbi_test_result = GNC_DBI_PASS;
    PairVec options;

    g_return_if_fail (session != nullptr);
    g_return_if_fail (book_id != nullptr);

    ENTER (" ");

    /* Split the book-id
     * Format is protocol://username:password@hostname:port/dbname
     where username, password and port are optional) */
    UriStrings uri(book_id);

    if (Type == DbType::DBI_PGSQL)
    {
        if (uri.m_portnum == 0)
            uri.m_portnum = PGSQL_DEFAULT_PORT;
        /* Postgres's SQL interface coerces identifiers to lower case, but the
         * C interface is case-sensitive. This results in a mixed-case dbname
         * being created (with a lower case name) but then dbi can't conect to
         * it. To work around this, coerce the name to lowercase first. */
        auto lcname = g_utf8_strdown (uri.dbname(), -1);
        uri.m_dbname = std::string{lcname};
        g_free(lcname);
    }
    connect(nullptr);

    auto conn = conn_setup(options, uri);
    if (conn == nullptr)
    {
        LEAVE("Error");
        return;
    }

    m_exists = true; //May be unset in the error handler.
    auto result = dbi_conn_connect (conn);
    if (result == 0)
    {
        if (Type == DbType::DBI_MYSQL)
            adjust_sql_options (conn);
        if(!conn_test_dbi_library(conn))
        {
            dbi_conn_close(conn);
            LEAVE("Error");
            return;
        }
        if (create && !force && save_may_clobber_data (conn,
                                                       uri.quote_dbname(Type)))
        {
            set_error (ERR_BACKEND_STORE_EXISTS);
            PWARN ("Databse already exists, Might clobber it.");
            dbi_conn_close(conn);
            LEAVE("Error");
            return;
        }

    }
    else
    {

        if (m_exists)
        {
            PERR ("Unable to connect to database '%s'\n", uri.dbname());
            set_error (ERR_BACKEND_SERVER_ERR);
            dbi_conn_close(conn);
            LEAVE("Error");
            return;
        }

        if (create)
        {
            if (!create_database(conn, uri.quote_dbname(Type).c_str()))
            {
                dbi_conn_close(conn);
                LEAVE("Error");
                return;
            }
            conn = conn_setup(options, uri);
            result = dbi_conn_connect (conn);
            if (result < 0)
            {
                PERR ("Unable to create database '%s'\n", uri.dbname());
                set_error (ERR_BACKEND_SERVER_ERR);
                dbi_conn_close(conn);
                LEAVE("Error");
                return;
            }
            if (Type == DbType::DBI_MYSQL)
                adjust_sql_options (conn);
            if (!conn_test_dbi_library(conn))
            {
                if (Type == DbType::DBI_PGSQL)
                    dbi_conn_select_db (conn, "template1");
                dbi_conn_queryf (conn, "DROP DATABASE %s",
                                 uri.quote_dbname(Type).c_str());
                dbi_conn_close(conn);
                return;
            }
        }
        else
        {
            set_error(ERR_BACKEND_NO_SUCH_DB);
            std::string msg{"Database "};
            set_message(msg + uri.dbname() + " not found");
        }
    }

    connect(nullptr);
    try
    {
        connect(new GncDbiSqlConnection(Type, this, conn, ignore_lock));
    }
    catch (std::runtime_error& err)
    {
        return;
    }
    /* We should now have a proper session set up.
     * Let's start logging */
    auto translog_path = gnc_build_translog_path (uri.basename().c_str());
    xaccLogSetBaseName (translog_path);
    PINFO ("logpath=%s", translog_path ? translog_path : "(null)");
    g_free (translog_path);

    LEAVE (" ");
}

template<> void
error_handler<DbType::DBI_PGSQL> (dbi_conn conn, void* user_data)
{
    GncDbiBackend<DbType::DBI_PGSQL>* dbi_be =
        static_cast<decltype(dbi_be)>(user_data);
    const char* msg;

    (void)dbi_conn_error (conn, &msg);
    if (g_str_has_prefix (msg, "FATAL:  database") &&
        g_str_has_suffix (msg, "does not exist\n"))
    {
        PINFO ("DBI error: %s\n", msg);
        dbi_be->set_exists(false);
    }
    else if (g_strrstr (msg,
                        "server closed the connection unexpectedly"))    // Connection lost
    {
        if (!dbi_be->connected())
        {
            PWARN ("DBI Error: Connection lost, connection pointer invalid");
            return;
        }
        PINFO ("DBI error: %s - Reconnecting...\n", msg);
        dbi_be->set_dbi_error (ERR_BACKEND_CONN_LOST, 1, true);
        dbi_be->retry_connection(msg);
    }
    else if (g_str_has_prefix (msg, "connection pointer is NULL") ||
             g_str_has_prefix (msg, "could not connect to server"))       // No connection
    {

        if (!dbi_be->connected())
            qof_backend_set_error(reinterpret_cast<QofBackend*>(dbi_be),
                                  ERR_BACKEND_CANT_CONNECT);
        else
        {
            dbi_be->set_dbi_error(ERR_BACKEND_CANT_CONNECT, 1, true);
            dbi_be->retry_connection (msg);
        }
    }
    else
    {
        PERR ("DBI error: %s\n", msg);
        if (dbi_be->connected())
            dbi_be->set_dbi_error (ERR_BACKEND_MISC, 0, false);
    }
}

/* ================================================================= */

template <DbType Type> void
GncDbiBackend<Type>::session_end ()
{
    ENTER (" ");

    finalize_version_info ();
    connect(nullptr);

    LEAVE (" ");
}

template <DbType Type>
GncDbiBackend<Type>::~GncDbiBackend()
{
    /* Stop transaction logging */
    xaccLogSetBaseName (nullptr);
}

/* ================================================================= */

/* GNUCASH_RESAVE_VERSION indicates the earliest database version
 * compatible with this version of Gnucash; the stored value is the
 * earliest version of Gnucash conpatible with the database. If the
 * GNUCASH_RESAVE_VERSION for this Gnucash is newer than the Gnucash
 * version which created the database, a resave is offered. If the
 * version of this Gnucash is older than the saved resave version,
 * then the database will be loaded read-only. A resave will update
 * both values to match this version of Gnucash.
 */
template <DbType Type> void
GncDbiBackend<Type>::load (QofBook* book, QofBackendLoadType loadType)
{
    g_return_if_fail (book != nullptr);

    ENTER ("dbi_be=%p, book=%p", this, book);

    if (loadType == LOAD_TYPE_INITIAL_LOAD)
    {

        // Set up table version information
        init_version_info ();
        assert (m_book == nullptr);
        create_tables();
    }

    GncSqlBackend::load(book, loadType);

    if (GNUCASH_RESAVE_VERSION > get_table_version("Gnucash"))
    {
        /* The database was loaded with an older database schema or
         * data semantics. In order to ensure consistency, the whole
         * thing needs to be saved anew. */
        set_error(ERR_SQL_DB_TOO_OLD);
    }
    else if (GNUCASH_RESAVE_VERSION < get_table_version("Gnucash-Resave"))
    {
        /* Worse, the database was created with a newer version. We
         * can't safely write to this database, so the user will have
         * to do a "save as" to make one that we can write to.
         */
        set_error(ERR_SQL_DB_TOO_NEW);
    }


    LEAVE ("");
}

/* ================================================================= */
/* This is used too early to call GncDbiProvider::get_table_list(). */
static bool
save_may_clobber_data (dbi_conn conn, const std::string& dbname)
{

    /* Data may be clobbered iff the number of tables != 0 */
    auto result = dbi_conn_get_table_list (conn, dbname.c_str(), nullptr);
    bool retval = false;
    if (result)
    {
        retval =  dbi_result_get_numrows (result) > 0;
        dbi_result_free (result);
    }
    return retval;
}


/**
 * Safely resave a database by renaming all of its tables, recreating
 * everything, and then dropping the backup tables only if there were
 * no errors. If there are errors, drop the new tables and restore the
 * originals.
 *
 * @param book: QofBook to be saved in the database.
 */
template <DbType Type> void
GncDbiBackend<Type>::safe_sync (QofBook* book)
{
    auto conn = dynamic_cast<GncDbiSqlConnection*>(m_conn);

    g_return_if_fail (conn != nullptr);
    g_return_if_fail (book != nullptr);

    ENTER ("book=%p, primary=%p", book, m_book);
    if (!conn->table_operation (TableOpType::backup))
    {
        set_error(ERR_BACKEND_SERVER_ERR);
        conn->table_operation (TableOpType::rollback);
        LEAVE ("Failed to rename tables");
        return;
    }
    if (!conn->drop_indexes())
    {
        conn->table_operation (TableOpType::rollback);
        set_error (ERR_BACKEND_SERVER_ERR);
        set_message("Failed to drop indexes");
        LEAVE ("Failed to drop indexes");
        return;
    }

    sync(m_book);
    if (check_error())
    {
        conn->table_operation (TableOpType::rollback);
        LEAVE ("Failed to create new database tables");
        return;
    }
    conn->table_operation (TableOpType::drop_backup);
    LEAVE ("book=%p", m_book);
}
/* ================================================================= */

/*
 * Checks to see whether the file is an sqlite file or not
 *
 */
template<> bool
QofDbiBackendProvider<DbType::DBI_SQLITE>::type_check(const char *uri)
{
    FILE* f;
    gchar buf[50];
    G_GNUC_UNUSED size_t chars_read;
    gint status;
    gchar* filename;

    // BAD if the path is null
    g_return_val_if_fail (uri != nullptr, FALSE);

    filename = gnc_uri_get_path (uri);
    f = g_fopen (filename, "r");
    g_free (filename);

    // OK if the file doesn't exist - new file
    if (f == nullptr)
    {
        PINFO ("doesn't exist (errno=%d) -> DBI", errno);
        return TRUE;
    }

    // OK if file has the correct header
    chars_read = fread (buf, sizeof (buf), 1, f);
    status = fclose (f);
    if (status < 0)
    {
        PERR ("Error in fclose(): %d\n", errno);
    }
    if (g_str_has_prefix (buf, "SQLite format 3"))
    {
        PINFO ("has SQLite format string -> DBI");
        return TRUE;
    }
    PINFO ("exists, does not have SQLite format string -> not DBI");

    // Otherwise, BAD
    return FALSE;
}

void
gnc_module_init_backend_dbi (void)
{
    const char* driver_dir;
    int num_drivers;
    gboolean have_sqlite3_driver = FALSE;
    gboolean have_mysql_driver = FALSE;
    gboolean have_pgsql_driver = FALSE;

    /* Initialize libdbi and see which drivers are available.  Only register qof backends which
       have drivers available. */
    driver_dir = g_getenv ("GNC_DBD_DIR");
    if (driver_dir == nullptr)
    {
        PINFO ("GNC_DBD_DIR not set: using libdbi built-in default\n");
    }

    /* dbi_initialize returns -1 in case of errors */
#if HAVE_LIBDBI_R
    if (dbi_instance)
        return;
    num_drivers = dbi_initialize_r (driver_dir, &dbi_instance);
#else
    num_drivers = dbi_initialize (driver_dir);
#endif
    if (num_drivers <= 0)
    {
        gchar* dir = g_build_filename (gnc_path_get_libdir (), "dbd", nullptr);
#if HAVE_LIBDBI_R
        if (dbi_instance)
            return;
        num_drivers = dbi_initialize_r (dir, &dbi_instance);
#else
        num_drivers = dbi_initialize (dir);
#endif
        g_free (dir);
    }
    if (num_drivers <= 0)
    {
        PWARN ("No DBD drivers found\n");
    }
    else
    {
        dbi_driver driver = nullptr;
        PINFO ("%d DBD drivers found\n", num_drivers);

        do
        {
#if HAVE_LIBDBI_R
            driver = dbi_driver_list_r (driver, dbi_instance);
#else
            driver = dbi_driver_list (driver);
#endif

            if (driver != nullptr)
            {
                const gchar* name = dbi_driver_get_name (driver);

                PINFO ("Driver: %s\n", name);
                if (strcmp (name, "sqlite3") == 0)
                {
                    have_sqlite3_driver = TRUE;
                }
                else if (strcmp (name, "mysql") == 0)
                {
                    have_mysql_driver = TRUE;
                }
                else if (strcmp (name, "pgsql") == 0)
                {
                    have_pgsql_driver = TRUE;
                }
            }
        }
        while (driver != nullptr);
    }

    if (have_sqlite3_driver)
    {
        const char* name = "GnuCash Libdbi (SQLITE3) Backend";
        auto prov = QofBackendProvider_ptr(new QofDbiBackendProvider<DbType::DBI_SQLITE>{name, FILE_URI_TYPE});
        qof_backend_register_provider(std::move(prov));
        prov = QofBackendProvider_ptr(new QofDbiBackendProvider<DbType::DBI_SQLITE>{name, SQLITE3_URI_TYPE});
        qof_backend_register_provider(std::move(prov));
    }

    if (have_mysql_driver)
    {
        const char *name = "GnuCash Libdbi (MYSQL) Backend";
        auto prov = QofBackendProvider_ptr(new QofDbiBackendProvider<DbType::DBI_MYSQL>{name, "mysql"});
        qof_backend_register_provider(std::move(prov));
    }

    if (have_pgsql_driver)
    {
        const char* name = "GnuCash Libdbi (POSTGRESQL) Backend";
        auto prov = QofBackendProvider_ptr(new QofDbiBackendProvider<DbType::DBI_PGSQL>{name, "postgres"});
        qof_backend_register_provider(std::move(prov));
    }

    /* If needed, set log level to DEBUG so that SQl statements will be put into
       the gnucash.trace file. */
    /*    qof_log_set_level( log_module, QOF_LOG_DEBUG ); */
}

#ifndef GNC_NO_LOADABLE_MODULES
G_MODULE_EXPORT void
qof_backend_module_init (void)
{
    gnc_module_init_backend_dbi ();
}

G_MODULE_EXPORT void
qof_backend_module_finalize (void)
{
    gnc_module_finalize_backend_dbi ();
}
#endif /* GNC_NO_LOADABLE_MODULES */

void
gnc_module_finalize_backend_dbi (void)
{
#if HAVE_LIBDBI_R
    if (dbi_instance)
    {
        dbi_shutdown_r (dbi_instance);
        dbi_instance = nullptr;
    }
#else
    dbi_shutdown ();
#endif
}

/* --------------------------------------------------------- */

/** Users discovered a bug in some distributions of libdbi, where if
 * it is compiled on certain versions of gcc with the -ffast-math
 * compiler option it fails to correctly handle saving of 64-bit
 * values. This function tests for the problem.
 * @param: conn: The just-opened dbi_conn
 * @returns: GNC_DBI_PASS if the dbi library is safe to use,
 * GNC_DBI_FAIL_SETUP if the test could not be completed, or
 * GNC_DBI_FAIL_TEST if the bug was found.
 */
static GncDbiTestResult
dbi_library_test (dbi_conn conn)
{
    int64_t testlonglong = -9223372036854775807LL, resultlonglong = 0;
    uint64_t testulonglong = 9223372036854775807LLU, resultulonglong = 0;
    double testdouble = 1.7976921348623157E+307, resultdouble = 0.0;
    dbi_result result;
    GncDbiTestResult retval = GNC_DBI_PASS;

    result = dbi_conn_query (conn, "CREATE TEMPORARY TABLE numtest "
                             "( test_int BIGINT, test_unsigned BIGINT,"
                             " test_double FLOAT8 )");
    if (result == nullptr)
    {
        PWARN ("Test_DBI_Library: Create table failed");
        return GNC_DBI_FAIL_SETUP;
    }
    dbi_result_free (result);
    std::stringstream querystr;
    querystr << "INSERT INTO numtest VALUES (" << testlonglong <<
        ", " << testulonglong << ", " << std::setprecision(12) <<
        testdouble << ")";
    auto query = querystr.str();
    result = dbi_conn_query (conn, query.c_str());
    if (result == nullptr)
    {
        PWARN ("Test_DBI_Library: Failed to insert test row into table");
        return GNC_DBI_FAIL_SETUP;
    }
    dbi_result_free (result);
    gnc_push_locale (LC_NUMERIC, "C");
    result = dbi_conn_query (conn, "SELECT * FROM numtest");
    if (result == nullptr)
    {
        const char* errmsg;
        dbi_conn_error (conn, &errmsg);
        PWARN ("Test_DBI_Library: Failed to retrieve test row into table: %s",
               errmsg);
        dbi_conn_query (conn, "DROP TABLE numtest");
        gnc_pop_locale (LC_NUMERIC);
        return GNC_DBI_FAIL_SETUP;
    }
    while (dbi_result_next_row (result))
    {
        resultlonglong = dbi_result_get_longlong (result, "test_int");
        resultulonglong = dbi_result_get_ulonglong (result, "test_unsigned");
        resultdouble = dbi_result_get_double (result, "test_double");
    }
    gnc_pop_locale (LC_NUMERIC);
    if (testlonglong != resultlonglong)
    {
        PWARN ("Test_DBI_Library: LongLong Failed %" PRId64 " != % " PRId64,
               testlonglong, resultlonglong);
        retval = GNC_DBI_FAIL_TEST;
    }
    if (testulonglong != resultulonglong)
    {
        PWARN ("Test_DBI_Library: Unsigned longlong Failed %" PRIu64 " != %"
               PRIu64, testulonglong, resultulonglong);
        retval = GNC_DBI_FAIL_TEST;
    }
    /* A bug in libdbi stores only 7 digits of precision */
    if (testdouble >= resultdouble + 0.000001e307 ||
        testdouble <= resultdouble - 0.000001e307)
    {
        PWARN ("Test_DBI_Library: Double Failed %17e != %17e",
               testdouble, resultdouble);
        retval = GNC_DBI_FAIL_TEST;
    }
    return retval;
}

template <DbType Type> bool
GncDbiBackend<Type>::conn_test_dbi_library(dbi_conn conn)
{
    auto result = dbi_library_test (conn);
    switch (result)
    {
        case GNC_DBI_PASS:
            break;

        case GNC_DBI_FAIL_SETUP:
            set_error(ERR_SQL_DBI_UNTESTABLE);
            set_message ("DBI library large number test incomplete");
            break;

        case GNC_DBI_FAIL_TEST:
            set_error (ERR_SQL_BAD_DBI);
            set_message ("DBI library fails large number test");
            break;
    }
    return result == GNC_DBI_PASS;
}

/* ========================== END OF FILE ===================== */
