/********************************************************************\
 * qofsession.hpp -- declarations for QOF sessions.                 *
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

/*
 * HISTORY:
 * Copyright (c) 2001 Linux Developers Group
 * Copyright (c) 1998-2003 Linas Vepstas <linas@linas.org>
 */

#ifndef QOF_SESSION_P_H
#define QOF_SESSION_P_H

#include "qofbook.h"
#include "qofsession.h"
#include <utility>
#include <string>

struct QofSessionImpl
{
    QofSessionImpl () noexcept;
    /* Ends the current session, destroys the backend, and destroys the book.  */
    ~QofSessionImpl () noexcept;

    /** Begin this session.  */
    void begin (std::string book_id, bool ignore_lock, bool create, bool force) noexcept;

    /** Swap books with another session */
    void swap_books (QofSessionImpl &) noexcept;
    void ensure_all_data_loaded () noexcept;
    void load (QofPercentageFunc) noexcept;
    void save (QofPercentageFunc) noexcept;
    void safe_save (QofPercentageFunc) noexcept;
    bool save_in_progress () const noexcept;
    bool export_session (QofSessionImpl & real_session, QofPercentageFunc) noexcept;

    bool events_pending () const noexcept;
    bool process_events () const noexcept;

    void clear_error () noexcept;
    QofBackendError pop_error () noexcept;

    /**
     * We return by reference so that a pointer to the data of the string lives
     * long enough to make it back to C code.
     */
    std::string const & get_book_id () const noexcept;
    /**
     * Returns and clears the local cached error. If there is no local error, we check
     * for an error in the backend.
     */
    QofBackendError get_error () noexcept;
    std::string get_error_message () const noexcept;
    QofBook * get_book () const noexcept;
    QofBackend * get_backend () const noexcept;
    std::string get_file_path () const noexcept;
    bool is_saving () const noexcept;

    /**
     * Terminates the current backend.
     */
    void end () noexcept;
    void destroy_backend () noexcept;

private:
    void push_error (QofBackendError const err, std::string message) noexcept;

    void load_backend (std::string access_method) noexcept;

    /* A book holds pointers to the various types of datasets.
     * A session has exactly one book. */
    QofBook * m_book;

    /* The requested book id, in the form or a URI, such as
     * file:/some/where, or sql:server.host.com:555
     */
    std::string m_book_id;

    bool m_saving;
    bool m_creating;

    /* If any book subroutine failed, this records the failure reason
     * (file not found, etc).
     * This is a 'stack' that is one deep.  (Should be deeper ??)
     * FIXME: Each backend has its own error stack. The session
     * and the backends should all be using (or making it look like)
     * there is only one stack.
     */
    QofBackendError m_last_err;
    std::string m_error_message;

    /* These functions support the old testing infrastructure and should
     * be removed when they are no longer necessary.*/
    friend void qof_session_load_backend (QofSession *, const char *);
    friend char const * qof_session_get_book_id (QofSession *);
    friend void qof_session_set_book_id (QofSession *, char const *);
};

typedef struct qof_instance_copy_data
{
    QofInstance *from;
    QofInstance *to;
    QofParam  *param;
    GList  *referenceList;
    GSList *param_list;
    QofSession *new_session;
    gboolean error;
} QofInstanceCopyData;

#ifdef __cplusplus
extern "C"
{
#endif

QofBackend * qof_session_get_backend (const QofSession *session);

#ifdef __cplusplus
}
#endif

#endif
