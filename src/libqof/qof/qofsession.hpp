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

struct QofSessionImpl
{
    QofSessionImpl () noexcept;
    /*
     * Ends the current session, destroys the backend, and destroys the book.
     */
    ~QofSessionImpl () noexcept;

    /**
     * Swap books with another session
     */
    void swap_books (QofSessionImpl &) noexcept;

    /**
     * Begin this session.
     */
    void begin (const char * book_id, bool ignore_lock, bool create, bool force) noexcept;

    const char * get_error_message () const noexcept;

    void clear_error () noexcept;

    void push_error (QofBackendError const err, const char * message) noexcept;

    QofBackendError pop_error () noexcept;

    /**
     * Returns the local cached error. If there is no local error, we check
     * for an error in the backend.
     */
    QofBackendError get_error () noexcept;

    QofBook * get_book () const noexcept;

    const char * get_file_path () const noexcept;

    void ensure_all_data_loaded () noexcept;

    void load_backend (const char * access_method) noexcept;

    void destroy_backend () noexcept;

    void load (QofPercentageFunc) noexcept;

    void save (QofPercentageFunc) noexcept;

    void safe_save (QofPercentageFunc) noexcept;

    bool save_in_progress () const noexcept;

    void end () noexcept;

    bool events_pending () const noexcept;

    bool process_events () const noexcept;

    bool export_session (QofSessionImpl & real_session, QofPercentageFunc) noexcept;



    /* This is just a "fake" entry point to allow me to pass a Session as
     * an Entity.  NOTE:  THIS IS NOT AN ENTITY!  THE ONLY PART OF ENTITY
     * THAT IS VALID IS E_TYPE!
     */
    QofInstance entity;

    /* A book holds pointers to the various types of datasets.
     * A session has exactly one book. */
    QofBook *book;

    /* The requested book id, in the form or a URI, such as
     * file:/some/where, or sql:server.host.com:555
     */
    char *book_id;

    /* If any book subroutine failed, this records the failure reason
     * (file not found, etc).
     * This is a 'stack' that is one deep.  (Should be deeper ??)
     * FIXME: Each backend has its own error stack. The session
     * and the backends should all be using (or making it look like)
     * there is only one stack.
     */
    QofBackendError last_err;
    char *error_message;

    /* ---------------------------------------------------- */
    /* Pointer to the backend that is actually used to move data
     * between the persistant store and the local engine.  */
    QofBackend *backend;

    bool saving;
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
