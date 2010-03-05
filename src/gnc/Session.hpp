/*
 * Session.hpp
 * Copyright (C) 2010 Christian Stimming
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

#ifndef GNC_SESSION_HPP
#define GNC_SESSION_HPP

// gnucash includes
#include "config.h" // required by qof/qofutil.h
#include <glib/gi18n.h>
extern "C"
{
#include "qof.h"
#include "engine/gnc-hooks.h"
}

#include "gnc/WeakPointer.hpp"
#include <QString>

namespace gnc
{

class Book;

/** ScopedPointer object around a QofSession object, which also owns the
 * QofSession object.
 */
class Session : public WeakPointer< ::QofSession >
{
public:
    typedef WeakPointer< ::QofSession > base_class;

    Session(element_type *ptr = 0)
            : base_class(ptr)
    {}

    // Now the actual functions on the object

    void begin(const QString& book_id, bool ignore_lock, bool create_if_nonexistent)
    {
        qof_session_begin(get(), book_id.toUtf8(), ignore_lock, create_if_nonexistent);
    }
    void load (QofPercentageFunc percentage_func)
    {
        qof_session_load(get(), percentage_func);
    }
    QofBackendError get_error ()
    {
        return qof_session_get_error(get());
    }
    QofBackendError pop_error ()
    {
        return qof_session_pop_error(get());
    }
    QString get_error_message() const
    {
        return QString::fromUtf8(qof_session_get_error_message(get()));
    }
    Book get_book () const;

    QString get_file_path () const
    {
        return QString::fromUtf8(qof_session_get_file_path(get()));
    }

    QString get_url() const
    {
        return QString::fromUtf8(qof_session_get_url(get()));
    }

    bool save_in_progress() const
    {
        return qof_session_save_in_progress(get());
    }
    bool save_may_clobber_data () const
    {
        return qof_session_save_may_clobber_data(get());
    }
    void save (QofPercentageFunc percentage_func)
    {
        qof_session_save(get(), percentage_func);
    }


    void call_close_hooks ()
    {
        qof_session_call_close_hooks (get());
    }


};

std::pair<QString, QString> errorToStringPair(QofBackendError err);

} // END namespace gnc

#endif
