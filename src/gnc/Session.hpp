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
#include "libqof/qof/guid.hpp"
extern "C"
{
#include "qof.h"
#include "engine/gnc-hooks.h"
}

#include "gnc/WeakPointer.hpp"
#include <glibmm/refptr.h>
#include <QString>

namespace gnc
{

class Book;

/** Wrapper around a gnucash ::QofSession pointer with C++ methods for
 * easier setter and getter access.
 *
 * Unfortunately this object has no information about whether the
 * underlying gnucash ::QofSession object is still alive or has been
 * deleted.
 */
class Session : public WeakPointer< ::QofSession >
{
public:
    typedef WeakPointer< ::QofSession > base_class;

    Session(element_type *ptr = 0)
            : base_class(ptr)
    {}

    // Now the actual functions on the object

    void begin(const QString& book_id, bool ignore_lock, bool create_if_nonexistent, bool force)
    {
        qof_session_begin(gobj(), book_id.toUtf8(), ignore_lock, create_if_nonexistent, force);
    }
    void load (QofPercentageFunc percentage_func)
    {
        qof_session_load(gobj(), percentage_func);
    }
    QofBackendError get_error ()
    {
        return qof_session_get_error(gobj());
    }
    QofBackendError pop_error ()
    {
        return qof_session_pop_error(gobj());
    }
    QString get_error_message() const
    {
        return QString::fromUtf8(qof_session_get_error_message(gobj()));
    }
    Glib::RefPtr<Book> get_book () const;

    QString get_file_path () const
    {
        return QString::fromUtf8(qof_session_get_file_path(gobj()));
    }

    QString get_url() const
    {
        return QString::fromUtf8(qof_session_get_url(gobj()));
    }

    bool save_in_progress() const
    {
        return qof_session_save_in_progress(gobj());
    }
    void save (QofPercentageFunc percentage_func)
    {
        qof_session_save(gobj(), percentage_func);
    }
};

std::pair<QString, QString> errorToStringPair(QofBackendError err);

} // END namespace gnc

#endif
