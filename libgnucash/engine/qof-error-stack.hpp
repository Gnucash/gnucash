/********************************************************************\
 * qof-error-stack.hpp -- A stack of backend errors                 *
 * Copyright 2024 GnuCash Development Team                          *
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

#ifndef __QOF_ERROR_STACK_HPP__
#define __QOF_ERROR_STACK_HPP__

#include "qofbackend.h"
#include <string>

/**
 * @brief A class to manage a stack of backend errors.
 *
 * This class encapsulates the error reporting logic previously split between
 * QofSession and QofBackend. It currently maintains a "stack" of one error,
 * where the first error reported is the one that is preserved until cleared.
 */
class QofErrorStack
{
public:
    QofErrorStack() = default;

    /** Set the error value only if there isn't one already. */
    void set_error(QofBackendError err) noexcept;

    /** Retrieve the currently-stored error and clear it. */
    QofBackendError fetch_error() noexcept;

    /** Retrieve the currently-stored error without clearing it. */
    QofBackendError peek_error() const noexcept;

    /** Report if there is an error. */
    bool check_error() const noexcept;

    /** Set a descriptive message for the error. */
    void set_message(std::string msg) noexcept;

    /** Retrieve and clear the stored error message. */
    std::string fetch_message() noexcept;

    /** Retrieve the stored error message without clearing it. */
    const std::string& peek_message() const noexcept;

    /** Set both error and message, overwriting any existing ones. */
    void push_error(QofBackendError err, std::string msg) noexcept;

    /** Clear both error and message. */
    void clear() noexcept;

private:
    QofBackendError m_err{ERR_BACKEND_NO_ERR};
    std::string m_msg;
};

#endif /* __QOF_ERROR_STACK_HPP__ */
