/********************************************************************\
 * qof-error-stack.cpp -- A stack of backend errors                 *
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

#include <config.h>
#include "qof-error-stack.hpp"
#include <utility>

void
QofErrorStack::set_error(QofBackendError err) noexcept
{
    /* use stack-push semantics. Only the earliest error counts */
    if (m_err != ERR_BACKEND_NO_ERR) return;
    m_err = err;
}

QofBackendError
QofErrorStack::fetch_error() noexcept
{
    /* use 'stack-pop' semantics */
    auto err = m_err;
    m_err = ERR_BACKEND_NO_ERR;
    return err;
}

QofBackendError
QofErrorStack::peek_error() const noexcept
{
    return m_err;
}

bool
QofErrorStack::check_error() const noexcept
{
    return m_err != ERR_BACKEND_NO_ERR;
}

void
QofErrorStack::set_message(std::string msg) noexcept
{
    m_msg = std::move(msg);
}

std::string
QofErrorStack::fetch_message() noexcept
{
    auto msg = std::move(m_msg);
    m_msg.clear();
    return msg;
}

const std::string&
QofErrorStack::peek_message() const noexcept
{
    return m_msg;
}

void
QofErrorStack::push_error(QofBackendError err, std::string msg) noexcept
{
    m_err = err;
    m_msg = std::move(msg);
}

void
QofErrorStack::clear() noexcept
{
    m_err = ERR_BACKEND_NO_ERR;
    m_msg.clear();
}
