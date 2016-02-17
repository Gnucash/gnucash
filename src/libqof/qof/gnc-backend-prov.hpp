/********************************************************************\
 * qofbackend-prov.hpp -- Manage creation of a QofBackend subclass.  *
 *                                                                  *
 * Copyright 2016 John Ralls <jralls@ceridwen.us>                   *
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

#ifndef __GNC_BACKEND_PROV_HPP__
#define  __GNC_BACKEND_PROV_HPP__

#include <memory>

struct QofBackendProvider
{
    QofBackendProvider(const char* name, const char* type) :
        provider_name {name}, access_method {type} {}
    QofBackendProvider(QofBackendProvider&) = delete;
    QofBackendProvider(QofBackendProvider&&) = delete;
    virtual ~QofBackendProvider() {}

    /** Return a new, fully initialized backend.
     *
     * If the backend supports configuration, all configuration options
     * should be initialised to usable values here.
     * */
    virtual QofBackend * create_backend(void) = 0;

    /** \brief Distinguish two providers with same access method.

      More than 1 backend can be registered under the same access_method,
      so each one is passed the path to the data (e.g. a file) and
      should return TRUE only:
    -# if the backend recognises the type as one that it can load and write or
    -# if the path contains no data but can be used (e.g. a new session).

      \note If the backend can cope with more than one type, the backend
      should not try to store or cache the sub-type for this data.
      It is sufficient only to return TRUE if any ONE of the supported
      types match the incoming data. The backend should not assume that
      returning TRUE will mean that the data will naturally follow.
      */
    /*@ null @*/
    virtual bool type_check(const char*) = 0;
    /** Some arbitrary name given for this particular backend provider */
    const char * provider_name;

    /** The access method that this provider provides, for example,
     *  file:// http:// postgres:// or sqlite://, but without the :// at the end
     */
    const char * access_method;
};

using QofBackendProvider_ptr = std::unique_ptr<QofBackendProvider>;

/** Let the sytem know about a new provider of backends.  This function
 *  is typically called by the provider library at library load time.
 *  This function allows the backend library to tell the QOF infrastructure
 *  that it can handle URL's of a certain type.  Note that a single
 *  backend library may register more than one provider, if it is
 *  capable of handling more than one URL access method.
 */
void qof_backend_register_provider (QofBackendProvider_ptr&&);

#endif // __GNC_BACKEND_PROV_HPP__
