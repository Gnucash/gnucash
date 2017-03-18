/*
 * QofEventWrapper.hpp
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

#ifndef GNC_QOFEVENTWRAPPER_HPP
#define GNC_QOFEVENTWRAPPER_HPP

// gnucash includes
#include "config.h" // required by qof/qofutil.h
#include "libqof/qof/guid.hpp"
extern "C"
{
#include "qof.h"
#include "engine/gnc-engine.h"
#include "engine/Account.h"
#include "engine/Transaction.h"
#include "engine/gnc-commodity.h"
}

#include <QString>
#include <QDebug>
#include <stdexcept>

namespace gnc
{

/** Conversion of the event_type to string (for debugging) */
QString qofEventToString(QofEventId event_type);

/** Retrieval of the Qof instance class name (for debugging) */
inline const char* getQofType( ::QofInstance* obj)
{
    return obj->e_type;
}

namespace detail
{
    template<class ValuePtrT> inline const char* getQofTypeT()
    {
        Q_ASSERT(false);
        // This would need a BOOST_STATIC_ASSERT(sizeof(T)==0) to
        // trigger a compile-time warning in case this function is
        // erroneously instantiated.
        // This template must not be instantiated. Instead, for each
        // templated usage there must exist a specialization which
        // will return the correct QOF_ID string!
        throw std::runtime_error("Should not have been instantiated");
    }
    template<> inline const char* getQofTypeT< ::Transaction*>()
    {
        return GNC_ID_TRANS;
    }
    template<> inline const char* getQofTypeT< ::Account*>()
    {
        return GNC_ID_ACCOUNT;
    }
    template<> inline const char* getQofTypeT< ::QofBook*>()
    {
        return QOF_ID_BOOK;
    }
    template<> inline const char* getQofTypeT< ::QofSession*>()
    {
        return QOF_ID_SESSION;
    }
    template<> inline const char* getQofTypeT< ::gnc_commodity*>()
    {
        return GNC_ID_COMMODITY;
    }
    template<> inline const char* getQofTypeT< ::Split*>()
    {
        return GNC_ID_SPLIT;
    }
}

/** Template wrapper class for objects which want to receive
 * notifications from the qof_event system in any of their member
 * functions.
 *
 * The receiver's class is the first template argument; the argument
 * type of the to-be-called member function is the second template
 * (usually a pointer type). */
template<class ReceiverT, class ValuePtrT, typename SlotFunc = void (ReceiverT::*)(ValuePtrT, QofEventId)>
class QofEventWrapper
{
public:
    QofEventWrapper(ReceiverT& receiver,
                    SlotFunc recvSlot)
            : m_receiver(receiver)
            , m_receiveFunc(recvSlot)
            , m_qof_type(gnc::detail::getQofTypeT<ValuePtrT>())
    {
        m_handler_id = qof_event_register_handler(QofEventWrapper::event_handler, this);
    }

    ~QofEventWrapper()
    {
        qof_event_unregister_handler(m_handler_id);
    }

    static void event_handler (::QofInstance *entity,  QofEventId event_type,
                               gpointer user_data, gpointer event_data)
    {
        QofEventWrapper* wrapper = static_cast<QofEventWrapper *>(user_data);
        wrapper->private_event_handler(entity, event_type, event_data);
    }

private:
    void private_event_handler (::QofInstance *entity,  QofEventId event_type,
                                gpointer event_data)
    {
//         qDebug() << "private_event_handler, id=" << qofEventToString(event_type)
//         << "entity=" << getQofType(entity);

        // Verify that we have the correct QofInstance type and also
        // correct event type
        if (!QOF_CHECK_TYPE(entity, m_qof_type))
            return;
//         if ((event_type & m_event_type) == 0)
//             return;

//         qDebug() << "private_event_handler, id=" << qofEventToString(event_type)
//         << "entity=" << getQofType(entity);

        ValuePtrT vptr = reinterpret_cast<ValuePtrT>(entity);

        // Call the pointer-to-member function with that weird C++
        // syntax
        (m_receiver.*m_receiveFunc)(vptr, event_type);
    }

    ReceiverT& m_receiver;
    SlotFunc m_receiveFunc;
    const char* m_qof_type;
    gint m_handler_id;
};

} // END namespace gnc

#endif
