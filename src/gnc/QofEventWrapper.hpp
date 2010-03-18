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
extern "C"
{
#include "qof.h"
}

#include <QString>
#include <QDebug>

namespace gnc
{

/** Conversion of the event_type to string (for debugging) */
QString qofEventToString(QofEventId event_type);

/** Retrieval of the Qof instance class name (for debugging) */
inline const char* getQofType( ::QofInstance* obj)
{
    return obj->e_type;
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
                    SlotFunc recvSlot,
                    const char* qof_type)
            : m_receiver(receiver)
            , m_receiveFunc(recvSlot)
            , m_qof_type(qof_type)
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
