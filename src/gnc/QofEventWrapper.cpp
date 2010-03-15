/*
 * QofEventWrapper.cpp
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

#include "QofEventWrapper.hpp"
#include "engine/gnc-event.h"

#include "gnc/SplitListModel.hpp"
#include "gnc/Transaction.hpp"

namespace gnc
{

// Explicit instantiation to check for compiler errors
template class QofEventWrapper<SplitListModel, ::Transaction*>;

#define EVENT_TO_STR(result, input, evtype) \
    if ((input) & (evtype)) result += QString(result.isEmpty() ? "" : ",") + #evtype

QString qofEventToString(QofEventId event_type)
{
    if (event_type == QOF_EVENT_NONE)
        return "NONE";
    QString r;
    EVENT_TO_STR(r, event_type, QOF_EVENT_CREATE);
    EVENT_TO_STR(r, event_type, QOF_EVENT_MODIFY);
    EVENT_TO_STR(r, event_type, QOF_EVENT_DESTROY);
    EVENT_TO_STR(r, event_type, QOF_EVENT_ADD);
    EVENT_TO_STR(r, event_type, QOF_EVENT_REMOVE);

    EVENT_TO_STR(r, event_type, GNC_EVENT_ITEM_ADDED);
    EVENT_TO_STR(r, event_type, GNC_EVENT_ITEM_REMOVED);
    EVENT_TO_STR(r, event_type, GNC_EVENT_ITEM_CHANGED);

    if (r.isEmpty())
        r = QString::number(event_type);
    return r;
}


} // END namespace gnc
