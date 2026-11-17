/********************************************************************\
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
 * along with this program; if not, see                             *
 * <https://www.gnu.org/licenses/>                                  *
 *                                                                  *
\********************************************************************/

#include <config.h>
#include "gnc-event.h"

const char* qofeventid_to_string(QofEventId id)
{
    switch (id)
    {
    case 0:
        return "NONE";
    case QOF_EVENT_CREATE:
        return "CREATE";
    case QOF_EVENT_MODIFY:
        return "MODIFY";
    case QOF_EVENT_DESTROY:
        return "DESTROY";
    case QOF_EVENT_ADD:
        return "ADD";
    case QOF_EVENT_REMOVE:
        return "REMOVE";

    case GNC_EVENT_ITEM_ADDED:
        return "ITEM_ADDED";
    case GNC_EVENT_ITEM_REMOVED:
        return "ITEM_REMOVED";
    case GNC_EVENT_ITEM_CHANGED:
        return "ITEM_CHANGED";

    default:
        return "<unknown, maybe multiple>";
    }
}
