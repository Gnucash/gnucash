#include "config.h"
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
