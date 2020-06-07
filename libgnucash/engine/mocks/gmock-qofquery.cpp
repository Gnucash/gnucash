#include <config.h>

#include "gmock-qofquery.h"
#include "gmock-qofbook.h"

QofQuery *
qof_query_create_for (QofIdTypeConst obj_type)
{
    return (QofQuery*)qof_query_factory.create();
/*
    // \todo create typed query objects
    QofQuery *ret = NULL;

    if (g_strcmp0(obj_type, GNC_ID_SPLIT) == 0)
        ret = (QofQuery*)qof_query_factory.createForSplit();
//    else
//        FAIL();

    return ret;
*/
}

void
qof_query_set_book (QofQuery *query, QofBook *book)
{
    ASSERT_TRUE(QOF_IS_MOCK_BOOK(book));
    ((QofMockQuery*)query)->setBook(book);
}

GList *
qof_query_run (QofQuery *query)
{
    GList *matching_objects = NULL;

    // \todo use typed mock objects
    auto matchingObjects = ((QofMockQuery*)query)->run();

    for (auto object : matchingObjects)
    {
        matching_objects = g_list_append(matching_objects, static_cast<gpointer>(object));
    }

    return matching_objects;
}

void
xaccQueryAddDateMatchTT (
        QofQuery *query,
        gboolean use_start,
        time64 stt,
        gboolean use_end,
        time64 ett,
        QofQueryOp op)
{
    ((QofMockQuery*)query)->addDateMatchTT(use_start, stt, use_end, ett, op);
}

void
xaccQueryAddSingleAccountMatch(QofQuery *query, Account *acc, QofQueryOp op)
{
    ((QofMockQuery*)query)->addSingleAccountMatch(acc, op);
}

void
qof_query_destroy (QofQuery *query)
{
    ((QofMockQuery*)query)->destroy();
}
