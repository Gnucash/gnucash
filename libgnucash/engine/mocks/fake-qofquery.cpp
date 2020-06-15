#include <config.h>

#include <list>

#include "fake-qofquery.h"
#include "gmock-qofbook.h"



/* class QofFakeQueryPool */

static class QofFakeQueryPool
{
public:
    void addQuery(QofFakeQuery *query)
    {
        m_queriesNew.push_back(query);
    }

    QofFakeQuery* requestQuery(QofIdTypeConst obj_type)
    {
        QofFakeQuery* query = nullptr;

        auto it = std::find_if(m_queriesNew.begin(), m_queriesNew.end(),
            [obj_type](QofFakeQuery const* query) {
                return (g_strcmp0(query->m_obj_type, obj_type) == 0);
            });
        if (it != m_queriesNew.end())
        {
            query = *it;
            m_queriesNew.erase(it);
            m_queriesUsed.push_back(query);
        }

        EXPECT_NE(query, nullptr);
        return query;
    }

    bool queryUsed(QofQuery *query)
    {
        auto it = std::find(m_queriesUsed.begin(), m_queriesUsed.end(), (QofFakeQuery*)query);

        return (it != m_queriesUsed.end());
    }

    void releaseQuery(QofFakeQuery *query)
    {
        ASSERT_TRUE(query_used((QofQuery*)query));
        auto it = std::find(m_queriesUsed.begin(), m_queriesUsed.end(), query);
        m_queriesUsed.erase(it);
        m_queriesConsumed.push_back(*it);
    }

    void removeQuery(QofFakeQuery *query)
    {
        ASSERT_FALSE(queryUsed((QofQuery*)query));
        auto it = std::find(m_queriesConsumed.begin(), m_queriesConsumed.end(), (QofFakeQuery*)query);
        if (it != m_queriesConsumed.end())
            m_queriesConsumed.erase(it);
        else
        {
            it = std::find(m_queriesNew.begin(), m_queriesNew.end(), (QofFakeQuery*)query);
            bool query_found = (it != m_queriesNew.end());
            ASSERT_TRUE(query_found);
            m_queriesNew.erase(it);
        }
    }

private:
    std::list<QofFakeQuery*> m_queriesNew {};
    std::list<QofFakeQuery*> m_queriesUsed {};
    std::list<QofFakeQuery*> m_queriesConsumed {};
} queryPool;



/* class QofFakeQuery */

QofFakeQuery::QofFakeQuery(QofIdTypeConst obj_type) :
    m_obj_type(obj_type)
{
    queryPool.addQuery(this);
}

QofFakeQuery::~QofFakeQuery()
{
    queryPool.removeQuery(this);
}



/* mock functions */

QofQuery *
qof_query_create_for (QofIdTypeConst obj_type)
{
    return (QofQuery*)queryPool.requestQuery(obj_type);
}

void
qof_query_destroy (QofQuery *query)
{
    queryPool.releaseQuery((QofFakeQuery*)query);
}

void
qof_query_set_book (QofQuery *query, QofBook *book)
{
    ASSERT_TRUE(queryPool.queryUsed(query));
    ASSERT_TRUE(QOF_IS_MOCK_BOOK(book));
    ((QofFakeQuery*)query)->setBook(book);
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
    ASSERT_TRUE(queryPool.queryUsed(query));
    ((QofFakeQuery*)query)->addDateMatchTT(use_start, stt, use_end, ett, op);
}

void
xaccQueryAddSingleAccountMatch(QofQuery *query, Account *acc, QofQueryOp op)
{
    ASSERT_TRUE(queryPool.queryUsed(query));
    ((QofFakeQuery*)query)->addSingleAccountMatch(acc, op);
}

GList *
qof_query_run (QofQuery *query)
{
    GList *matching_objects = NULL;
    bool  query_used        = queryPool.queryUsed(query);

    EXPECT_TRUE(query_used);
    if (query_used)
    {
        auto matchingObjects = ((QofFakeQuery*)query)->run();

        for (auto object : matchingObjects)
        {
            matching_objects = g_list_append(matching_objects, static_cast<gpointer>(object));
        }
    }

    return matching_objects;
}
