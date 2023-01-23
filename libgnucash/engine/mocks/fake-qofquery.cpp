/**
 * @file fake-qofquery.cpp
*/

#include <config.h>

#include <qofbook.h>

#include <list>

#include "fake-qofquery.h"



/* class QofFakeQueryPool */

static class QofFakeQueryPool
{
public:

    /* Add QofFakeQuery object to the pool */
    void add_query(QofFakeQuery *query)
    {
        m_queriesNew.push_back(query);
    }

    /* Request to use a QofFakeQuery object */
    QofFakeQuery* request_query(QofIdTypeConst obj_type)
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

    /* Check if a QofFakeQuery object is currently used, i.e. it has been
     * requested before */
    bool query_used(QofQuery *query)
    {
        auto it = std::find(m_queriesUsed.begin(), m_queriesUsed.end(), (QofFakeQuery*)query);

        return (it != m_queriesUsed.end());
    }

    /* Release a formerly requested QofFakeQuery object, which is not used
     * anymore */
    void release_query(QofFakeQuery *query)
    {
        ASSERT_TRUE(query_used((QofQuery*)query));
        auto it = std::find(m_queriesUsed.begin(), m_queriesUsed.end(), query);
        m_queriesConsumed.push_back(*it);
        m_queriesUsed.erase(it);
    }

    /* Remove a formerly added QofFakeQueryObject from the pool */
    void remove_query(QofFakeQuery *query)
    {
        ASSERT_FALSE(query_used((QofQuery*)query));
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
    queryPool.add_query(this);
}

QofFakeQuery::~QofFakeQuery()
{
    queryPool.remove_query(this);
}



/* mock functions */

QofQuery *
qof_query_create_for (QofIdTypeConst obj_type)
{
    return (QofQuery*)queryPool.request_query(obj_type);
}

void
qof_query_destroy (QofQuery *query)
{
    queryPool.release_query((QofFakeQuery*)query);
}

void
qof_query_set_book (QofQuery *query, QofBook *book)
{
    ASSERT_TRUE(queryPool.query_used(query));
    ASSERT_TRUE(QOF_IS_BOOK(book));
    ((QofFakeQuery*)query)->set_book(book);
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
    ASSERT_TRUE(queryPool.query_used(query));
    ((QofFakeQuery*)query)->add_date_match_tt(use_start, stt, use_end, ett, op);
}

void
xaccQueryAddSingleAccountMatch(QofQuery *query, Account *acc, QofQueryOp op)
{
    ASSERT_TRUE(queryPool.query_used(query));
    ((QofFakeQuery*)query)->add_single_account_match(acc, op);
}


GList *
qof_query_run (QofQuery *query)
{
    GList *matching_objects = NULL;
    bool  query_used        = queryPool.query_used(query);

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
