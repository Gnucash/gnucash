/**
 * @file fake-qofquery.h
 *
 * @brief Mocking qof queries
*/

#ifndef FAKE_QOFQUERY_H
#define FAKE_QOFQUERY_H

#include <gmock/gmock.h>

#include <qofquery.h>

#include <Query.h>

/** Fake object providing functionality similar to QofQuery
 *
 * @note QofQuery is a @c typedef for @c struct _QofQuery, which is not
 * public. Therefore class QofFakeQuery is not derived from QofQuery.
 *
 * To use a QofFakeQuery object simply create it before the GnuCash code
 * performs a query. Check that the QofFakeQuery object is created with the
 * correct object type. Also define all expectations and return values on the
 * created QofFakeQuery object before the GnuCash code performs the query.
 *
 * After the query is finished, the QofFakeQuery object can be destroyed. A
 * QofFakeQuery object can only be used once.
 *
 * Internally each created QofFakeQuery object is registered at a
 * QofFakeQueryPool, which provides it to the GnuCash code on request. This
 * pool observes the life-cycle of each QofFakeQuery object. The following
 * steps are expected to be done on each QofFakeQuery object in the
 * specified order:
 *    -# create QofFakeQuery object (test application)
 *    -# call qof_query_create_for() (GnuCash code)
 *    -# call qof_query_run() (GnuCash code)
 *    -# call qof_query_destroy() (GnuCash code)
 *    -# destroy QofFakeQuery object (test application)
 *
 * The calls to qof_query_create_for(), qof_query_run() and qof_query_destroy()
 * are optional, but
 *    -  qof_query_create_for() and qof_query_destroy() have to be called in
 *       pairs
 *    -  if qof_query_run() is called, qof_query_create_for() has to be called
 *       before as well
 *
 * Several GTest assertions are implemented to signal violations of the
 * QofFakeQuery object life-cycle.
 *
 * @note If you want to check, that a certain query is run by the GnuCash code,
 * then define the appropriate expectations on the QofFakeQuery object in your
 * test application.
 */
class QofFakeQuery
{
public:
    QofFakeQuery(QofIdTypeConst obj_type);
    ~QofFakeQuery();

    MOCK_METHOD1(set_book, void(QofBook*));
    MOCK_METHOD5(add_date_match_tt, void(gboolean, time64, gboolean, time64, QofQueryOp));
    MOCK_METHOD2(add_single_account_match, void(Account*, QofQueryOp));
    MOCK_METHOD0(run, std::vector<void*>());

    QofIdTypeConst m_obj_type;
};

#endif
