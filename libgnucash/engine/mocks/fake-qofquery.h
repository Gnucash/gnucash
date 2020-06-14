#ifndef FAKE_QOFQUERY_H
#define FAKE_QOFQUERY_H

#include <gmock/gmock.h>

#include <qofquery.h>

extern "C"
{
#include <Query.h>
}

// Fake object providing functionality similar to QofQuery
// Note: QofQuery is a typedef for struct _QofQuery, which is is not public
class QofFakeQuery
{
public:
    QofFakeQuery() {};

    MOCK_METHOD1(setBook, void(QofBook*));
    MOCK_METHOD0(destroy, void());
    MOCK_METHOD5(addDateMatchTT, void(gboolean, time64, gboolean, time64, QofQueryOp));
    MOCK_METHOD2(addSingleAccountMatch, void(Account*, QofQueryOp));
    MOCK_METHOD0(run, std::vector<void*>());
};

/*
// typed mock up for QofQuery
template <typename T>
class MockQofQueryWithType : MockQofQuery
{
public:
    // \todo: write constructor
    MOCK_METHOD0_T(run, std::list<T*>());
};
*/

class QofQueryFactory
{
public:
//    MOCK_METHOD0(createForSplit, MockQofQueryWithType<Split>*());
    MOCK_METHOD0(create, QofFakeQuery*());
} qof_query_factory;

#endif
