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
    QofFakeQuery(QofIdTypeConst obj_type);
    ~QofFakeQuery();

    MOCK_METHOD1(setBook, void(QofBook*));
    MOCK_METHOD5(addDateMatchTT, void(gboolean, time64, gboolean, time64, QofQueryOp));
    MOCK_METHOD2(addSingleAccountMatch, void(Account*, QofQueryOp));
    MOCK_METHOD0(run, std::vector<void*>());

    QofIdTypeConst m_obj_type;
};

#endif
