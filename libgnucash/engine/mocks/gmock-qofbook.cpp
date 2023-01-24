#include "gmock-qofbook.h"

struct _QofMockBookClass
{
    QofInstanceClass parent_class;
};
typedef struct _QofMockBookClass QofMockBookClass;

G_DEFINE_TYPE(QofMockBook, qof_mockbook, QOF_TYPE_INSTANCE)

static void
qof_mockbook_init (QofMockBook *inst)
{
    // function is unused, initialization is done in the QofMockBook's C++ constructor
}

static void
qof_mockbook_class_init(QofMockBookClass *klass)
{
    // function is unused, class functions are defined in C++ code
}


GType qof_book_get_type(void)
{
    return qof_mockbook_get_type();
}

gboolean
qof_book_use_split_action_for_num_field (const QofBook *book)
{
    SCOPED_TRACE("");
    auto mockbook = qof_mockbook(book);
    return mockbook ? mockbook->use_split_action_for_num_field() : FALSE;
}

