#ifndef GMOCK_QOFBOOK_H
#define GMOCK_QOFBOOK_H

#include <gmock/gmock.h>

#include <qofbook.h>
#include <qofbook-p.h>
#include <Split.h>

#include "gmock-gobject.h"


GType qof_mockbook_get_type(void);

#define QOF_TYPE_MOCKBOOK   (qof_mockbook_get_type ())
#define QOF_IS_MOCKBOOK(o)  (G_TYPE_CHECK_INSTANCE_TYPE ((o), QOF_TYPE_MOCKBOOK))


// mock up for QofBook
class QofMockBook : public QofBook
{
public:
    QofMockBook()
    {
        hash_of_collections   = nullptr;
        data_tables           = nullptr;
        data_table_finalizers = nullptr;

        book_open     = 'n';
        read_only     = TRUE;
        session_dirty = FALSE;

        version = 0;

        cached_num_field_source_isvalid      = FALSE;
        cached_num_days_autoreadonly_isvalid = FALSE;
    }
    void* operator new(size_t size)
    {
        return mock_g_object_new (QOF_TYPE_MOCKBOOK, NULL, size);
    }

    // define separate free() function since destructor is protected
    void free()
    {
        delete this;
    }
    void operator delete(void* book, size_t size)
    {
        mock_g_object_unref(book, size);
    }

    MOCK_METHOD0(malloc_split, Split *());
    MOCK_CONST_METHOD0(use_split_action_for_num_field, gboolean());

protected:
    // Protect destructor to avoid MockQofBook objects to be created on stack. MockQofBook
    // objects can only be dynamically created, since they are derived from GObject.
    ~QofMockBook() {}
};


// type conversion functions
static inline QofMockBook*
qof_mockbook (QofBook *book)
{
    if (QOF_IS_MOCKBOOK(book))
        return static_cast<QofMockBook*>(book);
    ADD_FAILURE() << "Expected 'book' to be of type 'QofMockBook'";
    return nullptr;
}

static inline const QofMockBook*
qof_mockbook (const QofBook *book)
{
    if (QOF_IS_MOCKBOOK(book))
        return static_cast<const QofMockBook*>(book);
    ADD_FAILURE() << "Expected 'book' to be of type 'QofMockBook'";
    return nullptr;
}


#endif
