#ifndef GMOCK_QOFBOOK_H
#define GMOCK_QOFBOOK_H

#include <gmock/gmock.h>

#include <qofbook.h>
#include <qofbook-p.h>

#include "gmock-gobject.h"
#include "gmock-Split.h"


GType qof_mock_book_get_type(void);

#define QOF_TYPE_MOCK_BOOK   (qof_mock_book_get_type ())
#define QOF_IS_MOCK_BOOK(o)  (G_TYPE_CHECK_INSTANCE_TYPE ((o), QOF_TYPE_MOCK_BOOK))


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
        return mock_g_object_new (QOF_TYPE_MOCK_BOOK, NULL, size);
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

    MOCK_METHOD0(mallocSplit, Split *());
    MOCK_METHOD0(useSplitActionForNumField, gboolean());

protected:
    // Protect destructor to avoid MockQofBook objects to be created on stack. MockQofBook
    // objects can only be dynamically created, since they are derived from GObject.
    ~QofMockBook() {}
};

#endif
