#ifndef GNC_SESSION_HPP
#define GNC_SESSION_HPP

// gnucash includes
#include "config.h" // required by qof/qofutil.h
extern "C"
{
#include <glib/gi18n.h>
#include "qof.h"
#include "engine/gnc-hooks.h"
}

#include "gnc/ScopedPointer.hpp"
#include <boost/noncopyable.hpp>
#include <QString>

namespace gnc
{

class Book;

/** ScopedPointer object around a QofSession object, which also owns the
 * QofSession object.
 */
class Session : public ScopedPointer< ::QofSession >
            , boost::noncopyable
{
public:
    typedef ScopedPointer< ::QofSession > base_class;

    Session()
            : base_class()
    {}
    Session(element_type *ptr)
            : base_class(ptr, qof_session_destroy)
    {}
    void reset(element_type *ptr)
    {
        base_class::reset(ptr, qof_session_destroy);
    }
    using base_class::reset;
    static element_type* newInstance()
    {
        return qof_session_new();
    }

    // Now the actual functions on the object

    void begin(const std::string& book_id, bool ignore_lock, bool create_if_nonexistent)
    {
        qof_session_begin(get(), book_id.c_str(), ignore_lock, create_if_nonexistent);
    }
    void load (QofPercentageFunc percentage_func)
    {
        qof_session_load(get(), percentage_func);
    }
    QofBackendError get_error ()
    {
        return qof_session_get_error(get());
    }
    QofBackendError pop_error ()
    {
        return qof_session_pop_error(get());
    }
    QString get_error_message() const
    {
        return QString::fromUtf8(qof_session_get_error_message(get()));
    }
    Book get_book () const;

    QString get_file_path () const
    {
        return QString::fromUtf8(qof_session_get_file_path(get()));
    }

    QString get_url() const
    {
        return QString::fromUtf8(qof_session_get_url(get()));
    }

    bool save_in_progress() const
    {
        return qof_session_save_in_progress(get());
    }
    bool save_may_clobber_data () const
    {
        return qof_session_save_may_clobber_data(get());
    }
    void save (QofPercentageFunc percentage_func)
    {
        qof_session_save(get(), percentage_func);
    }


    void call_close_hooks ()
    {
        qof_session_call_close_hooks (get());
    }


};

std::pair<QString, QString> errorToStringPair(QofBackendError err);

} // END namespace gnc

#endif
