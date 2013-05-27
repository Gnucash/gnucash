/*
 * Transaction.hpp
 * Copyright (C) 2011 Christian Stimming
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, contact:
 *
 * Free Software Foundation           Voice:  +1-617-542-5942
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
 * Boston, MA  02110-1301,  USA       gnu@gnu.org
 */

#ifndef GNC_TRANSACTION_HPP
#define GNC_TRANSACTION_HPP

// gnucash includes
#include "config.h"
extern "C"
{
#include "qof.h"
#include "engine/Transaction.h"
}

#include "Account.hpp"
#include "Book.hpp"
#include "Commodity.hpp"
#include "Numeric.hpp"
#include "GncInstance.hpp"
#include "Split.hpp"

#include <glibmm/object.h>
#include <glibmm/date.h>
#include <glibmm/ustring.h>
#include <vector>

namespace gnc
{
class Transaction_Class;
} // END namespace gnc

namespace gnc
{
class Account;
class Split;
class TmpSplit;

/** Wrapper around a gnucash ::Transaction pointer with C++ methods for
 * easier setter and getter access.
 */
class Transaction : public GncInstance
{
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    typedef Transaction CppObjectType;
    typedef Transaction_Class CppClassType;
    typedef ::Transaction BaseObjectType;
    typedef ::TransactionClass BaseClassType;

private:
    friend class Transaction_Class;
    static CppClassType transaction_class_;

private:
    // noncopyable
    Transaction(const Transaction&);
    Transaction& operator=(const Transaction&);

protected:
    explicit Transaction(const Glib::ConstructParams& construct_params);
    explicit Transaction(::Transaction* castitem);

#endif /* DOXYGEN_SHOULD_SKIP_THIS */

public:
    virtual ~Transaction();
#ifndef DOXYGEN_SHOULD_SKIP_THIS
    static GType get_type()      G_GNUC_CONST;
    static GType get_base_type() G_GNUC_CONST;
#endif

    ///Provides access to the underlying C GObject.
    ::Transaction*       gobj()
    {
        return reinterpret_cast< ::Transaction*>(gobject_);
    }

    ///Provides access to the underlying C GObject.
    const ::Transaction* gobj() const
    {
        return reinterpret_cast< ::Transaction*>(gobject_);
    }

    ///Provides access to the underlying C instance. The caller is responsible for unrefing it. Use when directly setting fields in structs.
    ::Transaction* gobj_copy();

private:
public:

    void begin_edit()
    {
        xaccTransBeginEdit(gobj());
    }
    void commit_edit()
    {
        xaccTransCommitEdit(gobj());
    }
    void rollback_edit()
    {
        xaccTransRollbackEdit(gobj());
    }
    bool is_open() const
    {
        return xaccTransIsOpen(gobj());
    }


    Glib::ustring get_num() const
    {
        return xaccTransGetNum(gobj());
    }
    void set_num(const Glib::ustring& v)
    {
        xaccTransSetNum(gobj(), v.c_str());
    }

    Glib::ustring get_description() const
    {
        return xaccTransGetDescription(gobj());
    }
    void set_description(const Glib::ustring& v)
    {
        xaccTransSetDescription(gobj(), v.c_str());
    }

    Glib::ustring get_notes() const
    {
        return xaccTransGetNotes(gobj());
    }
    void set_notes(const Glib::ustring& v)
    {
        xaccTransSetNotes(gobj(), v.c_str());
    }

    int get_num_splits() const
    {
        return xaccTransCountSplits(gobj());
    }
    Glib::RefPtr<Split> find_split_by_account(const Account& acc) const;
    void append_split(Glib::RefPtr<Split> split);
    Glib::RefPtr<Split> get_split(int i) const;
    int get_split_index(const Split& split) const;
    ::SplitList* get_split_list() const
    {
        return xaccTransGetSplitList(gobj());
    }

    Glib::RefPtr<Commodity> get_currency() const
    {
        return Glib::wrap(xaccTransGetCurrency(gobj()));
    }
    void set_currency(const Glib::RefPtr<Commodity>& c)
    {
        if (c) xaccTransSetCurrency(gobj(), c->gobj());
    }

    Numeric get_imbalance_value() const
    {
        return xaccTransGetImbalanceValue(gobj());
    }
    bool is_balanced() const
    {
        return xaccTransIsBalanced(gobj());
    }
    Numeric get_account_conv_rate(const Account& acc) const
    {
        return xaccTransGetAccountConvRate(gobj(), acc.gobj());
    }

    void set_date_posted(const Glib::Date& d)
    {
        xaccTransSetDatePostedGDate(gobj(), *d.gobj());
    }
    Glib::Date get_date_posted() const
    {
        return Glib::Date(xaccTransGetDatePostedGDate(gobj()));
    }
    void set_date_entered(time_t t)
    {
        xaccTransSetDateEnteredSecs(gobj(), t);
    }
//    void set_date_entered(const Glib::DateTime& t) { xaccTransSetDateEnteredSecs(gobj(), t.toTime_t()); }
    time_t get_date_entered_tt() const
    {
        return timespecToTime64 (xaccTransRetDateEnteredTS(gobj()));
    }
    //Glib::DateTime get_date_entered() const { return toGDateTime(xaccTransRetDateEnteredTS(gobj())); }

    static ::Transaction* new_instance(const Glib::RefPtr<Book> b);
};


/** This is a temporary transaction. Each of this tmp transactions has
 * all data fields just like a "real" transaction, but it is not (yet)
 * added to the respective Account and Book. In other words, it is not
 * stored in the book yet.
 *
 * For this reason this class supports a full copy-by-value, which
 * will create new independent instances of all data fields. */
class TmpTransaction
{
public:
    typedef std::vector<TmpSplit> TmpSplitList;

    /** Creates an empty tmp transaction */
    TmpTransaction();

    /** Creates a tmp transaction whose content is copied from the
     * given real transaction */
    TmpTransaction(const Transaction& t);

    /** Clears all data fields of this transaction, including deletion
     * of all splits stored here. */
    void clear();

    /** Clears all data fields, but does not delete the splits and
     * instead only resets the data fields of all splits */
    void reset_content();

    /** Copies the content of this tmp transaction into the given real
     * transaction. */
    void copy_to(Glib::RefPtr<Transaction> t) const;

    /** Allocates a new real transaction in the Book and Account as
     * stored in the tmp transaction, copies the content of this tmp
     * transaction into the newly allocated one, and returns the
     * pointer to the newly created real transaction. */
    Glib::RefPtr<Transaction> create_as_real() const;

    Glib::ustring get_num() const
    {
        return m_num;
    }
    void set_num(const Glib::ustring& v)
    {
        m_num = v;
    }

    Glib::ustring get_description() const
    {
        return m_description;
    }
    void set_description(const Glib::ustring& v)
    {
        m_description = v;
    }

    void push_back(const TmpSplit& s);
    const TmpSplitList& get_splits() const
    {
        return m_splits;
    }
    TmpSplitList& get_splits()
    {
        return m_splits;
    }
    int get_num_splits() const
    {
        return m_splits.size();
    }

    Glib::RefPtr<Commodity> get_commodity() const
    {
        return m_commodity;
    }
    void set_commodity(const Glib::RefPtr<Commodity>& v)
    {
        m_commodity = v;
    }

    Glib::Date get_date_posted() const
    {
        return m_datePosted;
    }
    void set_date_posted(const Glib::Date& v)
    {
        m_datePosted = v;
    }

    time_t get_date_entered_tt() const
    {
        return m_dateTimeEntered;
    }
    void set_date_entered(time_t v)
    {
        m_dateTimeEntered = v;
    }
    //Glib::DateTime get_date_entered() const { return m_dateTimeEntered; }
    //void set_date_entered(const Glib::DateTime& v) { m_dateTimeEntered = v; }

private:
    Glib::ustring m_num;
    Glib::ustring m_description;
    Glib::ustring m_notes;
    TmpSplitList m_splits;
    Glib::RefPtr<Commodity> m_commodity;
    Glib::Date m_datePosted;
    time_t m_dateTimeEntered;
    //Glib::DateTime m_dateTimeEntered;
};

} // END namespace gnc

namespace Glib
{
/** A Glib::wrap() method for this object.
 *
 * @param object The C instance.
 * @param take_copy False if the result should take ownership of the C instance. True if it should take a new copy or ref.
 * @result A C++ instance that wraps this C instance.
 *
 * @relates Gio::FileInfo
 */
Glib::RefPtr<gnc::Transaction> wrap(::Transaction* object, bool refuse_ownership = true);
}

#endif
