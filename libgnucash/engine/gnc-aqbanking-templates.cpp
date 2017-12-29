/********************************************************************
 * gnc-aqbanking-templates.cpp implements transaction templates     *
 * for AQBanking.                                                   *
 * Copyright 2015 John Ralls <jralls@ceridwen.us>                   *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
 ********************************************************************/

/** Class for managing AQBanking Transaction Templates.
 */

#include <string>

extern "C"
{
#include "gnc-aqbanking-templates.h"
}

#include "qofinstance-p.h"
#include "kvp-frame.hpp"
#include "gnc-rational.hpp"

namespace {
    static const char* TT_NAME {"name"};
    static const char* TT_RNAME {"rnam"};
    static const char* TT_RACC {"racc"};
    static const char* TT_RBCODE {"rbcd"};
    static const char* TT_PURPOS {"purp"};
    static const char* TT_PURPOSCT {"purc"};
    static const char* TT_AMOUNT {"amou"};
}

struct _GncABTransTempl
{
public:
    _GncABTransTempl () :
        m_name(), m_recipient_name(), m_recipient_account(),
        m_recipient_bankcode(), m_amount(gnc_numeric_zero()), m_purpose(),
        m_purpose_continuation() {}
    _GncABTransTempl (const std::string& name,
                      const std::string& recip_name,
                      const std::string& recip_account,
                      const std::string& recip_code,
                      const GncRational& amount,
                      const std::string& purpose,
                      const std::string& purpose_cont) :
        m_name(name), m_recipient_name(recip_name),
        m_recipient_account(recip_account), m_recipient_bankcode(recip_code),
        m_amount(amount), m_purpose(purpose),
        m_purpose_continuation(purpose_cont) {}
    KvpFrame* make_kvp_frame();
    const char* name() const { return m_name.c_str(); }
    const char* recipient_name() const { return m_recipient_name.c_str(); }
    const char* recipient_account() const
        {
            return m_recipient_account.c_str();
        }
    const char* recipient_bankcode() const
        {
            return m_recipient_bankcode.c_str();
        }
    const GncRational amount() const { return m_amount; }
    const char* purpose() const { return m_purpose.c_str(); }
    const char* purpose_continuation() const
        {
            return m_purpose_continuation.c_str();
        }
    void set_name (const char* name) { m_name = name; }
    void set_recipient_name (const char* name) { m_recipient_name = name; }
    void set_recipient_account (const char* account) {
        m_recipient_account = account;
    }
    void set_recipient_bankcode (const char* code) {
        m_recipient_bankcode = code;
    }
    void set_amount (GncRational amount) { m_amount = amount; }
    void set_purpose (const char* purpose) { m_purpose = purpose; }
    void set_purpose_continuation (const char* name) { m_name = name; }
private:
    std::string m_name;
    std::string m_recipient_name;
    std::string m_recipient_account;
    std::string m_recipient_bankcode;
    GncRational m_amount;
    std::string m_purpose;
    std::string m_purpose_continuation;
};

KvpFrame*
_GncABTransTempl::make_kvp_frame()
{
    auto frame = new KvpFrame;
    frame->set({TT_NAME}, new KvpValue(m_name.c_str()));
    frame->set({TT_RNAME}, new KvpValue(m_recipient_name.c_str()));
    frame->set({TT_RACC}, new KvpValue(m_recipient_account.c_str()));
    frame->set({TT_RBCODE}, new KvpValue(m_recipient_bankcode.c_str()));
    frame->set({TT_AMOUNT}, new KvpValue(m_amount));
    frame->set({TT_PURPOS}, new KvpValue(m_purpose.c_str()));
    frame->set({TT_PURPOSCT}, new KvpValue(m_purpose_continuation.c_str()));
    return frame;
}

GncABTransTempl*
gnc_ab_trans_templ_new()
{
    return new _GncABTransTempl;
}

GncABTransTempl*
gnc_ab_trans_templ_new_full(const gchar *name, const gchar *recp_name,
                            const gchar *recp_account,
                            const gchar *recp_bankcode, gnc_numeric amount,
                            const gchar *purpose, const gchar *purpose_cont)
{
    return new _GncABTransTempl(name, recp_name, recp_account, recp_bankcode,
                                amount, purpose, purpose_cont);
}

GList*
gnc_ab_trans_templ_list_new_from_book(QofBook *b)
{
    GList *retval = NULL;
    auto toplevel = qof_instance_get_slots (QOF_INSTANCE (b));
    auto slot = toplevel->get_slot({"hbci", "template-list"});
    if (slot == nullptr)
        return retval;
    auto list = slot->get<GList*>();
    for (auto node = list; node != NULL; node = g_list_next (node))
    {
        KvpFrame *frame = static_cast<KvpValue*>(node->data)->get<KvpFrame*>();
        auto c_func = [frame](const char* key)
            { auto slot = frame->get_slot({key});
              return slot == nullptr ? std::string("") : std::string(slot->get<const char*>());};
        auto n_func = [frame](const char* key)
            { auto slot = frame->get_slot({key});
              return slot == nullptr ? gnc_numeric_zero() : slot->get<gnc_numeric>();};
        auto amt_slot = frame->get_slot({TT_AMOUNT});
        auto templ = new _GncABTransTempl (c_func(TT_NAME), c_func(TT_RNAME),
                                           c_func(TT_RACC), c_func(TT_RBCODE),
                                           n_func(TT_AMOUNT), c_func(TT_PURPOS),
                                           c_func(TT_PURPOSCT));
        retval = g_list_prepend (retval, templ);
    }
    retval = g_list_reverse (retval);
    return retval;
}

void
gnc_ab_trans_templ_free (GncABTransTempl *t)
{
    delete t;
}

void
gnc_ab_trans_templ_list_free (GList *l)
{
    for(GList *node = l; node != NULL; node = g_list_next(node))
        delete static_cast<_GncABTransTempl*>(node->data);
}
static void*
copy_list_value(const void* pvalue, void* pdata)
{
    auto new_value = new KvpValue(*static_cast<const KvpValue*>(pvalue));
    return new_value;
}

void
gnc_ab_set_book_template_list (QofBook *b, GList *template_list)
{
    GList *kvp_list = NULL;
    for (auto node = template_list; node != NULL; node = g_list_next (node))
    {
        auto templ = static_cast<_GncABTransTempl*>(node->data);
        auto value = new KvpValue(templ->make_kvp_frame());
        kvp_list = g_list_prepend (kvp_list, value);
    }
    kvp_list = g_list_reverse (kvp_list);
    auto value = new KvpValue(g_list_copy_deep(kvp_list, copy_list_value,
                                               nullptr));
    qof_book_begin_edit(b);
    KvpFrame *toplevel = qof_instance_get_slots (QOF_INSTANCE (b));
    delete toplevel->set_path({"hbci", "template-list"}, value);
    qof_instance_set_dirty_flag (QOF_INSTANCE (b), TRUE);
    qof_book_commit_edit(b);
}

const gchar *
gnc_ab_trans_templ_get_name(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->name();
}

const gchar *
gnc_ab_trans_templ_get_recp_name(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->recipient_name();
}

const gchar *
gnc_ab_trans_templ_get_recp_account(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->recipient_account();
}

const gchar *
gnc_ab_trans_templ_get_recp_bankcode(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->recipient_bankcode();
}

gnc_numeric
gnc_ab_trans_templ_get_amount(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, gnc_numeric_zero());
    return t->amount();
}

const gchar *
gnc_ab_trans_templ_get_purpose(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->purpose();
}

const gchar *
gnc_ab_trans_templ_get_purpose_cont(const GncABTransTempl *t)
{
    g_return_val_if_fail(t, NULL);
    return t->purpose_continuation();
}

void
gnc_ab_trans_templ_set_name(GncABTransTempl *t, const gchar *name)
{
    g_return_if_fail(t);
    t->set_name(name);
}

void
gnc_ab_trans_templ_set_recp_name(GncABTransTempl *t, const gchar *recp_name)
{
    g_return_if_fail(t);
    t->set_recipient_name(recp_name);
}

void
gnc_ab_trans_templ_set_recp_account(GncABTransTempl *t,
                                    const gchar *recp_account)
{
    g_return_if_fail(t);
    t->set_recipient_account(recp_account);
}

void
gnc_ab_trans_templ_set_recp_bankcode(GncABTransTempl *t,
                                     const gchar *recp_bankcode)
{
    g_return_if_fail(t);
    t->set_recipient_bankcode(recp_bankcode);
}

void
gnc_ab_trans_templ_set_amount(GncABTransTempl *t, gnc_numeric amount)
{
    g_return_if_fail(t);
    t->set_amount(amount);
}

void
gnc_ab_trans_templ_set_purpose(GncABTransTempl *t, const gchar *purpose)
{
    g_return_if_fail(t);
    t->set_purpose(purpose);
}

void
gnc_ab_trans_templ_set_purpose_cont(GncABTransTempl *t,
                                    const gchar *purpose_cont)
{
    g_return_if_fail(t);
    t->set_purpose_continuation (purpose_cont);
}
