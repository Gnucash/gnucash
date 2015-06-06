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
#include "kvp_frame.h"
#include "qofinstance-p.h"
}

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
    auto frame = kvp_frame_new();
    kvp_frame_set_slot(frame, TT_NAME, kvp_value_new_string(m_name.c_str()));
    kvp_frame_set_slot(frame, TT_RNAME,
                       kvp_value_new_string(m_recipient_name.c_str()));
    kvp_frame_set_slot(frame, TT_RACC,
                       kvp_value_new_string(m_recipient_account.c_str()));
    kvp_frame_set_slot(frame, TT_RBCODE,
                       kvp_value_new_string(m_recipient_bankcode.c_str()));
    kvp_frame_set_slot(frame, TT_AMOUNT, kvp_value_new_gnc_numeric(m_amount));
    kvp_frame_set_slot(frame, TT_PURPOS,
                       kvp_value_new_string(m_purpose.c_str()));
    kvp_frame_set_slot(frame, TT_PURPOSCT,
                       kvp_value_new_string(m_purpose_continuation.c_str()));
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
    KvpFrame *toplevel = qof_instance_get_slots (QOF_INSTANCE (b));
    KvpFrame *hbci = kvp_frame_get_frame (toplevel, "hbci");
    KvpValue *listval = kvp_frame_get_slot (hbci, "template-list");
    GList *list = kvp_value_get_glist (listval);
    for (auto node = list; node != NULL; node = g_list_next (node))
    {
        KvpFrame *frame = kvp_value_get_frame (static_cast<KvpValue*>(node->data));
        auto func = [frame](const char* key)
            {return kvp_value_get_string(kvp_frame_get_slot(frame, key));};
        auto templ = new _GncABTransTempl (func(TT_NAME), func(TT_RNAME),
                                           func(TT_RACC), func(TT_RBCODE),
                                           kvp_value_get_numeric(kvp_frame_get_slot(frame, TT_AMOUNT)),
                                           func(TT_PURPOS), func(TT_PURPOSCT));
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

void
gnc_ab_set_book_template_list (QofBook *b, GList *template_list)
{
    GList *kvp_list = NULL;
    for (auto node = template_list; node != NULL; node = g_list_next (node))
    {
        auto value = kvp_value_new_frame_nc (static_cast<_GncABTransTempl*>(node->data)->make_kvp_frame());
        kvp_list = g_list_prepend (kvp_list, value);
    }
    kvp_list = g_list_reverse (kvp_list);
    auto value = kvp_value_new_glist_nc(kvp_list);
    KvpFrame *toplevel = qof_instance_get_slots (QOF_INSTANCE (b));
    KvpFrame *hbci = kvp_frame_get_frame (toplevel, "hbci");
    kvp_frame_set_slot_nc (hbci, "template-list", value);
    qof_instance_set_dirty_flag (QOF_INSTANCE (b), TRUE);
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
