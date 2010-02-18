/*
 * gnc-ab-kvp.c --
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

/**
 * @internal
 * @file gnc-ab-kvp.c
 * @brief AqBanking KVP handling
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#include "config.h"

#include "gnc-ab-kvp.h"

#define AB_KEY "hbci"
#define AB_ACCOUNT_ID "account-id"
#define AB_ACCOUNT_UID "account-uid"
#define AB_BANK_CODE "bank-code"
#define AB_TRANS_RETRIEVAL "trans-retrieval"
#define AB_TEMPLATES "template-list"

/* This static indicates the debugging module that this .o belongs to.  */
static QofLogModule log_module = G_LOG_DOMAIN;

static void force_account_dirty(Account *acct);
static kvp_frame *gnc_ab_get_account_kvp(const Account *a, gboolean create);
static kvp_frame *gnc_ab_get_book_kvp(QofBook *b, gboolean create);

G_CONST_RETURN gchar *
gnc_ab_get_account_accountid(const Account *a)
{
    kvp_frame *frame = gnc_ab_get_account_kvp(a, FALSE);
    kvp_value *value = kvp_frame_get_slot(frame, AB_ACCOUNT_ID);
    return kvp_value_get_string(value);
}

void
gnc_ab_set_account_accountid(Account *a, const gchar *id)
{
    kvp_frame *frame = gnc_ab_get_account_kvp(a, TRUE);
    kvp_value *value = kvp_value_new_string(id);
    xaccAccountBeginEdit(a);
    kvp_frame_set_slot_nc(frame, AB_ACCOUNT_ID, value);
    force_account_dirty(a);
    xaccAccountCommitEdit(a);
}

G_CONST_RETURN gchar *
gnc_ab_get_account_bankcode(const Account *a)
{
    kvp_frame *frame = gnc_ab_get_account_kvp(a, FALSE);
    kvp_value *value = kvp_frame_get_slot(frame, AB_BANK_CODE);
    return kvp_value_get_string(value);
}

void
gnc_ab_set_account_bankcode(Account *a, const gchar *code)
{
    kvp_frame *frame = gnc_ab_get_account_kvp(a, TRUE);
    kvp_value *value = kvp_value_new_string(code);
    xaccAccountBeginEdit(a);
    kvp_frame_set_slot_nc(frame, AB_BANK_CODE, value);
    force_account_dirty(a);
    xaccAccountCommitEdit(a);
}

guint32
gnc_ab_get_account_uid(const Account *a)
{
    kvp_frame *frame = gnc_ab_get_account_kvp(a, FALSE);
    kvp_value *value = kvp_frame_get_slot(frame, AB_ACCOUNT_UID);
    return (guint32) kvp_value_get_gint64(value);
}

void
gnc_ab_set_account_uid(Account *a, guint32 uid)
{
    kvp_frame *frame = gnc_ab_get_account_kvp(a, TRUE);
    kvp_value *value = kvp_value_new_gint64(uid);
    xaccAccountBeginEdit(a);
    kvp_frame_set_slot_nc(frame, AB_ACCOUNT_UID, value);
    force_account_dirty(a);
    xaccAccountCommitEdit(a);
}

Timespec
gnc_ab_get_account_trans_retrieval(const Account *a)
{
    kvp_frame *frame = gnc_ab_get_account_kvp(a, FALSE);
    kvp_value *value = kvp_frame_get_slot(frame, AB_TRANS_RETRIEVAL);
    return kvp_value_get_timespec(value);
}

void
gnc_ab_set_account_trans_retrieval(Account *a, Timespec time)
{
    kvp_frame *frame = gnc_ab_get_account_kvp(a, TRUE);
    kvp_value *value = kvp_value_new_timespec(time);
    xaccAccountBeginEdit(a);
    kvp_frame_set_slot_nc(frame, AB_TRANS_RETRIEVAL, value);
    force_account_dirty(a);
    xaccAccountCommitEdit(a);
}

GList *
gnc_ab_get_book_template_list(QofBook *b)
{
    kvp_frame *frame = gnc_ab_get_book_kvp(b, FALSE);
    kvp_value *value = kvp_frame_get_slot(frame, AB_TEMPLATES);
    return kvp_value_get_glist(value);
}

void
gnc_ab_set_book_template_list(QofBook *b, GList *template_list)
{
    kvp_frame *frame = gnc_ab_get_book_kvp(b, TRUE);
    kvp_value *value = kvp_value_new_glist_nc(template_list);
    kvp_frame_set_slot_nc(frame, AB_TEMPLATES, value);
    qof_book_kvp_changed(b);
}

static void
force_account_dirty(Account *acct)
{
    gchar *name = g_strdup(xaccAccountGetName(acct));

    /* This is necessary because modifying the KvpFrames doesn't mark
     * accounts dirty, which means the changes wont be propagated to the
     * backend.
     */
    xaccAccountSetName(acct, name);
    g_free(name);
}

static kvp_frame *
gnc_ab_get_account_kvp(const Account *a, gboolean create)
{
    kvp_frame *toplevel = xaccAccountGetSlots(a);
    kvp_frame *result = kvp_frame_get_frame(toplevel, AB_KEY);
    if (!result && create)
    {
        result = kvp_frame_new();
        kvp_frame_add_frame_nc(toplevel, AB_KEY, result);
    }
    return result;
}

static kvp_frame *
gnc_ab_get_book_kvp(QofBook *b, gboolean create)
{
    kvp_frame *toplevel = qof_book_get_slots(b);
    kvp_frame *result = kvp_frame_get_frame(toplevel, AB_KEY);
    if (!result && create)
    {
        result = kvp_frame_new();
        kvp_frame_add_frame_nc(toplevel, AB_KEY, result);
    }
    return result;
}
