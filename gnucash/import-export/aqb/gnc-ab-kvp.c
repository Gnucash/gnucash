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

#include <config.h>
#include "gnc-ui-util.h"

#include "gnc-ab-kvp.h"
#include "qofinstance-p.h"

#define AB_STANDING_ORDER_KVP_ROOT "aqbanking-standing-order"
#define AB_STANDING_ORDER_KVP_ID "id"
#define AB_STANDING_ORDER_KVP_ACCOUNT_GUID "account-guid"

/* This static indicates the debugging module that this .o belongs to.  */
G_GNUC_UNUSED static QofLogModule log_module = G_LOG_DOMAIN;

const gchar *
gnc_ab_get_account_accountid(const Account *a)
{
    gchar *id = NULL;
    qof_instance_get (QOF_INSTANCE (a),
		      "ab-account-id", &id,
		      NULL);
    return id;
}

void
gnc_ab_set_account_accountid(Account *a, const gchar *id)
{
    xaccAccountBeginEdit(a);
    qof_instance_set (QOF_INSTANCE (a),
		      "ab-account-id", id,
		      NULL);
    xaccAccountCommitEdit(a);
}

const gchar *
gnc_ab_get_account_bankcode(const Account *a)
{
    gchar *code = NULL;
    qof_instance_get (QOF_INSTANCE (a),
		      "ab-bank-code", &code,
		      NULL);
    return code;
}

void
gnc_ab_set_account_bankcode(Account *a, const gchar *code)
{
    xaccAccountBeginEdit(a);
    qof_instance_set (QOF_INSTANCE (a),
		      "ab-bank-code", code,
		      NULL);
    xaccAccountCommitEdit(a);
}

guint32
gnc_ab_get_account_uid(const Account *a)
{
    guint64 uid = 0LL;
    qof_instance_get (QOF_INSTANCE (a),
		      "ab-account-uid", &uid,
		      NULL);
    return (guint32)uid;
}

void
gnc_ab_set_account_uid(Account *a, guint32 uid)
{
    xaccAccountBeginEdit(a);
    qof_instance_set (QOF_INSTANCE (a),
		      "ab-account-uid", (guint64)uid,
		      NULL);
    xaccAccountCommitEdit(a);
}

time64
gnc_ab_get_account_trans_retrieval(const Account *a)
{
    Time64 *t = NULL;
    qof_instance_get (QOF_INSTANCE (a),
		      "ab-trans-retrieval", &t,
		      NULL);
    return t ? t->t : 0;
}

void
gnc_ab_set_account_trans_retrieval(Account *a, time64 time)
{
    Time64 t = {time};
    xaccAccountBeginEdit(a);
    qof_instance_set (QOF_INSTANCE (a),
		      "ab-trans-retrieval", &t,
		      NULL);
    xaccAccountCommitEdit(a);
}

static gchar *
get_standing_order_string (const SchedXaction *sx, const gchar *slot)
{
    GValue value = G_VALUE_INIT;
    gchar *result = NULL;

    g_return_val_if_fail (GNC_IS_SX (sx), NULL);

    qof_instance_get_kvp (QOF_INSTANCE (sx), &value, 2,
                          AB_STANDING_ORDER_KVP_ROOT, slot);
    if (G_VALUE_HOLDS_STRING (&value))
        result = g_value_dup_string (&value);
    if (G_IS_VALUE (&value))
        g_value_unset (&value);

    return result;
}

static void
set_standing_order_string (SchedXaction *sx, const gchar *slot,
                           const gchar *string)
{
    GValue value = G_VALUE_INIT;

    g_value_init (&value, G_TYPE_STRING);
    g_value_set_string (&value, string ? string : "");
    qof_instance_set_kvp (QOF_INSTANCE (sx), &value, 2,
                          AB_STANDING_ORDER_KVP_ROOT, slot);
    g_value_unset (&value);
}

gchar *
gnc_ab_get_standing_order_id (const SchedXaction *sx)
{
    return get_standing_order_string (sx, AB_STANDING_ORDER_KVP_ID);
}

gchar *
gnc_ab_get_standing_order_account_guid (const SchedXaction *sx)
{
    return get_standing_order_string (sx, AB_STANDING_ORDER_KVP_ACCOUNT_GUID);
}

void
gnc_ab_set_standing_order_metadata (SchedXaction *sx,
                                    const gchar *id,
                                    const gchar *account_guid)
{
    g_return_if_fail (GNC_IS_SX (sx));

    gnc_sx_begin_edit (sx);
    set_standing_order_string (sx, AB_STANDING_ORDER_KVP_ID, id);
    set_standing_order_string (sx, AB_STANDING_ORDER_KVP_ACCOUNT_GUID,
                               account_guid);
    qof_instance_set_dirty (QOF_INSTANCE (sx));
    gnc_sx_commit_edit (sx);
}
