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
