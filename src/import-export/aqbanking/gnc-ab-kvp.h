/*
 * gnc-ab-kvp.h --
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
 * @addtogroup Import_Export
 * @{
 * @addtogroup AqBanking
 * @{
 * @file gnc-ab-kvp.h
 * @brief AqBanking KVP handling
 * @author Copyright (C) 2002 Christian Stimming <stimming@tuhh.de>
 * @author Copyright (C) 2008 Andreas Koehler <andi5.py@gmx.net>
 */

#ifndef GNC_AB_KVP_H
#define GNC_AB_KVP_H

#include <glib.h>

#include "Account.h"

G_BEGIN_DECLS

/** @name Account
 *  @{ */

/**
 * Returns a non-copied pointer to the accountid string in the Account @a a.
 * The gchar* is still owned by the kvp_frame, so don't free it until you want
 * to delete the whole kvp_frame.
 *
 * @param a Account
 * @return Account ID
 */
G_CONST_RETURN gchar *gnc_ab_get_account_accountid(const Account *a);

/**
 * Set the accountid string in the Account @a a to @a id.  A copy of the string
 * will be stored.  The Account will be marked as "dirty".
 *
 * @param a Account
 * @param id Account ID
 */
void gnc_ab_set_account_accountid(Account *a, const gchar *id);

/**
 * Returns a non-copied pointer to the bankcode string in the Account @a a.  The
 * gchar* is still owned by the kvp_frame, so don't free it until you want to
 * delete the whole kvp_frame.
 *
 * @param a Account
 * @return Bank code
 */
G_CONST_RETURN gchar *gnc_ab_get_account_bankcode(const Account *a);

/**
 * Set the bankcode string in the Account @a a to @a code.  A copy of the string
 * will be stored.  The Account will be marked as "dirty".
 *
 * @param a Account
 * @param code Bank code
 */
void gnc_ab_set_account_bankcode(Account *a, const gchar *code);

/**
 * Returns the unique id for the AB_BANKING account in the Account @a a.
 *
 * @param a Account
 * @return Unique ID
 */
guint32 gnc_ab_get_account_uid(const Account *a);

/**
 * Set the unique id for the AB_BANKING account in the Account @a a to @a uid.
 * The Account will be marked as "dirty".
 *
 * @param a Account
 * @param uid Unique ID
 */
void gnc_ab_set_account_uid(Account *a, guint32 uid);

/**
 * Returns the time of last online transaction retrieval for Account @a a.
 *
 * @param a Account
 * @return Retrieval time
 */
Timespec gnc_ab_get_account_trans_retrieval(const Account *a);

/**
 * Set the time of last online transaction retrieval for Account @a a.  The
 * account will be marked as "dirty".
 *
 * @param a Account
 * @param time Retrieval time
 */
void gnc_ab_set_account_trans_retrieval(Account *a, Timespec time);

/** @} */

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_AB_KVP_H */
