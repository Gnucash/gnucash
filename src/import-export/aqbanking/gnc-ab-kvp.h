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
 * Return a non-copied pointer to the accountid string in the Account @a a.
 * The gchar* is still owned by the kvp_frame, so don't free it until you want
 * to delete the whole kvp_frame.
 *
 * @param a Account
 * @return Account ID
 */
const gchar *gnc_ab_get_account_accountid(const Account *a);

/**
 * Set the accountid string in the Account @a a to @a id.  A copy of the string
 * will be stored.  The Account will be marked as "dirty".
 *
 * @param a Account
 * @param id Account ID
 */
void gnc_ab_set_account_accountid(Account *a, const gchar *id);

/**
 * Return a non-copied pointer to the bankcode string in the Account @a a.  The
 * gchar* is still owned by the kvp_frame, so don't free it until you want to
 * delete the whole kvp_frame.
 *
 * @param a Account
 * @return Bank code
 */
const gchar *gnc_ab_get_account_bankcode(const Account *a);

/**
 * Set the bankcode string in the Account @a a to @a code.  A copy of the string
 * will be stored.  The Account will be marked as "dirty".
 *
 * @param a Account
 * @param code Bank code
 */
void gnc_ab_set_account_bankcode(Account *a, const gchar *code);

/**
 * Return the unique id for the AB_BANKING account in the Account @a a.
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
 * Return the time of last online transaction retrieval for Account @a a.
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

/** @name Book
 *  @{ */

/**
 * Return a non-copied pointer to the GList of kvp_frames which eventually are
 * the template transactions, stored in the given book.
 *
 * @param b Book
 * @return Template list
 */
GList *gnc_ab_get_book_template_list(QofBook *b);

/**
 * Set the GList of kvp_frames of template transactions in the Book @a b to @a
 * template_list.  No copy of the GList will be stored, the callee becomes the
 * owner and the caller must not free it.  The book will be marked "dirty".
 *
 * @param b Book
 * @param template_list Template list
 */
void gnc_ab_set_book_template_list(QofBook *b, GList *template_list);

/** @} */

G_END_DECLS

/** @} */
/** @} */

#endif /* GNC_AB_KVP_H */
