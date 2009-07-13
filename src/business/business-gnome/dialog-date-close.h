/*
 * dialog-date-close.h -- Dialog to ask a question and request a date
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
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

#ifndef _DIALOG_DATE_CLOSE_H
#define _DIALOG_DATE_CLOSE_H

#include "Account.h"
#include "gnc-book.h"
#include "qof.h"
#include "gncBillTerm.h"

gboolean
gnc_dialog_date_close_parented (GtkWidget *parent, const char *message,
				const char *label_message,
				gboolean ok_is_default,
				/* Returned data ... */
				Timespec *date);


/* 
 * Note that the dialog will "own" (and free) the acct_types list.
 * it should be a list of GNCAccountTypes.  If memo is non-NULL,
 * it will g_malloc() a string.  The caller should g_free() it.
 * The value of *acct will be used as preselection of the account
 * selection widget.
 */

gboolean
gnc_dialog_dates_acct_question_parented (GtkWidget *parent, const char *message,
				const char *ddue_label_message,
				const char *post_label_message,
				const char *acct_label_message,
				const char *question_check_message,
				gboolean ok_is_default,
                                gboolean set_default_acct,
				GList * acct_types, GList * acct_commodities, 
                                QofBook *book, GncBillTerm *terms,
				/* Returned Data... */
				Timespec *ddue, Timespec *post,
				char **memo, Account **acct, gboolean *answer);

#define gnc_dialog_dates_acct_parented(parent, message,	\
				ddue_label_message,							\
				post_label_message,							\
				acct_label_message,							\
				ok_is_default,								\
				acct_types, book,							\
				terms,										\
				/* Returned Data... */						\
				ddue, post,									\
				memo, acct)									\
		gnc_dialog_dates_acct_question_parented (parent, message,	\
				ddue_label_message,							\
				post_label_message,							\
				acct_label_message,							\
				NULL,						\
				ok_is_default,								\
				acct_types, book,							\
				terms,										\
				/* Returned Data... */						\
				ddue, post,									\
				memo, acct, NULL)									\


gboolean
gnc_dialog_date_acct_parented (GtkWidget *parent, const char *message,
			       const char *date_label_message,
			       const char *acct_label_message,
			       gboolean ok_is_default,
			       GList * acct_types, QofBook *book,
				/* Returned Data... */
			       Timespec *date, Account **acct);

#endif /* _DIALOG_DATE_CLOSE_H */
