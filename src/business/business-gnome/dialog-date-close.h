/*
 * dialog-date-close.h -- Dialog to ask a question and request a date
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef _DIALOG_DATE_CLOSE_H
#define _DIALOG_DATE_CLOSE_H

#include "date.h"
#include "Account.h"
#include "gnc-book.h"
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
 */

gboolean
gnc_dialog_dates_acct_question_parented (GtkWidget *parent, const char *message,
				const char *ddue_label_message,
				const char *post_label_message,
				const char *acct_label_message,
				const char *question_check_message,
				gboolean ok_is_default,
				GList * acct_types, GNCBook *book,
				GncBillTerm *terms,
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
			       GList * acct_types, GNCBook *book,
				/* Returned Data... */
			       Timespec *date, Account **acct);

#endif /* _DIALOG_DATE_CLOSE_H */
