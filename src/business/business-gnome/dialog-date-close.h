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

gboolean
gnc_dialog_date_close_parented (GtkWidget *parent, const char *message,
				const char *label_message,
				gboolean ok_is_default,
				/* Returned data ... */
				Timespec *date);


gboolean
gnc_dialog_date_acct_parented (GtkWidget *parent, const char *message,
			       const char *ddue_label_message,
			       const char *post_label_message,
			       const char *acct_label_message,
			       gboolean ok_is_default,
			       GNCAccountType acct_type, GNCBook *book,
			       /* Returned Data... */
			       Timespec *ddue, Timespec *post, Account **acct);


#endif /* _DIALOG_DATE_CLOSE_H */
