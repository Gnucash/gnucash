/*
 * dialog-date-close.h -- Dialog to ask a question and request a date
 * Copyright (C) 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */

#ifndef _DIALOG_DATE_CLOSE_H
#define _DIALOG_DATE_CLOSE_H

#include "date.h"

gboolean
gnc_dialog_date_close_parented (GtkWidget *parent, const char *message,
				const char *label_message,
				gboolean ok_is_default, Timespec *date);

#endif /* _DIALOG_DATE_CLOSE_H */
