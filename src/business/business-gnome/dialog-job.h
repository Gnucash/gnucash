/*
 * dialog-job.h -- Dialog(s) for Job search and entry
 * Copyright (C) 2001 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_JOB_H_
#define GNC_DIALOG_JOB_H_

/* Functions to create and edit jobs */
GncJob * gnc_job_new (GtkWidget *parent, GncBusiness *bus, GncCustomer *cust);
void gnc_job_edit (GtkWidget *parent, GncJob *job);

/* Callback to choose a job from a customer, for use with the
 * general-select widget.  Provides both "new" and "edit" buttons.
 */
gpointer        gnc_job_edit_new_select (gpointer start_job,
					 GtkWidget *toplevel,
					 GncCustomer *cust);

#endif /* GNC_DIALOG_JOB_H_ */
