/*
 * dialog-job.h -- Dialog(s) for Job search and entry
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_JOB_H_
#define GNC_DIALOG_JOB_H_

#include "gncJob.h"
#include "gncOwner.h"

/* Functions to create and edit jobs */
GncJob * gnc_job_new (GtkWidget *parent, GncOwner *owner, GNCBook *book);
void gnc_job_edit (GtkWidget *parent, GncJob *job);

/* Callback to choose a job from a customer, for use with the
 * general-select widget.  Provides both "new" and "edit" buttons.
 */
gpointer        gnc_job_edit_new_select (gpointer start_job,
					 GtkWidget *toplevel,
					 GncCustomer *cust);

/* Callback to view/edit a job, for use with a general_select */
gpointer	gnc_job_edit_new_edit (gpointer bookp,
				       gpointer this_job,
				       GtkWidget *toplevel);
gpointer	gnc_job_select_new_select (gpointer bookp,
					   gpointer this_job,
					   GtkWidget *parent);


#endif /* GNC_DIALOG_JOB_H_ */
