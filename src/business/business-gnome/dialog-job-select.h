/*
 * dialog-job-select.h -- Job Selection Dialog for GNC Business Objects
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_JOB_SELECT_H_
#define GNC_DIALOG_JOB_SELECT_H_

/* Create a dialog to select a job based upon potentially-existing
 * customer and jobs.  Allows creation of both jobs and customers.
 */
GncJob *
gnc_ui_select_job_new (GtkWidget * parent, GNCBook *book,
		       GncOwner *owner, GncJob *job);

#endif /* GNC_DIALOG_JOB_SELECT_H_ */
