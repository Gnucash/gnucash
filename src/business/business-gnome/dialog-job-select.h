/*
 * dialog-job-select.h -- Job Selection Dialog for GNC Business Objects
 * Copyright (C) 2001, 2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_JOB_SELECT_H_
#define GNC_DIALOG_JOB_SELECT_H_
#if 0

#include "gncJob.h"
#include "gncOwner.h"

/* Create a dialog to select a job based upon potentially-existing
 * owner and jobs.  Allows creation of both jobs and owners.
 */
void gnc_job_find (GncJob *start_job, GncOwner *owner, GNCBook *book);
GncJob *
gnc_job_choose (GtkWidget * parent, GncJob *start_job,
		GncOwner *owner, GNCBook *book);

#endif
#endif /* GNC_DIALOG_JOB_SELECT_H_ */
