/*
 * dialog-job.h -- Dialog(s) for Job search and entry
 * Copyright (C) 2001,2002 Derek Atkins
 * Author: Derek Atkins <warlord@MIT.EDU>
 */


#ifndef GNC_DIALOG_JOB_H_
#define GNC_DIALOG_JOB_H_

typedef struct _job_window JobWindow;

#include "gncJob.h"
#include "gncOwner.h"
#include "dialog-search.h"

/* Create or Edit a job */
GncJob * gnc_ui_job_new_return_handle (GncOwner *owner, GNCBook *book);
JobWindow * gnc_ui_job_edit (GncJob *job);
JobWindow * gnc_ui_job_new (GncOwner *owner, GNCBook *book);

/* Search for Jobs */
GNCSearchWindow * gnc_job_search (GncJob *start, GncOwner *owner,
				  GNCBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing customer for editing and returns NULL.
 */
GNCSearchWindow * gnc_job_search_select (gpointer start, gpointer book);
GNCSearchWindow * gnc_job_search_edit (gpointer start, gpointer book);

#endif /* GNC_DIALOG_JOB_H_ */
