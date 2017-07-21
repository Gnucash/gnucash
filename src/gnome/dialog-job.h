/*
 * dialog-job.h -- Dialog(s) for Job search and entry
 * Copyright (C) 2001,2002 Derek Atkins
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


#ifndef GNC_DIALOG_JOB_H_
#define GNC_DIALOG_JOB_H_

typedef struct _job_window JobWindow;

#include "gncJob.h"
#include "gncOwner.h"
#include "dialog-search.h"

/* Create or Edit a job */
GncJob * gnc_ui_job_new_return_handle (GncOwner *owner, QofBook *book);
JobWindow * gnc_ui_job_edit (GncJob *job);
JobWindow * gnc_ui_job_new (GncOwner *owner, QofBook *book);

/* Search for Jobs */
GNCSearchWindow * gnc_job_search (GncJob *start, GncOwner *owner,
                                  QofBook *book);

/*
 * These callbacks are for use with the gnc_general_search widget
 *
 * select() provides a Select Dialog and returns it.
 * edit() opens the existing customer for editing and returns NULL.
 */
GNCSearchWindow * gnc_job_search_select (gpointer start, gpointer book);
GNCSearchWindow * gnc_job_search_edit (gpointer start, gpointer book);

#endif /* GNC_DIALOG_JOB_H_ */
