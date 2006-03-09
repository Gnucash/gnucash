/***************************************************************************
 *            deprecated.h
 *
 *  Mon Nov 21 14:08:25 2005
 *  Copyright  2005  Neil Williams
 *  linux@codehelp.co.uk
 ****************************************************************************/
/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02110-1301, USA.
 */
 
#ifndef _DEPRECATED_H
#define _DEPRECATED_H
#include "qof.h"

/** @file deprecated.h
	@brief transitional header from libqof1 to libqof2
*/

/** \deprecated use QofLogLevel instead */
#define gncLogLevel QofLogLevel

/** \deprecated use qof_log_init_filename instead */
void gnc_log_init (void);

/** \deprecated use qof_log_set_level insead. */
void gnc_set_log_level(QofLogModule module, gncLogLevel level);

/** \deprecated use qof_log_set_level_registered instead. */
void gnc_set_log_level_global(gncLogLevel level);

/** \deprecated use qof_log_set_level_registered instead.

This function has been deprecated because the function name
is very misleading. It may, in future, be modified to be
truly global (i.e. make changes to log modules where no
log_level has been explicitly set) but, despite the name,
it only ever modified known (gnucash) modules. Future changes
would require that if this function is made truly global it must
preserve the effect of qof_log_set_level_registered and the advantages
of silent log_modules for those programs that do not use _global. Support
could be required for removing log_modules from the hashtable.
*/
void qof_log_set_level_global(QofLogLevel level);

/** \deprecated use qof_log_set_file instead. */
void gnc_set_logfile (FILE *outfile);

/** \deprecated use qof_log_prettify instead. */
const char * gnc_log_prettify (const char *name);

/** \deprecated use qof_log_check instead. */
gboolean gnc_should_log(QofLogModule log_module, gncLogLevel log_level);

/** \deprecated */
#define GNC_LOG_FATAL   QOF_LOG_FATAL
/** \deprecated */
#define GNC_LOG_ERROR   QOF_LOG_ERROR
/** \deprecated */
#define GNC_LOG_WARNING QOF_LOG_WARNING
/** \deprecated */
#define GNC_LOG_INFO    QOF_LOG_INFO
/** \deprecated */
#define GNC_LOG_DEBUG   QOF_LOG_DEBUG
/** \deprecated */
#define GNC_LOG_DETAIL  QOF_LOG_DETAIL
/** \deprecated */
#define GNC_LOG_TRACE   QOF_LOG_TRACE

/** \deprecated use qof_start_clock */
void gnc_start_clock (int, QofLogModule, gncLogLevel, const char*, const char*, ...);
/** \deprecated use qof_report_clock */
void gnc_report_clock (int, QofLogModule, gncLogLevel, const char*, const char*, ...);
/** \deprecated use qof_report_clock_total */
void gnc_report_clock_total (int, QofLogModule, gncLogLevel, const char*, const char*, ...);

/** \deprecated use QOF_EVENT_NONE instead. */
#define  GNC_EVENT_NONE   QOF_EVENT_NONE
/** \deprecated use QOF_EVENT_CREATE instead. */
#define  GNC_EVENT_CREATE QOF_EVENT_CREATE
/** \deprecated use QOF_EVENT_MODIFY instead. */
#define  GNC_EVENT_MODIFY QOF_EVENT_MODIFY
/** \deprecated use QOF_EVENT_DESTROY instead. */
#define  GNC_EVENT_DESTROY QOF_EVENT_DESTROY
/** \deprecated use QOF_EVENT_ADD instead. */
#define  GNC_EVENT_ADD    QOF_EVENT_ADD
/** \deprecated use QOF_EVENT_REMOVE instead. */
#define GNC_EVENT_REMOVE  QOF_EVENT_REMOVE
/** \deprecated */
#define GNC_EVENT_ALL     QOF_EVENT_ALL
/** \deprecated use QofEventName instead. */
#define GNCEngineEventType QofEventId
/** \deprecated use QofEventHandler instead. */
typedef void (*GNCEngineEventHandler) (GUID *entity, QofIdType type,
                                       GNCEngineEventType event_type,
                                       gpointer user_data);
/** \deprecated For backwards compatibility - New code must not use
this function. The function and the handler prototype will be remove
from qofevent.c in libqoqf2 */
gint qof_event_register_old_handler(GNCEngineEventHandler old_handler,
									gpointer user_data);
/** \deprecated use qof_event_register_handler instead. */
gint gnc_engine_register_event_handler (GNCEngineEventHandler handler,
                                   gpointer user_data);
/** \deprecated use qof_event_unregister_handler instead. */
void gnc_engine_unregister_event_handler (gint handler_id);
/** \deprecated use qof_event_gen instead. */
void gnc_engine_gen_event (QofEntity *entity, GNCEngineEventType event_type);
/** \deprecated use qof_event_suspend instead. */
void gnc_engine_suspend_events (void);
/** \deprecated use qof_event_resume instead. */
void gnc_engine_resume_events (void);
/** \deprecated use qof_event_generate instead. */
void gnc_engine_generate_event (const GUID *guid, QofIdType e_type, 
         GNCEngineEventType event_type);
/** \deprecated use QofBookMergeResult instead. */
#define qof_book_mergeResult QofBookMergeResult
/** \deprecated use QofBookMergeRule instead. */
#define qof_book_mergeRule QofBookMergeRule
/** \deprecated use QofBookMergeData instead. */
#define qof_book_mergeData QofBookMergeData
/** \deprecated use qof_book_merge_init instead. */
QofBookMergeData* qof_book_mergeInit( QofBook *importBook, QofBook *targetBook);
/** \deprecated use QofBookMergeRuleForeachCB instead. */
typedef void (* qof_book_mergeRuleForeachCB)(QofBookMergeData*, QofBookMergeRule*, guint);
/** \deprecated use qof_book_merge_rule_foreach instead. */
void qof_book_mergeRuleForeach(QofBookMergeData* mergeData,
                               QofBookMergeRuleForeachCB callback , 
                               QofBookMergeResult mergeResult);
/** \deprecated use qof_book_merge_update_result instead. */
QofBookMergeData*
qof_book_mergeUpdateResult(QofBookMergeData *mergeData, QofBookMergeResult tag);
/** \deprecated use qof_book_merge_commit instead. */
gint
qof_book_mergeCommit(QofBookMergeData *mergeData );

#endif /* _DEPRECATED_H */
