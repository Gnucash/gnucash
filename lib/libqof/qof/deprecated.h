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
#ifndef QOF_DISABLE_DEPRECATED 
 
#ifndef _DEPRECATED_H
#define _DEPRECATED_H
#include <glib.h> /* deprecated */
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
void gnc_engine_gen_event (QofInstance *entity, GNCEngineEventType event_type);
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
QofBookMergeData* qof_book_mergeUpdateResult(QofBookMergeData *mergeData,
                                QofBookMergeResult tag);
/** \deprecated use qof_book_merge_commit instead. */
gint qof_book_mergeCommit(QofBookMergeData *mergeData );
/** \deprecated Use the function versions, safe_strcmp() and
safe_strcasecmp() instead. */
#define SAFE_STRCMP_REAL(fcn,da,db) {    \
  if ((da) && (db)) {                    \
    if ((da) != (db)) {                  \
      gint retval = fcn ((da), (db));    \
      /* if strings differ, return */    \
      if (retval) return retval;         \
    }                                    \
  } else                                 \
  if ((!(da)) && (db)) {                 \
    return -1;                           \
  } else                                 \
  if ((da) && (!(db))) {                 \
    return +1;                           \
  }                                      \
}
/** \deprecated use safe_strcmp() */
#define SAFE_STRCMP(da,db) SAFE_STRCMP_REAL(strcmp,(da),(db))
/** \deprecated use safe_strcasecmp() */
#define SAFE_STRCASECMP(da,db) SAFE_STRCMP_REAL(strcasecmp,(da),(db))
/** \deprecated use qof_util_string_cache_insert instead. */
gpointer gnc_string_cache_insert(gconstpointer key);
#define GNC_SCANF_LLD QOF_SCANF_LLD /**< \deprecated use QOF_SCANF_LLD instead. */
/** \deprecated use qof_util_stpcpy instead. */
gchar * gnc_stpcpy (gchar *dest, const gchar *src);
/** \deprecated use qof_init instead. */
GCache* gnc_engine_get_string_cache(void);
/** \deprecated use qof_init instead. */
GCache* qof_util_get_string_cache(void);
/** \deprecated use qof_close instead. */
void gnc_engine_string_cache_destroy (void);
/** \deprecated use qof_util_string_cache_remove instead. */
void gnc_string_cache_remove(gconstpointer key);
/** \deprecated no replacement. */
void qof_book_set_schedxactions( QofBook *book, GList *newList );
#endif /* _DEPRECATED_H */
#endif /* QOF_DISABLE_DEPRECATED */
