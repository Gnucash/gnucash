/* *****************************************************************\
 * deprecated.c -- QOF deprecated function replacements            *
 * Copyright (c) 2005 Neil Williams <linux@codehelp.co.uk>          *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
 *                                                                  *
\********************************************************************/

#include "config.h"
#ifndef QOF_DISABLE_DEPRECATED
#include <glib.h>
#include "qof.h"

/* Don't be fooled: gnc_trace_num_spaces has external linkage and
   static storage, but can't be defined with 'extern' because it has
   an initializer, and can't be declared with 'static' because that
   would give it internal linkage. (this is why it is deprecated) */
gint __attribute__ ((unused)) gnc_trace_num_spaces = 0;
const char * gnc_log_prettify (const char *name)
{
	return qof_log_prettify(name);
}
void gnc_start_clock (int a, QofLogModule b, gncLogLevel c,  const char *d, const char *e, ...) { }
void gnc_report_clock (int a, QofLogModule b, gncLogLevel c, const char *d, const char *e, ...) { }
void gnc_report_clock_total (int a, QofLogModule b, gncLogLevel c, const char *d, const char *e, ...) { }

gint
gnc_engine_register_event_handler (GNCEngineEventHandler handler,
                                   gpointer user_data)
{
	return qof_event_register_old_handler(handler, user_data);
}
void gnc_engine_unregister_event_handler (gint handler_id)
{
	qof_event_unregister_handler(handler_id);
}
void gnc_engine_suspend_events (void)
{
	qof_event_suspend();
}
void gnc_engine_resume_events (void)
{
	qof_event_resume();
}
void gnc_engine_gen_event (QofEntity *entity, GNCEngineEventType event_type)
{
	qof_event_gen(entity, event_type, NULL);
}
QofBookMergeData*
qof_book_mergeInit(QofBook *importBook, QofBook *targetBook)
{
	return qof_book_merge_init(importBook, targetBook);
}
QofBookMergeData*
qof_book_mergeUpdateResult(QofBookMergeData *mergeData,
						QofBookMergeResult tag)
{
	return qof_book_merge_update_result(mergeData, tag);
}
gint
qof_book_mergeCommit(QofBookMergeData *mergeData )
{
	return qof_book_merge_commit(mergeData);
}
void 
qof_book_mergeRuleForeach(QofBookMergeData *mergeData, 
						  QofBookMergeRuleForeachCB cb, 
						  QofBookMergeResult mergeResult )
{
    qof_book_merge_rule_foreach(mergeData, cb, mergeResult);
}
gpointer gnc_string_cache_insert(gconstpointer key)
{
    return (gpointer)qof_util_string_cache_insert(key);
}
gchar * gnc_stpcpy (gchar *dest, const gchar *src)
{
    return g_stpcpy(dest, src);
}
GCache* gnc_engine_get_string_cache(void)
{
    return qof_util_get_string_cache();
}
void gnc_engine_string_cache_destroy (void)
{
    qof_util_string_cache_destroy();
}
void gnc_string_cache_remove(gconstpointer key)
{
    qof_util_string_cache_remove(key);
}

/* ==================================================================== */
#endif /* QOF_DISABLE_DEPRECATED */
