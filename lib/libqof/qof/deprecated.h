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

#endif /* _DEPRECATED_H */
