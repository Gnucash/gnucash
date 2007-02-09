/***************************************************************************
 *            qof-log.h
 *
 *  Author: Rob Clark (rclark@cs.hmc.edu)
 *  Copyright (C) 1998-2003 Linas Vepstas <linas@linas.org>
 *  Copyright  2005  Neil Williams <linux@codehelp.co.uk>
 *  Copyright 2007 Joshua Sled <jsled@asynchronous.org>
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301,  USA
 */

/** @addtogroup Trace
    @{ */

/** @file qoflog.h 
 *  @brief QOF error logging and tracing facility 
*/

#ifndef _QOF_LOG_H
#define _QOF_LOG_H

#include <stdarg.h>
#include <stdio.h>
#include <glib.h>
#include "qofutil.h"

#define QOF_MOD_ENGINE "qof.engine"

#define LOG_LEVEL_LIST(_) \
  _(QOF_LOG_FATAL,   = G_LOG_LEVEL_ERROR)   \
  _(QOF_LOG_ERROR,   = G_LOG_LEVEL_CRITICAL)   \
  _(QOF_LOG_WARNING, = G_LOG_LEVEL_WARNING) \
  _(QOF_LOG_INFO,    = G_LOG_LEVEL_INFO)    \
  _(QOF_LOG_DEBUG,   = G_LOG_LEVEL_DEBUG)

DEFINE_ENUM (QofLogLevel, LOG_LEVEL_LIST)

gchar* qof_log_level_to_string(QofLogLevel lvl);
QofLogLevel qof_log_level_from_string(gchar *str);

/** indents once for each ENTER macro **/
void qof_log_indent(void);

/**
 * drops back one indent for each LEAVE macro, capped at 0.
 **/
void qof_log_dedent(void);

/**
 * Initialize the error logging subsystem.  Defaults to a level-threshold of
 * "warning", and logging to stderr.
 **/
void qof_log_init (void);

/**
 * Set the logging level of the given log_module.
 **/
void qof_log_set_level(QofLogModule module, QofLogLevel level);

/**
 * Specify an alternate log output, to pipe or file.
 **/
void qof_log_set_file (FILE *outfile);

/**
 * Specify a filename for log output.
 **/
void qof_log_init_filename (const gchar* logfilename);

/** Be nice, close the logfile if possible. */
void qof_log_shutdown (void);

/**
 * qof_log_prettify() cleans up subroutine names. AIX/xlC has the habit
 * of printing signatures not names; clean this up. On other operating
 * systems, truncate name to QOF_LOG_MAX_CHARS chars.
 **/
const gchar * qof_log_prettify (const gchar *name);

/** Do not log log_modules that have not been enabled. */
gboolean qof_log_check(QofLogModule log_module, QofLogLevel log_level);

/** Set the default QOF log_modules to the log level. */
void qof_log_set_default(QofLogLevel log_level);

typedef void (*QofLogCB) (QofLogModule log_module, QofLogLevel* log_level, 
			gpointer user_data);

#define PRETTY_FUNC_NAME qof_log_prettify(__FUNCTION__)

/** Log a fatal error */
#define FATAL(format, args...) do { \
    g_log (log_module, G_LOG_LEVEL_FATAL, \
      "[%s()] " format, PRETTY_FUNC_NAME , ## args); \
} while (0)

/** Log a serious error */
#define PERR(format, args...) do { \
    g_log (log_module, G_LOG_LEVEL_CRITICAL, \
      "[%s()] " format, PRETTY_FUNC_NAME , ## args); \
} while (0)

/** Log a warning */
#define PWARN(format, args...) do { \
    g_log (log_module, G_LOG_LEVEL_WARNING, \
      "[%s()] " format, PRETTY_FUNC_NAME , ## args); \
} while (0)

/** Print an informational note */
#define PINFO(format, args...) do { \
    g_log (log_module, G_LOG_LEVEL_INFO, \
      "[%s] " format, PRETTY_FUNC_NAME , ## args); \
} while (0)

/** Print a debugging message */
#define DEBUG(format, args...) do { \
    g_log (log_module, G_LOG_LEVEL_DEBUG, \
      "[%s] " format, PRETTY_FUNC_NAME , ## args); \
} while (0)

/** Print a function entry debugging message */
#define ENTER(format, args...) do { \
    if (qof_log_check(log_module, G_LOG_LEVEL_DEBUG)) { \
      g_log (log_module, G_LOG_LEVEL_DEBUG, \
        "[enter %s:%s()] " format, __FILE__, \
        PRETTY_FUNC_NAME , ## args); \
      qof_log_indent(); \
    } \
} while (0)

#define gnc_leave_return_val_if_fail(test, val) do { \
  if (! (test)) { LEAVE(""); } \
  g_return_val_if_fail(test, val); \
} while (0);

#define gnc_leave_return_if_fail(test) do { \
  if (! (test)) { LEAVE(""); } \
  g_return_if_fail(test); \
} while (0);

/** Print a function exit debugging message */
#define LEAVE(format, args...) do { \
    if (qof_log_check(log_module, G_LOG_LEVEL_DEBUG)) { \
      qof_log_dedent(); \
      g_log (log_module, G_LOG_LEVEL_DEBUG, \
        "[leave %s()] " format, \
        PRETTY_FUNC_NAME , ## args); \
    } \
} while (0)

/* -------------------------------------------------------- */

/** Infrastructure to make timing measurements for critical pieces 
 * of code. Used for only for performance tuning & debugging. 
 */

void qof_start_clock (gint clockno, QofLogModule log_module, QofLogLevel log_level,
                      const gchar *function_name, const gchar *format, ...);

void qof_report_clock (gint clockno,
                       QofLogModule log_module,
                       QofLogLevel log_level,
                       const gchar *function_name,
                       const gchar *format, ...);

void qof_report_clock_total (gint clockno,
                             QofLogModule log_module,
                             QofLogLevel log_level,
                             const gchar *function_name,
                             const gchar *format, ...);

/** start a particular timer */
#define START_CLOCK(clockno,format, args...) do {        \
  if (qof_log_check (log_module, QOF_LOG_INFO))          \
    qof_start_clock (clockno, log_module, QOF_LOG_INFO,  \
             __FUNCTION__, format , ## args);               \
} while (0)

/** report elapsed time since last report on a particular timer */
#define REPORT_CLOCK(clockno,format, args...) do {       \
  if (qof_log_check (log_module, QOF_LOG_INFO))          \
    qof_report_clock (clockno, log_module, QOF_LOG_INFO, \
             __FUNCTION__, format , ## args);               \
} while (0)

/** report total elapsed time since timer started */
#define REPORT_CLOCK_TOTAL(clockno,format, args...) do {       \
  if (qof_log_check (log_module, QOF_LOG_INFO))                \
    qof_report_clock_total (clockno, log_module, QOF_LOG_INFO, \
             __FUNCTION__, format , ## args);               \
} while (0)


#endif /* _QOF_LOG_H */

/** @} */
