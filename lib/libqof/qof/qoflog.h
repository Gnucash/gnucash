/* qof-log.h
 * Author: Rob Clark <rclark@cs.hmc.edu>
 * Copyright (C) 1998-2003 Linas Vepstas <linas@linas.org>
 * Copyright 2005 Neil Williams <linux@codehelp.co.uk>
 * Copyright 2007 Joshua Sled <jsled@asynchronous.org>
 */

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

/**
 * @addtogroup Logging
 * @{
 * @ingroup QOF
 * @brief Logging and tracing facility.
 * @sa "Logging overhaul" announcement <http://lists.gnucash.org/pipermail/gnucash-devel/2007-February/019836.html>
 * 
 * qof_log_init(void) installs a handler that interprets the "log_domain"
 * as a "."-separated path.  Log level thresholds can be set for each level
 * in the tree.  When a message is logged, the longest level match is
 * found, and used as the threshold.
 *
 * For instance, we can set the levels as such:
 * @verbatim
   "qof"                        = WARN
   "gnc"                        = WARN
   "gnc.ui"                     = INFO
   "gnc.ui.plugin-page.sx-list" = DEBUG
 @endverbatim
 *
 * When code in the log_module of "gnc.import" attempts to log at DEBUG
 * (let's say), the handler will attempt to match the log domain to
 * successively-longer paths: first "", then "gnc", then "gnc.import".  Given
 * the settings above, the path "gnc" will match -- at a level of "WARN" --
 * and the DEBUG-level log will be rejected.  When code in the log domain of
 * "gnc.ui.plugin-page.sx-list" logs at DEBUG, however, it will match at
 * DEBUG, and be allowed.
 *
 * The current log format is as above:
 *
 * @verbatim
     * [timestamp] [level] <[log-domain]> [message]
 @endverbatim
 *
 * The timestamp and level are constant width (level is 5 characters).  The
 * log domain is re-iterated, which gives some context, but could become
 * annoying if they get long.
 * 
 * Trailing newlines (e.g. <tt>PINFO("...\n", ...)</tt>) are removed; the logger
 * will newline separate output.
 *
 * @section best Best Practices
 *
 * Code should:
 *
 * @li Define both <tt>static QofLogModule log_module</tt> and <tt>#define
 *   G_LOG_DOMAIN</tt> to the same value.
 * @li Define a logical, specific path as the log domain;
 *   @c "gnc.gui.plugin-pages.sx-list" or
 *   @c "gnc.register.gnome.cell.quickfill" are
 *   good examples.
 * @li Use glib-provided @c g_debug(...), @c g_message(...),
 *   @c g_warning(...), @c g_critical(...) and
 *   @c g_error(...) functions in preference to the historical qof/gnc @c
 *   PINFO, @c PERR (&c.) macros
 *
 * @see qof_log_parse_log_config(const char*)
 **/

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
QofLogLevel qof_log_level_from_string(const gchar *str);

/** Indents one level; see ENTER macro. **/
void qof_log_indent(void);

/**
 * De-dent one level, capped at 0; see LEAVE macro.
 **/
void qof_log_dedent(void);

/**
 * Initialize the error logging subsystem.  Defaults to a level-threshold of
 * "warning", and logging to stderr.
 **/
void qof_log_init (void);

/** Set the logging level of the given log_module. **/
void qof_log_set_level(QofLogModule module, QofLogLevel level);

/** Specify an alternate log output, to pipe or file. **/
void qof_log_set_file (FILE *outfile);

/** Specify a filename for log output. **/
void qof_log_init_filename (const gchar* logfilename);

/**
 * If @a log_to_filename is "stderr" or "stdout" (exactly,
 * case-insensitive), then those special files are used; otherwise, the
 * literal filename as given, as qof_log_init_filename(gchar*)
 **/
void qof_log_init_filename_special(const char *log_to_filename);

/**
 * Parse a log-configuration file.  A GKeyFile-format file of the schema:
 * @verbatim
    [levels] 
    # log.ger.path=level
    gnc.engine.sx=debug
    gnc.gui.sx=debug
    gnc.import-export.qif.parse=debug
    [output]
    # to=["stderr"|"stdout"|filename]
    to=stderr
 @endverbatim
 **/
void qof_log_parse_log_config(const char *filename);

/** Be nice, close the logfile if possible. */
void qof_log_shutdown (void);

/**
 * Cleans up subroutine names. AIX/xlC has the habit of printing signatures
 * not names; clean this up. On other operating systems, truncate name to
 * QOF_LOG_MAX_CHARS chars.
 **/
const gchar * qof_log_prettify (const gchar *name);

/** Check to see if the given @a log_module is configured to log at the given
 * @a log_level.  This implements the "log.path.hierarchy" logic. **/
gboolean qof_log_check(QofLogModule log_module, QofLogLevel log_level);

/** Set the default level for QOF-related log paths. **/
void qof_log_set_default(QofLogLevel log_level);

#define PRETTY_FUNC_NAME qof_log_prettify(G_STRFUNC)

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

/** Replacement for @c g_return_val_if_fail, but calls LEAVE if the test fails. **/
#define gnc_leave_return_val_if_fail(test, val) do { \
  if (! (test)) { LEAVE(""); } \
  g_return_val_if_fail(test, val); \
} while (0);

/** Replacement for @c g_return_if_fail, but calls LEAVE if the test fails. **/
#define gnc_leave_return_if_fail(test) do { \
  if (! (test)) { LEAVE(""); } \
  g_return_if_fail(test); \
} while (0);

/** Print a function exit debugging message. **/
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
             G_STRFUNC, format , ## args);               \
} while (0)

/** report elapsed time since last report on a particular timer */
#define REPORT_CLOCK(clockno,format, args...) do {       \
  if (qof_log_check (log_module, QOF_LOG_INFO))          \
    qof_report_clock (clockno, log_module, QOF_LOG_INFO, \
             G_STRFUNC, format , ## args);               \
} while (0)

/** report total elapsed time since timer started */
#define REPORT_CLOCK_TOTAL(clockno,format, args...) do {       \
  if (qof_log_check (log_module, QOF_LOG_INFO))                \
    qof_report_clock_total (clockno, log_module, QOF_LOG_INFO, \
             G_STRFUNC, format , ## args);               \
} while (0)


#endif /* _QOF_LOG_H */

/** @} */
