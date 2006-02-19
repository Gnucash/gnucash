/***************************************************************************
 *            qof-log.h
 *
 *  Mon Nov 21 14:35:26 2005
 *  Author: Rob Clark (rclark@cs.hmc.edu)
 *  Copyright (C) 1998-2003 Linas Vepstas <linas@linas.org>
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

#include <glib.h>
#include <stdarg.h>
#include <stdio.h>
#include "gnc-engine-util.h"

#define QOF_MOD_ENGINE "qof-engine"

#define LOG_LEVEL_LIST(_) \
  _(QOF_LOG_FATAL, = 0)   \
  _(QOF_LOG_ERROR, = 1)   \
  _(QOF_LOG_WARNING, = 2) \
  _(QOF_LOG_INFO, = 3)    \
  _(QOF_LOG_DEBUG, = 4)   \
  _(QOF_LOG_DETAIL, = 5)  \
  _(QOF_LOG_TRACE, = 6)

DEFINE_ENUM (QofLogLevel, LOG_LEVEL_LIST)

AS_STRING_DEC(QofLogLevel, LOG_LEVEL_LIST) /**< Convert QofLogLevel to a string.

The macro correlates the enum value and an
exact copy as a string, removing the need to
keep two separate lists in sync.
*/

FROM_STRING_DEC(QofLogLevel, LOG_LEVEL_LIST) /**< Convert the 
log_string to a QofLogLevel

Only for use as a partner to ::QofLogLevelasString
*/

/** indents once for each ENTER macro */
void qof_log_add_indent(void);

/** gets the running total of the indent */
gint qof_log_get_indent(void);

/** drops back one indent for each LEAVE macro

indent is reset to zero if less than a single indent would exist.
*/
void qof_log_drop_indent(void);

/** Initialize the error logging subsystem

\deprecated Applications need to call
qof_log_set_file to set the output, otherwise
the default of \a /tmp/qof.trace will be used.

Instead, use qof_log_init_filename
which sets the filename and initialises the
logging subsystem in one operation.
*/
void qof_log_init (void);

/** Set the logging level of the given log_module.

Registers the log_module with the qof_log hashtable and
sets an initial value for the loglevel for that log_module.
*/
void qof_log_set_level(QofLogModule module, QofLogLevel level);

/** Set the logging level for all registered log_modules.

\note Unless a log_module has been registered using
qof_log_set_level, it will be unaffected by this change because
there will be no entry in the hashtable.

"silent" log_modules are supported by the qof_log_set_level_registered
function which only  moderates log_levels for those modules actually
registered. The advantage is that a developer can omit existing
log_modules from the init code and cut down the amount of unwanted logging. 

e.g. if you are working in one section of the code and do not want
the extra log information created by allowing the default modules
to log as well. This makes the log itself easier to use when working
in a small area of the codebase. Silent log_modules can also be
useful where no default currently exists - again to isolate certain
sections of the default log output - and using qof_log_set_level_registered
allows these silent log_modules to be retained in the code without
being logged by other developers etc.
*/
void qof_log_set_level_registered(QofLogLevel level);

/** Specify an alternate log output, to pipe or file.
By default, all logging goes to /tmp/qof.trace 
 
Needs to be called \b before qof_log_init()
\deprecated
*/
void qof_log_set_file (FILE *outfile);

/** Specify a filename for log output.

Calls qof_log_init() for you.
*/
void qof_log_init_filename (const gchar* logfilename);

/** Be nice, close the logfile if possible. */
void qof_log_shutdown (void);

/** qof_log_prettify() cleans up subroutine names. AIX/xlC has the habit
 * of printing signatures not names; clean this up. On other operating
 * systems, truncate name to QOF_LOG_MAX_CHARS chars.  */
const char * qof_log_prettify (const char *name);

/** Do not log log_modules that have not been enabled. */
gboolean qof_log_check(QofLogModule log_module, QofLogLevel log_level);

/** Set the default QOF log_modules to the log level. */
void qof_log_set_default(QofLogLevel log_level);

typedef void (*QofLogCB) (QofLogModule log_module, QofLogLevel* log_level, 
			gpointer user_data);

/** Iterate over each known log_module

Only log_modules with log_levels set will 
be available.
*/
void qof_log_module_foreach(QofLogCB cb, gpointer data);

/** Number of log_modules registered*/
gint qof_log_module_count(void);

#define FUNK qof_log_prettify(__FUNCTION__)

/** Log error/warning/info messages to stderr or to a file.
 *  This logging infrastructure is meant for validating the 
 *  correctness of the execution of the code.  'Info' level 
 *  messages help trace program flow. 'Error' messages are 
 *  meant to indicate internal data inconsistencies.
 * 
 * Messages can be logged to stdout, stderr, or to any desired
 * file.
 */

/** Log a fatal error */
#define FATAL(format, args...) do {                  \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_ERROR,          \
      "Fatal Error: %s(): " format, FUNK , ## args); \
} while (0)

/** Log a serious error */
#define PERR(format, args...) do {                   \
  if (qof_log_check (log_module, GNC_LOG_ERROR)) {   \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_CRITICAL,     \
      "Error: %s(): " format, FUNK , ## args);     \
  }                                                \
} while (0)

/** Log a warning */
#define PWARN(format, args...) do {                    \
  if (qof_log_check (log_module, GNC_LOG_WARNING)) {   \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_WARNING,      \
      "Warning: %s(): " format, FUNK , ## args);   \
  }                                                \
} while (0)

/** Print an informational note */
#define PINFO(format, args...) do {                 \
  if (qof_log_check (log_module, GNC_LOG_INFO)) {   \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,         \
      "Info: %s(): " format,                       \
      FUNK , ## args);                             \
  }                                                \
} while (0)

/** Print a debugging message */
#define DEBUG(format, args...) do {                 \
  if (qof_log_check (log_module, GNC_LOG_DEBUG)) {  \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,        \
      "Debug: %s(): " format,                      \
      FUNK , ## args);                             \
  }                                                \
} while (0)

/** Print a function entry debugging message */
#define ENTER(format, args...) do {                 \
  if (qof_log_check (log_module, GNC_LOG_DEBUG)) {  \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,        \
      "Enter in %s: %s()" format, __FILE__,        \
      FUNK , ## args);                             \
    qof_log_add_indent();                           \
  }                                                \
} while (0)

/** Print a function exit debugging message */
#define LEAVE(format, args...) do {                 \
  if (qof_log_check (log_module, GNC_LOG_DEBUG)) {  \
    qof_log_drop_indent();                          \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,        \
      "Leave: %s()" format,                        \
      FUNK , ## args);                             \
  }                                                \
} while (0)

/** Print a function trace debugging message */
#define TRACE(format, args...) do {                 \
  if (qof_log_check (log_module, GNC_LOG_TRACE)) {  \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,        \
      "Trace: %s(): " format, FUNK , ## args);     \
  }                                                \
} while (0)

#define DEBUGCMD(x) do {                            \
  if (qof_log_check (log_module, GNC_LOG_DEBUG)) {  \
		(x);                                        \
	}                                               \
} while (0)

/* -------------------------------------------------------- */
/** Infrastructure to make timing measurements for critical pieces 
 * of code. Used for only for performance tuning & debugging. 
 */

void qof_start_clock (int clockno, QofLogModule log_module, QofLogLevel log_level,
                      const char *function_name, const char *format, ...);

void qof_report_clock (int clockno,
                       QofLogModule log_module,
                       QofLogLevel log_level,
                       const char *function_name,
                       const char *format, ...);

void qof_report_clock_total (int clockno,
                             QofLogModule log_module,
                             QofLogLevel log_level,
                             const char *function_name,
                             const char *format, ...);

/** start a particular timer */
#define START_CLOCK(clockno,format, args...) do {        \
  if (qof_log_check (log_module, GNC_LOG_INFO))          \
    qof_start_clock (clockno, log_module, GNC_LOG_INFO,  \
             __FUNCTION__, format , ## args);               \
} while (0)

/** report elapsed time since last report on a particular timer */
#define REPORT_CLOCK(clockno,format, args...) do {       \
  if (qof_log_check (log_module, GNC_LOG_INFO))          \
    qof_report_clock (clockno, log_module, GNC_LOG_INFO, \
             __FUNCTION__, format , ## args);               \
} while (0)

/** report total elapsed time since timer started */
#define REPORT_CLOCK_TOTAL(clockno,format, args...) do {       \
  if (qof_log_check (log_module, GNC_LOG_INFO))                \
    qof_report_clock_total (clockno, log_module, GNC_LOG_INFO, \
             __FUNCTION__, format , ## args);               \
} while (0)


#endif /* _QOF_LOG_H */

/** @} */
