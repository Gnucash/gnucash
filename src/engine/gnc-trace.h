/********************************************************************\
 * gnc-trace.h -- GnuCash error logging and tracing fcility         *
 * Copyright (C) 1998-2003 Linas Vepstas <linas@linas.org>          *
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
 * 59 Temple Place - Suite 330        Fax:    +1-617-542-2652       *
 * Boston, MA  02111-1307,  USA       gnu@gnu.org                   *
 *                                                                  *
 *   Author: Linas Vepstas (linas@linas.org)                        *
\********************************************************************/

/** @file gnc-trace.h @brief GnuCash error loging and tracing facility */

#ifndef GNC_TRACE_H
#define GNC_TRACE_H

#include <glib.h>
#include <stdarg.h>
#include <stdio.h>

/** DEBUGGING MACROS ************************************************/
/* The debuging macros enable the setting of trace messages */

/** If you modify this, modify the loglevel table in the .c file. */
typedef enum
{
  MOD_DUMMY     =  0,
  MOD_ENGINE    =  1,
  MOD_IO        =  2,
  MOD_REGISTER  =  3,
  MOD_LEDGER    =  4,
  MOD_HTML      =  5,
  MOD_GUI       =  6,
  MOD_SCRUB     =  7,
  MOD_GTK_REG   =  8,
  MOD_GUILE     =  9,
  MOD_BACKEND   = 10,
  MOD_QUERY     = 11,
  MOD_PRICE     = 12,
  MOD_EVENT     = 13,
  MOD_TXN       = 14,
  MOD_KVP       = 15,
  MOD_SX        = 16,
  MOD_BOOK      = 17,
  MOD_TEST      = 18,
  MOD_LOT       = 19,
  MOD_ACCOUNT   = 20,
  MOD_IMPORT    = 21,
  MOD_BUSINESS  = 22,
  MOD_DRUID     = 23,
  MOD_COMMODITY = 24,
  MOD_LAST      = 24
} gncModuleType;

typedef enum
{
  GNC_LOG_FATAL   = 0,
  GNC_LOG_ERROR   = 1,
  GNC_LOG_WARNING = 2,
  GNC_LOG_INFO    = 3,
  GNC_LOG_DEBUG   = 4,
  GNC_LOG_DETAIL  = 5,
  GNC_LOG_TRACE   = 6,
} gncLogLevel;

extern gncLogLevel gnc_log_modules[MOD_LAST + 1];

/** Initialize the error logging subsystem */
void gnc_log_init (void);

/** Set the logging level of the given module. */
void gnc_set_log_level(gncModuleType module, gncLogLevel level);

/** Set the logging level for all modules. */
void gnc_set_log_level_global(gncLogLevel level);

/** Specify an alternate log output, to pipe or file.  By default,
 *  all logging goes to STDERR. */
void gnc_set_logfile (FILE *outfile);


/** gnc_log_prettify() cleans up subroutine names. AIX/xlC has the habit
 * of printing signatures not names; clean this up. On other operating
 * systems, truncate name to 30 chars. Note this routine is not thread
 * safe. Note we wouldn't need this routine if AIX did something more
 * reasonable. Hope thread safety doesn't poke us in eye. */
const char * gnc_log_prettify (const char *name);

/* We want logging decisions to be made inline, rather than through
 * a CPU-cucking subroutine call. Thus, this is a #define, not a
 * subroutine call.  The prototype would have been:
 * gboolean gnc_should_log (gncModuleType module, gncLogLevel log_level); 
 */
#define gnc_should_log(module,log_level)   \
              (log_level <= gnc_log_modules[module]) 

#define FUNK gnc_log_prettify(__FUNCTION__)

/** Log error/waring/info messages to stderr or to other pipe. 
 *  This logging infrastructure is meant for validating the 
 *  correctness of the execution of the code.  'Info' level 
 *  messages help trace program flow. 'Error' messages are 
 *  meant to indicate internal data inconsistencies.
 * 
 * Messages can be logged to stdout, stderr, or to any desired
 * FILE * file handle. Use fdopen() to get a file handle from a 
 * file descriptor. Use gnc_set_logfile to set the logging file 
 * handle.
 */

#define FATAL(format, args...) {                     \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_ERROR,          \
      "Fatal Error: %s(): " format, FUNK , ## args); \
}

#define PERR(format, args...) {                    \
  if (gnc_should_log (module, GNC_LOG_ERROR)) {    \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_CRITICAL,     \
      "Error: %s(): " format, FUNK , ## args);     \
  }                                                \
}

#define PWARN(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_WARNING)) {  \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_WARNING,      \
      "Warning: %s(): " format, FUNK , ## args);   \
  }                                                \
}

#define PINFO(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_INFO)) {     \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_INFO,         \
      "Info: %s(): " format, FUNK , ## args);      \
  }                                                \
}

#define DEBUG(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_DEBUG)) {    \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,        \
      "Debug: %s(): " format, FUNK , ## args);     \
  }                                                \
}

#define ENTER(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_DEBUG)) {    \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,        \
      "Enter: %s" format, FUNK , ## args);         \
  }                                                \
}

#define LEAVE(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_DEBUG)) {    \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,        \
      "Leave: %s" format, FUNK , ## args);         \
  }                                                \
}

#define TRACE(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_TRACE)) {    \
    g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG,        \
      "Trace: %s(): " format, FUNK , ## args);     \
  }                                                \
}

#define DEBUGCMD(x) { if (gnc_should_log (module, GNC_LOG_DEBUG)) { (x); }}

/* -------------------------------------------------------- */
/* Infrastructure to make timing measurements for critical peices 
 * of code. Used for only for performance tuning & debugging. 
 */

void gnc_start_clock (int clockno, gncModuleType module, gncLogLevel log_level,
                      const char *function_name, const char *format, ...);

void gnc_report_clock (int clockno,
                       gncModuleType module,
                       gncLogLevel log_level,
                       const char *function_name,
                       const char *format, ...);

void gnc_report_clock_total (int clockno,
                             gncModuleType module,
                             gncLogLevel log_level,
                             const char *function_name,
                             const char *format, ...);

#define START_CLOCK(clockno,format, args...) {              \
  if (gnc_should_log (module, GNC_LOG_INFO))                \
    gnc_start_clock (clockno, module, GNC_LOG_INFO,         \
             __FUNCTION__, format , ## args);               \
}

#define REPORT_CLOCK(clockno,format, args...) {             \
  if (gnc_should_log (module, GNC_LOG_INFO))                \
    gnc_report_clock (clockno, module, GNC_LOG_INFO,        \
             __FUNCTION__, format , ## args);               \
}

#define REPORT_CLOCK_TOTAL(clockno,format, args...) {       \
  if (gnc_should_log (module, GNC_LOG_INFO))                \
    gnc_report_clock_total (clockno, module, GNC_LOG_INFO,  \
             __FUNCTION__, format , ## args);               \
}

#endif /* GNC_TRACE_H */
