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

/*
#include <errno.h>
#include <glib.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
*/

#include "config.h"

/** DEBUGGING MACROS ************************************************/
/* The debuging macros enable the setting of trace messages */

/** If you modify this, modify the loglevel table in the .c file. */
typedef enum
{
  MOD_DUMMY   =  0,
  MOD_ENGINE  =  1,
  MOD_IO      =  2,
  MOD_REGISTER=  3,
  MOD_LEDGER  =  4,
  MOD_HTML    =  5,
  MOD_GUI     =  6,
  MOD_SCRUB   =  7,
  MOD_GTK_REG =  8,
  MOD_GUILE   =  9,
  MOD_BACKEND = 10,
  MOD_QUERY   = 11,
  MOD_PRICE   = 12,
  MOD_EVENT   = 13,
  MOD_TXN     = 14,
  MOD_KVP     = 15,
  MOD_SX      = 16,
  MOD_BOOK    = 17,
  MOD_TEST    = 18,
  MOD_LOT     = 19,
  MOD_ACCOUNT = 20,
  MOD_IMPORT  = 21,
  MOD_BUSINESS= 22,
  MOD_LAST    = 22
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


typedef void (*GNCGuiMessage) (const gchar *format, va_list args);
void gnc_set_warning_message (GNCGuiMessage func);
void gnc_set_error_message (GNCGuiMessage func);

gboolean gnc_send_gui_warning (const gchar *format, ...);
gboolean gnc_send_gui_error (const gchar *format, ...);

/* FIXME: these logging functions should proably get replaced by
 * the glib.h g_error(), etc functions. That way, we would have
 * unified logging mechanism, instead of having some messages
 * work one way, and other a different way ... 
 *
 * FIXME: the if test should not be a subroutine call, it should
 * not use that many CPU cycles.  These logging functions are supposed
 * to be lightweight.  Who changed this ??? Why ??? 
 *
 */
gboolean gnc_should_log (gncModuleType module, gncLogLevel log_level);
void gnc_log (gncModuleType module, gncLogLevel log_level,
              const char *prefix, const char *function_name,
              const char *format, ...) G_GNUC_PRINTF(5,6);

#define FATAL(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_FATAL))      \
    gnc_log (module, GNC_LOG_FATAL, "Fatal Error", \
             __FUNCTION__, format, ## args);       \
}

#define PERR(format, args...) {                    \
  if (gnc_should_log (module, GNC_LOG_ERROR))      \
    gnc_log (module, GNC_LOG_ERROR, "Error",       \
             __FUNCTION__, format, ##args);        \
}

#define PWARN(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_WARNING))    \
    gnc_log (module, GNC_LOG_WARNING, "Warning",   \
             __FUNCTION__, format, ## args);       \
}

#define PWARN_GUI(format, args...) {               \
  if (!gnc_send_gui_error(format, ## args))        \
    PWARN(format, ## args);                        \
}

#define PINFO(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_INFO))       \
    gnc_log (module, GNC_LOG_INFO, "Info",         \
             __FUNCTION__, format, ## args);       \
}

#define DEBUG(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_DEBUG))      \
    gnc_log (module, GNC_LOG_DEBUG, "Debug",       \
             __FUNCTION__, format, ## args);       \
}

#define ENTER(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_DEBUG))      \
    gnc_log (module, GNC_LOG_DEBUG, "Enter",       \
             __FUNCTION__, format, ## args);       \
}

#define LEAVE(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_DEBUG))      \
    gnc_log (module, GNC_LOG_DEBUG, "Leave",       \
            __FUNCTION__, format, ## args);        \
}

#define DETAIL(format, args...) {                  \
  if (gnc_should_log (module, GNC_LOG_DETAIL))     \
    gnc_log (module, GNC_LOG_DETAIL, "Detail",     \
             __FUNCTION__, format, ## args);       \
}


#define DEBUGCMD(x) { if (gnc_should_log (module, GNC_LOG_DEBUG)) { x; }}

#define ERROR()     fprintf(stderr,"%s: Line %d, error = %s\n", \
			    __FILE__, __LINE__, strerror(errno));

#define TRACE(format, args...) {                   \
  if (gnc_should_log (module, GNC_LOG_TRACE))      \
    gnc_log (module, GNC_LOG_TRACE, "Trace",       \
             __FUNCTION__, format, ## args);       \
}

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

#define START_CLOCK(clockno,format, args...) {     \
  if (gnc_should_log (module, GNC_LOG_INFO))       \
    gnc_start_clock (clockno, module, GNC_LOG_INFO,\
             __FUNCTION__, format, ## args);       \
}

#define REPORT_CLOCK(clockno,format, args...) {    \
  if (gnc_should_log (module, GNC_LOG_INFO))       \
    gnc_report_clock (clockno, module, GNC_LOG_INFO,\
             __FUNCTION__, format, ## args);       \
}

#define REPORT_CLOCK_TOTAL(clockno,format, args...) {       \
  if (gnc_should_log (module, GNC_LOG_INFO))                \
    gnc_report_clock_total (clockno, module, GNC_LOG_INFO,  \
             __FUNCTION__, format, ## args);                \
}

/* Set the logging level of the given module. */
void gnc_set_log_level(gncModuleType module, gncLogLevel level);

/* Set the logging level for all modules. */
void gnc_set_log_level_global(gncLogLevel level);

/* Pipe log output to pipe or file */
void gnc_set_logfile (FILE *outfile);

#endif /* GNC_TRACE_H */
