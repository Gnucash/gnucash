/********************************************************************\
 * gnc-trace.c -- GnuCash error loging and tracing facility         *
 * Copyright (C) 1997-2003 Linas Vepstas <linas@linas.org>          *
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
 *   Author: Rob Clark (rclark@cs.hmc.edu)                          *
 *   Author: Linas Vepstas (linas@linas.org)                        *
\********************************************************************/

#include "config.h"

#include <glib.h>
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#else
  /* What to do? */
#endif
#include <stdarg.h>
#include <string.h>
#include <sys/time.h>

#include "gnc-trace.h"

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_ENGINE; */

gncLogLevel gnc_log_modules[MOD_LAST + 1] =
{
  GNC_LOG_FATAL,        /* DUMMY */
  GNC_LOG_WARNING,      /* ENGINE */
  GNC_LOG_WARNING,      /* IO */
  GNC_LOG_WARNING,      /* REGISTER */
  GNC_LOG_WARNING,      /* LEDGER */
  GNC_LOG_WARNING,      /* HTML */
  GNC_LOG_WARNING,      /* GUI */
  GNC_LOG_WARNING,      /* SCRUB */
  GNC_LOG_WARNING,      /* GTK_REG */
  GNC_LOG_WARNING,      /* GUILE */
  GNC_LOG_WARNING,      /* BACKEND */
  GNC_LOG_WARNING,      /* QUERY */
  GNC_LOG_WARNING,      /* PRICE */
  GNC_LOG_WARNING,      /* SQL EVENT */
  GNC_LOG_WARNING,      /* SQL TXN */
  GNC_LOG_WARNING,      /* KVP */
  GNC_LOG_WARNING,      /* SX */
  GNC_LOG_WARNING,      /* BOOK */
  GNC_LOG_TRACE,        /* TEST */
  GNC_LOG_WARNING,      /* LOT */
  GNC_LOG_WARNING,      /* ACCOUNT */
  GNC_LOG_WARNING,      /* IMPORT */
  GNC_LOG_WARNING,      /* BUSINESS */
  GNC_LOG_WARNING,      /* DRUID */
  GNC_LOG_WARNING,      /* COMMODITY */
};

static FILE *fout = NULL;
static const int MAX_TRACE_FILENAME = 100;

/* Don't be fooled: gnc_trace_num_spaces has external linkage and
   static storage, but can't be defined with 'extern' because it has
   an initializer, and can't be declared with 'static' because that
   would give it internal linkage. */
gint __attribute__ ((unused)) gnc_trace_num_spaces = 0;

static void
fh_printer (const gchar   *log_domain,
            GLogLevelFlags    log_level,
            const gchar   *message,
            gpointer    user_data)
{
  extern gint gnc_trace_num_spaces;
  FILE *fh = user_data;
  fprintf (fh, "%*s%s\n", gnc_trace_num_spaces, "", message);
  fflush(fh);
}

void 
gnc_log_init (void)
{
   char *filename;

   fout = fopen ("/tmp/gnucash.trace", "w");

   if(!fout && (filename = (char *)g_malloc(MAX_TRACE_FILENAME))) {
      snprintf(filename, MAX_TRACE_FILENAME-1, "/tmp/gnucash.trace.%d", 
	       getpid());
      fout = fopen (filename, "w");
      g_free(filename);
   }

   if(!fout)
      fout = stderr;

   g_log_set_handler (G_LOG_DOMAIN, G_LOG_LEVEL_MASK, fh_printer, fout);
}

/* Set the logging level of the given module. */
void
gnc_set_log_level(gncModuleType module, gncLogLevel level)
{
  if ((module < 0) || (module > MOD_LAST))
    return;

  gnc_log_modules[module] = level;
}

/* Set the logging level for all modules. */
void
gnc_set_log_level_global(gncLogLevel level)
{
  gncModuleType module;

  for (module = 0; module <= MOD_LAST; module++)
    gnc_log_modules[module] = level;
}

void
gnc_set_logfile (FILE *outfile)
{
   fout = outfile;
}

/* gnc_log_prettify() cleans up subroutine names. AIX/xlC has the habit
 * of printing signatures not names; clean this up. On other operating
 * systems, truncate name to 30 chars. Note this routine is not thread
 * safe. Note we wouldn't need this routine if AIX did something more
 * reasonable. Hope thread safety doesn't poke us in eye. */
const char *
gnc_log_prettify (const char *name)
{
  static char bf[128];
  char *p;

  if (!name)
    return "";

  strncpy (bf, name, 29); bf[28] = 0;
  p = strchr (bf, '(');

  if (p)
  {
    *(p+1) = ')';
    *(p+2) = 0x0;
  }
  else
    strcpy (&bf[26], "...()");

  return bf;
}

/********************************************************************\
\********************************************************************/

#define NUM_CLOCKS 10

static
struct timeval gnc_clock[NUM_CLOCKS] = {
   {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, 
   {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, 
};

static
struct timeval gnc_clock_total[NUM_CLOCKS] = {
   {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, 
   {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, 
};

void
gnc_start_clock (int clockno, gncModuleType module, gncLogLevel log_level,
                 const char *function_name, const char *format, ...)
{
  struct timezone tz;
  va_list ap;

  if ((0>clockno) || (NUM_CLOCKS <= clockno)) return;
  gettimeofday (&gnc_clock[clockno], &tz);

  if (!fout) gnc_log_init();

  fprintf (fout, "Clock %d Start: %s: ",
           clockno, gnc_log_prettify (function_name));

  va_start (ap, format);

  vfprintf (fout, format, ap);

  va_end (ap);

  fprintf (fout, "\n");
  fflush (fout);
}

void
gnc_report_clock (int clockno, gncModuleType module, gncLogLevel log_level,
                  const char *function_name, const char *format, ...)
{
  struct timezone tz;
  struct timeval now;
  va_list ap;

  if ((0>clockno) || (NUM_CLOCKS <= clockno)) return;
  gettimeofday (&now, &tz);

  /* need to borrow to make difference */
  if (now.tv_usec < gnc_clock[clockno].tv_usec)
  {
    now.tv_sec --;
    now.tv_usec += 1000000;
  }
  now.tv_sec -= gnc_clock[clockno].tv_sec;
  now.tv_usec -= gnc_clock[clockno].tv_usec;

  gnc_clock_total[clockno].tv_sec += now.tv_sec;
  gnc_clock_total[clockno].tv_usec += now.tv_usec;

  if (!fout) gnc_log_init();

  fprintf (fout, "Clock %d Elapsed: %ld.%06lds %s: ",
           clockno, (long int) now.tv_sec, (long int) now.tv_usec, 
	   gnc_log_prettify (function_name));

  va_start (ap, format);

  vfprintf (fout, format, ap);

  va_end (ap);

  fprintf (fout, "\n");
  fflush (fout);
}

void
gnc_report_clock_total (int clockno,
                        gncModuleType module, gncLogLevel log_level,
                        const char *function_name, const char *format, ...)
{
  va_list ap;

  if ((0>clockno) || (NUM_CLOCKS <= clockno)) return;

  /* need to normalize usec */
  while (gnc_clock_total[clockno].tv_usec >= 1000000)
  {
    gnc_clock_total[clockno].tv_sec ++;
    gnc_clock_total[clockno].tv_usec -= 1000000;
  }

  if (!fout) gnc_log_init();

  fprintf (fout, "Clock %d Total Elapsed: %ld.%06lds  %s: ",
           clockno,
           (long int) gnc_clock_total[clockno].tv_sec,
           (long int) gnc_clock_total[clockno].tv_usec,
           gnc_log_prettify (function_name));

  va_start (ap, format);

  vfprintf (fout, format, ap);

  va_end (ap);

  fprintf (fout, "\n");
  fflush (fout);
}

gboolean
gnc_should_log(gncModuleType module, gncLogLevel log_level)
{
  return (log_level <= gnc_log_modules[module]);
}

/************************* END OF FILE ******************************\
\********************************************************************/
