/* *****************************************************************\
 * gnc-trace.c -- QOF error logging and tracing facility            *
 * Copyright (C) 1997-2003 Linas Vepstas <linas@linas.org>          *
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
 *   Author: Rob Clark (rclark@cs.hmc.edu)                          *
 *   Author: Linas Vepstas (linas@linas.org)                        *
\********************************************************************/

/** @addtogroup Trace
@{ */

/** @file gnc-trace.c
    @brief QOF error logging facility 
		@author Neil Williams <linux@codehelp.co.uk>
*/

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
#include "qof.h"
#include "gnc-trace.h"

static FILE *fout = NULL;
static gchar* filename = NULL;

static const int MAX_TRACE_FILENAME = 100;
static GHashTable *log_table = NULL;

AS_STRING_FUNC(gncLogLevel, LOG_LEVEL_LIST)  /**< enum_as_string function

uses the enum_as_string macro from QOF
but the From macro is not required. Lookups
are done on the string. */

FROM_STRING_FUNC(gncLogLevel, LOG_LEVEL_LIST)

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
   if(!fout) //allow gnc_set_logfile
   {
	   fout = fopen ("/tmp/qof.trace", "w");
   }

   if(!fout && (filename = (char *)g_malloc(MAX_TRACE_FILENAME))) {
      snprintf(filename, MAX_TRACE_FILENAME-1, "/tmp/qof.trace.%d", 
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
gnc_set_log_level(QofLogModule log_module, gncLogLevel level)
{
	gchar* level_string;

	if(!log_module || level == 0) { return; }
	level_string = g_strdup(gncLogLevelasString(level));
	if(!log_table)
	{
		log_table = g_hash_table_new(g_str_hash, g_str_equal);
	}
	g_hash_table_insert(log_table, (gpointer)log_module, level_string);
}

static void
log_module_foreach(gpointer key, gpointer value, gpointer data)
{
	g_hash_table_insert(log_table, key, data);
}

/* Set the logging level for all known modules. */
void
gnc_set_log_level_global(gncLogLevel level)
{
	gchar* level_string;

	if(!log_table || level == 0) { return; }
	level_string = g_strdup(gncLogLevelasString(level));
	g_hash_table_foreach(log_table, log_module_foreach, level_string);
}

void
gnc_set_logfile (FILE *outfile)
{
   if(!outfile) { fout = stderr; return; }
   fout = outfile;
}

void
qof_log_init_filename (const gchar* logfilename)
{
	if(!logfilename)
	{
		fout = stderr;
	}
	else
	{
		filename = g_strdup(logfilename);
		fout = fopen(filename, "w");
	}
	gnc_log_init();
}

void
qof_log_shutdown (void)
{
	if(fout && fout != stderr) { fclose(fout); }
	if(filename) { g_free(filename); }
	g_hash_table_destroy(log_table);
}

#define MAX_CHARS 50
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

  strncpy (bf, name, MAX_CHARS-1); bf[MAX_CHARS-2] = 0;
  p = strchr (bf, '(');

  if (p)
  {
    *(p+1) = ')';
    *(p+2) = 0x0;
  }
  else
    strcpy (&bf[MAX_CHARS-4], "...()");

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
gnc_start_clock (int clockno, QofLogModule log_module, gncLogLevel log_level,
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
gnc_report_clock (int clockno, QofLogModule log_module, gncLogLevel log_level,
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
                        QofLogModule log_module, gncLogLevel log_level,
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
gnc_should_log(QofLogModule log_module, gncLogLevel log_level)
{
	gchar* log_string;
	gncLogLevel maximum; /* Any log_level less than this will be logged. */

	log_string = NULL;
	if(!log_table || log_module == NULL || log_level == 0) { return FALSE; }
	log_string = (gchar*)g_hash_table_lookup(log_table, log_module);
	/* if log_module not found, do not log. */
	if(!log_string) { return FALSE; }
	maximum = gncLogLevelfromString(log_string);
	if(log_level <= maximum) { return TRUE; }
	return FALSE;
}

void qof_log_set_default(gncLogLevel log_level)
{
	gnc_set_log_level(QOF_MOD_BACKEND, log_level);
	gnc_set_log_level(QOF_MOD_CLASS,   log_level);
	gnc_set_log_level(QOF_MOD_ENGINE,  log_level);
	gnc_set_log_level(QOF_MOD_OBJECT,  log_level);
	gnc_set_log_level(QOF_MOD_KVP,     log_level);
	gnc_set_log_level(QOF_MOD_MERGE,   log_level);
	gnc_set_log_level(QOF_MOD_QUERY,   log_level);
	gnc_set_log_level(QOF_MOD_SESSION, log_level);
}

struct hash_s
{
	QofLogCB cb;
	gpointer data;
};

static void hash_cb (gpointer key, gpointer value, gpointer data)
{
	struct hash_s *iter;

	iter = (struct hash_s*)data;
	if(!iter) { return; }
	(iter->cb)(key, value, iter->data);
}

void qof_log_module_foreach(QofLogCB cb, gpointer data)
{
	struct hash_s iter;

	if(!cb) { return; }
	iter.cb = cb;
	iter.data = data;
	g_hash_table_foreach(log_table, hash_cb, (gpointer)&iter);
}

gint qof_log_module_count(void)
{
	if(!log_table) { return 0; }
	return g_hash_table_size(log_table);
}

/** @} */

/************************* END OF FILE ******************************\
\********************************************************************/
