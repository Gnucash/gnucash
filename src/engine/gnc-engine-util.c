/********************************************************************\
 * gnc-engine-util.c -- GnuCash engine utility functions            *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2001 Linas Vepstas <linas@linas.org>          *
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

#include <ctype.h>
#include <errno.h>
#include <glib.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "gnc-engine-util.h"
#include "gnc-engine.h"

/** GLOBALS *********************************************************/

/* This static indicates the debugging module that this .o belongs to.  */
/* static short module = MOD_ENGINE; */

static gncLogLevel loglevel[MOD_LAST + 1] =
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
  GNC_LOG_DEBUG,        /* SX */
  GNC_LOG_WARNING,      /* BOOK */
  GNC_LOG_TRACE,        /* TEST */
};

static FILE *fout = NULL;

/* Set the logging level of the given module. */
void
gnc_set_log_level(gncModuleType module, gncLogLevel level)
{
  if ((module < 0) || (module > MOD_LAST))
    return;

  loglevel[module] = level;
}

/* Set the logging level for all modules. */
void
gnc_set_log_level_global(gncLogLevel level)
{
  gncModuleType module;

  for (module = 0; module <= MOD_LAST; module++)
    loglevel[module] = level;
}

void
gnc_set_logfile (FILE *outfile)
{
   fout = outfile;
}

/* prettify() cleans up subroutine names. AIX/xlC has the habit of
 * printing signatures not names; clean this up. On other operating
 * systems, truncate name to 30 chars. Note this routine is not thread
 * safe. Note we wouldn't need this routine if AIX did something more
 * reasonable. Hope thread safety doesn't poke us in eye. */
static const char *
prettify (const char *name)
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

gboolean
gnc_should_log (gncModuleType module, gncLogLevel log_level)
{
  if (module < 0 || module > MOD_LAST)
  {
    PERR ("Bad module: %d", module);
    return FALSE;
  }

  if (log_level > loglevel[module])
    return FALSE;

  return TRUE;
}

void
gnc_log (gncModuleType module, gncLogLevel log_level, const char *prefix,
         const char *function_name, const char *format, ...)
{
  va_list ap;

  if (!gnc_should_log (module, log_level))
    return;

  if (!fout) fout = stderr;

  fprintf (fout, "%s: %s: ",
           prefix ? prefix : "(null)",
           prettify (function_name));

  va_start (ap, format);

  vfprintf (fout, format, ap);

  va_end (ap);

  fprintf (fout, "\n");
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

  if (!fout) fout = stderr;

  fprintf (fout, "Clock %d Start: %s: ",
           clockno, prettify (function_name));

  va_start (ap, format);

  vfprintf (fout, format, ap);

  va_end (ap);

  fprintf (fout, "\n");
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

  if (!fout) fout = stderr;

  fprintf (fout, "Clock %d Elapsed: %ld.%06lds %s: ",
           clockno, now.tv_sec, now.tv_usec, prettify (function_name));

  va_start (ap, format);

  vfprintf (fout, format, ap);

  va_end (ap);

  fprintf (fout, "\n");
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

  if (!fout) fout = stderr;

  fprintf (fout, "Clock %d Total Elapsed: %ld.%06lds  %s: ",
           clockno,
           gnc_clock_total[clockno].tv_sec,
           gnc_clock_total[clockno].tv_usec,
           prettify (function_name));

  va_start (ap, format);

  vfprintf (fout, format, ap);

  va_end (ap);

  fprintf (fout, "\n");
}

/********************************************************************\
\********************************************************************/

/* Search for str2 in first nchar chars of str1, ignore case..  Return
 * pointer to first match, or null.  */
char *
strncasestr(const char *str1, const char *str2, size_t len) 
{
  while (*str1 && len--) 
  {
    if (toupper(*str1) == toupper(*str2)) 
    {
      if (strncasecmp(str1,str2,strlen(str2)) == 0) 
      {
        return (char *) str1;
      }
    }
    str1++;
  }
  return NULL;
}

/* Search for str2 in str1, ignore case.  Return pointer to first
 * match, or null.  */
char *
strcasestr(const char *str1, const char *str2) 
{
   size_t len = strlen (str1);
   char * retval = strncasestr (str1, str2, len);
   return retval;
}

/********************************************************************\
\********************************************************************/

int 
safe_strcmp (const char * da, const char * db)
{
   SAFE_STRCMP (da, db);
   return 0;
}

int 
null_strcmp (const char * da, const char * db)
{
   if (da && db) return strcmp (da, db);
   if (!da && db && 0==db[0]) return 0;
   if (!db && da && 0==da[0]) return 0;
   if (!da && db) return -1;
   if (da && !db) return +1;
   return 0;
}

/********************************************************************\
\********************************************************************/

#define MAX_DIGITS 50

/* inverse of strtoul */
char *
ultostr (unsigned long val, int base)
{
  char buf[MAX_DIGITS];
  unsigned long broke[MAX_DIGITS];
  int i;
  unsigned long places=0, reval;
  
  if ((2>base) || (36<base)) return NULL;

  /* count digits */
  places = 0;
  for (i=0; i<MAX_DIGITS; i++) {
     broke[i] = val;
     places ++;
     val /= base;
     if (0 == val) break;
  }

  /* normalize */
  reval = 0;
  for (i=places-2; i>=0; i--) {
    reval += broke[i+1];
    reval *= base;
    broke[i] -= reval;
  }

  /* print */
  for (i=0; i<places; i++) {
    if (10>broke[i]) {
       buf[places-1-i] = 0x30+broke[i];  /* ascii digit zero */
    } else {
       buf[places-1-i] = 0x41-10+broke[i];  /* ascii capital A */
    }
  }
  buf[places] = 0x0;

  return g_strdup (buf);
}

/********************************************************************\
 * returns TRUE if the string is a number, possibly with whitespace
\********************************************************************/

gboolean
gnc_strisnum(const char *s)
{
  if (s == NULL) return FALSE;
  if (*s == 0) return FALSE;

  while (*s && isspace(*s))
    s++;

  if (*s == 0) return FALSE;
  if (!isdigit(*s)) return FALSE;

  while (*s && isdigit(*s))
    s++;

  if (*s == 0) return TRUE;

  while (*s && isspace(*s))
    s++;

  if (*s == 0) return TRUE;

  return FALSE;
}

/********************************************************************\
 * our own version of stpcpy
\********************************************************************/

char *
gnc_stpcpy (char *dest, const char *src)
{
  strcpy (dest, src);
  return (dest + strlen (src));
}

/********************************************************************\
  See header for docs.
\********************************************************************/

static void
kv_pair_helper(gpointer key, gpointer val, gpointer user_data)
{
  GSList **result = (GSList **) user_data;
  GHashTableKVPair *kvp = g_new(GHashTableKVPair, 1);

  kvp->key = key;
  kvp->value = val;
  *result = g_slist_prepend(*result, kvp);
}

GSList *
g_hash_table_key_value_pairs(GHashTable *table)
{
  GSList *result_list = NULL;
  g_hash_table_foreach(table, kv_pair_helper, &result_list);
  return result_list;
}

void
g_hash_table_kv_pair_free_gfunc(gpointer data, gpointer user_data)
{
  GHashTableKVPair *kvp = (GHashTableKVPair *) data;
  g_free(kvp);
}


/************************* END OF FILE ******************************\
\********************************************************************/
