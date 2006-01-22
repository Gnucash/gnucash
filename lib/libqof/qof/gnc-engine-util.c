/********************************************************************\
 * gnc-engine-util.c -- QOF utility functions                       *
 * Copyright (C) 1997 Robin D. Clark                                *
 * Copyright (C) 1997-2001,2004 Linas Vepstas <linas@linas.org>     *
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

#include "config.h"

#include <ctype.h>
#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "qof.h"
#include "gnc-engine-util.h"


/********************************************************************\
\********************************************************************/

/* Search for str2 in first nchar chars of str1, ignore case..  Return
 * pointer to first match, or null.  */
char *
strncasestr(const unsigned char *str1, const unsigned char *str2, size_t len) 
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
safe_strcasecmp (const char * da, const char * db)
{
   SAFE_STRCASECMP (da, db);
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
  for (i=0; i<(int)places; i++) {
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
gnc_strisnum(const unsigned char *s)
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

/* =================================================================== */
/* Return NULL if the field is whitespace (blank, tab, formfeed etc.)  
 * Else return pointer to first non-whitespace character. */

const char *
qof_util_whitespace_filter (const char * val)
{
	size_t len;
	if (!val) return NULL;

	len = strspn (val, "\a\b\t\n\v\f\r ");
	if (0 == val[len]) return NULL;
	return val+len;
}

/* =================================================================== */
/* Return integer 1 if the string starts with 't' or 'T' or contains the 
 * word 'true' or 'TRUE'; if string is a number, return that number. */

int
qof_util_bool_to_int (const char * val)
{
	const char * p = qof_util_whitespace_filter (val);
	if (!p) return 0;
	if ('t' == p[0]) return 1;
	if ('T' == p[0]) return 1;
	if ('y' == p[0]) return 1;
	if ('Y' == p[0]) return 1;
	if (strstr (p, "true")) return 1;
	if (strstr (p, "TRUE")) return 1;
	if (strstr (p, "yes")) return 1;
	if (strstr (p, "YES")) return 1;
	return atoi (val);
}

/********************************************************************\
 * The engine string cache
\********************************************************************/

static GCache * gnc_string_cache = NULL;

#ifdef THESE_CAN_BE_USEFUL_FOR_DEGUGGING
static guint g_str_hash_KEY(gconstpointer v) {
    return g_str_hash(v);
}
static guint g_str_hash_VAL(gconstpointer v) {
    return g_str_hash(v);
}
static gpointer g_strdup_VAL(gpointer v) {
    return g_strdup(v);
}
static gpointer g_strdup_KEY(gpointer v) {
    return g_strdup(v);
}
static void g_free_VAL(gpointer v) {
    return g_free(v);
}
static void g_free_KEY(gpointer v) {
    return g_free(v);
}
static gboolean gnc_str_equal(gconstpointer v, gconstpointer v2)
{
    return (v && v2) ? g_str_equal(v, v2) : FALSE;
}
#endif

GCache*
gnc_engine_get_string_cache(void)
{
    if(!gnc_string_cache) {
        gnc_string_cache = g_cache_new(
            (GCacheNewFunc) g_strdup, /* value_new_func     */
            g_free,                   /* value_destroy_func */
            (GCacheDupFunc) g_strdup, /* key_dup_func       */
            g_free,                   /* key_destroy_func   */
            g_str_hash,               /* hash_key_func      */
            g_str_hash,               /* hash_value_func    */
            g_str_equal);             /* key_equal_func     */
    }
    return gnc_string_cache;
}

void
gnc_engine_string_cache_destroy (void)
{
    if (gnc_string_cache)
        g_cache_destroy (gnc_string_cache);
    gnc_string_cache = NULL;
}

void
gnc_string_cache_remove(gconstpointer key)
{
    if (key) 
        g_cache_remove(gnc_engine_get_string_cache(), key);
}

/* TODO: It would be better if this returned gpointerconst.  The
   returned strings really should be treated as const.  Callers must
   not modify them. */
gpointer
gnc_string_cache_insert(gconstpointer key)
{
    if (key) 
        return g_cache_insert(gnc_engine_get_string_cache(), (gpointer)key);
    return NULL;
}

void
qof_init (void)
{
	gnc_engine_get_string_cache ();
	guid_init ();
	qof_object_initialize ();
	qof_query_init ();
	qof_book_register ();
}

void
qof_close(void)
{
	qof_query_shutdown ();
	qof_object_shutdown ();
	guid_shutdown ();
	gnc_engine_string_cache_destroy ();
}


/************************* END OF FILE ******************************\
\********************************************************************/
