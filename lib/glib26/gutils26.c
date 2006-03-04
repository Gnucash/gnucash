/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1998  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/. 
 */

/* 
 * MT safe for the unix part, FIXME: make the win32 part MT safe as well.
 */

#include "config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <locale.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_PWD_H
#include <pwd.h>
#endif
#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

/* implement gutils's inline functions
 */
#define	G_IMPLEMENT_INLINES 1
#define	__G_UTILS_C__
/* Start hacks to make this file compile with glib 2.4 */
#include <glib.h>
#include "gutils26.h"
/* End hacks to make this file compile with glib 2.4 */
#if 0
#include "glib.h"
#include "gprintfint.h"
#include "gthreadinit.h"
#include "galias.h"
#endif

#ifdef G_PLATFORM_WIN32
#  define STRICT		/* Strict typing, please */
#  include <windows.h>
#  undef STRICT
#  include <lmcons.h>		/* For UNLEN */
#endif /* G_PLATFORM_WIN32 */

#ifdef G_OS_WIN32
#  include <direct.h>
#  include <shlobj.h>
   /* older SDK (e.g. msvc 5.0) does not have these*/
#  ifndef CSIDL_INTERNET_CACHE
#    define CSIDL_INTERNET_CACHE 32
#  endif
#  ifndef CSIDL_COMMON_APPDATA
#    define CSIDL_COMMON_APPDATA 35
#  endif
#  ifndef CSIDL_COMMON_DOCUMENTS
#    define CSIDL_COMMON_DOCUMENTS 46
#  endif
#  ifndef CSIDL_PROFILE
#    define CSIDL_PROFILE 40
#  endif
#endif

#ifdef HAVE_CODESET
#include <langinfo.h>
#endif

#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
#include <libintl.h>
#endif

const guint glib_major_version = GLIB_MAJOR_VERSION;
const guint glib_minor_version = GLIB_MINOR_VERSION;
const guint glib_micro_version = GLIB_MICRO_VERSION;
#if 0
const guint glib_interface_age = GLIB_INTERFACE_AGE;
const guint glib_binary_age = GLIB_BINARY_AGE;
#endif

/**
 * glib_check_version:
 * @required_major: the required major version.
 * @required_minor: the required major version.
 * @required_micro: the required major version.
 *
 * Checks that the GLib library in use is compatible with the
 * given version. Generally you would pass in the constants
 * #GLIB_MAJOR_VERSION, #GLIB_MINOR_VERSION, #GLIB_MICRO_VERSION
 * as the three arguments to this function; that produces
 * a check that the library in use is compatible with
 * the version of GLib the application or module was compiled
 * against.
 *
 * Compatibility is defined by two things: first the version
 * of the running library is newer than the version
 * @required_major.required_minor.@required_micro. Second
 * the running library must be binary compatible with the
 * version @required_major.required_minor.@required_micro
 * (same major version.)
 *
 * Return value: %NULL if the GLib library is compatible with the
 *   given version, or a string describing the version mismatch.
 *   The returned string is owned by GLib and must not be modified
 *   or freed.
 *
 * Since: 2.6
 **/
const gchar *
glib_check_version (guint required_major,
                    guint required_minor,
                    guint required_micro)
{
  gint glib_effective_micro = 100 * GLIB_MINOR_VERSION + GLIB_MICRO_VERSION;
  gint required_effective_micro = 100 * required_minor + required_micro;

  if (required_major > GLIB_MAJOR_VERSION)
    return "GLib version too old (major mismatch)";
  if (required_major < GLIB_MAJOR_VERSION)
    return "GLib version too new (major mismatch)";
#if 0
  if (required_effective_micro < glib_effective_micro - GLIB_BINARY_AGE)
    return "GLib version too new (micro mismatch)";
#endif
  if (required_effective_micro > glib_effective_micro)
    return "GLib version too old (micro mismatch)";
  return NULL;
}

/* Based on execvp() from GNU Libc.
 * Some of this code is cut-and-pasted into gspawn.c
 */

G_LOCK_DEFINE_STATIC (g_utils_global);

static  gchar   *g_user_data_dir = NULL;
static  gchar  **g_system_data_dirs = NULL;
static  gchar   *g_user_cache_dir = NULL;
static  gchar   *g_user_config_dir = NULL;
static  gchar  **g_system_config_dirs = NULL;

#ifdef G_OS_WIN32

static gchar *
get_special_folder (int csidl)
{
  union {
    char c[MAX_PATH+1];
    wchar_t wc[MAX_PATH+1];
  } path;
  HRESULT hr;
  LPITEMIDLIST pidl = NULL;
  BOOL b;
  gchar *retval = NULL;

  hr = SHGetSpecialFolderLocation (NULL, csidl, &pidl);
  if (hr == S_OK)
    {
      if (G_WIN32_HAVE_WIDECHAR_API ())
	{
	  b = SHGetPathFromIDListW (pidl, path.wc);
	  if (b)
	    retval = g_utf16_to_utf8 (path.wc, -1, NULL, NULL, NULL);
	}
      else
	{
	  b = SHGetPathFromIDListA (pidl, path.c);
	  if (b)
	    retval = g_locale_to_utf8 (path.c, -1, NULL, NULL, NULL);
	}
      CoTaskMemFree (pidl);
    }
  return retval;
}

#endif

G_LOCK_DEFINE_STATIC (g_prgname);
static gchar *g_prgname = NULL;

gchar*
g_get_prgname (void)
{
  gchar* retval;

  G_LOCK (g_prgname);
#ifdef G_OS_WIN32
  if (g_prgname == NULL)
    {
      static gboolean beenhere = FALSE;

      if (!beenhere)
	{
	  gchar *utf8_buf = NULL;

	  beenhere = TRUE;
	  if (G_WIN32_HAVE_WIDECHAR_API ())
	    {
	      wchar_t buf[MAX_PATH+1];
	      if (GetModuleFileNameW (GetModuleHandle (NULL),
				      buf, G_N_ELEMENTS (buf)) > 0)
		utf8_buf = g_utf16_to_utf8 (buf, -1, NULL, NULL, NULL);
	    }
	  else
	    {
	      gchar buf[MAX_PATH+1];
	      if (GetModuleFileNameA (GetModuleHandle (NULL),
				      buf, G_N_ELEMENTS (buf)) > 0)
		utf8_buf = g_locale_to_utf8 (buf, -1, NULL, NULL, NULL);
	    }
	  if (utf8_buf)
	    {
	      g_prgname = g_path_get_basename (utf8_buf);
	      g_free (utf8_buf);
	    }
	}
    }
#endif
  retval = g_prgname;
  G_UNLOCK (g_prgname);

  return retval;
}

void
g_set_prgname (const gchar *prgname)
{
  G_LOCK (g_prgname);
  g_free (g_prgname);
  g_prgname = g_strdup (prgname);
  G_UNLOCK (g_prgname);
}

G_LOCK_DEFINE_STATIC (g_application_name);
static gchar *g_application_name = NULL;

/**
 * g_get_application_name:
 * 
 * Gets a human-readable name for the application, as set by
 * g_set_application_name(). This name should be localized if
 * possible, and is intended for display to the user.  Contrast with
 * g_get_prgname(), which gets a non-localized name. If
 * g_set_application_name() has not been called, returns the result of
 * g_get_prgname() (which may be %NULL if g_set_prgname() has also not
 * been called).
 * 
 * Return value: human-readable application name. may return %NULL
 *
 * Since: 2.2
 **/
G_CONST_RETURN gchar*
g_get_application_name (void)
{
  gchar* retval;

  G_LOCK (g_application_name);
  retval = g_application_name;
  G_UNLOCK (g_application_name);

  if (retval == NULL)
    return g_get_prgname ();
  
  return retval;
}

/**
 * g_set_application_name:
 * @application_name: localized name of the application
 *
 * Sets a human-readable name for the application. This name should be
 * localized if possible, and is intended for display to the user.
 * Contrast with g_set_prgname(), which sets a non-localized name.
 * g_set_prgname() will be called automatically by gtk_init(),
 * but g_set_application_name() will not.
 *
 * Note that for thread safety reasons, this function can only
 * be called once.
 *
 * The application name will be used in contexts such as error messages,
 * or when displaying an application's name in the task list.
 * 
 **/
void
g_set_application_name (const gchar *application_name)
{
  gboolean already_set = FALSE;
	
  G_LOCK (g_application_name);
  if (g_application_name)
    already_set = TRUE;
  else
    g_application_name = g_strdup (application_name);
  G_UNLOCK (g_application_name);

  if (already_set)
    g_warning ("g_set_application() name called multiple times");
}

/**
 * g_get_user_data_dir:
 * 
 * Returns a base directory in which to access application data such
 * as icons that is customized for a particular user.  
 *
 * On Unix platforms this is determined using the mechanisms described in
 * the <ulink url="http://www.freedesktop.org/Standards/basedir-spec">
 * XDG Base Directory Specification</ulink>
 * 
 * Return value: a string owned by GLib that must not be modified 
 *               or freed.
 * Since: 2.6
 **/
G_CONST_RETURN gchar*
g_get_user_data_dir (void)
{
  gchar *data_dir;  

  G_LOCK (g_utils_global);

  if (!g_user_data_dir)
    {
#ifdef G_OS_WIN32
      data_dir = get_special_folder (CSIDL_PERSONAL);
#else
      data_dir = (gchar *) g_getenv ("XDG_DATA_HOME");

      if (data_dir && data_dir[0])
        data_dir = g_strdup (data_dir);
#endif
      if (!data_dir || !data_dir[0])
	{
	  if (g_get_home_dir())
	    data_dir = g_build_filename (g_get_home_dir(), ".local", 
					 "share", NULL);
	  else
	    data_dir = g_build_filename (g_get_tmp_dir(), g_get_user_name(), ".local",
					 "share", NULL);
	}

      g_user_data_dir = data_dir;
    }
  else
    data_dir = g_user_data_dir;

  G_UNLOCK (g_utils_global);

  return data_dir;
}

/**
 * g_get_user_config_dir:
 * 
 * Returns a base directory in which to store user-specific application 
 * configuration information such as user preferences and settings. 
 *
 * On Unix platforms this is determined using the mechanisms described in
 * the <ulink url="http://www.freedesktop.org/Standards/basedir-spec">
 * XDG Base Directory Specification</ulink>
 * 
 * Return value: a string owned by GLib that must not be modified 
 *               or freed.
 * Since: 2.6
 **/
G_CONST_RETURN gchar*
g_get_user_config_dir (void)
{
  gchar *config_dir;  

  G_LOCK (g_utils_global);

  if (!g_user_config_dir)
    {
#ifdef G_OS_WIN32
      config_dir = get_special_folder (CSIDL_APPDATA);
#else
      config_dir = (gchar *) g_getenv ("XDG_CONFIG_HOME");

      if (config_dir && config_dir[0])
	config_dir = g_strdup (config_dir);
#endif
      if (!config_dir || !config_dir[0])
	{
	  if (g_get_home_dir())
	    config_dir = g_build_filename (g_get_home_dir(), ".config", NULL);
	  else
	    config_dir = g_build_filename (g_get_tmp_dir(), g_get_user_name(), ".config", NULL);
	}
      g_user_config_dir = config_dir;
    }
  else
    config_dir = g_user_config_dir;

  G_UNLOCK (g_utils_global);

  return config_dir;
}

/**
 * g_get_user_cache_dir:
 * 
 * Returns a base directory in which to store non-essential, cached
 * data specific to particular user.
 *
 * On Unix platforms this is determined using the mechanisms described in
 * the <ulink url="http://www.freedesktop.org/Standards/basedir-spec">
 * XDG Base Directory Specification</ulink>
 * 
 * Return value: a string owned by GLib that must not be modified 
 *               or freed.
 * Since: 2.6
 **/
G_CONST_RETURN gchar*
g_get_user_cache_dir (void)
{
  gchar *cache_dir;  

  G_LOCK (g_utils_global);

  if (!g_user_cache_dir)
    {
#ifdef G_OS_WIN32
      cache_dir = get_special_folder (CSIDL_INTERNET_CACHE); /* XXX correct? */
#else
      cache_dir = (gchar *) g_getenv ("XDG_CACHE_HOME");

      if (cache_dir && cache_dir[0])
          cache_dir = g_strdup (cache_dir);
#endif
      if (!cache_dir || !cache_dir[0])
	{
	  if (g_get_home_dir())
	    cache_dir = g_build_filename (g_get_home_dir(), ".cache", NULL);
	  else
	    cache_dir = g_build_filename (g_get_tmp_dir(), g_get_user_name(), ".cache", NULL);
	}
      g_user_cache_dir = cache_dir;
    }
  else
    cache_dir = g_user_cache_dir;

  G_UNLOCK (g_utils_global);

  return cache_dir;
}

/**
 * g_get_system_data_dirs:
 * 
 * Returns an ordered list of base directories in which to access 
 * system-wide application data.
 *
 * On Unix platforms this is determined using the mechanisms described in
 * the <ulink url="http://www.freedesktop.org/Standards/basedir-spec">
 * XDG Base Directory Specification</ulink>
 * 
 * Return value: a %NULL-terminated array of strings owned by GLib that must 
 *               not be modified or freed.
 * Since: 2.6
 **/
G_CONST_RETURN gchar * G_CONST_RETURN * 
g_get_system_data_dirs (void)
{
  gchar *data_dirs, **data_dir_vector;

  G_LOCK (g_utils_global);

  if (!g_system_data_dirs)
    {
#ifdef G_OS_WIN32
      char *appdata = get_special_folder (CSIDL_COMMON_APPDATA);
      char *docs = get_special_folder (CSIDL_COMMON_DOCUMENTS);
      
      if (appdata && docs)
	{
	  data_dirs = g_strconcat (appdata,
				   G_SEARCHPATH_SEPARATOR_S,
				   docs,
				   NULL);
	  g_free (appdata);
	  g_free (docs);
	}
      else if (appdata)
	data_dirs = appdata;
      else if (docs)
	data_dirs = docs;
      else
	data_dirs = NULL;

      if (data_dirs)
	{
	  data_dir_vector = g_strsplit (data_dirs, G_SEARCHPATH_SEPARATOR_S, 0);
	  g_free (data_dirs);
	}
      else
	{
	  /* Punt, return empty list */
	  data_dir_vector = g_strsplit ("", G_SEARCHPATH_SEPARATOR_S, 0);
	}
#else
      data_dirs = (gchar *) g_getenv ("XDG_DATA_DIRS");

      if (!data_dirs || !data_dirs[0])
          data_dirs = "/usr/local/share/:/usr/share/";

      data_dir_vector = g_strsplit (data_dirs, G_SEARCHPATH_SEPARATOR_S, 0);
#endif

      g_system_data_dirs = data_dir_vector;
    }
  else
    data_dir_vector = g_system_data_dirs;

  G_UNLOCK (g_utils_global);

  return (G_CONST_RETURN gchar * G_CONST_RETURN *) data_dir_vector;
}

/**
 * g_get_system_config_dirs:
 * 
 * Returns an ordered list of base directories in which to access 
 * system-wide configuration information.
 *
 * On Unix platforms this is determined using the mechanisms described in
 * the <ulink url="http://www.freedesktop.org/Standards/basedir-spec">
 * XDG Base Directory Specification</ulink>
 * 
 * Return value: a %NULL-terminated array of strings owned by GLib that must 
 *               not be modified or freed.
 * Since: 2.6
 **/
G_CONST_RETURN gchar * G_CONST_RETURN *
g_get_system_config_dirs (void)
{
  gchar *conf_dirs, **conf_dir_vector;

  G_LOCK (g_utils_global);

  if (!g_system_config_dirs)
    {
#ifdef G_OS_WIN32
      conf_dirs = get_special_folder (CSIDL_COMMON_APPDATA);
      if (conf_dirs)
	{
	  conf_dir_vector = g_strsplit (conf_dirs, G_SEARCHPATH_SEPARATOR_S, 0);
	  g_free (conf_dirs);
	}
      else
	{
	  /* Return empty list */
	  conf_dir_vector = g_strsplit ("", G_SEARCHPATH_SEPARATOR_S, 0);
	}
#else
      conf_dirs = (gchar *) g_getenv ("XDG_CONFIG_DIRS");

      if (!conf_dirs || !conf_dirs[0])
          conf_dirs = "/etc/xdg";

      conf_dir_vector = g_strsplit (conf_dirs, G_SEARCHPATH_SEPARATOR_S, 0);
#endif

      g_system_config_dirs = conf_dir_vector;
    }
  else
    conf_dir_vector = g_system_config_dirs;
  G_UNLOCK (g_utils_global);

  return (G_CONST_RETURN gchar * G_CONST_RETURN *) conf_dir_vector;
}

static GHashTable *alias_table = NULL;

/* read an alias file for the locales */
static void
read_aliases (gchar *file)
{
  FILE *fp;
  char buf[256];
  
  if (!alias_table)
    alias_table = g_hash_table_new (g_str_hash, g_str_equal);
  fp = fopen (file,"r");
  if (!fp)
    return;
  while (fgets (buf, 256, fp))
    {
      char *p, *q;

      g_strstrip (buf);

      /* Line is a comment */
      if ((buf[0] == '#') || (buf[0] == '\0'))
	continue;

      /* Reads first column */
      for (p = buf, q = NULL; *p; p++) {
	if ((*p == '\t') || (*p == ' ') || (*p == ':')) {
	  *p = '\0';
	  q = p+1;
	  while ((*q == '\t') || (*q == ' ')) {
	    q++;
	  }
	  break;
	}
      }
      /* The line only had one column */
      if (!q || *q == '\0')
	continue;
      
      /* Read second column */
      for (p = q; *p; p++) {
	if ((*p == '\t') || (*p == ' ')) {
	  *p = '\0';
	  break;
	}
      }

      /* Add to alias table if necessary */
      if (!g_hash_table_lookup (alias_table, buf)) {
	g_hash_table_insert (alias_table, g_strdup (buf), g_strdup (q));
      }
    }
  fclose (fp);
}

static char *
unalias_lang (char *lang)
{
  char *p;
  int i;

  if (!alias_table)
    read_aliases ("/usr/share/locale/locale.alias");

  i = 0;
  while ((p = g_hash_table_lookup (alias_table, lang)) && (strcmp (p, lang) != 0))
    {
      lang = p;
      if (i++ == 30)
        {
          static gboolean said_before = FALSE;
	  if (!said_before)
            g_warning ("Too many alias levels for a locale, "
		       "may indicate a loop");
	  said_before = TRUE;
	  return lang;
	}
    }
  return lang;
}

/* Mask for components of locale spec. The ordering here is from
 * least significant to most significant
 */
enum
{
  COMPONENT_CODESET =   1 << 0,
  COMPONENT_TERRITORY = 1 << 1,
  COMPONENT_MODIFIER =  1 << 2
};

/* Break an X/Open style locale specification into components
 */
static guint
explode_locale (const gchar *locale,
		gchar      **language, 
		gchar      **territory, 
		gchar      **codeset, 
		gchar      **modifier)
{
  const gchar *uscore_pos;
  const gchar *at_pos;
  const gchar *dot_pos;

  guint mask = 0;

  uscore_pos = strchr (locale, '_');
  dot_pos = strchr (uscore_pos ? uscore_pos : locale, '.');
  at_pos = strchr (dot_pos ? dot_pos : (uscore_pos ? uscore_pos : locale), '@');

  if (at_pos)
    {
      mask |= COMPONENT_MODIFIER;
      *modifier = g_strdup (at_pos);
    }
  else
    at_pos = locale + strlen (locale);

  if (dot_pos)
    {
      mask |= COMPONENT_CODESET;
      *codeset = g_strndup (dot_pos, at_pos - dot_pos);
    }
  else
    dot_pos = at_pos;

  if (uscore_pos)
    {
      mask |= COMPONENT_TERRITORY;
      *territory = g_strndup (uscore_pos, dot_pos - uscore_pos);
    }
  else
    uscore_pos = dot_pos;

  *language = g_strndup (locale, uscore_pos - locale);

  return mask;
}

/*
 * Compute all interesting variants for a given locale name -
 * by stripping off different components of the value.
 *
 * For simplicity, we assume that the locale is in
 * X/Open format: language[_territory][.codeset][@modifier]
 *
 * TODO: Extend this to handle the CEN format (see the GNUlibc docs)
 *       as well. We could just copy the code from glibc wholesale
 *       but it is big, ugly, and complicated, so I'm reluctant
 *       to do so when this should handle 99% of the time...
 */
GSList *_g_compute_locale_variants (const gchar *locale);
GSList *
_g_compute_locale_variants (const gchar *locale)
{
  GSList *retval = NULL;

  gchar *language = NULL;
  gchar *territory = NULL;
  gchar *codeset = NULL;
  gchar *modifier = NULL;

  guint mask;
  guint i;

  g_return_val_if_fail (locale != NULL, NULL);

  mask = explode_locale (locale, &language, &territory, &codeset, &modifier);

  /* Iterate through all possible combinations, from least attractive
   * to most attractive.
   */
  for (i = 0; i <= mask; i++)
    if ((i & ~mask) == 0)
      {
	gchar *val = g_strconcat (language,
				  (i & COMPONENT_TERRITORY) ? territory : "",
				  (i & COMPONENT_CODESET) ? codeset : "",
				  (i & COMPONENT_MODIFIER) ? modifier : "",
				  NULL);
	retval = g_slist_prepend (retval, val);
      }

  g_free (language);
  if (mask & COMPONENT_CODESET)
    g_free (codeset);
  if (mask & COMPONENT_TERRITORY)
    g_free (territory);
  if (mask & COMPONENT_MODIFIER)
    g_free (modifier);

  return retval;
}

/* The following is (partly) taken from the gettext package.
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.  */

static const gchar *
guess_category_value (const gchar *category_name)
{
  const gchar *retval;

  /* The highest priority value is the `LANGUAGE' environment
     variable.  This is a GNU extension.  */
  retval = g_getenv ("LANGUAGE");
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;

  /* `LANGUAGE' is not set.  So we have to proceed with the POSIX
     methods of looking to `LC_ALL', `LC_xxx', and `LANG'.  On some
     systems this can be done by the `setlocale' function itself.  */

  /* Setting of LC_ALL overwrites all other.  */
  retval = g_getenv ("LC_ALL");  
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;

  /* Next comes the name of the desired category.  */
  retval = g_getenv (category_name);
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;

  /* Last possibility is the LANG environment variable.  */
  retval = g_getenv ("LANG");
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;

#ifdef G_PLATFORM_WIN32
  /* g_win32_getlocale() first checks for LC_ALL, LC_MESSAGES and
   * LANG, which we already did above. Oh well. The main point of
   * calling g_win32_getlocale() is to get the thread's locale as used
   * by Windows and the Microsoft C runtime (in the "English_United
   * States" format) translated into the Unixish format.
   */
  retval = g_win32_getlocale ();
  if ((retval != NULL) && (retval[0] != '\0'))
    return retval;
#endif  

  return NULL;
}

typedef struct _GLanguageNamesCache GLanguageNamesCache;

struct _GLanguageNamesCache {
  gchar *languages;
  gchar **language_names;
};

static void
language_names_cache_free (gpointer data)
{
  GLanguageNamesCache *cache = data;
  g_free (cache->languages);
  g_strfreev (cache->language_names);
  g_free (cache);
}

/**
 * g_get_language_names:
 * 
 * Computes a list of applicable locale names, which can be used to 
 * e.g. construct locale-dependent filenames or search paths. The returned 
 * list is sorted from most desirable to least desirable and always contains 
 * the default locale "C".
 *
 * For example, if LANGUAGE=de:en_US, then the returned list is
 * "de", "en_US", "en", "C".
 *
 * This function consults the environment variables <envar>LANGUAGE</envar>, 
 * <envar>LC_ALL</envar>, <envar>LC_MESSAGES</envar> and <envar>LANG</envar> 
 * to find the list of locales specified by the user.
 * 
 * Return value: a %NULL-terminated array of strings owned by GLib 
 *    that must not be modified or freed.
 *
 * Since: 2.6
 **/
G_CONST_RETURN gchar * G_CONST_RETURN * 
g_get_language_names (void)
{
  static GStaticPrivate cache_private = G_STATIC_PRIVATE_INIT;
  GLanguageNamesCache *cache = g_static_private_get (&cache_private);
  const gchar *value;

  if (!cache)
    {
      cache = g_new0 (GLanguageNamesCache, 1);
      g_static_private_set (&cache_private, cache, language_names_cache_free);
    }

  value = guess_category_value ("LC_MESSAGES");
  if (!value)
    value = "C";

  if (!(cache->languages && strcmp (cache->languages, value) == 0))
    {
      gchar **languages;
      gchar **alist, **a;
      GSList *list, *l;
      gint i;

      g_free (cache->languages);
      g_strfreev (cache->language_names);
      cache->languages = g_strdup (value);

      alist = g_strsplit (value, ":", 0);
      list = NULL;
      for (a = alist; *a; a++)
	{
	  gchar *b = unalias_lang (*a);
	  list = g_slist_concat (list, _g_compute_locale_variants (b));
	}
      g_strfreev (alist);
      list = g_slist_append (list, g_strdup ("C"));

      cache->language_names = languages = g_new (gchar *, g_slist_length (list) + 1);
      for (l = list, i = 0; l; l = l->next, i++)
	languages[i] = l->data;
      languages[i] = NULL;

      g_slist_free (list);
    }

  return (G_CONST_RETURN gchar * G_CONST_RETURN *) cache->language_names;
}

guint
g_direct_hash (gconstpointer v)
{
  return GPOINTER_TO_UINT (v);
}

gboolean
g_direct_equal (gconstpointer v1,
		gconstpointer v2)
{
  return v1 == v2;
}

gboolean
g_int_equal (gconstpointer v1,
	     gconstpointer v2)
{
  return *((const gint*) v1) == *((const gint*) v2);
}

guint
g_int_hash (gconstpointer v)
{
  return *(const gint*) v;
}

/**
 * g_nullify_pointer:
 * @nullify_location: the memory address of the pointer.
 * 
 * Set the pointer at the specified location to %NULL.
 **/
void
g_nullify_pointer (gpointer *nullify_location)
{
  g_return_if_fail (nullify_location != NULL);

  *nullify_location = NULL;
}

#if 0
/**
 * g_get_codeset:
 * 
 * Get the codeset for the current locale.
 * 
 * Return value: a newly allocated string containing the name
 * of the codeset. This string must be freed with g_free().
 **/
gchar *
g_get_codeset (void)
{
  const gchar *charset;

  g_get_charset (&charset);

  return g_strdup (charset);
}

/* This is called from g_thread_init(). It's used to
 * initialize some static data in a threadsafe way.
 */
void
_g_utils_thread_init (void)
{
  g_get_language_names ();
}
#endif

#ifdef ENABLE_NLS

#include <libintl.h>

#ifdef G_PLATFORM_WIN32

G_WIN32_DLLMAIN_FOR_DLL_NAME (static, dll_name)

static const gchar *
_glib_get_locale_dir (void)
{
  static const gchar *cache = NULL;
  if (cache == NULL)
    cache = g_win32_get_package_installation_subdirectory
      (GETTEXT_PACKAGE, dll_name, "lib\\locale");

  return cache;
}

#undef GLIB_LOCALE_DIR
#define GLIB_LOCALE_DIR _glib_get_locale_dir ()

#endif /* G_PLATFORM_WIN32 */

#if 0
G_CONST_RETURN gchar *
_glib_gettext (const gchar *str)
{
  static gboolean _glib_gettext_initialized = FALSE;

  if (!_glib_gettext_initialized)
    {
      bindtextdomain(GETTEXT_PACKAGE, GLIB_LOCALE_DIR);
#    ifdef HAVE_BIND_TEXTDOMAIN_CODESET
      bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");
#    endif
      _glib_gettext_initialized = TRUE;
    }
  
  return dgettext (GETTEXT_PACKAGE, str);
}
#endif

#endif /* ENABLE_NLS */

/**
 * g_strv_length:
 * @str_array: a %NULL-terminated array of strings.
 * 
 * Returns the length of the given %NULL-terminated 
 * string array @str_array.
 * 
 * Return value: length of @str_array.
 *
 * Since: 2.6
 **/
guint
g_strv_length (gchar **str_array)
{
  guint i = 0;

  g_return_val_if_fail (str_array != NULL, 0);

  while (str_array[i])
    ++i;

  return i;
}

#if 0
#define __G_UTILS_C__
#include "galiasdef.c"
#endif
