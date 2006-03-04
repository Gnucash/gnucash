/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
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

#ifndef __G_UTILS_26_H__
#define __G_UTILS_26_H__

#include <glib/gtypes.h>
#include <stdarg.h>

/* Start hacks to make this file compile with glib 2.4 */
#define G_GNUC_MALLOC
/* End hacks to make this file compile with glib 2.4 */

G_BEGIN_DECLS

#ifdef G_OS_WIN32

/* On native Win32, directory separator is the backslash, and search path
 * separator is the semicolon.
 */
#define G_DIR_SEPARATOR '\\'
#define G_DIR_SEPARATOR_S "\\"
#define G_IS_DIR_SEPARATOR(c) ((c) == G_DIR_SEPARATOR || (c) == '/')
#define G_SEARCHPATH_SEPARATOR ';'
#define G_SEARCHPATH_SEPARATOR_S ";"

#else  /* !G_OS_WIN32 */

/* Unix */

#define G_DIR_SEPARATOR '/'
#define G_DIR_SEPARATOR_S "/"
#define G_IS_DIR_SEPARATOR(c) ((c) == G_DIR_SEPARATOR)
#define G_SEARCHPATH_SEPARATOR ':'
#define G_SEARCHPATH_SEPARATOR_S ":"

#endif /* !G_OS_WIN32 */

/* Retrive static string info
 */
#ifdef G_OS_WIN32
#define g_get_user_name g_get_user_name_utf8
#define g_get_real_name g_get_real_name_utf8
#define g_get_home_dir g_get_home_dir_utf8
#define g_get_tmp_dir g_get_tmp_dir_utf8
#endif

G_CONST_RETURN gchar* g_get_user_name        (void);
G_CONST_RETURN gchar* g_get_real_name        (void);
G_CONST_RETURN gchar* g_get_home_dir         (void);
G_CONST_RETURN gchar* g_get_tmp_dir          (void);
gchar*                g_get_prgname          (void);
void                  g_set_prgname          (const gchar *prgname);
G_CONST_RETURN gchar* g_get_application_name (void);
void                  g_set_application_name (const gchar *application_name);

G_CONST_RETURN gchar*    g_get_user_data_dir      (void);
G_CONST_RETURN gchar*    g_get_user_config_dir    (void);
G_CONST_RETURN gchar*    g_get_user_cache_dir     (void);
G_CONST_RETURN gchar* G_CONST_RETURN * g_get_system_data_dirs   (void);
G_CONST_RETURN gchar* G_CONST_RETURN * g_get_system_config_dirs (void);

G_CONST_RETURN gchar* G_CONST_RETURN * g_get_language_names (void);

/* Miscellaneous utility functions
 */
gint                  g_snprintf           (gchar       *string,
					    gulong       n,
					    gchar const *format,
					    ...) G_GNUC_PRINTF (3, 4);
gint                  g_vsnprintf          (gchar       *string,
					    gulong       n,
					    gchar const *format,
					    va_list      args);

#ifndef G_DISABLE_DEPRECATED

/* These two functions are deprecated and will be removed in the next
 * major release of GLib. Use g_path_get_dirname/g_path_get_basename
 * instead. Whatch out! The string returned by g_path_get_basename
 * must be g_freed, while the string returned by g_basename must not.*/
G_CONST_RETURN gchar* g_basename           (const gchar *file_name);
#define g_dirname g_path_get_dirname

#endif /* G_DISABLE_DEPRECATED */

#ifdef G_OS_WIN32
#define g_get_current_dir g_get_current_dir_utf8
#endif

/* The returned strings are newly allocated with g_malloc() */
gchar*                g_get_current_dir    (void);
gchar*                g_path_get_basename  (const gchar *file_name) G_GNUC_MALLOC;
gchar*                g_path_get_dirname   (const gchar *file_name) G_GNUC_MALLOC;

/* Set the pointer at the specified location to NULL */
void                  g_nullify_pointer    (gpointer    *nullify_location);

/* Look for an executable in PATH, following execvp() rules */
gchar*  g_find_program_in_path  (const gchar *program);


/* Glib version.
 * we prefix variable declarations so they can
 * properly get exported in windows dlls.
 */
GLIB_VAR const guint glib_major_version;
GLIB_VAR const guint glib_minor_version;
GLIB_VAR const guint glib_micro_version;
GLIB_VAR const guint glib_interface_age;
GLIB_VAR const guint glib_binary_age;

const gchar * glib_check_version (guint required_major,
                                  guint required_minor,
                                  guint required_micro);

#define GLIB_CHECK_VERSION(major,minor,micro)    \
    (GLIB_MAJOR_VERSION > (major) || \
     (GLIB_MAJOR_VERSION == (major) && GLIB_MINOR_VERSION > (minor)) || \
     (GLIB_MAJOR_VERSION == (major) && GLIB_MINOR_VERSION == (minor) && \
      GLIB_MICRO_VERSION >= (micro)))

G_END_DECLS

/*
 * On Windows, this macro defines a DllMain function that stores the
 * actual DLL name that the code being compiled will be included in.
 * STATIC should be empty or 'static'. DLL_NAME is the name of the
 * (pointer to the) char array where the DLL name will be stored. If
 * this is used, you must also include <windows.h>. If you need a more complex
 * DLL entry point function, you cannot use this.
 *
 * On non-Windows platforms, expands to nothing.
 */

#ifndef G_PLATFORM_WIN32
# define G_WIN32_DLLMAIN_FOR_DLL_NAME(static, dll_name)
#else
# define G_WIN32_DLLMAIN_FOR_DLL_NAME(static, dll_name)			   \
static char *dll_name;							   \
									   \
BOOL WINAPI								   \
DllMain (HINSTANCE hinstDLL,						   \
	 DWORD     fdwReason,						   \
	 LPVOID    lpvReserved)						   \
{									   \
  char bfr[1000];							   \
  switch (fdwReason)							   \
    {									   \
    case DLL_PROCESS_ATTACH:						   \
      GetModuleFileName ((HMODULE) hinstDLL, bfr, sizeof (bfr));	   \
      dll_name = g_path_get_basename (bfr);				   \
      break;								   \
    }									   \
									   \
  return TRUE;								   \
}
#endif /* G_PLATFORM_WIN32 */

guint                 g_strv_length    (gchar       **str_array);

#endif /* __G_UTILS_26_H__ */
