/* This file has been copied from glib-2.6.6 into libgsf-1.12.3 to support
 * compilation against glib-2.4.14. -- jsled, 2005-11-08
 */

#ifndef __GLIB_24_26_COMPAT_H__
#define __GLIB_24_26_COMPAT_H__

#include <glib.h>

// START from gstdio.h
/* gstdio.h - GFilename wrappers for C library functions
 *
 * Copyright 2004 Tor Lillqvist
 *
 * GLib is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * GLib is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with GLib; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <sys/stat.h>

/* Just pass on to the system functions, so there's no potential for data
 * format mismatches, especially with large file interfaces.
 */

#define g_open    open
#define g_rename  rename
#define g_mkdir   mkdir
#define g_stat    stat
#define g_lstat   lstat
#define g_unlink  unlink
#define g_remove  remove
#define g_rmdir   rmdir
#define g_fopen   fopen
#define g_freopen freopen

// END from gstdio.h

// START from gconvert.h
gchar *g_filename_display_name (const gchar *filename); // G_GNUC_MALLOC;
// END from gconvert.h

/* from glib-2.6[.6] gutils.h */
#ifdef G_OS_WIN32

/* On Win32, the canonical directory separator is the backslash, and
 * the search path separator is the semicolon. Note that also the
 * (forward) slash works as directory separator.
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

#endif /* __GLIB_24_26_COMPAT_H__ */
