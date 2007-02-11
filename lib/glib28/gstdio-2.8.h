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
/* Contains only relevant differences between GLib 2.6 and GLib 2.8 */

#ifndef __G_STDIO_28_H__
#define __G_STDIO_28_H__

#ifdef G_OS_WIN32
#error "On Windows HAVE_GLIB_2_8 must be defined and gstdio28.h not included."
#endif

G_BEGIN_DECLS

#define g_access  access
#define g_chmod   chmod
#define g_creat   creat
#define g_chdir   chdir

G_END_DECLS

#endif /* __G_STDIO_H__ */
