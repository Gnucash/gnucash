/* gfileutils.h - File utility functions
 *
 *  Copyright 2000 Red Hat, Inc.
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

#ifndef __G_FILEUTILS_2_8_H__
#define __G_FILEUTILS_2_8_H__

G_BEGIN_DECLS

gboolean g_file_set_contents (const gchar *filename,
			      const gchar *contents,
			      gssize	     length,
			      GError	   **error);

G_END_DECLS

#endif /* __G_FILEUTILS_2_8_H__ */
