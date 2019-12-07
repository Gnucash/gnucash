/********************************************************************\
 * glib-guile.h -- glib helper functions for guile                  *
 * Copyright (C) 2000 Linas Vepstas                                 *
 * Copyright (C) 2006 Chris Shoemaker <c.shoemaker@cox.net>         *
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
\********************************************************************/

#ifndef GLIB_GUILE_H
#define GLIB_GUILE_H

#include <glib.h>
#include <libguile.h>

SCM gnc_glist_to_scm_list(GList *glist, gchar *wct);
GList* gnc_scm_list_to_glist(SCM wcp_list);

SCM     gnc_glist_string_to_scm(GList * list);
GList * gnc_scm_to_glist_string(SCM list);
int     gnc_glist_string_p(SCM list);

GSList * gnc_scm_to_gslist_string(SCM list);

/** An opaque process structure returned by gnc_spawn_process_async. */
typedef struct _Process Process;

/** Wraps g_spawn_async_with_pipes minimally.  Use gnc_process_get_fd to access
 *  the file descriptors to the child.  To close them and free the memory
 *  allocated for the process once it has exited, call gnc_detach_process.
 *
 *  @param argl A list of null-terminated strings used as arguments for spawning,
 *  i.e. "perl" "-w" "my-perl-script".  Will be freed inside.
 *
 *  @param search_path Determines whether the first element of argl will be
 *  looked for in the user's PATH.
 *
 *  @return A pointer to a structure representing the process or NULL on failure.
 */
Process *gnc_spawn_process_async(GList *argl, const gboolean search_path);

/** Accesses a given process structure and returns the file descriptor connected
 *  to the childs stdin, stdout or stderr.
 *
 *  @param proc A process structure returned by gnc_spawn_process_async.
 *
 *  @param std_fd 0, 1 or 2.
 *
 *  @return The file descriptor to write to the child on 0, or read from the
 *  childs output or error on 1 or 2, resp. */
gint gnc_process_get_fd(const Process *proc, const gint std_fd);

/** Close the file descriptors to a given process and declare it as detached.  If
 *  it is both dead and detached, the allocated memory will be freed.
 *
 *  @param proc A process structure returned by gnc_spawn_process_async.
 *
 *  @param kill_it If TRUE, kill the process. */
void gnc_detach_process(Process *proc, const gboolean kill_it);

#endif
