/********************************************************************\
 * glib-helpers.h -- gnucash g-wrap helper functions                 *
 * Copyright (C) 2000 Linas Vepstas                                 *
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
\********************************************************************/

#ifndef GLIB_HELPERS_H
#define GLIB_HELPERS_H

#include <guile/gh.h>
#include <glib.h>

SCM gnc_glist_to_scm_list(GList *glist, SCM wct);
GList* gnc_scm_list_to_glist(SCM wcp_list);

SCM gnc_glist_scm_map(SCM wct, SCM thunk, GList *glist);
void gnc_glist_scm_for_each(SCM wct, SCM thunk, GList *glist);

SCM     gnc_glist_string_to_scm(GList * list);
GList * gnc_scm_to_glist_string(SCM list);
int     gnc_glist_string_p(SCM list);



#endif
