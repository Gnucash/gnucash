/********************************************************************\
 * gnc-helpers.c -- gnucash g-wrap helper functions                 *
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

#include "config.h"

#include <string.h>
#include <glib.h>
#include <libguile.h>
#include "guile-mappings.h"

#include <g-wrap-wct.h>

#include "glib-helpers.h"


/* These will eventually go into (g-wrapped glib). */

static SCM
glist_to_scm_list_helper(GList *glist, SCM wct)
{
  SCM list = SCM_EOL;
  GList *node;

  for (node = glist; node; node = node->next)
    list = scm_cons (gw_wcp_assimilate_ptr(node->data, wct), list);

  return scm_reverse (list);
}

SCM
gnc_glist_to_scm_list(GList *glist, SCM wct)
{
  SCM_ASSERT(gw_wct_p(wct), wct, SCM_ARG1, "gnc_glist_to_scm_list");
  return(glist_to_scm_list_helper(glist, wct));
}

GList*
gnc_scm_list_to_glist(SCM rest)
{
  GList *result = NULL;
  SCM scm_item;
  
  SCM_ASSERT(SCM_LISTP(rest), rest, SCM_ARG1, "gnc_scm_list_to_glist");

  while(!SCM_NULLP(rest))
  {
    void *item;

    scm_item = SCM_CAR(rest);
    rest = SCM_CDR(rest);

    /* fixes a bug in g-wrap */
    if (scm_item == SCM_BOOL_F)
    {
      result = g_list_prepend(result, NULL);
    }
    else
    {
      if (!gw_wcp_p(scm_item))
        scm_misc_error("gnc_scm_list_to_glist",
                       "Item in list not a gw:wcp.", scm_item);
      
      item = gw_wcp_get_ptr(scm_item);
      result = g_list_prepend(result, item);
    }
  }

  return g_list_reverse(result);
}

static SCM
glist_map_helper(GList *glist, SCM wct, SCM thunk)
{
  SCM list = SCM_EOL;
  GList *node;

  for (node = glist; node; node = node->next)
    list = scm_cons (scm_call_1(thunk, gw_wcp_assimilate_ptr(node->data, wct)),
		     list);

  return scm_reverse (list);
}

SCM
gnc_glist_scm_map(SCM wct, SCM thunk, GList* glist)
{
  SCM_ASSERT(gw_wct_p(wct), wct, SCM_ARG1, "gnc_glist_map");
  SCM_ASSERT(SCM_PROCEDUREP(thunk), thunk, SCM_ARG2, "gnc_glist_scm_map");  
  return(glist_map_helper(glist, wct, thunk));
}

void
gnc_glist_scm_for_each(SCM wct, SCM thunk, GList *glist)
{
  GList *lp;
  SCM_ASSERT(gw_wct_p(wct), wct, SCM_ARG1, "gnc_glist_map");
  SCM_ASSERT(SCM_PROCEDUREP(thunk), thunk, SCM_ARG2, "gnc_glist_scm_for_each");  
  for(lp = glist; lp; lp = lp->next) {
    scm_call_1(thunk, gw_wcp_assimilate_ptr(lp->data, wct));
  }
}

/********************************************************************
 * gnc_glist_string_to_scm
 * i.e. (glist-of (<gw:mchars> calee-owned) callee-owned)
 * or equivalently
 * i.e. (glist-of (<gw:gchars> calee-owned) callee-owned)
 ********************************************************************/
SCM
gnc_glist_string_to_scm(GList *glist)
{
  SCM list = SCM_EOL;
  GList *node;

  for (node = glist; node; node = node->next)
    list = scm_cons (scm_makfrom0str(node->data), list);

  return scm_reverse (list);
}




/********************************************************************
 * gnc_scm_to_glist_string
 * i.e. (glist-of (<gw:mchars> calee-owned) callee-owned)
 * or equivalently
 * i.e. (glist-of (<gw:gchars> calee-owned) callee-owned)
 ********************************************************************/

GList *
gnc_scm_to_glist_string(SCM list)
{
  GList *glist = NULL;

  while (!SCM_NULLP (list))
  {
    /* glist = g_list_prepend (glist, gh_scm2newstr(SCM_CAR(list), NULL)); */
    char * str = SCM_STRING_CHARS (SCM_CAR(list));
    if (str)
    {
      str = g_strdup (str);
      glist = g_list_prepend (glist, str);
    }
    list = SCM_CDR (list);
  }

  return g_list_reverse (glist);
}

/********************************************************************
 * gnc_glist_string_p
 ********************************************************************/

int
gnc_glist_string_p(SCM list) {
  return SCM_LISTP(list);
}
