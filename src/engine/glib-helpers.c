/********************************************************************\
 * gnc-helpers.c -- gnucash glib helper functions                   *
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

#include "config.h"

#include <string.h>
#include <glib.h>
#include <libguile.h>
#include "guile-mappings.h"
#include "swig-runtime.h"
#include "glib-helpers.h"


static SCM
glist_to_scm_list_helper(GList *glist, swig_type_info *wct)
{
  SCM list = SCM_EOL;
  GList *node;

  for (node = glist; node; node = node->next)
      list = scm_cons(SWIG_NewPointerObj(node->data, wct, 0), list);

  return scm_reverse (list);
}

SCM
gnc_glist_to_scm_list(GList *glist, gchar *wct)
{
    swig_type_info *stype = SWIG_TypeQuery(wct);
    g_return_val_if_fail(stype, SCM_UNDEFINED);
    return glist_to_scm_list_helper(glist, stype);
}

GList *
gnc_scm_list_to_glist(SCM rest)
{
  GList *result = NULL;
  SCM scm_item;

  SWIG_GetModule(NULL); /* Work-around for SWIG bug. */
  SCM_ASSERT(scm_is_list(rest), rest, SCM_ARG1, "gnc_scm_list_to_glist");

  while(!scm_is_null(rest))
  {
    void *item;

    scm_item = SCM_CAR(rest);
    rest = SCM_CDR(rest);

    if (scm_item == SCM_BOOL_F)
    {
      result = g_list_prepend(result, NULL);
    }
    else
    {
      if (!SWIG_IsPointer(scm_item))
        scm_misc_error("gnc_scm_list_to_glist",
                       "Item in list not a wcp.", scm_item);
      
      item = (void *)SWIG_PointerAddress(scm_item);
      result = g_list_prepend(result, item);
    }
  }

  return g_list_reverse(result);
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
 * i.e. (glist-of (<gw:mchars> callee-owned) callee-owned)
 * or equivalently
 * i.e. (glist-of (<gw:gchars> callee-owned) callee-owned)
 ********************************************************************/

GList *
gnc_scm_to_glist_string(SCM list)
{
  GList *glist = NULL;

  while (!scm_is_null (list))
  {
    const gchar * str = scm_to_locale_string (SCM_CAR(list));
    if (str)
      glist = g_list_prepend (glist, g_strdup (str));
    list = SCM_CDR (list);
  }

  return g_list_reverse (glist);
}

GSList *
gnc_scm_to_gslist_string(SCM list)
{
  GSList *gslist = NULL;

  while (!scm_is_null (list))
  {
    const gchar * str = scm_to_locale_string (SCM_CAR(list));
    if (str)
      gslist = g_slist_prepend (gslist, g_strdup (str));
    list = SCM_CDR (list);
  }

  return g_slist_reverse (gslist);
}

/********************************************************************
 * gnc_glist_string_p
 ********************************************************************/

int
gnc_glist_string_p(SCM list) {
  return scm_is_list(list);
}
